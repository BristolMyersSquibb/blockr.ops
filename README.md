# blockr.ops

Maintenance tooling for a fleet of
[blockr](https://github.com/BristolMyersSquibb/blockr.core) boards: read and
write them, turn one into the R script that rebuilds it, count what it holds,
and find the defects that accumulate in a stored board.

Operator tooling, and it reads like it. Output is dense, the findings assume you
know what a board is made of, and nothing is smoothed over. Nothing here is
needed to build a board in a browser. It is for the point where the boards
themselves became the artefact you maintain: many of them, kept over time, in
version control.

## Install

```r
pak::pak("BristolMyersSquibb/blockr.ops")
```

## Working on a stored board

The whole flow, on one file.

```r
library(blockr.ops)

board <- read_board("study-0001.json")
```

**What is in it.**

```r
board_stats(board)
#> board study-0001-v003
#>
#> 104 blocks, 103 links, 10 views, 12 stacks, 6 extensions, 11 options
#> 5347 payload entries, 2432 distinct (2915 duplicated, 55%; see board_lint())
#>
#> blocks by package          panels per view
#>   blockr.viz        46       Study Population   12
#>   blockr.extra      28       Adverse Events     16
#>   blockr.dm         21       Laboratory         14
#>   blockr.dplyr       5       Vital Signs        12
#>   blockr.pharma      4       ...
```

**What is wrong with it.**

```r
board_lint(board)
#> board lint: 105 findings
#>
#>   duplicate_state        63
#>   ambiguous_visible      32
#>   suffixed_state          8
#>   unexported_ctor         2
#>
#> first few:
#>   ambiguous_visible  block arczvqjr  `visible` stored 39 times with no exact
#>                                       name; attr() lookup is ambiguous
#>   ... (100 more; the return value is a data frame)
```

**How far it has drifted from the layout it is supposed to have.** State the
shape once; the columns are in twelfths, and the rail is in pixels because that
is what a grid stores.

```r
tmpl <- layout_template(
  columns       = c(filters = 2, main = 6),
  rail_position = "right",
  rail_size     = rail_twelfths(4)
)

layout_drift(board, tmpl)
#> layout drift: 37 findings across 10 views
#>
#>   Laboratory        column_sizes  30%/70%   -> 25%/75%
#>   Laboratory        focus         block_panel-ebyeagxk -> dropped
#>   Laboratory        nested_sizes  44%/56%   -> 50%/50%
#>   Laboratory        rail_size     607       -> 533
#>   Study Population  column_sizes  24%/76%   -> 25%/75%
#>   Exposure          column_sizes  17%/83%   -> 25%/75%
#>   Data              column_count  3 columns -> 2 columns; left alone
#>   ...
```

Ten views intended to share one column ratio had ten different ones. That is
what a year of people dragging sashes looks like.

**Put it back.**

```r
clean <- normalize_board(board, tmpl)

board_lint(clean)
#> board lint: 2 findings
#>   unexported_ctor  2

layout_drift(clean, tmpl)
#> layout drift: 1 findings across 1 views
#>   Data  column_count  3 columns -> 2 columns; left alone

write_board(clean, "study-0001.json")
#> 419,663 -> 299,088 bytes
```

The two survivors are both refusals rather than failures. The Data view has
three columns on purpose, so it is left alone and named. The two
`unexported_ctor` findings need an `@export` in blockr.assistant, not a
cleanup.

**Check it still round-trips**, and read it as R.

```r
check_board_script(clean, rename = FALSE, tidy = FALSE)
#> board script round trip: ok
#>   blocks ok | links ok | stacks ok | options ok
#>   extensions ok | views ok | grids ok

write_board_script(clean, "study-0001.R", name = "study_board")
```

### One thing that will bite you

`read_board()` rebuilds the blocks, so **every package the board's blocks come
from has to be installed**. A board carrying a `blockr.pharma` block will not
load without blockr.pharma, and the error names the package. `board_stats()`
tells you which packages a board needs, but only once it has loaded, so for a
board you cannot open, read the JSON directly and look at the `constructor`
entries.

## Normalizing

`normalize_board()` is the two halves together. Run them separately when you
only want one.

| | what it changes | what a user would notice |
|---|---|---|
| `normalize_layout()` | column ratios, nested split ratios, rail width and collapsed state, drops `focus` | panels change size |
| `normalize_state()` | collapses duplicate payload entries | 14 blocks stop showing sections they were not saved with |

Neither moves a block between columns, adds or removes a panel from a view, or
touches block configuration. A view whose column count does not match the
template is left alone and reported: three columns where the template says two
is more likely a considered exception than drift.

`normalize_state()` is the one that changes what people see, and that is the
repair rather than a side effect. Look at `board_lint()`'s `ambiguous_visible`
rows before writing.

`layout_drift()` and `board_lint()` are the report-only halves. Both return data
frames, so `subset(board_lint(board), kind == "ambiguous_visible")` gets you the
list.

## Reading and writing

A board file is JSON, and reading it wrong is easy to do quietly.
`jsonlite::fromJSON()` simplifies by default: a one-element list becomes a
scalar, a list of equal-length lists becomes a data frame, and a payload that
went in as a list comes back as something `blockr_deser()` cannot rebuild. The
failure is not always an error; sometimes the board just loads wrong.
`read_board()` passes the arguments that keep the structure intact, and
`write_board()` writes pretty, because a board file that diffs is the point of
keeping it in git.

## The lint checks

`board_lint()` returns a data frame with `kind`, `id` and `detail`. The
checks, each grounded in something found on a real board:

**`ambiguous_visible`.** Several `visible.N` payload entries and no exact
`visible`. `visible_sections()` in blockr.dock reads `attr(blk, "visible")`, and
`attr()` partial-matches by default:

| stored | result |
|---|---|
| exact `visible` present | it wins, suffixed entries ignored |
| one `visible.N`, no exact | partial-matches, silently acts as the value |
| several `visible.N`, no exact | ambiguous, `attr()` is `NULL`, falls back to `c("inputs", "outputs")` |

So the block silently loses its chrome setting, with no error anywhere. Not
every one looks wrong; the fallback is sometimes what the block wanted. On a
ten-view board we checked, a third of the ambiguous ones rendered differently
because of it.

**`duplicate_state`.** Duplicated payload entries without that ambiguity.
Harmless but not free: 55% of every entry in the file, and all of them in the
diff.

**`unexported_ctor`.** The recorded constructor is not exported by its package.
Deserialization reaches it anyway (`get0()` on the namespace), so this never
fails at load. It means a constructor a board can store is not part of any
package's API, and generated source has to use `:::`.

**`serialized_ctor`.** The constructor is stored as a serialized closure rather
than a name and a package: kilobytes of R bytecode in the file, pinned to the R
version that wrote it, unreadable in a diff. This is what an unexported
constructor degrades to when it is called without being told its own identity.

**`non_literal_payload`.** A payload value that is a function, environment or
raw vector. Everything else is a literal, which is why boards can be written
back as source at all.

## Boards as R source

A board's JSON is a serialization of the constructor calls that produced it:
every object records its constructor name, the package it lives in, and a
payload of literals. So the R form and the JSON form carry the same information,
and the conversion is mechanical.

```r
cat(board_script(board, name = "my_board"))
write_board_script(board, "my-board.R", name = "my_board")
check_board_script(board)
```

Two things this is for. A readable artefact: generated source is smaller than
pretty JSON, has block ids you can read, and puts a code block's script back on
its own lines so it diffs one line at a time instead of hiding behind `\n`
escapes. And a test: serialize, generate, evaluate, re-serialize, compare. Run
it over every board you store and it fails the day one starts carrying state
that no constructor argument can restore. A board file should be a specification
of a pipeline, not a specification plus whatever the upstream happened to look
like when it was saved.

`check_board_script()` has two modes.

- `rename = FALSE, tidy = FALSE` compares exactly: the re-serialized board must
  be `identical()` to the stored one, component by component. This is the CI
  invariant.
- The defaults rename ids and tidy the payloads, so the comparison is
  equivalence instead: same block set, same payloads once ids are remapped, same
  link topology, same view membership.

Sweeping a directory:

```r
files <- list.files("boards", "[.]json$", full.names = TRUE)
bad <- Filter(
  function(f) !check_board_script(read_board(f), rename = FALSE, tidy = FALSE)$ok,
  files
)
```

### What makes the output readable

A plain `dput()` of a board is not. Five things make the difference.

**Slugged block ids.** A stored id is a random slug; the block's display name is
not. `bsifddnw` becomes `global_population_filter`. Every reference moves with
it, and there are more references than the obvious ones: besides links, view
members, grid panels and stack members, blocks point at each other through
payload fields.

**Slugged view ids**, carried into the `grids` keys and `active`.

**Collapsed state**, so a block comes back with the `visible` setting it lost.

**Code blocks as code.** A code block's `script` is a character vector, one
element per source line. Deparsed at width it becomes a wrapped blob; emitted
one element per line it reads as the R it is.

**Blocks grouped by view.** The file follows the app rather than the storage
order, under a comment naming the view that first shows each block.

Do not run styler on the output. It reflows the code-block script vectors back
into a blob.

`board_script()` also takes `ref_fields`, the payload fields that hold a block
id. Only those are rewritten when ids are renamed, because a payload value can
coincide with an id without being a reference: a `head` block stores
`direction = "head"`, and on a board with a block called `head` rewriting it
breaks the board. Anything the rename leaves behind raises a warning naming the
field, which is how you find a block type with a reference field the default
does not cover.

### The layout

A persisted `dock_grid` has exactly five fields: `orientation`, `children`,
`sizes`, `focus` and `rails`. The authoring DSL (`dock_grid()`, `group()`,
`panels()`, `rail()`) expresses four of them exactly, including rail width,
collapsed state and open tab, so generated `dock_grid()` source reproduces a
view's geometry exactly. The layout needs no approximation and no restriction
of what the UI may produce.

`focus` is the fifth and is deliberately dropped. It is dockView's
`activeGroup`, the group that last held keyboard focus. At runtime it only feeds
where a newly added panel lands, and `prev_active_group()` already falls back to
the last group. It is not which tab is open; that is `active` on the leaf, which
is emitted.

## Tests

```r
devtools::test()
```

The suite runs against a small board built from blockr.core and blockr.dock
alone. For a real check, run `check_board_script()` and `board_lint()` over your
own stored boards; that is what the package is for.

## Caveats

**Renaming ids** is only safe if nothing outside the board keys off them. Use
`rename = FALSE` if you are not sure.

**`rail()` is not in every blockr.dock.** A board with a rail generates source
that calls `blockr.dock::rail()`, which some released versions do not export.
`board_script()` warns when the script it just produced names a framework
function the installed packages cannot supply, so you find out before you save
the file rather than when someone tries to run it.
