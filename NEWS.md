# blockr.ops 0.1.0

First version. Four areas.

**Board files.** `read_board()` and `write_board()`, with the `jsonlite`
arguments that keep a board's structure intact. Getting these wrong does not
always error; sometimes the board just loads wrong.

**Statistics.** `board_stats()` counts blocks by package and by type, links,
views with their panel counts, stacks, extensions, options, and the size of the
serialized payload.

**Lint.** `board_lint()` reports the defects that accumulate in a stored board:
`ambiguous_visible` (several `visible.N` entries and no exact one, so `attr()`
partial matching goes ambiguous and the block silently shows every section),
`duplicate_state`, `unexported_ctor`, `serialized_ctor` and
`non_literal_payload`.

**Boards as R source.** `board_script()` and `write_board_script()` turn a board
into the R that rebuilds it, with slugged ids, collapsed state, and code blocks
emitted as code. `check_board_script()` generates, evaluates and compares, in an
exact mode for CI and an equivalence mode for the readable output.
