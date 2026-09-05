# A block's serialized payload is state, not authorship, and it accumulates.
# On a large board 54% of all payload entries were duplicate `visible.N`
# attributes; one block carries 223 of them.
#
# They are not inert. `visible_sections()` in blockr.dock reads
# `attr(blk, "visible")`, and `attr()` partial-matches by default:
#
#   exact `visible` present    -> it wins, the suffixed entries are ignored
#   one `visible.N`, no exact  -> it partial-matches and acts as the value
#   several, no exact          -> ambiguous, `attr()` is NULL, the block falls
#                                 back to c("inputs", "outputs")
#
# So on a board that has been saved often enough, blocks silently lose their
# chrome setting. Collapsing the duplicates restores it. The rule below matters:
# the exact name is what the constructor's formal argument matches, so it wins;
# with no exact name the payload runs newest first, so the first suffixed entry
# is the current one. Keeping the first blindly gets real boards wrong.

# A suffixed name is accumulation when others share its base, or when the base
# is a field this board uses un-suffixed elsewhere. The second test is why
# `known` exists: `visible` is not a formal of any block constructor -- it
# arrives through `...` and becomes an attribute -- so neither the constructor
# nor `new_block()` can tell you it is a real field. The board can: on the board
# these were written against, `visible` appears un-suffixed on 70 of 104 blocks.
#
# Without either signal a lone `foo.1` is left alone. It might be accumulation,
# but it might equally be a field that ends in a number, and renaming that
# breaks the block.
collapse_state <- function(payload, known = character()) {

  if (!length(payload)) {
    return(payload)
  }

  nms <- names(payload)
  base <- sub("[.][0-9]+$", "", nms)

  garbage <- base != nms &
    (base %in% base[duplicated(base)] | base %in% setdiff(known, nms))

  # One entry per collapsed group: the exact name where there is one, since
  # that is what the constructor's formal argument matches; otherwise the first,
  # since the payload runs newest first.
  groups <- unique(base[garbage])

  drop <- unlist(lapply(groups, function(nm) {
    idx <- which(base == nm)
    exact <- idx[nms[idx] == nm]
    setdiff(idx, if (length(exact)) exact[[1L]] else idx[[1L]])
  }))

  keep <- setdiff(seq_along(payload), drop)

  out <- payload[keep]
  names(out) <- ifelse(garbage[keep], base[keep], nms[keep])
  out
}

# Field names this board uses un-suffixed somewhere, which is what makes a lone
# suffixed entry recognisable as accumulation.
known_fields <- function(blocks) {
  nms <- unlist(lapply(blocks, function(b) names(b[["payload"]])))
  unique(nms[nms == sub("[.][0-9]+$", "", nms)])
}

#' Collapse accumulated block state
#'
#' Saved boards accumulate duplicate payload entries. On the board this was
#' written against, 2915 of them, 55% of every entry in the file, all of them
#' `visible`.
#'
#' They are not inert. `visible_sections()` in blockr.dock reads
#' `attr(blk, "visible")`, and `attr()` partial-matches by default, so one
#' suffixed entry silently acts as the value while several make the lookup
#' ambiguous: `attr()` returns `NULL` and the block falls back to showing every
#' section. The setting is lost with no error anywhere. On that board 32 blocks
#' were ambiguous and 13 rendered differently because of it.
#'
#' `normalize_state()` collapses each run to one entry. The exact name wins
#' where there is one, because that is what the constructor's formal argument
#' matches; otherwise the first suffixed entry, because the payload runs newest
#' first. Blocks whose lookup was ambiguous get their setting back, so this
#' changes what some blocks render -- to what they were saved as.
#'
#' @inheritParams board_stats
#' @return A board.
#'
#' @examples
#' \dontrun{
#' board_lint(board)                       # what is there
#' clean <- normalize_state(board)         # collapse it
#' }
#'
#' @export
normalize_state <- function(x) {

  spec <- as_board_spec(x)

  blocks <- spec[["payload"]][["blocks"]][["payload"]]
  known <- known_fields(blocks)

  spec[["payload"]][["blocks"]][["payload"]] <- lapply(blocks, function(b) {
    b[["payload"]] <- collapse_state(b[["payload"]], known)
    b
  })

  blockr.core::blockr_deser(spec)
}

#' Put a board back on its template
#'
#' [normalize_layout()] and [normalize_state()] in one call: the shape a default
#' board is meant to have, and the state it is meant to carry. Nothing else
#' moves. No block changes column, no panel joins or leaves a view, no
#' configuration is touched.
#'
#' @inheritParams normalize_layout
#' @return A board.
#'
#' @examples
#' \dontrun{
#' clean <- normalize_board(board, layout_template(c(filters = 2, main = 6)))
#' }
#'
#' @export
normalize_board <- function(x, template = layout_template(),
                            even_nested = TRUE) {
  normalize_state(
    normalize_layout(x, template = template, even_nested = even_nested)
  )
}
