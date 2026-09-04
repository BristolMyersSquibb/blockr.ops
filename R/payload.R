# A block's serialized payload is state, not authorship, and it accumulates.
# On a large production board 54% of all payload entries were duplicate
# `visible.N` attributes; one block carries 223 of them.
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

collapse_state <- function(payload) {

  if (!length(payload)) {
    return(payload)
  }

  base <- sub("[.][0-9]+$", "", names(payload))

  keep <- vapply(
    unique(base),
    function(nm) {
      idx <- which(base == nm)
      exact <- idx[names(payload)[idx] == nm]
      if (length(exact)) exact[[1]] else idx[[1]]
    },
    integer(1)
  )

  out <- payload[sort(keep)]
  names(out) <- sub("[.][0-9]+$", "", names(out))
  out
}
