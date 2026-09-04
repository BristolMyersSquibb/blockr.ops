#' What a board holds
#'
#' Counts, for when you have more boards than you can open. Blocks by package
#' and by type, links, views with their panel counts, stacks, extensions and
#' options, plus the size of the serialized payload.
#'
#' @param x A board, or the list `blockr.core::blockr_ser()` produces for one.
#' @return A `board_stats` list. Printed as a short report.
#'
#' @examples
#' \dontrun{
#' board_stats(read_board("board.json"))
#' }
#'
#' @export
board_stats <- function(x) {

  spec <- as_board_spec(x)
  p <- spec[["payload"]]

  blocks <- p[["blocks"]][["payload"]]
  views <- p[["views"]][["payload"]][["views"]]

  pkgs <- vapply(blocks, function(b) b[["constructor"]][["package"]], character(1))
  types <- vapply(blocks, function(b) b[["constructor"]][["constructor"]],
                  character(1))

  panels <- vapply(views, function(v) length(unlist(v[["payload"]])), integer(1))
  names(panels) <- unlist(Map(view_label, views, names(views)))

  # Per block, then summed. Pooling the names across blocks would count two
  # blocks' `block_name` as one duplicate, which is not what is being measured.
  entries <- vapply(blocks, function(b) length(b[["payload"]]), integer(1))
  distinct <- vapply(
    blocks,
    function(b) length(unique(sub("[.][0-9]+$", "", names(b[["payload"]])))),
    integer(1)
  )

  structure(
    list(
      blocks = length(blocks),
      links = length(p[["links"]][["payload"]]),
      views = length(views),
      stacks = length(p[["stacks"]][["payload"]]),
      extensions = length(p[["extensions"]][["payload"]]),
      options = length(p[["options"]][["payload"]]),
      packages = sort(table(pkgs), decreasing = TRUE),
      types = sort(table(types), decreasing = TRUE),
      panels_per_view = panels,
      payload_entries = sum(entries),
      distinct_entries = sum(distinct),
      board_name = board_name_of(p)
    ),
    class = "board_stats"
  )
}

board_name_of <- function(payload) {

  for (o in payload[["options"]][["payload"]]) {
    if (identical(o[["object"]][[1L]], "board_name_option")) {
      value <- o[["payload"]][["value"]]
      if (length(value) && nzchar(value[[1L]])) {
        return(value[[1L]])
      }
    }
  }

  NA_character_
}

#' @export
print.board_stats <- function(x, ...) {

  if (!is.na(x[["board_name"]])) {
    cat("board ", x[["board_name"]], "\n\n", sep = "")
  }

  cat(sprintf(
    "%d blocks, %d links, %d views, %d stacks, %d extensions, %d options\n",
    x[["blocks"]], x[["links"]], x[["views"]], x[["stacks"]],
    x[["extensions"]], x[["options"]]
  ))

  bloat <- x[["payload_entries"]] - x[["distinct_entries"]]

  cat(sprintf(
    "%d payload entries, %d distinct%s\n",
    x[["payload_entries"]], x[["distinct_entries"]],
    if (bloat > 0L) {
      sprintf(" (%d duplicated, %.0f%%; see board_lint())",
              bloat, 100 * bloat / x[["payload_entries"]])
    } else {
      ""
    }
  ))

  cat("\nblocks by package\n")
  print_counts(x[["packages"]])

  cat("\npanels per view\n")
  print_counts(x[["panels_per_view"]])

  invisible(x)
}

print_counts <- function(counts, n = 8L) {

  keep <- utils::head(counts, n)

  for (i in seq_along(keep)) {
    cat(sprintf("  %-28s %4d\n", names(keep)[[i]], as.integer(keep[[i]])))
  }

  if (length(counts) > n) {
    cat(sprintf("  %-28s %4d\n", paste0("(", length(counts) - n, " more)"),
                sum(as.integer(counts[-seq_len(n)]))))
  }

  invisible(NULL)
}
