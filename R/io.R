#' Read and write board files
#'
#' A board file is JSON, and reading it wrong is easy to do quietly.
#' `jsonlite::fromJSON()` simplifies by default: a one-element list becomes a
#' scalar, a list of equal-length lists becomes a data frame, and a payload that
#' went in as a list comes back as something `blockr_deser()` cannot rebuild. The
#' failure is not always an error; sometimes the board just loads wrong.
#' `read_board()` passes the arguments that keep the structure intact.
#'
#' `write_board()` writes pretty by default, because a board file that diffs is
#' the point of keeping it in version control, and `null = "null"` so an option
#' holding `NULL` survives the trip.
#'
#' @param file Path to a board JSON file.
#' @param x A board.
#' @param pretty Indent the output. Keep this on for anything in git.
#' @param ... Passed to `jsonlite::fromJSON()` / `jsonlite::toJSON()`.
#'
#' @return `read_board()` a board; `write_board()` its `file`, invisibly.
#'
#' @examples
#' \dontrun{
#' board <- read_board("board.json")
#' write_board(board, "board.json")
#' }
#'
#' @export
read_board <- function(file, ...) {

  check_jsonlite()

  blockr.core::blockr_deser(
    jsonlite::fromJSON(
      file, simplifyVector = FALSE, simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE, ...
    )
  )
}

#' @rdname read_board
#' @export
write_board <- function(x, file, pretty = TRUE, ...) {

  check_jsonlite()

  writeLines(
    jsonlite::toJSON(
      blockr.core::blockr_ser(x), auto_unbox = TRUE, null = "null",
      pretty = pretty, digits = NA, ...
    ),
    file
  )

  invisible(file)
}

check_jsonlite <- function() {

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("`jsonlite` is needed to read and write board files.", call. = FALSE)
  }

  invisible(NULL)
}
