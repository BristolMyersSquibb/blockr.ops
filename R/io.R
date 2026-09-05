#' Read and write board files
#'
#' These go through 'typedjson', which is what blockr.core's own serdes plugin
#' uses (`read_json()` / `write_json()` in `plugin-serdes.R`). Reading and
#' writing a board any other way means reading and writing it differently from
#' the app.
#'
#' It also makes the round trip exact. Plain JSON cannot express an empty typed
#' vector, so through 'jsonlite' a `character(0)` comes back `NULL`; on a real
#' board that silently retyped three blocks and a grid. 'typedjson' annotates
#' what JSON cannot carry -- empty typed vectors, integer against double, typed
#' `NA`, non-finite numbers, attributes -- and leaves everything else as
#' ordinary readable JSON, so the file still diffs.
#'
#' `read_board()` also reads plain JSON, so boards written before the switch
#' still load. The reverse does not hold: 'jsonlite' cannot read a typedjson
#' board (it hands back a typed `Inf` in a form the option constructor
#' rejects), so writing one is a one-way move.
#'
#' `write_board()` writes pretty by default, because a board file that diffs is
#' the point of keeping it in version control.
#'
#' @param file Path to a board JSON file.
#' @param x A board.
#' @param pretty Indent the output. Keep this on for anything in git.
#' @param ... Passed to `typedjson::json_read()` / `typedjson::json_write()`.
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
  blockr.core::blockr_deser(typedjson::json_read(file, ...))
}

#' @rdname read_board
#' @export
write_board <- function(x, file, pretty = TRUE, ...) {
  typedjson::json_write(blockr.core::blockr_ser(x), file, pretty = pretty, ...)
  invisible(file)
}
