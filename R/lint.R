#' Defects in a stored board
#'
#' A board file accumulates. Blocks are saved with whatever state they were last
#' left in, and some of what lands there is wrong in ways nothing reports at
#' runtime. `board_lint()` names them.
#'
#' The checks, each grounded in something found on a real production board:
#'
#' \describe{
#'   \item{`ambiguous_visible`}{Several `visible.N` payload entries and no exact
#'     `visible`. `attr()` partial-matches, so several candidates make the lookup
#'     ambiguous, `attr()` returns `NULL`, and the block silently falls back to
#'     showing every section. The setting is lost with no error anywhere. Not
#'     every one of these looks wrong: the fallback sometimes happens to be what
#'     the block wanted. On the board this was found on, 32 blocks were
#'     ambiguous and 13 rendered differently because of it. [board_script()]
#'     with `tidy = TRUE` writes the repaired value.}
#'   \item{`duplicate_state`}{Payload entries duplicated by base name, without
#'     the ambiguity above. Harmless but not free: on one board 54% of all
#'     entries, and every one of them lands in the file and in the diff.}
#'   \item{`unexported_ctor`}{The recorded constructor is not exported by its
#'     package. Deserialization reaches it anyway (`get0()` on the namespace),
#'     so this never fails at load; it means a constructor a board can store is
#'     not part of any package's API, and generated source has to use `:::`.}
#'   \item{`serialized_ctor`}{The constructor is stored as a serialized closure
#'     rather than a name and a package: kilobytes of R bytecode in the file,
#'     pinned to the R version that wrote it, and unreadable in a diff.}
#'   \item{`non_literal_payload`}{A payload value that is a function,
#'     environment or raw vector. Everything else is a literal, so this is the
#'     shape that cannot be written back as source.}
#' }
#'
#' @inheritParams board_stats
#' @return A data frame with `kind`, `id` and `detail`, one row per finding.
#'   Zero rows is the good case. Printed as a short report.
#'
#' @examples
#' \dontrun{
#' issues <- board_lint(read_board("board.json"))
#' subset(issues, kind == "ambiguous_visible")
#' }
#'
#' @export
board_lint <- function(x) {

  spec <- as_board_spec(x)
  p <- spec[["payload"]]

  parts <- c(
    named_parts(p[["blocks"]][["payload"]], "block"),
    named_parts(p[["extensions"]][["payload"]], "extension"),
    named_parts(p[["stacks"]][["payload"]], "stack"),
    unnamed_parts(p[["options"]][["payload"]], "option")
  )

  found <- unlist(lapply(names(parts), function(id) lint_part(parts[[id]], id)),
                  recursive = FALSE)

  out <- if (length(found)) {
    data.frame(
      kind = vapply(found, `[[`, character(1), "kind"),
      id = vapply(found, `[[`, character(1), "id"),
      detail = vapply(found, `[[`, character(1), "detail"),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(kind = character(), id = character(), detail = character(),
               stringsAsFactors = FALSE)
  }

  structure(out[order(out[["kind"]], out[["id"]]), , drop = FALSE],
            class = c("board_lint", "data.frame"))
}

named_parts <- function(x, what) {
  if (!length(x)) return(list())
  stats::setNames(x, paste0(what, " ", names(x)))
}

unnamed_parts <- function(x, what) {
  if (!length(x)) return(list())
  stats::setNames(x, paste0(what, " ", vapply(x, function(o) o[["object"]][[1L]],
                                              character(1))))
}

lint_part <- function(part, id) {

  finding <- function(kind, detail) list(kind = kind, id = id, detail = detail)

  out <- list()
  payload <- part[["payload"]]
  ctor <- part[["constructor"]]

  base <- sub("[.][0-9]+$", "", names(payload))
  dupes <- unique(base[duplicated(base)])

  for (nm in dupes) {

    n <- sum(base == nm)
    exact <- nm %in% names(payload)

    out <- c(out, list(
      if (!exact && n > 1L) {
        finding(
          "ambiguous_visible",
          sprintf("`%s` stored %d times with no exact name; attr() lookup is ambiguous", nm, n)
        )
      } else {
        finding("duplicate_state", sprintf("`%s` stored %d times", nm, n))
      }
    ))
  }

  if (length(ctor)) {

    if (is.null(ctor[["package"]])) {
      out <- c(out, list(finding(
        "serialized_ctor",
        sprintf("constructor stored as %d bytes of serialized closure",
                length(ctor[["constructor"]]))
      )))
    } else if (!is_exported(ctor)) {
      out <- c(out, list(finding(
        "unexported_ctor",
        sprintf("%s is not exported by %s", ctor[["constructor"]],
                ctor[["package"]])
      )))
    }
  }

  for (nm in names(payload)) {
    if (length(non_literal(payload[[nm]]))) {
      out <- c(out, list(finding(
        "non_literal_payload", sprintf("`%s` holds a non-literal value", nm)
      )))
    }
  }

  out
}

non_literal <- function(v) {

  if (is.function(v) || is.environment(v) || is.raw(v)) {
    return(TRUE)
  }

  if (is.list(v)) {
    return(unlist(lapply(v, non_literal)))
  }

  NULL
}

#' @export
print.board_lint <- function(x, ...) {

  if (!nrow(x)) {
    cat("board lint: nothing found\n")
    return(invisible(x))
  }

  counts <- table(x[["kind"]])

  cat("board lint:", nrow(x), "findings\n\n")

  for (kind in names(sort(counts, decreasing = TRUE))) {
    cat(sprintf("  %-20s %4d\n", kind, as.integer(counts[[kind]])))
  }

  cat("\nfirst few:\n")

  for (i in seq_len(min(5L, nrow(x)))) {
    cat(sprintf("  %-20s %-24s %s\n", x[["kind"]][[i]], x[["id"]][[i]],
                x[["detail"]][[i]]))
  }

  if (nrow(x) > 5L) {
    cat("  ... (", nrow(x) - 5L, " more; the return value is a data frame)\n",
        sep = "")
  }

  invisible(x)
}
