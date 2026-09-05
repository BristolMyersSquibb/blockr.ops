#' Check that a board survives the round trip
#'
#' Generates the construction source for a board, evaluates it, and compares
#' the result against the original. This is the point of the package: run it
#' over every board you store and it fails the day one starts carrying state
#' that no constructor argument can restore.
#'
#' With `rename = FALSE` and `tidy = FALSE` the comparison is exact: the
#' re-serialized board must be `identical()` to the stored one, component by
#' component. The one allowance is `focus`, which the generated script never
#' emits (see `grid_src()`), so it is stripped from both sides before the grids
#' are compared. With the defaults the ids have changed on purpose, so the check
#' is equivalence instead -- same block set, same payloads once ids are
#' remapped, same link topology, same view membership.
#'
#' @inheritParams board_script
#' @return A list with `ok` and one entry per compared component. Printed as a
#'   short report.
#'
#' @examples
#' \dontrun{
#' res <- check_board_script(board)
#' stopifnot(res$ok)
#' }
#'
#' @export
check_board_script <- function(x, rename = TRUE, tidy = TRUE) {

  spec <- as_board_spec(x)
  exact <- !rename && !tidy

  src <- board_script(
    spec, name = "..board..", rename = rename, tidy = tidy, group = !exact
  )

  env <- new.env(parent = globalenv())
  eval(parse(text = src), envir = env)
  rebuilt <- blockr.core::blockr_ser(env[["..board.."]]())

  res <- if (exact) {
    exact_report(spec, rebuilt)
  } else {
    equivalence_report(spec, rebuilt, tidy)
  }

  structure(c(list(ok = all(unlist(res))), res), class = "board_script_check")
}

exact_report <- function(spec, rebuilt) {

  # `focus` is never emitted, so it cannot come back. Compare the grids without
  # it rather than pretend the round trip failed.
  drop_focus <- function(x) {
    x[["payload"]][["grids"]][["payload"]] <- lapply(
      x[["payload"]][["grids"]][["payload"]],
      function(g) { g[["focus"]] <- NULL; g }
    )
    x[["payload"]]
  }

  a <- drop_focus(spec)
  b <- drop_focus(rebuilt)

  stats::setNames(
    lapply(names(a), function(f) identical(a[[f]], b[[f]])),
    names(a)
  )
}

equivalence_report <- function(spec, rebuilt, tidy) {

  b1 <- spec[["payload"]][["blocks"]][["payload"]]
  b2 <- rebuilt[["payload"]][["blocks"]][["payload"]]
  bmap <- block_id_map(b1)

  ctors <- function(b) vapply(b, function(x) x[["constructor"]][["constructor"]],
                              character(1))
  t1 <- stats::setNames(unname(ctors(b1)), unname(bmap[names(b1)]))
  t2 <- ctors(b2)

  payloads <- function(b) {
    known <- known_fields(b)
    lapply(b, function(x) {
      p <- if (isTRUE(tidy)) collapse_state(x[["payload"]], known) else x[["payload"]]
      p[sort(names(p))]
    })
  }
  p1 <- lapply(payloads(b1), remap_payload, map = bmap)
  names(p1) <- unname(bmap[names(b1)])
  p2 <- payloads(b2)

  l1 <- link_frame(spec, bmap)
  l2 <- link_frame(rebuilt, NULL)

  v1 <- view_members(spec, bmap)
  v2 <- view_members(rebuilt, NULL)

  list(
    blocks = identical(t1[sort(names(t1))], t2[sort(names(t2))]),
    payloads = identical(p1[sort(names(p1))], p2[sort(names(p2))]),
    links = identical(l1, l2),
    views = identical(v1, v2),
    options = identical(
      spec[["payload"]][["options"]], rebuilt[["payload"]][["options"]]
    )
  )
}

link_frame <- function(spec, bmap) {

  links <- spec[["payload"]][["links"]][["payload"]]

  if (!length(links)) {
    return(character())
  }

  out <- vapply(links, function(l) {
    p <- l[["payload"]]
    from <- if (is.null(bmap)) p[["from"]] else unname(bmap[p[["from"]]])
    to <- if (is.null(bmap)) p[["to"]] else unname(bmap[p[["to"]]])
    paste(from, to, p[["input"]], sep = " -> ")
  }, character(1))

  sort(unname(out))
}

view_members <- function(spec, bmap) {

  views <- spec[["payload"]][["views"]][["payload"]][["views"]]

  out <- lapply(views, function(v) {
    m <- unlist(v[["payload"]])
    sort(if (is.null(bmap)) m else remap_panels(m, bmap))
  })

  names(out) <- unlist(Map(view_label, views, names(views)))
  out[sort(names(out))]
}

#' @export
print.board_script_check <- function(x, ...) {

  cat("board script round trip:", if (isTRUE(x[["ok"]])) "ok" else "FAILED", "\n")

  for (nm in setdiff(names(x), "ok")) {
    cat(sprintf("  %-10s %s\n", nm, if (isTRUE(x[[nm]])) "ok" else "differs"))
  }

  invisible(x)
}
