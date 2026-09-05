#' A canonical view layout
#'
#' The shape every view of a default board is meant to have: some columns in
#' the splitview, and a rail pinned to one edge. `columns` is in twelfths, the
#' way you would say it out loud ("two for filters, six for content"); what a
#' grid stores is ratios, so they are normalised.
#'
#' The rail is in pixels, not twelfths, because that is what a `dock_grid`
#' stores: a rail sits outside the splitview and does not take a share of it.
#' `rail_twelfths()` converts against an assumed viewport if you would rather
#' state it the same way as the columns.
#'
#' @param columns Named numeric, the splitview columns in twelfths. Names are
#'   for reading; only the ratios are used.
#' @param rail_position Edge the rail pins to, or `NULL` for no rail.
#' @param rail_size,rail_collapsed_size Rail width in pixels, open and collapsed.
#' @param rail_collapsed Whether the rail opens collapsed.
#' @param twelfths,viewport For `rail_twelfths()`, the share and the viewport
#'   width to convert against.
#'
#' @return A `layout_template`.
#'
#' @examples
#' # two columns of filters, six of content, four for the rail on a 1600px
#' # viewport
#' layout_template(
#'   columns = c(filters = 2, main = 6),
#'   rail_size = rail_twelfths(4)
#' )
#'
#' @export
layout_template <- function(columns = c(filters = 2, main = 6),
                            rail_position = "right",
                            rail_size = 600,
                            rail_collapsed_size = 35,
                            rail_collapsed = FALSE) {

  stopifnot(is.numeric(columns), length(columns) > 0L, all(columns > 0))

  structure(
    list(
      columns = columns / sum(columns),
      column_labels = names(columns),
      rail_position = rail_position,
      rail_size = rail_size,
      rail_collapsed_size = rail_collapsed_size,
      rail_collapsed = rail_collapsed
    ),
    class = "layout_template"
  )
}

#' @rdname layout_template
#' @export
rail_twelfths <- function(twelfths, viewport = 1600) {
  round(viewport * twelfths / 12)
}

#' @export
print.layout_template <- function(x, ...) {

  cat("layout template\n")

  cat("  columns  ", paste(sprintf(
    "%s %.0f%%", x[["column_labels"]], 100 * x[["columns"]]
  ), collapse = ", "), "\n")

  if (is.null(x[["rail_position"]])) {
    cat("  rail      none\n")
  } else {
    cat(sprintf("  rail      %s, %gpx (%gpx collapsed)%s\n",
                x[["rail_position"]], x[["rail_size"]],
                x[["rail_collapsed_size"]],
                if (isTRUE(x[["rail_collapsed"]])) ", opens collapsed" else ""))
  }

  invisible(x)
}

#' Layout drift, and how to remove it
#'
#' A board that people have used has been dragged about. Sash positions, rail
#' widths and the focused group are all stored, so a default board slowly stops
#' being the shape it was designed as, one drag at a time. On the board these
#' were written against, ten views intended to share one column ratio had ten
#' different ones.
#'
#' `layout_drift()` reports it. `normalize_layout()` removes it, and touches
#' nothing else: no block moves between columns, no panel joins or leaves a
#' view, no payload changes. Only the numbers.
#'
#' A view whose column count does not match the template is left alone and
#' reported. That is deliberate: a view with three columns where the template
#' says two is more likely to be a considered exception than drift, and
#' flattening it silently would be the wrong call.
#'
#' @param x A board, or the list `blockr.core::blockr_ser()` produces for one.
#' @param template A [layout_template()].
#' @param even_nested Even out the sizes of branches nested inside a column.
#'   The template says nothing about those, so they are drift by the same
#'   argument, but set this `FALSE` to leave them.
#'
#' @return `layout_drift()` a data frame with `view`, `kind`, `found` and
#'   `wanted`, one row per finding. `normalize_layout()` a board.
#'
#' @examples
#' \dontrun{
#' template <- layout_template(c(filters = 2, main = 6), rail_size = 533)
#' layout_drift(board, template)
#' clean <- normalize_layout(board, template)
#' }
#'
#' @export
layout_drift <- function(x, template = layout_template(), even_nested = TRUE) {

  spec <- as_board_spec(x)
  grids <- spec[["payload"]][["grids"]][["payload"]]
  views <- spec[["payload"]][["views"]][["payload"]][["views"]]

  labels <- unlist(Map(view_label, views, names(views)))

  found <- unlist(
    lapply(names(grids), function(gid) {
      drift_of(grids[[gid]], labels[[gid]], template, even_nested)
    }),
    recursive = FALSE
  )

  out <- if (length(found)) {
    data.frame(
      view = vapply(found, `[[`, character(1), "view"),
      kind = vapply(found, `[[`, character(1), "kind"),
      found = vapply(found, `[[`, character(1), "found"),
      wanted = vapply(found, `[[`, character(1), "wanted"),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(view = character(), kind = character(), found = character(),
               wanted = character(), stringsAsFactors = FALSE)
  }

  structure(out, class = c("layout_drift", "data.frame"))
}

pct <- function(x) paste(sprintf("%.0f%%", 100 * x), collapse = "/")

drift_of <- function(grid, view, template, even_nested) {

  out <- list()
  note <- function(kind, found, wanted) {
    list(view = view, kind = kind, found = found, wanted = wanted)
  }

  wanted <- template[["columns"]]

  if (length(grid[["children"]]) != length(wanted)) {

    out <- c(out, list(note(
      "column_count", paste(length(grid[["children"]]), "columns"),
      paste(length(wanted), "columns; left alone")
    )))

  } else if (!isTRUE(all.equal(as.numeric(grid[["sizes"]]), as.numeric(wanted),
                               tolerance = 1e-3))) {

    out <- c(out, list(note("column_sizes", pct(grid[["sizes"]]), pct(wanted))))
  }

  if (not_null(grid[["focus"]])) {
    out <- c(out, list(note("focus", grid[["focus"]], "dropped")))
  }

  if (isTRUE(even_nested)) {
    out <- c(out, lapply(uneven_branches(grid[["children"]]), function(sizes) {
      note("nested_sizes", pct(sizes), pct(even_shares(length(sizes))))
    }))
  }

  c(out, rail_drift(grid, view, template))
}

rail_drift <- function(grid, view, template) {

  note <- function(kind, found, wanted) {
    list(view = view, kind = kind, found = found, wanted = wanted)
  }

  pos <- template[["rail_position"]]

  if (is.null(pos)) {
    return(list())
  }

  rail <- grid[["rails"]][[pos]]

  if (is.null(rail)) {
    return(list())
  }

  out <- list()

  fields <- list(
    size = template[["rail_size"]],
    collapsed_size = template[["rail_collapsed_size"]],
    collapsed = template[["rail_collapsed"]]
  )

  for (nm in names(fields)) {
    if (!isTRUE(all.equal(rail[[nm]], fields[[nm]]))) {
      out <- c(out, list(note(
        paste0("rail_", nm), as.character(rail[[nm]]),
        as.character(fields[[nm]])
      )))
    }
  }

  out
}

even_shares <- function(n) if (n) rep(1 / n, n) else numeric()

uneven_branches <- function(children) {

  out <- list()

  for (node in children) {

    if (!is.null(node[["panels"]])) {
      next
    }

    sizes <- node[["sizes"]]

    if (length(sizes) &&
          !isTRUE(all.equal(as.numeric(sizes), even_shares(length(sizes)),
                            tolerance = 1e-3))) {
      out <- c(out, list(sizes))
    }

    out <- c(out, uneven_branches(node[["children"]]))
  }

  out
}

#' @export
print.layout_drift <- function(x, ...) {

  if (!nrow(x)) {
    cat("layout drift: none\n")
    return(invisible(x))
  }

  cat("layout drift:", nrow(x), "findings across",
      length(unique(x[["view"]])), "views\n\n")

  for (i in seq_len(nrow(x))) {
    cat(sprintf("  %-18s %-16s %-16s -> %s\n", x[["view"]][[i]],
                x[["kind"]][[i]], x[["found"]][[i]], x[["wanted"]][[i]]))
  }

  invisible(x)
}

#' @rdname layout_drift
#' @export
normalize_layout <- function(x, template = layout_template(),
                             even_nested = TRUE) {

  spec <- as_board_spec(x)
  grids <- spec[["payload"]][["grids"]][["payload"]]

  spec[["payload"]][["grids"]][["payload"]] <- lapply(
    grids, normalize_grid, template = template, even_nested = even_nested
  )

  blockr.core::blockr_deser(spec)
}

normalize_grid <- function(grid, template, even_nested) {

  wanted <- template[["columns"]]

  # A view whose column count does not match is an exception, not drift.
  if (length(grid[["children"]]) == length(wanted)) {
    grid[["sizes"]] <- as.numeric(wanted)
  }

  if (isTRUE(even_nested)) {
    grid[["children"]] <- lapply(grid[["children"]], even_out)
  }

  grid[["focus"]] <- NULL

  pos <- template[["rail_position"]]

  if (not_null(pos) && !is.null(grid[["rails"]][[pos]])) {
    grid[["rails"]][[pos]][["size"]] <- template[["rail_size"]]
    grid[["rails"]][[pos]][["collapsed_size"]] <-
      template[["rail_collapsed_size"]]
    grid[["rails"]][[pos]][["collapsed"]] <- template[["rail_collapsed"]]
  }

  grid
}

even_out <- function(node) {

  if (!is.null(node[["panels"]])) {
    return(node)
  }

  node[["children"]] <- lapply(node[["children"]], even_out)
  node[["sizes"]] <- even_shares(length(node[["children"]]))
  node
}

not_null <- function(x) !is.null(x)
