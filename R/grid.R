# A view's geometry, back into the `dock_grid()` authoring DSL.
#
# A persisted `dock_grid` has exactly five fields: `orientation`, `children`,
# `sizes`, `focus` and `rails`. The DSL expresses four of them exactly, which
# is why the layout needs no approximation and no restriction: measured against
# a ten-view production board, generated `dock_grid()` source reproduces every
# view's geometry exactly.
#
# `focus` is the fifth, and it is deliberately dropped. It is dockView's
# `activeGroup`, the group that last held keyboard focus; at runtime it only
# feeds where a newly added panel lands, and that already falls back to the
# last group. It is NOT which tab is open -- that is `active` on the leaf,
# which is emitted.

grid_leaf_src <- function(node) {

  ids <- node[["panels"]]
  active <- node[["active"]]

  # A single panel whose tab is the open one is just its id.
  if (length(ids) == 1L && (is.null(active) || identical(active, ids[[1L]]))) {
    return(deparse(ids))
  }

  args <- vapply(ids, deparse, character(1))

  if (!is.null(active) && !identical(active, ids[[1L]])) {
    args <- c(args, paste0("active = ", deparse(active)))
  }

  paste0("blockr.dock::panels(\n", indent(paste(args, collapse = ",\n")), "\n)")
}

grid_sizes_src <- function(sizes) {

  if (!length(sizes)) {
    return(character())
  }

  paste0("sizes = c(", paste(sprintf("%.4f", sizes), collapse = ", "), ")")
}

grid_node_src <- function(node) {

  if (!is.null(node[["panels"]])) {
    return(grid_leaf_src(node))
  }

  args <- c(
    vapply(node[["children"]], grid_node_src, character(1)),
    grid_sizes_src(node[["sizes"]])
  )

  paste0("blockr.dock::group(\n", indent(paste(args, collapse = ",\n")), "\n)")
}

grid_rail_src <- function(rail) {

  panels <- unlist(rail[["panels"]])

  args <- if (length(panels)) vapply(panels, deparse, character(1)) else character()
  args <- c(args, paste0("position = ", deparse(rail[["position"]])))

  if (!is.null(rail[["active"]])) {
    args <- c(args, paste0("active = ", deparse(rail[["active"]])))
  }

  args <- c(
    args,
    paste0("collapsed = ", rail[["collapsed"]]),
    paste0("size = ", rail[["size"]]),
    paste0("collapsed_size = ", rail[["collapsed_size"]])
  )

  paste0("blockr.dock::rail(\n", indent(paste(args, collapse = ",\n")), "\n)")
}

grid_src <- function(grid) {

  args <- c(
    vapply(grid[["children"]], grid_node_src, character(1)),
    vapply(grid[["rails"]], grid_rail_src, character(1)),
    paste0("orientation = ", deparse(grid[["orientation"]])),
    grid_sizes_src(grid[["sizes"]])
  )

  paste0("blockr.dock::dock_grid(\n", indent(paste(args, collapse = ",\n")), "\n)")
}
