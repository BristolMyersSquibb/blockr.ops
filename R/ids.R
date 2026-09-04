# Board ids are random slugs. They are internal handles, so the generated
# script can rename them to something a reader can follow -- but only if every
# reference moves with them. There are more references than the obvious ones:
# besides links, view members, grid panels and stack members, blocks point at
# each other through payload fields (`ctrl_target`, for one).
#
# Rewriting every payload string that happens to equal an id is NOT safe. A
# `head` block stores `direction = "head"`, and on a board with a block called
# `head` that value is indistinguishable from a reference; rewritten, the board
# no longer builds. So payload remapping is confined to `ref_fields`, and
# `dangling_ids()` then scans everything else for ids the rename left behind --
# which is what tells you a block type has a reference field this does not know
# about.

slugify <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  x <- gsub("^_+|_+$", "", x)
  ifelse(nzchar(x), x, "block")
}

# Blocks are named after `block_name`, falling back to the constructor with its
# `new_` / `_block` affixes stripped. `make.unique()` keeps two "Summary"
# blocks apart.
block_id_map <- function(blocks) {
  nm <- vapply(
    blocks,
    function(b) {
      v <- b[["payload"]][["block_name"]]
      if (is.null(v) || !length(v) || !nzchar(v[[1]])) {
        sub("_block$", "", sub("^new_", "", b[["constructor"]][["constructor"]]))
      } else {
        v[[1]]
      }
    },
    character(1)
  )
  stats::setNames(make.unique(slugify(nm), sep = "_"), names(blocks))
}

# A view built by `default_layout()` carries no name, so every reader of a
# view's label needs the same fallback.
view_label <- function(view, id = "view") {
  n <- view[["name"]]
  if (is.null(n) || !length(n) || !nzchar(n[[1]])) id else n[[1]]
}

view_id_map <- function(views) {
  nm <- unname(Map(view_label, views, names(views)))
  stats::setNames(make.unique(slugify(unlist(nm)), sep = "_"), names(views))
}

# Rewrite every string in a nested structure that names a known id.
remap <- function(x, map) {

  if (is.character(x)) {
    hit <- x %in% names(map)
    x[hit] <- unname(map[x[hit]])
    return(x)
  }

  if (is.list(x)) {
    return(lapply(x, remap, map = map))
  }

  x
}

# Payload fields that hold a block id. Passed through `board_script()` so a new
# block type with a reference field can be handled without a code change.
default_ref_fields <- function() "ctrl_target"

remap_payload <- function(payload, map, ref_fields = default_ref_fields()) {

  hit <- intersect(names(payload), ref_fields)

  for (nm in hit) {
    payload[[nm]] <- remap(payload[[nm]], map)
  }

  payload
}

# Every stored id that survives the rename, and where. Empty is the good case.
dangling_ids <- function(x, ids, path = "") {

  if (is.character(x)) {
    hit <- intersect(x, ids)
    return(if (length(hit)) stats::setNames(list(hit), path) else list())
  }

  if (!is.list(x) || !length(x)) {
    return(list())
  }

  nms <- names(x)
  keys <- if (is.null(nms)) seq_along(x) else nms

  out <- list()

  for (i in seq_along(x)) {
    out <- c(out, dangling_ids(x[[i]], ids, paste0(path, "/", keys[[i]])))
  }

  out
}

# Layout structures name blocks as `block_panel-<id>`, so the map needs both
# forms. Extension panels are left alone: their ids are already words.
remap_panels <- function(x, map) {
  panels <- stats::setNames(
    paste0("block_panel-", map), paste0("block_panel-", names(map))
  )
  remap(x, c(map, panels))
}
