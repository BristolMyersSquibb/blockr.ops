# The framework calls the emitter can write into a generated script. Nothing
# here calls them: they only ever appear inside a string, so neither `R CMD
# check` nor the reader can see that a generated script depends on them.
#
# Keeping the list explicit buys two things. `@importFrom` below makes the
# blockr.dock dependency visible to the checker, and `unresolved_api()` lets
# `board_script()` say up front that the script it just produced will not run
# here -- which is not hypothetical: `rail()` is absent from some released
# blockr.dock versions, so a board with a rail generates source that errors at
# evaluation with nothing to explain why.
#
#' @importFrom blockr.dock dock_grid dock_view new_dock_board
NULL

emitted_api <- function() {
  c(
    "blockr.dock::dock_grid",
    "blockr.dock::group",
    "blockr.dock::panels",
    "blockr.dock::rail",
    "blockr.dock::dock_view",
    "blockr.dock::new_dock_board",
    "blockr.core::new_link",
    "blockr.core::new_board_options"
  )
}

resolves <- function(ref) {

  parts <- strsplit(ref, "::", fixed = TRUE)[[1L]]

  !is.null(
    tryCatch(getExportedValue(parts[[1L]], parts[[2L]]), error = function(e) NULL)
  )
}

# Framework calls the generated source makes that the installed packages cannot
# supply. Block constructors are excluded: those come from the board and their
# packages have to be present anyway.
unresolved_api <- function(src) {

  called <- unique(unlist(regmatches(
    src, gregexpr("blockr[.][a-z]+::[a-zA-Z_.]+", src)
  )))

  framework <- intersect(called, emitted_api())

  framework[!vapply(framework, resolves, logical(1))]
}

warn_unresolved <- function(src) {

  missing <- unresolved_api(src)

  if (!length(missing)) {
    return(invisible(NULL))
  }

  warning(
    "The generated script calls ", paste(missing, collapse = ", "),
    ", which the installed packages do not export. It will not run here. ",
    "Check the blockr.dock version.",
    call. = FALSE
  )
}
