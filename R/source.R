# Turning values back into source. Nothing clever: every payload value in a
# saved board is a literal (no functions, environments or raw vectors), so
# `deparse()` covers it. The two shapes worth special handling are long
# character vectors -- a code block's `script` is one element per source line,
# and deparsing it at width gives an unreadable blob -- and argument names that
# are not syntactic.

indent <- function(txt, n = 2L) {
  paste(
    paste0(strrep(" ", n), strsplit(txt, "\n", fixed = TRUE)[[1L]]),
    collapse = "\n"
  )
}

is_syntactic <- function(x) {
  grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", x) & make.names(x) == x
}

name_src <- function(x) {
  if (is_syntactic(x)) x else paste0("`", x, "`")
}

value_src <- function(v) {

  if (is.character(v) && length(v) > 3L && is.null(names(v))) {
    return(
      paste0(
        "c(\n",
        indent(paste(vapply(v, deparse, character(1)), collapse = ",\n")),
        "\n)"
      )
    )
  }

  paste(deparse(v, width.cutoff = 70L), collapse = "\n")
}

args_src <- function(payload, extra = character()) {

  parts <- c(
    vapply(
      names(payload),
      function(nm) paste0(name_src(nm), " = ", value_src(payload[[nm]])),
      character(1)
    ),
    extra
  )

  if (!length(parts)) "" else paste(parts, collapse = ",\n")
}

# `blockr_deser()` resolves a constructor with `get0(name, asNamespace(pkg))`,
# which reaches unexported functions. Generated source cannot use `::` for
# those, so it falls back to `:::` -- and a `:::` call also has to be told its
# own identity, or the object records itself as a serialized closure (kilobytes
# of R bytecode, pinned to an R version) instead of a name and a package.
# Blocks name that argument `ctor_pkg`; board options name it `pkg`.
#
# A `:::` in the output is a finding, not a feature: it means a constructor a
# board can store is not part of any package's API.
is_exported <- function(ctor) {
  !is.null(
    tryCatch(
      getExportedValue(ctor[["package"]], ctor[["constructor"]]),
      error = function(e) NULL
    )
  )
}

ctor_ref <- function(ctor) {
  paste0(
    ctor[["package"]], if (is_exported(ctor)) "::" else ":::",
    ctor[["constructor"]]
  )
}

ctor_identity_args <- function(ctor, pkg_arg = "ctor_pkg") {

  if (is_exported(ctor)) {
    return(character())
  }

  c(
    paste0("ctor = ", deparse(ctor[["constructor"]])),
    paste0(pkg_arg, " = ", deparse(ctor[["package"]]))
  )
}

ctor_call_src <- function(ctor, payload, pkg_arg = "ctor_pkg") {

  args <- args_src(payload, ctor_identity_args(ctor, pkg_arg))

  if (!nzchar(args)) {
    return(paste0(ctor_ref(ctor), "()"))
  }

  paste0(ctor_ref(ctor), "(\n", indent(args), "\n)")
}

named_list_src <- function(call, items, nms) {
  paste0(
    call, "(\n",
    paste(
      unname(Map(function(nm, src) indent(paste0(name_src(nm), " = ", src)),
                 nms, items)),
      collapse = ",\n"
    ),
    "\n)"
  )
}
