demo <- function() {
  blockr.dock::new_dock_board(
    blocks = c(
      data = blockr.core::new_dataset_block("iris", block_name = "Source"),
      top = blockr.core::new_head_block(n = 3L, block_name = "Rows")
    ),
    links = c(l1 = blockr.core::new_link("data", "top"))
  )
}

# Everything the generated source calls is either a constructor the board
# carries or one of the emitter's own framework calls. Splitting them by where
# they come from rather than by how they are named keeps this exact.
spec_ctors <- function(spec) {
  collect <- function(x) {

    if (!is.list(x)) {
      return(NULL)
    }

    ctor <- if ("constructor" %in% names(x)) x[["constructor"]]

    c(
      if (is.list(ctor) && is.character(ctor[["constructor"]])) {
        paste0(ctor[["package"]], "::", ctor[["constructor"]])
      },
      unlist(lapply(x, collect))
    )
  }
  unique(collect(spec))
}

test_that("the emitter only writes framework calls it declares", {
  spec <- blockr.core::blockr_ser(demo())
  src <- board_script(spec)

  called <- unique(unlist(regmatches(
    src, gregexpr("blockr[.][a-z]+::[a-zA-Z_.]+", src)
  )))

  expect_true(all(setdiff(called, spec_ctors(spec)) %in% blockr.ops:::emitted_api()))
})

test_that("unresolved_api ignores calls the emitter never writes", {
  expect_identical(
    blockr.ops:::unresolved_api("blockr.dock::no_such_function()"), character()
  )
  expect_identical(
    blockr.ops:::unresolved_api("x <- blockr.core::new_link(1)"), character()
  )
})

test_that("unresolved_api agrees with what the packages actually export", {
  api <- blockr.ops:::emitted_api()
  src <- paste(paste0(api, "()"), collapse = "\n")
  expect_identical(
    blockr.ops:::unresolved_api(src),
    api[!vapply(api, blockr.ops:::resolves, logical(1))]
  )
})

test_that("a script calling an unavailable framework function warns", {
  api <- blockr.ops:::emitted_api()
  gone <- api[!vapply(api, blockr.ops:::resolves, logical(1))]

  # `rail()` is absent from some released blockr.dock versions; where every
  # declared call resolves there is nothing to warn about.
  skip_if(!length(gone), "every framework call resolves here")

  expect_warning(
    blockr.ops:::warn_unresolved(paste0(gone[[1L]], "()")), "will not run here"
  )
})
