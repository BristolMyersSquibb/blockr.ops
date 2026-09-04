test_that("collapse_state keeps the exact name over the suffixed ones", {
  p <- list(table = "lb", visible = "outputs", visible.1 = "inputs", n = 1L)
  out <- blockr.ops:::collapse_state(p)
  expect_identical(names(out), c("table", "visible", "n"))
  expect_identical(out$visible, "outputs")
})

test_that("collapse_state takes the first suffixed entry when none is exact", {
  # The payload runs newest first, so `visible.3` is the current value.
  p <- list(script = "x", visible.3 = "a", visible.2 = "b", visible.1 = "c")
  out <- blockr.ops:::collapse_state(p)
  expect_identical(names(out), c("script", "visible"))
  expect_identical(out$visible, "a")
})

test_that("collapse_state leaves a clean payload alone", {
  p <- list(a = 1L, b = "x")
  expect_identical(blockr.ops:::collapse_state(p), p)
})

test_that("collapse_state handles an empty payload", {
  expect_identical(blockr.ops:::collapse_state(list()), list())
})
