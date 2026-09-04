demo_spec <- function() {
  blockr.core::blockr_ser(
    blockr.dock::new_dock_board(
      blocks = c(
        data = blockr.core::new_dataset_block("iris", block_name = "Source")
      )
    )
  )
}

test_that("a clean board lints clean", {
  issues <- board_lint(demo_spec())
  expect_s3_class(issues, "board_lint")
  expect_identical(nrow(issues), 0L)
  expect_output(print(issues), "nothing found")
})

test_that("several suffixed entries with no exact name are ambiguous", {
  spec <- demo_spec()
  spec$payload$blocks$payload$data$payload$visible.2 <- "inputs"
  spec$payload$blocks$payload$data$payload$visible.1 <- "outputs"

  issues <- board_lint(spec)
  expect_identical(issues$kind, "ambiguous_visible")
  expect_identical(issues$id, "block data")
  expect_match(issues$detail, "ambiguous")
})

test_that("an exact name alongside suffixed ones is only duplication", {
  spec <- demo_spec()
  spec$payload$blocks$payload$data$payload$visible <- "inputs"
  spec$payload$blocks$payload$data$payload$visible.1 <- "outputs"

  expect_identical(board_lint(spec)$kind, "duplicate_state")
})

test_that("an unexported constructor is reported", {
  spec <- demo_spec()
  spec$payload$blocks$payload$data$constructor$constructor <- "no_such_ctor"

  issues <- board_lint(spec)
  expect_true("unexported_ctor" %in% issues$kind)
})

test_that("a constructor stored as a closure is reported", {
  spec <- demo_spec()
  spec$payload$blocks$payload$data$constructor$package <- NULL
  spec$payload$blocks$payload$data$constructor$constructor <- as.raw(1:16)

  issues <- board_lint(spec)
  expect_true("serialized_ctor" %in% issues$kind)
  expect_match(issues$detail[issues$kind == "serialized_ctor"], "16 bytes")
})

test_that("a non-literal payload value is reported", {
  spec <- demo_spec()
  spec$payload$blocks$payload$data$payload$fn <- function(x) x

  expect_true("non_literal_payload" %in% board_lint(spec)$kind)
})
