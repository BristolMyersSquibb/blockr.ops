demo <- function() {
  blockr.dock::new_dock_board(
    blocks = c(
      data = blockr.core::new_dataset_block("iris", block_name = "Source"),
      top = blockr.core::new_head_block(n = 3L, block_name = "Rows")
    ),
    links = c(l1 = blockr.core::new_link("data", "top")),
    views = list(
      v1 = blockr.dock::dock_view(
        members = c("block_panel-data", "block_panel-top"), name = "Explore"
      )
    ),
    active = "v1"
  )
}

test_that("board_stats counts what the board holds", {
  s <- board_stats(demo())
  expect_s3_class(s, "board_stats")
  expect_identical(s$blocks, 2L)
  expect_identical(s$links, 1L)
  expect_identical(s$views, 1L)
  expect_identical(unname(s$panels_per_view[["Explore"]]), 2L)
  expect_identical(names(s$panels_per_view), "Explore")
})

test_that("board_stats counts duplicate payload entries", {
  spec <- blockr.core::blockr_ser(demo())
  expect_identical(spec$payload$blocks$payload$data$payload$visible.1, NULL)
  spec$payload$blocks$payload$data$payload$visible <- "inputs"
  spec$payload$blocks$payload$data$payload$visible.1 <- "outputs"

  s <- board_stats(spec)
  expect_identical(s$payload_entries - s$distinct_entries, 1L)
})

test_that("board_stats prints without error", {
  expect_output(print(board_stats(demo())), "2 blocks")
})
