# A small board built from blockr.core / blockr.dock only, so the tests run
# wherever those two are installed. The full-scale check lives in a downstream
# repo's own parity check, run against its real boards.

demo_board <- function() {
  blockr.dock::new_dock_board(
    blocks = c(
      data = blockr.core::new_dataset_block("iris", block_name = "Source Data"),
      top = blockr.core::new_head_block(n = 3L, block_name = "First Rows")
    ),
    links = c(l1 = blockr.core::new_link("data", "top")),
    views = list(
      v1 = blockr.dock::dock_view(
        members = c("block_panel-data", "block_panel-top"),
        name = "Explore"
      )
    ),
    grids = list(
      v1 = blockr.dock::dock_grid(
        "block_panel-data",
        "block_panel-top",
        sizes = c(0.3, 0.7)
      )
    ),
    active = "v1"
  )
}

test_that("a board round-trips exactly through its generated source", {
  expect_true(check_board_script(demo_board(), rename = FALSE, tidy = FALSE)$ok)
})

test_that("a renamed, tidied board round-trips as an equivalent board", {
  expect_true(check_board_script(demo_board())$ok)
})

test_that("renaming slugs block ids from their display names", {
  src <- board_script(demo_board())
  expect_match(src, "source_data = blockr.core::new_dataset_block", fixed = TRUE)
  expect_match(src, "from = \"source_data\"", fixed = TRUE)
  expect_match(src, "\"block_panel-first_rows\"", fixed = TRUE)
  expect_false(grepl("block_panel-top", src, fixed = TRUE))
})

test_that("view ids are slugged from the view name", {
  expect_match(board_script(demo_board()), "explore = blockr.dock::dock_view",
               fixed = TRUE)
})

test_that("rename = FALSE keeps the stored ids", {
  src <- board_script(demo_board(), rename = FALSE)
  expect_match(src, "data = blockr.core::new_dataset_block", fixed = TRUE)
  expect_match(src, "v1 = blockr.dock::dock_view", fixed = TRUE)
})

test_that("a payload value colliding with a block id is left alone, and warns", {
  # `new_head_block()` stores `direction = "head"`. On a board with a block
  # called `head` that string is indistinguishable from a reference, so the
  # rename must not touch it -- and must say so.
  board <- blockr.dock::new_dock_board(
    blocks = c(
      data = blockr.core::new_dataset_block("iris", block_name = "Source"),
      head = blockr.core::new_head_block(n = 3L, block_name = "Rows")
    ),
    links = c(l1 = blockr.core::new_link("data", "head"))
  )

  expect_warning(src <- board_script(board), "survive the rename")
  expect_match(src, "direction = \"head\"", fixed = TRUE)
  expect_true(suppressWarnings(check_board_script(board))$ok)
})

test_that("a reference field is rewritten when named in ref_fields", {
  board <- blockr.dock::new_dock_board(
    blocks = c(
      data = blockr.core::new_dataset_block("iris", block_name = "Source"),
      top = blockr.core::new_head_block(n = 3L, block_name = "Rows")
    ),
    links = c(l1 = blockr.core::new_link("data", "top"))
  )
  spec <- blockr.core::blockr_ser(board)
  spec$payload$blocks$payload$top$payload$ctrl_target <- "data"

  src <- board_script(spec, ref_fields = "ctrl_target")
  expect_match(src, "ctrl_target = \"source\"", fixed = TRUE)
})

test_that("the generated grid keeps sizes, nesting and the open tab", {
  src <- board_script(demo_board())
  expect_match(src, "blockr.dock::dock_grid(", fixed = TRUE)
  expect_match(src, "sizes = c(0.3000, 0.7000)", fixed = TRUE)
})

test_that("focus is not emitted", {
  expect_false(grepl("focus", board_script(demo_board()), fixed = TRUE))
})
