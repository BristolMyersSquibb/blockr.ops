

demo <- function() {
  blockr.dock::new_dock_board(
    blocks = c(
      data = blockr.core::new_dataset_block("iris", block_name = "Source"),
      top = blockr.core::new_head_block(n = 3L, block_name = "Rows")
    ),
    links = c(l1 = blockr.core::new_link("data", "top")),
    views = list(
      v1 = blockr.dock::dock_view(
        members = c("block_panel-data", "block_panel-top"), name = "One"
      )
    ),
    grids = list(
      v1 = blockr.dock::dock_grid("block_panel-data", "block_panel-top",
                                  sizes = c(0.3, 0.7))
    ),
    active = "v1"
  )
}

test_that("a board survives write_board() then read_board()", {
  f <- withr::local_tempfile(fileext = ".json")
  write_board(demo(), f)

  expect_identical(
    blockr.core::blockr_ser(read_board(f)), blockr.core::blockr_ser(demo())
  )
})

test_that("the round trip keeps the layout, not just the blocks", {
  f <- withr::local_tempfile(fileext = ".json")
  write_board(demo(), f)

  grid <- blockr.dock::board_grids(read_board(f))[[1]]
  expect_equal(unname(grid$sizes), c(0.3, 0.7))
})

test_that("an empty typed vector survives, which plain JSON cannot do", {
  # Through jsonlite a character(0) comes back NULL. On a real board that
  # silently retyped three blocks; typedjson annotates it and it round-trips.
  f <- withr::local_tempfile(fileext = ".json")
  board <- demo()
  spec <- blockr.core::blockr_ser(board)
  spec$payload$blocks$payload$data$payload$empty <- character()

  typedjson::json_write(spec, f)
  back <- blockr.core::blockr_ser(blockr.core::blockr_deser(typedjson::json_read(f)))

  expect_identical(back$payload$blocks$payload$data$payload$empty, character())
})

test_that("read_board still reads plain JSON written before the switch", {
  skip_if_not_installed("jsonlite")
  f <- withr::local_tempfile(fileext = ".json")
  writeLines(
    jsonlite::toJSON(blockr.core::blockr_ser(demo()), auto_unbox = TRUE,
                     null = "null", pretty = TRUE, digits = NA),
    f
  )
  expect_s3_class(read_board(f), "dock_board")
})

test_that("write_board writes pretty by default", {
  f <- withr::local_tempfile(fileext = ".json")
  write_board(demo(), f)
  expect_gt(length(readLines(f)), 50L)

  g <- withr::local_tempfile(fileext = ".json")
  write_board(demo(), g, pretty = FALSE)
  expect_identical(length(readLines(g)), 1L)
})
