spec_with <- function(...) {
  spec <- blockr.core::blockr_ser(
    blockr.dock::new_dock_board(
      blocks = c(
        data = blockr.core::new_dataset_block("iris", block_name = "Source")
      )
    )
  )
  extra <- list(...)
  for (nm in names(extra)) spec$payload$blocks$payload$data$payload[[nm]] <- extra[[nm]]
  spec
}

visible_of <- function(board) attr(blockr.core::board_blocks(board)[["data"]], "visible")

test_that("the exact name wins over the suffixed ones", {
  spec <- spec_with(visible = "outputs", visible.1 = "inputs")

  clean <- normalize_state(spec)
  expect_identical(visible_of(clean), "outputs")

  p <- blockr.core::blockr_ser(clean)$payload$blocks$payload$data$payload
  expect_identical(sum(grepl("^visible", names(p))), 1L)
})

test_that("with no exact name the first suffixed entry is taken", {
  # the payload runs newest first, so `visible.3` is the current value
  spec <- spec_with(visible.3 = "inputs", visible.2 = "outputs", visible.1 = "x")
  expect_identical(visible_of(normalize_state(spec)), "inputs")
})

test_that("a lone suffixed entry is left alone when nothing names the field", {
  # `foo.1` on a board where no block has a `foo` could equally be a field that
  # ends in a number. Renaming it would break the block, so it is not touched.
  spec <- spec_with(foo.1 = "x")

  p <- blockr.core::blockr_ser(normalize_state(spec))$payload$blocks$payload$data$payload
  expect_true("foo.1" %in% names(p))
  expect_identical(nrow(board_lint(spec)), 0L)
})

test_that("an ambiguous block gets its setting back", {
  spec <- spec_with(visible.2 = "inputs", visible.1 = "inputs")

  # several suffixed entries and no exact one: attr() cannot resolve it
  broken <- blockr.core::blockr_deser(spec)
  expect_null(attr(blockr.core::board_blocks(broken)[["data"]], "visible"))

  expect_identical(visible_of(normalize_state(spec)), "inputs")
})

test_that("normalize_state leaves a clean board alone", {
  # built through the constructor, so the payload is in its natural order
  spec <- blockr.core::blockr_ser(
    blockr.dock::new_dock_board(
      blocks = c(
        data = blockr.core::new_dataset_block(
          "iris", block_name = "Source", visible = "inputs"
        )
      )
    )
  )

  expect_identical(
    blockr.core::blockr_ser(normalize_state(spec))$payload$blocks,
    spec$payload$blocks
  )
})

test_that("normalize_state changes nothing but the block payloads", {
  spec <- spec_with(visible = "inputs", visible.1 = "outputs")
  after <- blockr.core::blockr_ser(normalize_state(spec))

  for (f in c("links", "stacks", "views", "grids", "options", "extensions")) {
    expect_identical(after$payload[[f]], spec$payload[[f]], info = f)
  }
})

test_that("normalize_board does both halves", {
  board <- blockr.dock::new_dock_board(
    blocks = c(
      data = blockr.core::new_dataset_block("iris", block_name = "Source"),
      # carries `visible`, which is what makes the stray entry below
      # recognisable as accumulation rather than a field ending in a number
      top = blockr.core::new_head_block(n = 3L, block_name = "Rows",
                                        visible = "inputs")
    ),
    links = c(l1 = blockr.core::new_link("data", "top")),
    views = list(
      v1 = blockr.dock::dock_view(
        members = c("block_panel-data", "block_panel-top"), name = "One"
      )
    ),
    grids = list(
      v1 = blockr.dock::dock_grid("block_panel-data", "block_panel-top",
                                  sizes = c(0.9, 0.1))
    ),
    active = "v1"
  )

  spec <- blockr.core::blockr_ser(board)
  spec$payload$blocks$payload$data$payload$visible.1 <- "outputs"

  t <- layout_template(c(a = 1, b = 3), rail_position = NULL)

  expect_true(nrow(layout_drift(spec, t)) > 0L)
  expect_true(nrow(board_lint(spec)) > 0L)

  clean <- normalize_board(spec, t)

  expect_identical(nrow(layout_drift(clean, t)), 0L)
  expect_identical(nrow(board_lint(clean)), 0L)
})
