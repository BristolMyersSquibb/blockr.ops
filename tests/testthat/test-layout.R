two_col <- function(sizes = c(0.3, 0.7), rail_size = 607, focus = NULL) {
  blockr.dock::new_dock_board(
    blocks = c(
      filt = blockr.core::new_dataset_block("iris", block_name = "Filter"),
      main = blockr.core::new_head_block(n = 3L, block_name = "Main")
    ),
    links = c(l1 = blockr.core::new_link("filt", "main")),
    views = list(
      v1 = blockr.dock::dock_view(
        members = c("block_panel-filt", "block_panel-main"), name = "One"
      )
    ),
    grids = list(
      v1 = blockr.dock::dock_grid(
        "block_panel-filt", "block_panel-main", sizes = sizes
      )
    ),
    active = "v1"
  )
}

tmpl <- function() {
  layout_template(c(filters = 2, main = 6), rail_position = NULL)
}

test_that("rail_twelfths converts a share to pixels", {
  expect_identical(rail_twelfths(4, viewport = 1200), 400)
  expect_identical(rail_twelfths(4, viewport = 1600), 533)
})

test_that("layout_template normalises columns to ratios", {
  t <- layout_template(c(a = 2, b = 6))
  expect_equal(unname(t$columns), c(0.25, 0.75))
  expect_identical(t$column_labels, c("a", "b"))
})

test_that("drifted column sizes are reported and fixed", {
  board <- two_col(sizes = c(0.3, 0.7))

  d <- layout_drift(board, tmpl())
  expect_identical(d$kind, "column_sizes")
  expect_identical(d$found, "30%/70%")
  expect_identical(d$wanted, "25%/75%")

  fixed <- normalize_layout(board, tmpl())
  expect_equal(
    unname(blockr.dock::board_grids(fixed)[[1]]$sizes), c(0.25, 0.75)
  )
  expect_identical(nrow(layout_drift(fixed, tmpl())), 0L)
})

test_that("a board already on the template shows no drift", {
  expect_identical(nrow(layout_drift(two_col(c(0.25, 0.75)), tmpl())), 0L)
  expect_output(print(layout_drift(two_col(c(0.25, 0.75)), tmpl())), "none")
})

test_that("a view with the wrong column count is left alone, and said so", {
  t <- layout_template(c(a = 1, b = 1, c = 1), rail_position = NULL)

  d <- layout_drift(two_col(), t)
  expect_identical(d$kind, "column_count")
  expect_match(d$wanted, "left alone")

  # the sizes it cannot interpret are not touched
  fixed <- normalize_layout(two_col(c(0.3, 0.7)), t)
  expect_equal(unname(blockr.dock::board_grids(fixed)[[1]]$sizes), c(0.3, 0.7))
})

test_that("normalize_layout moves no panel and changes no payload", {
  board <- two_col(sizes = c(0.42, 0.58))
  before <- blockr.core::blockr_ser(board)
  after <- blockr.core::blockr_ser(normalize_layout(board, tmpl()))

  expect_identical(after$payload$blocks, before$payload$blocks)
  expect_identical(after$payload$links, before$payload$links)
  expect_identical(after$payload$views, before$payload$views)
  expect_identical(after$payload$options, before$payload$options)
})

test_that("nested branch sizes are evened out, unless told not to", {
  spec <- blockr.core::blockr_ser(two_col())
  grid <- spec$payload$grids$payload$v1
  grid$children[[1]] <- list(
    children = list(list(panels = "block_panel-filt", active = "block_panel-filt"),
                    list(panels = "block_panel-main", active = "block_panel-main")),
    sizes = c(0.8, 0.2)
  )
  spec$payload$grids$payload$v1 <- grid

  expect_true("nested_sizes" %in% layout_drift(spec, tmpl())$kind)
  expect_false("nested_sizes" %in% layout_drift(spec, tmpl(), even_nested = FALSE)$kind)
})

test_that("focus is reported and dropped", {
  spec <- blockr.core::blockr_ser(two_col(c(0.25, 0.75)))
  spec$payload$grids$payload$v1$focus <- "block_panel-main"

  d <- layout_drift(spec, tmpl())
  expect_identical(d$kind, "focus")

  fixed <- blockr.core::blockr_ser(normalize_layout(spec, tmpl()))
  expect_null(fixed$payload$grids$payload$v1$focus)
})
