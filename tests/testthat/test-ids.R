test_that("slugify makes syntactic, lowercase names", {
  expect_identical(
    blockr.ops:::slugify(c("Sales By Region", "Cost / Unit", "  ")),
    c("sales_by_region", "cost_unit", "block")
  )
})

test_that("the id map keeps duplicate display names apart", {
  blocks <- list(
    a = list(payload = list(block_name = "Summary"), constructor = list()),
    b = list(payload = list(block_name = "Summary"), constructor = list())
  )
  expect_identical(
    unname(blockr.ops:::block_id_map(blocks)), c("summary", "summary_1")
  )
})

test_that("the id map falls back to the constructor when unnamed", {
  blocks <- list(
    a = list(payload = list(),
             constructor = list(constructor = "new_crossfilter_block"))
  )
  expect_identical(unname(blockr.ops:::block_id_map(blocks)), "crossfilter")
})

test_that("remap rewrites ids wherever they are nested", {
  map <- c(old = "new")
  expect_identical(
    blockr.ops:::remap(list(a = "old", b = list(c = c("old", "keep"))), map),
    list(a = "new", b = list(c = c("new", "keep")))
  )
})

test_that("remap_panels rewrites both the bare id and its panel id", {
  expect_identical(
    blockr.ops:::remap_panels(c("old", "block_panel-old"), c(old = "new")),
    c("new", "block_panel-new")
  )
})
