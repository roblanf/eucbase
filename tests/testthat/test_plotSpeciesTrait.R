context("plotSpeciesTrait")

skip("too long")

file.path <-  paste(getwd(), "/plot_species_trait.png", sep = "")
#ex.plot.out <- plotSpeciesTrait(ex.in, ex.gs, "genome size (pg/2C)", 100)

ex.plot.op <- capture_output(plotSpeciesTrait(ex.in, ex.gs, "genome size (pg/2C)", 100))
ex.plot.w <- capture_warning(plotSpeciesTrait(ex.in, ex.gs, "genome size (pg/2C)", 100))

test_that('all messages are output', {
  expect_match(ex.plot.op, "Unmatched species removed. Proceeding with 8/9 species.")
  expect_match(ex.plot.op, "Loading map...")
  expect_match(ex.plot.op, "Generating raster with 100km")
  expect_match(ex.plot.op, "Calculating grid means... ")
  expect_match(ex.plot.op, "Calculating grid standard deviations... ")
  expect_match(ex.plot.op, "Calculating grid species richness... ")
  expect_match(ex.plot.op, "Writing plots to plot_species_trait.png...")
  expect_match(ex.plot.op, "Done!")
})

test_that('species columns are characters to prevent warning during joining', {
  expect_null(ex.plot.w)
})

test_that('.png is printed successfully', {
  expect_true(file_test("-f", file.path))
})
