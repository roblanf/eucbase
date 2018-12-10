context("rasMean")

ex.ras.out <- rasMean(ex.in.spp, 100)
ex.ras.out2 <- rasMean(ex.in.spp, 12.9823744)

#ex.ras.outb <- rasMean(ex.in.spp, 1) good error, but crashes testing??

test_that('raster is output', {
  expect_is(ex.ras.out, 'RasterBrick')
  expect_is(ex.ras.out2, 'RasterBrick')
})

#test_that('small grid sizes give a blank dataframe??', {
#  expect_output(print(ex.ras.outb), "data frame with 0 columns and 0 rows")
#})
