context("ras")

e.n1 <- setExt(ex.in)
ex.ras.outb.n1 <- rasMean(ex.in.n1, km = 100, e.n1)
ex.rasSD.outb.n1 <- rasMean(ex.in.n1, km = 100, e.n1)

#ex.ras.outb <- rasMean(ex.in.spp, 1) good error, but crashes testing??

test_that('raster is output', {
  expect_is(rasMean(ex.in.spp, 100), 'RasterBrick')
  expect_is(rasMean(ex.in.spp, 12.9823744), 'RasterBrick')
  expect_is(rasSD(ex.in.spp, 100), 'RasterBrick')
  expect_is(rasSD(ex.in.spp, 12.9823744), 'RasterBrick')
})

#test_that('small grid sizes give a blank dataframe??', {
#  expect_output(print(ex.ras.outb), "data frame with 0 columns and 0 rows")
#})

test_that('no output produced when Species n = 1', {
  expect_output(sum(ex.ras.outb.n1@data@values), NA)
  expect_output(sum(ex.rasSD.outb.n1@data@values), NA)
})
