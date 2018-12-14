context("gridMean")

ex.outG     <- gridMean(ex.in,  100)
ex.outG2    <- gridMean(ex.in,  43.324975)
ex.ext.outG <- gridMean(ex.ext, 100)
ex.NA.outG  <- gridMean(ex.NA,  100)

test_that('calling km value with quotes gives error', {
  expect_error(gridMean(ex.in, "100"), "non-numeric argument to binary operator")
})

test_that('calling data with quotes gives error', {
  expect_error(gridMean("ex.in", 100), "attempt to set 'colnames' on an object with less than two dimensions")
})

test_that('small grid sizes do not produce an error', {
  expect_silent(gridMean(ex.in, 1))
  expect_silent(gridMean(ex.in.spp, 1))
  expect_silent(gridMean(ex.in.spp, 0.5))
})

test_that('no error when input file has only one latlong', {
  ex.in.n1.out <- gridMean(ex.in.n1, 100)
})

test_that('no error when colnames differ', {
  expect_silent(gridMean(ex.in.c1, 100))
})

test_that('data.frame is output', {
  expect_is(ex.outG,     'data.frame')
  expect_is(ex.outG2,    'data.frame')
  expect_is(ex.ext.outG, 'data.frame')
  expect_is(ex.NA.outG,  'data.frame')
})

test_that('species with all layer NAs prints output', {
  expect_output(gridMean(ex.NA, 100), "Corymbia calophylla does not have layer values. Omitting from analysis.")
})

test_that('species with all layer NAs is not output', {
  expect_equal((length(unique(as.character(ex.NA$Species))))-1, length(unique(as.character(ex.NA.outG$Species))))
})

test_that('all species are output (if no species = all layer NAs)', {
  expect_equal(sort(unique(as.character(ex.in $Species))), sort(unique(as.character(ex.outG    $Species))))
  expect_equal(sort(unique(as.character(ex.in $Species))), sort(unique(as.character(ex.outG2   $Species))))
  expect_equal(sort(unique(as.character(ex.ext$Species))), sort(unique(as.character(ex.ext.outG$Species))))
})

test_that('two columns (mean, sd) per variable is output', {
  expect_equal(length(ex.in[ ,4:ncol(ex.in)]), length(ex.outG[ ,5:ncol(ex.outG)])/2)
  expect_equal(ncol(ex.in), (ncol(ex.outG)/2)+1)
  
  expect_equal(length(ex.in[ ,4:ncol(ex.in)]), length(ex.outG[ ,5:ncol(ex.outG2)])/2)
  expect_equal(ncol(ex.in), (ncol(ex.outG2)/2)+1)
  
  expect_equal(length(ex.ext[ ,4:ncol(ex.in)]), length(ex.ext.outG[ ,5:ncol(ex.ext.outG)])/2)
  expect_equal(ncol(ex.ext), (ncol(ex.ext.outG)/2)+1)
  
  expect_equal(length(ex.NA[ ,4:ncol(ex.in)]), length(ex.NA.outG[ ,5:ncol(ex.NA.outG)])/2)
  expect_equal(ncol(ex.NA), (ncol(ex.NA.outG)/2)+1)
})
