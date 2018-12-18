context("speciesMean")

ex.outS      <- speciesMean(ex.in,    100)
ex.outS2     <- speciesMean(ex.in,    43.324975)
ex.ext.out   <- speciesMean(ex.ext,   100)
ex.NA.out    <- speciesMean(ex.NA,    100) # testthat no error when input file has only one latlong

# Grid sizes are too small, resulting in all variables for 1 species == NAs
# Issue 
#ex.outSb <- speciesMean(ex.in, 1)
#ex.outSb1 <- speciesMean(ex.in, 1)

test_that('calling km value with quotes gives error', {
  expect_error(speciesMean(ex.in, "100"), "non-numeric argument to binary operator")
})

test_that('calling data with quotes gives error', {
  expect_error(speciesMean("ex.in", 100), "attempt to set 'colnames' on an object with less than two dimensions")
})

test_that('no error when input file has only one latlong', {
  expect_silent(speciesMean(ex.in.n1, 100))
})

test_that('no error when colnames differ', {
  expect_silent(suppressWarnings(speciesMean(ex.in.c1, 100)))
})

test_that('all layer fields arent output as NaN', {
  expect_lt(sum(is.na(ex.outS   [,-1])), nrow(ex.outS   [,-1]) * ncol(ex.outS   [,-1]))
  expect_lt(sum(is.na(ex.outS2  [,-1])), nrow(ex.outS2  [,-1]) * ncol(ex.outS2  [,-1]))
  expect_lt(sum(is.na(ex.ext.out[,-1])), nrow(ex.ext.out[,-1]) * ncol(ex.ext.out[,-1]))
  expect_lt(sum(is.na(ex.NA.out [,-1])), nrow(ex.NA.out [,-1]) * ncol(ex.NA.out [,-1]))
})

test_that('data.frame is output', {
  expect_is(ex.outS,    'data.frame')
  expect_is(ex.outS2,   'data.frame')
  expect_is(ex.ext.out, 'data.frame')
  expect_is(ex.NA.out,  'data.frame')
})

test_that('species with all layer NAs prints output', {
  expect_output(speciesMean(ex.NA, 100), "Corymbia calophylla does not have layer values. Omitting from analysis.")
})

test_that('species with all layer NAs is not output', {
  expect_equal((length(unique(ex.NA$Species)))-1, length(unique(ex.NA.out$Species)))
})

test_that('one row per species is output', {
  expect_equal(length(unique(ex.in$Species)), nrow(ex.outS))
  expect_equal(length(unique(ex.in$Species)), length(unique(ex.outS$Species)))
  
  expect_equal(length(unique(ex.in$Species)), nrow(ex.outS2))
  expect_equal(length(unique(ex.in$Species)), length(unique(ex.outS2$Species)))
  
  expect_equal(length(unique(ex.ext$Species)), nrow(ex.ext.out))
  expect_equal(length(unique(ex.ext$Species)), length(unique(ex.ext.out$Species)))
  
  expect_equal(length(unique(ex.NA$Species))-1, nrow(ex.NA.out))
  expect_equal(length(unique(ex.NA$Species))-1, length(unique(ex.NA.out$Species)))
})

test_that('one column per variable is output', {
  expect_equal(length(ex.in[ ,4:ncol(ex.in)]), length(ex.outS[ ,2:ncol(ex.outS)]))
  expect_equal(ncol(ex.in), ncol(ex.outS)+2)
  
  expect_equal(length(ex.in[ ,4:ncol(ex.in)]), length(ex.outS2[ ,2:ncol(ex.outS2)]))
  expect_equal(ncol(ex.in), ncol(ex.outS2)+2)
  
  expect_equal(length(ex.ext[ ,4:ncol(ex.ext)]), length(ex.ext.out[ ,2:ncol(ex.ext.out)]))
  expect_equal(ncol(ex.ext), ncol(ex.ext.out)+2)
  
  expect_equal(length(ex.NA[ ,4:ncol(ex.NA)]), length(ex.NA.out[ ,2:ncol(ex.NA.out)]))
  expect_equal(ncol(ex.NA), ncol(ex.NA.out)+2)
})
