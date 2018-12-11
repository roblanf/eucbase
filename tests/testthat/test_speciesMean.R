context("speciesMean")

ex.outS <- speciesMean(ex.in, 100)
ex.outS2 <- speciesMean(ex.in, 43.324975)

ex.NA.out <- speciesMean(ex.NA, 100)

# Grid sizes are too small, resulting in all variables for 1 species == NAs
#ex.outSb <- speciesMean(ex.in, 1)
#ex.outSb1 <- speciesMean(ex.in, 1)

test_that('all layer fields produce NAs if a single species contains all NA layers', {
  expect_output(print(sum(ex.NA.out[,-1])), "NaN")
})

test_that('calling km value with quotes gives error', {
  expect_error(speciesMean(ex.in, "100"), "non-numeric argument to binary operator")
})

test_that('calling data with quotes gives error', {
  expect_error(speciesMean("ex.in", 100), "incorrect number of dimensions")
})

test_that('data.frame is output', {
  expect_is(ex.outS, 'data.frame')
  expect_is(ex.outS2, 'data.frame')
})

test_that('one row per species is output', {
  expect_equal(length(unique(ex.in$Species)), nrow(ex.outS))
  expect_equal(length(unique(ex.in$Species)), length(unique(ex.outS$Species)))
  
  expect_equal(length(unique(ex.in$Species)), nrow(ex.outS2))
  expect_equal(length(unique(ex.in$Species)), length(unique(ex.outS2$Species)))
})

test_that('column classes are correct', {
  expect_is(ex.outS[ , 1], 'character')
  expect_is(sum(ex.outS[2:nrow(ex.outS), 2:ncol(ex.outS)], na.rm = T), 'numeric')
  
  expect_is(ex.outS2[ , 1], 'character')
  expect_is(sum(ex.outS2[2:nrow(ex.outS2), 2:ncol(ex.outS2)], na.rm = T), 'numeric')
})

test_that('one column per variable is output', {
  expect_equal(length(ex.in[ ,4:ncol(ex.in)]), length(ex.outS[ ,2:ncol(ex.outS)]))
  expect_equal(ncol(ex.in), ncol(ex.outS)+2)
  
  expect_equal(length(ex.in[ ,4:ncol(ex.in)]), length(ex.outS2[ ,2:ncol(ex.outS2)]))
  expect_equal(ncol(ex.in), ncol(ex.outS2)+2)
})
