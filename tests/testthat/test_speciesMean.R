context("speciesMean")

ex.outS <- speciesMean(ex.in, 100)
ex.outS2 <- speciesMean(ex.in, 43.324975)
ex.ext.out <- speciesMean(ex.ext, 100)
ex.NA.out <- speciesMean(ex.NA, 100) # Output rows of NA, write test for this

# Grid sizes are too small, resulting in all variables for 1 species == NAs
#ex.outSb <- speciesMean(ex.in, 1)
#ex.outSb1 <- speciesMean(ex.in, 1)


test_that('calling km value with quotes gives error', {
  expect_error(speciesMean(ex.in, "100"), "non-numeric argument to binary operator")
})

test_that('calling data with quotes gives error', {
  expect_error(speciesMean("ex.in", 100), "incorrect number of dimensions")
})

test_that('all layer fields arent output as NaN', {
  expect_lt(sum(is.na(ex.outS[,-1])), nrow(ex.outS[,-1]) * ncol(ex.outS[,-1]))
  expect_lt(sum(is.na(ex.outS2[,-1])), nrow(ex.outS2[,-1]) * ncol(ex.outS2[,-1]))
  expect_lt(sum(is.na(ex.ext.out[,-1])), nrow(ex.ext.out[,-1]) * ncol(ex.ext.out[,-1]))
  expect_lt(sum(is.na(ex.NA.out[,-1])), nrow(ex.NA.out[,-1]) * ncol(ex.NA.out[,-1]))
})

test_that('data.frame is output', {
  expect_is(ex.outS, 'data.frame')
  expect_is(ex.outS2, 'data.frame')
  expect_is(ex.ext.out, 'data.frame')
  expect_is(ex.NA.out, 'data.frame')
})

test_that('one row per species is output', {
  expect_equal(length(unique(ex.in$Species)), nrow(ex.outS))
  expect_equal(length(unique(ex.in$Species)), length(unique(ex.outS$Species)))
  
  expect_equal(length(unique(ex.in$Species)), nrow(ex.outS2))
  expect_equal(length(unique(ex.in$Species)), length(unique(ex.outS2$Species)))
  
  expect_equal(length(unique(ex.ext$Species)), length(unique(ex.ext.out$Species)))
  expect_equal(length(unique(ex.ext$Species)), length(unique(ex.ext.out$Species)))
  
  expect_equal(length(unique(ex.NA$Species)), length(unique(ex.NA.out$Species)))
  expect_equal(length(unique(ex.NA$Species)), length(unique(ex.NA.out$Species)))
})

test_that('column classes are correct', {
  expect_is(ex.outS[ , 1], 'character')
  expect_is(sum(ex.outS[2:nrow(ex.outS), 2:ncol(ex.outS)], na.rm = T), 'numeric')
  
  expect_is(ex.outS2[ , 1], 'character')
  expect_is(sum(ex.outS2[2:nrow(ex.outS2), 2:ncol(ex.outS2)], na.rm = T), 'numeric')
  
  expect_is(ex.ext.out[ , 1], 'character')
  expect_is(sum(ex.ext.out[2:nrow(ex.outS2), 2:ncol(ex.outS2)], na.rm = T), 'numeric')
  
  expect_is(ex.NA.out[ , 1], 'character')
  expect_is(sum(ex.NA.out[2:nrow(ex.NA.out), 2:ncol(ex.NA.out)], na.rm = T), 'numeric')
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
