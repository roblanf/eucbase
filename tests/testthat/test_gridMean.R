context("gridMean")

ex.outG <- gridMean(ex.in, 100)
ex.outG2 <- gridMean(ex.in, 43.324975)

test_that('calling km value with quotes gives error', {
  expect_error(gridMean(ex.in, "100"), "non-numeric argument to binary operator")
})

test_that('calling data with quotes gives error', {
  expect_error(gridMean("ex.in", 100), "incorrect number of dimensions")
})

test_that('data.frame is output', {
  expect_is(ex.outG, 'data.frame')
  expect_is(ex.outG2, 'data.frame')
})

test_that('column classes are correct', {
  expect_is(ex.outG[ , 1], 'character')
  expect_is(sum(ex.outG[2:nrow(ex.outG), 2:ncol(ex.outG)], na.rm = T), 'numeric')
  
  
  expect_is(ex.outG2[ , 1], 'character')
  expect_is(sum(ex.outG2[2:nrow(ex.outG2), 2:ncol(ex.outG2)], na.rm = T), 'numeric')
})

test_that('one column per variable is output', {
  expect_equal(length(ex.in[ ,4:ncol(ex.in)]), length(ex.outG[ ,5:ncol(ex.outG)]))
  expect_equal(ncol(ex.in), ncol(ex.outG)-1)
  
  expect_equal(length(ex.in[ ,4:ncol(ex.in)]), length(ex.outG[ ,5:ncol(ex.outG2)]))
  expect_equal(ncol(ex.in), ncol(ex.outG2)-1)
})
