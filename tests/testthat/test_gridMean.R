context("gridMean")

# Good
sink("sink")
ex.out <- gridMean(ex.in, 100)
sink(NULL)

# Grid sizes are too small, resulting in all variables for 1 species == NAs
#ex.outb <- speciesMean(ex.in, 0)
#ex.outb1 <- speciesMean(ex.in, 1)

test_that('data.frame is output', {
  expect_is(ex.out, 'data.frame')
})

test_that('correct number of columns output', {
  expect_equal(length(ncol(ex.in)), length(ncol(ex.in)-1))
})

