context("speciesMean")

# Good
sink("sink")
ex.out <- speciesMean(ex.in, 100)
sink(NULL)

# Grid sizes are too small, resulting in all variables for 1 species == NAs
#ex.outb <- speciesMean(ex.in, 0)
#ex.outb1 <- speciesMean(ex.in, 1)

test_that('data.frame is output', {
  expect_is(ex.out, 'data.frame')
})

test_that('one row per species is output', {
  expect_equal(length(unique(ex.in$Species)), nrow(ex.out))
})

test_that('one column per variable is output', {
  expect_equal(length(ex.in[4:ncol(ex.in)]), length(ex.out[2:ncol(ex.out)]))
  expect_equal(length(ncol(ex.in)), length(ncol(ex.out)-2))
})