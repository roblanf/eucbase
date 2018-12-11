context("nTest")

test_that('return the same dataframe when there are more than 1 rows', {
  expect_equal(ex.in, nTest(ex.in))
})

test_that('return a two duplicate rows when there is only one row', {
  expect_equal(rbind(ex.in.n1, ex.in.n1), nTest(ex.in.n1))
})