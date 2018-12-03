context("input data format")

test.df <- data.frame('names' = c('Aa aa', 'Bb_bb_001', 'Cc cc13', 'Bb_bb_001'),
                      'lat'   = c(-2.0, -1.32, 0.00, 2.123),
                      'long'  = c(-5.0, -67.32, 179.24089, -132.9),
                      'var1'  = c(20, 22, NA, 30.12),
                      'var2'  = c(12.9, -4.231, NA, NA),
                      'var3'  = c(NA, NA, NA, NA)
                       )

test_that('input file is a dataframe', {
  expect_is(test.df, 'data.frame')
})

test_that('latlongs and vars are numeric', {
  expect_is(test.df$lat, 'numeric')
  expect_is(test.df$long, 'numeric')
  expect_is(test.df$var1, 'numeric')
  expect_is(test.df$var2, 'numeric')
})

test_that('no missing names or latlongs', {
  expect_identical(test.df$names, na.omit(test.df$names))
  expect_identical(test.df$lat, na.omit(test.df$lat))
  expect_identical(test.df$long, na.omit(test.df$long))
})

test_that('latlong columns are in the correct order', {
  expect_lt(max(test.df$lat), 90)
  expect_gt(min(test.df$lat), -90)
  expect_lt(max(test.df$long), 180)
  expect_gt(min(test.df$lat), -180)
})

test_that('minimum column length (name, lat, long, >= 1 var)', {
  expect_gt(ncol(test.df), 3)
})
