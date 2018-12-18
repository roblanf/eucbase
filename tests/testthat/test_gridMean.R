context("gridMean")
library(eucbase)

## INPUTS ##
ex.outG     <- gridMean(ex.in,  100)
ex.outG2    <- gridMean(ex.in,  43.324975)
ex.ext.outG <- gridMean(ex.ext, 100)
ex.NA.outG  <- gridMean(ex.NA,  100)
ex.out.sg   <- gridMean(ex.in,  0.5)

## BUGS TESTED ##
test_that('calling km value with quotes gives error', {
  expect_error(gridMean(ex.in, "100"), "non-numeric argument to binary operator")
})

test_that('calling data with quotes gives error', {
  expect_error(gridMean("ex.in", 100), "attempt to set 'colnames' on an object with less than two dimensions")
})

test_that('small grid sizes prints output', {
  expect_output(gridMean(ex.in,     0.5), "please specify a larger grid size!", all = F)
  expect_output(gridMean(ex.in.spp, 0.1), "please specify a larger grid size!", all = F)
})

test_that('no error when input file has only one latlong', {
  expect_silent(suppressWarnings(gridMean(ex.in.n1, 100)))
})

test_that('no error when colnames differ', {
  expect_silent(suppressWarnings(gridMean(ex.in.c1, 100)))
})

test_that('species with all layer NAs prints output', {
  expect_output(gridMean(ex.NA, 100), "Corymbia calophylla does not have layer values. NAs will be output.")
})

test_that('species with all layer NAs is output as a single row', {
  expect_equal((length(unique(as.character(ex.NA$Species)))), length(unique(as.character(ex.NA.outG$Species))))
})

test_that('all layer fields arent output as NaN', {
  expect_false(all(is.na(ex.outG   [,5:ncol(ex.outG)])))
  expect_false(all(is.na(ex.outG2   [,5:ncol(ex.outG2)])))
  expect_false(all(is.na(ex.ext.outG   [,5:ncol(ex.ext.outG)])))
  expect_false(all(is.na(ex.NA.outG   [,5:ncol(ex.NA.outG)])))
  expect_false(all(is.na(ex.out.sg   [,5:ncol(ex.out.sg)])))
})

## TESTING OUTPUTS ARE CORRECT ##
test_that('data.frame is output', {
  expect_is(ex.outG,     'data.frame')
  expect_is(ex.outG2,    'data.frame')
  expect_is(ex.ext.outG, 'data.frame')
  expect_is(ex.NA.outG,  'data.frame')
  expect_is(ex.out.sg,   'data.frame')
})

test_that('all species are output (if no species = all layer NAs)', {
  expect_equal(sort(unique(as.character(ex.in $Species))), sort(unique(as.character(ex.outG    $Species))))
  expect_equal(sort(unique(as.character(ex.in $Species))), sort(unique(as.character(ex.outG2   $Species))))
  expect_equal(sort(unique(as.character(ex.ext$Species))), sort(unique(as.character(ex.ext.outG$Species))))
  expect_equal(sort(unique(as.character(ex.in $Species))), sort(unique(as.character(ex.out.sg  $Species))))
})

test_that('one column per variable is output', {
  expect_equal(length(ex.in[ ,4:ncol(ex.in)]), length(ex.outG[ ,5:ncol(ex.outG)]))
  expect_equal(ncol(ex.in), (ncol(ex.outG))-1)
  
  expect_equal(length(ex.in[ ,4:ncol(ex.in)]), length(ex.outG[ ,5:ncol(ex.outG2)]))
  expect_equal(ncol(ex.in), (ncol(ex.outG2))-1)
  
  expect_equal(length(ex.ext[ ,4:ncol(ex.in)]), length(ex.ext.outG[ ,5:ncol(ex.ext.outG)]))
  expect_equal(ncol(ex.ext), (ncol(ex.ext.outG))-1)
  
  expect_equal(length(ex.NA[ ,4:ncol(ex.in)]), length(ex.NA.outG[ ,5:ncol(ex.NA.outG)]))
  expect_equal(ncol(ex.NA), (ncol(ex.NA.outG))-1)
  
  expect_equal(length(ex.NA[ ,4:ncol(ex.in)]), length(ex.NA.outG[ ,5:ncol(ex.out.sg)]))
  expect_equal(ncol(ex.NA), (ncol(ex.out.sg))-1)
  
})
