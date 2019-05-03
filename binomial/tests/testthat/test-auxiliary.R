context("Check summary measure arguments")

test_that("aux_mean returns a numeric value", {
  expect_is(aux_mean(10,0.5), "numeric")
})

test_that("aux_mean returns a single numeric value", {
  expect_length(aux_mean(10,0.5), 1)
})

test_that("aux_mean needs exactly two input values", {
  expect_error(aux_mean(10, 0.5, 5))
  expect_error(aux_mean(10))
})

test_that("aux_variance returns a numeric value", {
  expect_is(aux_variance(10, 0.5), "numeric")
})

test_that("aux_variance returns a single numeric value", {
  expect_length(aux_variance(10, 0.5), 1)
})

test_that("aux_variance needs exactly two input values", {
  expect_error(aux_variance(10, 0.5, 5))
  expect_error(aux_variance(10))
})

test_that("aux_mode returns an integer value", {
  expect_equal(aux_mode(10, 0.5) %% 1, 0)
})

test_that("aux_mode returns a single numeric value", {
  expect_length(aux_mode(10, 0.5), 1)
})

test_that("aux_mode needs exactly two input values", {
  expect_error(aux_mode(10, 0.5, 5))
  expect_error(aux_mode(10))
})

test_that("aux_skewness returns a numeric value", {
  expect_is(aux_mode(10, 0.5), "numeric")
})

test_that("aux_skewness returns a single numeric value", {
  expect_length(aux_skewness(10, 0.5), 1)
})

test_that("aux_skewness needs exactly two input values", {
  expect_error(aux_skewness(10, 0.5, 5))
  expect_error(aux_skewness(10))
})

test_that("aux_kurtosis returns a numeric value", {
  expect_is(aux_kurtosis(10, 0.5), "numeric")
})

test_that("aux_kurtosis returns a single numeric value", {
  expect_length(aux_kurtosis(10, 0.5), 1)
})

test_that("aux_kurtosis needs exactly two input values", {
  expect_error(aux_kurtosis(10, 0.5, 5))
  expect_error(aux_kurtosis(10))
})
