context("Check main arguments")

test_that("bin_choose must have numeric input values", {
  expect_error(bin_choose("puppy", 5))
})

test_that("bin_choose cannot have a k input greater than its n input", {
  expect_error(bin_choose(5, 10))
})

test_that("bin_choose must return a numeric value", {
  expect_is(bin_choose(10, 5), "numeric")
})

test_that("bin_probability must have numeric input values", {
  expect_error(bin_probability("cat", 5))
})

test_that("bin_probability must return a numeric value", {
  expect_is(bin_probability(10, 5, 0.5), "numeric")
})

test_that("bin_probability must have 3 input values", {
  expect_error(bin_probability(10, 5))
  expect_error(bin_probability(10, 5, 5, 10))
})

test_that("bin_distribution must have 2 input values", {
  expect_error(bin_distribution(10, 5, 0.5))
  expect_error(bin_distribution(10))
})

test_that("bin_distribution must have numeric input values", {
  expect_error(bin_distribution("monkey", 5))
})

test_that("bin_cumulative must have 2 input values", {
  expect_error(bin_cumulative(10, 5, 0.5))
})

test_that("bin_cumulative must have numeric input values", {
  expect_error(bin_cumulative("elephant", 5))
  expect_error(bin_cumulative("giraffe", 5))
})

