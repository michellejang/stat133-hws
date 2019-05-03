context("Check prob, number of trials, and number of success arguments")

test_that("check_prob checks that prob is a number between 0 and 1", {
  expect_true(check_prob(0.5))
  expect_true(check_prob(0.001))
})

test_that("check_prob has length 1", {
  expect_error(check_prob(c(0.2, 0.3)))
})

test_that("check_prob fails if not a numeric value", {
  expect_error(check_prob("dog"))
})

test_that("check_trials fails if not a numeric value", {
  expect_error(check_trials("cat"))
})

test_that("check_trials fails if negative number", {
  expect_error(check_trials(-5))
})

test_that("check_trials must be an integer", {
  expect_true(check_trials(6))
})

test_that("check_success fails if not a numeric value", {
  expect_error(check_success("puppy", 5))
})

test_that("check_success fails if negative number",{
  expect_error(check_success(-5, 10))
})

test_that("check_success fails if greater than the number of trials", {
  expect_error(check_success(10, 5))
})