# Basic validation tests that can be run to verify implementation
library(testthat)

test_that("check_convergence basic validation works", {
  # Test basic functionality that should always work
  old_vals <- c(1.0, 2.0, 3.0)
  new_vals <- c(1.01, 2.01, 3.01)
  
  # Basic method tests
  expect_no_error(check_convergence(new_vals, old_vals, method = "root_mean_square"))
  expect_no_error(check_convergence(new_vals, old_vals, method = "aggregate"))
  expect_no_error(check_convergence(new_vals, old_vals, method = "element_wise"))
  expect_no_error(check_convergence(new_vals, old_vals, method = "sample"))
  
  # Perfect convergence
  expect_equal(check_convergence(old_vals, old_vals), 0, tolerance = 1e-10)
  
  # Input validation  
  expect_error(check_convergence(c(1,2), c(1,2,3)))
  expect_error(check_convergence(c(1,2), c(1,2), method="invalid"))
})