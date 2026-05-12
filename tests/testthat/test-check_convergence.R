# Test file for check_convergence function
library(testthat)

test_that("check_convergence handles basic functionality", {
  old_vals <- c(1.0, 2.0, 3.0, 4.0)
  new_vals <- c(1.01, 2.02, 2.99, 4.01)
  
  # Test that all methods return finite positive values
  expect_true(is.finite(check_convergence(new_vals, old_vals, method = "root_mean_square")))
  expect_true(is.finite(check_convergence(new_vals, old_vals, method = "aggregate")))
  expect_true(is.finite(check_convergence(new_vals, old_vals, method = "element_wise")))
  expect_true(is.finite(check_convergence(new_vals, old_vals, method = "sample")))
  
  # Test that convergence values are non-negative
  expect_gte(check_convergence(new_vals, old_vals, method = "root_mean_square"), 0)
  expect_gte(check_convergence(new_vals, old_vals, method = "aggregate"), 0)
  expect_gte(check_convergence(new_vals, old_vals, method = "element_wise"), 0)
  expect_gte(check_convergence(new_vals, old_vals, method = "sample"), 0)
})

test_that("check_convergence handles input validation", {
  # Test length mismatch
  expect_error(
    check_convergence(c(1, 2), c(1, 2, 3)),
    "new_values and old_values must have the same length"
  )
  
  # Test invalid method
  expect_error(
    check_convergence(c(1, 2), c(1, 2), method = "invalid"),
    "method must be one of"
  )
  
  # Test invalid sample_fraction
  expect_error(
    check_convergence(c(1, 2), c(1, 2), method = "sample", sample_fraction = 0),
    "sample_fraction must be between 0 and 1"
  )
  
  expect_error(
    check_convergence(c(1, 2), c(1, 2), method = "sample", sample_fraction = 1.5),
    "sample_fraction must be between 0 and 1"
  )
})

test_that("check_convergence handles edge cases", {
  # Test identical values (perfect convergence)
  expect_equal(check_convergence(c(1, 2, 3), c(1, 2, 3)), 0, tolerance = 1e-10)
  expect_equal(check_convergence(c(1, 2, 3), c(1, 2, 3), method = "aggregate"), 0, tolerance = 1e-10)
  expect_equal(check_convergence(c(1, 2, 3), c(1, 2, 3), method = "element_wise"), 0, tolerance = 1e-10)
  expect_equal(check_convergence(c(1, 2, 3), c(1, 2, 3), method = "sample"), 0, tolerance = 1e-10)
  
  # Test zero values
  expect_true(is.finite(check_convergence(c(0, 0, 0), c(1, 1, 1))))
  expect_true(is.finite(check_convergence(c(1, 1, 1), c(0, 0, 0))))
  expect_equal(check_convergence(c(0, 0, 0), c(0, 0, 0)), 0, tolerance = 1e-10)
  
  # Test single element
  expect_true(is.finite(check_convergence(1.1, 1.0)))
  expect_equal(check_convergence(1.0, 1.0), 0, tolerance = 1e-10)
  
  # Test very small values
  small_vals <- c(1e-10, 2e-10, 3e-10)
  slightly_diff <- c(1.01e-10, 2.01e-10, 2.99e-10)
  expect_true(is.finite(check_convergence(slightly_diff, small_vals)))
})

test_that("check_convergence handles NA values correctly", {
  # Test NA handling with na.rm = TRUE behavior
  old_vals <- c(1, 2, NA, 4)
  new_vals <- c(1.1, 2.1, NA, 4.1)
  
  # All methods should handle NAs gracefully
  expect_true(is.finite(check_convergence(new_vals, old_vals, method = "root_mean_square")))
  expect_true(is.finite(check_convergence(new_vals, old_vals, method = "element_wise")))
  expect_true(is.finite(check_convergence(new_vals, old_vals, method = "sample")))
  
  # Test case where only NAs are present
  all_na_old <- c(NA, NA, NA)
  all_na_new <- c(NA, NA, NA)
  expect_true(is.finite(check_convergence(all_na_new, all_na_old, method = "root_mean_square")))
})

test_that("check_convergence methods give expected relative ordering", {
  # Create test data where methods should show different behavior
  old_vals <- c(1, 10, 100, 1000)
  new_vals <- c(1.1, 10.01, 99.9, 1001)  # Different relative changes
  
  rms <- check_convergence(new_vals, old_vals, method = "root_mean_square")
  agg <- check_convergence(new_vals, old_vals, method = "aggregate")
  elem <- check_convergence(new_vals, old_vals, method = "element_wise")
  
  # Element-wise should be largest (max of relative changes)
  # RMS should be between element-wise and aggregate
  expect_gte(elem, rms)
  
  # All should be positive for different values
  expect_gt(rms, 0)
  expect_gt(agg, 0)
  expect_gt(elem, 0)
})

test_that("check_convergence sample method behaves correctly", {
  # Large vector to test sampling
  set.seed(123)  # For reproducible test
  n <- 1000
  old_vals <- rnorm(n, mean = 100, sd = 10)
  new_vals <- old_vals + rnorm(n, mean = 0, sd = 0.1)  # Small changes
  
  # Test different sample fractions
  sample_10pct <- check_convergence(new_vals, old_vals, method = "sample", sample_fraction = 0.1)
  sample_50pct <- check_convergence(new_vals, old_vals, method = "sample", sample_fraction = 0.5)
  
  # Both should be finite
  expect_true(is.finite(sample_10pct))
  expect_true(is.finite(sample_50pct))
  
  # Test minimum sample size
  small_vals <- c(1, 2, 3, 4, 5)
  small_new <- c(1.1, 2.1, 3.1, 4.1, 5.1)
  
  # With min_sample = 10, should fall back to element-wise for small vectors
  sample_result <- check_convergence(small_new, small_vals, method = "sample", min_sample = 10)
  element_result <- check_convergence(small_new, small_vals, method = "element_wise")
  
  # Should be identical when fallback occurs
  expect_equal(sample_result, element_result, tolerance = 1e-10)
})

test_that("check_convergence handles different data types", {
  # Test with matrices converted to vectors
  old_matrix <- matrix(1:12, nrow = 3, ncol = 4)
  new_matrix <- old_matrix + 0.01
  
  # Should handle matrix input by converting to vectors
  expect_true(is.finite(check_convergence(new_matrix, old_matrix)))
  
  # Test with arrays
  old_array <- array(1:24, dim = c(2, 3, 4))
  new_array <- old_array + 0.01
  
  expect_true(is.finite(check_convergence(new_array, old_array)))
})

test_that("check_convergence performance characteristics", {
  # Test that sampling method is faster for large arrays
  # This is more of a smoke test than a strict performance test
  n <- 10000
  old_vals <- rnorm(n)
  new_vals <- old_vals + rnorm(n, sd = 0.01)
  
  # All methods should complete without error on large data
  expect_true(is.finite(check_convergence(new_vals, old_vals, method = "aggregate")))
  expect_true(is.finite(check_convergence(new_vals, old_vals, method = "root_mean_square")))
  expect_true(is.finite(check_convergence(new_vals, old_vals, method = "element_wise")))
  expect_true(is.finite(check_convergence(new_vals, old_vals, method = "sample", sample_fraction = 0.01)))
})

test_that("check_convergence mathematical correctness", {
  # Test mathematical properties
  old_vals <- c(1, 2, 3, 4, 5)
  
  # Same proportional changes should give same relative change
  new_vals1 <- c(2, 4, 6, 8, 10)  # Each element doubled
  new_vals2 <- c(20, 40, 60, 80, 100)  # Each element doubled from scaled base
  
  conv1 <- check_convergence(new_vals1, old_vals, method = "aggregate")
  conv2 <- check_convergence(new_vals2, old_vals * 10, method = "aggregate")
  
  # Should be identical for aggregate method (same relative change)
  expect_equal(conv1, conv2, tolerance = 1e-10)
  
  # Test that larger absolute differences give larger convergence measures
  small_diff <- old_vals + 0.01
  large_diff <- old_vals + 0.1
  
  conv_small <- check_convergence(small_diff, old_vals, method = "root_mean_square")
  conv_large <- check_convergence(large_diff, old_vals, method = "root_mean_square")
  
  expect_lt(conv_small, conv_large)
})