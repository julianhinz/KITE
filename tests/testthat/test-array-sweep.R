# file: tests/testthat/test-array-sweep.R
library(testthat)

set.seed(123)

ref_sweep <- function(...) sweep(...)

basic_dims <- c(6, 5, 4)
basic_array <- array(runif(prod(basic_dims)), basic_dims)
dimnames(basic_array) <- list(
  country = paste0("c", seq_len(basic_dims[1])),
  sector = paste0("s", seq_len(basic_dims[2])),
  partner = paste0("p", seq_len(basic_dims[3]))
)

make_stats <- function(margins, dims = basic_dims) {
  dims_sel <- dims[margins]
  array(runif(prod(dims_sel)), dims_sel)
}

test_that("array_sweep matches base sweep across ops", {
  ops <- c("+", "-", "*", "/")
  for (op in ops) {
    stats <- make_stats(c(1, 3))
    expect_equal(
      array_sweep(basic_array, MARGIN = c("country", "partner"), STATS = stats, FUN = op),
      ref_sweep(basic_array, MARGIN = c("country", "partner"), STATS = stats, FUN = op),
      tolerance = 1e-12
    )
  }
})

test_that("scalar and recycled stats work", {
  stats_scalar <- 2
  expect_equal(
    array_sweep(basic_array, MARGIN = c(1, 2), STATS = stats_scalar, FUN = "*"),
    ref_sweep(basic_array, MARGIN = c(1, 2), STATS = stats_scalar, FUN = "*")
  )

  stats_vec <- runif(basic_dims[1])
  expect_equal(
    array_sweep(basic_array, MARGIN = 1, STATS = stats_vec, FUN = "+"),
    ref_sweep(basic_array, MARGIN = 1, STATS = stats_vec, FUN = "+"),
    tolerance = 1e-12
  )
})

test_that("char margins require named dimnames", {
  x <- array(1:8, dim = c(2, 2, 2))
  expect_error(
    array_sweep(x, MARGIN = "missing", STATS = 1, FUN = "*"),
    "dimnames"
  )

  dimnames(x) <- list(a = c("a1", "a2"), b = c("b1", "b2"), c = c("c1", "c2"))
  expect_error(
    array_sweep(x, MARGIN = "not_there", STATS = 1, FUN = "*"),
    "match"
  )
})

test_that("invalid inputs raise informative errors", {
  x <- array(0, c(4, 3, 2))
  stats_bad_dim <- array(0, c(5, 2))
  expect_error(
    array_sweep(x, c(1, 3), stats_bad_dim, "*"),
    "dim\\(STATS\\)"
  )
  expect_error(
    array_sweep(x, c(1, 1), array(1, c(4, 2)), "*"),
    "repeat"
  )
  expect_error(
    array_sweep(x, c(0, 2), array(1, c(4, 2)), "*"),
    "bounds"
  )
})

test_that("array_sum collapses dimensions to those kept", {
  arr <- array(1:24, dim = c(2, 3, 4))
  expect_equal(array_sum(arr, keep_dims = c(1, 2)),
               array(apply(arr, c(1, 2), sum), dim = c(2, 3)))
  expect_equal(array_sum(arr, keep_dims = 3),
               array(apply(arr, 3, sum), dim = 4))
})
