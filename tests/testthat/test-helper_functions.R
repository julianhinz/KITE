library(data.table)

cast_variable <- KITE:::cast_variable
melt_variable <- KITE:::melt_variable

#' Helper to create simple elasticity data
create_elasticity_dt <- function(values) {
  data.table(sector = names(values), value = as.numeric(values))
}

test_that("cast_variable handles lists of data.tables", {
  elasticities <- list(trade_elasticity = create_elasticity_dt(c(A = -4, B = -6)))

  casted <- cast_variable(elasticities)

  expect_type(casted, "list")
  expect_true(is.array(casted$trade_elasticity))
  expect_equal(dim(casted$trade_elasticity), c(2))
  expect_equal(dimnames(casted$trade_elasticity)[[1]], c("A", "B"))
  expect_equal(as.numeric(casted$trade_elasticity), c(-4, -6))
})

test_that("melt_variable handles lists of arrays", {
  elasticity_array <- array(c(-3, -5), dim = c(2), dimnames = list(sector = c("X", "Y")))
  elasticities <- list(trade_elasticity = elasticity_array)

  melted <- melt_variable(elasticities)

  expect_type(melted, "list")
  expect_true(is.data.table(melted$trade_elasticity))
  expect_equal(names(melted$trade_elasticity), c("sector", "value"))
  expect_equal(melted$trade_elasticity$sector, c("X", "Y"))
  expect_equal(melted$trade_elasticity$value, c(-3, -5))
})
