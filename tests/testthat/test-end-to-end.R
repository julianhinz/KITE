library(testthat)
library(data.table)

test_that("the shipped example.R runs all models end-to-end", {
  installed <- system.file("examples", "example.R", package = "KITE")
  source_path <- if (installed != "") installed else file.path("..", "..", "inst", "examples", "example.R")
  skip_if_not(file.exists(source_path), paste("example.R not found at", source_path))

  env <- new.env()
  source(source_path, local = env)
  expect_true(exists("run_kite_example", envir = env))

  res <- env$run_kite_example()
  expect_named(res, c("cp", "chkw", "mw", "cp_summary", "chkw_summary", "mw_summary"))
  expect_s3_class(res$cp, "caliendo_parro_2015")
  expect_s3_class(res$cp, "kite_result")
  expect_s3_class(res$chkw, "chowdhry_hinz_kamin_wanner_2022")
  expect_s3_class(res$chkw, "kite_result")
  expect_s3_class(res$mw, "mahlkow_wanner_2021")
  expect_true(isTRUE(res$mw$info$convergence))
  expect_true(all(res$mw_summary$output$emissions_change$value < 1 |
                    !res$mw_summary$output$emissions_change$country %in% c("c1", "c3")))
})
