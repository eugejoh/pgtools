context("test writing options")


test_that("data frame input, bad table name (period or uppercase)", {

  expect_error(pgtools::connect_pg(getenv = NULL, verbose = NULL))

})