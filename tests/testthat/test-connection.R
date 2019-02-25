context("check db connection")

test_that("null entries for `connect_pg()`", {

  expect_error(pgtools::connect_pg(getenv = NULL, verbose = NULL))

})


test_that("conflict for getenv specified", {

  expect_success(connect_pg(getenv = TRUE, host = Sys.getenv("db_ip")))

})