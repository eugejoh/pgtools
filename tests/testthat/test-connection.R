context("check db connection")

test_that("null entries for `connect_pg()`", {

  expect_error(pgtools::connect_pg(getenv = NULL, verbose = NULL))

})

