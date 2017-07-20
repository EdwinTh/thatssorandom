context("test_rd_to_wsg84")
testthat::test_that("conversion works correctly", {
  testthat::expect_equal(round(rd_to_wgs84(120700.723, 487525.501), 5),
                         c(52.37453, 4.88353))
  testthat::expect_equal(round(rd_to_wgs84(233883.131, 582065.167), 5),
                         c(53.21938, 6.56820))
})
