# context("String length")
library(testthat)
library(windfarmGA)

test_that("BaroHoehe works", {
  data <- matrix(seq(0,5000,500));  
  expect_s3_class(BaroHoehe(data), "data.frame")
})
