context("Basic Functions")

library(testthat)
library(windfarmGA)

test_that("Test Basic Functions", {
  
  ## euc.dist ################
  x=c(200,100); y=c(1000,2000)
  ed = euc.dist(x, y)
  expect_is(ed, "numeric")
  
  ## Control with stats::dist function
  ctrl = stats::dist(rbind(x, y))
  expect_true(round(ed) == round(ctrl))
  rm(x,y,ed,ctrl)
  
  x = cbind(runif(100, 1, 1000),runif(100, 1, 1000))
  y = cbind(runif(100, 100, 10000), runif(100, 100, 100000))
  z = cbind(x,y)
  ed = apply(z, MARGIN = 1, FUN = function(i) euc.dist(i[1:2], i[3:4]))
  expect_false(all(is.na(ed)))
  
  ## TODO
  # compare with cpp results
  
  ## PointToLine2 ################
  x <- c(100,100); y <- c(500,500);
  res <- PointToLine2(x,y,FALSE)
  expect_true(all(res[3:4] == x))
  expect_true(all(res[1:2] == y))
  expect_false(any(is.na(res)))
  rm(x, res)
  
  ## TODO
  # compare with cpp results
  
  
  ## splitAt #####################
  a <- splitAt(1:100,20)
  expect_true(length(a[[1]]) == 19)
  expect_false(any(is.na(unlist(a))))
  expect_true(all(unlist(a) %in% 1:100))
  rm(a)
  
  a <- splitAt(as.matrix(1:100),20)
  expect_true(length(a[[1]]) == 19)
  expect_false(any(is.na(unlist(a))))
  expect_true(all(unlist(a) %in% 1:100))
  rm(a)
  
})