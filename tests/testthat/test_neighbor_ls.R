test_that("grid neighbors are rook-adjacent on a rectangle", {
  grid_xy <- cbind(
    ID = 1:6,
    X = c(0, 10, 20, 0, 10, 20),
    Y = c(0, 0, 0, 10, 10, 10)
  )
  nbr <- windfarmGA:::grid_neighbors(grid_xy)
  expect_setequal(nbr[["1"]], c(2L, 4L))
  expect_setequal(nbr[["5"]], c(2L, 4L, 6L))
  expect_false(3L %in% nbr[["1"]])
})

test_that("neighbor swap moves one turbine to an adjacent free cell", {
  grid_xy <- cbind(
    ID = 1:6,
    X = c(0, 10, 20, 0, 10, 20),
    Y = c(0, 0, 0, 10, 10, 10)
  )
  nbr <- windfarmGA:::grid_neighbors(grid_xy)
  set.seed(1)
  out <- windfarmGA:::neighbor_swap(c(1L, 2L, 3L), nbr, n_moves = 1L)
  expect_equal(length(unique(out)), 3L)
  expect_equal(sum(out %in% 1:3), 2L)
  moved <- setdiff(out, c(1L, 2L, 3L))
  expect_length(moved, 1L)
  expect_true(moved %in% c(4L, 5L, 6L))
  left <- setdiff(c(1L, 2L, 3L), out)
  expect_true(moved %in% nbr[[as.character(left)]])
})

test_that("a one-row-inland turbine can step onto the north row", {
  grid_xy <- cbind(
    ID = 1:6,
    X = c(0, 10, 20, 0, 10, 20),
    Y = c(0, 0, 0, 10, 10, 10)
  )
  nbr <- windfarmGA:::grid_neighbors(grid_xy)
  expect_true(6L %in% nbr[["3"]])
})
