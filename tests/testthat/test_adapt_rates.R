test_that("operator rates explore then anneal", {
  rates0 <- list(
    phase = "explore", stall = 5L, coverage = 0.5, generation = 5L,
    teil = 1.8, mut_adapt = 0.1, p_inj = 0.25,
    mut_floor = 0.05, mut_ceil = 0.15, mut_target = 0.1
  )
  up <- do.call(windfarmGA:::adapt_operator_rates, rates0)
  expect_equal(up$phase, "explore")
  expect_gt(up$p_inj, 0.25)
  expect_gt(up$mut_adapt, 0.1)
  expect_lt(up$teil, 1.8)

  switched <- do.call(
    windfarmGA:::adapt_operator_rates,
    modifyList(rates0, list(stall = 12L, generation = 18L, coverage = 0.5))
  )
  expect_equal(switched$phase, "refine")
  expect_lt(switched$p_inj, 0.25)

  cooled <- switched
  cooled$phase_age <- 0L
  for (k in seq_len(20L)) {
    cooled <- windfarmGA:::adapt_operator_rates(
      phase = cooled$phase, stall = 20L, coverage = 0.6, generation = 40L,
      teil = cooled$teil, mut_adapt = cooled$mut_adapt, p_inj = cooled$p_inj,
      mut_floor = 0.05, mut_ceil = 0.15, mut_target = 0.1,
      phase_age = cooled$phase_age, refine_hold = 100L
    )
  }
  expect_equal(cooled$phase, "refine")
  expect_lte(cooled$p_inj, 0.16)
  expect_lte(cooled$mut_adapt, 0.105)
  expect_gte(cooled$teil, 2.1)

  thin <- do.call(
    windfarmGA:::adapt_operator_rates,
    modifyList(rates0, list(phase = "refine", coverage = 0.2, generation = 30L))
  )
  expect_equal(thin$phase, "explore")
})

test_that("refine pulses even when new maxes keep arriving", {
  state <- list(
    phase = "refine", phase_age = 0L, stall = 0L, coverage = 0.6,
    generation = 80L, teil = 2.2, mut_adapt = 0.1, p_inj = 0.15,
    mut_floor = 0.05, mut_ceil = 0.15, mut_target = 0.1,
    refine_hold = 8L, explore_pulse = 5L
  )
  for (k in seq_len(8L)) {
    state <- windfarmGA:::adapt_operator_rates(
      phase = state$phase, stall = 0L, coverage = 0.6, generation = 80L,
      teil = state$teil, mut_adapt = state$mut_adapt, p_inj = state$p_inj,
      mut_floor = 0.05, mut_ceil = 0.15, mut_target = 0.1,
      phase_age = state$phase_age, refine_hold = 8L, explore_pulse = 5L
    )
  }
  expect_equal(state$phase, "pulse")
  expect_gt(state$p_inj, 0.15)
  expect_gt(state$mut_adapt, 0.1)
  expect_lt(state$teil, 2.2)

  for (k in seq_len(5L)) {
    state <- windfarmGA:::adapt_operator_rates(
      phase = state$phase, stall = 0L, coverage = 0.6, generation = 90L,
      teil = state$teil, mut_adapt = state$mut_adapt, p_inj = state$p_inj,
      mut_floor = 0.05, mut_ceil = 0.15, mut_target = 0.1,
      phase_age = state$phase_age, refine_hold = 8L, explore_pulse = 5L
    )
  }
  expect_equal(state$phase, "refine")
})

test_that("elite count rises on a refine stall and drops in a pulse", {
  expect_equal(windfarmGA:::adapt_elite_n(3L, 40L, "explore", 0L), 3L)
  expect_equal(windfarmGA:::adapt_elite_n(3L, 40L, "refine", 3L), 3L)
  expect_equal(windfarmGA:::adapt_elite_n(3L, 40L, "refine", 8L), 5L)
  expect_equal(windfarmGA:::adapt_elite_n(3L, 40L, "refine", 20L), 6L)
  expect_equal(windfarmGA:::adapt_elite_n(3L, 40L, "pulse", 20L), 2L)
  expect_equal(windfarmGA:::adapt_elite_n(3L, 4L, "refine", 20L), 4L)
})
