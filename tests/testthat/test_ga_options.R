test_that("ga_options lists and sets windfarmGA options", {
  old <- options(windfarmGA.immigrants = 3L)
  on.exit(options(old), add = TRUE)
  opts <- capture.output(out <- ga_options())
  expect_true(is.list(out))
  expect_true("windfarmGA.immigrants" %in% names(out))
  expect_true(any(grepl("immigrants", opts)))

  ga_options(immigrants = 7L)
  expect_equal(getOption("windfarmGA.immigrants"), 7L)
  ga_options(list(local_search_tries = 4L))
  expect_equal(getOption("windfarmGA.local_search_tries"), 4L)
})

test_that("as_windfarmGA print and plot dispatch", {
  res <- as_windfarmGA(resultrect)
  expect_s3_class(res, "windfarmGA")
  out <- paste(capture.output(print(res)), collapse = "\n")
  expect_match(out, "windfarmGA result")
  expect_match(out, "generations")
  expect_error(plot(res), "polygon")
})

test_that("power curve lookup and plot", {
  curve <- data.frame(ws = c(0, 3, 12, 25), power = c(0, 0, 2000, 2000))
  expect_equal(lookup_power_curve(12, curve), 2000)
  expect_equal(lookup_power_curve(0, curve), 0)
  expect_equal(lookup_power_curve(7.5, curve), 1000)
  xy <- plot_power_curve(curve, plot = FALSE)
  expect_equal(xy$power[1], 0)
})
