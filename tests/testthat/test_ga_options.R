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
  expect_match(out, "Inputs")
  expect_match(out, "turbines")
  expect_match(out, "Wind")
  expect_match(out, "New best")
  expect_match(out, "generation")
  expect_error(plot(res), "polygon")
  expect_s3_class(as_windfarmGA(res), "windfarmGA")

  empty <- matrix(list(NULL), ncol = 1, dimnames = list(NULL, "allparkcoeff"))
  expect_null(windfarmGA:::ga_result_records(empty))
  expect_null(windfarmGA:::ga_result_best(empty))
  expect_null(windfarmGA:::ga_result_inputs(empty))
  expect_true(is.na(windfarmGA:::ga_inp(NULL, "x")))

  w_wrap <- matrix(
    list(list(Windspeed_Data = data.frame(ws = 7, wd = 90))),
    ncol = 1, dimnames = list(NULL, "inputWind")
  )
  expect_equal(windfarmGA:::ga_result_wind(w_wrap)$wd, 90)
  w_lst <- matrix(
    list(list(data.frame(ws = 6, wd = 45), 100)),
    ncol = 1, dimnames = list(NULL, "inputWind")
  )
  expect_equal(windfarmGA:::ga_result_wind(w_lst)$probab, 100)
  w_mat <- matrix(
    list(matrix(c(5, 180), nrow = 1, dimnames = list(NULL, c("ws", "wd")))),
    ncol = 1, dimnames = list(NULL, "inputWind")
  )
  expect_equal(windfarmGA:::ga_result_wind(w_mat)$ws, 5)
  expect_null(windfarmGA:::ga_result_wind(
    matrix(list(data.frame()), ncol = 1, dimnames = list(NULL, "inputWind"))
  ))

  out_w <- paste(capture.output(windfarmGA:::print_ga_wind(NULL)), collapse = "\n")
  expect_match(out_w, "none stored")
  many <- data.frame(wd = seq_len(20) * 10, ws = 8, probab = 5)
  out_m <- paste(capture.output(windfarmGA:::print_ga_wind(many)), collapse = "\n")
  expect_match(out_m, "20 directions")
  capture.output(windfarmGA:::print_ga_inputs(NULL))
  inp <- resultrect[1, "inputData"][[1]]
  inp["Percentage of Polygon", 1] <- 80
  out_i <- paste(capture.output(windfarmGA:::print_ga_inputs(inp)), collapse = "\n")
  expect_match(out_i, "80")
})

test_that("power curve lookup and plot", {
  curve <- data.frame(ws = c(0, 3, 12, 25), power = c(0, 0, 2000, 2000))
  expect_equal(lookup_power_curve(12, curve), 2000)
  expect_equal(lookup_power_curve(0, curve), 0)
  expect_equal(lookup_power_curve(7.5, curve), 1000)
  xy <- plot_power_curve(curve, plot = FALSE)
  expect_equal(xy$power[1], 0)
})
