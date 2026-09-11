## Profile calculate_energy() (GitHub only).
## source("experimental/profile_energy.R")
## profile_calculate_energy()

profile_calculate_energy <- function(n_turb = 15L,
                                     n_dir = 12L,
                                     n_rep = 8L,
                                     rotor = 50) {
  area <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(0, 0, 2000, 2000, 0),
      c(0, 2000, 2000, 0, 0)
    ))),
    crs = 3035
  ))
  grid <- windfarmGA::grid_area(
    area = area, size = rotor * 3, prop = 1, plot_grid = FALSE
  )[[1]]
  layout <- windfarmGA::init_population(grid = grid, n = n_turb, n_start = 1L)[[1]]
  wind <- data.frame(
    ws = 8 + 2 * sin((seq_len(n_dir) - 1) * 2 * pi / n_dir),
    wd = (seq_len(n_dir) - 1) * (360 / n_dir)
  )

  one <- function() {
    windfarmGA::calculate_energy(
      layout = layout,
      reference_height = 50,
      rotor_height = 50,
      surface_roughness = 0.14,
      wake_angle = 20,
      wake_distance = 100000,
      wind = wind,
      rotor = rotor,
      area = area,
      terrain = FALSE,
      weibull = FALSE
    )
  }

  invisible(one())
  wall <- system.time(replicate(n_rep, one(), simplify = FALSE))
  tmp <- tempfile(fileext = ".out")
  utils::Rprof(tmp, interval = 0.002, memory.profiling = FALSE)
  replicate(max(20L, n_rep), one(), simplify = FALSE)
  utils::Rprof(NULL)
  sm <- utils::summaryRprof(tmp)
  unlink(tmp)

  list(
    n_turb = n_turb,
    n_dir = n_dir,
    n_rep = n_rep,
    elapsed_s = unname(wall["elapsed"]),
    per_call_s = unname(wall["elapsed"]) / n_rep,
    by_total = profile_keep_hotspots(sm$by.total, 12L),
    by_self = profile_keep_hotspots(sm$by.self, 12L)
  )
}

profile_keep_hotspots <- function(df, n = 12L) {
  drop <- c(
    "check", "tryCatch", "doTryCatch", "tryCatchList", "tryCatchOne",
    "print_energy_profile", "test_climate_helpers", "cat", "sprintf",
    "replicate", "source", "eval", "eval.with.vis", "withVisible"
  )
  keep <- !rownames(df) %in% drop
  utils::head(df[keep, , drop = FALSE], n)
}

print_energy_profile <- function(p) {
  if (missing(p)) {
    p <- profile_calculate_energy()
  }
  cat(sprintf(
    "calculate_energy: %d turbines, %d directions, %.3f s/call (n=%d)\n",
    p$n_turb, p$n_dir, p$per_call_s, p$n_rep
  ))
  cat("\nby.total (top):\n")
  print(p$by_total)
  cat("\nby.self (top):\n")
  print(p$by_self)
  invisible(p)
}

# print_energy_profile()
