#' @title Shiny explorer for a GA result
#' @name explore_result
#' @description One-page viewer for a [genetic_algorithm()] result: Leaflet
#'   map of that generation's best layout (downwind wake cones, optional
#'   terrain layers), a wind rose, and one plotly figure with fitness,
#'   operator rates and population. New fitness maxima are marked; click
#'   a marker to jump to that generation. The cell heatmap is not
#'   included: use [plot_cell_heatmap()] or [plot_generation()] offline.
#'   Requires Suggests `shiny`. Plotly is used when installed.
#' @export
#'
#' @inheritParams plot_result
#' @return The Shiny app object, invisibly. Called for its side effect.
#'
#' @examples \dontrun{
#' explore_result(resultrect, sp_polygon)
#' }
explore_result <- function(result, area) {
  if (!is_shiny_installed()) {
    stop(
      "explore_result() needs the 'shiny' package.\n",
      "Install it with install.packages('shiny')"
    )
  }
  has_plotly <- is_plotly_installed()
  if (!has_plotly && !is_ggplot2_installed()) {
    stop(
      "explore_result() needs 'plotly' or 'ggplot2'.\n",
      "Install with install.packages('plotly')"
    )
  }
  result <- as_windfarmGA(result)
  area <- isSpatial(area)
  s <- ga_series(result)
  cen <- tryCatch(population_census(result), error = function(e) NULL)
  has_leaflet <- is_leaflet_installed()
  has_ggplot <- is_ggplot2_installed()
  overall <- ga_result_best(result)
  wind <- ga_result_wind(result)
  terrain_ll <- leaflet_prepare_terrain(ga_result_terrain(result))
  rose <- if (has_ggplot) {
    tryCatch(explore_windrose_plot(wind), error = function(e) NULL)
  } else {
    NULL
  }

  ui <- shiny::fluidPage(
    shiny::titlePanel("windfarmGA"),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::sliderInput(
          "gen", "Generation",
          min = 1L, max = s$n, value = s$n, step = 1L, width = "100%"
        ),
        shiny::verbatimTextOutput("stats"),
        if (!is.null(rose)) {
          shiny::tagList(
            shiny::h5("Wind rose"),
            shiny::plotOutput("windrose", height = "180px")
          )
        }
      ),
      shiny::column(
        9,
        if (has_leaflet) {
          leaflet::leafletOutput("map", height = "540px")
        } else {
          shiny::plotOutput("map_static", height = "440px")
        }
      )
    ),
    if (has_plotly) {
      plotly::plotlyOutput("series", height = "900px")
    } else {
      shiny::plotOutput("series_gg", height = "900px")
    }
  )

  layout_stats <- function(gen) {
    lay <- tryCatch(result[gen, "bestPaEn"][[1]], error = function(e) NULL)
    if (is.null(lay) || !nrow(lay)) {
      return(NULL)
    }
    list(
      energy = lay[1, "EnergyOverall"],
      efficiency = lay[1, "EfficAllDir"],
      fitness = if ("Parkfitness" %in% colnames(lay)) {
        lay[1, "Parkfitness"]
      } else {
        NA_real_
      }
    )
  }

  server <- function(input, output, session) {
    output$stats <- shiny::renderText({
      shiny::req(input$gen)
      cur <- layout_stats(input$gen)
      new_max <- isTRUE(s$improved[[input$gen]])
      lines <- c(paste0("Generation ", input$gen, " / ", s$n))
      if (new_max) {
        lines <- c(lines, "New fitness maximum")
      }
      if (has_plotly && any(s$improved)) {
        lines <- c(lines, "Click a New max marker to jump")
      }
      if (!is.null(cur)) {
        lines <- c(
          lines,
          sprintf("Best this gen:  %s kW", format(cur$energy, digits = 6)),
          sprintf("Efficiency:     %s %%", format(cur$efficiency, digits = 5)),
          sprintf("Fitness:        %s", format(cur$fitness, digits = 6))
        )
      }
      if (!is.null(terrain_ll)) {
        lines <- c(lines, "Terrain: toggle Elevation / Wind multiplier")
      }
      if (!is.null(overall)) {
        lines <- c(
          lines,
          "",
          sprintf("Run best:       %s kW", format(overall$energy, digits = 6)),
          sprintf("                %s %%", format(overall$efficiency, digits = 5))
        )
      }
      paste(lines, collapse = "\n")
    })

    if (has_leaflet) {
      output$map <- leaflet::renderLeaflet({
        shiny::req(input$gen)
        plot_leaflet(
          result, area,
          which = input$gen,
          orderitems = FALSE,
          wind = wind,
          terrain = terrain_ll
        )
      })
    } else {
      output$map_static <- shiny::renderPlot({
        shiny::req(input$gen)
        plot_result(
          result, area,
          best = 1, plot_en = 1,
          terrain = FALSE, plot_grid = TRUE
        )
      })
    }

    if (!is.null(rose)) {
      output$windrose <- shiny::renderPlot({
        print(rose)
      })
    }

    if (has_plotly) {
      output$series <- plotly::renderPlotly({
        explore_series_plotly(s, cen, gen = s$n)
      })
      shiny::observeEvent(input$gen, {
        plotly::plotlyProxyInvoke(
          plotly::plotlyProxy("series", session),
          "relayout",
          list(shapes = explore_gen_shape(input$gen))
        )
      }, ignoreNULL = TRUE)
      shiny::observeEvent(
        plotly::event_data("plotly_click", source = "explore_ga"),
        {
          ed <- plotly::event_data("plotly_click", source = "explore_ga")
          g <- explore_click_generation(ed, which(s$improved))
          if (is.null(g)) {
            return()
          }
          g <- max(1L, min(as.integer(s$n), g))
          if (identical(as.integer(input$gen), g)) {
            return()
          }
          shiny::updateSliderInput(session, "gen", value = g)
        },
        ignoreNULL = TRUE
      )
    } else {
      output$series_gg <- shiny::renderPlot({
        shiny::req(input$gen)
        explore_series_ggplot(s, cen, gen = input$gen)
      })
    }
  }

  app <- shiny::shinyApp(ui, server)
  if (interactive()) {
    shiny::runApp(app, launch.browser = TRUE)
  }
  invisible(app)
}

explore_windrose_plot <- function(wind) {
  if (is.null(wind) || !is.data.frame(wind) || !nrow(wind)) {
    return(NULL)
  }
  plot_windrose(wind, plot = FALSE)
}

explore_gen_shape <- function(gen) {
  list(list(
    type = "line",
    x0 = gen, x1 = gen, xref = "x",
    y0 = 0, y1 = 1, yref = "paper",
    line = list(color = "grey45", dash = "dash", width = 1)
  ))
}

explore_click_generation <- function(ed, improved) {
  if (is.null(ed) || !NROW(ed)) {
    return(NULL)
  }
  if ("customdata" %in% names(ed)) {
    g <- suppressWarnings(as.integer(unlist(ed$customdata)[[1]]))
    if (length(g) && is.finite(g)) {
      return(g)
    }
  }
  if (length(improved) && "x" %in% names(ed)) {
    g <- suppressWarnings(as.integer(round(as.numeric(ed$x[[1]]))))
    if (length(g) && is.finite(g) && g %in% as.integer(improved)) {
      return(g)
    }
  }
  NULL
}

explore_series_plotly <- function(s, cen, gen, source = "explore_ga") {
  improved <- which(s$improved)
  p_fit <- plotly::plot_ly(source = source)
  p_fit <- plotly::add_lines(
    p_fit, x = s$gen, y = s$fit_max, name = "Max Fitness",
    legendgroup = "fit", line = list(color = "#1B4F72", width = 2)
  )
  p_fit <- plotly::add_lines(
    p_fit, x = s$gen, y = s$fit_mean, name = "Mean Fitness",
    legendgroup = "fit", line = list(color = "#2980B9", width = 1.6)
  )
  p_fit <- plotly::add_lines(
    p_fit, x = s$gen, y = s$fit_min, name = "Min Fitness",
    legendgroup = "fit", line = list(color = "#AED6F1", width = 1.4)
  )
  if (length(improved)) {
    p_fit <- plotly::add_markers(
      p_fit,
      x = s$gen[improved], y = s$fit_max[improved],
      customdata = s$gen[improved],
      name = "New max", legendgroup = "fit",
      marker = list(symbol = "diamond", size = 11, color = "#C0392B"),
      hovertemplate = paste0(
        "New max (gen %{x}): %{y:.4f}",
        "<br>Click to open this generation<extra></extra>"
      )
    )
  }
  p_fit <- plotly::layout(p_fit, yaxis = list(title = "Fitness"))

  p_rate <- plotly::plot_ly(source = source)
  p_rate <- plotly::add_lines(
    p_rate, x = s$gen, y = s$sel_pct, name = "Selection",
    legendgroup = "rate", line = list(color = "#27AE60", width = 1.8)
  )
  p_rate <- plotly::add_lines(
    p_rate, x = s$gen, y = 100 * s$inject, name = "Crossover inject",
    legendgroup = "rate", line = list(color = "#F39C12", width = 1.8)
  )
  p_rate <- plotly::add_lines(
    p_rate, x = s$gen, y = 100 * s$mut, name = "Mutation",
    legendgroup = "rate", line = list(color = "#8E44AD", width = 1.8)
  )
  p_rate <- plotly::layout(p_rate, yaxis = list(title = "%"))

  plots <- list(p_fit, p_rate)
  if (!is.null(cen) && nrow(cen)) {
    p_pop <- plotly::plot_ly(source = source)
    p_pop <- plotly::add_lines(
      p_pop, x = cen$generation, y = cen$evaluated, name = "Evaluated",
      legendgroup = "pop", line = list(color = "#34495E", width = 1.8)
    )
    p_pop <- plotly::add_lines(
      p_pop, x = cen$generation, y = cen$elites, name = "Elites",
      legendgroup = "pop", line = list(color = "#E67E22", width = 1.8)
    )
    p_pop <- plotly::add_lines(
      p_pop, x = cen$generation, y = cen$cells, name = "Cells",
      legendgroup = "pop", line = list(color = "#16A085", width = 1.8)
    )
    p_pop <- plotly::layout(p_pop, yaxis = list(title = "Count"))
    plots <- c(plots, list(p_pop))
  }

  ply <- plotly::subplot(
    plots,
    nrows = length(plots),
    shareX = TRUE,
    titleY = TRUE,
    margin = 0.04
  )
  ply <- plotly::layout(
    ply,
    hovermode = "x unified",
    legend = list(
      orientation = "h",
      x = 0,
      y = 1.1,
      yanchor = "bottom",
      xanchor = "left",
      font = list(size = 11),
      bgcolor = "rgba(255,255,255,0.95)",
      tracegroupgap = 8
    ),
    margin = list(t = 10, l = 55, r = 24, b = 48),
    xaxis = list(title = "Generation"),
    shapes = explore_gen_shape(gen)
  )
  reg <- get0("event_register", envir = asNamespace("plotly"), inherits = FALSE)
  if (is.function(reg)) {
    ply <- reg(ply, "plotly_click")
  }
  ply
}

explore_series_ggplot <- function(s, cen, gen) {
  improved <- which(s$improved)
  df_fit <- data.frame(
    generation = rep(s$gen, 3),
    value = c(s$fit_max, s$fit_mean, s$fit_min),
    series = factor(
      rep(c("Max", "Mean", "Min"), each = s$n),
      levels = c("Max", "Mean", "Min")
    )
  )
  p1 <- ggplot2::ggplot(df_fit, ggplot2::aes(generation, value, color = series)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_vline(xintercept = gen, linetype = 2, color = "grey40") +
    ggplot2::labs(y = "Fitness", x = NULL, title = "Park fitness") +
    ga_plot_theme(legend = "bottom")
  if (length(improved)) {
    p1 <- p1 + ggplot2::geom_point(
      data = data.frame(generation = s$gen[improved], value = s$fit_max[improved]),
      ggplot2::aes(generation, value),
      inherit.aes = FALSE, color = "#C0392B", shape = 18, size = 3
    )
  }

  df_rate <- data.frame(
    generation = rep(s$gen, 3),
    value = c(s$sel_pct, 100 * s$inject, 100 * s$mut),
    series = factor(
      rep(c("Selection", "Crossover inject", "Mutation"), each = s$n),
      levels = c("Selection", "Crossover inject", "Mutation")
    )
  )
  p2 <- ggplot2::ggplot(df_rate, ggplot2::aes(generation, value, color = series)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_vline(xintercept = gen, linetype = 2, color = "grey40") +
    ggplot2::labs(y = "%", x = NULL, title = "Operator rates") +
    ga_plot_theme(legend = "bottom")

  plots <- list(p1, p2)
  if (!is.null(cen) && nrow(cen)) {
    df_pop <- data.frame(
      generation = rep(cen$generation, 3),
      value = c(cen$evaluated, cen$elites, cen$cells),
      series = factor(
        rep(c("Evaluated", "Elites", "Cells"), each = nrow(cen)),
        levels = c("Evaluated", "Elites", "Cells")
      )
    )
    df_pop <- df_pop[is.finite(df_pop$value), , drop = FALSE]
    p3 <- ggplot2::ggplot(df_pop, ggplot2::aes(generation, value, color = series)) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::geom_vline(xintercept = gen, linetype = 2, color = "grey40") +
      ggplot2::labs(y = "Count", x = "Generation", title = "Population") +
      ga_plot_theme(legend = "bottom")
    plots <- c(plots, list(p3))
  }

  n <- length(plots)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(n, 1)))
  for (i in seq_along(plots)) {
    print(plots[[i]], vp = grid::viewport(layout.pos.row = i, layout.pos.col = 1))
  }
}
