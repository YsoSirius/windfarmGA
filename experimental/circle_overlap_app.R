## Shiny check of circle_intersection() (GitHub only, not on CRAN).
## source("experimental/circle_overlap_app.R"); circle_overlap_app()

circle_overlap_app <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("install.packages('shiny')")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("install.packages('ggplot2')")
  }
  if (!requireNamespace("ggforce", quietly = TRUE)) {
    stop("install.packages('ggforce')")
  }

  x1 <- 10
  ui <- shiny::fluidPage(
    shiny::sidebarPanel(
      shiny::sliderInput("radius1", "Radius 1 (green)", 12, min = 1, max = 30),
      shiny::sliderInput("radius2", "Radius 2 (red)", 26, min = 1, max = 30),
      shiny::sliderInput("height1", "Height 1", 10, min = 1, max = 100),
      shiny::sliderInput("height2", "Height 2", 50, min = 1, max = 100),
      shiny::sliderInput("distance", "Distance on x", 10, min = 0, max = 100)
    ),
    shiny::mainPanel(
      shiny::plotOutput("circles", height = "700px"),
      shiny::verbatimTextOutput("overlap"),
      shiny::verbatimTextOutput("centre_distance")
    )
  )

  server <- function(input, output, session) {
    output$circles <- shiny::renderPlot({
      ggplot2::ggplot() +
        ggforce::geom_circle(
          ggplot2::aes(x0 = x1, y0 = input$height1, r = input$radius1),
          fill = "green", alpha = 0.5
        ) +
        ggforce::geom_circle(
          ggplot2::aes(x0 = x1 + input$distance, y0 = input$height2, r = input$radius2),
          fill = "red", alpha = 0.5
        ) +
        ggplot2::coord_fixed()
    })
    output$centre_distance <- shiny::renderText({
      d <- sqrt(input$distance^2 + (input$height2 - input$height1)^2)
      paste("Distance of circle centres:", round(d, 2))
    })
    output$overlap <- shiny::renderText({
      overlap <- windfarmGA::circle_intersection(
        input$radius1, input$radius2,
        input$height1, input$height2, input$distance
      )
      area1 <- pi * input$radius1^2
      sprintf(
        "Overlap: %s m2 (%.1f %% of circle 1)",
        round(overlap, 2), 100 * overlap / area1
      )
    })
  }

  shiny::runApp(shiny::shinyApp(ui, server), launch.browser = TRUE)
}
