#' @title Plot a Windrose
#' @name plot_windrose
#' @description  Plot a wind rose of the wind data frame.
#'
#' @export
#'
#' @param data A data.frame containing the wind information
#' @param spd The column of the wind speeds in "data"
#' @param dir The column of the wind directions in "data"
#' @param spdres The increment of the wind speed legend. Default is 2
#' @param dirres The size of the wind sectors. Default is 10
#' @param spdmin Minimum wind speed. Default is 1
#' @param spdmax Maximal wind speed. Default is 30
#' @param palette A color palette used for drawing the wind rose
#' @param spdseq A wind speed sequence, that is used for plotting
#' @param plotit Should the windrose be plotted? Default is TRUE
#'
#' @family Plotting Functions
#' @return NULL
#'
#' @examples
#' ## Exemplary Input Wind speed and direction data frame
#' # Uniform wind speed and single wind direction
#' data.in <- data.frame(ws = 12, wd = 0)
#' windrosePlot <- plot_windrose(data = data.in, spd = data.in$ws,
#'    dir = data.in$wd)
#'
#' # Random wind speeds and random wind directions
#' data.in <- data.frame(ws = sample(1:25, 10), 
#'                       wd = sample(1:260, 10))
#' windrosePlot <- plot_windrose(data = data.in, spd = data.in$ws,
#'    dir = data.in$wd)
#'
plot_windrose <- function(data, spd, dir, spdres = 2, dirres = 10, spdmin = 1,
                         spdmax = 30, palette = "YlGnBu",
                         spdseq = NULL, plotit = TRUE) {
  
  if (!missing(data) && exists("data")) {
    # Assume that we've been given a data frame. Lets find the correct columns
    if (length(colnames(data))) {
      accep_speed <- c("SPEED", "GESCH", "V", "WS")
      accep_direc <- c("DIR", "RICHT", "WD")
      sum_col_match <- sum(sapply(c(accep_speed, accep_direc), grepl,
                                  toupper(colnames(data)) ))
      if (sum_col_match >= 2) {
        speed_match <- which(sapply(
          lapply(accep_speed, grepl, toupper(colnames(data))),
          any))
        direc_match <- which(sapply(
          lapply(accep_direc, grepl, toupper(colnames(data))),
          any))
        
        speed_index <- which(grepl(accep_speed[speed_match],
                                   toupper(colnames(data))))
        direc_index <- which(grepl(accep_direc[direc_match],
                                   toupper(colnames(data))))
        data[, c(speed_index[1], direc_index[1])]
        
        spd <- colnames(data)[speed_index]
        dir <- colnames(data)[direc_index]
      } else {
        col_numeric <- which(sapply(data[1, ], is.numeric))
        data <- data[, col_numeric]
        colnames(data) <- c("spd", "dir")
        spd <- "spd"
        dir <- "dir"
      }
    } else {
      col_numeric <- which(sapply(data[1, ], is.numeric))
      data <- data[, col_numeric]
      colnames(data) <- c("spd", "dir")
      spd <- "spd"
      dir <- "dir"
    }
  } 
  else if (!missing(spd) && !missing(dir) && is.numeric(spd) && is.numeric(dir)) {
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd, dir = dir)
    spd <- "spd"
    dir <- "dir"
  }

  # Tidy up input data #################
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA

  # figure out the wind speed bins #################
  if (missing(spdseq) || is.null(spdseq)) {
    spdseq <- seq(spdmin, spdmax, spdres)
  }

  # get some information about the number of bins, etc. #################
  seq_length <- length(spdseq)
  colorpal_n <- seq_length - 1

  # create the color map #################
  wind_colorpal <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(
    min(max(3, colorpal_n), min(9, colorpal_n)), palette))(colorpal_n)
  
  if (max(data[[spd]], na.rm = TRUE) > spdmax) {
    speed_brks <- c(spdseq, max(data[[spd]], na.rm = TRUE))
    speed_labls <- c(paste(c(spdseq[1:seq_length - 1]), '-', 
                          c(spdseq[2:seq_length])),
                    paste(spdmax, "-", max(data[[spd]], na.rm = TRUE)))
    wind_colorpal <- c(wind_colorpal, "grey50")
  } else{
    speed_brks <- spdseq
    speed_labls <- paste(c(spdseq[1:seq_length - 1]), '-', 
                        c(spdseq[2:seq_length]))
  }
  speed_bins <- cut(x = data[[spd]], breaks = speed_brks, 
                        labels = speed_labls, ordered_result = TRUE)

  # figure out the wind direction bins #################
  dir_brks <- c(-dirres / 2, seq(dirres / 2, 360 - dirres/2, 
                                   by = dirres), 360 + dirres / 2)
  dir_labls <- c(paste(360 - dirres / 2, "-", dirres / 2),
                  paste(seq(dirres / 2, 360 - 3 * dirres / 2, 
                            by = dirres), "-", 
                        seq(3 * dirres/2, 360 - dirres / 2, by = dirres)),
                  paste(360 - dirres / 2, "-", dirres / 2))
  # assign each wind direction to a bin
  dir_bins <- cut(data[[dir]], breaks = dir_brks,
                    ordered_result = TRUE)
  levels(dir_bins) <- dir_labls
  data$dir_bins <- dir_bins

  # create the plot #################
  plot_windrose <- ggplot2::ggplot(data = data,
                           ggplot2::aes(x = dir_bins,
                           fill = speed_bins)) +
    ggplot2::geom_bar() +
    ggplot2::scale_x_discrete(drop = FALSE, labels = ggplot2::waiver()) +
    ggplot2::coord_polar(start = -((dirres / 2) / 360) * 2 * pi) +
    ggplot2::scale_fill_manual(name = "Wind Speed (m/s)",
                      values = wind_colorpal,
                      drop = FALSE) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   legend.background = ggplot2::element_rect(fill = "gray96",
                                                             colour = "gray96"),
                   panel.background = ggplot2::element_rect(fill = "gray96",
                                                            colour = "gray96"),
                   panel.grid.minor.y = ggplot2::element_line(size = 2),
                   panel.grid.major = ggplot2::element_line(colour = "gray86"),
                   panel.border = ggplot2::element_blank(),
                   plot.background = ggplot2::element_rect(fill = "gray96",
                                                           colour = "gray96"),
                   strip.background = ggplot2::element_rect(fill = "gray96",
                                                           colour = "gray96"),
                   plot.margin = ggplot2::unit(c(0, 0, 0, 0), "lines"))


  if (plotit) {
    # print the plot #################
    print(plot_windrose)
  }

  # return the handle to the wind rose #################
  invisible(plot_windrose)
}