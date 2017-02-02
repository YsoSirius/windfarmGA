#' @title Plot a Windrose
#' @name plotWindrose
#' @description  Plot a wind rose of the the wind data frame.
#'
#' @export
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot aes geom_bar scale_x_discrete waiver
#'  coord_polar scale_fill_manual theme element_blank ylim
#'
#' @param data A data.frame containing the wind information (data.frame)
#' @param spd The column of the wind speeds in the "data"-data.frame
#' (numeric)
#' @param dir The column of the wind directions in the "data"-data.frame
#' (numeric)
#' @param spdres The increment of the wind speed legend. Default is 2
#' (numeric)
#' @param dirres The size of the wind sectors. Default is 10  (numeric)
#' @param spdmin Minimum wind speed. Default is 1 (numeric)
#' @param spdmax Maximal wind speed. Default is 30 (numeric)
#' @param palette A color palette used for drawing the wind rose (character)
#' @param debug For running a debug. Default is 0 (numeric)
#' @param spdseq A wind speed sequence, that is used for plotting (numeric)
#'
#' @return NULL
#'
#' @examples \donttest{
#' ## Exemplary Input Wind speed and direction data frame
#' # Uniform wind speed and single wind direction
#' data.in <- as.data.frame(cbind(ws=12,wd=0))
#' windrosePlot <- plotWindrose(data = data.in, spd = data.in$ws,
#'    dir = data.in$wd)
#'
#' # Random wind speeds and random wind directions
#' data.in <- as.data.frame(cbind(ws=sample(1:25,10),wd=sample(1:260,10)))
#' windrosePlot <- plotWindrose(data = data.in, spd = data.in$ws,
#'    dir = data.in$wd)
#'}
#' @author Sebastian Gatscha

##utils::globalVariables("spd.binned");

plotWindrose <- function(data,spd,dir,spdres = 2,dirres = 10,spdmin = 1,
                          spdmax = 30, palette = "YlGnBu",  debug = 0,  spdseq = NULL){

  countmax = NA
  # require(ggplot2);   require(RColorBrewer)
  # Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (exists("data")){
    # Assume that we've been given a data frame, and the name of the speed
    # and direction columns. This is the format we want for later use.
  }

  # Tidy up input data
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA

  # figure out the wind speed bins
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1

  # create the color map
  spd.colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(min(max(3,n.colors.in.range),min(9,n.colors.in.range)),palette))(n.colors.in.range)

  if (max(data[[spd]],na.rm = TRUE) > spdmax){
    spd.breaks <- c(spdseq,max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),'-',c(spdseq[2:n.spd.seq])),paste(spdmax,"-",max(data[[spd]],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),'-',c(spdseq[2:n.spd.seq]))
  }
  data$spd.binned <- cut(x = data[[spd]],breaks = spd.breaks,labels = spd.labels,ordered_result = TRUE)

  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,seq(dirres/2, 360-dirres/2, by = dirres),360+dirres/2)
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),paste(seq(dirres/2, 360-3*dirres/2, by = dirres),"-",seq(3*dirres/2, 360-dirres/2, by = dirres)),paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned

  # Run debug if required
  if (debug>0){
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")
  }

  # create the plot
  p.windrose <- ggplot2::ggplot(data = data,
                           ggplot2::aes(x = dir.binned,
                           fill = spd.binned)) +
    ggplot2::geom_bar() +
    ggplot2::scale_x_discrete(drop = FALSE,
                     labels = ggplot2::waiver()) +
    ggplot2::coord_polar(start = -((dirres/2)/360) * 2*pi) +
    ggplot2::scale_fill_manual(name = "Wind Speed (m/s)",
                      values = spd.colors,
                      drop = FALSE) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y=element_blank(),
                   legend.background = element_rect(fill = "gray96", colour = "gray96"),
                   panel.background = element_rect(fill = "gray96", colour = "gray96"),
                   panel.grid.minor.y = element_line(size=2),
                   panel.grid.major = element_line(colour = "gray86"),
                   panel.border=element_blank(),
                   plot.background = element_rect(fill = "gray96", colour = "gray96"),
                   strip.background=element_rect(fill="gray96", colour="gray96"),
                   plot.margin=unit(c(0, 0, 0, 0), "lines"))
  
  

  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ggplot2::ylim(c(0,countmax))
  }

  # print the plot
  print(p.windrose)

  # return the handle to the wind rose
  return(p.windrose)
}



