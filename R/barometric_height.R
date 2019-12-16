#' @title Calculates Air Density, Air Pressure and Temperature according
#' to the Barometric Height Formula
#' @name barometric_height
#' @description Calculates air density, temperature and air pressure
#'     respective to certain heights according to the International
#'     standard atmosphere and the barometric height formula.
#'
#' @export
#'
#' @param data A data.frame containing the height values
#' @param height Column name of the height values 
#' @param po Standardized air pressure at sea level (101325 Pa)
#' @param ro Standardized air density at sea level (1,225 kg per m3)
#'
#' @family Wind Energy Calculation Functions
#' @return Returns a data.frame with height values and corresponding
#' air pressures, air densities and temperatures in Kelvin and Celsius.
#'
#' @examples
#' data <- matrix(seq(0,5000,500));
#' barometric_height(data)
#' plot.ts(barometric_height(data))
barometric_height         <- function(data, height, po = 101325, ro = 1.225) {
  if ( is.numeric(data) || (ncol(data)) == 1 ) {
    ## Luftdruck auf Hoehe h berechnen
    ph <- as.numeric(po * exp(-data * 0.0001252))
    ## Luftdichte berechnen
    rh <- as.numeric(ro * exp(-data * 0.0001252))
    ## Temperatur auf Hoehe berechnen
    Th <- as.numeric(288.15 - ((6.5 * data) / 1000))
  } else {
    if (missing(height)) {
      stop("Height column not given.")
    }
    data <- data[, height]
    ## Luftdruck auf Hoehe h berechnen
    ph <- po * exp(-data * 0.0001252)
    ## Luftdichte berechnen
    rh <- ro * exp(-data * 0.0001252)
    ## Temperatur auf Hoehe berechnen
    Th <- 288.15 - ((6.5 * data) / 1000)
  }

  if (class(data)[1] != "data.frame") {
    data <- data.frame(data)
  }
  colnames(data) <- "Height"

  celsius <- as.numeric(Th - 273.15)
  data$ph <- ph
  data$rh <- rh
  data$tempK <- Th
  data$tempC <- celsius

  return(data)
}
