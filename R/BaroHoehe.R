#' @title Calculates Air Density, Air Pressure and Temperature according
#' to the Barometric Height Formula
#' @name BaroHoehe
#' @description Calculates air density, temperature and air pressure
#'     respective to certain heights according to the International
#'     standard atmosphere and the barometric height formula.
#' @export
#'
#'
#' @param data A data.frame containing the height values (data.frame)
#' @param height Column name of the height values (character)
#' @param po Standardized air pressure at sea level (101325 Pa) (numeric)
#' @param ro Standardized air density at sea level (1,225 kg per m3)
#' (numeric)
#'
#' @return Returns a data.frame with height values and corresponding
#' air pressures, air densities and temperatures in Kelvin and Celsius.
#' (data.frame)
#'
#' @examples \donttest{
#' data <- matrix(seq(0,5000,500));
#' BaroHoehe(data)
#' plot.ts(BaroHoehe(data))
#'}
#'
#' @author Sebastian Gatscha

BaroHoehe         <- function(data, height, po=101325, ro=1.225) {
  if ((ncol(data))==1) {
    ## Luftdruck auf Höhe h berechnen
    ph = po * exp(-data * 0.0001252);  names(ph) <- "ph"
    ## Luftdichte berechnen
    rh = ro * exp(-data * 0.0001252); names(rh) <- "rh"
    ## Temperatur auf Höhe berechnen
    Th <- 288.15 - ((6.5 * data)/1000); names(Th) <- "tempK"
  } else {
    ## Luftdruck auf Höhe h berechnen
    ph = po * exp(-data[,height] * 0.0001252);  ph
    ## Luftdichte berechnen
    rh = ro * exp(-data[,height] * 0.0001252); rh
    ## Temperatur auf Höhe berechnen
    Th <- 288.15 - ((6.5 * data[,height])/1000); Th
  }

  if (class(data)!= "data.frame") {
    data <- as.data.frame(data)
  }
  colnames(data) <- "Height"

  Celsius = Th - 273.15; names(Celsius) <- "tempC"
  data$ph <- ph
  data$rh <- rh
  data$tempK <- Th
  data$tempC <- Celsius
  colnames(data) <- names(data)

  return(data)
}




