

## Download Corine-Land-Cover Raster ######################
ccl_raster_url <-
  "https://www.eea.europa.eu/data-and-maps/data/clc-2006-raster-3/clc-2006-100m/g100_06.zip/at_download/file"
temp <- tempfile()
download.file(ccl_raster_url, temp, method = "libcurl", mode = "wb")
unzip(temp, "g100_06.tif")
unlink(temp)
ccl <- raster::raster("g100_06.tif")
usethis::use_data(ccl, overwrite = TRUE)

## Download Corine-Land-Cover Legend, save as .csv and add column for Rauhigkeit ###############
## TODO - add function to create a legend with other classification
legend_url <- "https://www.eea.europa.eu/data-and-maps/data/corine-land-cover-3/corine-land-cover-classes-and/clc_legend.csv/at_download/file"
data <- read.csv(legend_url)


## Weibull Raster ######################
## TODO - add data a github repo or somewhere accessible
a_weibull <- raster("a120_100m_Lambert.tif")
usethis::use_data(a_weibull, overwrite = TRUE)

k_weibull <- raster("k120_100m_Lambert.tif")
usethis::use_data(k_weibull, overwrite = TRUE)
