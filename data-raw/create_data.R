

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



## Result with Rect and 200 Iteration ##################
sp_polygon <- Polygon(rbind(c(4498482, 2668272), c(4498482, 2669343),
                            c(4499991, 2669343), c(4499991, 2668272)))
sp_polygon <- Polygons(list(sp_polygon), 1)
sp_polygon <- SpatialPolygons(list(sp_polygon))
projection <- paste("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000",
                    "+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
proj4string(sp_polygon) <- CRS(projection)
usethis::use_data(sp_polygon, overwrite = TRUE)

winddat <- data.frame(ws = 12, wd = 0)
# plotWindrose(winddat, "ws", "wd")
resultrect <- genAlgo(Polygon1 = sp_polygon,
                      n = 12, iteration = 200,
                      vdirspe = winddat,
                      Rotor = 30,
                      RotorHeight = 100)
usethis::use_data(resultrect, overwrite = TRUE)