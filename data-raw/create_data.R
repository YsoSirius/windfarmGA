## Create some Polygons ###################
library(sf)
Projection <- 3035
Proj84 <- 4326

cordslist <- list(structure(c(
  15.8862501316651, 15.8802827241768, 15.8814016130809,
  15.9090008727144, 15.9168330950428, 15.9265301322114, 15.9608427252693,
  15.9668101327576, 15.9970201331673, 16.0167871704724, 15.9884419849028,
  15.9709127254058, 15.9168330950428, 15.9145953172347, 15.8582779090636,
  15.8500727237671, 15.8608886498397, 15.8862501316651, 47.5568250061956,
  47.5522942591554, 47.5399585741679, 47.5392032338379, 47.5522942591554,
  47.5641237191238, 47.5583351681567, 47.5225829933608, 47.5160341797293,
  47.4918468635213, 47.4908388168251, 47.4739511582908, 47.4794969582227,
  47.4986506726316, 47.5006664467725, 47.5273681479331, 47.5505321961794,
  47.5568250061956
), .Dim = c(18L, 2L)))
big_shape <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(cordslist),
  crs = Proj84
))
big_shape <- st_transform(big_shape, st_crs(Projection))
usethis::use_data(big_shape, overwrite = TRUE)



multi_shape <- structure(list(geometry = structure(list(structure(list(list(
  structure(c(
    4706637.78558716, 4706737.60253152, 4707713.80970733,
    4707961.29557821, 4707133.01838236, 4706350.92354101, 4706637.78558716,
    2680054.22040814, 2680046.94334248, 2679025.59536289, 2678293.295334,
    2677727.24251873, 2679030.85582766, 2680054.22040814
  ), .Dim = c(
    7L,
    2L
  ))
), list(structure(c(
  4707423.53324718, 4707496.17058471,
  4708202.20440833, 4708570.20940268, 4707929.74466997, 4707476.83619554,
  4707056.98074784, 4707423.53324718, 2681041.10586442, 2681441.82034079,
  2681066.50590348, 2680229.71267987, 2679832.13928397, 2680027.11186695,
  2680365.729403, 2681041.10586442
), .Dim = c(8L, 2L))), list(structure(c(
  4710233.9640452,
  4710413.02205149, 4710118.34571033, 4709407.37701243, 4709146.54928107,
  4708668.30978493, 4708561.09271218, 4708739.0516252, 4709403.64495557,
  4710233.9640452, 2682083.27204177, 2680894.38466092, 2680421.63001891,
  2680231.03457879, 2680523.96775665, 2681310.67868044, 2682066.39980067,
  2682375.44038217, 2682407.27559588, 2682083.27204177
), .Dim = c(
  10L,
  2L
)))), class = c("XY", "MULTIPOLYGON", "sfg"))), class = c(
  "sfc_MULTIPOLYGON",
  "sfc"
), precision = 0, bbox = structure(c(
  xmin = 4706350.92354101,
  ymin = 2677727.24251873, xmax = 4710413.02205149, ymax = 2682407.27559588
), class = "bbox"), n_empty = 0L)), row.names = 1L, class = c(
  "sf",
  "data.frame"
), sf_column = "geometry", agr = structure(integer(0), class = "factor", .Label = c(
  "constant",
  "aggregate", "identity"
), .Names = character(0)))
st_crs(multi_shape) <- Projection

usethis::use_data(multi_shape, overwrite = TRUE)





cordlist <- structure(c(
  4585270.60241151, 4584979.13247779, 4585868.18527401,
  4586290.18668221, 4587632.0127656, 4588005.31974706, 4587933.05332353,
  4586395.52121144, 4586361.43925126, 4585360.80689825, 4585175.08261164,
  4584678.33214126, 4584402.04363248, 4584174.83760134, 4585270.60241151,
  4585976.13994593, 4585791.24297194, 4586035.56149181, 4586465.59095286,
  4586802.20350957, 4586755.56930998, 4585976.13994593, 2655882.39876357,
  2656642.18821161, 2655893.72473733, 2655340.35909448, 2654724.59708819,
  2654018.63277193, 2652784.8782917, 2652651.15264346, 2653278.66439827,
  2653888.59490599, 2654286.23636729, 2654391.02616789, 2655062.69468666,
  2655886.82997589, 2655882.39876357, 2654869.24044678, 2654754.8149049,
  2654298.7450985, 2653940.44349285, 2654284.70017805, 2654449.48867358,
  2654869.24044678, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1
), .Dim = c(22L, 4L), .Dimnames = list(
  NULL, c("X", "Y", "L1", "L2")
))
df <- data.frame(cordlist)
df <- split(df[, 1:2], df[, "L1"])
names(df) <- NULL
hole_shape <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(lapply(df, as.matrix)),
  crs = Proj84
))
# plot(hole_shape, col="red")
usethis::use_data(hole_shape, overwrite = TRUE)

## Download Corine-Land-Cover Raster ######################
ccl_raster_url <-
  "https://www.eea.europa.eu/data-and-maps/data/clc-2006-raster-3/clc-2006-100m/g100_06.zip/at_download/file"
temp <- tempfile()
download.file(ccl_raster_url, temp, method = "libcurl", mode = "wb")
unzip(temp, "g100_06.tif")
unlink(temp)
ccl <- terra::rast("g100_06.tif")
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
sp_polygon <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4498482, 4498482, 4499991, 4499991, 4498482),
    c(2668272, 2669343, 2669343, 2668272, 2668272)
  ))),
  crs = 3035
))
usethis::use_data(sp_polygon, overwrite = TRUE)

winddat <- data.frame(ws = 12, wd = 0)
resultrect <- genetic_algorithm(
  Polygon1 = sp_polygon,
  n = 12, iteration = 200,
  vdirspe = winddat,
  Rotor = 30, RotorHeight = 100
)
# plot_windfarmGA(resultrect, sp_polygon)
usethis::use_data(resultrect, overwrite = TRUE)


resulthex <- genetic_algorithm(
  Polygon1 = sp_polygon, GridMethod = "h",
  n = 12, iteration = 10,
  vdirspe = winddat,
  Rotor = 30, RotorHeight = 100
)
# plot_windfarmGA(resulthex, sp_polygon, GridMethod = "h")
usethis::use_data(resulthex, overwrite = TRUE)
