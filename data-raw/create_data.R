## Create some Polygons ###################
library(sf)
Projection <- 3035
Proj84 <- 4326

cordslist <- list(structure(c(15.8862501316651, 15.8802827241768, 15.8814016130809, 
                              15.9090008727144, 15.9168330950428, 15.9265301322114, 15.9608427252693, 
                              15.9668101327576, 15.9970201331673, 16.0167871704724, 15.9884419849028, 
                              15.9709127254058, 15.9168330950428, 15.9145953172347, 15.8582779090636, 
                              15.8500727237671, 15.8608886498397, 15.8862501316651, 47.5568250061956, 
                              47.5522942591554, 47.5399585741679, 47.5392032338379, 47.5522942591554, 
                              47.5641237191238, 47.5583351681567, 47.5225829933608, 47.5160341797293, 
                              47.4918468635213, 47.4908388168251, 47.4739511582908, 47.4794969582227, 
                              47.4986506726316, 47.5006664467725, 47.5273681479331, 47.5505321961794, 
                              47.5568250061956), .Dim = c(18L, 2L)))
big_shape <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(cordslist), 
  crs = Proj84
))
big_shape <- st_transform(big_shape, st_crs(Projection))
usethis::use_data(big_shape, overwrite = TRUE)



# cordslist <- list(structure(c(15.0817690096444, 15.0830743800325, 15.0950091950091, 
#                               15.0976199357853, 15.0862445652606, 15.0771069725441, 15.0817690096444, 
#                               47.1150101971301, 47.1148832909329, 47.1051106048956, 47.0983829082324, 
#                               47.0938126671506, 47.1059991050483, 47.1150101971301), .Dim = c(7L, 2L)), 
#                   structure(c(15.092957898685, 15.0942632690731, 15.1032143803056, 
#                               15.1073169729538, 15.0985523432053, 15.092771417201, 15.0875499356487, 
#                               15.092957898685, 47.1233853370016, 47.1269380283696, 47.1231315642533, 
#                               47.1153909139058, 47.1122181908802, 47.1142487554071, 47.1175482575189, 
#                               47.1233853370016), .Dim = c(8L, 2L)), 
#                   structure(c(15.1308136399391, 
#                               15.1321190103272, 15.127829936195, 15.1183193805104, 15.1151491952822, 
#                               15.1095547507619, 15.1088088248259, 15.111419565602, 15.1201841953505, 
#                               15.1308136399391, 47.1309979565454, 47.1202130906391, 47.1161523392859, 
#                               47.1148832909329, 47.1176751573605, 47.1250348303582, 47.1318860245243, 
#                               47.1345501394874, 47.1344232798964, 47.1309979565454), .Dim = c(10L, 2L)))
# multi_shape <- sf::st_as_sf(sf::st_sfc(
#   sf::st_polygon(cordslist), 
#   crs = Proj84
# ))
# multi_shape <- st_transform(multi_shape, st_crs(Projection))
multi_shape <- structure(list(geometry = structure(list(structure(list(list(
  structure(c(4706637.78558716, 4706737.60253152, 4707713.80970733, 
              4707961.29557821, 4707133.01838236, 4706350.92354101, 4706637.78558716, 
              2680054.22040814, 2680046.94334248, 2679025.59536289, 2678293.295334, 
              2677727.24251873, 2679030.85582766, 2680054.22040814), .Dim = c(7L, 
                                                                              2L))), list(structure(c(4707423.53324718, 4707496.17058471, 
                                                                                                      4708202.20440833, 4708570.20940268, 4707929.74466997, 4707476.83619554, 
                                                                                                      4707056.98074784, 4707423.53324718, 2681041.10586442, 2681441.82034079, 
                                                                                                      2681066.50590348, 2680229.71267987, 2679832.13928397, 2680027.11186695, 
                                                                                                      2680365.729403, 2681041.10586442), .Dim = c(8L, 2L))), list(structure(c(4710233.9640452, 
                                                                                                                                                                              4710413.02205149, 4710118.34571033, 4709407.37701243, 4709146.54928107, 
                                                                                                                                                                              4708668.30978493, 4708561.09271218, 4708739.0516252, 4709403.64495557, 
                                                                                                                                                                              4710233.9640452, 2682083.27204177, 2680894.38466092, 2680421.63001891, 
                                                                                                                                                                              2680231.03457879, 2680523.96775665, 2681310.67868044, 2682066.39980067, 
                                                                                                                                                                              2682375.44038217, 2682407.27559588, 2682083.27204177), .Dim = c(10L, 
                                                                                                                                                                                                                                              2L)))), class = c("XY", "MULTIPOLYGON", "sfg"))), class = c("sfc_MULTIPOLYGON", 
                                                                                                                                                                                                                                                                                                          "sfc"), precision = 0, bbox = structure(c(xmin = 4706350.92354101, 
                                                                                                                                                                                                                                                                                                                                                    ymin = 2677727.24251873, xmax = 4710413.02205149, ymax = 2682407.27559588
                                                                                                                                                                                                                                                                                                          ), class = "bbox"), crs = structure(list(input = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", 
                                                                                                                                                                                                                                                                                                                                                   wkt = "BOUNDCRS[\n    SOURCECRS[\n        PROJCRS[\"unknown\",\n            BASEGEOGCRS[\"unknown\",\n                DATUM[\"Unknown based on GRS80 ellipsoid\",\n                    ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n                        LENGTHUNIT[\"metre\",1],\n                        ID[\"EPSG\",7019]]],\n                PRIMEM[\"Greenwich\",0,\n                    ANGLEUNIT[\"degree\",0.0174532925199433],\n                    ID[\"EPSG\",8901]]],\n            CONVERSION[\"unknown\",\n                METHOD[\"Lambert Azimuthal Equal Area\",\n                    ID[\"EPSG\",9820]],\n                PARAMETER[\"Latitude of natural origin\",52,\n                    ANGLEUNIT[\"degree\",0.0174532925199433],\n                    ID[\"EPSG\",8801]],\n                PARAMETER[\"Longitude of natural origin\",10,\n                    ANGLEUNIT[\"degree\",0.0174532925199433],\n                    ID[\"EPSG\",8802]],\n                PARAMETER[\"False easting\",4321000,\n                    LENGTHUNIT[\"metre\",1],\n                    ID[\"EPSG\",8806]],\n                PARAMETER[\"False northing\",3210000,\n                    LENGTHUNIT[\"metre\",1],\n                    ID[\"EPSG\",8807]]],\n            CS[Cartesian,2],\n                AXIS[\"(E)\",east,\n                    ORDER[1],\n                    LENGTHUNIT[\"metre\",1,\n                        ID[\"EPSG\",9001]]],\n                AXIS[\"(N)\",north,\n                    ORDER[2],\n                    LENGTHUNIT[\"metre\",1,\n                        ID[\"EPSG\",9001]]]]],\n    TARGETCRS[\n        GEOGCRS[\"WGS 84\",\n            DATUM[\"World Geodetic System 1984\",\n                ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                    LENGTHUNIT[\"metre\",1]]],\n            PRIMEM[\"Greenwich\",0,\n                ANGLEUNIT[\"degree\",0.0174532925199433]],\n            CS[ellipsoidal,2],\n                AXIS[\"latitude\",north,\n                    ORDER[1],\n                    ANGLEUNIT[\"degree\",0.0174532925199433]],\n                AXIS[\"longitude\",east,\n                    ORDER[2],\n                    ANGLEUNIT[\"degree\",0.0174532925199433]],\n            ID[\"EPSG\",4326]]],\n    ABRIDGEDTRANSFORMATION[\"Transformation from unknown to WGS84\",\n        METHOD[\"Position Vector transformation (geog2D domain)\",\n            ID[\"EPSG\",9606]],\n        PARAMETER[\"X-axis translation\",0,\n            ID[\"EPSG\",8605]],\n        PARAMETER[\"Y-axis translation\",0,\n            ID[\"EPSG\",8606]],\n        PARAMETER[\"Z-axis translation\",0,\n            ID[\"EPSG\",8607]],\n        PARAMETER[\"X-axis rotation\",0,\n            ID[\"EPSG\",8608]],\n        PARAMETER[\"Y-axis rotation\",0,\n            ID[\"EPSG\",8609]],\n        PARAMETER[\"Z-axis rotation\",0,\n            ID[\"EPSG\",8610]],\n        PARAMETER[\"Scale difference\",1,\n            ID[\"EPSG\",8611]]]]"), class = "crs"), n_empty = 0L)), row.names = 1L, class = c("sf", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "data.frame"), sf_column = "geometry", agr = structure(integer(0), class = "factor", .Label = c("constant", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "aggregate", "identity"), .Names = character(0)))

usethis::use_data(multi_shape, overwrite = TRUE)





cordlist <- structure(c(4585270.60241151, 4584979.13247779, 4585868.18527401, 
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
                        1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(22L, 4L), .Dimnames = list(
                          NULL, c("X", "Y", "L1", "L2")))
df <- data.frame(cordlist)
df <- split(df[,1:2], df[,"L1"])
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
sp_polygon <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4498482, 4498482, 4499991, 4499991, 4498482),
    c(2668272, 2669343, 2669343, 2668272, 2668272)))),
  crs = 3035
))
usethis::use_data(sp_polygon, overwrite = TRUE)

winddat <- data.frame(ws = 12, wd = 0)
# plotWindrose(winddat, "ws", "wd")
resultrect <- genetic_algorithm(Polygon1 = sp_polygon,
                                n = 12, iteration = 200,
                                vdirspe = winddat,
                                Rotor = 30, RotorHeight = 100)
# plot_windfarmGA(resultrect, sp_polygon)
usethis::use_data(resultrect, overwrite = TRUE)


resulthex <- genetic_algorithm(Polygon1 = sp_polygon, GridMethod ="h", 
                               n=12, iteration=10, 
                               vdirspe = winddat,
                               Rotor=30, RotorHeight = 100)
# plot_windfarmGA(resulthex, sp_polygon, GridMethod = "h")
usethis::use_data(resulthex, overwrite = TRUE)

