library(plyr)
library(dplyr)
library(raster)
library(windfarmGA)
# library(stars)
# library(Rcpp)

# library(windfarmGA)
# load(file = system.file("extdata/resultrect.rda", package = "windfarmGA"))
# load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))
# Grid <- GridFilter(shape = polygon,resol = 175, prop = 1, plotGrid = T)
# Polygon1 <- polygon


## cpp - tests ################
# xy1 <- coordinates(turbloc)
# xy2 <- xy1[1,]
# h1=0;
# h2=0;
# r = DEM_meter
# plot(r)
# points(xy1[,1], xy1[,2])
# points(xy2[,1], xy2[,2])


# viewTo(r, xy = turbine_locs[1,], xy2 = sample_xy, h1, h2)
a<-cansee(r,xy1 = turbine_locs[1,], xy2 = sample_xy[1,], h1,h2);a
# r=DEM_meter;
xy1 = turbine_locs[1,]; xy2 = sample_xy[1,]

dim(xyz)
length(xyz[,1])-1
xyz

h1 = xyz[["z"]][1] + h1
h2 = xyz[["z"]][np] + h2


cppFunction('
IntegerVector canseeC(NumericMatrix xyz, double h1, double h2) {
  int np = xyz.nrow() - 1;
  double h1n = xyz(0, 2) + h1;
  double h2n = xyz(np - 1, 2) + h2;
  IntegerVector yy = seq( 0, np );
  IntegerVector hpath = h1n + ((yy * (h2n - h1n)) / np);
  return hpath;
}')

canseeC(as.matrix(xyz), 0, 0)
h1=0;h2=0;

# np
length(xyz[,1])-1
#h1
h1=xyz[["z"]][1] + h1
#h2
h2=xyz[["z"]][np] + h2
# hpath
h1 + (0:np)*(h2-h1)/np


n1= 150
n2=25
a=cansee(r = DEM_meter, xy1=sample_xy[n2,], xy2 = sample_xy[n1,]);a
a=canseeCpp(r = DEM_meter, xy1=sample_xy[n2,], xy2 = sample_xy[n1,]);a


mc <- microbenchmark::microbenchmark(times=1000,
  r = cansee(r = DEM_meter, xy1=sample_xy[n2,], xy2 = sample_xy[n1,]),
  cp = canseeCpp(r = DEM_meter, xy1=sample_xy[n2,], xy2 = sample_xy[n1,])
)
mc

#################



## Functions ###########
cansee <- function(r, xy1, xy2, h1=0, h2=0){
  ### can xy1 see xy2 on DEM r?
  ### r is a DEM in same x,y, z units
  ### xy1 and xy2 are 2-length vectors of x,y coords
  ### h1 and h2 are extra height offsets
  ###  (eg top of mast, observer on a ladder etc)
  
  xyz = rasterprofile(r, xy1, xy2)
  
  np = length(xyz[,1])-1
  h1 = xyz[["z"]][1] + h1
  h2 = xyz[["z"]][np] + h2
  hpath = h1 + (0:np)*(h2-h1)/np
  # hpath = floor(hpath)
  invisible(!any(hpath < xyz[["z"]]))
}
canseeCpp <- function(r, xy1, xy2, h1=0, h2=0){
  xyz = rasterprofile(r, xy1, xy2)
  hpath = canseeC(as.matrix(xyz), h1, h2)
  invisible(!any(hpath < xyz[["z"]]))
}

viewTo <- function(r, xy, xy2, h1=0, h2=0, progress="none"){
  ## xy2 is a matrix of x,y coords (not a data frame)
  a <- aaply(xy2, 1, function(d){cansee(r,xy1 = xy,xy2 = d,h1,h2)}, .progress=progress)
  a[is.na(a)] <- FALSE
  return(a)
}

rasterprofile <- function(r, xy1, xy2){
  # r = DEM_meter; xy1 = sample_xy[1,]; xy2 = sample_xy[-1,]
  
  ### sample a raster along a straight line between two points
  ### try to match the sampling size to the raster resolution
  dx = sqrt( (xy1[1]-xy2[1])^2 + (xy1[2]-xy2[2])^2 )
  nsteps = 1 + round(dx/ min(res(r)))
  xc = xy1[1] + (0:nsteps) * (xy2[1]-xy1[1])/nsteps
  yc = xy1[2] + (0:nsteps) * (xy2[2]-xy1[2])/nsteps
  data.frame(x=xc, y=yc, z=r[raster::cellFromXY(r,cbind(xc,yc))])
}

viewshed <- function(r, shape, turbine_locs, h1=0, h2=0, progress="none"){
  r = DEM_meter; shape=shape_meter; turbine_locs = turbloc
  h1=0; h2=0;
  
  if (class(shape)[1] == "sf") {
    shape <- as(shape, "Spatial")  
  }
  if (class(turbine_locs) == "SpatialPoints") {
    turbine_locs = coordinates(turbine_locs)
  }
  
  sample_POI <- spsample(shape, n = ncell(r), type = "regular")
  sample_xy <- coordinates(sample_POI)
  
  ## xy2 is a matrix of x,y coords (not a data frame)
  res <- aaply(turbine_locs, 1, function(d){
    viewTo(r, xy = d, xy2 = sample_xy, h1, h2)
    }, .progress=progress)
  
  
  if (is.matrix(res)) {
    res <- res[1:nrow(res),1:nrow(sample_xy)]
  }
  if (is.logical(res)) {
    res[1:nrow(sample_xy)]
  }
    
  
  
  return(list("Result"=res, "Raster_POI" = sample_xy, 
              "Area" = st_as_sf(shape), "DEM" = r, "Turbines" = turbine_locs))
}
viewshed_par <- function(r, shape, turbine_locs, h1=0, h2=0, progress="none"){
  # r = DEM_meter; shape=shape_meter; turbine_locs = turbloc
  # h1=0; h2=0;
  
  if (class(shape)[1] == "sf") {
    shape <- as(shape, "Spatial")  
  }
  if (class(turbine_locs) == "SpatialPoints") {
    turbine_locs = coordinates(turbine_locs)
  }
  
  sample_POI <- spsample(shape, n = ncell(r), type = "regular")
  sample_xy <- coordinates(sample_POI)
  
  
  library(parallel)
  nCore <- detectCores()
  cl <- makeCluster(nCore)
  clusterEvalQ(cl, {
    library(plyr)
    library(raster)
  })
  clusterExport(cl, varlist = c("turbine_locs", "sample_xy", 
                                "viewTo", "cansee", "rasterprofile", 
                                "r", "h1", "h2"))
  
  res <- parApply(cl = cl, X = turbine_locs, 1, function(d){
    viewTo(r, xy = d, xy2 = sample_xy, h1, h2)
  })
  res <- t(res)
  
  stopCluster(cl)
  
  if (is.matrix(res)) {
    res <- res[1:nrow(res),1:nrow(sample_xy)]
  }
  if (is.logical(res)) {
    res[1:nrow(sample_xy)]
  }
  
  return(list("Result"=res, "Raster_POI" = sample_xy, 
              "Area" = st_as_sf(shape), "DEM" = r, "Turbines" = turbine_locs))
}

plot_viewshed <- function(res) {
  # r=DEM_meter; shape_meter
  plot(st_geometry(res[[3]]))
  plot(res[[4]], add = T)
  points(res[[2]], col="green", pch=20)
  points(res[[5]], col="black", pch=20)
  if (is.matrix(res[[1]])) {
    invisible(apply(res[[1]], 1, function(d) {points(res[[2]][d,], col="red", pch=20)}))
  } else {
    points(res[[2]][res[[1]],], col="red", pch=20)
    # invisible(apply(res[[1]], 1, function(d) {points(res[[2]][d,], col="red", pch=20)}))
  }
}

interpol_view <- function(res, plot=TRUE, breakseq, plotDEM=FALSE, ...) {
  
  if (nrow(res$Result) > 1) {
    res$Result <- apply(res$Result, 2, function(d) {
      sum(d)
    })
  }
  
  rasterpois <- cbind(res$Raster_POI, "z" = res$Result)
  visible = rasterize(res$Raster_POI, res$DEM, field = res$Result, fun = mean)
  if (plot) {
    # raster::plot(visible, col=c("green", "red"))
    
    pal <- colorRampPalette(c("green","orange","red"))
    maxR = max(rasterpois[,3])
    
    if (missing(breakseq)) {
      breakseq = seq(0,maxR,0.2)
      # breakseq <- as.numeric(quantile(rasterpois[,3]))
      if (!any(breakseq == maxR)) {
        breakseq <- c(breakseq, maxR)
        # breakseq <- as.numeric(quantile(rasterpois[,3]))
      }
      breakseq <- breakseq[!duplicated(breakseq)]
    } 
    # print(breakseq)
    
    if (plotDEM) {
      raster::plot(res$DEM, legend = F)
      raster::plot(visible, breaks=breakseq, add = T, col=pal(length(breakseq)), ...)
    } else {
      raster::plot(visible, breaks=breakseq, col=pal(length(breakseq)), ...)
      # raster::plot(visible, breaks=breakseq, col=pal(length(breakseq)))
    }
    
    points(res$Turbines, pch=20, col="black", cex=1.5)
  }
  return(visible)
}

##### Get DATA ################
PROJ <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

library(mapedit)
library(mapview)
shape <- mapview() %>%
  editMap(targetLayerId = "pol")
shape <- shape$drawn
# plot(shape)

library(rworldmap)
## check counry ISO code
countriesSP <- getMap(resolution='low')
pointsSP = spTransform(sample_POI, CRSobj = CRS(proj4string(countriesSP)))  
# use 'over' to get indices of the Polygons object containing each point 
indices = over(pointsSP, countriesSP)
# return the ADMIN names of each country
unique(as.character(indices$ISO3))
ISOCODE <- unique(as.character(indices$ISO3))


# DEM <- getData("SRTM", lon = st_bbox(shape)[1], lat=st_bbox(shape)[2])
DEM <- getData("alt", country=ISOCODE)

library(sf)
# shape <- st_as_sf(shape)
shape <- st_transform(shape, crs = projection(DEM))
shape_SP <- as(shape, "Spatial")

DEM_clip <- crop(x = DEM, extent(shape_SP))
# DEM_clip <- mask(DEM_clip, mask = shape_SP)


DEM_meter <- projectRaster(DEM_clip, crs = PROJ)
shape_meter <- st_transform(shape, PROJ)
shape_SP <- spTransform(shape_SP, CRSobj = crs(PROJ))



sample_POI <- spsample(shape_SP, n = ncell(DEM_meter), type = "regular")


############################

sample_xy <- coordinates(sample_POI)
class(sample_xy)

n=145
plot(st_geometry(shape_meter))
plot(DEM_meter, add=T)
points(sample_POI, pch=20, col="red")

##########################



turbloc = spsample(shape_SP, 10, type = "random");length(turbloc)
res <- viewshed(r = DEM_meter, shape=shape_meter, turbine_locs = turbloc,  h1=1.8, h2=50)

plot_viewshed(res)
interpol_view(res, plot = T)
plot(st_geometry(shape_meter), add=T)


## RAYSHADER #########################
# install.packages("rayshader")
# devtools::install_github("tylermorganwall/rayshader")
library(rayshader)
packageVersion("rayshader")

plot_farm_3d <- function(DEM, turbines, z_scale= 1000, zoom1=0.4, 
                         windowsize=c(1000,800),
                         fov=10, theta=-100, phi=10, solid=T,
                         freetype=F, relativez=T, labl = "Turbine",
                         texture = "imhof1") {

  
  # DEM <- DEM_meter[[1]]; turbines = turbloc
  # z_scale= 1000; zoom1=0.4; windowsize=c(1000,800)
  # fov=10; theta=-100; phi=10; solid=T
  # freetype=F; relativez=T; labl = "Turbine"
  # texture = "imhof1";
  
  DEM_matrix = t(as.matrix(DEM))
  a <- matrix(raster::extract(DEM,raster::extent(DEM),buffer=1000),
         nrow=ncol(DEM),ncol=nrow(DEM))
  identical(DEM_matrix, a)
  
  library(microbenchmark)
  mc = microbenchmark(
    old = matrix(raster::extract(DEM,raster::extent(DEM),buffer=1000),
                 nrow=ncol(DEM),ncol=nrow(DEM)),
    new = t(as.matrix(DEM))
  )
  mc

  
  turbloc_proj <- sp::spTransform(turbines, CRSobj = raster::crs(DEM))
  turbines_df <- as.data.frame(sp::coordinates(turbloc_proj))
  
  # raster_indic <- raster::cellFromXY(DEM, turbines_df)
  raster_x <- (raster::colFromX(DEM, x = turbines_df$x))
  raster_y <- (raster::rowFromY(DEM, y = turbines_df$y))
  
  ## Merge with DEM_matrix to get z-values
  z_vals <- raster::extract(DEM, turbloc_proj)
  turbines_df$z <- z_vals 
  
  ## fill NAs?
  i=1
  raster_x[i]
  raster_y[i]
  turbines_df$x[i]
  turbines_df$y[i]
  turbines_df$z[i]
  DEM_matrix[raster_x[i],raster_y[i]]
  
  plot(DEM_meter[[1]])
  points(turbines_df$x[i],turbines_df$y[i], pch=20)
  
  shadow = ray_shade(DEM_matrix, zscale=z_scale, lambert=FALSE)
  amb = ambient_shade(DEM_matrix,zscale=z_scale, sunbreaks=30, maxsearch=40)
  DEM_matrix %>%
    sphere_shade(zscale=z_scale, texture = texture) %>%
    add_shadow(shadow, 0.5) %>%
    add_shadow(amb) %>%
    plot_3d(DEM_matrix, zscale=z_scale, fov=fov, theta=theta, phi=phi, windowsize=windowsize,
            solid=solid, zoom=zoom1)

  a <- lapply(1:length(raster_x), function(i) {
    i=2
    render_label(DEM_matrix, x=raster_x[i], y=raster_y[i], z=turbines_df$z[i], freetype = freetype, 
                 zscale=z_scale, text = paste(labl, i), relativez = relativez)
                 # zscale=z_scale, text = paste(labl, i), relativez = FALSE)
  })
  
  return(NULL)
}
plot_farm_3d(DEM_meter[[1]], turbloc)
plot_farm_3d(DEM_meter[[1]], turbloc, texture="imhof4")
plot_farm_3d(DEM_meter[[1]], turbloc, texture="imhof4", zoom=0.8)
plot_farm_3d(DEM_meter[[1]], turbloc, z_scale= 1000, zoom1=0.4, 
             windowsize=c(1000,800),
             fov=120, theta=100, phi=10, solid=T,
             freetype=F, relativez=T, labl = "Turbine",
             texture = "imhof1")

library(rgl)
# rgl::text3d()
# rgl::par3d()
obj_turb <- readOBJ(con = "C:/Users/gatscha/Desktop/ShinyDemos/OBJ/Turbine/17489_Wind_Turbine_v1.obj")
shade3d(obj_turb)


## Benchmark ############
library(microbenchmark)

mc <- microbenchmark(times = 10,
  old = viewshed(r = DEM_meter, shape=shape_meter, turbine_locs = turbloc,  h1=1.8, h2=50),
  par = viewshed_par(r = DEM_meter, shape=shape_meter, turbine_locs = turbloc,  h1=1.8, h2=50)
)
mc


if(!require(installr)) {
  install.packages("installr"); require(installr)} #load / install+load installr


par(mfrow = c(1,1))
plot_viewshed(res)
interpol_view(res, alpha=1, breakseq=c(0,0.1,0.3,1))
interpol_view(res, alpha=1)

par(mfrow = c(1,2))
interpol_view(res, alpha=1, cex=1.5)
plot(DEM_meter)
points(turbloc, pch=20, col="black", cex=1.5)



## visibility.R tests ---------------------
library(sp)
library(raster)
library(plyr)
library(sf)
library(rworldmap)
library(dplyr)


Polygon1 <- Polygon(rbind(c(4488182, 2663172), c(4488182, 2669343),
                          c(4499991, 2669343), c(4499991, 2663172)))
Polygon1 <- Polygons(list(Polygon1), 1);
Polygon1 <- SpatialPolygons(list(Polygon1))
Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
proj4string(Polygon1) <- CRS(Projection)
plot(Polygon1)
DEM_meter <- getDEM(Polygon1)

sample_POI <- spsample(DEM_meter[[2]], n = ncell(DEM_meter[[1]]), type = "regular")
sample_xy <- coordinates(sample_POI)

turbloc = spsample(DEM_meter[[2]], 10, type = "random");
res <- viewshed(r = DEM_meter[[1]], shape=DEM_meter[[2]], turbine_locs = turbloc,  h1=1.8, h2=5)
plot_viewshed(res)
plot_viewshed(res, leg = T)
intviw <- interpol_view(res, plotDEM = T)
interpol_view(res, plotDEM = T, alpha=0.5)
interpol_view(res)
interpol_view(res, breakform=quantile)
interpol_view(res, breakform=fivenum)
interpol_view(res, breakform=IQR)
interpol_view(res, breakform=factor)
interpol_view(res, fun=mean)

interpol_view(res, plotDEM = F, breakform=sum, breakseq = seq(0,max(colSums(res$Result)),0.5))
interpol_view(res, plotDEM = F, breakseq = seq(0,6,1), colNA="black")
interpol_view(res, plotDEM = F, breakseq = seq(-1,10,1), colNA="black")
plot(DEM_meter[[2]], add=T)

## editMap with OSM Search ---------------------
# library(mapedit)
# library(mapview)
# library(leaflet)
# library(leaflet.extras)
# polygon = editMap(  
#   leaflet() %>% 
#     leaflet::addTiles(group = "OSM") %>%
#     leaflet::addProviderTiles("Stamen.Terrain", group="Terrain") %>%
#     leaflet::addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
#     leaflet::addProviderTiles("Stamen.Toner", group = "Toner") %>%
#     setView(12, 49, 4) %>% 
#     addSearchOSM() %>% 
#     leaflet::addLayersControl(baseGroups = c(
#       "OSM",
#       "Terrain",
#       "Satellite",
#       "Toner"),
#       options = leaflet::layersControlOptions(collapsed = T)
#     )
# )
# is.null(polygon$finished)
# poly <- polygon$finished
# polygon = as(polygon$finished, "Spatial")
# plot(st_geometry(poly))