library(sp)
library(windfarmGA)
Polygon1 <- Polygon(rbind(c(4488182, 2663172), c(4488182, 2669343),
                          c(4499991, 2669343), c(4499991, 2663172)))
Polygon1 <- Polygons(list(Polygon1), 1);
Polygon1 <- SpatialPolygons(list(Polygon1))
Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
proj4string(Polygon1) <- CRS(Projection)
DEM_meter <- getDEM(Polygon1)

turbloc = spsample(DEM_meter[[2]], 10, type = "random");

plot_farm_3d(DEM_meter[[1]], turbloc, texture="imhof4", txt = "Thingi", )
plot_farm_3d(DEM_meter[[1]], turbloc, texture="imhof4", zoom=0.8)
plot_farm_3d(DEM_meter[[1]], turbloc, z_scale= 1000, zoom1=0.4,  
             windowsize=c(1000,800),
             fov=120, theta=100, phi=10, solid=T,
             freetype=F, relativez=T, txt = "Thing",
             texture = "imhof1")

################################
montshadow = ray_shade(montereybay,zscale=50,lambert=FALSE)
montamb = ambient_shade(montereybay,zscale=50,)
montereybay %>% 
  sphere_shade(zscale=10,texture = "imhof1") %>% 
  add_shadow(montshadow,0.5) %>%
  add_shadow(montamb) %>%
  plot_3d(montereybay,zscale=50,fov=0,theta=-100,phi=30,
          windowsize=c(1000,800),zoom=0.6)

render_label(montereybay,x=350,y=240, z=4000, zscale=50, 
             text = "Moss Landing", textsize = 2, linewidth = 5)


labels = data.frame(xs = c(350,220,300,50), ys = c(240,330,130,130))
lapply(1:nrow(labels), function(i) {
  render_label(montereybay,x=labels[i,1],y=labels[i,2], z=4000, zscale=50, 
               text = paste("Label", i), linewidth = 5,
               textsize = 2, freetype = F)
})