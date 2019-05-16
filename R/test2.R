library(tmap)
library(raster)
library(rgdal)

T2 <- "data/t2.tif"
T2 <- raster(T2)-273.15
proj4 <- projection(T2)

pol <- readOGR("data/POL_adm0.shp")
pol <- spTransform(pol, proj4)

obj <- mask(T2, pol)
obj <- stack(obj, obj+5)


breaks <-seq(-51, 51, 1)

tempcolores <- c("#f6c39f","#e3ac89","#cb9881","#b58575","#9c716e","#865c62","#704754",
                "#57344a","#3f1f3f","#240d2b","#260225","#2e0331","#370938","#420a40",
                "#431243","#481046","#571658","#5e185e","#5f1b60","#671e67","#6d2069",
                "#853a85","#964299","#9a559d","#a665a3","#ae74a9","#b485b3","#ba93b9",
                "#c6a5c5","#cbb4cb","#d3c1d2","#c3cad5","#b6b7c6","#9ca4b9","#8992b0",
                "#5e689b","#5e699d","#48528f","#374182","#1d2e77","#0b1761","#162e74",
                "#234080","#37578c","#456f9a","#5a88ab","#78b2c4","#9fdbdc","#b1f0ee",
                "#83c9a7","#72c29a","#67b78c","#69ba8f","#61b080","#56a573","#4c9d64",
                "#3a9152","#368a45","#2a7f39","#2b7234","#1d681c","#29741a","#44851e",
                "#578c25","#759c2b","#84a935","#afbf3c","#d8d952","#d4d755","#efe362",
                "#e9d04f","#e1b845","#d9a53f","#c68f3d","#cc8c38","#c27b31","#ba6323",
                "#b74d22","#ac4e28","#9f2715","#7b1b11","#80110c","#741105","#6f0d07",
                "#630c06","#5a0c0c","#540904","#4b0504","#400401","#3f0101","#2d0708",
                "#442321","#583e3a","#6f5652","#866e6a","#9c8982","#b2a59c","#c8bcb1",
                "#c9bdb1","#ddd5c9","#f5efe3","#f4efe3")

# our legend:
image(matrix(-51:51), breaks = -51:51, col = tempcolores, xaxt= 'n', yaxt='n')
axis(1, at=0:102/102, labels = -51:51)


tm_shape(obj) +
  tm_raster(
    title = "title"  ,
    palette = tempcolores,
    breaks = breaks,
    interpolate = FALSE, 
    legend.is.portrait = FALSE
  )  + tm_shape(pol) +
  tm_borders(lwd=3.5, col="black") +
  tm_layout(#title = "Maximum temperature [*C]: \n2019-04-21 00:00 UTC",title.size = 1,
            sepia.intensity = 0.2,title.color = "blue",
            compass.type = "arrow",title.bg.color = "white", title.bg.alpha = 0.5,
            legend.outside =TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            legend.bg.color = "grey96",
            legend.frame = F,
            legend.bg.alpha = 1,
            space.color="grey90")+
  #Compass
  tm_compass(position = c("left","top"), color.light = "grey90") +
  
  #  tm_credits("(c) WindHydro 2019", position = c(.85, 0), size = 0.7) 
  
  # Lon/lat
  tm_grid(projection = "longlat", x = 10:30, y=40:60, labels.col = "black", labels.size = 0.8, labels.inside.frame = T)
  

# a tak to wyglada w plocie:
plot(obj, breaks = -51:51, col=tempcolores)
plot(pol, add=T)
# dodajmy +5
plot(obj+5, breaks = -51:51, col=tempcolores)
