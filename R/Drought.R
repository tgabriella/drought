setwd("D:\\Dokumentumok\\Traineeship\\Forecast_datas\\Daily_forecast")

library(ncdf4)
library(rwrfhydro)
library(rgdal)
library(raster)
library(tmap)
library(sp)
library(rgeos)

readfunction<- function(data="file"){
  print(data)
  
  
  myfunction_for_converting <- function(input = 'sth',  variable = "var"){
    
    geofile <- input
    proj4<- GetProj(geofile)
    output <- paste0(input, "_", variable, "_.tif")
    ExportGeogrid(inFile = input, inVar = variable, outFile = output)
    
  }
  
  allfiles <- dir(pattern='wrfout_d03')
  ourfiles <- as.list(allfiles[-grep(pattern = "_.tif", x = allfiles)]) 
  

  #ourfiles <- as.list(dir(pattern='wrfout_d03')) 
  lapply(ourfiles, function(x) myfunction_for_converting(input = x, variable="T2"))
  lapply(ourfiles, function(x) myfunction_for_converting(input = x, variable="U10"))
  lapply(ourfiles, function(x) myfunction_for_converting(input = x, variable="V10"))
  lapply(ourfiles, function(x) myfunction_for_converting(input = x, variable="T02_MIN"))
  lapply(ourfiles, function(x) myfunction_for_converting(input = x, variable="T02_MAX"))
  lapply(ourfiles, function(x) myfunction_for_converting(input = x, variable="T02_MEAN"))
  
}
res<- readfunction("wrfout_d03_2019-04-21_00_00_00")


  ##########################################################
  
  
  # Taking the single variables for all timestep
  U10 <- dir(pattern="U10_.tif")
  U <- stack(U10)
  V10 <- dir(pattern="V10_.tif")
  V <- stack(V10)
  
 
  wsfun<- function(x,y){
    sqrt((x^2)+(y^2)) }
  
  ws<- overlay(U,V, fun=wsfun )

  wdfun<- function(x,y){
    (57.2957*atan2(x,y)+180)}
  
  wd <- overlay(U,V, fun=wdfun)
  
  T2<-dir(pattern="T2_.tif")
  T2 <- stack(T2)
  T02_MIN<-dir(pattern="T02_MIN_.tif")
  T02_MIN <- stack(T02_MIN)
  T02_MAX<-dir(pattern="T02_MAX_.tif")
  T02_MAX <- stack(T02_MAX)
  T02_MEAN<-dir(pattern="T02_MEAN_.tif")
  T02_MEAN <- stack(T02_MEAN)
  
 # datas<-list(U,V, ws, wd, T2, T02_MIN, T02_MEAN, T02_MAX)
   

  # daily means:
  dailymean_T2<- calc(T2, mean)
  dailymean_ws<-calc(ws, mean)
  dailymean_wd<-calc(wd, mean)
  dailymean_T02_MAX<-calc(T02_MAX, mean)
  dailymean_T02_MEAN<-calc(T02_MEAN, mean)
  dailymean_T02_MIN<-calc(T02_MIN, mean)
  
  
  
  # daily maximums:
  dailymax_T2<- calc(T2, max)
  dailymax_ws<-calc(ws, max)
  dailymax_wd<-calc(wd, max)
  dailymax_T02_MAX<-calc(T02_MAX, max)
  dailymax_T02_MEAN<-calc(T02_MEAN, max)
  dailymax_T02_MIN<-calc(T02_MIN, max)
  
  
  # daily minimums:
  dailymin_T2<- calc(T2, min)
  dailymin_ws<-calc(ws, min)
  dailymin_wd<-calc(wd, min)
  dailymin_T02_MAX<-calc(T02_MAX, min)
  dailymin_T02_MEAN<-calc(T02_MEAN, min)
  dailymin_T02_MIN<-calc(T02_MIN, min)
  
  
  outputs<- list(dailymean_T2, dailymean_ws,dailymean_wd,  dailymean_T02_MAX, dailymean_T02_MIN, dailymean_T02_MEAN,
                 dailymax_T2, dailymax_ws,dailymax_wd, dailymax_T02_MAX, dailymax_T02_MIN, dailymax_T02_MEAN,
                 dailymin_T2, dailymin_ws,dailymin_wd,  dailymin_T02_MAX, dailymin_T02_MIN, dailymin_T02_MEAN)
  #return(outputs)



# Select the file and apply the function



#Figures

# Color scale
colores<- c( "#ebf7f9", "#d1ecf9", "#c1e1f5", "#a1cde7", "#87c3e2", "#81b9db", "#7b9fc0", "#7794c3",
             "#7685b8", "#737aa8", "#716a9c", "#6a569b", "#6b478b", "#6e3882", "#6a2878", "#52265e",
             "#284c4e", "#2c6546", "#3b7f33", "#508d33", "#54a637", "#63b31c", "#77ba15", "#88c015",
             "#9ec61a", "#b3cf1e", "#bfca3a", "#f8ec3e", "#fed522", "#fdc220", "#fab118", "#fb9c18",
             "#f8891f", "#e76931", "#f35820", "#eb4917", "#e33f1e", "#d73216", "#ce2518", "#cd251c",
             "#c41b18", "#9c3e3b", "#973941", "#8a3144", "#7b2e46", "#792e4c", "#6f2a4b", "#602650",
             "#5c245d", "#752178", "#7f2686", "#883094", "#8d3da3", "#9745b4", "#9f4fbe", "#a759cc",
             "#ad68d4", "#da6ae6", "#e477ea", "#e78bed", "#ee98f1", "#f1a8f2", "#f0b9f7", "#fac7fc", 
             "#f8ddfb", "#dbd9da", "#d0d0d0", "#c4c5c5", "#b9b9b9", "#b0b0b0", "#a3a3a3", "#9a9a9a", 
             "#8e8e8e", "#858585", "#797979", "#6f6f6f", "#666666", "#5a5a5a", "#4c4c4c", "#4e4e4e", 
             "#363636", "#232323", "#1c1613", "#372416", "#46331d", "#544327", "#534025", "#5d4b2b", 
             "#675a35", "#73673c", "#7f7746", "#89834a", "#969153", "#a29e62", "#acaa69", "#b9b872", 
             "#c1c780", "#c6e08b", "#b8da84", "#b2d17a", "#a5c57b", "#9dbf75", "#8eb16c", "#87a96c",
             "#6f9864", "#5c845d", "#5a815d", "#507b5a", "#507b5c", "#447056", "#2d5d4b", "#2e614e", 
             "#2d5d4b", "#23554b", "#214946", "#193841", "#2d3f4c", "#414750", "#574b53", "#6a575d", 
             "#7a5d65", "#8e646a", "#9e6a70", "#c77b7b", "#ec8c87", "#ef8c8c", "#e38685", "#d77e7c", 
             "#cc7674", "#c0706d", "#b76767", "#a6605f", "#8d5350", "#814a4a", "#724042", "#6f4546", 
             "#653d3b", "#633939", "#583634", "#573836", "#583333")



#geoFile <- "test.nc"
#proj4 <- GetProj(geoFile)
#proj4

proj4<- "+proj=lcc +lat_1=49.826000213623 +lat_2=49.826000213623 +lat_0=51.8421516418457 +lon_0=16.2469997406006 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs"

proj4<- projection(outputs[[1]])

#Read the objects which determines the selected area: counties, lakes, riwers
library(rgdal)
library(raster)
wojewodztwa <- readOGR("D:\\Dokumentumok\\Traineeship\\Forecast_datas\\Daily_forecast\\POL_adm1.shp")
pol <- readOGR("D:\\Dokumentumok\\Traineeship\\Forecast_datas\\Daily_forecast\\POL_adm0.shp")
rzeki <- readOGR("D:\\Dokumentumok\\Traineeship\\Forecast_datas\\Daily_forecast\\rzekiPL.shp") 
jeziora <- readOGR("D:\\Dokumentumok\\Traineeship\\Forecast_datas\\Daily_forecast\\ne_10m_lakes.shp")
jeziora <- (crop(pol, jeziora))


wojewodztwa <- spTransform(wojewodztwa,proj4)
pol <- spTransform(pol, proj4)
jeziora <- (crop(pol, jeziora))
jeziora <- spTransform(jeziora, proj4)
rzeki <- spTransform(rzeki, proj4)





#Plotting object (temperature)
obj<- mask(outputs[[1]]-273.15, pol)         



centroidy = gCentroid(wojewodztwa,byid=TRUE)
centroidy$temp <- round(extract(obj, centroidy),1)


breaks<-seq(0, 20, 0.2)

tm_shape(obj) +
  tm_raster(title= "Temperature", palette = colores, breaks= breaks, interpolate = F) +
  
  #Border  
  tm_shape(pol) +
  tm_polygons(alpha = 0.001, lwd=3.5) +
  
  #Border of counties 
  tm_shape(wojewodztwa)+
  tm_polygons(alpha = 0.01, lwd=0.5)+
  
  #Rivers    
  tm_shape(rzeki)+
  tm_lines(col="#2669d6", lwd=1.5) +
 
   #Lakes
  tm_shape(jeziora)+
  tm_polygons(col="#2669d6") +
  
  #Writeing the mean values of tke counties
  tm_shape(centroidy)+
  tm_text("temp") +   #mert centroidy$temp - nek nevezt?k el
  
  #Title of the figure
  tm_layout(title = "Maximum temperature [°C]: \n2019-04-21 00:00 UTC",title.size = 1,
            sepia.intensity = 0.2,legend.just = "right",title.color = "blue",
            compass.type = "arrow",title.bg.color = "white", title.bg.alpha = 0.5,title.position = c(0.02,0.06),
            legend.outside = F,
            legend.width = 1.7,
            legend.title.size = 4,
            legend.text.size = 2,
            legend.position = c("right","bottom"),
            legend.bg.color = "grey96",
            legend.height = 0.95,
            legend.frame.lwd = 1,
            legend.frame = F,
            legend.bg.alpha = 1,
            space.color="grey90")+
  #Compass
  tm_compass(position = c("left","top"), color.light = "grey90") +

  #  tm_credits("(c) WindHydro 2019", position = c(.85, 0), size = 0.7) 

  # Lon/lat
  tm_grid(projection = "longlat", x = 10:30, y=40:60, labels.col = "black", labels.size = 0.8, labels.inside.frame = T)






#Plotting variable (wind)
obj<- mask(outputs[[8]], pol)          



centroidy = gCentroid(wojewodztwa,byid=TRUE)
centroidy$wind <- round(extract(obj, centroidy),1)


breaks2<-seq(0,45,1)

tm_shape(obj ) +
  tm_raster(title= "Wind speed", palette = colores, breaks=breaks2, interpolate = F) +
  
  #Border  
  tm_shape(pol) +
  tm_polygons(alpha = 0.001, lwd=3.5) +
  
  #Border of counties 
  tm_shape(wojewodztwa)+
  tm_polygons(alpha = 0.01, lwd=0.5)+
  
  #Rivers    
  tm_shape(rzeki)+
  tm_lines(col="#2669d6", lwd=1.5) +

  #Lakes
  tm_shape(jeziora)+
  tm_polygons(col="#2669d6") +
  
  #Mean values of counties
  tm_shape(centroidy)+
  tm_text("wind") +   #mert centroidy$temp - nek nevezt?k el
  
  #Title of the figure
  tm_layout(title = "Wind speed: \n2019-04-21 00:00 UTC",title.size = 1,
            sepia.intensity = 0.2,legend.just = "right",title.color = "blue",
            compass.type = "arrow",title.bg.color = "white", title.bg.alpha = 0.5,title.position = c(0.02,0.06),
            legend.outside = F,
            legend.width = 2,
            legend.title.size = 1,
            legend.text.size = 1,
            legend.position = c("right","bottom"),
            legend.bg.color = "grey96",
            legend.height = 0.95,
            legend.frame.lwd = 1,
            legend.frame = F,
            legend.bg.alpha = 1,
            space.color="grey90")+
  #Compass
  tm_compass(position = c("left","top"), color.light = "grey90") +
  
  #  tm_credits("(c) WindHydro 2019", position = c(.85, 0), size = 0.7) 
 
  #Lon/Lat    
  tm_grid(projection = "longlat", x = 10:30, y=40:60, labels.col = "black", labels.size = 0.8, labels.inside.frame = T)



