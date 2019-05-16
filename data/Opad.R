setwd("D:\\Dokumentumok\\Traineeship\\Forecast_datas\\Daily_forecast\\drought\\data")


library(ncdf4)
library(rwrfhydro)
library(rgdal)
library(raster)
library(tmap)
library(sp)
library(rgeos)
library(rgdal)

opadfiles<-dir(pattern = "opad")


jan<- dir(pattern = "opad_2015.01.")
feb<- dir(pattern = "opad_2015.02.")
mar<- dir(pattern = "opad_2015.03.")
apr<- dir(pattern = "opad_2015.04.")
maj<- dir(pattern = "opad_2015.05.")
jun<- dir(pattern = "opad_2015.06.")
jul<- dir(pattern = "opad_2015.07.")
aug<- dir(pattern = "opad_2015.08.")
sep<- dir(pattern = "opad_2015.09.")
oct<- dir(pattern = "opad_2015.10.")
nov<- dir(pattern = "opad_2015.11.")
dec<- dir(pattern = "opad_2015.12.")


##########################  opad  #######################################

library(raster)
library(dplyr)
library(lubridate)

df1 <- data.frame(date = seq.Date(as.Date("2015-01-01"), as.Date("2015-12-31"), by="day"))
df1$decade <- ceiling((day(df1$date)-0.1)/10)
df1$decade <- ifelse(df1$decade>3, 3, df1$decade) + (month(df1$date)-1)*3
df1$filename <- paste0("opad_", format(as.Date(df1$date), "%Y.%m.%d"), ".tif")

 

sumfunction<- function(input="inp"){
  r1<-stack(input)
  r1<- calc(r1, function(x) sum(x, na.rm=T))
}

#Period apr-oct.

files1<-list()
prec_cum<-list()

for(i in 10:30){

  files1[[i]]<- df1[df1$decade==i,"filename"]


prec_cum[[i]]<-sumfunction(files1[[i]])

}

#files12 <- df1[df1$decade %in% 10:30,"filename"]
#prec_cum<- lapply(files12, function(x) sumfunction(input=x))

#################################### temperatura ###################################x


tempfiles<-dir(pattern = "tavg_2015.")


df2 <- data.frame(date = seq.Date(as.Date("2015-01-01"), as.Date("2015-12-31"), by="day"))
df2$decade <- ceiling((day(df2$date)-0.1)/10)
df2$decade <- ifelse(df2$decade>3, 3, df2$decade) + (month(df2$date)-1)*3
df2$filename <- paste0("tavg_", format(as.Date(df2$date), "%Y.%m.%d"), ".tif")


meanfunction<- function(input="inp"){
  
  r2<-stack(input)
  r2<-calc(r2,function(x) mean(x,na.rm=T))
}


#Period apr-oct.

files2<-list()
mean_temp<-list()

for(i in 10:30){
  
  files2[[i]]<- df2[df2$decade==i,"filename"]
  
  
  mean_temp[[i]]<-meanfunction(files2[[i]])
  
}




files3 <- df1[df1$decade %in% 4:6,"filename"]


r3 <- stack(files3)
r3 <- calc(r3, function(x) sum(x, na.rm=T))

#plot(mean_temp)

proj4<- projection(mean_temp[[2]])
proj4<- projection(r3)



# Creating figure

#color scale
tempcolores<- c("#f6c39f","#e3ac89","#cb9881","#b58575","#9c716e","#865c62","#704754",
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









#Read the objects which determines the selected area: counties, lakes, riwers

wojewodztwa <- readOGR("D:\\Dokumentumok\\Traineeship\\Forecast_datas\\Daily_forecast\\POL_adm1.shp")
pol <- readOGR("D:\\Dokumentumok\\Traineeship\\Forecast_datas\\Daily_forecast\\POL_adm0.shp")
rzeki <- readOGR("D:\\Dokumentumok\\Traineeship\\Forecast_datas\\Daily_forecast\\rzekiPL.shp") 
jeziora <- readOGR("D:\\Dokumentumok\\Traineeship\\Forecast_datas\\Daily_forecast\\ne_10m_lakes.shp")
jeziora <- (crop(pol, jeziora))



wojewodztwa <- spTransform(wojewodztwa,proj4)
pol <- spTransform(pol, proj4)
jeziora <- spTransform(jeziora, proj4)
rzeki <- spTransform(rzeki, proj4)


# Creating the map of av.temperature
figures_temp<- function(input="in"){
  print(input)
  
  
  obj<- mask(input, pol)
  
  
  centroidy = gCentroid(wojewodztwa,byid=TRUE)
  centroidy$var <- round(extract(obj, centroidy),1)
  
  tit<-substring(input, first= 1, last = 4)

  breaks <-seq(-51, 51, 1)
  
  tm_shape(obj) +
    tm_raster(title= "Av_temp"  ,palette = tempcolores, breaks=breaks, interpolate = T)  +
    
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
    tm_text("var") +   
    
    #Title of the figure
    tm_layout(title = "Mean temperature",title.size = 1,
              aes.palette = "div",
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
    
    tm_credits("(c) WXCHARTS", position = c(.825, .04), size = 0.6) +
    
    #Lon/Lat    
    tm_grid(projection = "longlat", x = 10:30, y=40:60, labels.col = "black", labels.size = 0.8, labels.inside.frame = T)
  
  print(substring(input, first= 6, last = 15))
  print(tit)
}


figures_temp(input = mean_temp[[2]] ) 

map<- lapply(mean_temp, function(x) figures_temp(input = x))

