start.time <- Sys.time()
#Daiel Woodrich
#August 2017
#AFSC
#daniel.woodrich@noaa.gov, d.woodrich1@gmail.com
#V8: Works with internal months with no data (data gaps). 


#Usable by any AFSC PC with Acoustics permission. Create multiplots and gifs that summarize presence. Automated: should be able to handle varying species, years, and temporal gaps in data, over different regions. 


#record of whether or not you have run code placed on C drive. 
dir.create("C:/R_map_status")
setwd("C:/R_map_status")
if(!file.exists("map_run_status.txt")){
  install.packages("raster")
  install.packages("rgdal")
  install.packages("ggplot2")
  install.packages("ggmap")
  install.packages("qmap")
  install.packages("rasterVis")
  install.packages("viridis")
  install.packages("ggthemes")
  install.packages("rgeos")
  install.packages("scales")
  install.packages("rasterImage")
  install.packages("png")
  install.packages("tiff")
  install.packages("animation")
  install.packages("grid")
  #install.packages("datasets")
  install.packages("maps")
  install.packages("dplyr")
  install.packages("smoothr")
  
  fileConn<-file("map_run_status.txt")
  writeLines(c("I have run the map script at least once, meaning that the necessary r packages are installed on my local machine."), fileConn)
  close(fileConn)
}

#load mapping package libraries
library(raster)
library(tiff)
library(sp)
library(rgdal)
library(ggplot2)
library(ggmap)
library(qmap)
library(rasterVis)
library(viridis)
library(ggthemes)
library(rgeos)
library(scales)
library(rasterImage)
library(animation)
library(png)
library(grid)
library(datasets)
library(maps)
library(dplyr)
library(smoothr)


#define useful functions

scale_x_longitude <- function(xmin=-180, xmax=180, step=1, namme="longitude", ...) {
  xbreaks <- seq(xmin,xmax,step)
  xlabels <- unlist(lapply(xbreaks, function(x) ifelse(x > 180, parse(text=paste0(360-x,"*W")), ifelse(x < 180, parse(text=paste0(x,"*E")),x))))
  return(scale_x_continuous(namme,limits = c(Boundaries[1], Boundaries[2]), breaks = xbreaks, labels = xlabels, expand = c(0, 0), ...))
}

scale_y_latitude <- function(ymin=-90, ymax=90, step=0.5, namme="latitude", ...) {
  ybreaks <- seq(ymin,ymax,step)
  ylabels <- unlist(lapply(ybreaks, function(x) ifelse(x < 0, parse(text=paste0(-1*x,"*S")), ifelse(x > 0, parse(text=paste0(x,"*N")),x))))
  return(scale_y_continuous(namme,limits = c(Boundaries[3], Boundaries[4]), breaks = ybreaks, labels = ylabels, expand = c(0, 0), ...))
}    

mround <- function(x,base){ 
  base*round(x/base) 
} 

darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

lighten <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col*factor
  col <- rgb(t(as.matrix(apply(col, 1, function(x) if (x > 255) 255 else x))), maxColorValue=255)
  col
}

rotate_df <- function(df){
  df <- within(df, {
    long <- ifelse(long < 0, long + 360, long)
  })
  return(df)
}

trim_shp_df <- function(df){
  df <- within(df, {
    long <- ifelse((long < Boundaries[1]),Boundaries[1],long)
    long <- ifelse((long > Boundaries[2]),Boundaries[2],long)
    lat <- ifelse((lat < Boundaries[3]),Boundaries[3],lat)
    lat <- ifelse((lat > Boundaries[4]),Boundaries[4],lat)
  })
  df <- subset(df, !is.na(df$lat) & !is.na(df$long))
  return(df)
}

trim_pdata_df <- function(df){
  df <- within(df, {
    long <- ifelse((long < Boundaries[1]),NA,long)
    long <- ifelse((long > Boundaries[2]),NA,long)
    lat <- ifelse((lat < Boundaries[3]),NA,lat)
    lat <- ifelse((lat > Boundaries[4]),NA,lat)
  })
  return(df)
}

Unproject_shp <- function(shp){
  shp <- spTransform(shp,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 +init=epsg:4326"))
  return(shp) 
}

Import_shp <- function(shp){
  shp2 <- readOGR(dsn=paste(input_folder,"/Shapefiles",sep=""), layer=shp)
  return(shp2)
}

Import_ice_shp <- function(shp,lyr){
  shp2 <- readOGR(dsn=shp, layer=lyr)
  return(shp2)
}

zero1_to_latpos <- function(x){
  latpos <- (x*latrange)+Boundaries[3]
  return(latpos)
}

zero1_to_longpos <- function(x){
  longpos <- (x*longrange)+Boundaries[1]
  return(longpos)
}


plot_base <- function(Land,Reg,MAP_df,raster_color_palette,image_width,image_height) #plots raster 
{
  ggplot()+ 
    geom_tile(data=MAP_df,aes(x=long,y=lat,fill=value))+
    scale_fill_gradientn(colours=raster_color_palette,guide=FALSE) +
    {if(ice!="y"|icefast=="y") geom_polygon(data=Reg,aes(x=long, y=lat, color=color,group=group),fill=NA,size=0.75)} +
    {if(ice!="y"|icefast=="y") scale_color_manual(name="not shown",values = as.character(unique(Reg$color)),guide=FALSE)}+
    {if(ice!="y"|icefast=="y") geom_polygon(aes(x = long, y = lat, group = group), data = Land,fill= "grey50",color = "gray25",size = 0.1)}+     #attempt flipping with other geom.polygon if having layering error
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    theme(axis.title=element_blank(),
          legend.box.spacing = unit(0, "mm"),
          axis.text=element_blank(),
          axis.ticks.length = unit(0, "mm"),
          plot.margin=unit(c(0,(image_width/50),-(image_height/50),-(image_width/50)),"mm"))                 #ultra small right/left margin to fix graphics bug where white lines are draw. negative value to remove bottom space
}

plot_blank <- function(MAP_df,longlow,longhigh,longbreak,latlow,lathigh,latbreak,Boundaries){ #just plots a blank space
  ggplot()+ geom_tile(data=MAP_df,aes(x=long,y=lat,fill=value))+
    scale_fill_gradientn(colours=c("white","white"),guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak,position="top") +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          legend.position = "none",
          plot.margin=unit(c(0,0,0,0),"mm"))
}

#plots actual data; here is where you would change species colors - under geom_point(fill="color")
plot_data <- function(data_plt,Boundaries,longlow,longhigh,longbreak,latlow,lathigh,latbreak,gridlines_transparency,legend_position_plt,legend_text_size_plt,legend_size_modifier_plt,axis_text_size_plt,axis_label_size_sslix,axis_label_size_ssliy,space_bt_labelsx,space_bt_labelsy,map_img_plt,counter,asterisk_size,maptype,low_leg_num_plt,high_leg_num_plt,week_gif_dot_threshold,dotcolor,mapplace,lllegend,nammme,Ice_file,ice_color,Land,Reg)
{
  ggplot()+
    annotation_raster(map_img_plt[[1]], xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
    {if(ice=="y"&map_img_plt[2]!="blank") geom_polygon(aes(x = long, y = lat, group = group), data = Ice_file,fill= ice_color,color = ice_color,size = 0.1)}+     #attempt flipping with other geom.polygon if having layering error
    {if(ice=="y"&icefast!="y"&map_img_plt[2]!="blank") geom_polygon(data=Reg,aes(x=long, y=lat, color=color,group=group),fill=NA,size=0.75)} +
    {if(ice=="y"&icefast!="y"&map_img_plt[2]!="blank") scale_color_manual(name="not shown",values = as.character(unique(Reg$color)),guide=FALSE)}+
    {if(ice=="y"&icefast!="y"&map_img_plt[2]!="blank") geom_polygon(aes(x = long, y = lat, group = group), data = Land,fill= "grey50",color = "gray25",size = 0.1)}+     #attempt flipping with other geom.polygon if having layering error
    {if(map_img_plt[2]!="blank") geom_point(data=data_plt, aes(x=long, y=lat, size = data_plt[,8],shape=data_plt[,16]),stroke=1.5,fill=dotcolor,color="black")}+
    scale_radius(name="",limits=c(0.0001,100),range=c(legend_size_modifier_plt,legend_size_modifier_plt*10),guide = guide_legend(override.aes = list(shape=21,colour = "black",fill=dotcolor)))+
    {if(maptype=="n"|maptype=="s"|maptype=="ss") scale_shape_manual(name="not shown ",values = c('no' = 21,'yes' = no_presence_symbol),guide=FALSE) else scale_shape_manual(name="not shown",values = c('no' = 21,'yes' = no_presence_symbol, 'point'= 16),guide=FALSE)}+
    {if((maptype=="n"|maptype=="s"|maptype=="ss")&map_img_plt[2]!="blank") geom_text(data=data_plt,aes(x=(long+((0.5/5*legend_size_modifier_plt))+(data_plt[,8]/80/5*legend_size_modifier_plt*asterisk_mod_long)), y=(lat+(0.04+data_plt[,8]/120/5*legend_size_modifier_plt*asterisk_mod_lat)),label=ifelse(data_plt[,10]<low_effort_threshold,"*",'')),size=asterisk_size) else geom_hline()}+    #mess with number values in this line if asterisk is not scaling correctly
    {if(map_img_plt[2]!="blank") geom_vline(xintercept=seq(longlow,Boundaries[2],longbreak),color=alpha('black',gridlines_transparency))}+ 
    {if(map_img_plt[2]!="blank") geom_hline(yintercept=seq(latlow,Boundaries[4],latbreak),color=alpha('black',gridlines_transparency))}+
    {if(maptype!="ss") scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak,position="top",namme=NULL) else scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak,position="top",namme=nammme[1])}+
    {if(maptype!="ss") scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak,namme=NULL) else scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak,namme=nammme[2])}+
    {if((mapplace=="topleft"))                         
      {if(lllegend=="notdone") 
        theme(axis.title.x=element_text(size=axis_label_size_sslix,face="bold",margin = ggplot2::margin(t = 0, r = space_bt_labelsy, b = space_bt_labelsx, l = 0)),
            axis.title.y=element_text(size=axis_label_size_ssliy,face="bold",margin = ggplot2::margin(t = 0, r = space_bt_labelsy, b = space_bt_labelsx, l = 0)),
            axis.text=element_text(size=axis_text_size_plt),
            plot.margin=unit(c(0,0,0,0),"mm"),
            legend.position = c(legend_position_plt[1],legend_position_plt[2]),
            legend.key=element_rect(colour=alpha('gray50',0.0),fill=alpha('gray50',0.0),size=0.5),
            legend.background = element_rect(colour = alpha('gray50',0.0), fill = alpha('gray50',0.0)),
            legend.text=element_text(size=legend_text_size_plt,color="white",face="bold"),
            legend.key.size = unit(0, 'lines'),
            legend.margin=ggplot2::margin(t=0, r=0, b=0, l=0, unit="cm"))
       else
         theme(axis.title.x=element_text(size=axis_label_size_sslix,face="bold",margin = ggplot2::margin(t = 0, r = space_bt_labelsy, b = space_bt_labelsx, l = 0)),
            axis.title.y=element_text(size=axis_label_size_ssliy,face="bold",margin = ggplot2::margin(t = 0, r = space_bt_labelsy, b = space_bt_labelsx, l = 0)),
            axis.text=element_text(size=axis_text_size_plt),
            plot.margin=unit(c(0,0,0,0),"mm"),
            legend.box.spacing = unit(0, "mm"),
            legend.position = "none")}
      else if(mapplace=="toprow")  
      {if(lllegend=="notdone")
        theme(axis.title.x=element_text(size=axis_label_size_sslix,face="bold",margin = ggplot2::margin(t = 0, r = space_bt_labelsy, b = space_bt_labelsx, l = 0)),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.x=element_text(size=axis_text_size_plt),
              plot.margin=unit(c(0,0,0,0),"mm"), 
              legend.position = c(legend_position_plt[1],legend_position_plt[2]),
              legend.key=element_rect(colour=alpha('gray50',0.0),fill=alpha('gray50',0.0),size=0.5),
              legend.background = element_rect(colour = alpha('gray50',0.0), fill = alpha('gray50',0.0)),
              legend.text=element_text(size=legend_text_size_plt,color="white",face="bold"),
              legend.key.size = unit(0, 'lines'),
              legend.margin=ggplot2::margin(t=0, r=0, b=0, l=0, unit="cm"))
      else
        theme(axis.title.x=element_text(size=axis_label_size_sslix,face="bold",margin = ggplot2::margin(t = 0, r = space_bt_labelsy, b = space_bt_labelsx, l = 0)),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.x=element_text(size=axis_text_size_plt),
              plot.margin=unit(c(0,0,0,0),"mm"), 
              legend.box.spacing = unit(0, "mm"),
              legend.position = "none")}
              
      else if(mapplace=="sideleft")
        theme(axis.title.x=element_blank(),
              axis.title.y=element_text(size=axis_label_size_ssliy,face="bold",margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.box.spacing = unit(0, "mm"),
              legend.position = "none",
              axis.text.y=element_text(size=axis_text_size_plt),
              plot.margin=unit(c(0,0,0,0),"mm")) 
      else if(mapplace=="internal")
        theme(axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              legend.box.spacing = unit(0, "mm"),
              legend.position = "none",
              plot.margin=unit(c(0,0,0,0),"mm"))}+ 
              {if(lllegend=="notdone" & (maptype=="n"|maptype=="s"||maptype=="ss")) annotate(geom="text",low_leg_num_plt[1],low_leg_num_plt[2],label="0",color="white",size=(legend_text_size_plt*0.375),fontface=2) else geom_hline()}+
              {if(counter==1 & maptype=="y") annotate(geom="text",low_leg_num_plt[1],low_leg_num_plt[2],label=paste("<",week_gif_dot_threshold,sep=""),color="white",size=(legend_text_size_plt*0.375),fontface=2) else geom_hline()}+
              {if(counter==1 & maptype=="y") annotate(geom="text",high_leg_num_plt[1],high_leg_num_plt[2],label="0",color="white",size=(legend_text_size_plt*0.375),fontface=2) else geom_hline()}+
              {if(maptype=="s") annotate(geom="text",month_label_position_plt[1],month_label_position_plt[2],label=paste(monthvec[data_plt[1,6]],sep=""),color="white",size=month_label_size_plt,fontface=2) else if(maptype=="n") annotate(geom="text",month_label_position_plt[1],month_label_position_plt[2],label=paste(monthvec[data_plt[1,6]],data_plt[1,5],sep=" "),color="white",size=month_label_size_plt,fontface=2) else if(maptype=="y") annotate(geom="text",month_label_position_plt[1],month_label_position_plt[2],label=paste(data_plt[1,7]),color="white",size=month_label_size_plt,fontface=2) else if(maptype=="ss") geom_hline()}

}


#OPTIONAL USER INPUTS. Defaults specified. 

#program flow
run_name <- "dan function test"      #name your  output folder. Unique names will generate a folder with this name in \\nmfs\akc-nmml\CAEP\Acoustics\ArcMAP\Mapping with R\Output files\. Duplicate names will overwrite that folder. trailing space in string will crash code.
#to run whole dataset, default next 3 lines to "y", "n", "n" 

ice<-"y" #NEW y/n
icefast<-"n" #plots quicker but sloppier. Disregard if ice=="n"

run_slides                <-"n"   #NEW
run_seasonslides          <-"n"   #NEW
run_month_gifs_and_videos <-"y"   #NEW
run_week_gifs_and_videos  <-"y"   #NEW


###################################################
#to change legend position
legend_position_sli <- c(.815,.72)                                    #scaled from 0-1, easiest to just enter test values here to reposition. 
legend_position_ind <- c(.835,.72)                                 #gifs and videos
#points on legend for the zero data and <2 data. Which number and symbol plots where depends on type of plot. 
low_leg_sym_sli <- c(.7844,.965) 
low_leg_num_sli <- c(.9085,.965)
low_leg_sym_ind <- c(.818,.45) 
low_leg_num_ind <- c(.9245,.45) 
high_leg_sym_ind <- c(.818,.5)
high_leg_num_ind <- c(.9375,.5)
#to change location of month on map 
month_label_position_sli <- c(.9,.052)                   #for slide plot month labels, 0-1
month_label_position_mon <- c(.83,.04)              #for gif and video month labels, 0-1
month_label_position_ind <- c(.85,.03)         #for month gifs and video week labels, 0-1
###################################################
num_lat_ticks <- 4
num_long_ticks <- 4
low_effort_threshold <- 15                                        #threshold below which an asterisk is placed on the dots. 
week_gif_dot_threshold <- 2
no_presence_symbol <- 3                                              #see http://www.statmethods.net/advgraphs/parameters.html for more options
no_presence_symbol_size <- 6
lessthantwo_symbol_size <- 2
legend_text_size_sli <- 40
legend_text_size_ind <- 30
legend_size_modifier_sli <- 5                                         #bigger # = bigger legend and dots. for the slides 
legend_size_modifier_ind <- 4                                         #bigger # = bigger legend and dots. for the gifs and videos
asterisk_size <-15
asterisk_mod_lat <- 0.5                                             #modifier for asterisk distance from dot. Scales by lat/long so may have to tweak between datasets. Higher values= longer distance
asterisk_mod_long <- 0.5                                             #modifier for asterisk distance from dot. Scales by lat/long so may have to tweak between datasets. Higher values= longer distance
gridlines_transparency <- 0.2                                        #0-1, 0 being fully transparent
ice_transparency<-0.5                                                #0-1, 0 being fully transparent
month_label_size_sli <- 24
month_label_size_ind <- 19
axis_text_size_sli <- 40                                            #text size for axes on multiplot 
axis_text_size_ind <- 24                                            #text size for axes on gif,video  
axis_label_size_sslix<- 60                       #NEW                #text size for labels on season slide  
axis_label_size_ssliy<- 60                       #NEW                #text size for labels on season slide  
image_height <- 11.0558                                              #height of output image (just the map region). Default value (11.0558) determined from arcGIS map window dimensions of Dana Bathymetry.
image_width <-  9.3902                                               #width of output image (just the map region). Default value (9.3902) determined from arcGIS map window dimensions of Dana Bathymetry.
image_correction_height <- 1.059796                              #play around with number to make sure top maps have same height as other 8.
image_correction_width <- 1.136496                               #play around with number to make sure left side maps have same width as other 9. 
image_correction_height_ss <- 1.12              #NEW             #size varies with axis_label_size_sslix
image_correction_width_ss <- 1.16               #NEW             #size varies with axis_label_size_ssliy
dist_between_plots <- 0.025
space_bt_labelsx<- 0                        #NEW                #distance between labels and axis text on season slide x axis 
space_bt_labelsy<- 10                       #NEW                #distance between labels and axis text on season slide y axis 
gifspeed_month <- 160   #default 90                              #larger numbers are slower, unsure of units. CHAOZ-X default = 160.
framerate_month <- 1                                                  #fps
gifspeed_week <- 60                                                  #larger numbers are slower, unsure of units. CHAOZ-X default = 160.
framerate_week <- 2                                                   #fps
gif_month_res <- 100
gif_week_res <- 60
slide_res <- 64
custom_extent <- "n"                                                  #"y" or "n". Put "y" if you want to manually specify the lat/long. Will default to extent to extent of current raster file if "n".

raster_color_palette <- c("white",lighten("skyblue1"),darken("skyblue1"),"steelblue4","steelblue4",darken("steelblue4")) #manually define to liking. Can be any length and combination
dotcolor <- "gold1"                                           #color of presence circles. 
ice_color <-rgb(1,1,1,ice_transparency)                                   #r g and b values. 4th argument specifies transparency. 
Region_colors <- c("deeppink", "green","gold1","lightpink") #steelblue4 if hiding RWCH for Bering                                 #NEW       #manually define region colors, in order that you type them. Must be same length as region_vector1
Seasonnamevec<- c("Spring","Summer","Winter")       #NEW       #Manually define month regions for season slide plot. Must be same length as Seasonnamevec 
Seasonvec <- c("Mar-Jun","Jul-Oct","Nov-Feb")    #NEW  #month must be 3 letters     #Format c("Mmm-Mmm","Mmm-Mmm"). Do not overlap months. Fine to cross over years (ie Dec-Mar). Must be same length as Seasonnamevec

###################import data files.###################################################

input_folder <- "E:/Other/Map_input" #file path for necessary data files, in quotations, with forward slash
output_folder <- "E:/Other/Map_output" #file path for map output, in quotations, with forward slash

#bathy raster. If loading from arcGIS: export data, select "Data frame Current" for option "extent". For "spatial reference", select "Raster Dataset (Original)". Specify name to match path below:
#should be raster file saved from arcGIS of JUST area of interest
#MAP <- raster(paste(input_folder,"/Rasters/","etopo1bedrk1.tif",sep=""))  # this was the AW/CHX map
MAP <- raster(paste(input_folder,"/Rasters/","Bering.tif",sep="")) #Bering is the name for Bering, JustBathy_NBerChukBeau name for other

#Import shape files of interest(right whale critical habitat, study boundaries, etc) 
# Region_vector1 <- c("CHX_area2r","AW_area2r")  # this was for the AW/CHX maps
#Region_vector1 <- c("DBOregions2017") #NEW (that it's working!)
Region_vector1 <- c("SLIP_Hotspot","Chirikov_Hotspot","SECS_Hotspot","NECS_Hotspot") #BSHC = Bering RWCH

#import land (polygon) shapefiles. (should not ever need to change unless plotting other part of world)
Land_vector1 <- c("AKBering","RusIslands")

#presence data - actual species data#
pdata <- read.csv(paste(input_folder,"/","Data frame","/","Humpback_Arctic_Bins.csv",sep="")) #monthly
colnames(pdata) <- c('species','mooringName','lat','long','year','month', 'date','percDaysWcalls', 'X.daysWcalls','X.daysWrecs')
if(run_week_gifs_and_videos=="y"){
pdatad <- read.csv(paste(input_folder,"/","Data frame","/","AWCHX_LTWeeklymapRfinal2.csv",sep="")) #weekly
colnames(pdatad) <- c('species','mooringName','lat','long','year','week', 'date','percBinsWcalls', 'X.binsWcalls','X.binsWrecs')
}

######### end of where you fuck with code to plot presence ###########################

########################Map data processing#######################

#change data frame dates to r date objects. 
pdata$dateformat <- as.Date(as.character(pdata$date),format="%m/%d/%Y")

Monthvec<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
Seasonvecnum<-NULL
for(n in 1:length(Seasonvec)){
start <- which(Monthvec==unlist(strsplit(Seasonvec[n],split="-"))[1])
stop <- which(Monthvec==unlist(strsplit(Seasonvec[n],split="-"))[2])
if(stop<start){
  list1<- c(start:12,1:stop)
}else{
list1 <- start:stop
}
Seasonvecnum[[length(Seasonvecnum)+1]] <- list1
}

pdata<- arrange(pdata,pdata$dateformat)

#season slide: create season column 
pdata$season<-"fill"
for(n in 1:length(Seasonnamevec)){
  pdata$season<- ifelse(pdata$month %in% Seasonvecnum[[n]],Seasonnamevec[n],pdata$season)
}

pdata$seasonNum<- "fill"
counter=1
for(g in 1:length(Seasonvecnum)){
  for(h in 1:nrow(pdata)){
    if(pdata$month[h] %in% Seasonvecnum[[g]] & pdata$month[h+1] %in% Seasonvecnum[[g]]){
      pdata$seasonNum[h]<- counter
    } else if(pdata$month[h] %in% Seasonvecnum[[g]]) {
      pdata$seasonNum[h]<- counter
      counter<-counter+1
    }else{
      pdata$seasonNum[h]<-pdata$seasonNum[h]
    }
  }
  counter=1
}
pdata$seasonFull<- paste(pdata$season,pdata$seasonNum,sep="_")

pdata$seasonYrs<-"fill"
pdata2<-NULL
for(i in 1:length(unique(pdata$seasonFull))){
  pdatatemp<-pdata[pdata$seasonFull==unique(pdata$seasonFull)[i],]
  pdatatemp$seasonYrs<- paste(unique(pdatatemp$year),collapse="-")
  pdata2<-rbind(pdata2,pdatatemp)
}

pdata<-pdata2
pdata2<-NULL

#Get rid of positive values so bathymetry will scale better
MAP[MAP>=-3]=-3  #set negative values to a point so that intermediate values will scale more naturally
MAP[MAP<=-1300] <- -1300
MAP <- (MAP*-1)+1
#transform data to create a beautiful depth illusion
MAP <- log(MAP,2)
#shift and trim  raster 
MAP <- shift(raster::rotate(shift(MAP, 180)), 180)

#Unproject shapefiles 
Land_vector2 <- lapply(Land_vector1,Import_shp)
Region_vector2 <- lapply(Region_vector1,Import_shp)

longlats1 <- lapply(Land_vector2,Unproject_shp)
longlats1reg <- lapply(Region_vector2,Unproject_shp)

#convert raster into useable ggplot2 format
MAP_spdf <- as(MAP,"SpatialPixelsDataFrame")
MAP_df <- as.data.frame(MAP_spdf)
colnames(MAP_df) <- c("value","long","lat")

if(custom_extent=="y"){
  latlow <- NULL #default NULL. either leave all 4 parameters null or specify each, for long in degrees from 0 to 360 (ex: latlow <- 66, longlow <- 185)
  longlow <-NULL #default NULL
  lathigh<- NULL #default NULL
  longhigh <-NULL #default NULL
  Boundaries <- c(longlow,longhigh,latlow,lathigh)
  latrange <- lathigh-latlow
  longrange <- longhigh-longlow
  latbreak <- round(round(latrange)/num_lat_ticks)
  longbreak <- round(round(longrange)/num_long_ticks)
  
}else{
  Boundaries <- as.vector(MAP@extent)
  latrange <- Boundaries[4]-Boundaries[3]
  longrange <- Boundaries[2]-Boundaries[1]
  latbreak <- round(round(latrange)/num_lat_ticks)
  longbreak <- round(round(longrange)/num_long_ticks)
  latlow <- mround(Boundaries[3],latbreak)
  longlow <-mround(Boundaries[1],longbreak)
  lathigh<- mround(Boundaries[4],latbreak)
  longhigh <-mround(Boundaries[2],longbreak)
}

#calculate placement of some variables in lat/long from 0-1
low_leg_sym_sli <- c(zero1_to_longpos(low_leg_sym_sli[1]),zero1_to_latpos(low_leg_sym_sli[2]))
low_leg_num_sli <- c(zero1_to_longpos(low_leg_num_sli[1]),zero1_to_latpos(low_leg_num_sli[2]))
low_leg_sym_ind <- c(zero1_to_longpos(low_leg_sym_ind[1]),zero1_to_latpos(low_leg_sym_ind[2]))
low_leg_num_ind <- c(zero1_to_longpos(low_leg_num_ind[1]),zero1_to_latpos(low_leg_num_ind[2]))
high_leg_sym_ind <- c(zero1_to_longpos(high_leg_sym_ind[1]),zero1_to_latpos(high_leg_sym_ind[2]))
high_leg_num_ind <- c(zero1_to_longpos(high_leg_num_ind[1]),zero1_to_latpos(high_leg_num_ind[2]))
  
  
month_label_position_sli <- c(zero1_to_longpos(month_label_position_sli[1]),zero1_to_latpos(month_label_position_sli[2]))
month_label_position_mon <- c(zero1_to_longpos(month_label_position_mon[1]),zero1_to_latpos(month_label_position_mon[2]))
month_label_position_ind <- c(zero1_to_longpos(month_label_position_ind[1]),zero1_to_latpos(month_label_position_ind[2]))


longlats2 <- lapply(longlats1,fortify)
longlats2reg <- lapply(longlats1reg,fortify)
longlats3 <- lapply(longlats2,rotate_df)
longlats3reg <- lapply(longlats2reg,rotate_df)
longlats3 <- lapply(longlats3,trim_shp_df)
longlats3reg <- lapply(longlats3reg,trim_shp_df)

for(i in 1:length(Land_vector1)){
  group_change <- as.numeric(longlats3[[i]]$group)
  group_change <- group_change*(.9273+(i*0.0001))   #Ideally makes group ID unique from one shapefile to another. If bad geometry appears, can tweak this formula. Arbitrary values to create uniqueness between shapefiles. 
  longlats3[[i]]$group <- as.factor(group_change)
}

for(i in 1:length(Region_vector1)){
  group_change <- as.numeric(longlats3reg[[i]]$group)
  group_change <- group_change*(.9273+(i*0.0001))   #Ideally makes group ID unique from one shapefile to another. If bad geometry appears, can tweak this formula. Arbitrary values to create uniqueness between shapefiles. 
  longlats3reg[[i]]$group <- as.factor(group_change)
}

#additionally adds color column 
for(i in 1:length(Region_vector1)){
  longlats3reg[[i]]<- cbind(longlats3reg[[i]],Region_colors[i])
  colnames(longlats3reg[[i]])[8]<-"color" 
  longlats3reg[[i]]$color <- as.factor(longlats3reg[[i]]$color)
  group_change <- as.numeric(longlats3reg[[i]]$group)
  group_change <- group_change*(0+i)   #Ideally makes group ID unique from one shapefile to another. If bad geometry appears, can tweak this formula. Arbitrary values to create uniqueness between shapefiles. 
  longlats3reg[[i]]$group <- as.factor(group_change)
}



Land <- do.call(rbind,longlats3)
Reg <- do.call(rbind,longlats3reg)
Reg$color <- factor(Reg$color,levels=unique(Region_colors))

#################Presence data processing######################

monthvec <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
pdata$percDWCis0 <- ifelse(pdata$percDaysWcalls==0,"yes","no")
pdata <- rotate_df(pdata)
pdata <- trim_pdata_df(pdata)

#remove species with no calling. Change to remove based on calling max() == 0
for(r in unique(pdata$species)){
  if(max(pdata[pdata$species==r,]$percDaysWcalls)==0){
    pdata <- pdata[pdata$species!=r,]
  }else{
    pdata<- pdata
  }
}

pdata$percDaysWcalls[pdata$percDaysWcalls==0] <- no_presence_symbol_size



if(run_week_gifs_and_videos=="y"){
  pdatad$dateformat <- as.Date(as.character(pdatad$date),format="%m/%d/%y")
  pdatad$season<-""
  pdatad$seasonNum<-""
  pdatad$seasonFull<-""
  pdatad$seasonYrs<-""
  pdatad$percDWCis0 <- ifelse(pdatad$percBinsWcalls==0,"yes","no")
  pdatad <- rotate_df(pdatad)
  pdatad <- trim_pdata_df(pdatad)

  for(q in unique(pdatad$species)){
    if(max(pdatad[pdatad$species==q,]$percBinsWcalls)==0){
      pdatad <- pdatad[pdatad$species!=q,]
    }else{
      pdatad <- pdatad
    }
  }
  
  pdatad$percBinsWcalls[pdatad$percBinsWcalls==0] <- no_presence_symbol_size
  pdatad[pdatad$percBinsWcalls<week_gif_dot_threshold,16] <- "point"   
  pdatad[pdatad$percBinsWcalls<week_gif_dot_threshold,8] <-lessthantwo_symbol_size   
  

}


topfolder <- paste(output_folder,"/",run_name,sep="")
dir.create(topfolder)


  
#############slides######################
#plot base and blank map 

base_folder <- paste(topfolder,"/","Base_maps",sep="")
dir.create(base_folder)

tiff(paste(base_folder,"/base_map.tif",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
print(plot_base(Land,Reg,MAP_df,raster_color_palette,image_width,image_height))
dev.off()

Base_map_img <- readTIFF(paste(base_folder,"/base_map.tif",sep=""))

tiff(paste(base_folder,"/blank_map.tif",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
print(plot_blank(MAP_df,longlow,longhigh,longbreak,latlow,lathigh,latbreak,Boundaries))
dev.off()

Blank_map_img <- readTIFF(paste(base_folder,"/blank_map.tif",sep=""))

low_leg_sym_plt <- low_leg_sym_sli
low_leg_num_plt <- low_leg_num_sli
high_leg_sym_plt <- NULL
high_leg_num_plt <- NULL

axis_text_size_plt <- axis_text_size_sli
month_label_size_plt <- month_label_size_sli
month_label_position_plt <- month_label_position_sli
legend_size_modifier_plt <- legend_size_modifier_sli
legend_text_size_plt <- legend_text_size_sli
legend_position_plt <- legend_position_sli
map_img_plt <- list(Base_map_img,"base")

maptype <- "s"

#######################SLIDES################################

if(run_slides=="y"|run_seasonslides=="y"){
slide_folder <- paste(topfolder,"/","Slides",sep="")
dir.create(slide_folder)

for(a in unique(pdata$species)){
  maptype <- "s"
  pdata_sp <- pdata[pdata$species==a,]
  sp_folder <- paste(slide_folder,"/",a,sep="")
  dir.create(sp_folder)
  #Don't run basic slides
  if(run_slides=="y"){
  for(b in sort(unique(pdata_sp$year))){
    pdata_sp_y<- pdata_sp[pdata_sp$year==b,]
    months_in_y <- unique(pdata_sp_y$month)
    months_in_y <- sort(months_in_y)
    month_span <- seq(months_in_y[1],tail(months_in_y,1))
    white_plots_needed <- 12-length(month_span)
    pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$month==months_in_y[1],]
    dummy_row <- data.frame(as.character(pdata_sp_y_m[1,1])," ",low_leg_sym_plt[2],low_leg_sym_plt[1],pdata_sp_y_m[1,5],pdata_sp_y_m[1,6],pdata_sp_y_m[1,7],no_presence_symbol_size,0,low_effort_threshold+1,pdata_sp_y_m$dateformat[1],pdata_sp_y_m$season[1],pdata_sp_y_m$seasonNum[1],pdata_sp_y_m$seasonFull[1],pdata_sp_y_m$seasonYrs[1],"yes")
    colnames(dummy_row)<- colnames(pdata_sp_y_m)
    counter<- 0
    nammme<-c("1","2")
    map_img_plt <- list(Base_map_img,"base")
    for(c in month_span){
      counter <- counter+1
      lllegend<-"done"
      #HERE IS WHERE I WOULD LOAD ICE DATA ? variable c denotes month b denotes year
      if(ice=="y"){
      Ice_file<-Import_ice_shp(paste(input_folder,"/Shapefiles_ice_extent/",monthvec[c],"/extent_N_",b,sprintf("%02d", c),"_polygon_v3.0",sep=""),paste("extent_N_",b,sprintf("%02d", c),"_polygon_v3.0",sep=""))
      Ice_file<-smooth(Ice_file,method = "chaikin")
      Ice_file<-Unproject_shp(Ice_file)
      Ice_file<-fortify(Ice_file)
      Ice_file<-rotate_df(Ice_file)
      Ice_file<-trim_shp_df(Ice_file)
      }
      ###############
      if(nrow(pdata_sp_y[pdata_sp_y$month==c,])!=0){
        data_plt <- pdata_sp_y[pdata_sp_y$month==c,]
      }else{
        nodatrow <- data.frame(as.character(pdata_sp_y_m[1,1])," ",0,0,pdata_sp_y_m[1,5],c,pdata_sp_y_m[1,7],no_presence_symbol_size,0,low_effort_threshold+1,pdata_sp_y_m$dateformat[1],pdata_sp_y_m$season[1],pdata_sp_y_m$seasonNum[1],pdata_sp_y_m$seasonFull[1],pdata_sp_y_m$seasonYrs[1],"yes")
        colnames(nodatrow)<- colnames(pdata_sp_y_m)
        data_plt <- nodatrow
      }
          if(counter==5|counter==9){
            image_height_plt <- image_height
            image_width_plt <- (image_width*image_correction_width)
            mapplace<-"sideleft"
          }else if(counter==2|counter==3|counter==4){
            image_height_plt <- (image_height*image_correction_height)
            image_width_plt <- image_width
            mapplace<-"toprow"
          }else if(counter==1){
            lllegend<-"notdone"
            image_height_plt <- (image_height*image_correction_height)
            image_width_plt <- (image_width*image_correction_width)
            data_plt <- rbind(data_plt,dummy_row)
            mapplace<-"topleft"
          }else{
            image_height_plt <- image_height
            image_width_plt <- image_width
            mapplace<-"internal"
          }
          tiff(paste(sp_folder,"/",b,"_",counter,"_temp.tif",sep=""),width=(image_width_plt),height=(image_height_plt),units="in",res=slide_res) 
          print(plot_data(data_plt,Boundaries,longlow,longhigh,longbreak,latlow,lathigh,latbreak,gridlines_transparency,legend_position_plt,legend_text_size_plt,legend_size_modifier_plt,axis_text_size_plt,axis_label_size_sslix,axis_label_size_ssliy,space_bt_labelsx,space_bt_labelsy,map_img_plt,counter,asterisk_size,maptype,low_leg_num_plt,high_leg_num_plt,week_gif_dot_threshold,dotcolor,mapplace,lllegend,nammme,Ice_file,ice_color,Land,Reg))
          print(paste("Writing slides:",sp_folder,"",b))
          dev.off()
    }
    for(d in 1:white_plots_needed){
    counter <- counter+1
    tiff(paste(sp_folder,"/",b,"_",counter,"_temp.tif",sep=""),width=(image_width_plt),height=(image_height_plt),units="in",res=slide_res)
    print(Blank_map_img)
    dev.off()
    }
    counter<-0
    tiff(paste(sp_folder,"/",a,"_",b,".tif",sep=""),width=((image_width*image_correction_width)+image_width*3),height=((image_height*image_correction_height)+(image_height*2)),units="in",res=(slide_res))
    par(mai=c(dist_between_plots,dist_between_plots,dist_between_plots,dist_between_plots), oma=c(0,0,0,0),xpd=NA)
    layout(matrix(1:12, ncol=4, byrow=TRUE),widths=c((image_width*image_correction_width),image_width,image_width,image_width),heights=c((image_height*image_correction_height),image_height,image_height))
    for(e in 1:12){
      plot(NA,xlim=0:1,ylim=0:1,axes=0,bty="n",xaxs="i",yaxs="i",ylab="")
      rasterImage(readTIFF(paste(sp_folder,"/",b,"_",e,"_temp.tif",sep="")),0,0,1,1)
    }
    dev.off()
    setwd(paste(sp_folder,sep=""))
    file.remove(list.files(pattern="temp.tif"))
  }
  }
  #season slide
  if(run_seasonslides=="y"){
  maptype <- "ss" #slide, seasons
  
  for(l in 1:length(Seasonnamevec)){
    pdata_sp_sea<- pdata_sp[pdata_sp$season==Seasonnamevec[l],]
    seasonvec2<-sort(unique(as.numeric(pdata_sp_sea$seasonNum)))
    months_in_sea<- sort(unique(pdata_sp_sea$month))
    yearvec2<- unique(pdata_sp_sea$seasonYrs)
    #create vector of widths for layout function
    layoutwidthvec<-image_width*image_correction_width_ss
    for(r in 1:(length(seasonvec2)-1)){
      layoutwidthvec<-c(layoutwidthvec,image_width)
    }
    #create vector of lengths for layout function
    layoutheightvec<-image_height*image_correction_height_ss
    for(r in 1:(length(months_in_sea)-1)){
      layoutheightvec<-c(layoutheightvec,image_height)
    }
    counter=0
    lllegend<-"notdone"
    for(q in 1:length(seasonvec2)){
      pdata_sp_sea_num<- pdata_sp_sea[pdata_sp_sea$seasonNum==seasonvec2[q],]
      months_in_sea_num<- sort(unique(pdata_sp_sea_num$month))
      plotmonthvec <- NULL
      for(p in 1:length(months_in_sea)){
        plotmonthvec[[length(plotmonthvec)+1]] <- ifelse(Seasonvecnum[[l]][p] %in% months_in_sea_num,"yes","no")
      }
      for(m in 1:length(months_in_sea)){
        counter=counter+1
        if(q==1){
          image_width_plt<-(image_width*image_correction_width_ss)
        }else{
          image_width_plt<-image_width
        }
        if(m==1){
          image_height_plt<-(image_height*image_correction_height_ss)
        }else{
          image_height_plt<-image_height
        }
        if(m==1&q==1){
          mapplace<-"topleft"
        }else if(m!=1&q==1){
          mapplace<-"sideleft"
        }else if(m==1&q!=1){
          mapplace<-"toprow"
        }else{
          mapplace<-"internal"
        }
        if(plotmonthvec[m]=="yes"){
          pdata_sp_sea_num_mon <- pdata_sp_sea_num[pdata_sp_sea_num$month==Seasonvecnum[[l]][m],]
          data_plt <- pdata_sp_sea_num_mon
          nammme<- c(paste(data_plt[1,15]),paste(monthvec[data_plt[1,6]],sep=""))
          #LOAD ICE FILE HERE. nammme has m and y info 
          if(ice=="y"){
          Ice_file<-Import_ice_shp(paste(input_folder,"/Shapefiles_ice_extent/",nammme[2],"/extent_N_",data_plt[1,5],sprintf("%02d", data_plt[1,6]),"_polygon_v3.0",sep=""),paste("extent_N_",data_plt[1,5],sprintf("%02d", data_plt[1,6]),"_polygon_v3.0",sep=""))
          Ice_file<-smooth(Ice_file,method = "chaikin")
          Ice_file<-Unproject_shp(Ice_file)
          Ice_file<-fortify(Ice_file)
          Ice_file<-rotate_df(Ice_file)
          Ice_file<-trim_shp_df(Ice_file)
          }
          ##############
          map_img_plt <- list(Base_map_img,"base")
          if(m==1){
            if(lllegend=="notdone"){
              dummy_row <- data.frame(as.character(pdata_sp_sea_num_mon[1,1])," ",low_leg_sym_plt[2],low_leg_sym_plt[1],pdata_sp_sea_num_mon[1,5],pdata_sp_sea_num_mon[1,6],pdata_sp_sea_num_mon[1,7],no_presence_symbol_size,0,low_effort_threshold+1,pdata_sp_sea_num_mon$dateformat[1],pdata_sp_sea_num_mon$season[1],pdata_sp_sea_num_mon$seasonNum[1],pdata_sp_sea_num_mon$seasonFull[1],pdata_sp_sea_num_mon$seasonYrs[1],"yes")
              colnames(dummy_row)<- colnames(pdata_sp_sea_num_mon)
              data_plt <- rbind(pdata_sp_sea_num_mon,dummy_row)
              tiff(paste(sp_folder,"/",a," ",Seasonnamevec[l]," all years_",counter,"_temp.tif",sep=""),width=(image_width_plt),height=(image_height_plt),units="in",res=slide_res) 
              print(plot_data(data_plt,Boundaries,longlow,longhigh,longbreak,latlow,lathigh,latbreak,gridlines_transparency,legend_position_plt,legend_text_size_plt,legend_size_modifier_plt,axis_text_size_plt,axis_label_size_sslix,axis_label_size_ssliy,space_bt_labelsx,space_bt_labelsy,map_img_plt,counter,asterisk_size,maptype,low_leg_num_plt,high_leg_num_plt,week_gif_dot_threshold,dotcolor,mapplace,lllegend,nammme,Ice_file,ice_color,Land,Reg))
              print(paste("Writing slides:",sp_folder,"",a,Seasonnamevec[l],"all years"))
              dev.off()
              lllegend<-"done"
            }else{
              lllegend2<- lllegend
              lllegend<- "done"
              tiff(paste(sp_folder,"/",a," ",Seasonnamevec[l]," all years_",counter,"_temp.tif",sep=""),width=(image_width_plt),height=(image_height_plt),units="in",res=slide_res) 
              print(plot_data(data_plt,Boundaries,longlow,longhigh,longbreak,latlow,lathigh,latbreak,gridlines_transparency,legend_position_plt,legend_text_size_plt,legend_size_modifier_plt,axis_text_size_plt,axis_label_size_sslix,axis_label_size_ssliy,space_bt_labelsx,space_bt_labelsy,map_img_plt,counter,asterisk_size,maptype,low_leg_num_plt,high_leg_num_plt,week_gif_dot_threshold,dotcolor,mapplace,lllegend,nammme,Ice_file,ice_color,Land,Reg))
              print(paste("Writing slides:",sp_folder,"",a,Seasonnamevec[l],"all years"))
              dev.off()
              lllegend <- lllegend2  
            }
          }else{
          lllegend2<- lllegend
          lllegend<- "done"
          tiff(paste(sp_folder,"/",a," ",Seasonnamevec[l]," all years_",counter,"_temp.tif",sep=""),width=(image_width_plt),height=(image_height_plt),units="in",res=slide_res) 
          print(plot_data(data_plt,Boundaries,longlow,longhigh,longbreak,latlow,lathigh,latbreak,gridlines_transparency,legend_position_plt,legend_text_size_plt,legend_size_modifier_plt,axis_text_size_plt,axis_label_size_sslix,axis_label_size_ssliy,space_bt_labelsx,space_bt_labelsy,map_img_plt,counter,asterisk_size,maptype,low_leg_num_plt,high_leg_num_plt,week_gif_dot_threshold,dotcolor,mapplace,lllegend,nammme,Ice_file,ice_color,Land,Reg))
          print(paste("Writing slides:",sp_folder,a,Seasonnamevec[l],"all years"))
          dev.off()
          lllegend <- lllegend2
          }
        }else{
          data_plt <- data.frame(as.character(pdata_sp_sea_num[1,1])," ",low_leg_sym_plt[2],low_leg_sym_plt[1],pdata_sp_sea_num[1,5],pdata_sp_sea_num[1,6],pdata_sp_sea_num[1,7],no_presence_symbol_size,0,low_effort_threshold+1,pdata_sp_sea_num$season[1],"yes",pdata_sp_sea_num$dateformat[1])
          nammme<- c(paste(rep(yearvec2,each=length(months_in_sea))[counter]),paste(monthvec[rep(months_in_sea,length(seasonvec2))[counter]],sep=""))
          map_img_plt <- list(Blank_map_img,"blank")
          tiff(paste(sp_folder,"/",a," ",Seasonnamevec[l]," all years_",counter,"_temp.tif",sep=""),width=(image_width_plt),height=(image_height_plt),units="in",res=slide_res) 
          print(plot_data(data_plt,Boundaries,longlow,longhigh,longbreak,latlow,lathigh,latbreak,gridlines_transparency,legend_position_plt,legend_text_size_plt,legend_size_modifier_plt,axis_text_size_plt,axis_label_size_sslix,axis_label_size_ssliy,space_bt_labelsx,space_bt_labelsy,map_img_plt,counter,asterisk_size,maptype,low_leg_num_plt,high_leg_num_plt,week_gif_dot_threshold,dotcolor,mapplace,lllegend,nammme,Ice_file,ice_color,Land,Reg))
          print(paste("Writing slides:",sp_folder,a,Seasonnamevec[l],"all years"))
          dev.off()
        }
      }
    }
    tiff(paste(sp_folder,"/",a," ",Seasonnamevec[l]," all years.tif",sep=""),width=((image_width*image_correction_width_ss)+image_width*(length(seasonvec2)-1)),height=((image_height*image_correction_height_ss)+(image_height*(length(months_in_sea)-1))),units="in",res=(slide_res))
    par(mai=c(dist_between_plots,dist_between_plots,dist_between_plots,dist_between_plots), oma=c(0,0,0,0),xpd=NA)
    layout(matrix(1:counter, ncol=length(seasonvec2), byrow=FALSE),widths=layoutwidthvec,heights=layoutheightvec)
    for(e in 1:counter){
      plot(NA,xlim=0:1,ylim=0:1,axes=0,bty="n",xaxs="i",yaxs="i",ylab="")
      rasterImage(readTIFF(paste(sp_folder,"/",a," ",Seasonnamevec[l]," all years_",e,"_temp.tif",sep="")),0,0,1,1)
    }
    counter=0
    dev.off()
    setwd(paste(sp_folder,sep=""))
    file.remove(list.files(pattern="temp.tif"))
  }
  }
}
}




#######################GIFS and Video################################

if(run_month_gifs_and_videos=="y"){
  gif_folder <- paste(topfolder,"/","Gifs and Videos",sep="")
  dir.create(gif_folder)

low_leg_sym_plt <- low_leg_sym_ind
low_leg_num_plt <- low_leg_num_ind

axis_text_size_plt <- axis_text_size_ind
month_label_size_plt <- month_label_size_ind       
month_label_position_plt <- month_label_position_mon #put month in same spot as slides, otherwise share parameters with week gifs/videos
legend_size_modifier_plt <- legend_size_modifier_ind
legend_text_size_plt <- legend_text_size_ind
legend_position_plt <- legend_position_ind
map_img_plt  <- list(Base_map_img,"base")

maptype <- "n"
counter<-1
mapplace<-"topleft"
lllegend<-"notdone"
nammme<-c("1","2")

  for(f in unique(pdata$species)){
    pdata_sp <- pdata[pdata$species==f,] 
    sp_folder <- paste(gif_folder,"/",f,sep="")
    dir.create(sp_folder)
    counterG <- "0001"
    for(g in sort(unique(pdata_sp$year))){
      pdata_sp_y<- pdata_sp[pdata_sp$year==g,]
      pdata_sp_y <- pdata_sp_y[order(pdata_sp_y$dateformat),]
      for(h in unique(pdata_sp_y$month)){
        #LOAD ICE FILE HERE var g and h contain y/m info
        if(ice=="y"){
        Ice_file<-Import_ice_shp(paste(input_folder,"/Shapefiles_ice_extent/",monthvec[h],"/extent_N_",g,sprintf("%02d", h),"_polygon_v3.0",sep=""),paste("extent_N_",g,sprintf("%02d", h),"_polygon_v3.0",sep=""))
        Ice_file<-smooth(Ice_file,method = "chaikin")
        Ice_file<-Unproject_shp(Ice_file)
        Ice_file<-fortify(Ice_file)
        Ice_file<-rotate_df(Ice_file)
        Ice_file<-trim_shp_df(Ice_file)
        }
        ##############
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$month==h,] 
        dummy_row <- data.frame(as.character(pdata_sp_y_m[1,1])," ",low_leg_sym_plt[2],low_leg_sym_plt[1],pdata_sp_y_m[1,5],pdata_sp_y_m[1,6],pdata_sp_y_m[1,7],no_presence_symbol_size,0,low_effort_threshold+1,pdata_sp_y_m$dateformat[1],pdata_sp_y_m$season[1],pdata_sp_y_m$seasonNum[1],pdata_sp_y_m$seasonFull[1],pdata_sp_y_m$seasonYrs[1],"yes")
        colnames(dummy_row)<- colnames(pdata_sp_y_m)
        data_plt <- rbind(pdata_sp_y_m,dummy_row)
        finalfilename <- paste("img",counterG,sep="")
        tiff(paste(sp_folder,"/",finalfilename,".tif",sep=""),width=(image_width*image_correction_width),height=(image_height*image_correction_height),units="in",res=gif_month_res) 
        print(plot_data(data_plt,Boundaries,longlow,longhigh,longbreak,latlow,lathigh,latbreak,gridlines_transparency,legend_position_plt,legend_text_size_plt,legend_size_modifier_plt,axis_text_size_plt,axis_label_size_sslix,axis_label_size_ssliy,space_bt_labelsx,space_bt_labelsy,map_img_plt,counter,asterisk_size,maptype,low_leg_num_plt,high_leg_num_plt,week_gif_dot_threshold,dotcolor,mapplace,lllegend,namme,Ice_file,ice_color,Land,Reg))
        print(paste("Writing Month GIFS and videos:",sp_folder,g,counterG))
        dev.off()
        counterG <- as.numeric(counterG)
        counterG <- counterG +1
        if(nchar(counterG)==1){
          counterG <- paste("000",counterG,sep="")
        }
        if(nchar(counterG)==2){
          counterG <- paste("00",counterG,sep="")
        }
        if(nchar(counterG)==3){
          counterG <- paste("0",counterG,sep="")
        }
        else{
          counterG <- as.character(counterG)
        }
      }
    }
    setwd(sp_folder)
    system(paste('"C:/Program Files/ImageMagick-7.0.8-Q16/magick.exe" -delay ',gifspeed_month,' *.tif ',f,'_month1.gif',sep=""))
    system(paste('"D:/DDownloads/ffmpeg-20190122-87c165c-win64-static/ffmpeg-20190122-87c165c-win64-static/bin/ffmpeg.exe" -r ',framerate_month,' -i "img%04d.tif" -codec:a libmp3lame ',f,'_month_video.avi',sep=""))
   # system(paste(shQuote("E:/mapATTACK/Accessory code/gifsicle.exe",type="cmd"), " gifsicle -O3 ",f,"_month1.gif -o ",f,"_month.gif",sep=""))
    #system(paste("gifsicle -O3 --colors 256 ",f,"_month1.gif -o ",f,"_month.gif"," -w",sep=""))

    file.remove(list.files(pattern=paste(".tif",sep="")))
    #file.remove(list.files(pattern=paste(f,"_month1.gif",sep="")))
  }
}

if(run_week_gifs_and_videos=="y"){
  gif_folder <- paste(topfolder,"/","Gifs and Videos",sep="")
  dir.create(gif_folder)

low_leg_sym_plt <- low_leg_sym_ind
low_leg_num_plt <- low_leg_num_ind
high_leg_sym_plt <- high_leg_sym_ind
high_leg_num_plt <- high_leg_num_ind
  
axis_text_size_plt <- axis_text_size_ind
month_label_size_plt <- month_label_size_ind
month_label_position_plt <- month_label_position_ind
legend_size_modifier_plt <- legend_size_modifier_ind
legend_text_size_plt <- legend_text_size_ind
legend_position_plt <- legend_position_ind

maptype <- "y"
counter<-1
mapplace<-"topleft"
lllegend<-"notdone"
nammme<-c("1","2")

  for(i in unique(pdatad$species)){
    pdatad_sp <- pdatad[pdatad$species==i,] 
    sp_folder <- paste(gif_folder,"/",i,sep="")
    dir.create(sp_folder)
    counterG <- "00001"
    for(j in sort(unique(pdatad_sp$year))){
      pdatad_sp_y<- pdatad_sp[pdatad_sp$year==j,]
      pdatad_sp_y <- pdatad_sp_y[order(pdatad_sp_y$dateformat),]
      for(k in unique(pdatad_sp_y$date)){
        pdatad_sp_y_d <- pdatad_sp_y[pdatad_sp_y$date==k,] 
        dummy_row1 <- data.frame(as.character(pdatad_sp_y_d[1,1])," ",high_leg_sym_plt[2],high_leg_sym_plt[1],pdatad_sp_y_d[1,5],pdatad_sp_y_d[1,6],pdatad_sp_y_d[1,7],no_presence_symbol_size,0,low_effort_threshold+1,pdatad_sp_y_d$dateformat[1],pdatad_sp_y_d$season[1],pdatad_sp_y_d$seasonNum[1],pdatad_sp_y_d$seasonFull[1],pdatad_sp_y_d$seasonYrs[1],"yes")
        dummy_row2 <- data.frame(as.character(pdatad_sp_y_d[1,1])," ",low_leg_sym_plt[2],low_leg_sym_plt[1],pdatad_sp_y_d[1,5],pdatad_sp_y_d[1,6],pdatad_sp_y_d[1,7],lessthantwo_symbol_size,0,low_effort_threshold+1,pdatad_sp_y_d$dateformat[1],pdatad_sp_y_d$season[1],pdatad_sp_y_d$seasonNum[1],pdatad_sp_y_d$seasonFull[1],pdatad_sp_y_d$seasonYrs[1],"point")
        colnames(dummy_row1)<- colnames(pdatad_sp_y_d)
        colnames(dummy_row2)<- colnames(pdatad_sp_y_d)
        data_plt <- rbind(pdatad_sp_y_d,dummy_row1,dummy_row2)
        finalfilename <- paste("img",counterG,sep="")
        tiff(paste(sp_folder,"/",finalfilename,".tif",sep=""),width=(image_width*image_correction_width),height=(image_height*image_correction_height),units="in",res=gif_week_res) 
        ###################
        if(ice=="y"){
        month<-substr(pdatad_sp_y_d[1,7],1,2)
        monthnum<-as.numeric(month)
        Ice_file<-Import_ice_shp(paste(input_folder,"/Shapefiles_ice_extent/",monthvec[monthnum],"/extent_N_",j,month,"_polygon_v3.0",sep=""),paste("extent_N_",j,month,"_polygon_v3.0",sep=""))
        Ice_file<-smooth(Ice_file,method = "chaikin")
        Ice_file<-Unproject_shp(Ice_file)
        Ice_file<-fortify(Ice_file)
        Ice_file<-rotate_df(Ice_file)
        Ice_file<-trim_shp_df(Ice_file)
        }
        ##############
        print(plot_data(data_plt,Boundaries,longlow,longhigh,longbreak,latlow,lathigh,latbreak,gridlines_transparency,legend_position_plt,legend_text_size_plt,legend_size_modifier_plt,axis_text_size_plt,axis_label_size_sslix,axis_label_size_ssliy,space_bt_labelsx,space_bt_labelsy,map_img_plt,counter,asterisk_size,maptype,low_leg_num_plt,high_leg_num_plt,week_gif_dot_threshold,dotcolor,mapplace,lllegend,nammme,Ice_file,ice_color,Land,Reg))
        print(paste("Writing Week GIFS and videos:",sp_folder,j,counterG))
        dev.off()
        counterG <- as.numeric(counterG)
        counterG <- counterG +1
        if(nchar(counterG)==1){
          counterG <- paste("0000",counterG,sep="")
        }
        if(nchar(counterG)==2){
          counterG <- paste("000",counterG,sep="")
        }
        if(nchar(counterG)==3){
          counterG <- paste("00",counterG,sep="")
        }
        if(nchar(counterG)==4){
          counterG <- paste("0",counterG,sep="")
        }
        else{
          counterG <- as.character(counterG)
        }
      }
    }
    setwd(sp_folder)
    system(paste('"C:/Program Files/ImageMagick-7.0.8-Q16/magick.exe" -delay ',gifspeed_week,' *.tif ',i,"_week1.gif",sep=""))
    system(paste('"D:/DDownloads/ffmpeg-20190122-87c165c-win64-static/ffmpeg-20190122-87c165c-win64-static/bin/ffmpeg.exe" -r ',framerate_week,' -i "img%05d.tif" -codec:a libmp3lame ',i,'_week_video.avi',sep=""))
    #system(paste(shQuote("E:/mapATTACK/Accessory Code/gifsicle.exe",type="cmd"), " gifsicle -O3 ",i,"_week1.gif -o ",i,"_week.gif",sep=""))
    file.remove(list.files(pattern=paste(".tif",sep="")))
    #file.remove(list.files(pattern=paste(i,"_week1.gif",sep="")))
  }
}

end.time <- Sys.time()

print(end.time-start.time)















