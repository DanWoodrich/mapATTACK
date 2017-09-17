start.time <- Sys.time()
#Daniel Woodrich
#June 2017
#AFSC
#Monthly presence map generator
#daniel.woodrich@noaa.gov
#Create multiplots and gifs that summarize presence. Automated: should be able to handle varying species, years, and gaps in data, over different regions.

#To do list before running. 
#1. Download imagemagick (ImageMagick-7.0.6-0-Q16-x64-dll.exe) from http://imagemagick.org/script/index.php. This is a preapproved program, but need to contact IT to install.
#If this version is no longer available, download the most recent version but check to see if magick.exe is still pathable (within the program files).
#2. Uncomment the following block of code to install each package. Comment out if running the script multiple times on the same computer. 

#install mapping packages. Must be done once when running on a new computer and can then be commented out. 
#install.packages("raster")
#install.packages("rgdal")
#install.packages("ggplot2")
#install.packages("ggmap")
#install.packages("qmap")
#install.packages("rasterVis")
#install.packages("viridis")
#install.packages("ggthemes")
#install.packages("rgeos")
#install.packages("scales")
#install.packages("rasterImage")
#install.packages("png")
#install.packages("animation")
#install.packages("grid")

#load mapping package libraries
library(raster)
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


#define useful functions
scale_x_longitude <- function(xmin=-180, xmax=180, step=1, ...) {
  xbreaks <- seq(xmin,xmax,step)
  xlabels <- unlist(lapply(xbreaks, function(x) ifelse(x < 0, parse(text=paste0(-1*x,"*W")), ifelse(x > 0, parse(text=paste0(x,"*E")),x))))
  return(scale_x_continuous("Longitude", breaks = xbreaks, labels = xlabels, expand = c(0, 0), ...))
}

scale_y_latitude <- function(ymin=-90, ymax=90, step=0.5, ...) {
  ybreaks <- seq(ymin,ymax,step)
  ylabels <- unlist(lapply(ybreaks, function(x) ifelse(x < 0, parse(text=paste0(-1*x,"*S")), ifelse(x > 0, parse(text=paste0(x,"*N")),x))))
  return(scale_y_continuous("Latitude", breaks = ybreaks, labels = ylabels, expand = c(0, 0), ...))
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

plot_xyax <- function(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    geom_polygon(data=AKCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=RusCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=CHXarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("red",0.75), size=0.75) +
    geom_polygon(data=AWarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("yellow",0.75), size=0.75) +
    scale_fill_gradientn(colours=raster_color_palette,guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak) +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    geom_point(data=pdata_sp_y_m, aes(x=Long, y=Lat, size = PercDayWcalls,shape=PercDWCis0,color=PercDWCis0),stroke=1.5,fill="dodgerblue2")+
    scale_radius(name=" ",limits=c(1,100),range=c(5,50),guide = guide_legend(override.aes = list(shape=21,colour = "black",fill="dodgerblue2")))+
    scale_color_manual(name="Symbols",values = c('yes' = "black", 'no' = "black"),guide=FALSE)+
    scale_shape_manual(name="Symbols ",values = c('no' = 21,'yes' = no_presence_symbol),guide=FALSE)+
    geom_text(data=pdata_sp_y_m,aes(x=(Long+(0.5+PercDayWcalls/130)), y=(Lat+(0.04+PercDayWcalls/260)),label=ifelse(X.DaysWrecs<low_effort_threshold,"*",'')),size=15) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position = c(legend_position[1],legend_position[2]),
          legend.key=element_rect(colour=alpha('gray50',0.0),fill=alpha('gray50',0.0),size=0.5),
          legend.background = element_rect(colour = alpha('gray50',0.0), fill = alpha('gray50',0.0)),
          legend.text=element_text(size=legend_text_size,color="white"),legend.key.size = unit(0, 'lines'),
          legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"),
          axis.text=element_text(size=axis_text_size_slides))+
    annotate(geom="text",zero_value_scale_position[1],zero_value_scale_position[2],label="0",color="white",size=(legend_text_size*0.375))+
    annotate(geom="text",month_label_position[1],month_label_position[2],label=paste(monthvec[pdata_sp_y_m[1,6]]),color="white",size=month_label_size,fontface=2)+
    geom_vline(xintercept=seq(longlow,Boundaries[2],longbreak),color=alpha('black',gridlines_transparency))+
    geom_hline(yintercept=seq(latlow,Boundaries[4],latbreak),color=alpha('black',gridlines_transparency))
}

plot_xax <- function(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    geom_polygon(data=AKCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=RusCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=CHXarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("red",0.75), size=0.75) +
    geom_polygon(data=AWarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("yellow",0.75), size=0.75) +
    scale_fill_gradientn(colours=raster_color_palette,guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak) +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    geom_point(data=pdata_sp_y_m, aes(x=Long, y=Lat, size = PercDayWcalls,shape=PercDWCis0,color=PercDWCis0),stroke=1.5,fill="dodgerblue2")+
    scale_radius(name=" ",limits=c(1,100),range=c(5,50),guide = guide_legend(override.aes = list(shape=21,colour = "black",fill="dodgerblue2")))+
    scale_color_manual(name="Symbols",values = c('yes' = "black", 'no' = "black"),guide=FALSE)+
    scale_shape_manual(name="Symbols ",values = c('no' = 21,'yes' = no_presence_symbol),guide=FALSE)+
    geom_text(data=pdata_sp_y_m,aes(x=(Long+(0.5+PercDayWcalls/130)), y=(Lat+(0.04+PercDayWcalls/260)),label=ifelse(X.DaysWrecs<low_effort_threshold,"*",'')),size=15) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          legend.position = c(legend_position[1],legend_position[2]),
          legend.key=element_rect(colour=alpha('gray50',0.0),fill=alpha('gray50',0.0),size=0.5),
          legend.background = element_rect(colour = alpha('gray50',0.0), fill = alpha('gray50',0.0)),
          legend.text=element_text(size=legend_text_size,color="white"),legend.key.size = unit(0, 'lines'),
          legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"),
          axis.text.x=element_text(size=axis_text_size_slides))+
    annotate(geom="text",zero_value_scale_position[1],zero_value_scale_position[2],label="0",color="white",size=(legend_text_size*0.375))+
    annotate(geom="text",month_label_position[1],month_label_position[2],label=paste(monthvec[pdata_sp_y_m[1,6]]),color="white",size=month_label_size,fontface=2)+
    geom_vline(xintercept=seq(longlow,Boundaries[2],longbreak),color=alpha('black',gridlines_transparency))+
    geom_hline(yintercept=seq(latlow,Boundaries[4],latbreak),color=alpha('black',gridlines_transparency))
}

plot_yax <- function(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    geom_polygon(data=AKCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=RusCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=CHXarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("red",0.75), size=0.75) +
    geom_polygon(data=AWarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("yellow",0.75), size=0.75) +
    scale_fill_gradientn(colours=raster_color_palette,guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak) +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    geom_point(data=pdata_sp_y_m, aes(x=Long, y=Lat, size = PercDayWcalls,shape=PercDWCis0,color=PercDWCis0),stroke=1.5,fill="dodgerblue2")+
    scale_radius(name=" ",limits=c(1,100),range=c(5,50),guide = guide_legend(override.aes = list(shape=21,colour = "black",fill="dodgerblue2")))+
    scale_color_manual(name="Symbols",values = c('yes' = "black", 'no' = "black"),guide=FALSE)+
    scale_shape_manual(name="Symbols ",values = c('no' = 21,'yes' = no_presence_symbol),guide=FALSE)+
    geom_text(data=pdata_sp_y_m,aes(x=(Long+(0.5+PercDayWcalls/130)), y=(Lat+(0.04+PercDayWcalls/260)),label=ifelse(X.DaysWrecs<low_effort_threshold,"*",'')),size=15) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_blank(),
          legend.position = c(legend_position[1],legend_position[2]),
          legend.key=element_rect(colour=alpha('gray50',0.0),fill=alpha('gray50',0.0),size=0.5),
          legend.background = element_rect(colour = alpha('gray50',0.0), fill = alpha('gray50',0.0)),
          legend.text=element_text(size=legend_text_size,color="white"),legend.key.size = unit(0, 'lines'),
          legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"),
          axis.text.y=element_text(size=axis_text_size_slides))+
    annotate(geom="text",zero_value_scale_position[1],zero_value_scale_position[2],label="0",color="white",size=(legend_text_size*0.375))+
    annotate(geom="text",month_label_position[1],month_label_position[2],label=paste(monthvec[pdata_sp_y_m[1,6]]),color="white",size=month_label_size,fontface=2)+
    geom_vline(xintercept=seq(longlow,Boundaries[2],longbreak),color=alpha('black',gridlines_transparency))+
    geom_hline(yintercept=seq(latlow,Boundaries[4],latbreak),color=alpha('black',gridlines_transparency))
}
plot_nax <- function(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    geom_polygon(data=AKCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=RusCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=CHXarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("red",0.75), size=0.75) +
    geom_polygon(data=AWarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("yellow",0.75), size=0.75) +
    scale_fill_gradientn(colours=raster_color_palette,guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak) +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    geom_point(data=pdata_sp_y_m, aes(x=Long, y=Lat, size = PercDayWcalls,shape=PercDWCis0,color=PercDWCis0),stroke=1.5,fill="dodgerblue2")+
    scale_radius(name=" ",limits=c(1,100),range=c(5,50),guide = guide_legend(override.aes = list(shape=21,colour = "black",fill="dodgerblue2")))+
    scale_color_manual(name="Symbols",values = c('yes' = "black", 'no' = "black"),guide=FALSE)+
    scale_shape_manual(name="Symbols ",values = c('no' = 21,'yes' = no_presence_symbol),guide=FALSE)+
    geom_text(data=pdata_sp_y_m,aes(x=(Long+(0.5+PercDayWcalls/130)), y=(Lat+(0.04+PercDayWcalls/260)),label=ifelse(X.DaysWrecs<low_effort_threshold,"*",'')),size=15) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text=element_blank(),
          legend.position = c(legend_position[1],legend_position[2]),
          legend.key=element_rect(colour=alpha('gray50',0.0),fill=alpha('gray50',0.0),size=0.5),
          legend.background = element_rect(colour = alpha('gray50',0.0), fill = alpha('gray50',0.0)),
          legend.text=element_text(size=legend_text_size,color="white"),legend.key.size = unit(0, 'lines'),
          legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"))+
    annotate(geom="text",zero_value_scale_position[1],zero_value_scale_position[2],label="0",color="white",size=(legend_text_size*0.375))+
    annotate(geom="text",month_label_position[1],month_label_position[2],label=paste(monthvec[pdata_sp_y_m[1,6]]),color="white",size=month_label_size,fontface=2)+
    geom_vline(xintercept=seq(longlow,Boundaries[2],longbreak),color=alpha('black',gridlines_transparency))+
    geom_hline(yintercept=seq(latlow,Boundaries[4],latbreak),color=alpha('black',gridlines_transparency))
}

plot_xyax_nl <- function(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    geom_polygon(data=AKCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=RusCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=CHXarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("red",0.75), size=0.75) +
    geom_polygon(data=AWarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("yellow",0.75), size=0.75) +
    scale_fill_gradientn(colours=raster_color_palette,guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak) +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    geom_point(data=pdata_sp_y_m, aes(x=Long, y=Lat, size = PercDayWcalls,shape=PercDWCis0,color=PercDWCis0),stroke=1.5,fill="dodgerblue2")+
    scale_radius(name=" ",limits=c(1,100),range=c(5,50),guide = guide_legend(override.aes = list(shape=21,colour = "black",fill="dodgerblue2")))+
    scale_color_manual(name="Symbols",values = c('yes' = "black", 'no' = "black"),guide=FALSE)+
    scale_shape_manual(name="Symbols ",values = c('no' = 21,'yes' = no_presence_symbol),guide=FALSE)+
    geom_text(data=pdata_sp_y_m,aes(x=(Long+(0.5+PercDayWcalls/130)), y=(Lat+(0.04+PercDayWcalls/260)),label=ifelse(X.DaysWrecs<low_effort_threshold,"*",'')),size=15) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position = "none",
          axis.text=element_text(size=axis_text_size_slides))+
    annotate(geom="text",month_label_position[1],month_label_position[2],label=paste(monthvec[pdata_sp_y_m[1,6]]),color="white",size=month_label_size,fontface=2)+
    geom_vline(xintercept=seq(longlow,Boundaries[2],longbreak),color=alpha('black',gridlines_transparency))+
    geom_hline(yintercept=seq(latlow,Boundaries[4],latbreak),color=alpha('black',gridlines_transparency))
}

plot_xax_nl <- function(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    geom_polygon(data=AKCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=RusCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=CHXarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("red",0.75), size=0.75) +
    geom_polygon(data=AWarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("yellow",0.75), size=0.75) +
    scale_fill_gradientn(colours=raster_color_palette,guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak) +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    geom_point(data=pdata_sp_y_m, aes(x=Long, y=Lat, size = PercDayWcalls,shape=PercDWCis0,color=PercDWCis0),stroke=1.5,fill="dodgerblue2")+
    scale_radius(name=" ",limits=c(1,100),range=c(5,50),guide = guide_legend(override.aes = list(shape=21,colour = "black",fill="dodgerblue2")))+
    scale_color_manual(name="Symbols",values = c('yes' = "black", 'no' = "black"),guide=FALSE)+
    scale_shape_manual(name="Symbols ",values = c('no' = 21,'yes' = no_presence_symbol),guide=FALSE)+
    geom_text(data=pdata_sp_y_m,aes(x=(Long+(0.5+PercDayWcalls/130)), y=(Lat+(0.04+PercDayWcalls/260)),label=ifelse(X.DaysWrecs<low_effort_threshold,"*",'')),size=15) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          legend.position = "none",
          axis.text.x=element_text(size=axis_text_size_slides))+
    annotate(geom="text",month_label_position[1],month_label_position[2],label=paste(monthvec[pdata_sp_y_m[1,6]]),color="white",size=month_label_size,fontface=2)+
    geom_vline(xintercept=seq(longlow,Boundaries[2],longbreak),color=alpha('black',gridlines_transparency))+
    geom_hline(yintercept=seq(latlow,Boundaries[4],latbreak),color=alpha('black',gridlines_transparency))
}

plot_yax_nl <- function(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    geom_polygon(data=AKCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=RusCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=CHXarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("red",0.75), size=0.75) +
    geom_polygon(data=AWarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("yellow",0.75), size=0.75) +
    scale_fill_gradientn(colours=raster_color_palette,guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak) +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    geom_point(data=pdata_sp_y_m, aes(x=Long, y=Lat, size = PercDayWcalls,shape=PercDWCis0,color=PercDWCis0),stroke=1.5,fill="dodgerblue2")+
    scale_radius(name=" ",limits=c(1,100),range=c(5,50),guide = guide_legend(override.aes = list(shape=21,colour = "black",fill="dodgerblue2")))+
    scale_color_manual(name="Symbols",values = c('yes' = "black", 'no' = "black"),guide=FALSE)+
    scale_shape_manual(name="Symbols ",values = c('no' = 21,'yes' = no_presence_symbol),guide=FALSE)+
    geom_text(data=pdata_sp_y_m,aes(x=(Long+(0.5+PercDayWcalls/130)), y=(Lat+(0.04+PercDayWcalls/260)),label=ifelse(X.DaysWrecs<low_effort_threshold,"*",'')),size=15) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_blank(),
          legend.position = "none",
          axis.text.y=element_text(size=axis_text_size_slides))+
    annotate(geom="text",month_label_position[1],month_label_position[2],label=paste(monthvec[pdata_sp_y_m[1,6]]),color="white",size=month_label_size,fontface=2)+
    geom_vline(xintercept=seq(longlow,Boundaries[2],longbreak),color=alpha('black',gridlines_transparency))+
    geom_hline(yintercept=seq(latlow,Boundaries[4],latbreak),color=alpha('black',gridlines_transparency))
}
plot_nax_nl <- function(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    geom_polygon(data=AKCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=RusCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=CHXarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("red",0.75), size=0.75) +
    geom_polygon(data=AWarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("yellow",0.75), size=0.75) +
    scale_fill_gradientn(colours=raster_color_palette,guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak) +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    geom_point(data=pdata_sp_y_m, aes(x=Long, y=Lat, size = PercDayWcalls,shape=PercDWCis0,color=PercDWCis0),stroke=1.5,fill="dodgerblue2")+
    scale_radius(name=" ",limits=c(1,100),range=c(5,50),guide = guide_legend(override.aes = list(shape=21,colour = "black",fill="dodgerblue2")))+
    scale_color_manual(name="Symbols",values = c('yes' = "black", 'no' = "black"),guide=FALSE)+
    scale_shape_manual(name="Symbols ",values = c('no' = 21,'yes' = no_presence_symbol),guide=FALSE)+
    geom_text(data=pdata_sp_y_m,aes(x=(Long+(0.5+PercDayWcalls/130)), y=(Lat+(0.04+PercDayWcalls/260)),label=ifelse(X.DaysWrecs<low_effort_threshold,"*",'')),size=15) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text=element_blank(),
          legend.position = "none")+
    annotate(geom="text",month_label_position[1],month_label_position[2],label=paste(monthvec[pdata_sp_y_m[1,6]]),color="white",size=month_label_size,fontface=2)+
    geom_vline(xintercept=seq(longlow,Boundaries[2],longbreak),color=alpha('black',gridlines_transparency))+
    geom_hline(yintercept=seq(latlow,Boundaries[4],latbreak),color=alpha('black',gridlines_transparency))
}

plot_blankxy <- function(pdata_sp_y_m,MAP_df,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    scale_fill_gradientn(colours=c("white","white"),guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak) +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position = "none",
          axis.text=element_text(size=axis_text_size_slides))
}

plot_blankx <- function(pdata_sp_y_m,MAP_df,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    scale_fill_gradientn(colours=c("white","white"),guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak) +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position = "none",
          axis.text=element_text(size=axis_text_size_slides))
}

plot_blanky <- function(pdata_sp_y_m,MAP_df,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    scale_fill_gradientn(colours=c("white","white"),guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak) +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position = "none",
          axis.text=element_text(size=axis_text_size_slides))
}

plot_blankn <- function(pdata_sp_y_m,MAP_df,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    scale_fill_gradientn(colours=c("white","white"),guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak) +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          legend.position = "none")
}

plot_xyax_wyear <- function(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_gif,month_and_year_label_position){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    geom_polygon(data=AKCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=RusCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=CHXarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("red",0.75), size=0.75) +
    geom_polygon(data=AWarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("yellow",0.75), size=0.75) +
    scale_fill_gradientn(colours=raster_color_palette,guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak) +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    geom_point(data=pdata_sp_y_m, aes(x=Long, y=Lat, size = PercDayWcalls,shape=PercDWCis0,color=PercDWCis0),stroke=1.5,fill="dodgerblue2")+
    scale_radius(name=" ",limits=c(1,100),range=c(legend_size_modifier_ind,legend_size_modifier_ind*10),guide = guide_legend(override.aes = list(shape=21,colour = "black",fill="dodgerblue2")))+
    scale_color_manual(name="Symbols",values = c('yes' = "black", 'no' = "black"),guide=FALSE)+
    scale_shape_manual(name="Symbols ",values = c('no' = 21,'yes' = no_presence_symbol),guide=FALSE)+
    geom_text(data=pdata_sp_y_m,aes(x=(Long+(0.5+PercDayWcalls/130)), y=(Lat+(0.04+PercDayWcalls/260)),label=ifelse(X.DaysWrecs<low_effort_threshold,"*",'')),size=15) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position = c(legend_position_ind[1],legend_position_ind[2]),
          legend.key=element_rect(colour=alpha('gray50',0.0),fill=alpha('gray50',0.0),size=0.5),
          legend.background = element_rect(colour = alpha('gray50',0.0), fill = alpha('gray50',0.0)),
          legend.text=element_text(size=legend_text_size_ind,color="white"),legend.key.size = unit(0, 'lines'),
          legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"),
          axis.text=element_text(size=axis_text_size_gif))+
    annotate(geom="text",zero_value_scale_position_ind[1],zero_value_scale_position_ind[2],label="0",color="white",size=(legend_text_size_ind*0.375))+
    annotate(geom="text",month_and_year_label_position[1],month_and_year_label_position[2],label=paste(monthvec[pdata_sp_y_m[1,6]],pdata_sp_y_m[1,5]),color="white",size=month_label_size_ind,fontface=2)+
    geom_vline(xintercept=seq(longlow,Boundaries[2],longbreak),color=alpha('black',gridlines_transparency))+
    geom_hline(yintercept=seq(latlow,Boundaries[4],latbreak),color=alpha('black',gridlines_transparency))
}

plot_xyax_mdy<- function(pdata_sp_y_d,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_gif,month_and_year_label_position){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    geom_polygon(data=AKCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=RusCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=CHXarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("red",0.75), size=0.75) +
    geom_polygon(data=AWarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("yellow",0.75), size=0.75) +
    scale_fill_gradientn(colours=raster_color_palette,guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak) +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    geom_point(data=pdata_sp_y_d, aes(x=Long, y=Lat, size = PercDayWcalls,shape=PercDWCis0,color=PercDWCis0),stroke=1.5,fill="dodgerblue2")+
    scale_radius(name=" ",limits=c(1,100),range=c(legend_size_modifier_ind,legend_size_modifier_ind*10),guide = guide_legend(override.aes = list(shape=21,colour = "black",fill="dodgerblue2")))+
    scale_color_manual(name="Symbols",values = c('yes' = "black", 'no' = "black"),guide=FALSE)+
    scale_shape_manual(name="Symbols ",values = c('no' = 21,'yes' = no_presence_symbol),guide=FALSE)+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position = c(legend_position_ind[1],legend_position_ind[2]),
          legend.key=element_rect(colour=alpha('gray50',0.0),fill=alpha('gray50',0.0),size=0.5),
          legend.background = element_rect(colour = alpha('gray50',0.0), fill = alpha('gray50',0.0)),
          legend.text=element_text(size=legend_text_size_ind,color="white"),legend.key.size = unit(0, 'lines'),
          legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"),
          axis.text=element_text(size=axis_text_size_gif))+
    annotate(geom="text",zero_value_scale_position_ind[1],zero_value_scale_position_ind[2],label="0",color="white",size=(legend_text_size_ind*0.375))+
    annotate(geom="text",month_and_year_label_position[1],month_and_year_label_position[2],label=as.character(pdata_sp_y_d[1,7]),color="white",size=month_label_size_ind,fontface=2)+
    geom_vline(xintercept=seq(longlow,Boundaries[2],longbreak),color=alpha('black',gridlines_transparency))+
    geom_hline(yintercept=seq(latlow,Boundaries[4],latbreak),color=alpha('black',gridlines_transparency))
}


#OPTIONAL USER INPUTS. Defaults specified. 
run_name <- "CHAOZ-X map run test 4"                            #name your run. Unique names will generate a new folder with this name in \\nmfs\akc-nmml\CAEP\Acoustics\ArcMAP\Mapping with R\Output files\. Duplicate names will overwrite that folder. 
test_mode <- "y"                                               #test new graph parameters w/o making full complement of figures
gif_interval <- "day"                                          #"day" or "month"
run_type <- "gifs"                                             #"slides","gifs", or "both 

num_lat_ticks <- 4
num_long_ticks <- 4
low_effort_threshold <- 15                                           #threshold below which an asterisk is placed on the dots. 
raster_color_palette <- c("white",lighten("skyblue1"),darken("skyblue1"),"steelblue4","steelblue4",darken("steelblue4")) 
legend_position <- c(.78,.325)                                       #scaled from 0-1, easiest to just enter test values here to reposition. 
legend_position_ind <- c(.875,.22)  
scale_symbol_position <- c(-157.185,70.175)                            #unfortunately need to play around with this one too, scaled in degrees so can look at axes to reposition.
zero_value_scale_position <- c(-154.60,scale_symbol_position[2])
scale_symbol_position_ind <- c(-155.288,68.46)                            #unfortunately need to play around with this one too, scaled in degrees so can look at axes to reposition.
zero_value_scale_position_ind <- c(-153.725,scale_symbol_position_ind[2])
month_label_position <- c(-154.3,65.85)                                   #for grid and individual plot month labels, lat/long
month_and_year_label_position <- c(-155.2,65.7)         #for slide month and year labels, lat/long
no_presence_symbol <- 3                                              #see http://www.statmethods.net/advgraphs/parameters.html for more options
no_presence_symbol_size <- 6
legend_text_size <- 40
legend_text_size_ind <- 30
legend_size_modifier_ind <- 3                                         #bigger # = bigger legend. def 
gridlines_transparency <- 0.2                                        #0-1, 0 being fully transparent
month_label_size <- 24
month_label_size_ind <- 17
axis_text_size_slides <- 24                                         #text size for axes, multiplot
axis_text_size_gif <- 12                                            #text size for gif and single slides. 
image_height <- 11.0558                                             #height of output image (just the map region). Default value (11.0558) determined from arcGIS map window dimensions of  CHAOZ-X project extent.
image_width <-  9.3902                                              #width of output image (just the map region). Default value (9.3902) determined from arcGIS map window dimensions of CHAOZ-X project extent.
image_correction_height <- ((0.00129*axis_text_size_slides)+1.007246) #correction which changes plot size so that map regions are the same w & w/o axes. Eventually, find math relationship and base it on axis text size. Default 1.02266 for CHAOZ-X dimensions and size 12 text. 
image_correction_width <- ((0.0032*axis_text_size_slides)+1.007246)   #correction which changes plot size so that map regions are the same w & w/o axes. Eventually, find math relationship and base it on axis text size. Default 1.05405405 for CHAOZ-X dimensions and size 12 text.                  
gifspeed <- 160                                                     #larger numbers are slower, unsure of units. CHAOZ-X default = 160.
gifspeed_day <- 40                                                     #larger numbers are slower, unsure of units. CHAOZ-X default = 160.
gif_month_res <- 200
gif_day_res <- 50
custom_extent <- "n"                                                  #"y" or "n". Will default to extent of etopo1bedrk.tif file if "n".




input_folder <- "//nmfs/akc-nmml/CAEP/Acoustics/ArcMAP/Mapping with R/Input files" #file path for necessary data files, in quotations, with forward slash
output_folder <- "//nmfs/akc-nmml/CAEP/Acoustics/ArcMAP/Mapping with R/Output files" #file path for map output, in quotations, with forward slash


###################import data files.###################################################

#bathy raster. If loading from arcGIS: export data, select "Data frame Current" for option "extent". For "spatial reference", select "Raster Dataset (Original)". Specify name to match path below:
MAP <- raster(paste(input_folder,"/Rasters/","etopo1bedrk1.tif",sep=""))
#Import chukchi and AW region shapefile. 
CHXarea <- readOGR(dsn=paste(input_folder,"/Shapefiles",sep=""), layer="CHX_area2r")
AWarea <- readOGR(dsn=paste(input_folder,"/Shapefiles",sep=""), layer="AW_area2r")
#import chukchi and AW study area shapefiles. From ArcGIS: Right click on layer, go to data, select  "all features in view extent", "same crs as data frame" options. MAke sure saved name matches paths
AKCont <- readOGR(dsn=paste(input_folder,"/Shapefiles",sep=""), layer="AlaskaCont")
RusCont <- readOGR(dsn=paste(input_folder,"/Shapefiles",sep=""), layer="RussiaCont")
#presence data (turn to file.choose() in final vers)
pdata <- read.csv(paste(input_folder,"/","Data frame","/","AWCHX_LTMonthlymapR.csv",sep=""))
pdatad <- read.csv(paste(input_folder,"/","Data frame","/","AWCHX_LTDailymapR.csv",sep=""))

########################Map data processing#######################

#Easier to just mess with data here than try to change scaling in figure. Get rid of positive values so bathymetry will scale better
MAP[MAP>=-3]=-3  #set negative values to a point so that intermediate values will scale more naturally
#MAP[MAP<=-1300] <- -1300
MAP <- (MAP*-1)+1
#transform data to create a beautiful depth illusion
MAP <- log(MAP,2)

#reproject shapefiles to match raster map projection. 
CHXarea <- spTransform(CHXarea,"+proj=longlat")
AWarea <- spTransform(AWarea,"+proj=longlat")
AKCont_longlat <- spTransform(AKCont,"+proj=longlat")
RusCont_longlat <- spTransform(RusCont,"+proj=longlat")

#convert raster into useable ggplot2 format
MAP_spdf <- as(MAP,"SpatialPixelsDataFrame")
MAP_df <- as.data.frame(MAP_spdf)
colnames(MAP_df) <- c("value","x","y")

if(custom_extent=="y"){
  latlow <- NULL #default NULL. either leave all 4 parameters null or specify each, in +/- degrees (ex: latlow <- 66, longlow <- -175)
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

#Crop shapefiles down to match raster
AKCrop <- crop(AKCont_longlat,extent(Boundaries))
RusCrop <- crop(RusCont_longlat,extent(Boundaries))


#################Presence data processing######################
monthvec <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
pdata$PercDWCis0 <- ifelse(pdata$PercDayWcalls==0,"yes","no")
pdatad$PercDWCis0 <- ifelse(pdatad$PercDayWcalls==0,"yes","no")

pdata$PercDayWcalls[pdata$PercDayWcalls==0] <- no_presence_symbol_size
pdatad$PercDayWcalls[pdatad$PercDayWcalls==0] <- no_presence_symbol_size
#removes rows outside of raster extent
pdata <- pdata[!(pdata$Lat<=Boundaries[3]|pdata$Lat>=Boundaries[4]|pdata$Long<=Boundaries[1]|pdata$Long>=Boundaries[2]),]
pdatad <- pdatad[!(pdatad$Lat<=Boundaries[3]|pdatad$Lat>=Boundaries[4]|pdatad$Long<=Boundaries[1]|pdatad$Long>=Boundaries[2]),]


topfolder <- paste(output_folder,"/",run_name,sep="")
dir.create(topfolder)

#plot blank files w/ axes
blankfolder <- paste(topfolder,"/","Blanks",sep="")
dir.create(blankfolder)

pdata_sp <- pdata[pdata$Species=="Gunshot",]
pdata_sp_y <- pdata_sp[pdata_sp$Year=="2010",]
pdata_sp_m <- pdata_sp[pdata_sp$Month==pdata_sp$Month[1],]

png(paste(blankfolder,"/blank_xy.png",sep=""),width=(image_width*image_correction_width),height=(image_height*image_correction_height),units="in",res=300) 
print(plot_blankxy(pdata_sp_y_m,MAP_df,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides))
dev.off()

png(paste(blankfolder,"/blank_x.png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=300) 
print(plot_blankx(pdata_sp_y_m,MAP_df,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides))
dev.off()

png(paste(blankfolder,"/blank_y.png",sep=""),width=(image_width*image_correction_width),height=(image_height),units="in",res=300) 
print(plot_blanky(pdata_sp_y_m,MAP_df,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides))
dev.off()

png(paste(blankfolder,"/blank_n.png",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
print(plot_blankn(pdata_sp_y_m,MAP_df,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec))
dev.off()

if(test_mode !="y"){
if(run_type=="slides"|run_type=="both"){

###########make multiplot##############



slide_folder <- paste(topfolder,"/","Slides and component plots",sep="")
dir.create(slide_folder)
for(p in unique(pdata$Species)){
  pdata_sp <- pdata[pdata$Species==p,]
  sp_folder <- paste(slide_folder,"/",p,sep="")
  dir.create(sp_folder)
  for(x in unique(pdata_sp$Year)){
    pdata_sp_y<- pdata_sp[pdata_sp$Year==x,]
    months_in_y <- unique(pdata_sp_y$Month)
    months_in_y <- sort(months_in_y)
    month_span <- seq(months_in_y[1],tail(months_in_y,1))
    year_plots <- paste(sp_folder,"/",x,"_plots",sep="")
    dir.create(year_plots)
    if(length(month_span)<=4){
          pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[1],]
          dummy_row <- data.frame(as.character(pdata_sp_y_m[1,1])," ",scale_symbol_position[2],scale_symbol_position[1],pdata_sp_y_m[1,5],pdata_sp_y_m[1,6],pdata_sp_y_m[1,7],no_presence_symbol_size,0,low_effort_threshold+1,"yes")
          colnames(dummy_row)<- colnames(pdata_sp_y_m)
          pdata_sp_y_m <- rbind(pdata_sp_y_m,dummy_row)
          png(paste(year_plots,"/",monthvec[month_span[1]],"_xy",".png",sep=""),width=(image_width*image_correction_width),height=(image_height*image_correction_height),units="in",res=300) 
          print(plot_xyax(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
          dev.off()
       if(month_span[2] %in% months_in_y){
          pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[2],] 
          png(paste(year_plots,"/",monthvec[month_span[2]],"_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=300) 
          print(plot_xax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
          dev.off()
          }
       if(month_span[3] %in% months_in_y){
          pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[3],] 
          png(paste(year_plots,"/",monthvec[month_span[3]],"_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=300) 
          print(plot_xax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
          dev.off()
          }
       if(month_span[4] %in% months_in_y){
          pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[4],] 
          png(paste(year_plots,"/",monthvec[month_span[4]],"_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=300) 
          print(plot_xax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
          dev.off()
          }
      png(paste(sp_folder,"/Slide",substr(as.character(x),3,4),".png",sep=""),width=((image_width*image_correction_width)+image_width*3),height=((image_height*image_correction_height)+(image_height*2)),units="in",res=150)
      par(mai=c(0,0,0,0),xpd=NA, oma=c(0,0,0,0))
      layout(matrix(1:12, ncol=4, byrow=TRUE),widths=c((image_width*image_correction_width),image_width,image_width,image_width),heights=c((image_height*image_correction_height),image_height,image_height))
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[1]],"_xy",".png",sep="")),0,0,1,1)
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")  
      if(month_span[2] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[2]],"_x",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_x.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")  
      if(month_span[3] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[3]],"_x",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_x.png",sep="")),0,0,1,1) 
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")  
      if(month_span[4] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[4]],"_x",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_x.png",sep="")),0,0,1,1)
      }
      for(a in 5:12){
        plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
        rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)  
      }
    }
    if(4<length(month_span) & length(month_span)<=8){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[1],]
        dummy_row <- data.frame(as.character(pdata_sp_y_m[1,1])," ",scale_symbol_position[2],scale_symbol_position[1],pdata_sp_y_m[1,5],pdata_sp_y_m[1,6],pdata_sp_y_m[1,7],no_presence_symbol_size,0,low_effort_threshold+1,"yes")
        colnames(dummy_row)<- colnames(pdata_sp_y_m)
        pdata_sp_y_m <- rbind(pdata_sp_y_m,dummy_row)
        png(paste(year_plots,"/",monthvec[month_span[1]],"_y",".png",sep=""),width=(image_width*image_correction_width),height=(image_height),units="in",res=300) 
        print(plot_yax(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
      if(month_span[2] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[2],] 
        png(paste(year_plots,"/",monthvec[month_span[2]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
        print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
        dev.off()
        }
      if(month_span[3] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[3],] 
        png(paste(year_plots,"/",monthvec[month_span[3]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
        print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
        dev.off()
        }
      if(month_span[4] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[4],]
        png(paste(year_plots,"/",monthvec[month_span[4]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
        print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
        dev.off()
        }
      if(month_span[5] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[5],] 
        png(paste(year_plots,"/",monthvec[month_span[5]],"_xy",".png",sep=""),width=(image_width*image_correction_width),height=(image_height*image_correction_height),units="in",res=300) 
        print(plot_xyax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
        }
      if(month_span[6] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[6],] 
        png(paste(year_plots,"/",monthvec[month_span[6]],"_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=300) 
        print(plot_xax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
        }
      if(month_span[7] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[7],] 
        png(paste(year_plots,"/",monthvec[month_span[7]],"_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=300) 
        print(plot_xax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
        }
      if(month_span[8] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[8],] 
        png(paste(year_plots,"/",monthvec[month_span[8]],"_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=300) 
        print(plot_xax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
        }
      png(paste(sp_folder,"/Slide",substr(as.character(x),3,4),".png",sep=""),width=((image_width*image_correction_width)+image_width*3),height=((image_height*image_correction_height)+(image_height*2)),units="in",res=150)
      par(mai=c(0,0,0,0),xpd=NA, oma=c(0,0,0,0))
      layout(matrix(1:12, ncol=4, byrow=TRUE),widths=c((image_width*image_correction_width),image_width,image_width,image_width),heights=c(image_height,(image_height*image_correction_height),image_height))
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[1]],"_y",".png",sep="")),0,0,1,1)
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[2] %in% months_in_y){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[2]],"_n",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[3] %in% months_in_y){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[3]],"_n",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[4] %in% months_in_y){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[4]],"_n",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[5] %in% months_in_y){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[5]],"_xy",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_xy.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")  
      if(month_span[6] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[6]],"_x",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_x.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")  
      if(month_span[7] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[7]],"_x",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_x.png",sep="")),0,0,1,1)  
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[8] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[8]],"_x",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_x.png",sep="")),0,0,1,1)  
      }
    for(b in 9:12){
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)  
    }
    }
    if(8<length(month_span) & length(month_span)<=12){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[1],]
        dummy_row <- data.frame(as.character(pdata_sp_y_m[1,1])," ",scale_symbol_position[2],scale_symbol_position[1],pdata_sp_y_m[1,5],pdata_sp_y_m[1,6],pdata_sp_y_m[1,7],no_presence_symbol_size,0,low_effort_threshold+1,"yes")
        colnames(dummy_row)<- colnames(pdata_sp_y_m)
        pdata_sp_y_m <- rbind(pdata_sp_y_m,dummy_row)
        png(paste(year_plots,"/",monthvec[month_span[1]],"_y",".png",sep=""),width=(image_width*image_correction_width),height=(image_height),units="in",res=300) 
        print(plot_yax(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
      if(month_span[2] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[2],] 
        png(paste(year_plots,"/",monthvec[month_span[2]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
        print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
        dev.off()
      }
      if(month_span[3] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[3],] 
        png(paste(year_plots,"/",monthvec[month_span[3]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
        print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
        dev.off()
      }
      if(month_span[4] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[4],]
        png(paste(year_plots,"/",monthvec[month_span[4]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
        print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
        dev.off()
      }
      if(month_span[5] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[5],] 
        png(paste(year_plots,"/",monthvec[month_span[5]],"_y",".png",sep=""),width=(image_width*image_correction_width),height=(image_height),units="in",res=300) 
        print(plot_yax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
      }
      if(month_span[6] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[6],] 
        png(paste(year_plots,"/",monthvec[month_span[6]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
        print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
        dev.off()
      }
      if(month_span[7] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[7],] 
        png(paste(year_plots,"/",monthvec[month_span[7]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
        print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
        dev.off()
      }
      if(month_span[8] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[8],] 
        png(paste(year_plots,"/",monthvec[month_span[8]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
        print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
        dev.off()
      }
      if(month_span[9] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[9],] 
        png(paste(year_plots,"/",monthvec[month_span[9]],"_xy",".png",sep=""),width=(image_width*image_correction_width),height=(image_height*image_correction_height),units="in",res=300) 
        print(plot_xyax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
      }
      if(month_span[10] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[10],] 
        png(paste(year_plots,"/",monthvec[month_span[10]],"_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=300) 
        print(plot_xax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
      }
      if(month_span[11] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[11],] 
        png(paste(year_plots,"/",monthvec[month_span[11]],"_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=300) 
        print(plot_xax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
      }
      if(month_span[12] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[12],] 
        png(paste(year_plots,"/",monthvec[month_span[12]],"_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=300) 
        print(plot_xax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
      }
      png(paste(sp_folder,"/Slide",substr(as.character(x),3,4),".png",sep=""),width=((image_width*image_correction_width)+image_width*3),height=((image_height*image_correction_height)+(image_height*2)),units="in",res=150)
      par(mai=c(0,0,0,0),xpd=NA, oma=c(0,0,0,0))
      layout(matrix(1:12, ncol=4, byrow=TRUE),widths=c((image_width*image_correction_width),image_width,image_width,image_width),heights=c(image_height,image_height,(image_height*image_correction_height)))
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[1]],"_y",".png",sep="")),0,0,1,1)
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[2] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[2]],"_n",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[3] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[3]],"_n",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[4] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[4]],"_n",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[5] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[5]],"_y",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_y.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")  
      if(month_span[6] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[6]],"_n",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")  
      if(month_span[7] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[7]],"_n",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)  
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[8] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[8]],"_n",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)  
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[9] %in% months_in_y){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[9]],"_xy",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_xy.png",sep="")),0,0,1,1)  
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n") 
      if(month_span[10] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[10]],"_x",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_x.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n") 
      if(month_span[11] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[11]],"_x",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_x.png",sep="")),0,0,1,1) 
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[12] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[12]],"_x",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_x.png",sep="")),0,0,1,1)  
      }
    }
    dev.off()
  }
  
}

}else if(run_type=="both"|run_type=="gifs"){
  
  
################make gifs#############################
  
gif_folder <- paste(topfolder,"/","Gifs and stand alone slides",sep="")
dir.create(gif_folder)

if(gif_interval == "month"){
  
  
  for(q in unique(pdata$Species)){
    pdata_sp <- pdata[pdata$Species==q,] 
    sp_folder <- paste(gif_folder,"/",q,sep="")
    dir.create(sp_folder)
    counter <- "0001"
    for(r in unique(pdata_sp$Year)){
      pdata_sp_y<- pdata_sp[pdata_sp$Year==r,] 
      for( s in unique(pdata_sp_y$Month)){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==s,] 
        dummy_row <- data.frame(as.character(pdata_sp_y_m[1,1])," ",scale_symbol_position_ind[2],scale_symbol_position_ind[1],pdata_sp_y_m[1,5],pdata_sp_y_m[1,6],pdata_sp_y_m[1,7],no_presence_symbol_size,0,low_effort_threshold+1,"yes")
        colnames(dummy_row)<- colnames(pdata_sp_y_m)
        pdata_sp_y_m <- rbind(pdata_sp_y_m,dummy_row)
        finalfilename <- paste(counter,substr(q,1,3),"_",substr(monthvec[s],1,3),substr(as.character(r),3,4),sep="")
        png(paste(sp_folder,"/",finalfilename,".png",sep=""),width=(image_width*1.054054),height=(image_height*1.02266),units="in",res=gif_month_res) 
        print(plot_xyax_wyear(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_gif,month_and_year_label_position))
        dev.off()
        counter <- as.numeric(counter)
        counter <- counter +1
        if(nchar(counter)==1){
          counter <- paste("000",counter,sep="")
        }
        if(nchar(counter)==2){
          counter <- paste("00",counter,sep="")
        }
        if(nchar(counter)==3){
          counter <- paste("0",counter,sep="")
        }
        else{
          counter <- as.character(counter)
        }
      }
    }
    setwd(sp_folder)
    system(paste('"C:\\Program Files\\ImageMagick-7.0.6-Q16\\magick.exe" -delay ',gifspeed,' *.png ',q,".gif",sep=""))
  }
  }else{
    
    
    #just bearded and bowhead for 1st run 
    pdatad <- pdatad[pdatad$Species=="Bearded"|pdatad$Species=="Bowhead",] 
    pdatad <- pdatad[1:500,] 
    
    for(q in unique(pdatad$Species)){
      pdatad_sp <- pdatad[pdatad$Species==q,] 
      sp_folder <- paste(gif_folder,"/",q,sep="")
      dir.create(sp_folder)
      counter <- "00001"
      for(r in unique(pdatad_sp$Year)){
        pdatad_sp_y<- pdatad_sp[pdatad_sp$Year==r,] 
        for(s in unique(pdatad_sp_y$Date)){
          pdatad_sp_y_d <- pdatad_sp_y[pdatad_sp_y$Date==s,] 
          dummy_row <- data.frame(as.character(pdatad_sp_y_d[1,1])," ",scale_symbol_position_ind[2],scale_symbol_position_ind[1],pdatad_sp_y_d[1,5],pdatad_sp_y_d[1,6],pdatad_sp_y_d[1,7],no_presence_symbol_size,0,low_effort_threshold+1,"yes")
          colnames(dummy_row)<- colnames(pdatad_sp_y_d)
          pdatad_sp_y_d <- rbind(pdatad_sp_y_d,dummy_row)
          finalfilename <- paste(counter,substr(q,1,3),"_",vapply(strsplit(s,"/"), `[`, 1, FUN.VALUE=character(1)),"_",vapply(strsplit(s,"/"), `[`, 2, FUN.VALUE=character(1)),"_", as.character(r),sep="")
          png(paste(sp_folder,"/",finalfilename,".png",sep=""),width=(image_width*1.054054),height=(image_height*1.02266),units="in",res=gif_day_res) 
          print(plot_xyax_mdy(pdatad_sp_y_d,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_gif,month_and_year_label_position))
          dev.off()
          counter <- as.numeric(counter)
          counter <- counter +1
          if(nchar(counter)==1){
            counter <- paste("0000",counter,sep="")
          }
          if(nchar(counter)==2){
            counter <- paste("000",counter,sep="")
          }
          if(nchar(counter)==3){
            counter <- paste("00",counter,sep="")
          }
          if(nchar(counter)==4){
            counter <- paste("0",counter,sep="")
          }
          else{
            counter <- as.character(counter)
          }
        }
      }
      setwd(sp_folder)
      system(paste('"C:\\Program Files\\ImageMagick-7.0.6-Q16\\magick.exe" -delay ',gifspeed_day,' *.png ',q,".gif",sep=""))
      file.remove(list.files(pattern=".png"))
    }
  }
  
  
}

#make animations: imagemagick. If you are getting errors here, make sure path at line " system(paste('"C:\\Program Files\\ImageMagick-7.0.6-Q16\\magick.exe" -delay 160 *.png ',q,".gif",sep=""))" matches the install directory on your computer, and the correct version of imagemagick. If a new version is available, check to make sure that "magick.exe" is still pathable. 


}else{
############test mode. Plots one gif ind map and one multiplot. 
  
test_folder <- paste(topfolder,"/","aesthetic_test",sep="")
dir.create(test_folder)


 #gif ind map test. 
pdata_sp <- pdata[pdata$Species=="Gunshot",]
pdata_sp_y <- pdata_sp[pdata_sp$Year=="2011",]
pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==pdata_sp_y$Month[1],]

dummy_row <- data.frame(as.character(pdata_sp_y_m[1,1])," ",scale_symbol_position_ind[2],scale_symbol_position_ind[1],pdata_sp_y_m[1,5],pdata_sp_y_m[1,6],pdata_sp_y_m[1,7],no_presence_symbol_size,0,low_effort_threshold+1,"yes")
colnames(dummy_row)<- colnames(pdata_sp_y_m)
pdata_sp_y_m <- rbind(pdata_sp_y_m,dummy_row)
  finalfilename <- paste(substr("Gunshot",1,3),"_",substr(monthvec[pdata_sp$Month[1]],1,3),substr(as.character(2011),3,4),sep="")
  png(paste(test_folder,"/",finalfilename,".png",sep=""),width=(image_width*1.054054),height=(image_height*1.02266),units="in",res=200) 
  print(plot_xyax_wyear(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_gif,month_and_year_label_position))
  dev.off()
  
  #One slide test
pdata_sp <- pdata[pdata$Species=="Gunshot",]
#4 month year, 11 month year, 9 month year
pdata<- pdata_sp[pdata_sp$Year=="2010"|pdata_sp$Year=="2011"|pdata_sp$Year=="2015",]  


for(p in unique(pdata$Species)){
  pdata_sp <- pdata[pdata$Species==p,]
  sp_folder <- paste(test_folder,"/",p,sep="")
  dir.create(sp_folder)
  for(x in unique(pdata_sp$Year)){
    pdata_sp_y<- pdata_sp[pdata_sp$Year==x,]
    months_in_y <- unique(pdata_sp_y$Month)
    months_in_y <- sort(months_in_y)
    month_span <- seq(months_in_y[1],tail(months_in_y,1))
    year_plots <- paste(sp_folder,"/",x,"_plots",sep="")
    dir.create(year_plots)
    if(length(month_span)<=4){
      pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[1],]
      dummy_row <- data.frame(as.character(pdata_sp_y_m[1,1])," ",scale_symbol_position[2],scale_symbol_position[1],pdata_sp_y_m[1,5],pdata_sp_y_m[1,6],pdata_sp_y_m[1,7],no_presence_symbol_size,0,low_effort_threshold+1,"yes")
      colnames(dummy_row)<- colnames(pdata_sp_y_m)
      pdata_sp_y_m <- rbind(pdata_sp_y_m,dummy_row)
      png(paste(year_plots,"/",monthvec[month_span[1]],"_xy",".png",sep=""),width=(image_width*image_correction_width),height=(image_height*image_correction_height),units="in",res=300) 
      print(plot_xyax(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
      dev.off()
      if(month_span[2] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[2],] 
        png(paste(year_plots,"/",monthvec[month_span[2]],"_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=300) 
        print(plot_xax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
      }
      if(month_span[3] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[3],] 
        png(paste(year_plots,"/",monthvec[month_span[3]],"_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=300) 
        print(plot_xax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
      }
      if(month_span[4] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[4],] 
        png(paste(year_plots,"/",monthvec[month_span[4]],"_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=300) 
        print(plot_xax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
      }
      png(paste(sp_folder,"/Slide",substr(as.character(x),3,4),".png",sep=""),width=((image_width*image_correction_width)+image_width*3),height=((image_height*image_correction_height)+(image_height*2)),units="in",res=150)
      par(mai=c(0,0,0,0),xpd=NA, oma=c(0,0,0,0))
      layout(matrix(1:12, ncol=4, byrow=TRUE),widths=c((image_width*image_correction_width),image_width,image_width,image_width),heights=c((image_height*image_correction_height),image_height,image_height))
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[1]],"_xy",".png",sep="")),0,0,1,1)
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")  
      if(month_span[2] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[2]],"_x",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_x.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")  
      if(month_span[3] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[3]],"_x",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_x.png",sep="")),0,0,1,1) 
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")  
      if(month_span[4] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[4]],"_x",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_x.png",sep="")),0,0,1,1)
      }
      for(a in 5:12){
        plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
        rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)  
      }
    }
    if(4<length(month_span) & length(month_span)<=8){
      pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[1],]
      dummy_row <- data.frame(as.character(pdata_sp_y_m[1,1])," ",scale_symbol_position[2],scale_symbol_position[1],pdata_sp_y_m[1,5],pdata_sp_y_m[1,6],pdata_sp_y_m[1,7],no_presence_symbol_size,0,low_effort_threshold+1,"yes")
      colnames(dummy_row)<- colnames(pdata_sp_y_m)
      pdata_sp_y_m <- rbind(pdata_sp_y_m,dummy_row)
      png(paste(year_plots,"/",monthvec[month_span[1]],"_y",".png",sep=""),width=(image_width*image_correction_width),height=(image_height),units="in",res=300) 
      print(plot_yax(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
      dev.off()
      if(month_span[2] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[2],] 
        png(paste(year_plots,"/",monthvec[month_span[2]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
        print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
        dev.off()
      }
      if(month_span[3] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[3],] 
        png(paste(year_plots,"/",monthvec[month_span[3]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
        print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
        dev.off()
      }
      if(month_span[4] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[4],]
        png(paste(year_plots,"/",monthvec[month_span[4]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
        print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
        dev.off()
      }
      if(month_span[5] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[5],] 
        png(paste(year_plots,"/",monthvec[month_span[5]],"_xy",".png",sep=""),width=(image_width*image_correction_width),height=(image_height*image_correction_height),units="in",res=300) 
        print(plot_xyax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
      }
      if(month_span[6] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[6],] 
        png(paste(year_plots,"/",monthvec[month_span[6]],"_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=300) 
        print(plot_xax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
      }
      if(month_span[7] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[7],] 
        png(paste(year_plots,"/",monthvec[month_span[7]],"_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=300) 
        print(plot_xax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
      }
      if(month_span[8] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[8],] 
        png(paste(year_plots,"/",monthvec[month_span[8]],"_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=300) 
        print(plot_xax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
      }
      png(paste(sp_folder,"/Slide",substr(as.character(x),3,4),".png",sep=""),width=((image_width*image_correction_width)+image_width*3),height=((image_height*image_correction_height)+(image_height*2)),units="in",res=150)
      par(mai=c(0,0,0,0),xpd=NA, oma=c(0,0,0,0))
      layout(matrix(1:12, ncol=4, byrow=TRUE),widths=c((image_width*image_correction_width),image_width,image_width,image_width),heights=c(image_height,(image_height*image_correction_height),image_height))
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[1]],"_y",".png",sep="")),0,0,1,1)
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[2] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[2]],"_n",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[3] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[3]],"_n",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[4] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[4]],"_n",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[5] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[5]],"_xy",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_xy.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")  
      if(month_span[6] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[6]],"_x",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_x.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")  
      if(month_span[7] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[7]],"_x",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_x.png",sep="")),0,0,1,1)  
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[8] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[8]],"_x",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_x.png",sep="")),0,0,1,1)  
      }
      for(b in 9:12){
        plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
        rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)  
      }
    }
    if(8<length(month_span) & length(month_span)<=12){
      pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[1],]
      dummy_row <- data.frame(as.character(pdata_sp_y_m[1,1])," ",scale_symbol_position[2],scale_symbol_position[1],pdata_sp_y_m[1,5],pdata_sp_y_m[1,6],pdata_sp_y_m[1,7],no_presence_symbol_size,0,low_effort_threshold+1,"yes")
      colnames(dummy_row)<- colnames(pdata_sp_y_m)
      pdata_sp_y_m <- rbind(pdata_sp_y_m,dummy_row)
      png(paste(year_plots,"/",monthvec[month_span[1]],"_y",".png",sep=""),width=(image_width*image_correction_width),height=(image_height),units="in",res=300) 
      print(plot_yax(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
      dev.off()
      if(month_span[2] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[2],] 
        png(paste(year_plots,"/",monthvec[month_span[2]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
        print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
        dev.off()
      }
      if(month_span[3] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[3],] 
        png(paste(year_plots,"/",monthvec[month_span[3]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
        print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
        dev.off()
      }
      if(month_span[4] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[4],]
        png(paste(year_plots,"/",monthvec[month_span[4]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
        print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
        dev.off()
      }
      if(month_span[5] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[5],] 
        png(paste(year_plots,"/",monthvec[month_span[5]],"_y",".png",sep=""),width=(image_width*image_correction_width),height=(image_height),units="in",res=300) 
        print(plot_yax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
      }
      if(month_span[6] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[6],] 
        png(paste(year_plots,"/",monthvec[month_span[6]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
        print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
        dev.off()
      }
      if(month_span[7] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[7],] 
        png(paste(year_plots,"/",monthvec[month_span[7]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
        print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
        dev.off()
      }
      if(month_span[8] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[8],] 
        png(paste(year_plots,"/",monthvec[month_span[8]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
        print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
        dev.off()
      }
      if(month_span[9] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[9],] 
        png(paste(year_plots,"/",monthvec[month_span[9]],"_xy",".png",sep=""),width=(image_width*image_correction_width),height=(image_height*image_correction_height),units="in",res=300) 
        print(plot_xyax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
      }
      if(month_span[10] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[10],] 
        png(paste(year_plots,"/",monthvec[month_span[10]],"_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=300) 
        print(plot_xax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
      }
      if(month_span[11] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[11],] 
        png(paste(year_plots,"/",monthvec[month_span[11]],"_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=300) 
        print(plot_xax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
      }
      if(month_span[12] %in% months_in_y){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[12],] 
        png(paste(year_plots,"/",monthvec[month_span[12]],"_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=300) 
        print(plot_xax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
        dev.off()
      }
      png(paste(sp_folder,"/Slide",substr(as.character(x),3,4),".png",sep=""),width=((image_width*image_correction_width)+image_width*3),height=((image_height*image_correction_height)+(image_height*2)),units="in",res=150)
      par(mai=c(0,0,0,0),xpd=NA, oma=c(0,0,0,0))
      layout(matrix(1:12, ncol=4, byrow=TRUE),widths=c((image_width*image_correction_width),image_width,image_width,image_width),heights=c(image_height,image_height,(image_height*image_correction_height)))
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[1]],"_y",".png",sep="")),0,0,1,1)
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[2] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[2]],"_n",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[3] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[3]],"_n",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[4] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[4]],"_n",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[5] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[5]],"_y",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_y.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")  
      if(month_span[6] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[6]],"_n",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")  
      if(month_span[7] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[7]],"_n",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)  
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[8] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[8]],"_n",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_n.png",sep="")),0,0,1,1)  
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[9] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[9]],"_xy",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_xy.png",sep="")),0,0,1,1)  
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n") 
      if(month_span[10] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[10]],"_x",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_x.png",sep="")),0,0,1,1)
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n") 
      if(month_span[11] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[11]],"_x",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_x.png",sep="")),0,0,1,1) 
      }
      plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      if(month_span[12] %in% months_in_y){
        rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[12]],"_x",".png",sep="")),0,0,1,1)
      }else{
        rasterImage(readPNG(paste(blankfolder,"/blank_x.png",sep="")),0,0,1,1)  
      }
    }
    dev.off()
  }
  
}





}
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

######################in progress###########

#setwd("//AKC0SS-N086/NMML_Users/daniel.woodrich/My Document")

workdir <- "//nmfs/akc-nmml/CAEP/Acoustics/ArcMAP/Mapping with R/Scripts/Accesory Code/gifsicle-master/src"
setwd(workdir)
system("nmake -f Makefile.bcc")
system("gifsicle -O1 Bowhead.gif -o Bowhead_optimized.gif")
setwd("//nmfs/akc-nmml/CAEP/Acoustics/ArcMAP/Mapping with R/Output files/CHAOZ-X map run test 2/Gifs and stand alone slides/Bowhead")

shell("gifsicle -O1 Bowhead.gif -o Bowhead-optimized.gif","\\nmfs\\akc-nmml\\CAEP\\Acoustics\\ArcMAP\\Mapping with R\\Scripts\\Accesory Code\\gifsicle-1.88-win64.zip\\gifsicle-1.88-win64\\gifsicle.exe")
system("\\nmfs\\akc-nmml\\CAEP\\Acoustics\\ArcMAP\\Mapping with R\\Scripts\\Accesory Code\\gifsicle-1.88-win64.zip\\gifsicle-1.88-win64\\gifsicle.exe gifsicle -O1 Bowhead.gif -o Bowhead_optimized.gif")
