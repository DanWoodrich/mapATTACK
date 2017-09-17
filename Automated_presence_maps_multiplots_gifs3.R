start.time <- Sys.time()
#Daniel Woodrich
#June 2017
#AFSC
#Monthly presence map generator
#daniel.woodrich@noaa.gov
#Create multiplots and gifs that summarize presence. Automated: should be able to handle varying species, years, and gaps in data, over different regions. Make sure dataset is formatted a la AWCHX_LTMonthlymapR.csv in ~/input_files. Make sure variables are pointing to the layers you want, and layers you do not want are removed from the code. 

dir.create("C:/R_map_status")
setwd("C:/R_map_status")
if(!file.exists("map_run_status.txt")){
#install mapping packages. Must be done once when running on a new computer and can then be commented out. 
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
install.packages("animation")
install.packages("grid")

fileConn<-file("map_run_status.txt")
writeLines(c("I have run the map script at least once, meaning that the necessary r packages are installed on my local machine."), fileConn)
close(fileConn)
}

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

points2polygons <- function(df,data) {
  get.grpPoly <- function(group,ID,df) {
    Polygon(coordinates(df[df$id==ID & df$group==group,]))
  }
  get.spPoly  <- function(ID,df) {
    Polygons(lapply(unique(df[df$id==ID,]$group),get.grpPoly,ID,df),ID)
  }
  spPolygons  <- SpatialPolygons(lapply(unique(df$id),get.spPoly,df))
  SpatialPolygonsDataFrame(spPolygons,match.ID=T,data=data)
}

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
    geom_vline(xintercept=seq(longlow,Boundaries[2],longbreak),color=alpha('black',gridlines_transparency))+
    #can use to pintpoint smymbol position relative to circles
    #geom_vline(xintercept=scale_symbol_position[1],color=alpha('black',gridlines_transparency))+
    geom_hline(yintercept=seq(latlow,Boundaries[4],latbreak),color=alpha('black',gridlines_transparency))+
    scale_fill_gradientn(colours=raster_color_palette,guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak,position="top") +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    geom_point(data=pdata_sp_y_m, aes(x=Long, y=Lat, size = PercDayWcalls,shape=PercDWCis0,color=PercDWCis0),stroke=1.5,fill="dodgerblue2")+
    scale_radius(name=" ",limits=c(1,100),range=c(legend_size_modifier_sli,legend_size_modifier_sli*10),guide = guide_legend(override.aes = list(shape=21,colour = "black",fill="dodgerblue2")))+
    scale_color_manual(name="Symbols",values = c('yes' = "black", 'no' = "black"),guide=FALSE)+
    scale_shape_manual(name="Symbols ",values = c('no' = 21,'yes' = no_presence_symbol),guide=FALSE)+
    geom_text(data=pdata_sp_y_m,aes(x=(Long+((0.5/5*legend_size_modifier_sli))+(PercDayWcalls/130/5*legend_size_modifier_sli)), y=(Lat+(0.04+PercDayWcalls/260/5*legend_size_modifier_sli)),label=ifelse(X.DaysWrecs<low_effort_threshold,"*",'')),size=15) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position = c(legend_position[1],legend_position[2]),
          legend.key=element_rect(colour=alpha('gray50',0.0),fill=alpha('gray50',0.0),size=0.5),
          legend.background = element_rect(colour = alpha('gray50',0.0), fill = alpha('gray50',0.0)),
          legend.text=element_text(size=legend_text_size,color="white",face="bold"),
          legend.key.size = unit(0, 'lines'),
          legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"),
          axis.text=element_text(size=axis_text_size_slides),
          plot.margin=unit(c(0,0,0,0),"mm"))+
    annotate(geom="text",zero_value_scale_position[1],zero_value_scale_position[2],label="0",color="white",size=(legend_text_size*0.375),fontface=2)+
    annotate(geom="text",month_label_position[1],month_label_position[2],label=paste(monthvec[pdata_sp_y_m[1,6]]),color="white",size=month_label_size,fontface=2)


}

plot_xax_nl <- function(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    geom_polygon(data=AKCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=RusCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=CHXarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("red",0.75), size=0.75) +
    geom_polygon(data=AWarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("yellow",0.75), size=0.75) +
    geom_vline(xintercept=seq(longlow,Boundaries[2],longbreak),color=alpha('black',gridlines_transparency))+
    geom_hline(yintercept=seq(latlow,Boundaries[4],latbreak),color=alpha('black',gridlines_transparency))+
    scale_fill_gradientn(colours=raster_color_palette,guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak,position="top") +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    geom_point(data=pdata_sp_y_m, aes(x=Long, y=Lat, size = PercDayWcalls,shape=PercDWCis0,color=PercDWCis0),stroke=1.5,fill="dodgerblue2")+
    scale_radius(name=" ",limits=c(1,100),range=c(legend_size_modifier_sli,legend_size_modifier_sli*10),guide = guide_legend(override.aes = list(shape=21,colour = "black",fill="dodgerblue2")))+
    scale_color_manual(name="Symbols",values = c('yes' = "black", 'no' = "black"),guide=FALSE)+
    scale_shape_manual(name="Symbols ",values = c('no' = 21,'yes' = no_presence_symbol),guide=FALSE)+
    geom_text(data=pdata_sp_y_m,aes(x=(Long+((0.5/5*legend_size_modifier_sli))+(PercDayWcalls/130/5*legend_size_modifier_sli)), y=(Lat+(0.04+PercDayWcalls/260/5*legend_size_modifier_sli)),label=ifelse(X.DaysWrecs<low_effort_threshold,"*",'')),size=15) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position = "none",
          axis.text.x=element_text(size=axis_text_size_slides),
          plot.margin=unit(c(0,0,0,0),"mm"))+
    annotate(geom="text",month_label_position[1],month_label_position[2],label=paste(monthvec[pdata_sp_y_m[1,6]]),color="white",size=month_label_size,fontface=2)

}

plot_yax_nl <- function(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    geom_polygon(data=AKCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=RusCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=CHXarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("red",0.75), size=0.75) +
    geom_polygon(data=AWarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("yellow",0.75), size=0.75) +
    geom_vline(xintercept=seq(longlow,Boundaries[2],longbreak),color=alpha('black',gridlines_transparency))+
    geom_hline(yintercept=seq(latlow,Boundaries[4],latbreak),color=alpha('black',gridlines_transparency))+
    scale_fill_gradientn(colours=raster_color_palette,guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak,position="top") +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    geom_point(data=pdata_sp_y_m, aes(x=Long, y=Lat, size = PercDayWcalls,shape=PercDWCis0,color=PercDWCis0),stroke=1.5,fill="dodgerblue2")+
    scale_radius(name=" ",limits=c(1,100),range=c(legend_size_modifier_sli,legend_size_modifier_sli*10),guide = guide_legend(override.aes = list(shape=21,colour = "black",fill="dodgerblue2")))+
    scale_color_manual(name="Symbols",values = c('yes' = "black", 'no' = "black"),guide=FALSE)+
    scale_shape_manual(name="Symbols ",values = c('no' = 21,'yes' = no_presence_symbol),guide=FALSE)+
    geom_text(data=pdata_sp_y_m,aes(x=(Long+((0.5/5*legend_size_modifier_sli))+(PercDayWcalls/130/5*legend_size_modifier_sli)), y=(Lat+(0.04+PercDayWcalls/260/5*legend_size_modifier_sli)),label=ifelse(X.DaysWrecs<low_effort_threshold,"*",'')),size=15) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position = "none",
          axis.text.y=element_text(size=axis_text_size_slides),
          plot.margin=unit(c(0,0,0,0),"mm"))+
    annotate(geom="text",month_label_position[1],month_label_position[2],label=paste(monthvec[pdata_sp_y_m[1,6]]),color="white",size=month_label_size,fontface=2)

}
plot_nax_nl <- function(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    geom_polygon(data=AKCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=RusCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=CHXarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("red",0.75), size=0.75) +
    geom_polygon(data=AWarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("yellow",0.75), size=0.75) +
    geom_vline(xintercept=seq(longlow,Boundaries[2],longbreak),color=alpha('black',gridlines_transparency))+
    geom_hline(yintercept=seq(latlow,Boundaries[4],latbreak),color=alpha('black',gridlines_transparency))+
    scale_fill_gradientn(colours=raster_color_palette,guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak,position="top") +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    geom_point(data=pdata_sp_y_m, aes(x=Long, y=Lat, size = PercDayWcalls,shape=PercDWCis0,color=PercDWCis0),stroke=1.5,fill="dodgerblue2")+
    scale_radius(name=" ",limits=c(1,100),range=c(legend_size_modifier_sli,legend_size_modifier_sli*10),guide = guide_legend(override.aes = list(shape=21,colour = "black",fill="dodgerblue2")))+
    scale_color_manual(name="Symbols",values = c('yes' = "black", 'no' = "black"),guide=FALSE)+
    scale_shape_manual(name="Symbols ",values = c('no' = 21,'yes' = no_presence_symbol),guide=FALSE)+
    geom_text(data=pdata_sp_y_m,aes(x=(Long+((0.5/5*legend_size_modifier_sli))+(PercDayWcalls/130/5*legend_size_modifier_sli)), y=(Lat+(0.04+PercDayWcalls/260/5*legend_size_modifier_sli)),label=ifelse(X.DaysWrecs<low_effort_threshold,"*",'')),size=15) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          legend.position = "none",
          plot.margin=unit(c(0,0,0,0),"mm"))+
    annotate(geom="text",month_label_position[1],month_label_position[2],label=paste(monthvec[pdata_sp_y_m[1,6]]),color="white",size=month_label_size,fontface=2)

}

plot_blankx <- function(t,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    geom_polygon(data=AKCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=RusCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=CHXarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("red",0.75), size=0.75) +
    geom_polygon(data=AWarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("yellow",0.75), size=0.75) +
    geom_vline(xintercept=seq(longlow,Boundaries[2],longbreak),color=alpha('black',gridlines_transparency))+
    geom_hline(yintercept=seq(latlow,Boundaries[4],latbreak),color=alpha('black',gridlines_transparency))+
    scale_fill_gradientn(colours=raster_color_palette,guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak,position="top") +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position = "none",
          axis.text=element_text(size=axis_text_size_slides),
          plot.margin=unit(c(0,0,0,0),"mm"))+
    annotate(geom="text",month_label_position[1],month_label_position[2],label=monthvec[t],color="white",size=month_label_size,fontface=2)
}

plot_blanky <- function(t,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    geom_polygon(data=AKCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=RusCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=CHXarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("red",0.75), size=0.75) +
    geom_polygon(data=AWarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("yellow",0.75), size=0.75) +
    geom_vline(xintercept=seq(longlow,Boundaries[2],longbreak),color=alpha('black',gridlines_transparency))+
    geom_hline(yintercept=seq(latlow,Boundaries[4],latbreak),color=alpha('black',gridlines_transparency))+
    scale_fill_gradientn(colours=raster_color_palette,guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak,position="top") +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position = "none",
          axis.text=element_text(size=axis_text_size_slides),
          plot.margin=unit(c(0,0,0,0),"mm"))+
    annotate(geom="text",month_label_position[1],month_label_position[2],label=monthvec[t],color="white",size=month_label_size,fontface=2)

}

plot_blankn <- function(t,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    geom_polygon(data=AKCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=RusCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=CHXarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("red",0.75), size=0.75) +
    geom_polygon(data=AWarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("yellow",0.75), size=0.75) +
    geom_vline(xintercept=seq(longlow,Boundaries[2],longbreak),color=alpha('black',gridlines_transparency))+
    geom_hline(yintercept=seq(latlow,Boundaries[4],latbreak),color=alpha('black',gridlines_transparency))+
    scale_fill_gradientn(colours=raster_color_palette,guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak,position="top") +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          legend.position = "none",
          plot.margin=unit(c(0,0,0,0),"mm"))+
    annotate(geom="text",month_label_position[1],month_label_position[2],label=monthvec[t],color="white",size=month_label_size,fontface=2)
}

plot_blankn_true <- function(pdata_sp_y_m,MAP_df,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
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

plot_xyax_wyear <- function(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_gif,month_and_year_label_position){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    geom_polygon(data=AKCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=RusCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=CHXarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("red",0.75), size=0.75) +
    geom_polygon(data=AWarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("yellow",0.75), size=0.75) +
    geom_vline(xintercept=seq(longlow,Boundaries[2],longbreak),color=alpha('black',gridlines_transparency))+
    geom_hline(yintercept=seq(latlow,Boundaries[4],latbreak),color=alpha('black',gridlines_transparency))+
    scale_fill_gradientn(colours=raster_color_palette,guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak) +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    geom_point(data=pdata_sp_y_m, aes(x=Long, y=Lat, size = PercDayWcalls,shape=PercDWCis0,color=PercDWCis0),stroke=1.5,fill="dodgerblue2")+
    scale_radius(name=" ",limits=c(1,100),range=c(legend_size_modifier_ind,legend_size_modifier_ind*10),guide = guide_legend(override.aes = list(shape=21,colour = "black",fill="dodgerblue2")))+
    scale_color_manual(name="Symbols",values = c('yes' = "black", 'no' = "black"),guide=FALSE)+
    scale_shape_manual(name="Symbols ",values = c('no' = 21,'yes' = no_presence_symbol),guide=FALSE)+
    geom_text(data=pdata_sp_y_m,aes(x=(Long+((0.5/5*legend_size_modifier_ind))+(PercDayWcalls/130/5*legend_size_modifier_ind)), y=(Lat+(0.04+PercDayWcalls/260/5*legend_size_modifier_ind)),label=ifelse(X.DaysWrecs<low_effort_threshold,"*",'')),size=15) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position = c(legend_position_ind[1],legend_position_ind[2]),
          legend.key=element_rect(colour=alpha('gray50',0.0),fill=alpha('gray50',0.0),size=0.5),
          legend.background = element_rect(colour = alpha('gray50',0.0), fill = alpha('gray50',0.0)),
          legend.text=element_text(size=legend_text_size_ind,color="white"),legend.key.size = unit(0, 'lines'),
          legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"),
          axis.text=element_text(size=axis_text_size_gif))+
    annotate(geom="text",zero_value_scale_position_ind[1],zero_value_scale_position_ind[2],label="0",color="white",size=(legend_text_size_ind*0.375))+
    annotate(geom="text",month_and_year_label_position[1],month_and_year_label_position[2],label=paste(monthvec[pdata_sp_y_m[1,6]],pdata_sp_y_m[1,5]),color="white",size=month_label_size_ind,fontface=2)

}

plot_xyax_mdy<- function(pdatad_sp_y_d,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_gif,month_and_year_label_position){
  ggplot()+ geom_tile(data=MAP_df,aes(x=x,y=y,fill=value))+
    geom_polygon(data=AKCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=RusCrop, aes(x=long, y=lat, group=group),fill="grey50", color="gray25", size=0.10) +
    geom_polygon(data=CHXarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("red",0.75), size=0.75) +
    geom_polygon(data=AWarea, aes(x=long, y=lat, group=group),fill=NA, color=alpha("yellow",0.75), size=0.75) +
    geom_vline(xintercept=seq(longlow,Boundaries[2],longbreak),color=alpha('black',gridlines_transparency))+
    geom_hline(yintercept=seq(latlow,Boundaries[4],latbreak),color=alpha('black',gridlines_transparency))+
    #geom_hline(zero_value_scale_position_ind[2],color=alpha('black',gridlines_transparency))+
    scale_fill_gradientn(colours=raster_color_palette,guide=FALSE) +
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak) +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    geom_point(data=pdatad_sp_y_d, aes(x=Long, y=Lat, size = PercBinsWcalls,shape=PercDWCis0,color=PercDWCis0),stroke=1.25,fill="dodgerblue2")+
    scale_radius(name=" ",limits=c(0.01,100),range=c(legend_size_modifier_ind,legend_size_modifier_ind*10),guide = guide_legend(override.aes = list(shape=21,colour = "black",fill="dodgerblue2")))+
    scale_color_manual(name="Symbols",values = c('yes' = "black", 'no' = "black",'point' = "black"),guide=FALSE)+
    scale_shape_manual(name="Symbols ",values = c('no' = 21,'yes' = no_presence_symbol, 'point'= 16),guide=FALSE)+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position = c(legend_position_ind[1],legend_position_ind[2]),
          legend.key=element_rect(colour=alpha('gray50',0.0),fill=alpha('gray50',0.0),size=0.5),
          legend.background = element_rect(colour = alpha('gray50',0.0), fill = alpha('gray50',0.0)),
          legend.text=element_text(size=legend_text_size_ind,color="white"),
          legend.key.size = unit(0, 'lines'),
          legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"),
          axis.text=element_text(size=axis_text_size_gif))+
    annotate(geom="text",zero_value_scale_position_wgif[1],zero_value_scale_position_wgif[2],label="0",color="white",size=(legend_text_size_ind*0.375))+
    annotate(geom="text",lessthantwo_text_pos[1],lessthantwo_text_pos[2],label="<2",color="white",size=(legend_text_size_ind*0.375))+
    annotate(geom="text",month_and_year_label_position[1],month_and_year_label_position[2],label=as.character(pdatad_sp_y_d[1,7]),color="white",size=month_label_size_ind,fontface=2)
    #can use to pintpoint smymbol position relative to circles

}


#OPTIONAL USER INPUTS. Defaults specified. 
run_name <- "dan shapefile"                             #name your run. Unique names will generate a new folder with this name in \\nmfs\akc-nmml\CAEP\Acoustics\ArcMAP\Mapping with R\Output files\. Duplicate names will overwrite that folder. trailing space in string will crash code.
test_mode <- "y"                                                    #"y" or "n". subsets the data to first two years and first two species. Set to y to give a sample of all figure, recommend doing this before fully plotting.
run_type <- "both"                                             #"slides","gifs", or "both" 
gif_interval <- "month"                                          #"week" or "month" or "both"
change_gif_speed <- "n"                                           #"y" or "n". update gif speeds for current run based on parameters in below block. much faster than replotting. "y" on this option skips all plotting sections. 


num_lat_ticks <- 4
num_long_ticks <- 4
low_effort_threshold <- 15                                           #threshold below which an asterisk is placed on the dots. 
raster_color_palette <- c("white",lighten("skyblue1"),darken("skyblue1"),"steelblue4","steelblue4",darken("steelblue4")) 
legend_position <- c(.78,.325)                                       #scaled from 0-1, easiest to just enter test values here to reposition. 
legend_position_ind <- c(.875,.22) 
month_label_position <- c(-154.3,65.85)                                   #for grid and individual plot month labels, lat/long
month_and_year_label_position <- c(-155.2,65.7)         #for slide month and year labels, lat/long
no_presence_symbol <- 3                                              #see http://www.statmethods.net/advgraphs/parameters.html for more options
no_presence_symbol_size <- 6
lessthantwo_symbol_size <- 2
legend_text_size <- 40
legend_text_size_ind <- 30
legend_size_modifier_sli <- 5                                         #bigger # = bigger legend and dots. for the slides
legend_size_modifier_ind <- 3                                         #bigger # = bigger legend and dots. for the gifs 
gridlines_transparency <- 0.2                                        #0-1, 0 being fully transparent
month_label_size <- 24
month_label_size_ind <- 17
axis_text_size_slides <- 40                                         #text size for axes, multiplot
axis_text_size_gif <- 12                                            #text size for gif and single slides. 
image_height <- 11.0558                                             #height of output image (just the map region). Default value (11.0558) determined from arcGIS map window dimensions of  CHAOZ-X project extent.
image_width <-  9.3902                                              #width of output image (just the map region). Default value (9.3902) determined from arcGIS map window dimensions of CHAOZ-X project extent.
image_correction_height <- ((0.00129*axis_text_size_slides)+1.007246) #correction which changes plot size so that map regions are the same w & w/o axes. Eventually, find math relationship and base it on axis text size. Default 1.02266 for CHAOZ-X dimensions and size 12 text. 
image_correction_width <- ((0.0032*axis_text_size_slides)+1.007246)   #correction which changes plot size so that map regions are the same w & w/o axes. Eventually, find math relationship and base it on axis text size. Default 1.05405405 for CHAOZ-X dimensions and size 12 text.                  
gifspeed_month <- 90                                                     #larger numbers are slower, unsure of units. CHAOZ-X default = 160.
framerate_month <- 1                                                  #fps
gifspeed_week <- 60                                                  #larger numbers are slower, unsure of units. CHAOZ-X default = 160.
framerate_week <- 2                                                   #fps
gif_month_res <- 100
gif_week_res <- 60
slide_res <- 64
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
pdata <- read.csv(paste(input_folder,"/","Data frame","/","AWCHX_LTMonthlymapRnew.csv",sep=""))
pdatad <- read.csv(paste(input_folder,"/","Data frame","/","AWCHX_LTWeeklymapRNEW.csv",sep=""))

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
benhot <- spTransform(benhot,"+proj=longlat")
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

#calculate plot location variables in terms of boundaries
#for slides, in terms of Boundaries, legend_position, and legend_size_modifier_sli
scale_symbol_position <- c(((Boundaries[2]-Boundaries[1])*legend_position[1])+Boundaries[1]+(-0.0912*(legend_size_modifier_sli)-0.5025),((Boundaries[4]-Boundaries[3])*legend_position[2])+Boundaries[3]+(0.38925*(legend_size_modifier_sli)+0.074))                  
zero_value_scale_position <- c(scale_symbol_position[1]+(0.426*(legend_size_modifier_sli)+0.285),scale_symbol_position[2])

#for month gifs, in terms of Boundaries, legend_position, and legend_size_modifier_sli
scale_symbol_position_ind <- c(((Boundaries[2]-Boundaries[1])*legend_position_ind[1])+Boundaries[1]+(-0.0912*(legend_size_modifier_ind)-0.5025),((Boundaries[4]-Boundaries[3])*legend_position_ind[2])+Boundaries[3]+(0.38925*(legend_size_modifier_ind)+0.074))     
zero_value_scale_position_ind <- c(scale_symbol_position_ind[1]+(0.426*(legend_size_modifier_ind)+0.285),scale_symbol_position_ind[2])

#for week gifs (near same pos as month gif 0 line)
scale_symbol_lessthantwo <- scale_symbol_position_ind
lessthantwo_text_pos <- c(zero_value_scale_position_ind[1]+0.2,zero_value_scale_position_ind[2])  

#for week gifs (raised)
scale_symbol_position_wgif <- c(scale_symbol_position_ind[1],scale_symbol_position_ind[2]+0.3)                            
zero_value_scale_position_wgif <- c(zero_value_scale_position_ind[1],zero_value_scale_position_ind[2]+0.3)

#Crop shapefiles down to match raster
AKCrop <- crop(AKCont_longlat,extent(Boundaries))
RusCrop <- crop(RusCont_longlat,extent(Boundaries))


#################Presence data processing######################
monthvec <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
pdata$PercDWCis0 <- ifelse(pdata$PercDayWcalls==0,"yes","no")
pdatad$PercDWCis0 <- ifelse(pdatad$PercBinsWcalls==0,"yes","no")


#remove species with no calling. Change to remove based on calling max() == 0
for(r in unique(pdata$Species)){
  if(max(pdata[pdata$Species==r,]$PercDayWcalls)==0){
    pdata <- pdata[pdata$Species!=r,]
  }else{
    pdata<- pdata
  }
}

for(q in unique(pdatad$Species)){
  if(max(pdatad[pdatad$Species==q,]$PercBinsWcalls)==0){
    pdatad <- pdatad[pdatad$Species!=q,]
  }else{
    pdatad <- pdatad
  }
}

#remove these species (not actually 0 calls but functionally, by C request.)

pdata <- pdata[pdata$Species!="Right"&pdata$Species!="Minke"&pdata$Species!="Boing"&pdata$Species!="Sperm",]
pdatad <- pdatad[pdatad$Species!="Right"&pdatad$Species!="Minke"&pdatad$Species!="Boing"&pdatad$Species!="Sperm",]

pdata$PercDayWcalls[pdata$PercDayWcalls==0] <- no_presence_symbol_size
pdatad$PercBinsWcalls[pdatad$PercBinsWcalls==0] <- no_presence_symbol_size
pdatad[pdatad$PercBinsWcalls<2,11] <- "point"   
pdatad[pdatad$PercBinsWcalls<2,8] <-lessthantwo_symbol_size   


#removes rows outside of raster extent
pdata <- pdata[!(pdata$Lat<=Boundaries[3]|pdata$Lat>=Boundaries[4]|pdata$Long<=Boundaries[1]|pdata$Long>=Boundaries[2]),]
pdatad <- pdatad[!(pdatad$Lat<=Boundaries[3]|pdatad$Lat>=Boundaries[4]|pdatad$Long<=Boundaries[1]|pdatad$Long>=Boundaries[2]),]

#change data frame dates to r date objects. May need to tweak this depending on format of inc dataset. 
pdata$Dateformat <- as.Date(as.character(pdata$Date),format="%m/%d/%Y")
pdatad$Dateformat <- as.Date(as.character(pdatad$Date),format="%m/%d/%y")


if(test_mode=="y"){
  
  pdata<- pdata[pdata$Species==unique(pdata$Species)[1]|pdata$Species==unique(pdata$Species)[2],]
  pdata<- pdata[pdata$Year==unique(pdata$Year)[1]|pdata$Year==unique(pdata$Year)[2],]
  
  pdatad<- pdatad[pdatad$Species==unique(pdatad$Species)[1]|pdatad$Species==unique(pdatad$Species)[2],]
  pdatad<- pdatad[pdatad$Year==unique(pdatad$Year)[1]|pdatad$Year==unique(pdatad$Year)[2],]
  
  topfolder <- paste(output_folder,"/",run_name," test",sep="")
  dir.create(topfolder) 
  
}else{

topfolder <- paste(output_folder,"/",run_name,sep="")
dir.create(topfolder)
}

blankfolder <- paste(topfolder,"/","Blanks",sep="")
dir.create(blankfolder)

pdata_sp <- pdata[pdata$Species==unique(pdata$Species)[1],]
pdata_sp_y <- pdata_sp[pdata$Year==unique(pdata$Year)[1],]
pdata_sp_m <- pdata_sp[pdata$Month==unique(pdata$Month)[1],]

png(paste(blankfolder,"/blank_n_true.png",sep=""),width=(image_width),height=(image_height),units="in",res=slide_res) 
print(plot_blankn_true(x,MAP_df,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec))
dev.off()



if(change_gif_speed !="y"){
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
    pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[1],]
    dummy_row <- data.frame(as.character(pdata_sp_y_m[1,1])," ",scale_symbol_position[2],scale_symbol_position[1],pdata_sp_y_m[1,5],pdata_sp_y_m[1,6],pdata_sp_y_m[1,7],no_presence_symbol_size,0,low_effort_threshold+1,"yes",pdata_sp_y_m$Dateformat[1])
    colnames(dummy_row)<- colnames(pdata_sp_y_m)
    pdata_sp_y_m <- rbind(pdata_sp_y_m,dummy_row)
    png(paste(year_plots,"/",monthvec[month_span[1]],"_xy",".png",sep=""),width=(image_width*image_correction_width),height=(image_height*image_correction_height),units="in",res=slide_res) 
    print(plot_xyax(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
    dev.off()
    if(month_span[2] %in% months_in_y){
      pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[2],] 
      png(paste(year_plots,"/",monthvec[month_span[2]],"_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=slide_res) 
      print(plot_xax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
      dev.off()
    }else if(!is.na(month_span[2])){
      png(paste(year_plots,"/",monthvec[month_span[2]],"blank_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=slide_res) 
      t <- month_span[2]
      print(plot_blankx(t,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
      dev.off()
    }
    if(month_span[3] %in% months_in_y){
      pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[3],] 
      png(paste(year_plots,"/",monthvec[month_span[3]],"_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=slide_res) 
      print(plot_xax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
      dev.off()
    }else if(!is.na(month_span[3])){
      png(paste(year_plots,"/",monthvec[month_span[3]],"blank_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=slide_res) 
      t <- month_span[3]
      print(plot_blankx(t,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
      dev.off()
    }
    if(month_span[4] %in% months_in_y){
      pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[4],]
      png(paste(year_plots,"/",monthvec[month_span[4]],"_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=slide_res) 
      print(plot_xax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
      dev.off()
    }else if(!is.na(month_span[4])){
      png(paste(year_plots,"/",monthvec[month_span[4]],"blank_x",".png",sep=""),width=(image_width),height=(image_height*image_correction_height),units="in",res=slide_res) 
      t <- month_span[4]
      print(plot_blankx(t,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
      dev.off()
    }
    if(month_span[5] %in% months_in_y){
      pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[5],] 
      png(paste(year_plots,"/",monthvec[month_span[5]],"_y",".png",sep=""),width=(image_width*image_correction_width),height=(image_height),units="in",res=slide_res) 
      print(plot_yax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
      dev.off()
    }else if(!is.na(month_span[5])){
      png(paste(year_plots,"/",monthvec[month_span[5]],"blank_y",".png",sep=""),width=(image_width),height=(image_height),units="in",res=slide_res) 
      t <- month_span[5]
      print(plot_blanky(t,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
      dev.off()
    }
    if(month_span[6] %in% months_in_y){
      pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[6],] 
      png(paste(year_plots,"/",monthvec[month_span[6]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=slide_res) 
      print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
      dev.off()
    }else if(!is.na(month_span[6])){
      png(paste(year_plots,"/",monthvec[month_span[6]],"blank_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=slide_res) 
      t <- month_span[6]
      print(plot_blankn(t,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
      dev.off()
    }
    if(month_span[7] %in% months_in_y){
      pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[7],] 
      png(paste(year_plots,"/",monthvec[month_span[7]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=slide_res) 
      print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
      dev.off()
    }else if(!is.na(month_span[7])){
      png(paste(year_plots,"/",monthvec[month_span[7]],"blank_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=slide_res) 
      t <- month_span[7]
      print(plot_blankn(t,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
      dev.off()
    }
    if(month_span[8] %in% months_in_y){
      pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[8],] 
      png(paste(year_plots,"/",monthvec[month_span[8]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=slide_res) 
      print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
      dev.off()
    }else if(!is.na(month_span[8])){
      png(paste(year_plots,"/",monthvec[month_span[8]],"blank_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=slide_res) 
      t <- month_span[8]
      print(plot_blankn(t,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
      dev.off()
    }
    if(month_span[9] %in% months_in_y){
      pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[9],] 
      png(paste(year_plots,"/",monthvec[month_span[9]],"_y",".png",sep=""),width=(image_width*image_correction_width),height=(image_height),units="in",res=slide_res) 
      print(plot_yax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
      dev.off()
    }else if(!is.na(month_span[9])){
      png(paste(year_plots,"/",monthvec[month_span[9]],"blank_y",".png",sep=""),width=(image_width*image_correction_width),height=(image_height),units="in",res=slide_res) 
      t <- month_span[9]
      print(plot_blanky(t,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,axis_text_size_slides,month_label_position))
      dev.off()
    }
    if(month_span[10] %in% months_in_y){
      pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[10],] 
      png(paste(year_plots,"/",monthvec[month_span[10]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=slide_res) 
      print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
      dev.off()
    }else if(!is.na(month_span[10])){
      png(paste(year_plots,"/",monthvec[month_span[10]],"blank_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=slide_res) 
      t <- month_span[10]
      print(plot_blankn(t,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
      dev.off()
    }
    if(month_span[11] %in% months_in_y){
      pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[11],] 
      png(paste(year_plots,"/",monthvec[month_span[11]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=slide_res) 
      print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
      dev.off()
    }else if(!is.na(month_span[11])){
      png(paste(year_plots,"/",monthvec[month_span[11]],"blank_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=slide_res) 
      t <- month_span[11]
      print(plot_blankn(t,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
      dev.off()
    }
    if(month_span[12] %in% months_in_y){
      pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==month_span[12],] 
      png(paste(year_plots,"/",monthvec[month_span[12]],"_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=slide_res) 
      print(plot_nax_nl(pdata_sp_y_m,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
      dev.off()
    }else if(!is.na(month_span[12])){
      png(paste(year_plots,"/",monthvec[month_span[12]],"blank_n",".png",sep=""),width=(image_width),height=(image_height),units="in",res=slide_res) 
      t <- month_span[12]
      print(plot_blankn(t,MAP_df,AKCrop,RusCrop,CHXarea,AWarea,raster_color_palette,longlow,longhigh,longbreak,latlow,lathigh,latbreak,low_effort_threshold,legend_position,zero_value_scale_position,no_presence_symbol,legend_text_size,gridlines_transparency,month_label_size,Boundaries,monthvec,month_label_position))
      dev.off()
    }
    png(paste(sp_folder,"/",substr(p,1,3),as.character(x),".png",sep=""),width=((image_width*image_correction_width)+image_width*3),height=((image_height*image_correction_height)+(image_height*2)),units="in",res=(slide_res))
    par(mai=c(0.1,0.1,0.1,0.1), oma=c(0,0,0,0),xpd=NA)
    layout(matrix(1:12, ncol=4, byrow=TRUE),widths=c((image_width*image_correction_width),image_width,image_width,image_width),heights=c((image_height*image_correction_height),image_height,image_height))
    plot(NA,xlim=0:1,ylim=0:1,axes=0,bty="n",xaxs="i",yaxs="i",ylab="")
    rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[1]],"_xy",".png",sep="")),0,0,1,1)
    plot(NA,xlim=0:1,ylim=0:1,axes=0,bty="n",xaxs="i",yaxs="i",ylab="")
    if((month_span[2] %in% months_in_y) & !is.na(month_span[2])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[2]],"_x",".png",sep="")),0,0,1,1)
    }else if(!(month_span[2] %in% months_in_y) & !is.na(month_span[2])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[2]],"blank_x",".png",sep="")),0,0,1,1)
    }else{
      rasterImage(readPNG(paste(blankfolder,"/blank_n_true.png",sep="")),0,0,1,1)
    }
    plot(NA,xlim=0:1,ylim=0:1,axes=0,bty="n",xaxs="i",yaxs="i",ylab="")
    if((month_span[3] %in% months_in_y) & !is.na(month_span[3])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[3]],"_x",".png",sep="")),0,0,1,1)
    }else if(!(month_span[3] %in% months_in_y) & !is.na(month_span[3])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[3]],"blank_x",".png",sep="")),0,0,1,1)
    }else{
      rasterImage(readPNG(paste(blankfolder,"/blank_n_true.png",sep="")),0,0,1,1)
    }
    plot(NA,xlim=0:1,ylim=0:1,axes=0,bty="n",xaxs="i",yaxs="i",ylab="")
    if((month_span[4] %in% months_in_y) & !is.na(month_span[4])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[4]],"_x",".png",sep="")),0,0,1,1)
    }else if(!(month_span[4] %in% months_in_y) & !is.na(month_span[4])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[4]],"blank_x",".png",sep="")),0,0,1,1)
    }else{
      rasterImage(readPNG(paste(blankfolder,"/blank_n_true.png",sep="")),0,0,1,1)
    }
    plot(NA,xlim=0:1,ylim=0:1,axes=0,bty="n",xaxs="i",yaxs="i",ylab="")
    if((month_span[5] %in% months_in_y) & !is.na(month_span[5])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[5]],"_y",".png",sep="")),0,0,1,1)
    }else if(!(month_span[5] %in% months_in_y) & !is.na(month_span[5])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[5]],"blank_y",".png",sep="")),0,0,1,1)
    }else{
      rasterImage(readPNG(paste(blankfolder,"/blank_n_true.png",sep="")),0,0,1,1)
    }
    plot(NA,xlim=0:1,ylim=0:1,axes=0,bty="n",xaxs="i",yaxs="i",ylab="")  
    if((month_span[6] %in% months_in_y) & !is.na(month_span[6])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[6]],"_n",".png",sep="")),0,0,1,1)
    }else if(!(month_span[6] %in% months_in_y) & !is.na(month_span[6])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[6]],"blank_n",".png",sep="")),0,0,1,1)
    }else{
      rasterImage(readPNG(paste(blankfolder,"/blank_n_true.png",sep="")),0,0,1,1)
    }
    plot(NA,xlim=0:1,ylim=0:1,axes=0,bty="n",xaxs="i",yaxs="i",ylab="")  
    if((month_span[7] %in% months_in_y) & !is.na(month_span[7])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[7]],"_n",".png",sep="")),0,0,1,1)
    }else if(!(month_span[7] %in% months_in_y) & !is.na(month_span[7])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[7]],"blank_n",".png",sep="")),0,0,1,1)
    }else{
      rasterImage(readPNG(paste(blankfolder,"/blank_n_true.png",sep="")),0,0,1,1)
    }
    plot(NA,xlim=0:1,ylim=0:1,axes=0,bty="n",xaxs="i",yaxs="i",ylab="")
    if((month_span[8] %in% months_in_y) & !is.na(month_span[8])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[8]],"_n",".png",sep="")),0,0,1,1)
    }else if(!(month_span[8] %in% months_in_y) & !is.na(month_span[8])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[8]],"blank_n",".png",sep="")),0,0,1,1)
    }else{
      rasterImage(readPNG(paste(blankfolder,"/blank_n_true.png",sep="")),0,0,1,1)
    }
    plot(NA,xlim=0:1,ylim=0:1,axes=0,bty="n",xaxs="i",yaxs="i",ylab="")
    if((month_span[9] %in% months_in_y) & !is.na(month_span[9])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[9]],"_y",".png",sep="")),0,0,1,1)
    }else if(!(month_span[9] %in% months_in_y) & !is.na(month_span[9])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[9]],"blank_y",".png",sep="")),0,0,1,1)
    }else{
      rasterImage(readPNG(paste(blankfolder,"/blank_n_true.png",sep="")),0,0,1,1)
    }
    plot(NA,xlim=0:1,ylim=0:1,axes=0,bty="n",xaxs="i",yaxs="i",ylab="")
    if((month_span[10] %in% months_in_y) & !is.na(month_span[10])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[10]],"_n",".png",sep="")),0,0,1,1)
    }else if(!(month_span[10] %in% months_in_y) & !is.na(month_span[10])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[10]],"blank_n",".png",sep="")),0,0,1,1)
    }else{
      rasterImage(readPNG(paste(blankfolder,"/blank_n_true.png",sep="")),0,0,1,1)
    }
    plot(NA,xlim=0:1,ylim=0:1,axes=0,bty="n",xaxs="i",yaxs="i",ylab="")
    if((month_span[11] %in% months_in_y) & !is.na(month_span[11])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[11]],"_n",".png",sep="")),0,0,1,1)
    }else if(!(month_span[11] %in% months_in_y) & !is.na(month_span[11])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[11]],"blank_n",".png",sep="")),0,0,1,1)
    }else{
      rasterImage(readPNG(paste(blankfolder,"/blank_n_true.png",sep="")),0,0,1,1)
    }
    plot(NA,xlim=0:1,ylim=0:1,axes=0,bty="n",xaxs="i",yaxs="i",ylab="")
    if((month_span[12] %in% months_in_y) & !is.na(month_span[12])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[12]],"_n",".png",sep="")),0,0,1,1)
    }else if(!(month_span[12] %in% months_in_y) & !is.na(month_span[12])){
      rasterImage(readPNG(paste(year_plots,"/",monthvec[month_span[12]],"blank_n",".png",sep="")),0,0,1,1)
    }else{
      rasterImage(readPNG(paste(blankfolder,"/blank_n_true.png",sep="")),0,0,1,1)
    }
    dev.off()
  }
}

}
  if(run_type=="both"|run_type=="gifs"){
  
  
################make gifs#############################
  

if(gif_interval == "month"|gif_interval == "both"){
  gif_folder_m <- paste(topfolder,"/","Month Gifs and Movies",sep="")
  dir.create(gif_folder_m)

  for(q in unique(pdata$Species)){
    pdata_sp <- pdata[pdata$Species==q,] 
    sp_folder <- paste(gif_folder_m,"/",q,sep="")
    dir.create(sp_folder)
    counter <- "0001"
    for(r in sort(unique(pdata_sp$Year))){
      pdata_sp_y<- pdata_sp[pdata_sp$Year==r,] 
      pdata_sp_y <- pdata_sp_y[order(pdata_sp_y$Dateformat),]
      for( s in unique(pdata_sp_y$Month)){
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$Month==s,] 
        dummy_row <- data.frame(as.character(pdata_sp_y_m[1,1])," ",scale_symbol_position_ind[2],scale_symbol_position_ind[1],pdata_sp_y_m[1,5],pdata_sp_y_m[1,6],pdata_sp_y_m[1,7],no_presence_symbol_size,0,low_effort_threshold+1,"yes",pdata_sp_y_m$Dateformat[1])
        colnames(dummy_row)<- colnames(pdata_sp_y_m)
        pdata_sp_y_m <- rbind(pdata_sp_y_m,dummy_row)
        finalfilename <- paste("img",counter,sep="")
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
    system(paste('"//nmfs/akc-nmml/CAEP/Acoustics/ArcMAP/Mapping with R/Scripts/Accesory Code/imagemagick/magick.exe" -delay ',gifspeed_month,' *.png ',q,"_month.gif",sep=""))
    system(paste('"//nmfs/akc-nmml/CAEP/Acoustics/ArcMAP/Mapping with R/Scripts/Accesory Code/ffmpeg/ffmpeg.exe" -r ',framerate_month,' -i "img%04d.png" -codec:a libmp3lame ',q,'_month_movie.avi',sep=""))
    system(paste(shQuote("//nmfs/akc-nmml/CAEP/Acoustics/ArcMAP/Mapping with R/Scripts/Accesory Code/gifsicle/gifsicle.exe",type="cmd"), " gifsicle -O3 ",q,"_month.gif -o ",q,"_month_optimized.gif",sep=""))
    file.remove(list.files(pattern=paste(q,"_month.gif",sep="")))
  }
  }
    if(gif_interval == "week"|gif_interval == "both"){
    
    gif_folder_w <- paste(topfolder,"/","Week Gifs and Movies",sep="")
    dir.create(gif_folder_w)  
    
    for(q in unique(pdatad$Species)){
      pdatad_sp <- pdatad[pdatad$Species==q,] 
      sp_folder <- paste(gif_folder_w,"/",q,sep="")
      dir.create(sp_folder)
      counter <- "00001"
      for(r in sort(unique(pdatad_sp$Year))){
        pdatad_sp_y<- pdatad_sp[pdatad_sp$Year==r,]
        pdatad_sp_y <- pdatad_sp_y[order(pdatad_sp_y$Dateformat),]
        for(s in unique(pdatad_sp_y$Date)){
          pdatad_sp_y_d <- pdatad_sp_y[pdatad_sp_y$Date==s,] 
          dummy_row1 <- data.frame(as.character(pdatad_sp_y_d[1,1])," ",scale_symbol_position_wgif[2],scale_symbol_position_wgif[1],pdatad_sp_y_d[1,5],pdatad_sp_y_d[1,6],pdatad_sp_y_d[1,7],no_presence_symbol_size,0,low_effort_threshold+1,"yes",pdatad_sp_y_d$Dateformat[1])
          dummy_row2 <- data.frame(as.character(pdatad_sp_y_d[1,1])," ",scale_symbol_lessthantwo[2],scale_symbol_lessthantwo[1],pdatad_sp_y_d[1,5],pdatad_sp_y_d[1,6],pdatad_sp_y_d[1,7],lessthantwo_symbol_size,0,low_effort_threshold+1,"point",pdatad_sp_y_d$Dateformat[1])
          colnames(dummy_row1)<- colnames(pdatad_sp_y_d)
          colnames(dummy_row2)<- colnames(pdatad_sp_y_d)
          pdatad_sp_y_d <- rbind(pdatad_sp_y_d,dummy_row1,dummy_row2)
          finalfilename <- paste("img",counter,sep="")
          png(paste(sp_folder,"/",finalfilename,".png",sep=""),width=(image_width*1.054054),height=(image_height*1.02266),units="in",res=gif_week_res) 
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
      system(paste('"//nmfs/akc-nmml/CAEP/Acoustics/ArcMAP/Mapping with R/Scripts/Accesory Code/imagemagick/magick.exe" -delay ',gifspeed_week,' *.png ',q,"_week.gif",sep=""))
      system(paste('"//nmfs/akc-nmml/CAEP/Acoustics/ArcMAP/Mapping with R/Scripts/Accesory Code/ffmpeg/ffmpeg.exe" -r ',framerate_week,' -i "img%05d.png" -codec:a libmp3lame ',q,'_week_movie.avi',sep=""))
      system(paste(shQuote("//nmfs/akc-nmml/CAEP/Acoustics/ArcMAP/Mapping with R/Scripts/Accesory Code/gifsicle/gifsicle.exe",type="cmd"), " gifsicle -O3 ",q,"_week.gif -o ",q,"_week_optimized.gif",sep=""))
      file.remove(list.files(pattern=paste(q,"_week.gif",sep="")))
    }
  }
  
  


#make animations: imagemagick. If you are getting errors here, make sure path at line " system(paste('"C:\\Program Files\\ImageMagick-7.0.6-Q16\\magick.exe" -delay 160 *.png ',q,".gif",sep=""))" matches the install directory on your computer, and the correct version of imagemagick. If a new version is available, check to make sure that "magick.exe" is still pathable. 

}
}else{
  
############gif speed change#####################################
  
if(dir.exists(paste(topfolder,"/","Month Gifs",sep=""))){
  species <- list.files(paste(topfolder,"/","Month Gifs",sep=""))
  for(l in species){
    print(paste("converting",l,"month gif"))
    setwd(paste(topfolder,"/","Month Gifs","/",l,sep=""))
    system(paste('"//nmfs/akc-nmml/CAEP/Acoustics/ArcMAP/Mapping with R/Scripts/Accesory Code/imagemagick/magick.exe" convert -delay ',gifspeed_month,'x100 ',l,'_month_optimized.gif ',l,'_month_optimized_x',gifspeed_month,'.gif',sep=""))
    
 }
  
}
  
if(dir.exists(paste(topfolder,"/","Week Gifs",sep=""))){
  species <- list.files(paste(topfolder,"/","Week Gifs",sep=""))
  for(m in species){
    print(paste("converting",m,"week gif"))
    setwd(paste(topfolder,"/","Week Gifs","/",m,sep=""))
    system(paste('"//nmfs/akc-nmml/CAEP/Acoustics/ArcMAP/Mapping with R/Scripts/Accesory Code/imagemagick/magick.exe" convert -delay ',gifspeed_week,'x100 ',m,'_week_optimized.gif ',m,'_week_optimized_x',gifspeed_week,'.gif',sep=""))
  }
  
}


}
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)


######################in progress###########

#setwd("//AKC0SS-N086/NMML_Users/daniel.woodrich/My Document")

#workdir <- "//nmfs/akc-nmml/CAEP/Acoustics/ArcMAP/Mapping with R/Scripts/Accesory Code/gifsicle-master/src"
#setwd(workdir)
#system("nmake -f Makefile.bcc")
#system("gifsicle -O1 Bowhead.gif -o Bowhead_optimized.gif")
#setwd("//nmfs/akc-nmml/CAEP/Acoustics/ArcMAP/Mapping with R/Output files/CHAOZ-X map run test 2/Gifs and stand alone slides/Bowhead")

#workdir <- "//nmfs/akc-nmml/CAEP/Acoustics/ArcMAP/Mapping with R/Output files/CHAOZ-X map run gifs err3/Gifs and stand alone slides/Gunshot"
#setwd(workdir) 
#system(paste(shQuote("//nmfs/akc-nmml/CAEP/Acoustics/ArcMAP/Mapping with R/Scripts/Accesory Code/gifsicle/gifsicle.exe",type="cmd"), "gifsicle -O3 Gunshot.gif -o Gunshot_optimized.gif"))

#test area:
#q <- "Gunshot"
#sp_folder <- paste(gif_folder_m,"/Gunshot",sep="")
#setwd(sp_folder)
#system(paste('"//nmfs/akc-nmml/CAEP/Acoustics/ArcMAP/Mapping with R/Scripts/Accesory Code/ffmpeg/ffmpeg.exe" -r ',framerate_month,' -i "img%04d.png" -codec:a libmp3lame ',q,'_month_movie.avi',sep=""))
#system(paste('"//nmfs/akc-nmml/CAEP/Acoustics/ArcMAP/Mapping with R/Scripts/Accesory Code/ffmpeg/ffmpeg.exe" -r ',framerate_week,' -i "img%05d.png" -codec:a libmp3lame ',q,'_week_movie.avi',sep=""))
