start.time <- Sys.time()
#Daiel Woodrich
#further edited by Dana Wright
#August 2017
#AFSC
#daniel.woodrich@noaa.gov, d.woodrich1@gmail.com
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
  install.packages("animation")
  install.packages("grid")
  install.packages("cowplot")
  install.packages("datasets")
  install.packages("maps")
  
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
library(cowplot)
library(datasets)
library(maps)


#define useful functions
scale_x_longitude <- function(xmin=-180, xmax=180, step=1, ...) {
  xbreaks <- seq(xmin,xmax,step)
  xlabels <- unlist(lapply(xbreaks, function(x) ifelse(x > 180, parse(text=paste0(360-x,"*W")), ifelse(x < 180, parse(text=paste0(x,"*E")),x))))
  return(scale_x_continuous("longitude",limits = c(Boundaries[1], Boundaries[2]), breaks = xbreaks, labels = xlabels, expand = c(0, 0), ...))
}

scale_y_latitude <- function(ymin=-90, ymax=90, step=0.5, ...) {
  ybreaks <- seq(ymin,ymax,step)
  ylabels <- unlist(lapply(ybreaks, function(x) ifelse(x < 0, parse(text=paste0(-1*x,"*S")), ifelse(x > 0, parse(text=paste0(x,"*N")),x))))
  return(scale_y_continuous("latitude",limits = c(Boundaries[3], Boundaries[4]), breaks = ybreaks, labels = ylabels, expand = c(0, 0), ...))
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

rotate_df_trim_long <- function(df){
  df <- within(df, {
    long <- ifelse(long < 0, long + 360, long)
    long <- ifelse((long < Boundaries[1]),Boundaries[1],long)
    long <- ifelse((long > Boundaries[2]),Boundaries[2],long)
  })
  return(df)
}

trim_lat <- function(df){
  df <- within(df, {
    lat <- ifelse((lat < Boundaries[3]),Boundaries[3],lat)
    lat <- ifelse((lat > Boundaries[4]),Boundaries[4],lat)
  })
  return(df)
}

Unproject_shp <- function(shp){
  shp <- spTransform(shp,"+proj=longlat")
  return(shp) 
}

Import_shp <- function(shp){
  shp2 <- readOGR(dsn=paste(input_folder,"/Shapefiles",sep=""), layer=shp)
  return(shp2)
}


plot_base <- function(Land,Reg,MAP_df,raster_color_palette,image_height)
{
  ggplot()+ 
    geom_tile(data=MAP_df,aes(x=long,y=lat,fill=value))+
    scale_fill_gradientn(colours=raster_color_palette,guide=FALSE) +
    geom_polygon(data=Reg,aes(x=long, y=lat, color=color),fill=NA,size=0.75) +
    scale_color_manual(name="not shown",values = as.character(unique(Reg$color)),guide=FALSE)+
    geom_polygon(aes(x = long, y = lat, group = group), data = Land,fill= "grey50",color = "gray25",size = 0.1)+
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    theme(axis.title=element_blank(),
          legend.box.spacing = unit(0, "mm"),
          axis.text=element_blank(),
          axis.ticks.length = unit(0, "mm"),
          plot.margin=unit(c(0,(image_width/50),-(image_height/50),-(image_width/50)),"mm"))                 #ultra small right/left margin to fix graphics bug where white lines are draw. negative value to remove bottom space
}

plot_blank <- function(MAP_df,longlow,longhigh,longbreak,latlow,lathigh,latbreak,Boundaries){
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

plot_data <- function(pdata_sp_y_m,Boundaries,longlow,longhigh,longbreak,latlow,lathigh,latbreak,gridlines_transparency,legend_position,legend_text_size,axis_text_size_slides,Base_map_img,counter,nodat)
{
  ggplot()+
    annotation_raster(Base_map_img, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
    geom_point(data=pdata_sp_y_m, aes(x=long, y=lat, size = percDaysWcalls,shape=percDWCis0),stroke=1.5,fill="green",color="black")+
    scale_radius(name="",limits=c(1,100),range=c(legend_size_modifier_sli,legend_size_modifier_sli*10),guide = guide_legend(override.aes = list(shape=21,colour = "black",fill="green")))+
    scale_shape_manual(name="not shown ",values = c('no' = 21,'yes' = no_presence_symbol),guide=FALSE)+
    geom_text(data=pdata_sp_y_m,aes(x=(long+((0.5/5*legend_size_modifier_sli))+(percDaysWcalls/80/5*legend_size_modifier_sli)), y=(lat+(0.04+percDaysWcalls/120/5*legend_size_modifier_sli)),label=ifelse(X.daysWrecs<low_effort_threshold,"*",'')),size=15)+
    geom_vline(xintercept=seq(longlow,Boundaries[2],longbreak),color=alpha('black',gridlines_transparency))+ 
    geom_hline(yintercept=seq(latlow,Boundaries[4],latbreak),color=alpha('black',gridlines_transparency))+
    scale_x_longitude(xmin=longlow, xmax=longhigh, step=longbreak,position="top") +
    scale_y_latitude(ymin=latlow, ymax=lathigh, step=latbreak)+
    {if((counter==1)) 
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position = c(legend_position[1],legend_position[2]),
            legend.key=element_rect(colour=alpha('gray50',0.0),fill=alpha('gray50',0.0),size=0.5),
            legend.background = element_rect(colour = alpha('gray50',0.0), fill = alpha('gray50',0.0)),
            legend.text=element_text(size=legend_text_size,color="white",face="bold"),
            legend.key.size = unit(0, 'lines'),
            legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"),
            axis.text=element_text(size=axis_text_size_slides),
            plot.margin=unit(c(0,0,0,0),"mm")) 
      else if((counter==2|counter==3|counter==4) & nodat==FALSE) 
        theme(axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              legend.position = "none",
              axis.text.x=element_text(size=axis_text_size_slides),
              plot.margin=unit(c(0,0,0,0),"mm")) 
      else if((counter==5|counter==9)& nodat==FALSE)
        theme(axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none",
              axis.text.y=element_text(size=axis_text_size_slides),
              plot.margin=unit(c(0,0,0,0),"mm")) 
      else if((counter==6|counter==7|counter==8|counter==10|counter==11|counter==12)& nodat==FALSE)
        theme(axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              legend.position = "none",
              plot.margin=unit(c(0,0,0,0),"mm")) 
      else 
        theme(axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              legend.position = "none",
              plot.margin=unit(c(0,0,0,0),"mm"))}+
              {if(counter==1) annotate(geom="text",zero_value_scale_position[1],zero_value_scale_position[2],label="0",color="white",size=(legend_text_size*0.375),fontface=2) else geom_hline()}+
    annotate(geom="text",month_label_position[1],month_label_position[2],label=paste(monthvec[pdata_sp_y_m[1,6]]),color="white",size=month_label_size,fontface=2)
  
}



#OPTIONAL USER INPUTS. Defaults specified. 
run_name <- "N Bering Chukchi and Beaufort Fall 2017 Timed run"      #name your run. Unique names will generate a new folder with this name in \\nmfs\akc-nmml\CAEP\Acoustics\ArcMAP\Mapping with R\Output files\. Duplicate names will overwrite that folder. trailing space in string will crash code.
run_type <- "slides"                                             #"slides","gifs", or "both"
gif_interval <- "month"                                          #"week" or "month" or "both"
test_params <- "n"                                                   #quickly display a single plot to view specified parameters
change_gif_speed <- "n"                                           #"y" or "n". update gif speeds for current run based on parameters in below block. much faster than replotting. "y" on this option skips all plotting sections. 

num_lat_ticks <- 4
num_long_ticks <- 4
low_effort_threshold <- 15                                           #threshold below which an asterisk is placed on the dots. 
legend_position <- c(.79,.43)                                       #scaled from 0-1, easiest to just enter test values here to reposition. 
legend_position_ind <- c(.837,.88)                                   #gifs and videos

#points on legend for the zero data and <2 data
scale_symbol_position <- c(360-154.85,68.8)
zero_value_scale_position <- c(360-151,68.8)
#scale_symbol_position_ind <- c(,)
#zero_value_scale_position_ind <- c(,)
#scale_symbol_lessthantwo <- c(,)
#lessthantwo_text_pos <- c(,)
#scale_symbol_position_wgif <- c(,)
#zero_value_scale_position_wgif <- c(,)


month_label_position <- c(360-174.5,59.2)                                   #for grid and individual plot month labels, lat/long
month_and_year_label_position <- c(360-155.2,65.7)         #for slide month and year labels, lat/long
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
image_height <- 7                                              #height of output image (just the map region). Default value (11.0558) determined from arcGIS map window dimensions of Dana Bathymetry.
image_width <-  9.8                                              #width of output image (just the map region). Default value (9.3902) determined from arcGIS map window dimensions of Dana Bathymetry.
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
MAP <- raster(paste(input_folder,"/Rasters/","JustBathy_NBerChukBeau.tif",sep=""))
#manually define to liking. Can be any length and combination
raster_color_palette <- c("white",lighten("skyblue1"),darken("skyblue1"),"steelblue4","steelblue4",darken("steelblue4")) 

#Import chukchi and AW region shapefile. 
Region_vector1 <- c("AW_area2r","CHX_area2r")
#manually define region colors, in order that you type them in above. Must be same length 
Region_colors <- c("orange","purple") 


#import land (polygon) shapefiles. 
Land_vector1 <- c("AKBering","RusIslands")

#presence data 
pdata <- read.csv(paste(input_folder,"/","Data frame","/","ChuknBerMonthlymapRnew.csv",sep="")) #monthly
colnames(pdata) <- c('species','mooringName','lat','long','year','month', 'date','percDaysWcalls', 'X.daysWcalls','X.daysWrecs')
pdatad <- read.csv(paste(input_folder,"/","Data frame","/","AWCHX_LTWeeklymapRNEW.csv",sep="")) #weekly
colnames(pdatad) <- c('species','mooringName','lat','long','year','week', 'date','percBinsWcalls', 'X.binsWcalls','X.binsWrecs')


########################Map data processing#######################

#Get rid of positive values so bathymetry will scale better
MAP[MAP>=-3]=-3  #set negative values to a point so that intermediate values will scale more naturally
MAP[MAP<=-1300] <- -1300
MAP <- (MAP*-1)+1
#transform data to create a beautiful depth illusion
MAP <- log(MAP,2)
#shift and trim  raster 
MAP <- shift(rotate(shift(MAP, 180)), 180)

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

longlats2 <- lapply(longlats1,fortify)
longlats2reg <- lapply(longlats1reg,fortify)
longlats3 <- lapply(longlats2,rotate_df_trim_long)
longlats3reg <- lapply(longlats2reg,rotate_df_trim_long )
longlats3 <- lapply(longlats3,trim_lat)
longlats3reg <- lapply(longlats3reg,trim_lat)

for(i in 1:length(Land_vector1)){
  group_change <- as.numeric(longlats3[[i]]$group)
  group_change <- group_change*(.9273+(i*0.0001))   #Ideally makes group ID unique from one shapefile to another. If bad geometry appears, can tweak this formula. Arbitrary values to create uniqueness between shapefiles. 
  longlats3[[i]]$group <- as.factor(group_change)
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

Reg$color <- factor(Reg$color,levels=Region_colors)

monthvec <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
pdata$percDWCis0 <- ifelse(pdata$percDaysWcalls==0,"yes","no")
pdatad$percDWCis0 <- ifelse(pdatad$percBinsWcalls==0,"yes","no")

#################Presence data processing######################
pdata <- rotate_df_long(pdata)
pdatad <- rotate_df_long(pdatad)

#remove species with no calling. Change to remove based on calling max() == 0
for(r in unique(pdata$species)){
  if(max(pdata[pdata$species==r,]$percDaysWcalls)==0){
    pdata <- pdata[pdata$species!=r,]
  }else{
    pdata<- pdata
  }
}

for(q in unique(pdatad$species)){
  if(max(pdatad[pdatad$species==q,]$percBinsWcalls)==0){
    pdatad <- pdatad[pdatad$species!=q,]
  }else{
    pdatad <- pdatad
  }
}

pdata$percDaysWcalls[pdata$percDaysWcalls==0] <- no_presence_symbol_size
pdatad$percBinsWcalls[pdatad$percBinsWcalls==0] <- no_presence_symbol_size
pdatad[pdatad$percBinsWcalls<2,11] <- "point"   
pdatad[pdatad$percBinsWcalls<2,8] <-lessthantwo_symbol_size   

#change data frame dates to r date objects. 
pdata$dateformat <- as.Date(as.character(pdata$date),format="%m/%d/%Y")
pdatad$dateformat <- as.Date(as.character(pdatad$date),format="%m/%d/%y")

topfolder <- paste(output_folder,"/",run_name,sep="")
dir.create(topfolder)

#############slides######################

#plot base and blank map 

base_folder <- paste(topfolder,"/","Base_maps",sep="")
dir.create(base_folder)

png(paste(base_folder,"/base_map.png",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
print(plot_base(Land,Reg,MAP_df,raster_color_palette,image_height))
dev.off()

Base_map_img <- readPNG(paste(base_folder,"/base_map.png",sep=""))

png(paste(base_folder,"/blank_map.png",sep=""),width=(image_width),height=(image_height),units="in",res=100) 
print(plot_blank(MAP_df,longlow,longhigh,longbreak,latlow,lathigh,latbreak,Boundaries))
dev.off()

Blank_map_img <- readPNG(paste(base_folder,"/blank_map.png",sep=""))

#create slide folder 
slide_folder <- paste(topfolder,"/","Slides",sep="")
dir.create(slide_folder)

for(p in unique(pdata$species)){
  pdata_sp <- pdata[pdata$species==p,]
  sp_folder <- paste(slide_folder,"/",p,sep="")
  dir.create(sp_folder)
  for(x in unique(pdata_sp$year)){
    pdata_sp_y<- pdata_sp[pdata_sp$year==x,]
    year <- x
    months_in_y <- unique(pdata_sp_y$month)
    months_in_y <- sort(months_in_y)
    month_span <- seq(months_in_y[1],tail(months_in_y,1))
    white_plots_needed <- 12-length(month_span)
    pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$month==months_in_y[1],]
    dummy_row <- data.frame(as.character(pdata_sp_y_m[1,1])," ",scale_symbol_position[2],scale_symbol_position[1],pdata_sp_y_m[1,5],pdata_sp_y_m[1,6],pdata_sp_y_m[1,7],no_presence_symbol_size,0,low_effort_threshold+1,"yes",pdata_sp_y_m$dateformat[1])
    colnames(dummy_row)<- colnames(pdata_sp_y_m)
    counter<- 0
    for(q in month_span){
      counter <- counter+1
      nodat <- FALSE
        pdata_sp_y_m <- pdata_sp_y[pdata_sp_y$month==q,]
        if(nrow(pdata_sp_y_m)!=0){
          if(counter==1){
            pdata_sp_y_m <- rbind(pdata_sp_y_m,dummy_row)
          }
          if(counter==1|5|9){
            image_width_plt <- (image_width*image_correction_width)
          }else{
            image_width_plt <- (image_width)
          }
          if(counter==1|2|3|4){
            image_height_plt <- (image_height*image_correction_height)
          }else{
            image_height_plt <- (image_height)
          }
          png(paste(sp_folder,"/",year,"_",counter,"_temp.png",sep=""),width=(image_width_plt),height=(image_height_plt),units="in",res=slide_res) 
          print(plot_data(pdata_sp_y_m,Boundaries,longlow,longhigh,longbreak,latlow,lathigh,latbreak,gridlines_transparency,legend_position,legend_text_size,axis_text_size_slides,Base_map_img,counter,nodat))
          dev.off()
        }else{
          nodat <- TRUE
          #plot blank w month
          png(paste(sp_folder,"/",year,"_",counter,"_temp.png",sep=""),width=(image_width_plt),height=(image_height_plt),units="in",res=slide_res) 
          print(plot_data(pdata_sp_y_m,Boundaries,longlow,longhigh,longbreak,latlow,lathigh,latbreak,gridlines_transparency,legend_position,legend_text_size,axis_text_size_slides,Base_map_img,counter,nodat))
          dev.off()
          nodat <- FALSE
        }
        }
    for(k in 1:white_plots_needed){
    counter <- counter+1
    png(paste(sp_folder,"/",year,"_",counter,"_temp.png",sep=""),width=(image_width_plt),height=(image_height_plt),units="in",res=slide_res)
    print(Blank_map_img)
    dev.off()
    }
    png(paste(sp_folder,"/",p,"_",year,".png",sep=""),width=((image_width*image_correction_width)+image_width*3),height=((image_height*image_correction_height)+(image_height*2)),units="in",res=(slide_res))
    par(mai=c(0.1,0.1,0.1,0.1), oma=c(0,0,0,0),xpd=NA)
    layout(matrix(1:12, ncol=4, byrow=TRUE),widths=c((image_width*image_correction_width),image_width,image_width,image_width),heights=c((image_height*image_correction_height),image_height,image_height))
    counter<-0
    for(x in 1:12){
      counter <- counter+1
      plot(NA,xlim=0:1,ylim=0:1,axes=0,bty="n",xaxs="i",yaxs="i",ylab="")
      rasterImage(readPNG(paste(sp_folder,"/",year,"_",counter,"_temp.png",sep="")),0,0,1,1)
    }
    dev.off()
    setwd(paste(sp_folder,sep=""))
    file.remove(list.files(pattern="temp.png"))
  }
}

end.time <- Sys.time()

print(end.time-start.time)





















#pdata_sp <- pdata[pdata$species==unique(pdata$species)[1],]
#pdata_sp_y <- pdata_sp[pdata$year==unique(pdata$year)[1],]
#pdata_sp_y_m <- pdata_sp_y[pdata$month==unique(pdata$month)[1],]

#png(paste(slide_folder,"/base_map.png",sep=""),width=(image_width),height=(image_height),units="in",res=300) 
#print(plot_base(Land,Reg,MAP_df,raster_color_palette,image_height))
#dev.off()

#switch<- FALSE
#Base_map_img <- readPNG(paste(slide_folder,"/base_map.png",sep=""))
#png(paste(blankfolder,"/base_mapx.png",sep=""),width=(image_width),height=(image_height),units="in",res=slide_res) 

#counter <- 10
#plot_data(pdata_sp_y_m,Boundaries,longlow,longhigh,longbreak,latlow,lathigh,latbreak,gridlines_transparency,legend_position,legend_text_size,axis_text_size_slides,Base_map_img,counter,nodat)
#dev.off()
