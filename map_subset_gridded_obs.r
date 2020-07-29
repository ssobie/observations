##Compare monthly climatologies of pr,tasmax,tasmin
##from the gridded observational datasets
##PNWNAmet, PRISM
##ANSUPLIN, Daymet
##WorldClim, ERA5-Land

##CaPa for precipitation?

library(scales)
library(ncdf4)
library(maps)
source('/storage/home/ssobie/code/repos/assessments/resource.region.map.support.r',chdir=T)
source('/storage/home/ssobie/code/repos/LHASA/bc.albers.map.support.r')

##-----------------------------------------------------------

make_obs_subset_albers_plot <- function(var.name,plot.type,seas,plot.title,plot.data,shared.range=NULL,
                        xlabel=FALSE,xshort=FALSE,ylabel=FALSE,yshort=FALSE) {
   col.var <- var.name

   if (!is.null(shared.range)) {
      map.range <- shared.range
   } else {
      map.range <- range(as.matrix(plot.data),na.rm=T)
   }

   class.breaks <- get.class.breaks(col.var,plot.type,map.range,manual.breaks='')
   print('Default Breaks')

   print(get.class.breaks(col.var,plot.type,range(as.matrix(plot.data),na.rm=T),manual.breaks=''))

##   if (var.name == 'pr' & plot.type == 'past' & seas == 'Annual') {
##      class.breaks <- c(0,250,500,750,1000,1250,1500,1750,2000,2500,3000)
##      plot.data[plot.data > 3000] <- 2990
##   }

   if (var.name == 'pr' & plot.type == 'past' & seas == 'Annual') {
      class.breaks <- c(0,250,500,750,1000,1250,1500,2000,2500,3000,4000,5000,6000,7000)
      plot.data[plot.data > 7000] <- 6990
   }

   if (var.name == 'tasmax' & plot.type == 'past' & seas == 'Annual') {
      class.breaks <- seq(-4,16,2)
   }
   if (var.name == 'tasmin' & plot.type == 'past' & seas == 'Annual') {
      class.breaks <- seq(-14,8,2)
   }

   colour.ramp <- get.legend.colourbar(var.name=col.var,map.range=map.range,
                                       my.bp=0,class.breaks=class.breaks,
                                       type=plot.type)
   map.class.breaks.labels <- get.class.break.labels(class.breaks,type=plot.type)

   alb.crs <- "+init=epsg:4326"
   lons <- c(-124.0,-123.5, -123.0,-122.5,-122.0,-121.5,-121.0,-120.5,-120.0)
   lats <- c(48.75, 49.0, 49.25, 49.5, 49.75, 50.0, 50.25, 50.5, 50.75)
##   grats <- add.graticules(lons,lats,alb.crs)

   shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/bc'
   bc.shp <- get.region.shape('bc',shape.dir)

   map.extent <- extent(c(-123.7,-120.65,48.9,50.8))
   crop.extent <- map.extent
   
   plot.data.albers <- projectRaster(plot.data,crs=CRS(alb.crs))
   plot.window.xlim <- c(map.extent@xmin,map.extent@xmax)
   plot.window.ylim <- c(map.extent@ymin,map.extent@ymax)



   plot(c(),xlim=plot.window.xlim,ylim=plot.window.ylim,xaxs='i',yaxs='i',
   bg='white',# 'gray94',
   xlab='Longitude (\u00B0E)',ylab='Latitude (\u00B0N)',main='',##plot.title,
   cex.axis=2.5,cex.lab=2.5,cex.main=2.5,mgp=c(3.5,2,0),axes=F)
   rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='lightgray')

   xtks <- get.proj.xaxis(lons,alb.crs,plot.window.ylim)
   ytks <- get.proj.yaxis(lats,alb.crs,plot.window.xlim)
##   if (mark[2]==6) {axis(2,at=ytks,label=lats,cex.axis=2.5)}
##   if (mark[1]==6) {axis(1,at=xtks,label=lons,cex.axis=2.5)}
   if (ylabel | yshort) {axis(2,at=ytks,label=rep('',length(lats)),cex.axis=3)}
   if (ylabel) {axis(2,at=ytks,label=lats,cex.axis=3)}
   if (yshort) {axis(2,at=ytks[1:(length(lats)-2)],label=lats[1:(length(lats)-2)],cex.axis=3)}

   if (xlabel | xshort) {axis(1,at=xtks,label=rep('',length(lons)),cex.axis=3)}
   if (xshort) {axis(1,at=xtks[1:(length(lons)-2)],label=lons[1:(length(lons)-2)],cex.axis=3)}
   if (xlabel) {axis(1,at=xtks,label=lons,cex.axis=3)}

   bc.overlay <- 'north_america_state_provincial_boundaries'
   borders.shp <- readOGR(shape.dir, bc.overlay, stringsAsFactors=F, verbose=F)
   wco.shp <- readOGR('/storage/data/projects/rci/data/assessments/shapefiles/bc_common',
                           'ocean_mask', stringsAsFactors=F, verbose=F)
   shade.dir <- '/storage/data/projects/rci/data/winter_sports/study_map/'
   shade <- raster(paste0(shade.dir,'van_whistler_hillshade.tif'))

   wash.shp <- spTransform(readOGR(shade.dir, 'washington', stringsAsFactors=F, verbose=F),CRS("+init=epsg:4326"))
   rivers.shp <- spTransform(readOGR(shade.dir, 'van_whistler_rivers', stringsAsFactors=F, verbose=F),CRS("+init=epsg:4326"))
   lakes.shp <- spTransform(readOGR(shade.dir, 'van_whistler_lakes_simple_0.5%', stringsAsFactors=F, verbose=F),CRS("+init=epsg:4326"))


   image(shade,add=T,col = grey(1:100/100))
  
   image(plot.data.albers,add=T,col=alpha(colour.ramp,0.8),breaks=class.breaks)

   plot(lakes.shp,add=TRUE,col='lightgray',border='lightgray',xlim=plot.window.xlim,ylim=plot.window.ylim)
   plot(rivers.shp,add=TRUE,col='lightgray',border='lightgray',xlim=plot.window.xlim,ylim=plot.window.ylim)

   plot(spTransform(wco.shp,CRS(alb.crs)),add=TRUE,col='gray',lwd=0.25)
   plot(spTransform(wash.shp,CRS(alb.crs)),add=TRUE,col='gray',border='black',cex=0.5)

   ##plot(spTransform(borders.shp,CRS(alb.crs)),add=TRUE,border='black',lwd=0.25)
   ##plot(grats,add=TRUE,lty=3,col='gray',lwd=0.7)

   text(-121.2,50.5,plot.title,cex=2.5)

   leg.title <- 'Percent'
   if ((var.name=='pr' | var.name=='rx5dayETCCDI') & plot.type=='past') {
     leg.title <- 'mm'
   }
   if ((var.name=='tasmax' | var.name=='tasmin') & plot.type=='past') {
     leg.title <- '\u00B0C'
   }

   ##legend('topright', col = "black", legend=rev(map.class.breaks.labels), pch=22, pt.bg = rev(colour.ramp),
   ##      pt.cex=1.95, y.intersp=0.8, title.adj=0.2, title='mm', xjust=0, cex=2.25) ##leg.title
   box(which='plot',lwd=2)
   rv <- list(labels=rev(map.class.breaks.labels),cols=rev(colour.ramp))
   return(rv)
}

##-----------------------------------------------------------
read_climatologies <- function(var.name,clim,ex,
                               clim.dir,clim.file,mon) {

   clim.brick <- brick(paste0(clim.dir,clim.file))
   clim.data <- subset(clim.brick,mon)    
   clim.crop <- crop(clim.data,ex)
   return(clim.crop)
}


##*****************************************************************************************
##-----------------------------------------------------------
obs.dir <- '/storage/data/climate/observations/gridded/'
plot.dir <- '/storage/data/projects/rci/data/cas/gridded_observations/'

var.name <- 'pr'
clim <- 'annual'
mon <- 1
seas <- 'Annual'
agg <- 'Pr'


plot.file <- paste0(plot.dir,'PRECIP.',seas,'.Total.VW.Obs.Comparison.png')
##plot.file <- paste0(plot.dir,'TASMAX.',seas,'.Average.VW.Obs.Comparison.png')
##plot.file <- paste0(plot.dir,'TASMIN.',seas,'.Average.VW.Obs.Comparison.png')


##VW Extent
ex <- extent(c(-123.8,-120.55,48.8,50.9))


##-----------------------------------------------------------
##ANUSPLIN Climatologies
aplin.dir <- paste0(obs.dir,'ANUSPLIN/ANUSPLIN_60ARCSEC/climatology/')
aplin.file <- paste0(var.name,'_',clim,'_climatologies_ANUSPLIN_60ARCSEC_1981-2010.nc')

aplin.data <- read_climatologies(var.name,clim,ex,
                                 aplin.dir,aplin.file,mon)

##-----------------------------------------------------------
##PRISM Climatologies
prism.dir <- '/storage/data/climate/observations/gridded/PRISM/'
prism.var <- switch(var.name,pr='pr',tasmax='tmax',tasmin='tmin')
type <- switch(var.name,pr='total',tasmax='average',tasmin='average')
prism.file <- paste0(var.name,'_',clim,'_',type,'_climatology_PRISM_198101-201012.nc')
prism.mon <- mon
##if (clim == 'annual') {prism.mon <- 13}
prism.data <- read_climatologies(var.name,clim,ex,
                                 prism.dir,prism.file,prism.mon)

##-----------------------------------------------------------
##PNWNAmet Climatologies
pnw.dir <- paste0(obs.dir,'PNWNAmet/Derived/',clim,'/climatologies/')
type <- switch(var.name,pr='total',tasmax='average',tasmin='average')
pnw.file <- paste0(var.name,'_',clim,'_',type,'_climatology_PNWNAmet_observations_1981-2010.nc')

pnw.data <- read_climatologies(var.name,clim,ex,
                               pnw.dir,pnw.file,mon)

##-----------------------------------------------------------
##DAYMET Climatologies
daymet.dir <- paste0(obs.dir,'Daymet/climatologies/')
daymet.file <- paste0(var.name,'_',clim,'_climatologies_BC_Daymet_1981_2010.nc')
daymet.data <- read_climatologies(var.name,clim,ex,
                               daymet.dir,daymet.file,mon)

##-----------------------------------------------------------
##WorldClim Climatologies
worldclim.dir <- paste0(obs.dir,'world_clim/')
worldclim.file <- paste0(var.name,'_',clim,'_climatologies_British_Columbia_WorldClim_1970_2000.nc')
worldclim.data <- read_climatologies(var.name,clim,ex,
                               worldclim.dir,worldclim.file,mon)

##-----------------------------------------------------------
##ERA5-Land Climatologies
obs.dir <- '/storage/data/climate/observations/reanalysis/'
type <- switch(var.name,pr='total',tasmax='mean',tasmin='mean')
era5land.dir <- paste0(obs.dir,'ERA5-Land/climatologies/')
era5land.file <- paste0(var.name,'_',clim,'_',type,'_climatology_ERA5-Land_BC_1981-2010.nc')
era5land.data <- read_climatologies(var.name,clim,ex,
                                    era5land.dir,era5land.file,mon)



##-----------------------------------------------------------
##Annual Climatologies
##
   if ((var.name=='pr' | var.name=='rx5dayETCCDI')) {
     leg.title <- 'mm'
   }
   if ((var.name=='tasmax' | var.name=='tasmin')) {
     leg.title <- '\u00B0C'
   }


  png(file=plot.file,width=8,height=8,units='in',res=600,pointsize=6,bg='white')
  par(mfrow=c(3,2))
  par(mar=c(0,0,0,0),oma=c(6,6,4,18))
  par(mgp=c(4,1.5,0))

  rv <- make_obs_subset_albers_plot(var.name=var.name,plot.type='past',seas='Annual',
                                plot.title=paste0('Annual\n',agg,'\n(PNWNAmet)'),pnw.data,
                                ylabel=TRUE)
  rv2 <- make_obs_subset_albers_plot(var.name=var.name,plot.type='past',seas='Annual',
                          plot.title=paste0('Annual\n',agg,'\n(PRISM)'),prism.data)
  par(xpd=NA)
  legend('topright',inset=c(-0.32,0), col = "black",
                                      legend=rv$labels, pch=22, pt.bg = rv$cols,
          pt.cex=4, y.intersp=0.8, title.adj=0.2, title=leg.title, xjust=0, cex=2.2,box.lwd=2) 
  par(xpd=FALSE)

                          
  rv3 <- make_obs_subset_albers_plot(var.name=var.name,plot.type='past',seas='Annual',
                          plot.title=paste0('Annual\n',agg,'\n(ANUSPLIN)'),aplin.data,
                          ylabel=TRUE)
  rv4 <- make_obs_subset_albers_plot(var.name=var.name,plot.type='past',seas='Annual',
                          plot.title=paste0('Annual\n',agg,'\n(Daymet)'),daymet.data)
                          
  rv5 <- make_obs_subset_albers_plot(var.name=var.name,plot.type='past',seas='Annual',
                          plot.title=paste0('Annual\n',agg,'\n(WorldClim)'),worldclim.data,
                          xlabel=TRUE,ylabel=TRUE)

  rv6 <- make_obs_subset_albers_plot(var.name=var.name,plot.type='past',seas='Annual',
                          plot.title=paste0('Annual\n',agg,'\n(ERA5-Land)'),era5land.data,
                          xlabel=TRUE)



  mtext("Longitude (\u00B0E)",side=1,outer=TRUE,cex=2.0,line=3.6)
  mtext("Latitude (\u00B0N)",side=2,outer=TRUE,cex=2.0,line=3.6)

  dev.off()



##-----------------------------------------------------------
##Seasonal Climatologies

##-----------------------------------------------------------
##Monthly Climatologies