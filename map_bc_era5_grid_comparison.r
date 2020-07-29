##Compare monthly climatologies of pr,tasmax,tasmin
##from the gridded observational datasets
##PNWNAmet, PRISM
##ANSUPLIN, Daymet
##WorldClim, ERA5-Land

##CaPa for precipitation?

library(ncdf4)
library(maps)
source('/storage/home/ssobie/code/repos/assessments/resource.region.map.support.r',chdir=T)
source('/storage/home/ssobie/code/repos/LHASA/bc.albers.map.support.r')

##-----------------------------------------------------------

make_obs_bc_albers_plot <- function(var.name,plot.type,seas,plot.title,plot.data,shared.range=NULL,
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

   if (var.name == 'pr' & plot.type == 'past' & seas == 'Annual') {
      class.breaks <- c(0,250,500,750,1000,1250,1500,2000,2500,3000,4000,5000,6000,7000)
      plot.data[plot.data > 7000] <- 7000
   }
   if (var.name == 'pr' & plot.type == 'past' & seas == 'Winter') {
      class.breaks <- c(0,100,200,300,400,500,750,1000,1250,1500,2000,2500,3000)
      plot.data[plot.data > 3000] <- 2990
   }
   if (var.name == 'pr' & plot.type == 'past' & seas == 'Spring') {
      class.breaks <- c(0,100,200,300,400,500,750,1000,1250,1500,2000,2500)
      plot.data[plot.data > 2500] <- 2490
   }
   if (var.name == 'pr' & plot.type == 'past' & seas == 'Summer') {
      class.breaks <- c(0,50,100,150,200,250,300,400,500,750,1000,1250,1500,2000)
      plot.data[plot.data > 2000] <- 1990
   }
   if (var.name == 'pr' & plot.type == 'past' & seas == 'Fall') {
      class.breaks <- c(0,100,200,300,400,500,750,1000,1250,1500,2000,2500,3000)
      plot.data[plot.data > 3000] <- 2990
   }


   if (var.name == 'pr' & plot.type == 'diff' & seas == 'Annual') {
      class.breaks <- c(-6000,-4000,-2000,-1000,-750,-500,-250,-100,0,100,250,500,1000,3000)
   }
   if (var.name == 'pr' & plot.type == 'diff' & seas == 'Winter') {
      class.breaks <- c(-6000,-4000,-2000,-1000,-750,-500,-250,-100,0,100,250,500,1000,3000)
   }
   if (var.name == 'pr' & plot.type == 'diff' & seas == 'Spring') {
      class.breaks <- c(-1500,-1000,-750,-500,-250,-100,-50,0,50,100,250,500,1000)
   }
   if (var.name == 'pr' & plot.type == 'diff' & seas == 'Summer') {
      class.breaks <- c(-1500,-1000,-750,-500,-250,-200,-100,-50,0,50,100,250,400,500,600)
   }
   if (var.name == 'pr' & plot.type == 'diff' & seas == 'Fall') {
      class.breaks <- c(-6000,-4000,-2000,-1000,-750,-500,-250,-100,0,100,250,500,1000,3000)
   }

   ##---------------------

   if (var.name == 'tasmax' & plot.type == 'past' & seas == 'Winter') {
      class.breaks <- seq(-16,10,2)
   }
   if (var.name == 'tasmax' & plot.type == 'past' & seas == 'Spring') {
      class.breaks <- seq(-12,16,2)
   }
   if (var.name == 'tasmax' & plot.type == 'past' & seas == 'Summer') {
      class.breaks <- seq(0,30,2)
   }
   if (var.name == 'tasmax' & plot.type == 'past' & seas == 'Fall') {
      class.breaks <- seq(-10,16,2)
   }


   if (var.name == 'tasmax' & plot.type == 'diff' & seas == 'Annual') {
      class.breaks <- seq(-8,6,1)
   }
   if (var.name == 'tasmax' & plot.type == 'diff' & seas == 'Winter') {
      class.breaks <- seq(-14,8,2)
   }
   if (var.name == 'tasmax' & plot.type == 'diff' & seas == 'Spring') {
      class.breaks <- seq(-8,6,1)
   }
   if (var.name == 'tasmax' & plot.type == 'diff' & seas == 'Summer') {
      class.breaks <- seq(-10,8,1)
   }
   if (var.name == 'tasmax' & plot.type == 'diff' & seas == 'Fall') {
      class.breaks <- seq(-8,10,1)
   }



   if (var.name == 'tasmin' & plot.type == 'diff' & seas == 'Annual') {
      class.breaks <- seq(-8,6,1)
   }

 
   if (var.name == 'tasmax' & plot.type == 'past' & seas == 'Annual') {
      class.breaks <- seq(-20,20,2)
   }
   if (var.name == 'tasmin' & plot.type == 'past' & seas == 'Annual') {
      class.breaks <- seq(-24,8,2)
   }

   colour.ramp <- get.legend.colourbar(var.name=col.var,map.range=map.range,
                                       my.bp=0,class.breaks=class.breaks,
                                       type=plot.type)
   map.class.breaks.labels <- get.class.break.labels(class.breaks,type=plot.type)

   alb.crs <- "+init=epsg:3005"
   lons <- c(-145.0, -140.0,-135.0,-130.0,-125.0,-120.0,-115.0,-110.0)
   lats <- c(  48.0,  50.0,  52.0,  54.0,  56.0,  58.0,  60.0,  62.0)
   grats <- add.graticules(lons,lats,alb.crs)

   shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/bc'
   bc.shp <- get.region.shape('bc',shape.dir)

   map.extent <- extent(c(-136.0,-114.0,48.0,60.5))
   crop.extent <- extent(c(-145,-100,45.0,65))

   plot.data.albers <- projectRaster(plot.data,crs=CRS(alb.crs))
   map.bounds <- extent(plot.data.albers)
   plot.bounds <- make.plot.window(map.bounds,spTransform(bc.shp,CRS(alb.crs)))

   plot.window.xlim <- plot.bounds$xlim ##c(map.bounds@xmin,map.bounds@xmax)
   plot.window.ylim <- plot.bounds$ylim ##c(map.bounds@ymin,map.bounds@ymax)

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

   image(plot.data.albers,add=T,col=colour.ramp,breaks=class.breaks)

   ##plot(spTransform(wco.shp,CRS(alb.crs)),add=TRUE,col='gray',lwd=0.25)
   plot(spTransform(borders.shp,CRS(alb.crs)),add=TRUE,border='black',lwd=0.25)
   plot(grats,add=TRUE,lty=3,col='gray',lwd=0.7)

   text.albers <- convert.to.alb.coords(lon=-132,lat=49,crs=alb.crs)
   text(text.albers[1],text.albers[2],plot.title,cex=2.5)


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
seas <- 'Annual'
mon <- 1
agg <- 'Precip'

plot.file <- paste0(plot.dir,'PRECIP.',seas,'.Total.Difference.at.ERA5-Land.Grid.TC.png')
##plot.file <- paste0(plot.dir,'TASMAX.',seas,'.Average.Difference.at.ERA5-Land.Grid.TC.png')
##plot.file <- paste0(plot.dir,'TASMIN.',seas,'.Average.Difference.at.ERA5-Land.Grid.TC.png')

##BC Extent from PRISM extent
ex <- extent(x=c(-141.0,-105.0),y=c(47,63))

##-----------------------------------------------------------
##ANUSPLIN Climatologies
aplin.dir <- paste0(obs.dir,'ANUSPLIN/ANUSPLIN_60ARCSEC/monthly/climatology/ERA5-Grid/')
aplin.file <- paste0(var.name,'_',clim,'_climatologies_ANUSPLIN_at_ERA5-Land_Grid_1981-2010.nc')

aplin.data <- read_climatologies(var.name,clim,ex,
                                 aplin.dir,aplin.file,mon)

##-----------------------------------------------------------
##PRISM Climatologies
prism.dir <- '/storage/data/climate/observations/gridded/PRISM/ERA5-Grid/'
type <- switch(var.name,pr='total',tasmax='average',tasmin='average')
prism.file <- paste0(var.name,'_',clim,'_',type,'_climatology_PRISM_at_ERA5-Land_Grid_198101-201012.nc')
prism.mon <- mon
prism.data <- read_climatologies(var.name,clim,ex,
                                 prism.dir,prism.file,prism.mon)

##-----------------------------------------------------------
##PNWNAmet Climatologies
pnw.dir <- paste0(obs.dir,'PNWNAmet/Derived/ERA5-Grid/')
type <- switch(var.name,pr='total',tasmax='average',tasmin='average')
pnw.file <- paste0(var.name,'_',clim,'_',type,'_climatology_PNWNAmet_at_ERA5-Land_Grid_observations_1981-2010.nc')

pnw.data <- read_climatologies(var.name,clim,ex,
                               pnw.dir,pnw.file,mon)

##-----------------------------------------------------------
##DAYMET Climatologies
daymet.dir <- paste0(obs.dir,'Daymet/climatologies/ERA5-Grid/')
daymet.file <- paste0(var.name,'_',clim,'_climatologies_BC_Daymet_at_ERA5-Land_Grid_1981_2010.nc')
daymet.data <- read_climatologies(var.name,clim,ex,
                               daymet.dir,daymet.file,mon)

##-----------------------------------------------------------
##WorldClim Climatologies
worldclim.dir <- paste0(obs.dir,'world_clim/ERA5-Grid/')
worldclim.file <- paste0(var.name,'_',clim,'_climatologies_British_Columbia_WorldClim_at_ERA5-Land_Grid_1970_2000.nc')
worldclim.data <- read_climatologies(var.name,clim,ex,
                               worldclim.dir,worldclim.file,mon)

##-----------------------------------------------------------
##TerraClimate Climatologies
terraclim.dir <- paste0(obs.dir,'TerraClimate/ERA5-Grid/')
terraclim.file <- paste0(var.name,'_',clim,'_climatologies_British_Columbia_TerraClimate_at_ERA5-Land_Grid_1981_2010.nc')
terraclim.data <- read_climatologies(var.name,clim,ex,
                               terraclim.dir,terraclim.file,mon)


##-----------------------------------------------------------
##ERA5-Land Climatologies
obs.dir <- '/storage/data/climate/observations/reanalysis/'
type <- switch(var.name,pr='total',tasmax='mean',tasmin='mean')
era5land.dir <- paste0(obs.dir,'ERA5-Land/climatologies/')
era5land.file <- paste0(var.name,'_',clim,'_',type,'_climatology_ERA5-Land_BC_1981-2010.nc')
era5land.data <- read_climatologies(var.name,clim,ex,
                                    era5land.dir,era5land.file,mon)

pnw_prism.data <- pnw.data - prism.data
aplin_prism.data <- aplin.data - prism.data
daymet_prism.data <- daymet.data - prism.data
worldclim_prism.data <- worldclim.data - prism.data
terraclim_prism.data <- terraclim.data - prism.data
era5land_prism.data <- era5land.data - prism.data

##-----------------------------------------------------------
##Annual Climatologies
##
   if ((var.name=='pr' | var.name=='rx5dayETCCDI')) {
     leg.title <- 'mm'
   }
   if ((var.name=='tasmax' | var.name=='tasmin')) {
     leg.title <- '\u00B0C'
   }

  png(file=plot.file,width=8,height=9,units='in',res=600,pointsize=6,bg='white')
  par(mfrow=c(3,2))
  par(mar=c(0,0,0,0),oma=c(6,6,4,18))
  par(mgp=c(4,1.5,0))

  rv <- make_obs_bc_albers_plot(var.name=var.name,plot.type='diff',seas=seas,
                                plot.title=paste0(seas,'\n',agg,'\n(PNWNAmet)'),pnw_prism.data,
                                yshort=TRUE)
  rv2 <- make_obs_bc_albers_plot(var.name=var.name,plot.type='past',seas=seas,
                          plot.title=paste0(seas,'\n',agg,'\n(PRISM)'),prism.data)
  par(xpd=NA)
  legend('topright',inset=c(-0.32,0), col = "black",
                                      legend=rv2$labels, pch=22, pt.bg = rv2$cols,
          pt.cex=4, y.intersp=0.8, title.adj=0.2, title=leg.title, xjust=0, cex=2.2,box.lwd=2) 
  par(xpd=FALSE)
                          
  rv3 <- make_obs_bc_albers_plot(var.name=var.name,plot.type='diff',seas=seas,
                          plot.title=paste0(seas,'\n',agg,'\n(ANUSPLIN)'),aplin_prism.data,
                          yshort=TRUE)
  rv4 <- make_obs_bc_albers_plot(var.name=var.name,plot.type='diff',seas=seas,
                          plot.title=paste0(seas,'\n',agg,'\n(Daymet)'),daymet_prism.data)
                          
  par(xpd=NA)
  legend('topright',inset=c(-0.34,0), col = "black",
                                      legend=rv4$labels, pch=22, pt.bg = rv4$cols,
          pt.cex=4, y.intersp=0.8, title.adj=0.2, title=leg.title, xjust=0, cex=2.2,box.lwd=2) 
  par(xpd=FALSE)

  rv5 <- make_obs_bc_albers_plot(var.name=var.name,plot.type='diff',seas=seas,
                          plot.title=paste0(seas,'\n',agg,'\n(TerraClimate)'),terraclim_prism.data,
                          xshort=TRUE,yshort=TRUE)
  rv6 <- make_obs_bc_albers_plot(var.name=var.name,plot.type='diff',seas=seas,
                          plot.title=paste0(seas,'\n',agg,'\n(ERA5-Land)'),era5land_prism.data,
                          xlabel=TRUE)

  mtext("Longitude (\u00B0E)",side=1,outer=TRUE,cex=2.0,line=3.6)
  mtext("Latitude (\u00B0N)",side=2,outer=TRUE,cex=2.0,line=3.6)

  dev.off()



##-----------------------------------------------------------
##Seasonal Climatologies

##-----------------------------------------------------------
##Monthly Climatologies