##Compare monthly climatologies of pr,tasmax,tasmin
##from the gridded observational datasets
##PNWNAmet, PRISM
##ANSUPLIN, Daymet
##WorldClim, ERA5-Land

##Use regridded data and created histograms of the values

library(ncdf4)
library(scales)
library(raster)

##-----------------------------------------------------------
read_climatologies <- function(var.name,clim,ex,
                               clim.dir,clim.file,mon) {

   clim.brick <- brick(paste0(clim.dir,clim.file))
   clim.data <- subset(clim.brick,mon)    
   clim.crop <- crop(clim.data,ex)
   return(clim.crop)
}

##-----------------------------------------------------------

read_regridded_obs_data <- function(var.name,clim,seas,mon,agg,
                                    breaks=NULL) {

   obs.dir <- '/storage/data/climate/observations/gridded/'
   ex <- extent(x=c(-141.0,-105.0),y=c(47,63))


   ##-----------------------------------------------------------
   ##ANUSPLIN Climatologies
   aplin.dir <- paste0(obs.dir,'ANUSPLIN/ANUSPLIN_60ARCSEC/monthly/climatology/ERA5-Grid/')
   aplin.file <- paste0(var.name,'_',clim,'_climatologies_ANUSPLIN_at_ERA5-Land_Grid_1981-2010.nc')
   aplin.data <- read_climatologies(var.name,clim,ex,
                                 aplin.dir,aplin.file,mon)
   aplin.data[aplin.data > 7000] <- 7000

   ##-----------------------------------------------------------
   ##PRISM Climatologies
   prism.dir <- '/storage/data/climate/observations/gridded/PRISM/ERA5-Grid/'
   type <- switch(var.name,pr='total',tasmax='average',tasmin='average')
   prism.file <- paste0(var.name,'_',clim,'_',type,'_climatology_PRISM_at_ERA5-Land_Grid_198101-201012.nc')
   prism.mon <- mon
   prism.data <- read_climatologies(var.name,clim,ex,
                                 prism.dir,prism.file,prism.mon)
   prism.data[prism.data > 7000] <- 7000

   ##-----------------------------------------------------------
   ##PNWNAmet Climatologies
   pnw.dir <- paste0(obs.dir,'PNWNAmet/Derived/ERA5-Grid/')
   type <- switch(var.name,pr='total',tasmax='average',tasmin='average')
   pnw.file <- paste0(var.name,'_',clim,'_',type,'_climatology_PNWNAmet_at_ERA5-Land_Grid_observations_1981-2010.nc')

   pnw.data <- read_climatologies(var.name,clim,ex,
                                  pnw.dir,pnw.file,mon)
   pnw.data[pnw.data > 7000] <- 7000

   ##-----------------------------------------------------------
   ##DAYMET Climatologies
   daymet.dir <- paste0(obs.dir,'Daymet/climatologies/ERA5-Grid/')
   daymet.file <- paste0(var.name,'_',clim,'_climatologies_BC_Daymet_at_ERA5-Land_Grid_1981_2010.nc')
   daymet.data <- read_climatologies(var.name,clim,ex,
                               daymet.dir,daymet.file,mon)
   daymet.data[daymet.data > 7000] <- 7000

   ##-----------------------------------------------------------
   ##WorldClim Climatologies
   worldclim.dir <- paste0(obs.dir,'world_clim/ERA5-Grid/')
   worldclim.file <- paste0(var.name,'_',clim,'_climatologies_British_Columbia_WorldClim_at_ERA5-Land_Grid_1970_2000.nc')
   worldclim.data <- read_climatologies(var.name,clim,ex,
                                worldclim.dir,worldclim.file,mon)
   worldclim.data[worldclim.data > 7000] <- 7000

   ##-----------------------------------------------------------
   ##ERA5-Land Climatologies
   obs.dir <- '/storage/data/climate/observations/reanalysis/'
   type <- switch(var.name,pr='total',tasmax='mean',tasmin='mean')
   era5land.dir <- paste0(obs.dir,'ERA5-Land/climatologies/')
   era5land.file <- paste0(var.name,'_',clim,'_',type,'_climatology_ERA5-Land_BC_1981-2010.nc')
   era5land.data <- read_climatologies(var.name,clim,ex,
                                    era5land.dir,era5land.file,mon)
   era5land.data[era5land.data > 7000] <- 7000

   min.breaks <- c(cellStats(prism.data,min,na.rm=T),cellStats(pnw.data,min,na.rm=T),
                   cellStats(aplin.data,min,na.rm=T),cellStats(daymet.data,min,na.rm=T),
                   cellStats(worldclim.data,min,na.rm=T),cellStats(era5land.data,min,na.rm=T))
   max.breaks <- c(cellStats(prism.data,max,na.rm=T),cellStats(pnw.data,max,na.rm=T),
                   cellStats(aplin.data,max,na.rm=T),cellStats(daymet.data,max,na.rm=T),
                   cellStats(worldclim.data,max,na.rm=T),cellStats(era5land.data,max,na.rm=T))             

   prism.hist <- hist(prism.data,breaks=breaks,plot=F)
   pnw.hist <- hist(pnw.data,breaks=breaks,plot=F)
   aplin.hist <- hist(aplin.data,breaks=breaks,plot=F)
   daymet.hist <- hist(daymet.data,breaks=breaks,plot=F)
   worldclim.hist <- hist(worldclim.data,breaks=breaks,plot=F)
   era5land.hist <- hist(era5land.data,breaks=breaks,plot=F)

   rv <- list(aplin=aplin.hist,prism=prism.hist,pnw=pnw.hist,daymet=daymet.hist,
              worldclim=worldclim.hist,era5land=era5land.hist,bounds=c(min.breaks,max.breaks))
   return(rv)
}


##*****************************************************************************************
##-----------------------------------------------------------

plot.dir <- '/storage/data/projects/rci/data/cas/gridded_observations/'

ctitles <- c('PNWNAmet','ANUSPLIN','Daymet','WorldClim','ERA5-Land')
compares <- c('pnw','aplin','daymet','worldclim','era5land')

var.name <- 'tasmin'

if (var.name == 'pr') {
   main.title <- 'Precip'
   xtitle <- 'Precip (mm)'
   var.col <- 'blue'
   ann.sep <- 250
   ann.breaks <- seq(0,7000,ann.sep)
   ann.xlim <- c(0,6700)
   win.sep <- 100
   win.breaks <- seq(0,3600,win.sep)
   win.xlim <- c(0,2300)
   spr.sep <- 50
   spr.breaks <- seq(0,3600,spr.sep)
   spr.xlim <- c(0,1500)
   sum.sep <- 50
   sum.breaks <- seq(0,2200,sum.sep)
   sum.xlim <- c(0,1200)
   fall.sep <- 100
   fall.breaks <- seq(0,4600,fall.sep)
   fall.xlim <- c(0,2600)
}

if (var.name == 'tasmax') {
   main.title <- 'Max Temp.'
   xtitle <- 'Temp. (\u00B0C)'
   var.col <- 'red'
   ann.sep <- 1
   ann.breaks <- seq(-16,20,ann.sep)
   ann.xlim <- c(-10,16)
   win.sep <- 1
   win.breaks <- seq(-24,16,win.sep)
   win.xlim <- c(-24,10)
   spr.sep <- 1
   spr.breaks <- seq(-16,24,spr.sep)
   spr.xlim <- c(-10,20)
   sum.sep <- 1
   sum.breaks <- seq(-5,30,sum.sep)
   sum.xlim <- c(0,30)
   fall.sep <- 1
   fall.breaks <- seq(-16,20,fall.sep)
   fall.xlim <- c(-12,16)
}


if (var.name == 'tasmin') {
   main.title <- 'Min Temp.'
   xtitle <- 'Temp. (\u00B0C)'
   var.col <- 'orange'
   ann.sep <- 1
   ann.breaks <- seq(-26,10,ann.sep)
   ann.xlim <- c(-15,12)
   win.sep <- 1
   win.breaks <- seq(-34,6,win.sep)
   win.xlim <- c(-34,6)
   spr.sep <- 1
   spr.breaks <- seq(-26,14,spr.sep)
   spr.xlim <- c(-15,10)
   sum.sep <- 1
   sum.breaks <- seq(-15,20,sum.sep)
   sum.xlim <- c(-6,20)
   fall.sep <- 1
   fall.breaks <- seq(-24,16,fall.sep)
   fall.xlim <- c(-20,14)
}


   annual.data <- read_regridded_obs_data(var.name,clim='annual',seas='Annual',mon=1,agg,breaks=ann.breaks)
   winter.data <- read_regridded_obs_data(var.name,clim='seasonal',seas='Winter',mon=1,agg,breaks=win.breaks) 
   spring.data <- read_regridded_obs_data(var.name,clim='seasonal',seas='Spring',mon=2,agg,breaks=spr.breaks) 
   summer.data <- read_regridded_obs_data(var.name,clim='seasonal',seas='Summer',mon=3,agg,breaks=sum.breaks) 
   fall.data <- read_regridded_obs_data(var.name,clim='seasonal',seas='Fall',mon=4,agg,breaks=fall.breaks) 


for (i in seq_along(compares)) {
   compare <- compares[i]
   ctitle <- ctitles[i]
   print(ctitle)
   plot.file <- paste0(plot.dir,var.name,'.',compare,'.prism.Difference.Histograms.at.ERA5-Land.Grid.png')

  png(file=plot.file,width=8,height=8,units='in',res=600,pointsize=6,bg='white')
  ##par(mfrow=c(3,2))
  layout(mat=rbind(c(1,2,2,3),
                   c(4,4,5,5),
                   c(6,6,7,7)))

  par(mar=c(6,5,3,3))
  plot(0,axes=FALSE,type='n',xlab='',ylab='')
  ann.x <- pretty(ann.breaks)
  ann.dense <- sort(c(annual.data$prism$density,annual.data[[compare]]$density))
  ann.y <- pretty(ann.dense*ann.sep)
  cx <- 2
  plot(annual.data$prism,border='black',col=alpha('black',0.2),freq=F,axes=F,yaxs='i',xaxs='i',
       xlab=xtitle,ylab='Density',main=paste0('Annual ',main.title),
       cex.axis=cx,cex=cx,cex.lab=cx,cex.main=cx,
       xlim=ann.xlim,ylim=c(0,max(ann.dense)+0.01*max(ann.dense))) 
  plot(annual.data[[compare]],border=var.col,col=alpha(var.col,0.2),freq=F,add=T)
  axis(1,at=ann.x,label=ann.x,cex.axis=cx)
  axis(2,at=ann.y,label=ann.y,cex.axis=cx)
  legend('topright',leg=c('PRISM',ctitle),col=c('black',var.col),
          pt.bg=alpha(c('black',var.col),0.2),pt.cex=5,cex=2,pch=22)
  box(which='plot')
  plot(0,axes=FALSE,type='n',xlab='',ylab='')

##------------------
  win.x <- pretty(win.breaks)
  win.dense <- sort(c(winter.data$prism$density,winter.data[[compare]]$density))

  plot(winter.data$prism,border='black',col=alpha('black',0.2),freq=F,axes=F,yaxs='i',xaxs='i',
       xlab=xtitle,ylab='Density',main=paste0('Winter ',main.title),
       cex.axis=cx,cex=cx,cex.lab=cx,cex.main=cx,
       xlim=win.xlim,ylim=c(0,max(win.dense)+0.01*max(win.dense))) 
  plot(winter.data[[compare]],border=var.col,col=alpha(var.col,0.2),freq=F,add=T)
  axis(1,at=win.x,label=win.x,cex.axis=cx)
  axis(2,at=pretty(win.dense),label=pretty(win.dense)*win.sep,cex.axis=cx)
  legend('topright',leg=c('PRISM',ctitle),col=c('black',var.col),
          pt.bg=alpha(c('black',var.col),0.2),pt.cex=5,cex=2,pch=22)
  box(which='plot')

##------------------
  spr.x <- pretty(spr.breaks) ##[1:30])
  spr.dense <- sort(c(spring.data$prism$density,spring.data[[compare]]$density))

  plot(spring.data$prism,border='black',col=alpha('black',0.2),freq=F,axes=F,yaxs='i',xaxs='i',
       xlab=xtitle,ylab='Density',main=paste0('Spring ',main.title),
       cex.axis=cx,cex=cx,cex.lab=cx,cex.main=cx,
       xlim=spr.xlim,ylim=c(0,max(spr.dense)+0.01*max(spr.dense))) 
  plot(spring.data[[compare]],border=var.col,col=alpha(var.col,0.2),freq=F,add=T)
  axis(1,at=spr.x,label=spr.x,cex.axis=cx)
  axis(2,at=pretty(spr.dense),label=pretty(spr.dense)*spr.sep,cex.axis=cx)
  legend('topright',leg=c('PRISM',ctitle),col=c('black',var.col),
          pt.bg=alpha(c('black',var.col),0.2),pt.cex=5,cex=2,pch=22)
  box(which='plot')

##------------------
  sum.x <- pretty(sum.breaks) ##[1:20])
  sum.dense <- sort(c(summer.data$prism$density,summer.data[[compare]]$density))

  plot(summer.data$prism,border='black',col=alpha('black',0.2),freq=F,axes=F,yaxs='i',xaxs='i',
       xlab=xtitle,ylab='Density',main=paste0('Summer ',main.title),
       cex.axis=cx,cex=cx,cex.lab=cx,cex.main=cx,
       xlim=sum.xlim,ylim=c(0,max(sum.dense)+0.01*max(sum.dense))) 
  plot(summer.data[[compare]],border=var.col,col=alpha(var.col,0.2),freq=F,add=T)
  axis(1,at=sum.x,label=sum.x,cex.axis=cx)
  axis(2,at=pretty(sum.dense),label=pretty(sum.dense)*sum.sep,cex.axis=cx)
  legend('topright',leg=c('PRISM',ctitle),col=c('black',var.col),
          pt.bg=alpha(c('black',var.col),0.2),pt.cex=5,cex=2,pch=22)
  box(which='plot')

##------------------
  fall.x <- pretty(fall.breaks)
  fall.dense <- sort(c(fall.data$prism$density,fall.data[[compare]]$density))

  plot(fall.data$prism,border='black',col=alpha('black',0.2),freq=F,axes=F,yaxs='i',xaxs='i',
       xlab=xtitle,ylab='Density',main=paste0('Fall ',main.title),
       cex.axis=cx,cex=cx,cex.lab=cx,cex.main=cx,
       xlim=fall.xlim,ylim=c(0,max(fall.dense)+0.01*max(fall.dense))) 
  plot(fall.data[[compare]],border=var.col,col=alpha(var.col,0.2),freq=F,add=T)
  axis(1,at=fall.x,label=fall.x,cex.axis=cx)
  axis(2,at=pretty(fall.dense),label=pretty(fall.dense)*fall.sep,cex.axis=cx)
  legend('topright',leg=c('PRISM',ctitle),col=c('black',var.col),
          pt.bg=alpha(c('black',var.col),0.2),pt.cex=5,cex=2,pch=22)
  box(which='plot')


  dev.off()
}