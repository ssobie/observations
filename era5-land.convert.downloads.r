##Script to convert the downloaded ERA5 data into useful
##file formats and structures

library(ncdf4)
library(udunits2)
source('/storage/data/projects/rci/stat.downscaling/bccaq2/code/new.netcdf.calendar.R')

##-------------------------------------------------------------------------------------
##Uses Gregorian Calendar 
era5_time_series <- function(freq,dates) {

  calendar <- 'gregorian'
  origin <- '1950-01-01 00:00:00'
  pcict.origin <- as.PCICt(origin,cal=calendar)
  time.values <- dates-pcict.origin  

  ##Daily 
  daily.series <- format(round.PCICt(dates,freq),'%Y-%m-%d')
  daily.values <- as.numeric(time.values/86400 ) 
  daily.time <- list(calendar=calendar,
                    freq=freq,
                    units=paste0('days since ',origin),
                    long_name='time',
                    standard_name='time',
                    values=daily.values,
                    series=daily.series)

  ##Hourly 
  hourly.series <- format(round.PCICt(dates,freq),'%Y-%m-%d %H:%M:%S')
  hourly.values <- as.numeric(time.values/3600 ) 
  hourly.time <- list(calendar=calendar,
                    freq=freq,
                    units=paste0('hours since ',origin),
                    long_name='time',
                    standard_name='time',
                    values=hourly.values,
                    series=hourly.series)

  rv <- switch(freq,
               day=daily.time,
               hour=hourly.time)
  return(rv) 
}

##-------------------------------------------------------------------------------------
##Global ERA5 Attributes
get_global_atts <- function(freq) {
  global.atts <- list(institution="European Centre for Medium-Range Weather Forecasts",
                   contact="ECMWF",
                   Conventions="CF-1.6",
                   institute_id ="ECMWF",
                   domain='British Columbia',
                   creation_date=format(Sys.time(),'%Y-%m-%dT%H:%M:%S%Z'),
                   frequency=freq,
                   product="reanalysis",
                   modeling_realm="atmos",
                   project_id='ERA5-Land',
                   references="Copernicus Climate Change Service (C3S) (2017): ERA5: Fifth generation of ECMWF atmospheric reanalyses of the global climate .                         Copernicus Climate Change Service Climate Data Store (CDS), 28 March 2019. https://cds.climate.copernicus.eu/cdsapp#!/home")

}
##-------------------------------------------------------------------------------------
get_variable_units <- function(var.name) {

  units <- list(pr="kg m-2 h-1",prday="kg m-2 day-1",rhs='%',huss='kg kg-1',
                tasmax='degC',tasmin='degC',tasday='degC',tashour='degC',tasrange='degC',
                tasskew='',sp='Pa',uwind='m s-1',vwind='m s-1',dewpoint='degC',
                tcc='fraction',lcc='fraction',rain='kg m-2')             
  return(units[[var.name]])
}
##-------------------------------------------------------------------------------------

get_variable_specific_atts <- function(var.name) {

  pr.hour.atts <- list(long_name = "Hourly Total Precipitation",  
                      cell_methods = "time: sum",units=get_variable_units('pr'))
  pr.day.atts <- list(units = get_variable_units('prday'))
  tasmax.atts <- list(long_name = "Daily Maximum Near-Surface Air Temperature",
                      cell_methods = "time: maximum",units=get_variable_units('tasmax'))
  tasmin.atts <- list(long_name = "Daily Minimum Near-Surface Air Temperature",
                      cell_methods = "time: minimum",units=get_variable_units('tasmin'))
  tasday.atts <- list(long_name = "Daily Average Near-Surface Air Temperature",
                      cell_methods = "time: mean",units=get_variable_units('tasday'))
  tashour.atts <- list(long_name = "Hourly Average Near-Surface Air Temperature",
                       cell_methods = "time: mean",units=get_variable_units('tashour'))
  tasrange.atts <- list(long_name = "Daily Near-Surface Air Temperature Range",
                       cell_methods = "time: range",units=get_variable_units('tasrange'))
  tasskew.atts <- list(long_name = "Daily Near-Surface Air Temperature Skewness",
                       cell_methods = "time: skewness",units=get_variable_units('tasskew'))
  sp.atts <- list(long_name = "Surface pressure",
                       cell_methods = "time: mean",units=get_variable_units('sp'))
  rhs.atts <- list(long_name = "Relative Humidity",
                       cell_methods = "time: mean",units=get_variable_units('rhs'))
  huss.atts <- list(long_name = "Specific Humidity",
                       cell_methods = "time: mean",units=get_variable_units('huss'))
  uwind.atts <- list(long_name = "10 metre U wind component",
                     units=get_variable_units('uwind'))
  vwind.atts <- list(long_name = "10 metre V wind component",
                     units=get_variable_units('vwind'))
  dewpoint.atts <- list(long_name = "2 metre dewpoint temperature",
                        units=get_variable_units('dewpoint'))
  totalcloud.atts <- list(long_name = "Total cloud cover",
                        units=get_variable_units('totalcloud'))
  lowcloud.atts <- list(long_name = "Low cloud cover",
                        units=get_variable_units('lowcloud'))
  totalrain.atts <- list(long_name = "Total Column Rain Water",
                        units=get_variable_units('totalrain'))

  var.atts <- switch(var.name,
                     pr=pr.hour.atts,
                     prday=pr.day.atts,
                     tasmax=tasmax.atts,
                     tasmin=tasmin.atts,
                     tasday=tasday.atts,
                     tashour=tashour.atts,
                     tasrange=tasrange.atts,
                     tasskew=tasskew.atts,
                     sp=sp.atts,
                     rhs=rhs.atts,
                     huss=huss.atts,
                     uwind=uwind.atts,
                     vwind=vwind.atts,
                     dewpoint=dewpoint.atts,
                     tcc=totalcloud.atts,
                     lcc=lowcloud.atts,
                     rain=totalrain.atts)
  rv <- list(var=var.atts)
  return(rv)
}

##-------------------------------------------------------------------------------------
get_standard_atts <- function(var.name) {
  lon.atts <- list(standard_name="longitude",long_name = "longitude",
                   units = "degrees_east",axis = "X")

  lat.atts <- list(standard_name="latitude",long_name = "latitude",
                   units = "degrees_north",axis = "Y")

  pr.atts <- list(standard_name = "total_precipitation",
                  long_name = "Precipitation",
                  missing_value = 1.e+20,
                  cell_methods = "time: sum")

  tas.atts <- list(standard_name = "air_temperature",
                   missing_value = 1.e+20)

  sp.atts <- list(standard_name = "surface_pressure",
                   missing_value = 1.e+20)

  uwind.atts <- list(standard_name = "10 metre U wind component",
                   missing_value = 1.e+20)
  vwind.atts <- list(standard_name = "10 metre U wind component",
                   missing_value = 1.e+20)
  dewpoint.atts <- list(standard_name = "2 metre dewpoint temperature",
                   missing_value = 1.e+20)
  totalcloud.atts <- list(standard_name = "Cloud cover fraction",
                   missing_value = 1.e+20)
  lowcloud.atts <- list(standard_name = "Cloud cover fraction",
                   missing_value = 1.e+20)
  totalrain.atts <- list(standard_name = "Total Column Rain Water",
                   missing_value = 1.e+20)
  rhs.atts <- list(standard_name = "Relative Humidity",
                   missing_value = 1.e+20)
  huss.atts <- list(standard_name = "Specific Humidity",
                   missing_value = 1.e+20)

  var.atts <- switch(var.name,
                     pr=pr.atts,
                     tas=tas.atts,
                     sp=sp.atts,
                     rhs=rhs.atts,
                     huss=huss.atts,
                     uwind=uwind.atts,
                     vwind=vwind.atts,
                     dewpoint=dewpoint.atts,
                     totalcloud=totalcloud.atts,
                     lowcloud=lowcloud.atts,
                     totalrain=totalrain.atts)

  rv <- list(lon=lon.atts,
             lat=lat.atts,
             var=var.atts)
  return(rv)
}

add_attributes_ncdf <- function(var.info, time, nc) {

  standard.atts <- get_standard_atts(var.info$type)
  variable.atts <- get_variable_specific_atts(var.info$name)
  print('Lon names')
  lon.names <- names(standard.atts$lon)
  for (j in 1:length(standard.atts$lon))
    ncatt_put(nc,varid='lon',attname=lon.names[j],attval=standard.atts$lon[[j]])
  print('Lat names')
  lat.names <- names(standard.atts$lat)
  for (j in 1:length(standard.atts$lat))
    ncatt_put(nc,varid='lat',attname=lat.names[j],attval=standard.atts$lat[[j]])

  print('Standard names')
  var.names <- names(standard.atts$var)
  for (j in 1:length(standard.atts$var))
    ncatt_put(nc,varid=var.info$name,attname=var.names[j],attval=standard.atts$var[[j]])
  print('Variable names')
  var.names <- names(variable.atts$var)
  for (j in 1:length(variable.atts$var))
    ncatt_put(nc,varid=var.info$name,attname=var.names[j],attval=variable.atts$var[[j]])
 
  print('Time atts')
  ##Time attributes
  ncatt_put(nc,varid='time',attname='units',attval=time$units)
  ncatt_put(nc,varid='time',attname='long_name',attval=time$long_name)
  ncatt_put(nc,varid='time',attname='standard_name',attval=time$standard_name)
  ncatt_put(nc,varid='time',attname='calendar',attval=time$calendar)

  print('Global atts')
  ##Global Attributes
  global.atts <- get_global_atts(freq=time$freq)
  global.names <- names(global.atts)
  for (g in 1:length(global.atts))
    ncatt_put(nc,varid=0,attname=global.names[g],attval=global.atts[[g]])

  ##Clear extraneous history
  ncatt_put(nc,varid=0,attname='history',attval='')

}


##-------------------------------------------------------------------------------------
make_era5_netcdf <- function(var.info,base.file,dir) {

  nc <- nc_open(paste0(dir,base.file),write=FALSE)
  lon <- ncvar_get(nc,'longitude')
  lat <- ncvar_get(nc,'latitude')  

  time <- era5_time_series(var.info$freq,var.info$dates)

  ##Create new netcdf file
  x.geog <- ncdim_def('lon', 'degrees_east', lon)
  y.geog <- ncdim_def('lat', 'degrees_north', lat)
  t.geog <- ncdim_def('time', time$units, time$values,
                      unlim=FALSE, calendar=time$calendar)

  var.geog <- ncvar_def(var.info$name, units=get_variable_units(var.info$name),
                        dim=list(x.geog, y.geog,t.geog),
                        missval=1.e+20)
  new.nc <- nc_create(paste(dir,var.info$file,sep=''),var.geog)

  add_attributes_ncdf(var.info, time, new.nc)                        
  ncvar_put(new.nc,'lon',lon)
  ##Invert Lat values
  n.lat <- length(lat)
  ncvar_put(new.nc,'lat',lat[(n.lat:1)])
  
  nc_close(nc)
  nc_close(new.nc)
  
}

##-------------------------------------------------------------------------------------

make_daily_series <- function(year,input.data,agg.fxn,input.dates) {

   day.fac <- as.factor(format(input.dates,'%Y-%m-%d'))
   day.time <- as.Date(levels(day.fac)) 
   input.agg <- aperm(apply(input.data,c(1,2),function(x,y){tapply(x,y,agg.fxn)},day.fac),c(2,3,1))
   return(input.agg)
}

 
##-------------------------------------------------------------------------------------
concatenate_series <- function(var.name,year,vnc,var.info,input.data,input.dates) {
   all.dates <- var.info$dates
   freq <- var.info$freq
   ##year.dates <- seq(from=as.PCICt(paste0(year,'-01-01 00:00:00'),cal=calendar),
   ##                  by=freq,
   ##                  to=as.PCICt(paste0(year,'-12-31 23:59:59'),cal=calendar))

   yix <- which(all.dates %in% input.dates)
   yst <- head(yix,1)
   yen <- tail(yix,1)
   yct <- yen-yst+1

   ncvar_put(vnc,var.info$name,input.data,start=c(1,1,yst),count=c(-1,-1,yct))
}
 
##-------------------------------------------------------------------------------------


##*************************************************************************************

##read.dir <- '/storage/data/climate/downscale/BCCAQ2+PRISM/ERA5/'
read.dir <- '/storage/data/climate/observations/reanalysis/ERA5-Land/'

###tmpdir <- '/local_temp/ssobie/era5/'

args <- commandArgs(trailingOnly=TRUE)
for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
}

tmp.dir <- paste0(tmpdir,'/era5-land-convert/')
if (!file.exists(tmp.dir)) {
   dir.create(tmp.dir,recursive=TRUE)
}
##-------------------------------------------------------------------------------------
##From Hourly Temperature

##File for metadata attributes
base.file <- 'total_precipitation_hour_ERA5-Land_BC_1995.nc'
file.copy(from=paste0(read.dir,'downloads/',base.file),to=tmp.dir,overwrite=TRUE)

calendar <- 'gregorian'
hour.dates <- seq(from=as.PCICt('1981-01-01 01:00:00',cal=calendar),by='hour',to=as.PCICt('2019-12-31 23:59:59',cal=calendar))
day.dates <- seq(from=as.PCICt('1981-01-01',cal=calendar),by='day',to=as.PCICt('2019-12-31',cal=calendar))
years <- 1981:2019

##day.time <- era5_time_series('day',day.dates)
hour.time <- era5_time_series('hour',hour.dates)

##Create netcdf write files
var.list <- c('tasday','tasmax','tasmin') ## ,'tasrange','tasskew')'tashour') ##'tashour' ##
info.list <- list(tashour=list(name='tashour',filevar='t2m',type='tas',freq='hour',dates=hour.dates,
                  infile='2m_temperature_hour_ERA5-Land_BC',file='tas_hour_ERA5-Land_BC_19810101-20191231.nc'),  
                  tasday=list(name='tasday',filevar='t2m',type='tas',freq='day',dates=day.dates,
                  infile='2m_temperature_hour_ERA5-Land_BC',file='tas_day_ERA5-Land_BC_19810101-20191231.nc'),  
                  tasmax=list(name='tasmax',filevar='t2m',type='tas',freq='day',dates=day.dates,
                  infile='2m_temperature_hour_ERA5-Land_BC',file='tasmax_day_ERA5-Land_BC_19810101-20191231.nc'),                
                  tasmin=list(name='tasmin',filevar='t2m',type='tas',freq='day',dates=day.dates,
                  infile='2m_temperature_hour_ERA5-Land_BC',file='tasmin_day_ERA5-Land_BC_19810101-20191231.nc'),                
                  tasrange=list(name='tasrange',filevar='t2m',type='tas',freq='day',dates=day.dates,
                  infile='2m_temperature_hour_ERA5-Land_BC',file='tasrange_day_ERA5-Land_BC_19810101-20191231.nc'),
                  tasskew=list(name='tasskew',filevar='t2m',type='tas',freq='day',dates=day.dates,
                  infile='2m_temperature_hour_ERA5-Land_BC',file='tasskew_day_ERA5-Land_BC_19810101-20191231.nc'))

##var.list <- c('sp','uwind','vwind','dewpoint','totalcloud','lowcloud','totalrain')
##var.list <- 'pr'
other.list <- list(sp=list(name='sp',filevar='sp',type='sp',freq='hour',dates=hour.dates,
                                infile='surface_pressure_hour_ERA5_BC', file='surface_pressure_hour_ERA5_BC_19800101-20181231.nc'),
                  uwind=list(name='uwind',filevar='u10',type='uwind',freq='hour',dates=hour.dates,
                                infile='10m_u_component_of_wind_hour_ERA5_BC', file='uwind_hour_ERA5_BC_19800101-20181231.nc'),
                  vwind=list(name='vwind',filevar='v10',type='vwind',freq='hour',dates=hour.dates,
                                infile='10m_v_component_of_wind_hour_ERA5_BC', file='vwind_hour_ERA5_BC_19800101-20181231.nc'),
                  dewpoint=list(name='dewpoint',filevar='d2m',type='tas',freq='hour',dates=hour.dates,
                                infile='2m_dewpoint_temperature_hour_ERA5_BC', file='dewpoint_hour_ERA5_BC_19800101-20181231.nc'),
                  totalcloud=list(name='tcc',filevar='tcc',type='cloud',freq='hour',dates=hour.dates,
                                infile='total_cloud_cover_hour_ERA5_BC',file='total_cloud_cover_hour_ERA5_BC_19800101-20181231.nc'),
                  lowcloud=list(name='lcc',filevar='lcc',type='cloud',freq='hour',dates=hour.dates,
                                infile='low_cloud_cover_hour_ERA5_BC',file='low_cloud_cover_hour_ERA5_BC_19800101-20181231.nc'),
                  totalrain=list(name='rain',filevar='tcrw',type='rain',freq='hour',dates=hour.dates,
                                infile='total_column_rain_water_hour_ERA5_BC',file='total_column_rain_hour_ERA5_BC_19800101-20181231.nc'),
                  pr=list(name='pr',filevar='tp',type='pr',freq='hour',dates=hour.dates,
                                infile='total_precipitation_hour_ERA5-Land_BC', file='pr_hour_ERA5_BC_19810101-20191231.nc'),
                  rhs=list(name='rhs',filevar='r',type='rhs',freq='hour',dates=hour.dates,
                                infile='relative_humidity_hour_ERA5_BC', file='rhs_hour_ERA5_BC_19800101-20181231.nc'),
                  huss=list(name='huss',filevar='q',type='huss',freq='hour',dates=hour.dates,
                                infile='specific_humidity_hour_ERA5_BC', file='huss_hour_ERA5_BC_19800101-20181231.nc'))

                                


##----------------------------
##Create derived files
ncs.list <- vector(mode='list',length=length(var.list))
names(ncs.list) <- var.list
for (var.name in var.list) {
   print(paste0('Making file for ',var.name))
   var.info <- info.list[[var.name]]
   make_era5_netcdf(var.info,base.file=base.file,
                    dir=tmp.dir)
   ncs.list[[var.name]] <- nc_open(paste0(tmp.dir,var.info$file),write=TRUE)
 
}

##---------------------------
var.name <- 'tashour'
##for (var.name in var.list) {
  var.info <- info.list[[var.name]]
  for (year in years) {
    print(year)
    year.file <- paste0(var.info$infile,'_',year,'.nc')
    file.copy(from=paste0(read.dir,'downloads/',year.file),to=tmp.dir,overwrite=TRUE)

    ync <- nc_open(paste0(tmp.dir,year.file))
    data.raw <- ncvar_get(ync,var.info$filevar)
    input.dates <- netcdf.calendar(ync)

    if (var.info$type=='tas') {
      data.inv <- ud.convert(data.raw,'K','degC')
    } else if (var.info$type=='pr') {
      data.inv <- data.raw*1000 ##m to mm
    } else {
       data.inv <- data.raw
    }
    ##Invert tas to fix latitude order
    n.lat <- ync$dim$latitude$len
    data.fix <- data.inv[,(n.lat:1),]

    ##Concatenate Hourly temperatures
    ##concatenate_series(var.name,year,ncs.list[[var.name]],var.info,data.fix,input.dates)

    ##Calculate Daily Maximum Temperature
    day.dates <- seq(from=as.PCICt(paste0(year,'-01-01 00:00:00'),cal=calendar),
                     by='day',
                     to=as.PCICt(paste0(year,'-12-31 23:59:59'),cal=calendar))

    tasmax <- make_daily_series(year,data.fix,max,input.dates)
    concatenate_series('tasmax',year,ncs.list[['tasmax']],info.list[['tasmax']],tasmax,day.dates)
    tasmin <- make_daily_series(year,data.fix,min,input.dates)
    concatenate_series('tasmin',year,ncs.list[['tasmin']],info.list[['tasmin']],tasmin,day.dates)
    tas.day <- make_daily_series(year,data.fix,mean,input.dates)
    concatenate_series('tasday',year,ncs.list[['tasday']],info.list[['tasday']],tas.day,day.dates)
    ##tasrange <- tasmax-tasmin
    ##concatenate_series('tasrange',year,ncs.list[['tasrange']],info.list[['tasrange']],tasrange)
    ##tasskew <- (tas.day - tasmin)/tasrange
    ##concatenate_series('tasskew',year,ncs.list[['tasskew']],info.list[['tasskew']],tasskew)

    nc_close(ync)
    file.remove(paste0(tmp.dir,year.file))
  }

##}


for (var.name in var.list) {
  nc_close(ncs.list[[var.name]])
  print(info.list[[var.name]][['file']])
  file.copy(from=paste0(tmp.dir,info.list[[var.name]][['file']]),to=read.dir,overwrite=TRUE)
}





##Calculate TASMAX
##Calculate TASMIN
##Calculate TAS
##Calculate TAS RANGE
##Calculate TAS SKEW
##Concatenate daily values into single file



