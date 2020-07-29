##Assemble WorldClim downloaded tif files into single netcdf files

library(ncdf4)
library(raster)

##--------------------------------------------------------

get_var_atts <- function(var.name) {

  tasmax.atts <- list(long_name = "Average daily maximum surface air temperature",
                    standard_name = "air_temperature",
                    cell_methods = "time: max",
                    units = "degC",
                    missval=32767)

  tasmin.atts <- list(long_name = "Average daily minimum surface air temperature",
                    standard_name = "air_temperature",
                    cell_methods = "time: min",
                    units = "degC",
                    missval=32767)

  pr.atts <- list(long_name = "Average daily total precipitation",
                  standard_name = "precipitation",
                  cell_methods = "time: sum",
                  units = "mm",
                  missval=32767)

  lon.atts <- list(standard_name="longitude",
                   long_name = "longitude",
                   units = "degrees_east",
                   axis = "X")
  lat.atts <- list(standard_name="latitude",
                   long_name = "latitude",
                   units = "degrees_north",
                   axis = "Y")

  rv <- switch(var.name,
               tasmax=tasmax.atts,tasmin=tasmin.atts,pr=pr.atts,
               lon=lon.atts,lat=lat.atts)
  return(rv)

}

get_global_atts <- function() {
   
   glob.atts <- list(start_year = "1970",
                     climatology_interval = '1970-2000',
                     source = "WorldClim.org",
                     Version_software = "WorldClim Version 2.1 April 2020",
                     Version_data = "WorldClim Version 2.1 April 2020",
                     Conventions = "CF-1.6",
                     citation = "Please see https://www.worldclim.org/data/index.html for current WorldClim data citation information",
                     references = "Please see https://www.worldclim.org/data/index.html for current information on WorldClim references")
   return(glob.atts)
}

make_worldclim_netcdf_file <- function(var.name,tif.files,clim,
                                       read.dir,tmp.dir,
                                       write.dir,write.file,
                                       domain,ex=NULL) {
                                       
   var.atts <- get_var_atts(var.name)
 
   template.file <- tif.files[1]

   file.copy(from=paste0(read.dir,template.file),to=tmp.dir,overwrite=TRUE)

   if (is.null(ex)) {
      tif.temp <- brick(paste0(tmp.dir,template.file))
      lon <- xFromCol(tif.temp,1:dim(tif.temp)[2])
      lat <- yFromRow(tif.temp,1:dim(tif.temp)[1])
   } else {
      tif.global <- brick(paste0(tmp.dir,template.file))
      tif.temp <- crop(tif.global,ex)
      coords.temp <- coordinates(tif.temp)
      lon <- sort(unique(coords.temp[,1]))
      lat <- sort(unique(coords.temp[,2]))
   }     

   if (clim=='annual') {
      time.values <- as.numeric(as.Date('1985-07-01') - as.Date('1850-01-01'))
   }
   if (clim=='monthly') {
     mon.dates <- seq(from=as.Date('1985-01-01'),by='month',to=as.Date('1985-12-01'))
     time.values <- as.numeric(mon.dates - as.Date('1850-01-01')) 
   }
   x.geog <- ncdim_def('lon', 'degrees_east', lon)
   y.geog <- ncdim_def('lat', 'degrees_north', lat)
   t.geog <- ncdim_def('time', 'days since 1850-01-01 00:00:00', time.values,
                       unlim=FALSE, calendar='proleptic_gregorian')
   var.geog <- ncvar_def(var.name, units=var.atts$units, dim=list(x.geog, y.geog, t.geog),
                          missval=var.atts$missval,prec='float')
   write.nc <- nc_create(paste0(tmp.dir,write.file), var.geog)

   lon.atts <- get_var_atts('lon')
   lon.names <- names(lon.atts)
   for (j in 1:length(lon.atts))
      ncatt_put(write.nc,varid='lon',attname=lon.names[j],attval=lon.atts[[j]])

   lat.atts <- get_var_atts('lat')
   lat.names <- names(lat.atts)
   for (j in 1:length(lat.atts))
      ncatt_put(write.nc,varid='lat',attname=lat.names[j],attval=lat.atts[[j]])
   
   var.names <- names(var.atts)
   for (j in 1:length(var.atts))
      ncatt_put(write.nc,varid=var.name,attname=var.names[j],attval=var.atts[[j]])
   ncatt_put(write.nc,varid=var.name,attname='_FillValue',attval=var.atts$missval)

   global.atts <- get_global_atts()
   global.names <- names(global.atts)
   for (g in 1:length(global.atts))
      ncatt_put(write.nc,varid=0,attname=global.names[g],attval=global.atts[[g]])
   ncatt_put(write.nc,varid=0,attname='domain',attval=domain)
   ncatt_put(write.nc,varid=0,attname='history',attval='')

   ncvar_put(write.nc,'lon',lon)
   ncvar_put(write.nc,'lat',lat)

   rv <- list(var=var.name,nc=write.nc)
   return(rv)

}

##--------------------------------------------------------
##Insert data into annual netcdf

insert_monthly_clims <- function(proj.files,nc.file,tmp.dir,ex=NULL) {
   nc <- nc.file$nc

   for (j in seq_along(proj.files)) {  
     print(paste0('Inserting file: ',j))
     if (is.null(ex)) {
        tif.brick <- brick(paste0(tmp.dir,proj.files[j]))
        for (k in (dim(tif.brick)[1]):1) {
           ##print(k)   
           tif.vector <- getValues(tif.brick,row=k)
           ncvar_put(nc,nc.file$var,tif.vector,start=c(1,k,j),count=c(-1,1,1))
        }
     } else {
        tif.global <- brick(paste0(tmp.dir,proj.files[j]))
        tif.brick <- crop(tif.global,ex)
        tif.array <- as.array(tif.brick)
        tif.perm <- aperm(tif.array,c(2,1,3))
        d2 <- dim(tif.perm)[2]
        tif.flip <- tif.perm[,d2:1,]

        ncvar_put(nc,nc.file$var,tif.flip,start=c(1,1,j),count=c(-1,-1,1))
     }
   }
   return(nc.file)
}


##--------------------------------------------------------
##********************************************************

##args <- commandArgs(trailingOnly=TRUE)
##for(i in 1:length(args)){
##    eval(parse(text=args[[i]]))
##}

##var.name <- varname
var.name <- "tasmin"

bc.ex <- extent(x=c(-141.0,-105.0),y=c(47,63))
domain <- 'British Columbia'

wc.name <- switch(var.name,
                  pr='prec',
                  tas='tavg',
                  tasmax='tmax',
                  tasmin='tmin',
                  wind='wind',
                  srad='srad',
                  vapr='vapr')

tmp.dir <- '/local_temp/ssobie/worldclim/' ##paste0(tmpdir,'/worldclim/',varname,'/')  ##
if (!dir.exists(tmp.dir)) {
   dir.create(tmp.dir,recursive=TRUE)
}

proj.dir <- '/storage/data/climate/observations/gridded/world_clim/'

read.dir <- paste0(proj.dir,'download/',wc.name,'/')
write.dir <- '/storage/data/climate/observations/gridded/world_clim/'

write.file <- paste0(var.name,'_monthly_climatologies_British_Columbia_WorldClim_1970_2000.nc')

tif.files <- list.files(path=read.dir,pattern='tif')
file.copy(from=paste0(read.dir,tif.files),to=tmp.dir,overwrite=TRUE)
mon.file <- make_worldclim_netcdf_file(var.name,tif.files,'monthly',
                                       read.dir,tmp.dir,write.dir,write.file,
                                       domain=domain,ex=bc.ex)

print('Copied tif file to temp')
input.info <- insert_monthly_clims(tif.files,
                                   mon.file,tmp.dir,ex=bc.ex)

print('Added year data to netcdf')
nc_close(input.info$nc)

file.copy(from=paste0(tmp.dir,write.file),to=write.dir,overwrite=TRUE)
file.remove(paste0(tmp.dir,write.file))
file.remove(paste0(tmp.dir,tif.files))

