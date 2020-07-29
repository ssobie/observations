##Script to download the EUSTACE data

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
                  units = "degC",
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
   
   glob.atts <- list(source = "ftp://ftp.nrcan.gc.ca/pub/outgoing/NAM_grids/",
                     Version_software = "ANUSPLIN tif version March 2020",
                     Version_data = "ANUSPLIN tif version March 2020",
                     Conventions = "CF-1.6",
                     citation = "Please see ftp://ftp.nrcan.gc.ca/pub/outgoing/NAM_grids/",
                     references = "Please see ftp://ftp.nrcan.gc.ca/pub/outgoing/NAM_grids/")
   return(glob.atts)
}

make_anusplin_netcdf_file <- function(var.name,template.file,clim,
                                      read.dir,tmp.dir,
                                      write.dir,write.file,
                                      years,domain,ex=NULL) {
   var.atts <- get_var_atts(var.name)
 
   file.copy(from=paste0(read.dir,template.file),to=tmp.dir,overwrite=TRUE)
      
   if (is.null(ex)) {
      aplin.temp <- brick(paste0(tmp.dir,template.file))
   } else {
      global.temp <- brick(paste0(tmp.dir,template.file))
      aplin.temp <- crop(global.temp,ex)
   }

   coords.temp <- coordinates(aplin.temp)
   lon <- sort(unique(coords.temp[,1]))
   lat <- sort(unique(coords.temp[,2]))

   if (clim=='annual') {
      year.dates <- as.Date(paste(years,'01-01',sep='-'))
      time.values <- as.numeric(year.dates - as.Date('1850-01-01'))
   }
   if (clim=='monthly') {
      yr <- range(years)
      mon.dates <- seq(from=as.Date(paste0(yr[1],'-01-01')),by='month',to=as.Date(paste0(yr[2],'-12-01')))
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
insert_monthly_anusplin <- function(proj.files,write.file,
                                 nc.file,read.dir,tmp.dir,ex=NULL) {


   nc <- nc.file$nc
   mon.ix <- seq(1,length.out=length(proj.files),by=12)
   for (j in seq_along(proj.files)) {  
     print(paste0('Inserting file: ',j))
     file.copy(from=paste0(read.dir,proj.files[j]),to=tmp.dir,overwrite=TRUE)
     if (is.null(ex)) {
        tif.brick <- brick(paste0(tmp.dir,proj.files[j]))
     } else {
        tif.global <- brick(paste0(tmp.dir,proj.files[j]))
        tif.brick <- crop(tif.global,ex)
     }

     tif.array <- as.array(tif.brick)
     tif.perm <- aperm(tif.array,c(2,1,3))
     d2 <- dim(tif.perm)[2]
     tif.flip <- tif.perm[,d2:1,]

     ncvar_put(nc,nc.file$var,tif.flip,start=c(1,1,mon.ix[j]),count=c(-1,-1,12))

     file.remove(paste0(tmp.dir,proj.files[j]))
   }
   return(nc.file)
}

##--------------------------------------------------------
##********************************************************

##BC Extent from PRISM extent
bc.ex <- extent(x=c(-141.0,-112.0),y=c(47,63))

var.name <- "tasmin"
years <- 1981:2010
write.dir <- '/storage/data/climate/observations/gridded/ANUSPLIN/ANUSPLIN_60ARCSEC/monthly/'

tmp.dir <- paste0('/local_temp/ssobie/aplin/',var.name,'/')
if (!dir.exists(tmp.dir)) {
   dir.create(tmp.dir,recursive=TRUE)
}
##----------------------------------------------------------------
##Assembly the monthly AUSPLIN files

read.dir <- paste0(write.dir,var.name,'/')
write.file <- paste0(var.name,'_monthly_BC_ANUSPLIN_60ARCSEC_1981_2010.nc')

nc.files <- list.files(path=read.dir,pattern=var.name)
nc.file <- make_anusplin_netcdf_file(var.name,nc.files[1],'monthly',
                                   read.dir,tmp.dir,write.dir,
                                   write.file,years,domain='British Columbia',ex=bc.ex)

proj.years <- as.numeric(
               sapply(
                sapply(nc.files,function(x){strsplit(x,'_')[[1]][5]}),
                  substr,1,4))

proj.sub <- proj.years %in% years
proj.files <- nc.files[proj.sub]



print('Copied nc file to temp')
input.info <- insert_monthly_anusplin(proj.files,write.file,
                                      nc.file,read.dir,tmp.dir,ex=bc.ex)
print('Added monthly data to netcdf')
nc_close(input.info$nc)

file.copy(from=paste0(tmp.dir,write.file),to=write.dir,overwrite=TRUE)
file.remove(paste0(tmp.dir,write.file))






