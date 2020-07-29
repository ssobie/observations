##Script to download the EUSTACE data

library(ncdf4)


##--------------------------------------------------------

make_annual_netcdf_file <- function(var.name,var.atts,
                                    template.file,write.file,tmp.dir,
                                    year,lonc,latc) {

   tnc <- nc_open(paste0(tmp.dir,template.file))
   t.lon <- ncvar_get(tnc,'longitude')
   t.lat <- ncvar_get(tnc,'latitude')
   lon.atts <- ncatt_get(tnc,'longitude')
   lat.atts <- ncatt_get(tnc,'latitude')
   global.atts <- ncatt_get(tnc,0)

   lon.ix <- t.lon >= lonc$st & t.lon <= lonc$en
   lat.ix <- t.lat >= latc$st & t.lat <= latc$en
   lon.sub <- t.lon[lon.ix]
   lat.sub <- t.lat[lat.ix]

   year.dates <- seq(from=as.Date(paste0(year,'-01-01')),by='day',to=as.Date(paste0(year,'-12-31')))
   time.values <- as.numeric(year.dates - as.Date('1850-01-01'))

   x.geog <- ncdim_def('lon', 'degrees_east', lon.sub)
   y.geog <- ncdim_def('lat', 'degrees_north', lat.sub)
   t.geog <- ncdim_def('time', 'days since 1850-01-01 00:00:00', time.values,
                       unlim=FALSE, calendar='proleptic_gregorian')
   var.geog <- ncvar_def(var.name, units=var.atts$units, dim=list(x.geog, y.geog, t.geog),
                          missval=var.atts$missval,prec='float')
   write.nc <- nc_create(paste0(tmp.dir,write.file), var.geog)

   lon.names <- names(lon.atts)
   for (j in 1:length(lon.atts))
      ncatt_put(write.nc,varid='lon',attname=lon.names[j],attval=lon.atts[[j]])

   lat.names <- names(lat.atts)
   for (j in 1:length(lat.atts))
      ncatt_put(write.nc,varid='lat',attname=lat.names[j],attval=lat.atts[[j]])

   
   var.names <- names(var.atts)
   for (j in 1:length(var.atts))
      ncatt_put(write.nc,varid=var.name,attname=var.names[j],attval=var.atts[[j]])
   ncatt_put(write.nc,varid=var.name,attname='_FillValue',attval=var.atts$missval)

   global.names <- names(global.atts)
   for (g in 1:length(global.atts))
      ncatt_put(write.nc,varid=0,attname=global.names[g],attval=global.atts[[g]])
   ncatt_put(write.nc,varid=0,attname='domain',attval='North America')
   ncatt_put(write.nc,varid=0,attname='history',attval='')

   ncvar_put(write.nc,'lon',lon.sub)
   ncvar_put(write.nc,'lat',lat.sub)

   nc_close(tnc)
   rv <- list(var=var.name,nc=write.nc,lonx=lon.ix,latx=lat.ix,dates=year.dates)
   return(rv)

}


##--------------------------------------------------------
##Insert data into annual netcdf

insert_annual_data <- function(ann.info,year.files,
                               tmp.dir) {
   nc <- ann.info$nc

   lon.st <- head(which(ann.info$lonx),1)
   lon.cnt <- tail(which(ann.info$lonx),1) -  head(which(ann.info$lonx),1) + 1
   lat.st <- head(which(ann.info$latx),1)
   lat.cnt <- tail(which(ann.info$latx),1) -  head(which(ann.info$latx),1) + 1

   year.dates <- ann.info$dates
   for (i in seq_along(year.dates)) {
      fdate <- format(year.dates[i],'%Y%m%d')
      file <- year.files[grep(fdate,year.files)]
      ync <- nc_open(paste0(tmp.dir,file))
      year.input <- ncvar_get(ync,ann.info$var,start=c(lon.st,lat.st,1),
                              count=c(lon.cnt,lat.cnt,-1))
      ncvar_put(nc,ann.info$var,year.input,start=c(1,1,i),count=c(-1,-1,1))
      nc_close(ync)
   }  
   return(ann.info)
}

##--------------------------------------------------------
##Download EUSTACE file

download_eustace_file <- function(date,write.dir) {
  E.path <- "http://dap.ceda.ac.uk/thredds/fileServer/neodc/eustace/data/combined/mohc/eustace/v1.0/day/0/0/R001400/20190326/global/"
  year <- format(date,'%Y')
  fdate <- format(date,'%Y%m%d')
  path <- paste0(E.path,year,'/')
  file <- paste0("tas_global_eustace_0_",fdate,".nc")
  dest <- paste0(write.dir,file)
  url <- paste0(path,file)
  download.file(url,destfile=dest)
}


##--------------------------------------------------------
##********************************************************
var.atts <- list(long_name = "Average daily surface air temperature",
                 standard_name = "air_temperature",
                 cell_methods = "time: mean",
                 units = "degC",
                 missval=32767)

lonc <- list(st=-169,en=-51)
latc <- list(st=24,en=90)

tmp.dir <- '/local_temp/ssobie/eustace/'
if (!dir.exists(tmp.dir)) {
   dir.create(tmp.dir,recursive=TRUE)
}
read.dir <- "/storage/data/climate/observations/gridded/EUSTACE/incoming/"


var.name <- "tas"

years <- 2014:2015

for (year in years) {

   dates <- seq(from=as.Date(paste0(year,'-01-01')),by='day',to=as.Date(paste0(year,'-12-31')))
   fdates <- format(dates,'%Y%m%d')
   write.dir <- tmp.dir ##"/storage/data/climate/observations/gridded/EUSTACE/"
   for (i in seq_along(dates)) {
      print(dates[i])
      result <- 1
      while (result !=0) {
         result <- download_eustace_file(dates[i],write.dir)
      }
   }

   write.file <- paste0(var.name,"_day_north_america_eustace_0_",year,".nc")
   write.dir <- "/storage/data/climate/downscale/BCCAQ2+PRISM/CMIP5/EUSTACE/tas/" ###Temporary until space issue resolved
   ###paste0("/storage/data/climate/observations/gridded/EUSTACE/",var.name,'/')

   year.files <- list.files(path=tmp.dir,pattern=paste0('0_',year))

   template.file <- year.files[1]
   ##file.copy(from=paste0(read.dir,template.file),to=tmp.dir,overwrite=TRUE)

   ann.info <- make_annual_netcdf_file(var.name,var.atts,
                                    template.file,write.file,tmp.dir,
                                    year,lonc,latc)

   print(paste0('Finished creating file for ',year))
   ##file.copy(from=paste0(read.dir,year.files),to=tmp.dir,overwrite=TRUE)
   print('Copied year files to temp')
   input.info <- insert_annual_data(ann.info,year.files,
                                 tmp.dir)
   print('Added year data to netcdf')
   nc_close(input.info$nc)

   file.copy(from=paste0(tmp.dir,write.file),to=write.dir,overwrite=TRUE)

   file.remove(paste0(tmp.dir,year.files))
   file.remove(paste0(tmp.dir,write.file))
}






if (1==0) {

}
