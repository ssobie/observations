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
   
   glob.atts <- list(start_year = "1991s",
                     source = "Daymet Software Version 3.0",
                     Version_software = "Daymet Software Version 3.0",
                     Version_data = "Daymet Data Version 3.0",
                     Conventions = "CF-1.6",
                     citation = "Please see http://daymet.ornl.gov/ for current Daymet data citation information",
                     references = "Please see http://daymet.ornl.gov/ for current information on Daymet references")
   return(glob.atts)
}

make_daymet_netcdf_file <- function(var.name,tif.files,clim,
                                    read.dir,tmp.dir,
                                    write.dir,write.file,
                                    years,domain,ex=NULL) {
   var.atts <- get_var_atts(var.name)
 
   template.file <- tif.files[1]
   ##Convert tif LCC to WGS84   
   file.copy(from=paste0(read.dir,template.file),to=tmp.dir,overwrite=TRUE)
      
   template.proj <- convert_tif_projection(template.file,tmp.dir)

   if (is.null(ex)) {
      tif.temp <- brick(paste0(tmp.dir,template.proj))
   } else {
      tif.global <- brick(paste0(tmp.dir,template.proj))
      tif.temp <- crop(tif.global,ex)
   }

   coords.temp <- coordinates(tif.temp)
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
convert_tif_projection <- function(in.file,tmp.dir) {
 
   out.file <- paste0('wgs84_',in.file)

   warp <- paste0("gdalwarp -t_srs EPSG:4326 -te -180.0 10.0 -50.0 85.0 -overwrite ",tmp.dir,in.file," ",tmp.dir,out.file)
   system(warp)
   rv <- out.file
   return(out.file)
}


##--------------------------------------------------------
##Insert data into annual netcdf
insert_annual_clims <- function(proj.files,write.file,
                                nc.file,tmp.dir,ex=NULL) {
   nc <- nc.file$nc

   for (j in seq_along(proj.files)) {  
     print(paste0('Inserting file: ',j))
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
     ncvar_put(nc,nc.file$var,tif.flip,start=c(1,1,j),count=c(-1,-1,1))
   }
   return(nc.file)
}

##--------------------------------------------------------
##Insert data into annual netcdf
insert_monthly_clims <- function(proj.files,write.file,
                                 nc.file,tmp.dir,ex=NULL) {
   nc <- nc.file$nc
   mon.ix <- seq(1,length.out=length(proj.files),by=12)
   for (j in seq_along(proj.files)) {  
     print(paste0('Inserting file: ',j))
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
   }
   return(nc.file)
}

##--------------------------------------------------------
##Download Daymet file

download_daymet_file <- function(year,write.dir) {
  D.path <- "https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1343/"
  fdate <- format(date,'%Y%m%d')
  file <- paste0("daymet_v3_tmin_annavg_",year,"_na.nc4")
  dest <- paste0(write.dir,"daymet_v3_tasmin_annual_average_",year,"_north_america.nc")
  url <- paste0(D.path,file)
  download.file(url,destfile=dest)
}

download_monthly_daymet_files <- function(year,write.dir) {
  D.path <- "https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1345/"
  fdate <- format(date,'%Y%m%d')
  file <- paste0("daymet_v3_prcp_monttl_",year,"_na.nc4")
  dest <- paste0(write.dir,"daymet_v3_pr_monthly_totals_",year,"_north_america.nc")
  url <- paste0(D.path,file)
  download.file(url,destfile=dest)
}

wget_daymet_tifs <- function(years,write.dir,tmp.dir) {
   ##years <- 1980:2018
   for (year in years) {
      ##Download tifs
      result <- 1
      while (result !=0) {
         result <- download_monthly_daymet_files(year,write.dir)
      }
browser()
   }
}


##--------------------------------------------------------
##********************************************************

##BC Extent from PRISM extent
bc.ex <- extent(x=c(-141.0,-112.0),y=c(47,63))

var.name <- "tasmin"
years <- 1981:2010
write.dir <- '/storage/data/climate/observations/gridded/Daymet/'

tmp.dir <- paste0('/local_temp/ssobie/daymet/',var.name,'/')
if (!dir.exists(tmp.dir)) {
   dir.create(tmp.dir,recursive=TRUE)
}



###wget_daymet_tifs(years,write.dir,tmp.dir)

##----------------------------------------------------------------
##Assembly the annual Daymet files
if (1==0) {
read.dir <- paste0('/storage/data/climate/observations/gridded/Daymet/tif/orders/annual/',var.name,'/Daymet_V3_Annual_Climatology/data/')

##write.file <- paste0(var.name,'_annual_climatologies_north_america_Daymet_1980_2018.nc')
write.file <- paste0(var.name,'_annual_climatologies_BC_Daymet_1980_2018.nc')

tif.files <- list.files(path=read.dir,pattern='tif')
nc.file <- make_daymet_netcdf_file(var.name,tif.files,'annual',
                                   read.dir,tmp.dir,write.dir,write.file,
                                   years,domain='British Columbia',ex=bc.ex)
proj.files <- rep('F',length(tif.files))
for (i in seq_along(tif.files)) {
   daymet.tif <- tif.files[i]
   file.copy(from=paste0(read.dir,daymet.tif),to=tmp.dir,overwrite=TRUE)
   proj.files[i] <- convert_tif_projection(daymet.tif,tmp.dir)   
}


print('Copied tif file to temp')
input.info <- insert_annual_clims(proj.files,write.file,
                                  nc.file,tmp.dir,ex=bc.ex)
print('Added year data to netcdf')
nc_close(input.info$nc)

file.copy(from=paste0(tmp.dir,write.file),to=write.dir,overwrite=TRUE)
file.remove(paste0(tmp.dir,write.file))
file.remove(paste0(tmp.dir,proj.files))
file.remove(paste0(tmp.dir,tif.files))

}

##----------------------------------------------------------------
##Assembly the monthly Daymet files

if (1==1) {
read.dir <- paste0('/storage/data/climate/observations/gridded/Daymet/tif/orders/monthly/',var.name,'/')
##write.file <- paste0(var.name,'_monthly_climatologies_north_america_Daymet_1980_2018.nc')
write.file <- paste0(var.name,'_monthly_climatologies_BC_Daymet_1981_2010.nc')

tif.files <- list.files(path=read.dir,pattern='tif')
nc.file <- make_daymet_netcdf_file(var.name,tif.files,'monthly',
                                   read.dir,tmp.dir,write.dir,
                                   write.file,years,domain='British Columbia',ex=bc.ex)
proj.files <- rep('F',length(tif.files))
for (i in seq_along(tif.files)) {
   daymet.tif <- tif.files[i]
   file.copy(from=paste0(read.dir,daymet.tif),to=tmp.dir,overwrite=TRUE)
   proj.files[i] <- convert_tif_projection(daymet.tif,tmp.dir)   
}

####proj.files <- list.files(tmp.dir,pattern='wgs84_daymet_v3_prcp_monttl')

proj.years <- as.numeric( sapply(proj.files,function(x){strsplit(x,'_')[[1]][6]}))
proj.sub <- proj.years %in% years
proj.files <- proj.files[proj.sub]

print('Copied tif file to temp')
input.info <- insert_monthly_clims(proj.files,write.file,
                                   nc.file,tmp.dir,ex=bc.ex)
print('Added monthly data to netcdf')
nc_close(input.info$nc)

file.copy(from=paste0(tmp.dir,write.file),to=write.dir,overwrite=TRUE)
file.remove(paste0(tmp.dir,write.file))
file.remove(paste0(tmp.dir,proj.files))
file.remove(paste0(tmp.dir,tif.files))


}

##Use to convert daymet tifs
### gdalwarp -t_srs EPSG:4326 -te -180.0 10.0 -50.0 85.0 -overwrite daymet_v3_prcp_annttl_1993_na.tif test2.tif

