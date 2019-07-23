##Script to convert incoming ASCII values for ANUSPLIN 2km into netcdf

library(raster)
library(ncdf4)


make_year_file <- function(var.name,files,year,dates,
                           tmp.dir) {

  write.file <- paste0(var.name,"_monthly_ANUSPLIN_60ARCSEC_",year,".nc")

  r.list <- vector(mode='list',length=length(files))

  for (i in seq_along(files)) {
    r.list[[i]] <- raster(paste0(tmp.dir,files[i]))
  }

  r.stack <- stack(r.list)
  r.array <- as.array(r.stack)
  r.perm <- aperm(r.array,c(2,1,3))
  d2 <- dim(r.perm)[2]
  r.flip <- r.perm[,d2:1,]

  ##Coordinates
  r1 <- coordinates(r.list[[1]])
  lon <- sort(unique(r1[,1]))
  lat <- sort(unique(r1[,2]))

  time.units <- 'days since 1850-01-01'
  time.vals <- dates - as.Date('1850-01-01')
  time.calendar <- 'gregorian'
  ##--------------------------------------------------------------
  ##Create new netcdf file
  x.geog <- ncdim_def('lon', 'degrees_east', lon)
  y.geog <- ncdim_def('lat', 'degrees_north', lat)
  t.geog <- ncdim_def('time', time.units, as.numeric(time.vals),
                        unlim=TRUE, calendar=time.calendar)
  var.units <- 'degC'
  var.geog <- ncvar_def(var.name, units=var.units, dim=list(x.geog, y.geog, t.geog),
                        missval=-32768)
  file.nc <- nc_create(paste0(tmp.dir,write.file), var.geog)

  ncvar_put(file.nc,varid=var.name,vals=r.flip,
                    start=c(1,1,1),count=c(-1,-1,-1))
  nc_close(file.nc)
  return(write.file)
}

##-------------------------------------------------------------------------------------

##Test first with one year

proj.dir <- "/storage/data/climate/observations/gridded/incoming/anusplin_2km_monthly/"
write.dir <- "/storage/data/climate/observations/gridded/incoming/anusplin_2km_monthly/netcdf/"
tmp.dir <- '/local_temp/ssobie/aplin/'
if(!file.exists(tmp.dir)) {
  dir.create(tmp.dir,recursive=TRUE)
}

years <- 1952:2015
var.name <- "tasmax"
aplin.prefix <- "maxt60_"


for (yr in years) {
  yr.dir <- paste0(proj.dir,yr,'/')
  year.dates <- seq(from=as.Date(paste0(yr,'-01-01')),by='month',to=as.Date(paste0(yr,'-12-31')))
  print(yr)
  files <- sort(list.files(path=yr.dir,pattern='asc'))

  file.copy(from=paste0(yr.dir,files),to=tmp.dir)

  nc.file <- make_year_file(var.name,files,yr,year.dates,
                            tmp.dir)

  file.copy(from=paste0(tmp.dir,nc.file),to=write.dir,overwrite=TRUE)  
                 
}




