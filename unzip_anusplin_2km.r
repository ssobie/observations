##Script to unzip the downloaded 2km files

proj.dir <- '/storage/data/climate/observations/gridded/ANUSPLIN/ANUSPLIN_60ARCSEC/incoming/monthly_zip_files/'


var.name <- 'tasmin'
aplin.var <- 'mint'
tmp.dir <- '/local_temp/ssobie/aplin/'

if (!file.exists(tmp.dir)) {
   dir.create(tmp.dir,recursive=TRUE)
}

zip.files <- list.files(path=proj.dir,pattern=aplin.var,full.name=T)

file.copy(from=zip.files,to=tmp.dir,overwrite=TRUE)

exdir <- paste0(tmp.dir,var.name,'_added/')
for (zip.file in zip.files) {
   print(basename(zip.file))
   file.names <- unzip(paste0(tmp.dir,basename(zip.file)),list=TRUE)
   ix <- grep('tif',file.names$Name)
   tif.file <- file.names$Name[ix]
   unzip(paste0(proj.dir,basename(zip.file)),files=tif.file,exdir=exdir)  

}

file.copy(from=paste0(tmp.dir,var.name,'_added/'),
          to='/storage/data/climate/observations/gridded/ANUSPLIN/ANUSPLIN_60ARCSEC/incoming/anusplin_2km_monthly/',
          recursive=TRUE,overwrite=TRUE)

file.remove(paste0(tmp.dir,zip.files))