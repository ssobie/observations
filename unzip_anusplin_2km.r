##Script to unzip the downloaded 2km files

proj.dir <- '/storage/data/climate/observations/gridded/incoming/'


write.dir <- '/storage/data/climate/observations/gridded/incoming/anusplin_2km_monthly/'


tmp.dir <- '/local_temp/ssobie/aplin/'

if (!file.exists(tmp.dir)) {
   dir.create(tmp.dir,recursive=TRUE)
}

##file.copy(from=proj.dir,to=tmp.dir,overwrite=TRUE,recursive=TRUE)

zip.files <- list.files(path=paste0(tmp.dir,'incoming'),pattern='arcsec.zip')
exdir <- paste0(tmp.dir,'incoming/anusplin_2km_monthly/')
for (zip.file in zip.files) {
   print(zip.file)
   file.names <- unzip(paste0(proj.dir,zip.file),list=TRUE)
   ix <- grep('asc',file.names$Name)
   asc.file <- file.names$Name[ix]
   unzip(paste0(proj.dir,zip.file),files=asc.file,exdir=exdir)
}

file.copy(from=paste0(tmp.dir,'incoming/anusplin_2km_monthly/'),
          to='/storage/data/climate/observations/gridded/incoming/',
          recursive=TRUE,overwrite=TRUE)

