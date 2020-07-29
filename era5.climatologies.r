##Extract time series of ARI and Hazards for a shapefile region and
##save the output series as RData

library(ncdf4)
library(PCICt)

source('/storage/data/projects/rci/stat.downscaling/bccaq2/code/new.netcdf.calendar.R',chdir=T)


##--------------------------------------------------------------------

make_agg_file <- function(file,tmp.dir,pattern,replacement,agg.fxn) {
   ann.file <- gsub(pattern=pattern,
                    replacement=replacement,
                    file)
   work <- paste0('cdo -O ',agg.fxn,' ',tmp.dir,file,' ',tmp.dir,ann.file)
   system(work)
   return(ann.file)
}


time_subset <- function(file,bnds,tmp.dir) {

  time.write <- gsub(pattern='[0-9]{8}-[0-9]{8}',replacement=paste0(bnds,collapse='-'),file)
  work <- paste0('cdo -O seldate,',bnds[1],'-01-01T00:00:00,',bnds[2],'-12-31T23:59:59 ',
                  tmp.dir,file,' ',tmp.dir,time.write)
  print(work)
  system(work)

  return(time.write)
}

climatology <- function(file,fxn,tmp.dir,pattern,replacement) {

  clim.file <- gsub(pattern=pattern,replacement=replacement,
                    file)

  work <- paste0('cdo -O ',fxn,' ',tmp.dir,file,' ',tmp.dir,clim.file)
  print(work)
  system(work)
  return(clim.file)
}
 
make_ann_seas_mon_totals <- function(var.name,read.dir) {

      file <- list.files(read.dir,pattern=paste0(var.name,'_day_'))

      file.copy(from=paste0(read.dir,file),to=tmp.dir,overwrite=TRUE)

      ann.file <- make_annual_max_file(file,tmp.dir,
                                       pattern=paste0(var.name,'_day'),
                                       replacement=paste0(var.name,'_annual_max'))    
      file.copy(from=paste0(tmp.dir,ann.file),to=gcm.dir,overwrite=TRUE)

      ann.file <- make_annual_min_file(file,tmp.dir,
                                       pattern=paste0(var.name,'_day'),
                                       replacement=paste0(var.name,'_annual_max'))    
      file.copy(from=paste0(tmp.dir,ann.file),to=gcm.dir,overwrite=TRUE)


      seas.file <- make_seasonal_mean_file(file,tmp.dir,
                                       pattern=paste0(var.name,'_day'),
                                       replacement=paste0(var.name,'_seasonal_mean'))        
      file.copy(from=paste0(tmp.dir,seas.file),to=gcm.dir,overwrite=TRUE)

      mon.file <- make_monthly_mean_file(file,tmp.dir,
                                       pattern=paste0(var.name,'_day'),
                                       replacement=paste0(var.name,'_monthly_mean'))         
      file.copy(from=paste0(tmp.dir,mon.file),to=gcm.dir,overwrite=TRUE)

      ann.file <- make_annual_total_file(file,tmp.dir,
                                       pattern=paste0(var.name,'_day'),
                                       replacement=paste0(var.name,'_annual_total'))       
      file.copy(from=paste0(tmp.dir,ann.file),to=gcm.dir,overwrite=TRUE)

      mon.file <- make_monthly_total_file(file,tmp.dir,
                                       pattern=paste0(var.name,'_day'),
                                       replacement=paste0(var.name,'_monthly_total'))       
      file.copy(from=paste0(tmp.dir,mon.file),to=gcm.dir,overwrite=TRUE)

      seas.file <- make_seasonal_total_file(file,tmp.dir,
                                       pattern=paste0(var.name,'_day'),
                                       replacement=paste0(var.name,'_seasonal_total'))       
      file.copy(from=paste0(tmp.dir,seas.file),to=gcm.dir,overwrite=TRUE)

   rv <- list(apr=apr.file,ann=ann.file,seas=seas.file,mon=mon.file)
   return(rv)
}


##--------------------------------------------------------------------

##args <- commandArgs(trailingOnly=TRUE)
##for(i in 1:length(args)){
##    eval(parse(text=args[[i]]))
##}

intervals <- '1981-2010'

tmp.dir <- paste0('/local_temp/ssobie/era5_clims/')

if (!dir.exists(tmp.dir)) {
   dir.create(tmp.dir,recursive=TRUE)
}

proj.dir <- '/storage/data/climate/observations/reanalysis/ERA5-Land/'

clim.dir <- paste0(proj.dir,'climatologies/')

var.name <- 'pr'
base.file <- 'pr_day_ERA5-Land_BC_19810101-20191231.nc'
file.copy(from=paste0(proj.dir,base.file),to=tmp.dir,overwrite=TRUE)

pr.ann.total <- make_agg_file(file=base.file,tmp.dir=tmp.dir,
                              pattern='pr_day',replacement='pr_annual_total',
                              agg.fxn='yearsum') 

pr.seas.total <- make_agg_file(file=base.file,tmp.dir=tmp.dir,
                              pattern='pr_day',replacement='pr_seas_total',
                              agg.fxn='seassum') 

pr.mon.total <- make_agg_file(file=base.file,tmp.dir=tmp.dir,
                              pattern='pr_day',replacement='pr_monthly_total',
                              agg.fxn='monsum') 

file.list <- c(pr.ann.total,pr.seas.total,pr.mon.total)
file.fxn <- c('timmean','yseasmean','ymonmean')


if (1==0) {
tas.ann.mean <- make_agg_file(file=base.file,tmp.dir=tmp.dir,
                              pattern='tasmin_day',replacement='tasmin_annual_mean',
                              agg.fxn='yearmean') 
tas.seas.mean <- make_agg_file(file=base.file,tmp.dir=tmp.dir,
                              pattern='tasmin_day',replacement='tasmin_seasonal_mean',
                              agg.fxn='seasmean') 
tas.mon.mean <- make_agg_file(file=base.file,tmp.dir=tmp.dir,
                              pattern='tasmin_day',replacement='tasmin_monthly_mean',
                              agg.fxn='monmean') 

file.list <- c(tas.ann.mean,tas.seas.mean,tas.mon.mean)
file.fxn <- c('timmean','yseasmean','ymonmean')
}


##Standard Climatologies
for (f in seq_along(file.list)) {
    file <- file.list[f]
    for (i in seq_along(intervals)) {
         interval <- intervals[i]
         print(interval)
         bnds <- strsplit(interval,'-')[[1]]
         time.file <- time_subset(file,bnds,tmp.dir)
         clim.file <- climatology(time.file,file.fxn[f],tmp.dir,
                                pattern='ERA5',replacement='climatology_ERA5')

         file.copy(from=paste0(tmp.dir,clim.file),to=clim.dir,overwrite=TRUE)
         file.remove(paste0(tmp.dir,time.file))
         file.remove(paste0(tmp.dir,clim.file))

   }
   file.remove(paste0(tmp.dir,file))
}

