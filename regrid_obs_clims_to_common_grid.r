##Use conservative interpolation to convert the set of 
##gridded observations to ERA5-Land resolution (0.1 degree)
##which is the coarsest resolution

tmp.dir <- '/local_temp/ssobie/era5_grid/'

if(!file.exists(tmp.dir)) {
   dir.create(tmp.dir,recursive=T)
}

##----------------------------------------------------------------

regrid_file <- function(input.file,output.file,tmp.dir,
                        grid.file='/storage/home/ssobie/grid_files/bc.era5-land.grid.txt') {
   
   work <- paste0("cdo remapcon,",grid.file," ",tmp.dir,input.file,
                  " ",tmp.dir,output.file)
   system(work)

}

##---------------------------------------------------------------
##PRISM
regrid_prism <- function() {

   prism.dir <- '/storage/data/climate/observations/gridded/PRISM/'
   write.dir <- '/storage/data/climate/observations/gridded/PRISM/ERA5-Grid/'
   suffix <- "_climatology_PRISM_198101-201012.nc"
   var.names <- c('pr_annual_total','pr_seasonal_total','pr_monthly_total',
               'tasmax_annual_average','tasmax_seasonal_average','tasmax_monthly_average',
               'tasmin_annual_average','tasmin_seasonal_average','tasmin_monthly_average')   

   for (var.name in var.names) {
      input.file <- paste0(var.name,suffix)
      file.copy(from=paste0(prism.dir,input.file),to=tmp.dir)
      output.file <- gsub(pattern='_PRISM_',replacement='_PRISM_at_ERA5-Land_Grid_',input.file)
      regrid_file(input.file,output.file,tmp.dir)
      file.copy(from=paste0(tmp.dir,output.file),to=write.dir,overwrite=T)

      file.remove(paste0(tmp.dir,input.file))
      file.remove(paste0(tmp.dir,output.file))

   }
}


##---------------------------------------------------------------
##ANUSPLIN
regrid_aplin <- function() {

   aplin.dir <- '/storage/data/climate/observations/gridded/ANUSPLIN/ANUSPLIN_60ARCSEC/monthly/climatology/'
   write.dir <- '/storage/data/climate/observations/gridded/ANUSPLIN/ANUSPLIN_60ARCSEC/monthly/climatology/ERA5-Grid/'
   var.names <- c('pr_annual','pr_seasonal','pr_monthly',
               'tasmax_annual','tasmax_seasonal','tasmax_monthly',
               'tasmin_annual','tasmin_seasonal','tasmin_monthly')   
   suffix <- "_climatologies_ANUSPLIN_60ARCSEC_1981-2010.nc"

   for (var.name in var.names) {
      input.file <- paste0(var.name,suffix)
      file.copy(from=paste0(aplin.dir,input.file),to=tmp.dir)
      output.file <- gsub(pattern='_ANUSPLIN_60ARCSEC_',replacement='_ANUSPLIN_at_ERA5-Land_Grid_',input.file)
      regrid_file(input.file,output.file,tmp.dir)
      file.copy(from=paste0(tmp.dir,output.file),to=write.dir,overwrite=T)

      file.remove(paste0(tmp.dir,input.file))
      file.remove(paste0(tmp.dir,output.file))

   }
}


##---------------------------------------------------------------
##Daymet
regrid_daymet <- function() {

   daymet.dir <- '/storage/data/climate/observations/gridded/Daymet/climatologies/'
   write.dir <- '/storage/data/climate/observations/gridded/Daymet/climatologies/ERA5-Grid/'
   var.names <- c('pr_annual','pr_seasonal','pr_monthly',
               'tasmax_annual','tasmax_seasonal','tasmax_monthly',
               'tasmin_annual','tasmin_seasonal','tasmin_monthly')   
   suffix <- "_climatologies_BC_Daymet_1981_2010.nc"

   for (var.name in var.names) {
      input.file <- paste0(var.name,suffix)
      file.copy(from=paste0(daymet.dir,input.file),to=tmp.dir)
      output.file <- gsub(pattern='_Daymet_',replacement='_Daymet_at_ERA5-Land_Grid_',input.file)
      regrid_file(input.file,output.file,tmp.dir)
      file.copy(from=paste0(tmp.dir,output.file),to=write.dir,overwrite=T)

      file.remove(paste0(tmp.dir,input.file))
      file.remove(paste0(tmp.dir,output.file))

   }
}


##---------------------------------------------------------------
##PNWNAmet
regrid_pnwnamet <- function() {

   write.dir <- '/storage/data/climate/observations/gridded/PNWNAmet/Derived/ERA5-Grid/'
   var.names <- c('pr_annual_total','pr_seasonal_total','pr_monthly_total',
               'tasmax_annual_average','tasmax_seasonal_average','tasmax_monthly_average',
               'tasmin_annual_average','tasmin_seasonal_average','tasmin_monthly_average')   
   suffix <- "_climatology_PNWNAmet_observations_1981-2010.nc"

   for (var.name in var.names) {
      type <- strsplit(var.name,'_')[[1]][2]
      pnwnamet.dir <- paste0('/storage/data/climate/observations/gridded/PNWNAmet/Derived/',type,'/climatologies/')

      input.file <- paste0(var.name,suffix)
      file.copy(from=paste0(pnwnamet.dir,input.file),to=tmp.dir)
      output.file <- gsub(pattern='_PNWNAmet_',replacement='_PNWNAmet_at_ERA5-Land_Grid_',input.file)
      regrid_file(input.file,output.file,tmp.dir)
      file.copy(from=paste0(tmp.dir,output.file),to=write.dir,overwrite=T)

      file.remove(paste0(tmp.dir,input.file))
      file.remove(paste0(tmp.dir,output.file))

   }
}


##---------------------------------------------------------------
##WorldClim
regrid_worldclim <- function() {

   worldclim.dir <- '/storage/data/climate/observations/gridded/world_clim/'
   write.dir <- '/storage/data/climate/observations/gridded/world_clim/ERA5-Grid/'
   var.names <- c('pr_annual','pr_seasonal','pr_monthly',
               'tasmax_annual','tasmax_seasonal','tasmax_monthly',
               'tasmin_annual','tasmin_seasonal','tasmin_monthly')   
   suffix <- "_climatologies_British_Columbia_WorldClim_1970_2000.nc"

   for (var.name in var.names) {
      input.file <- paste0(var.name,suffix)
      file.copy(from=paste0(worldclim.dir,input.file),to=tmp.dir)
      output.file <- gsub(pattern='_WorldClim_',replacement='_WorldClim_at_ERA5-Land_Grid_',input.file)
      regrid_file(input.file,output.file,tmp.dir)
      file.copy(from=paste0(tmp.dir,output.file),to=write.dir,overwrite=T)

      file.remove(paste0(tmp.dir,input.file))
      file.remove(paste0(tmp.dir,output.file))

   }
}


##---------------------------------------------------------------
##TerraClimate
regrid_terra_climate <- function() {

   terraclim.dir <- '/storage/data/climate/observations/gridded/TerraClimate/'
   write.dir <- '/storage/data/climate/observations/gridded/TerraClimate/ERA5-Grid/'
   var.names <- c('pr_annual','pr_seasonal','pr_monthly',
               'tasmax_annual','tasmax_seasonal','tasmax_monthly',
               'tasmin_annual','tasmin_seasonal','tasmin_monthly')   
   suffix <- "_climatologies_Global_TerraClimate_1981_2010.nc"

   for (var.name in var.names) {
      input.file <- paste0(var.name,suffix)
      file.copy(from=paste0(terraclim.dir,input.file),to=tmp.dir)

      sub.file <- gsub(pattern='_TerraClimate_',replacement='_TerraClimate_at_ERA5-Land_Grid_',input.file)
      output.file <- gsub(pattern='_Global_',replacement='_British_Columbia_',sub.file)
      regrid_file(input.file,output.file,tmp.dir)
      file.copy(from=paste0(tmp.dir,output.file),to=write.dir,overwrite=T)

      file.remove(paste0(tmp.dir,input.file))
      file.remove(paste0(tmp.dir,output.file))

   }
}






