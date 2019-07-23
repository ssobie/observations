##Script to download the EUSTACE data

dates <- seq(from=as.Date('1850-01-01'),by='day',to=as.Date('2015-12-19'))
years <- format(dates,'%Y')
fdates <- format(dates,'%Y%m%d')

base.path <- "http://dap.ceda.ac.uk/thredds/fileServer/neodc/eustace/data/combined/mohc/eustace/v1.0/day/0/0/R001400/20190326/global/"

for (i in seq_along(dates)) {

  print(dates[i])

  path <- paste0(base.path,years[i],'/')
  file <- paste0("tas_global_eustace_0_",fdates[i],".nc")
  dest <- paste0("/storage/data/climate/observations/gridded/EUSTACE/",file)

  url <- paste0(path,file)
  download.file(url,destfile=dest)

}