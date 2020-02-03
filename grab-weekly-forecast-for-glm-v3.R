if (!"rNOMADS" %in% installed.packages()) install.packages("rNOMADS")
if (!"RCurl" %in% installed.packages()) install.packages("RCurl")
if (!"stringr" %in% installed.packages()) install.packages("stringr")

library(rNOMADS)
library(RCurl)
library(stringr)

#User defined location of interest and directory

lake_lat_n_list = c(37.27, 29.688149, 43.392869, 46.2106879) 
lake_lon_w_list = c(79.9, 82.017318, 72.058199, 89.4739973) 
#Degrees west (does not currently work for sites in eastern hemisphere)

lake_name_list = c("FCRE","SUGG","SUNP","CRAM")

directory = '/data/SCCData/NOAA/'
if(!file.exists(directory)) {
  dir.create(file.path(directory))
}

for(subdir in lake_name_list) {
  if(!file.exists(file.path(directory, subdir))) {
    dir.create(file.path(directory, subdir))
  }
}

#####

time <- c(0, 64) #6 hour prediction for 16 days
lon.dom <- seq(0, 360, by = 1) #domain of longitudes in model
lat.dom <- seq(-90, 90, by = 1) #domain of latitudes in model

for(lake_index in 1:length(lake_name_list)){
  
  lon <- which.min(abs(lon.dom  - (360 - lake_lon_w_list[lake_index]))) - 1 #NOMADS indexes start at 0
  lat <- which.min(abs(lat.dom - lake_lat_n_list[lake_index])) - 1 #NOMADS indexes start at 0 
  
  #Get yesterdays 6 am GMT, 12 pm GMT, 6 pm GMT, and todays 12 an GMT runs
  urls.out <- GetDODSDates(abbrev = "gens_bc")
  
  for(i in 1:length(urls.out$url)){
    
    model.url <- urls.out$url[i]
    model.url <- str_replace(model.url,"gens_bc","gens")
    run_date <- urls.out$date[i]
    model_list <- c('gep_all_00z', 'gep_all_06z', 'gep_all_12z', 'gep_all_18z')
    
    for(m in 1:length(model_list)){
      
      file_present_local <- file.exists(paste0(directory, lake_name_list[lake_index], '/', lake_name_list[lake_index], '_', run_date, '_', model_list[m], '.csv'))
      print(paste0(directory, lake_name_list[lake_index], '/', lake_name_list[lake_index], '_', run_date, '_', model_list[m], '.csv is already downloaded: ', file_present_local))
      
      #Check if already downloaded
      if(!file_present_local){
        
        model.runs <- GetDODSModelRuns(model.url)
        #check if avialable at NOAA
        if(model_list[m] %in% model.runs$model.run){
          
          model.run <- model.runs$model.run[which(model.runs$model.run == model_list[m])]
          #Get variables of interest for GLM
          
          #tmp2m #temp at 2 m
          
          #dlwrfsfc #surface downward long-wave rad. flux [w/m^2]
          
          #dswrfsfc #surface downward short-wave rad. flux [w/m^2]
          
          #pratesfc #surface precipitation rate [kg/m^2/s]
          
          #rh2m #2 m above ground relative humidity [%] 
          
          #vgrd10m  #10 m above ground v-component of wind [m/s] 
          
          #ugrd10m #10 m above ground u-component of wind [m/s] 
          
          #spfh2m #2 m above specific humidity  [kg/kg] 
          
          #pressfc #Surface pressure [pa]
          
          #tcdcclm #entire atmosphere total cloud cover [%]
          
          tmp2m <- DODSGrab(model.url, model.run, "tmp2m", time = time, lon = c(lon,lon), 
                            lat = c(lat,lat),ensembles=c(0,20))
          
          dlwrfsfc <- DODSGrab(model.url, model.run, "dlwrfsfc", time = time, lon = c(lon,lon), 
                               lat = c(lat,lat),ensembles=c(0,20))
          
          dswrfsfc <- DODSGrab(model.url, model.run, "dswrfsfc", time = time, lon = c(lon,lon), 
                               lat = c(lat,lat),ensembles=c(0,20))
          
          pratesfc <- DODSGrab(model.url, model.run, "pratesfc", time = time, lon = c(lon,lon), 
                               lat = c(lat,lat),ensembles=c(0,20))
          
          rh2m <- DODSGrab(model.url, model.run, "rh2m", time = time, lon = c(lon,lon), 
                           lat = c(lat,lat),ensembles=c(0,20))
          
          vgrd10m <- DODSGrab(model.url, model.run, "vgrd10m", time = time, lon = c(lon,lon), 
                              lat = c(lat,lat),ensembles=c(0,20))
          
          ugrd10m <- DODSGrab(model.url, model.run, "ugrd10m", time = time, lon = c(lon,lon), 
                              lat = c(lat,lat),ensembles=c(0,20))
          
          spfh2m <- DODSGrab(model.url, model.run, "spfh2m", time = time, lon = c(lon,lon), 
                             lat = c(lat,lat),ensembles=c(0,20))
          
          pressfc <- DODSGrab(model.url, model.run, "pressfc", time = time, lon = c(lon,lon), 
                              lat = c(lat,lat),ensembles=c(0,20))
          
          tcdcclm <- DODSGrab(model.url, model.run, "tcdcclm", time = time, lon = c(lon,lon), 
                              lat = c(lat,lat),ensembles=c(0,20))
          
          forecast.time <- strftime(tmp2m$forecast.date, format="%Y-%m-%d %H:%M:%S",tz = 'GMT')
          
          forecast_noaa <- data.frame(forecast.date = forecast.time, 
                                      ensembles = tmp2m$ensembles, 
                                      tmp2m = tmp2m$value, 
                                      dlwrfsfc= dlwrfsfc$value, 
                                      dswrfsfc = dswrfsfc$value, 
                                      pratesfc = pratesfc$value, 
                                      rh2m = rh2m$value, 
                                      vgrd10m = vgrd10m$value, 
                                      ugrd10m = ugrd10m$value, 
                                      spfh2m = spfh2m$value, 
                                      pressfc = pressfc$value,
                                      tcdcclm = tcdcclm$value)
          
          write.csv(forecast_noaa, paste0(directory, lake_name_list[lake_index], "/", lake_name_list[lake_index], "_", run_date, "_", model_list[m], '.csv'), row.names = FALSE)
        }
      }
    }
  }
}

