if (!"rNOMADS" %in% installed.packages()) install.packages("rNOMADS")

library(rNOMADS)



#Define location of interest

time <- c(0,64) #6 hour prediction for 14 days

lake_lat_n = 37.27

lake_lon_w = 79.9

lon.dom <- seq(0, 360, by = 1) #domain of longitudes in model

lat.dom <- seq(-90, 90, by = 1) #domain of latitudes in model

lon <- which.min(abs(lon.dom  - (360 - lake_lon_w))) - 1 #NOMADS indexes start at 0

lat <- which.min(abs(lat.dom - lake_lat_n)) - 1 #NOMADS indexes start at 0 



directory = '/home/scc/data/SCCData/NOAA/'



#Get yesterdays 6 am GMT, 12 pm GMT, 6 pm GMT, and todays 12 an GMT runs

urls.out <- GetDODSDates(abbrev = "gens")

model.url <- tail(urls.out$url, 2)[1] 

run_date <- tail(urls.out$date, 2)[1]

model_list <- c('gep_all_06z','gep_all_12z','gep_all_18z','gep_all_00z')

run_date <- c(tail(urls.out$date, 2)[1],tail(urls.out$date, 2)[1],tail(urls.out$date, 2)[1],tail(urls.out$date, 2)[2])

url  <- c(tail(urls.out$url, 2)[1],tail(urls.out$url, 2)[1],tail(urls.out$url, 2)[1],tail(urls.out$url, 2)[2])



for(i in 1:4){

  model.runs <- GetDODSModelRuns(url[i])

  model.run <- model.runs$model.run[which(model.runs$model.run == model_list[i])]

  

  #Get variables of interest for GLM

  #tmp2m #temp at 2 m

  #dlwrfsfc #surface downward long-wave rad. flux [w/m^2]

  #dswrfsfc #surface downward short-wave rad. flux [w/m^2]

  #pratesfc #surface precipitation rate [kg/m^2/s]

  #rh2m #2 m above ground relative humidity [%] 

  #vgrd10m  #10 m above ground v-component of wind [m/s] 

  #ugrd10m #10 m above ground u-component of wind [m/s] 

  

  tmp2m <- DODSGrab(url[i], model.run, "tmp2m", time = time, lon = c(lon,lon), 

                    lat = c(lat,lat),ensembles=c(0,20))

  dlwrfsfc <- DODSGrab(url[i], model.run, "dlwrfsfc", time = time, lon = c(lon,lon), 

                       lat = c(lat,lat),ensembles=c(0,20))

  dswrfsfc <- DODSGrab(url[i], model.run, "dswrfsfc", time = time, lon = c(lon,lon), 

                       lat = c(lat,lat),ensembles=c(0,20))

  pratesfc <- DODSGrab(url[i], model.run, "pratesfc", time = time, lon = c(lon,lon), 

                       lat = c(lat,lat),ensembles=c(0,20))

  rh2m <- DODSGrab(url[i], model.run, "rh2m", time = time, lon = c(lon,lon), 

                   lat = c(lat,lat),ensembles=c(0,20))

  vgrd10m <- DODSGrab(url[i], model.run, "vgrd10m", time = time, lon = c(lon,lon), 

                      lat = c(lat,lat),ensembles=c(0,20))

  ugrd10m <- DODSGrab(url[i], model.run, "ugrd10m", time = time, lon = c(lon,lon), 

                      lat = c(lat,lat),ensembles=c(0,20))

  

  forecast_noaa = data.frame(forecast.date = tmp2m$forecast.date,ensembles = tmp2m$ensembles,tmp2m = tmp2m$value, dlwrfsfc= dlwrfsfc$value, dswrfsfc = dswrfsfc$value, pratesfc = pratesfc$value,

                             rh2m = rh2m$value, vgrd10m = vgrd10m$value, ugrd10m = ugrd10m$value)

  

  write.csv(forecast_noaa,paste(directory,run_date[i],model_list[i],'.csv',sep=''),row.names = FALSE)

}


