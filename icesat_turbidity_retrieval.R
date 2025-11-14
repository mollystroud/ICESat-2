# This code will download all available ICESat-2 data from a location in the 
# Ohio River Basin
# Load in libraries
library(IceSat2R)
library(tidyr)
library(tidyverse)
library(dplyr)
library(fuzzyjoin)
library(dataRetrieval)
library(rgdal)
library(sf)
library(raster)
library(maptools)
library(geosphere)
library(parallel)
library(doParallel)
library(BBmisc)
library(foreach)
library(ranger)

################################################################################
# region of interest
bbox <- c(xmin = -87, ymin = 35, xmax = -83, ymax = 39)
# tss
param_codes <- c("63680")
################################################################################
# get gauge location in our bbox
siteno <- whatNWISsites(
  bBox = bbox,
  parameterCd = param_codes,
  hasDataTypeCd = "uv"
)
################################################################################
# get site widths
source("downloadUSGSwidth.R")
output <- lapply(siteno$site_no, downloadWidth)
for(site in 1:nrow(siteno)){
  print(site)
  width <- colMeans(data.frame(output[[site]]$w_m), na.rm = T)
  if(length(width) > 0){ # remove any NA types
    siteno$width[site] <- width
  } else {
    siteno$width[site] <- 0
  }
}
siteno <- siteno[siteno$width > 10,] # rivers greater than 10 m
################################################################################
# get all relevant dates in our region of interest
RGTdates <- revisit_time_RGTs(RGT_cycle = NULL, complete_date_sequence = T)
################################################################################
# download track data in parallel
cl <- makeCluster(14)
registerDoParallel(cl) # only run if new session
#for(gauge in 1:nrow(siteno)){
foreach(n=1:nrow(siteno), 
        .packages=c('BBmisc', 'IceSat2R', 'tidyverse')) %dopar% {
          
          gauge_bbox <- c(xmin = (siteno$dec_long_va[n] - 0.05), # within 1 km
                          ymin = (siteno$dec_lat_va[n] - 0.05), 
                          xmax = (siteno$dec_long_va[n] + 0.05), 
                          ymax = (siteno$dec_lat_va[n] + 0.05))
          dir.create(file.path(paste0('turbidity/', siteno$site_no[n]))) # create folder with name of gauge
          trackdates <- data.frame(matrix(ncol = 2, nrow = 0))
          for(cycle in RGTdates[["orbit_dates"]]) {
            # get track number in our region and date of interest
            for(date in 1:length(cycle)) {
              print(cycle[date])
              track <- getTracks(minx = as.numeric(gauge_bbox['xmin']),
                                 miny = as.numeric(gauge_bbox['ymin']),
                                 maxx = as.numeric(gauge_bbox['xmax']),
                                 maxy = as.numeric(gauge_bbox['ymax']),
                                 date = cycle[date],
                                 outputFormat = 'csv',
                                 download_method = 'internal'
              )
              trackdates <- rbind(trackdates, c(track$track[1], as.character(cycle[date])))
            } 
          }
          trackdates <- na.omit(trackdates)
          colnames(trackdates) <- c("track", "date")
          trackdates <- trackdates[!duplicated(trackdates),]
          if(nrow(trackdates != 0)){
           write_csv(trackdates, paste0('turbidity/', siteno$site_no[n], '/trackdates.csv'))
        } else {
          unlink(paste0('turbidity/', n), recursive = TRUE)
        }}
stopCluster(cl)

################################################################################
# download gauge data in parallel
cl <- makeCluster(10)
registerDoParallel(cl) # only run if new session
# for each date of interest, grab atlas data and join with usgs gauge data
beams <- c('gt1r', 'gt1l', 'gt2r', 'gt2l', 'gt3r', 'gt3l') # get each beam
foreach(n=1:nrow(siteno), 
        .packages=c('dataRetrieval', 'BBmisc', 'IceSat2R', 'tidyverse', 'fuzzyjoin', 'dplyr')) %dopar% {
          # READ IN APPROPRIATE TRACK DATES
          if(file.exists(paste0('turbidity/', siteno$site_no[n], '/trackdates.csv'))) {
          trackdates <- try(read_csv(paste0('turbidity/', siteno$site_no[n], '/trackdates.csv')))
          if(is.error(trackdates)) {next}
          trackdates$date <- as.character(trackdates$date)
          for(row in 1:nrow(trackdates)) {
            photons <- data.frame()
            for(beam in beams) {
              data <- try(get_atlas_data(minx = as.numeric(siteno$dec_long_va[n] - 0.01),
                                         miny = as.numeric(siteno$dec_lat_va[n] - 0.01),
                                         maxx = as.numeric(siteno$dec_long_va[n] + 0.01),
                                         maxy = as.numeric(siteno$dec_lat_va[n] + 0.01),
                                         date = trackdates[row, 2],
                                         trackId = trackdates[row, 1],
                                         beamName = beam,
                                         product = 'atl03',
                                         client = 'portal',
                                         photonConfidence = NULL,
                                         outputFormat = 'csv',
                                         file_path_zip = NULL,
                                         download_method = 'internal'))
              if(is.error(data)){next}
              if(nrow(data) != 0) { # if empty, move on
                photons <- rbind(photons, data)
              } 
            }

            if(nrow(photons) != 0) { # if not empty, get time data, join, get gauge data, save 
              colnames(photons) <- c('beam', 'lat', 'long', 'height', 'confidence') #, 'date', 'surfaceelev')
              # get the exact times of the icesat2 data
              timedata <- try(time_specific_orbits(date_from=as.character(trackdates[row, 2]),
                                                   date_to=as.character(trackdates[row, 2]), 
                                                   download_method='curl', 
                                                   threads=1, 
                                                   verbose=TRUE))
              if(is.error(timedata)){next}
              # extract geometry and clean
              timedata <- try(timedata %>% mutate(long = unlist(map(timedata$geometry,1)),
                                                  lat = unlist(map(timedata$geometry,2))))
              if(is.error(timedata)){next}
              timedata <- try(subset(data.frame(timedata), 
                                     select = c('Date_time', 'lat', 'long')))
              if(is.error(timedata)){next}
              # join together time data and actual photon data
              joined <- try(difference_left_join(photons[1], timedata, 
                                                 by = c('lat' = 'lat', 'long' = 'long'), 
                                                 max_dist = 1))
              if(is.error(joined)){next}
              joined$Date_time <- ymd_hm(format(joined$Date_time, format='%Y-%m-%d %H:%M'))
              photons$datetime <- joined$Date_time
              # get gauge data based on time and gauge number
              gaugedata <- try(readNWISuv(siteNumbers = siteno$site_no[n],
                                          parameterCd = param_codes,
                                          startDate = as.character(as.Date(as.character(trackdates[row, 2]))-1),
                                          endDate = as.character(trackdates[row, 2])))
              gaugedata$dateTime <- ymd_hm(format(gaugedata$dateTime, format='%Y-%m-%d %H:%M'))
              if(is.error(gaugedata)){next}
              if(nrow(gaugedata) !=0){
                # join with gauge data, get all data within 15 min
                joined <- try(difference_left_join(photons, gaugedata, 
                                                   by = c("datetime" = "dateTime"), 
                                                   max_dist = 15))
                if(is.error(joined)){next}
                # organized data
                joined <- joined %>% mutate('tss' = .[[10]])
                joined$tss <- mean(joined$tss)
                joined$track <- trackdates$track[row]
                joined <- subset(joined,
                                 select = c(beam, lat, long, height, 
                                            confidence, datetime, site_no, 
                                            tss, track))
                joined <- distinct(joined) # remove repeats
                # save out
                joined <- joined %>% drop_na(tss)
                
                if(nrow(joined != 0)) {
                  write_csv(joined, paste0('turbidity/', siteno$site_no[n], '/', trackdates$date[row], '.csv'))
                  
                }
              } else {next}
            }
          }
          }
        }
# END BIG LOOP
stopCluster(cl)


################################################################################
# get surface elevation in parallel
cl <- makeCluster(10)
registerDoParallel(cl) # only run if new session
foreach(n=1:nrow(siteno), .packages=c('BBmisc', 'IceSat2R', 'tidyverse', 'fuzzyjoin', 'dplyr')) %dopar% {
  files <- list.files(path = paste0('turbidity/', siteno$site_no[n]), pattern = '.csv')
  for(file in files) {
    data <- read_csv(paste0('turbidity/', siteno$site_no[n], '/', file))
    surfaceelev <- try(get_level3a_data(minx = as.numeric(siteno$dec_long_va[n] - 0.05),
                                        miny = as.numeric(siteno$dec_lat_va[n] - 0.05),
                                        maxx = as.numeric(siteno$dec_long_va[n] + 0.05),
                                        maxy = as.numeric(siteno$dec_lat_va[n] + 0.05),
                                        startDate = as.Date(data$datetime[n]),
                                        endDate = as.Date(data$datetime[n]),
                                        trackId = data$track[n],
                                        beamName = NULL,
                                        product = 'atl08',
                                        client = 'portal',
                                        outputFormat = 'csv',
                                        file_path_zip = NULL,
                                        download_method = 'internal'))
    if(is.error(surfaceelev)){next}
    if(nrow(surfaceelev != 0)) {
      joined <- distance_left_join(data, surfaceelev, by = c('lat' = 'latitude', 'long' = 'longitude'), max_dist = 0.001)
      out <- joined %>% dplyr::select(beam.x, lat, long, height, confidence, 
                                      datetime, site_no, tss, track, h_te_best_fit)
      write_csv(out, paste0('turbidity/', siteno$site_no[n], '/test_', file))
   # }
  }
}
}
stopCluster(cl)
################################################################################
# read in area shapefile, set projection
ohiorb_shp <- readOGR('ohio_rb_shp/ohiorb_shp.shp')
geo_proj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
watershp <- spTransform(ohiorb_shp, geo_proj)
# get data only over water bodies
cl <- makeCluster(10)
registerDoParallel(cl) # only run if new session
foreach(n=1:nrow(siteno), 
        .packages=c('BBmisc', 'tidyverse', 'sf', 'raster')) %dopar% {
          #for(gauge in 1:nrow(siteno)){
          allfiles <- list.files(path = paste0('turbidity/', siteno$site_no[n]), pattern = "test")
          if(length(allfiles) != 0){
            for(file in allfiles) {
              # read in file
              pts <- read_csv(paste0('turbidity/', siteno$site_no[n], '/', file))
              # convert to lat long
              coordinates(pts) <- ~ long + lat
              # set crs
              crs(pts) <- geo_proj
              # get points in shapefile
              matched <- over(pts, watershp)
              # read in original file again
              pts_df <- read_csv(paste0('turbidity/', siteno$site_no[n], '/', file))
              pts_df <- cbind(pts_df, matched) # bind data
              pts_df <- na.omit(pts_df) # get rid of NAs
              #pts_df <- subset(pts_df, select = -Acres ) # remove unecessary column
              pts_df <- distinct(pts_df) # remove repeats
              pts_df$beam_type <- ifelse(grepl("r", pts_df$beam), "strong", "weak") # name beams by type in separate column
              if(nrow(pts_df) !=0){ # if not empty, write out
                write_csv(pts_df, paste0('turbidity/', siteno$site_no[n], '/clipped_', file))
              }
            }
          }
        }
stopCluster(cl)
################################################################################
# split up files by beam
# r = strong, l = weak
for(gauge in 1:nrow(siteno)){
  allfiles_clipped <- list.files(path = paste0('turbidity/', siteno$site_no[gauge]), pattern = 'clipped')
  if(length(allfiles_clipped) != 0){
    for(file in allfiles_clipped) {
      data <- read_csv(paste0('turbidity/', siteno$site_no[gauge], '/', file))
      # only where height is less than surface
      data <- data[data$height <= data$h_te_best_fit, ]
      grouped <- split(data, data$beam.x)
      for(group in 1:length(grouped)) {
        write_csv(grouped[[group]], paste0('turbidity/', siteno$site_no[gauge], '/', names(grouped)[group], '_beam_', file))
      }
    }
  }
}

################################################################################
# get statistics on all paired data
siteno <- siteno[c(2, 24),]
siteno <- siteno[2,]

allstats <- data.frame()
for(gauge in 1:nrow(siteno)){
  clippedbeams <- list.files(path = paste0('turbidity/', siteno$site_no[gauge]), pattern = 'beam')
  if(length(clippedbeams) != 0){
    stats <- data.frame()
    for(file in clippedbeams) {
      data <- read_csv(paste0('turbidity/', siteno$site_no[gauge], '/', file)) # read in
      #if(max(data$confidence) < 3) next # if the confidence code are all low, skip
      data$time <- format(as.POSIXct(data$datetime), "%H:%M:%S") # date time format
      #if(data$time[1] >= "19:00:00" || data$time[1] <= "07:00:00") next # get only in day
      #if(data$time[1] <= "19:00:00" & data$time[1] >= "07:00:00") next # get only in night
      ### temporary gauge info
      discharge <- readNWISdata(sites = data$site_no[1],
                                service = "iv",
                                parameterCd = "00060",
                                startDate = as.character(as.Date(data$datetime[1] - 86400)),
                                endDate = as.character(as.Date(data$datetime[1])))
      
      dischargem <- discharge[discharge$dateTime >= data$datetime[1] - 2000 & discharge$dateTime <= data$datetime[1] + 2000,]
      print(dischargem$X_00060_00000)
      
      data <- data[data$confidence < 4, ] # only where confidence is as specified
      ## GET MEDIAN
      surfaceheight <- mean(data$h_te_best_fit) # get surface height
      data <- data[data$height < surfaceheight, ]
      
      



      if(nrow(data) < 5) next
      npoints <- nrow(data) # get number of points to normalize
      distance <- distm(c(min(data$long), min(data$lat)), c(max(data$long), max(data$lat))) # get distance
      tss <- data$tss[1]
      beam <- data$beam_type[1]
      variance <- var(data$height)
      heightdiff <- median(data$height) - min(data$height)
      minheight <- min(data$height)
      hour <- format(as.POSIXct(data$datetime), "%H")
      month <- format(as.POSIXct(data$datetime), "%m")
      fulltime <- format(as.POSIXct(data$datetime), "%H:%M:%S")
      stats <- rbind(stats, c(file, siteno$site_no[gauge], npoints, 
                              distance, tss, npoints/distance, beam, 
                              variance, heightdiff, minheight, month[1], 
                              hour[1], fulltime[1], 
                              mean(dischargem$X_00060_00000)))
    }
    colnames(stats) <- c('data', 'gauge', 'npoints', 'distance', 'tss', 
                         'normalizedpts', 'beam', 'variance', 'heightdiff',
                         'minheight', 'month', 'hour', 'fulltime', 'discharge')
    stats$normalizedpts <- as.numeric(stats$normalizedpts)
    stats$tss <- as.numeric(stats$tss)
    allstats <- rbind(stats, allstats)
  }
}

write_csv(allstats, 'allstats.csv')
allstats <- read_csv('allstats.csv')


################################################################################
# random forest
################################################################################

allstats <- read_csv('allstats.csv')
# add in widths
widths <- lapply(unique(allstats$gauge), downloadWidth)
allstats$width <- NA
for(i in 1:length(unique(allstats$gauge))) {
  width <- colMeans(data.frame(widths[[i]]$w_m), na.rm = T)
  site_no <- widths[[i]]$site_no[1]
  for(j in 1:nrow(allstats)) {
    if(allstats$gauge[j] == site_no) {
      allstats$width[j] <- width
    } else {next}
  }
}

# organize data for rf
allstats$beam <- as.factor(allstats$beam)
allstats$month <- as.numeric(allstats$month)
allstats$hour <- as.numeric(allstats$hour)
allstats <- allstats[,c(5:12, 14:15)]
allstats <- na.omit(allstats)
# average identical beams


set.seed(123) # for replicability
# create training sample partition (70 for training)
library(caret)
train <- createDataPartition(allstats, p = .7, times = 1, list = F)


rf_test <- ranger(tss ~ ., data = allstats[train,], importance = 'permutation')
rf_test

p.r <- predict(rf_test, allstats[-tr, -1],
               type = 'quantiles')

predictions <- ranger::predictions(rf_test, allstats$tss)
preds <- data.frame(cbind(allstats$tss, predictions))
ggplot(preds, aes(x = V1, y = predictions)) +
  geom_point(size = 2) +
  theme_bw() +
  labs(x = 'Measured TSS', y = 'Predicted TSS') +
  geom_abline(slope = 1, intercept = 0) +
  xlim(0, 150) + ylim(0, 150)






