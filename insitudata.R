###############################################################################
# Code to download ICESat-2 data over Lake Tahoe and visualize
# Written by Molly Stroud 6/2/25
###############################################################################
require(pacman)
p_load(ggplot2, tidyverse, IceSat2R, geosphere, patchwork)
###############################################################################
# get dates from our tahoe data
tahoe_LTP <- read_csv("/Users/mollystroud/Desktop/icesat/datasets/LakeTahoe_2022_2023/Tahoe_LTP_CTD_Data_matches.csv")
tahoe_MLTP <- read_csv("/Users/mollystroud/Desktop/icesat/datasets/LakeTahoe_2022_2023/Tahoe_MLTP_CTD_Data_matches.csv")
tahoe_LTP$Date_Formatted <- as.Date(tahoe_LTP$Date, format="%m/%d/%y")
tahoe_MLTP$Date_Formatted <- as.Date(tahoe_MLTP$Date, format="%m/%d/%y")
# get all dates within 3 days of sampling
LTP_dates <- unique(tahoe_LTP$Date_Formatted)
LTP_dates_all <- c(LTP_dates, (LTP_dates + 1), (LTP_dates + 2), 
                   (LTP_dates - 1), (LTP_dates - 2),
                   (LTP_dates - 3), (LTP_dates + 3))
MLTP_dates <- unique(tahoe_MLTP$Date_Formatted)
MLTP_dates_all <- c(MLTP_dates, (MLTP_dates + 1), (MLTP_dates + 2), 
                    (MLTP_dates - 1), (MLTP_dates - 2),
                    (MLTP_dates - 3), (MLTP_dates + 3))
###############################################################################
# Download data over both tracks that pass over Tahoe and all dates
###############################################################################
# for track 242 and LTP
for(day in LTP_dates_all){
  data <- get_atlas_data(
    minx = '-120.11', # lake tahoe region
    miny = '38.94',
    maxx = '-119.961',
    maxy = '39.21',
    date = as.character(as.Date(day)),
    trackId = '242',
    beamName = c('gt3r', 'gt2r', 'gt1r'), # only strong beams
    product = "atl03",
    client = "portal",
    photonConfidence = c('low', 'medium', 'high'),
    sampling = FALSE,
    outputFormat = "csv",
    file_path_zip = NULL,
    download_method = "curl",
    verbose = TRUE
  )
  if(nrow(data) > 1) {
    filename = paste0('downloaded_csvs/', as.character(as.Date(day)), '_LTP_242.csv')
    write_csv(x = data, filename)
  }else{print("No data")}
}

# for track 1210 and LTP
for(day in LTP_dates_all){
  data <- get_atlas_data(
    minx = '-120.11', # lake tahoe region
    miny = '38.94',
    maxx = '-119.961',
    maxy = '39.21',
    date = as.character(as.Date(day)),
    trackId = '1210',
    beamName = c('gt3r', 'gt2r', 'gt1r'), # only strong beams
    product = "atl03",
    client = "portal",
    photonConfidence = c('low', 'medium', 'high'),
    sampling = FALSE,
    outputFormat = "csv",
    file_path_zip = NULL,
    download_method = "curl",
    verbose = TRUE
  )
  if(nrow(data) > 1) {
    filename = paste0('downloaded_csvs/', as.character(as.Date(day)), '_LTP_1210.csv')
    write_csv(x = data, filename)
  }else{print("No data")}
}


# for track 242 and MLTP
for(day in MLTP_dates_all){
  data <- get_atlas_data(
    minx = '-120.11', # lake tahoe region
    miny = '38.94',
    maxx = '-119.961',
    maxy = '39.21',
    date = as.character(as.Date(day)),
    trackId = '242',
    beamName = c('gt3r', 'gt2r', 'gt1r'), # only strong beams
    product = "atl03",
    client = "portal",
    photonConfidence = c('low', 'medium', 'high'),
    sampling = FALSE,
    outputFormat = "csv",
    file_path_zip = NULL,
    download_method = "curl",
    verbose = TRUE
  )
  if(nrow(data) > 1) {
    filename = paste0('downloaded_csvs/', as.character(as.Date(day)), '_MLTP_242.csv')
    write_csv(x = data, filename)
  }else{print("No data")}
}

# for track 1210 and MLTP
for(day in MLTP_dates_all){
  data <- get_atlas_data(
    minx = '-120.11', # lake tahoe region
    miny = '38.94',
    maxx = '-119.961',
    maxy = '39.21',
    date = as.character(as.Date(day)),
    trackId = '1210',
    beamName = c('gt3r', 'gt2r', 'gt1r'), # only strong beams
    product = "atl03",
    client = "portal",
    photonConfidence = c('low', 'medium', 'high'),
    sampling = FALSE,
    outputFormat = "csv",
    file_path_zip = NULL,
    download_method = "curl",
    verbose = TRUE
  )
  if(nrow(data) > 1) {
    filename = paste0('downloaded_csvs/', as.character(as.Date(day)), '_MLTP_1210.csv')
    write_csv(x = data, filename)
  }else{print("No data")}
}

###############################################################################
# Look at and plot data
###############################################################################
# get files
tahoe_data_files <- list.files('downloaded_csvs', full.names = TRUE)
tahoe_LTP_surface <- tahoe_LTP[tahoe_LTP$Depth == 1,]
tahoe_MLTP_surface <- tahoe_MLTP[tahoe_MLTP$Depth == 1,]
# make graphs
for(file in tahoe_data_files){
  data <- read_csv(file)
  data <- data[data$`confidence code` > 2,]
  colnames(data)[4] <- 'height'
  date <- substr(file, start = 17, stop = 26)
  metadata <- substr(file, start = 28, stop = 34)
  # get along-track distance
  data <- mutate(data, 
                 Distance = distHaversine(cbind(longitude, latitude),
                                          cbind(lag(longitude), lag(latitude))))
  data <- na.omit(data)
  data$along_distance <- cumsum(data$Distance)
  # make plot for each beam
  plot1 <- ggplot(data[data$beam == 'gt1r',], aes(x = along_distance/1000, y = height)) + 
    geom_point(size = 0.5, color = '#087F8C') +
    labs(x = 'Along-Track Distance (km)', y = 'Height (m)', 
         title = paste0(date, ',', metadata)) +
    theme_bw() +
    ylim(1840, 1880)
  plot2 <- ggplot(data[data$beam == 'gt2r',], aes(x = along_distance/1000, y = height)) + 
    geom_point(size = 0.5, color = '#087F8C') +
    labs(x = 'Along-Track Distance (km)', y = 'Height (m)', 
         title = paste0(date, ',', metadata)) +
    theme_bw() +
    ylim(1840, 1880)
  plot3 <- ggplot(data[data$beam == 'gt3r',], aes(x = along_distance/1000, y = height)) + 
    geom_point(size = 0.5, color = '#087F8C') +
    labs(x = 'Along-Track Distance (km)', y = 'Height (m)', 
         title = paste0(date, ',', metadata)) +
    theme_bw() +
    ylim(1840, 1880)
  plot1 + plot2 + plot3
  ggsave(filename = paste0(substr(file, start = 1, stop = 34), '.pdf'),
         width = 7, height = 4)
}

# get time of icesat data at tahoe for each date
data <- time_specific_orbits(date_from = '2023-09-07',
                     date_to = '2023-09-07',
                     download_method = 'curl',
                     threads = 1,
                     verbose = T)


