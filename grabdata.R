###############################################################################
# Code to download ICESat-2 data over Lake Tahoe
# Written by Molly Stroud 6/2/25
###############################################################################
require(pacman)
p_load(ggplot2, tidyverse, IceSat2R)
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
