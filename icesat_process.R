###############################################################################
# Code to visualize ICESat-2 data from downloaded csv files
# Written by Molly Stroud 5/21/25
###############################################################################
require(pacman)
p_load(ggplot2, tidyverse, geosphere, patchwork, ggside, plotgrid)
###############################################################################


###############################################################################
# cedar lake, canada
###############################################################################
raw_cedar <- read_csv("/Users/mollystroud/Desktop/icesat/icesat_2023-08-17.csv")
colnames(raw_cedar)[4] <- 'height'
colnames(raw_cedar)[5] <- 'confidence'
cedar <- mutate(raw_cedar, 
               Distance = distHaversine(cbind(longitude, latitude),
                                        cbind(lag(longitude), lag(latitude))))
cedar <- na.omit(cedar)
cedar$along_distance <- cumsum(cedar$Distance)
cedar <- cedar[cedar$confidence > 3,]

cedar_plot <- ggplot() + 
  geom_point(data = cedar[cedar$along_distance/1000 > 25,], 
             aes(x = along_distance/1000, y = height), 
             size = 0.5, color = '#1D3557') +
  geom_point(data = cedar[cedar$along_distance/1000 < 25,],
             aes(x = along_distance/1000, y = height), 
             size = 0.5, color = '#E63946') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)') + #, 
       #title = "Cedar Lake and Lake Winnipegosis, Canada, 2023-08-17") +
  theme_classic() +
  ylim(220, 227) + xlim(4, 46.5)
cedar_plot
# plot density
cedar_density <- ggplot() +
  geom_density(data = cedar[cedar$along_distance/1000 < 25,],
               aes(y = height), color = '#E63946', linewidth = 1) +
  geom_density(data = cedar[cedar$along_distance/1000 > 25,],
                 aes(y = height), color = '#1D3557', linewidth = 1) +
  theme_classic() +
  ylim(220, 226) + xlim(0, 0.5)
  #ylim(218, 226) + xlim(0, 0.5)
cedar_density

  
###############################################################################
# lake buchanan, tx
###############################################################################
buchanan_raw <- read_csv("/Users/mollystroud/Desktop/icesat/icesat_2024-09-07.csv")
colnames(buchanan_raw)[4] <- 'height'
colnames(buchanan_raw)[5] <- 'confidence'
buchanan <- mutate(buchanan_raw, 
               Distance = distHaversine(cbind(longitude, latitude),
                                        cbind(lag(longitude), lag(latitude))))
buchanan <- na.omit(buchanan)
buchanan$along_distance <- cumsum(buchanan$Distance)
buchanan <- buchanan[buchanan$confidence > 3,]

buchanan_plot <- ggplot() +
  geom_point(data = buchanan[buchanan$along_distance/1000 > 6.5,], aes(x = along_distance/1000, y = height),
             size = 0.5, color = '#E63946') + 
  geom_point(data = buchanan[buchanan$along_distance/1000 < 6.5,], aes(x = along_distance/1000, y = height),
             size = 0.5, color = '#1D3557') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)') + #, 
       #title = "Lake Buchanan, TX, 2024-09-07") +
  theme_classic() +
  ylim(276, 281.5) + xlim(2.5, 16)
buchanan_plot

buchanan_density <- ggplot() +
  geom_density(data = buchanan[buchanan$along_distance/1000 < 6.5,],
               aes(y = height), color = '#E63946', linewidth = 1) +
  geom_density(data = buchanan[buchanan$along_distance/1000 > 6.5,],
               aes(y = height), color = '#1D3557', linewidth = 1) +
  theme_classic() +
  ylim(276, 281.2) + xlim(0, 0.75)
#ylim(218, 226) + xlim(0, 0.5)
buchanan_density


###############################################################################
# amazon confluence
###############################################################################
amazon_raw <- read_csv("amazon_madeira/icesat_2022-09-02.csv")
colnames(amazon_raw)[4] <- 'height'
colnames(amazon_raw)[5] <- 'confidence'
amazon <- mutate(amazon_raw, 
               Distance = distHaversine(cbind(longitude, latitude),
                                        cbind(lag(longitude), lag(latitude))))
amazon <- na.omit(amazon)
amazon$along_distance <- cumsum(amazon$Distance)
amazon <- amazon[amazon$confidence > 3,]

amazon_plot <- ggplot(amazon, aes(x = along_distance/1000, y = height)) + 
  geom_point(size = 0.5, color = '#087F8C') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)', 
       title = "Amazon/Madeira River confluence, 2022-09-02") +
  theme_classic() +
  ylim(-0, 4) + #xlim(4, 14) + 
  scale_x_reverse() + xlim(14, 4)
amazon_plot

amazon_raw_w <- read_csv("amazon_madeira/icesat_2024-08-16.csv")
colnames(amazon_raw_w)[4] <- 'height'
colnames(amazon_raw_w)[5] <- 'confidence'
amazon_w <- mutate(amazon_raw_w, 
               Distance = distHaversine(cbind(longitude, latitude),
                                        cbind(lag(longitude), lag(latitude))))
amazon_w <- na.omit(amazon_w)
amazon_w$along_distance <- cumsum(amazon_w$Distance)
amazon_w <- amazon_w[amazon_w$confidence > 3,]

amazon_plot_w <- ggplot(amazon_w, aes(x = along_distance/1000, y = height)) + 
  geom_point(size = 0.5, color = '#087F8C') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)', 
       title = "Amazon/Madeira River confluence, 2024-08-16") +
  theme_classic() +
  ylim(-2, 2) + xlim(4, 14)
amazon_plot_w


###############################################################################
# ohio / mississippi confluence
###############################################################################
cairo <- read_csv("icesat_2024-04-25_cairo.csv")
colnames(cairo)[4] <- 'height'
colnames(cairo)[5] <- 'confidence'
cairo <- mutate(cairo, 
               Distance = distHaversine(cbind(longitude, latitude),
                                        cbind(lag(longitude), lag(latitude))))
cairo <- na.omit(cairo)
cairo$along_distance <- cumsum(cairo$Distance)

ggplot(cairo[cairo$confidence > 3,], aes(x = along_distance/1000, y = height)) + 
  geom_point(size = 0.5, color = '#087F8C') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)', 
       title = "Ohio and Mississippi River confluence, 2024-04-25") +
  theme_classic() +
  ylim(57, 70) + xlim(0.4, 2)


#########################################
# Lake Tahoe example
#########################################

tahoe_1_2022 <- read_csv("/Users/mollystroud/Desktop/icesat/ICESat-2/downloaded_csvs/2022-01-07_LTP_242.csv")
tahoe_12_2022 <- read_csv("/Users/mollystroud/Desktop/icesat/ICESat-2/downloaded_csvs/2022-12-08_LTP_1210.csv")
tahoe_7_2023 <- read_csv("/Users/mollystroud/Desktop/icesat/ICESat-2/downloaded_csvs/2023-07-06_MLTP_242.csv")
tahoe_10_2023 <- read_csv("/Users/mollystroud/Desktop/icesat/ICESat-2/downloaded_csvs/2023-10-04_LTP_242.csv")

# 1-7-2022
colnames(tahoe_1_2022)[4] <- 'height'
colnames(tahoe_1_2022)[5] <- 'confidence'
tahoe_1_2022 <- mutate(tahoe_1_2022, 
                Distance = distHaversine(cbind(longitude, latitude),
                                         cbind(lag(longitude), lag(latitude))))
tahoe_1_2022 <- na.omit(tahoe_1_2022)
tahoe_1_2022$along_distance <- cumsum(tahoe_1_2022$Distance)

tahoe_plot_1_2022 <- ggplot(tahoe_1_2022[tahoe_1_2022$confidence > 2 & 
                                           tahoe_1_2022$beam == 'gt2r',], 
                            aes(x = along_distance/1000, y = height)) + 
  geom_point(size = 0.5, color = '#087F8C') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)', 
       title = "2022-01-07, turbidity (01-10) = 0.45 FNU") +
  theme_classic() +
  ylim(1860, 1878)
tahoe_plot_1_2022

# 12-8-2022
colnames(tahoe_12_2022)[4] <- 'height'
colnames(tahoe_12_2022)[5] <- 'confidence'
tahoe_12_2022 <- mutate(tahoe_12_2022, 
                       Distance = distHaversine(cbind(longitude, latitude),
                                                cbind(lag(longitude), lag(latitude))))
tahoe_12_2022 <- na.omit(tahoe_12_2022)
tahoe_12_2022$along_distance <- cumsum(tahoe_12_2022$Distance)

tahoe_plot_12_2022 <- ggplot(tahoe_12_2022[tahoe_12_2022$confidence > 2 &
                                             tahoe_12_2022$beam == 'gt1r',], 
                             aes(x = along_distance/1000, y = height)) + 
  geom_point(size = 0.5, color = '#087F8C') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)', 
       title = "2022-12-08, turbidity (12-07) = 21.2 FNU") +
  theme_classic() +
  ylim(1860, 1878)
tahoe_plot_12_2022



# 7-6-2023
colnames(tahoe_7_2023)[4] <- 'height'
colnames(tahoe_7_2023)[5] <- 'confidence'
tahoe_7_2023 <- mutate(tahoe_7_2023, 
                        Distance = distHaversine(cbind(longitude, latitude),
                                                 cbind(lag(longitude), lag(latitude))))
tahoe_7_2023 <- na.omit(tahoe_7_2023)
tahoe_7_2023$along_distance <- cumsum(tahoe_7_2023$Distance)

tahoe_plot_7_2023 <- ggplot(tahoe_7_2023[tahoe_7_2023$confidence > 2 &
                                           tahoe_7_2023$beam == 'gt2r',], 
                            aes(x = along_distance/1000, y = height)) + 
  geom_point(size = 0.5, color = '#087F8C') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)', 
       title = "2023-07-06, turbidity (07-05) = 19.7 FNU") +
  theme_classic() +
  ylim(1860, 1878)
tahoe_plot_7_2023



# 10-4-2023
colnames(tahoe_10_2023)[4] <- 'height'
colnames(tahoe_10_2023)[5] <- 'confidence'
tahoe_10_2023 <- mutate(tahoe_10_2023, 
                        Distance = distHaversine(cbind(longitude, latitude),
                                                 cbind(lag(longitude), lag(latitude))))
tahoe_10_2023 <- na.omit(tahoe_10_2023)
tahoe_10_2023$along_distance <- cumsum(tahoe_10_2023$Distance)

tahoe_plot_10_2023 <- ggplot(tahoe_10_2023[tahoe_10_2023$confidence > 3 &
                                             tahoe_10_2023$beam == 'gt2r',], 
                             aes(x = along_distance/1000, y = height)) + 
  geom_point(size = 0.5, color = '#087F8C') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)', 
       title = "2023-10-04, turbidity (10-03) = 20 FNU") +
  theme_classic() +
  ylim(1860, 1878)
tahoe_plot_10_2023

(tahoe_plot_1_2022 + tahoe_plot_12_2022) / (tahoe_plot_7_2023 + tahoe_plot_10_2023)

