###############################################################################
# Code to visualize ICESat-2 data from downloaded csv files
# Written by Molly Stroud 5/21/25
###############################################################################
require(pacman)
p_load(ggplot2, tidyverse, geosphere, patchwork)
###############################################################################


data <- read_csv("Desktop/icesat/icesat_2023-11-05.csv")
colnames(data)[4] <- 'height'
colnames(data)[5] <- 'confidence'

test <- mutate(data, 
               Distance = distHaversine(cbind(longitude, latitude),
                                        cbind(lag(longitude), lag(latitude))))
test <- na.omit(test)
test$along_distance <- cumsum(test$Distance)

ggplot(test[test$confidence > 3,], aes(x = along_distance/1000, y = height)) + 
  geom_point(size = 0.5, color = '#6C4B5E') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)', 
       title = "Plaquemines Parish, LA, 2023-11-05") +
  theme_bw() +
  ylim(-40, -20) + xlim(15, 80)



data <- read_csv("icesat_2023-08-17.csv")
colnames(data)[4] <- 'height'
colnames(data)[5] <- 'confidence'
test <- mutate(data, 
               Distance = distHaversine(cbind(longitude, latitude),
                                        cbind(lag(longitude), lag(latitude))))
test <- na.omit(test)
test$along_distance <- cumsum(test$Distance)

ggplot(test[test$confidence > 3,], aes(x = along_distance/1000, y = height)) + 
  geom_point(size = 0.5, color = '#087F8C') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)', 
       title = "Cedar Lake and Lake Winnipegosis, Canada, 2023-08-17") +
  theme_classic() +
  ylim(220, 235) + xlim(4, 46.5)



data <- read_csv("icesat_2024-09-07.csv")
colnames(data)[4] <- 'height'
colnames(data)[5] <- 'confidence'
test <- mutate(data, 
               Distance = distHaversine(cbind(longitude, latitude),
                                        cbind(lag(longitude), lag(latitude))))
test <- na.omit(test)
test$along_distance <- cumsum(test$Distance)

ggplot(test[test$confidence > 3,], aes(x = along_distance/1000, y = height)) + 
  geom_point(size = 0.5, color = '#087F8C') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)', 
       title = "Lake Buchanan, TX, 2024-09-07") +
  theme_classic() +
  ylim(274, 287) + xlim(2.5, 16)


data <- read_csv("amazon_madeira/icesat_2022-09-02.csv")
colnames(data)[4] <- 'height'
colnames(data)[5] <- 'confidence'
test <- mutate(data, 
               Distance = distHaversine(cbind(longitude, latitude),
                                        cbind(lag(longitude), lag(latitude))))
test <- na.omit(test)
test$along_distance <- cumsum(test$Distance)

working <- ggplot(test[test$confidence > 3,], aes(x = along_distance/1000, y = height)) + 
  geom_point(size = 0.5, color = '#087F8C') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)', 
       title = "Amazon/Madeira River confluence, 2022-09-02") +
  theme_classic() +
  ylim(-0, 4) + #xlim(4, 14) + 
  scale_x_reverse() + xlim(14, 4)
working

data_w <- read_csv("amazon_madeira/icesat_2024-08-16.csv")
colnames(data_w)[4] <- 'height'
colnames(data_w)[5] <- 'confidence'
test_w <- mutate(data_w, 
               Distance = distHaversine(cbind(longitude, latitude),
                                        cbind(lag(longitude), lag(latitude))))
test_w <- na.omit(test_w)
test_w$along_distance <- cumsum(test_w$Distance)

not_working <- ggplot(test_w[test_w$confidence > 3,], aes(x = along_distance/1000, y = height)) + 
  geom_point(size = 0.5, color = '#087F8C') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)', 
       title = "Amazon/Madeira River confluence, 2024-08-16") +
  theme_classic() +
  ylim(-2, 2) + xlim(4, 14)
not_working

library(patchwork)
working + not_working


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

tahoe_1_2022 <- read_csv("/Users/mollystroud/Desktop/icesat/tahoe/photon_2022-01-07_tahoe.csv")
tahoe_10_2022 <- read_csv("/Users/mollystroud/Desktop/icesat/tahoe/photon_2022-10-06_tahoe.csv")
tahoe_10_2023 <- read_csv("/Users/mollystroud/Desktop/icesat/tahoe/photon_2023-10-04_tahoe.csv")

# 1-7-2022
colnames(tahoe_1_2022)[4] <- 'height'
colnames(tahoe_1_2022)[5] <- 'confidence'
tahoe_1_2022 <- mutate(tahoe_1_2022, 
                Distance = distHaversine(cbind(longitude, latitude),
                                         cbind(lag(longitude), lag(latitude))))
tahoe_1_2022 <- na.omit(tahoe_1_2022)
tahoe_1_2022$along_distance <- cumsum(tahoe_1_2022$Distance)

tahoe_plot_1_2022 <- ggplot(tahoe_1_2022[tahoe_1_2022$confidence > 3,], aes(x = along_distance/1000, y = height)) + 
  geom_point(size = 0.5, color = '#087F8C') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)', 
       title = "2022-01-07, turbidity (01-20) = 0.45 FNU") +
  theme_classic() +
  ylim(1860, 1878)
tahoe_plot_1_2022

# 10-6-2022
colnames(tahoe_10_2022)[4] <- 'height'
colnames(tahoe_10_2022)[5] <- 'confidence'
tahoe_10_2022 <- mutate(tahoe_10_2022, 
                       Distance = distHaversine(cbind(longitude, latitude),
                                                cbind(lag(longitude), lag(latitude))))
tahoe_10_2022 <- na.omit(tahoe_10_2022)
tahoe_10_2022$along_distance <- cumsum(tahoe_10_2022$Distance)

tahoe_plot_10_2022 <- ggplot(tahoe_10_2022[tahoe_10_2022$confidence > 3,], aes(x = along_distance/1000, y = height)) + 
  geom_point(size = 0.5, color = '#087F8C') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)', 
       title = "2022-10-06, turbidity (10-11) = 20.9 FNU") +
  theme_classic() +
  ylim(1860, 1878) + xlim(4, 30)
tahoe_plot_10_2022

# 10-4-2023
colnames(tahoe_10_2023)[4] <- 'height'
colnames(tahoe_10_2023)[5] <- 'confidence'
tahoe_10_2023 <- mutate(tahoe_10_2023, 
                        Distance = distHaversine(cbind(longitude, latitude),
                                                 cbind(lag(longitude), lag(latitude))))
tahoe_10_2023 <- na.omit(tahoe_10_2023)
tahoe_10_2023$along_distance <- cumsum(tahoe_10_2023$Distance)

tahoe_plot_10_2023 <- ggplot(tahoe_10_2023[tahoe_10_2023$confidence > 3,], aes(x = along_distance/1000, y = height)) + 
  geom_point(size = 0.5, color = '#087F8C') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)', 
       title = "2023-10-04, turbidity (10-03) = 20 FNU") +
  theme_classic() +
  ylim(1860, 1878) + xlim(3, 35)
tahoe_plot_10_2023

tahoe_plot_1_2022 + tahoe_plot_10_2022 + tahoe_plot_10_2023 + 
  plot_annotation(title = 'Lake Tahoe ICESat-2 data') #+
  plot_layout(axis_titles = "collect")


