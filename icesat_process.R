###############################################################################
# Code to visualize ICESat-2 data from downloaded csv files
# Written by Molly Stroud 5/21/25
###############################################################################
require(pacman)
p_load(ggplot2, tidyverse, geosphere, patchwork, ggside, plotgrid, viridis, moments)
###############################################################################

###############################################################################
# Make function to get data + clean up
###############################################################################
clean_icesat <- function(dataset) {
  raw_data <- read_csv(dataset) # read in
  colnames(raw_data)[4] <- 'height' # rename columns
  colnames(raw_data)[5] <- 'confidence'
  data <- mutate(raw_data, # get along-track distance
                  Distance = distHaversine(cbind(longitude, latitude),
                                           cbind(lag(longitude), 
                                                 lag(latitude))))
  data <- na.omit(data)
  data$along_distance <- cumsum(data$Distance)
  data <- data[data$confidence > 3,] # get only good data
  return(data)
}

###############################################################################
# Make function to get relevant stats 
###############################################################################
photon_stats <- function(clear, turbid) {
  print(paste0("Clear stdev = ", sd(clear$height)))
  print(paste0("Turbid stdev = ", sd(turbid$height)))
  print(paste0("Clear skewness = ", moments::skewness(clear$height)))
  print(paste0("Turbid skewness = ", moments::skewness(turbid$height)))
  print(paste0("Clear kurtosis = ", moments::kurtosis(clear$height)))
  print(paste0("Turbid kurtosis = ", moments::kurtosis(turbid$height)))
}

###############################################################################
# cedar lake, canada
###############################################################################
cedar <- clean_icesat("/Users/mollystroud/Desktop/icesat/icesat_2023-08-17.csv")  
cedar <- cedar[cedar$along_distance >= 4000 & cedar$along_distance <= 46500,]
cedar <- cedar[cedar$height < 227,] # remove above-surface returns

cedar_plot <- ggplot() + 
  geom_point(data = cedar[cedar$along_distance/1000 > 25,], 
             aes(x = along_distance/1000, y = height), 
             size = 0.5, color = '#E63A47') +
  geom_point(data = cedar[cedar$along_distance/1000 < 25,],
             aes(x = along_distance/1000, y = height), 
             size = 0.5, color = '#4EA699') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)') + #, 
       #title = "Cedar Lake and Lake Winnipegosis, Canada, 2023-08-17") +
  theme_classic() +
  scale_y_continuous(limits = c(220, 228), breaks = c(220, 224, 228)) +
  xlim(4, 46.5) 
cedar_plot
ggsave(plot = cedar_plot, "cedar_returns.pdf", width = 5, height = 4)

# plot density
cedar_density <- ggplot() +
  geom_density(data = cedar[cedar$along_distance/1000 < 25,],
               aes(y = height), color = '#4EA699', linewidth = 1) +
  geom_density(data = cedar[cedar$along_distance/1000 > 25,],
                 aes(y = height), color = '#E63A47', linewidth = 1) +
  theme_classic() +
  ylim(220, 228)# + xlim(0, 0.5)
cedar_density
ggsave(plot = cedar_density, "cedar_density.pdf", width = 2, height = 5)
# subset
cedar_density_subset <- ggplot() +
  geom_density(data = cedar[cedar$along_distance/1000 < 25,],
               aes(y = height), color = '#4EA699', linewidth = 1) +
  geom_density(data = cedar[cedar$along_distance/1000 > 25,],
               aes(y = height), color = '#E63A47', linewidth = 1) +
  theme_classic() +
  coord_cartesian(ylim = c(220, 225), xlim = c(0, 0.06))
cedar_density_subset
ggsave(plot = cedar_density_subset, "cedar_density_subset.pdf", width = 2, height = 5)

# calculate stats
clear_cedar <- cedar[cedar$along_distance/1000 < 25,]
turb_cedar <- cedar[cedar$along_distance/1000 > 25,]
photon_stats(clear_cedar, turb_cedar)
ks.test(x = clear$height, y = turb$height)

###############################################################################
# lake buchanan, tx
###############################################################################
buchanan <- clean_icesat("/Users/mollystroud/Desktop/icesat/icesat_2024-09-07.csv")  
buchanan <- buchanan[buchanan$along_distance >= 2500 & buchanan$along_distance <= 16000,]
buchanan <- buchanan[buchanan$height < 281.5,]

buchanan_plot <- ggplot() +
  geom_point(data = buchanan[buchanan$along_distance/1000 > 6.5,], aes(x = along_distance/1000, y = height),
             size = 0.5, color = '#4EA699') + 
  geom_point(data = buchanan[buchanan$along_distance/1000 < 6.5,], aes(x = along_distance/1000, y = height),
             size = 0.5, color = '#E63A47') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)') + #, 
       #title = "Lake Buchanan, TX, 2024-09-07") +
  theme_classic() +
  ylim(276, 283) #+ xlim(2.5, 16)
buchanan_plot
ggsave(plot = buchanan_plot, "buchanan_returns.pdf", width = 5, height = 4)

buchanan_density <- ggplot() +
  geom_density(data = buchanan[buchanan$along_distance/1000 > 6.5,],
               aes(y = height), color = '#4EA699', linewidth = 1) +
  geom_density(data = buchanan[buchanan$along_distance/1000 < 6.5,],
               aes(y = height), color = '#E63A47', linewidth = 1) +
  theme_classic() +
  ylim(276, 283)
  #xlim(0, 0.5)
#ylim(218, 226) + xlim(0, 0.5)
buchanan_density
ggsave(plot = buchanan_density, "buchanan_density.pdf", width = 2, height = 5)

# subset
buchanan_density_subset <- ggplot() +
  geom_density(data = buchanan[buchanan$along_distance/1000 > 6.5,],
               aes(y = height), color = '#4EA699', linewidth = 1) +
  geom_density(data = buchanan[buchanan$along_distance/1000 < 6.5,],
               aes(y = height), color = '#E63A47', linewidth = 1) +
  theme_classic() +
  coord_cartesian(ylim = c(276, 279.5), xlim = c(0, 0.03))
#ylim(220, 224.5)# + xlim(0, 0.5)
buchanan_density_subset
ggsave(plot = buchanan_density_subset, "buchanan_density_subset.pdf", width = 2, height = 5)

# calculate stats
clear_buchanan <- buchanan[buchanan$along_distance/1000 > 6.5,]
turb_buchanan <- buchanan[buchanan$along_distance/1000 < 6.5,]
photon_stats(clear_buchanan, turb_buchanan)
ks.test(x = clear$height, y = turb$height)

###############################################################################
# amazon confluence
###############################################################################
amazon <- clean_icesat("/Users/mollystroud/Desktop/icesat/amazon_madeira/icesat_2022-09-02.csv")
amazon <- amazon[amazon$along_distance >= 4000 & amazon$along_distance <= 14000,]

amazon_plot <- ggplot() +
  geom_point(data = amazon[amazon$along_distance/1000 > 8.8,], aes(x = along_distance/1000, y = height),
             size = 0.5, color = '#E63A47') + 
  geom_point(data = amazon[amazon$along_distance/1000 < 8.8,], aes(x = along_distance/1000, y = height),
             size = 0.5, color = '#4EA699') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)') + #, 
       #title = "Amazon/Madeira River confluence, 2022-09-02") +
  theme_classic() +
  ylim(0, 4)
  #scale_x_reverse() + xlim(14, 4)
amazon_plot
ggsave(plot = amazon_plot, "amazon_returns_22.pdf", width = 5, height = 4)

# density
amazon_density_plot <- ggplot() +
  geom_density(data = amazon[amazon$along_distance/1000 > 8.8,],
               aes(y = height), color = '#E63A47', linewidth = 1) +
  geom_density(data = amazon[amazon$along_distance/1000 < 8.8,],
               aes(y = height), color = '#4EA699', linewidth = 1) +
  theme_classic() +
  ylim(0, 4)
amazon_density_plot
ggsave(plot = amazon_density_plot, "amazon_density_22.pdf", width = 2, height = 5)

# subset
amazon_density_subset <- ggplot() +
  geom_density(data = amazon[amazon$along_distance/1000 > 8.8,],
               aes(y = height), color = '#E63A47', linewidth = 1) +
  geom_density(data = amazon[amazon$along_distance/1000 < 8.8,],
               aes(y = height), color = '#4EA699', linewidth = 1) +
  theme_classic() +
  ylim(0, 4) +
  coord_cartesian(ylim = c(0, 2), xlim = c(0, 0.5))
amazon_density_subset
ggsave(plot = amazon_density_subset, "amazon_density_subset.pdf", width = 2, height = 5)

# calculate stats
clear_amazon <- amazon[amazon$along_distance/1000 < 8.8,]
turb_amazon <- amazon[amazon$along_distance/1000 > 8.8,]
photon_stats(clear_amazon, turb_amazon)
ks.test(x = clear$height, y = turb$height)

# amazon 2024
amazon_w <- clean_icesat("/Users/mollystroud/Desktop/icesat/amazon_madeira/icesat_2024-08-16.csv")
amazon_w <- amazon_w[amazon_w$along_distance >= 4300 & amazon_w$along_distance <= 16000,]

ggplot()+
geom_point(data = amazon_w[amazon_w$along_distance/1000 < 10.5,], aes(x = longitude, y = latitude),
           size = 0.5, color = '#E63A47') + 
geom_point(data = amazon_w[amazon_w$along_distance/1000 > 10.5,], aes(x = longitude, y = latitude),
           size = 0.5, color = '#4EA699')

amazon_plot_w <- ggplot() +
  geom_point(data = amazon_w[amazon_w$along_distance/1000 < 10.5,], aes(x = along_distance/1000, y = height),
             size = 0.5, color = '#E63A47') + 
  geom_point(data = amazon_w[amazon_w$along_distance/1000 > 10.5,], aes(x = along_distance/1000, y = height),
             size = 0.5, color = '#4EA699') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)') + #, 
       #title = "Amazon/Madeira River confluence, 2024-08-16") +
  theme_classic() +
  ylim(-2.5, 1.5) #+ xlim(4, 14)
amazon_plot_w
ggsave(plot = amazon_plot_w, "amazon_returns_24.pdf", width = 5, height = 4)

# density
amazon_density_plot_w <- ggplot() +
  geom_density(data = amazon_w[amazon_w$along_distance/1000 < 10.5,],
               aes(y = height), color = '#E63A47', linewidth = 1) +
  geom_density(data = amazon_w[amazon_w$along_distance/1000 > 10.5,],
               aes(y = height), color = '#4EA699', linewidth = 1) +
  theme_classic() +
  #ylim(276, buchanan_density_peak) + 
  ylim(-2.5, 1.5)
#ylim(218, 226) + xlim(0, 0.5)
amazon_density_plot_w
ggsave(plot = amazon_density_plot_w, "amazon_density_24.pdf", width = 2, height = 5)

# subset
amazon_density_subset_24 <- ggplot() +
  geom_density(data = amazon_w[amazon_w$along_distance/1000 < 10.5,],
               aes(y = height), color = '#E63A47', linewidth = 1) +
  geom_density(data = amazon_w[amazon_w$along_distance/1000 > 10.5,],
               aes(y = height), color = '#4EA699', linewidth = 1) +
  theme_classic() +
  ylim(-2.5, 1.5) +
  coord_cartesian(ylim = c(-2.5, -0.85), xlim = c(0, 0.1))
amazon_density_subset_24
ggsave(plot = amazon_density_subset_24, "amazon_density_subset_24.pdf", width = 2, height = 5)

# calculate stats
clear_amazon_w <- amazon_w[amazon_w$along_distance/1000 > 10.5,]
turb_amazon_w <- amazon_w[amazon_w$along_distance/1000 < 10.5,]
photon_stats(clear_amazon_w, turb_amazon_w)
ks.test(x = clear$height, y = turb$height)

###############################################################################
# ohio / mississippi confluence
###############################################################################
cairo <- clean_icesat("/Users/mollystroud/Desktop/icesat/icesat_2024-04-25_cairo.csv")
cairo <- cairo[cairo$along_distance >= 400 & cairo$along_distance <= 2000,]

cairo_plot <- ggplot() +
  geom_point(data = cairo[cairo$along_distance/1000 > 1,], aes(x = along_distance/1000, y = height),
             size = 0.5, color = '#E63A47') + 
  geom_point(data = cairo[cairo$along_distance/1000 < 1,], aes(x = along_distance/1000, y = height),
             size = 0.5, color = '#4EA699') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)') + #, 
       #title = "Ohio and Mississippi River confluence, 2024-04-25") +
  theme_classic() +
  ylim(59, 66)# + xlim(0.4, 2)
cairo_plot
ggsave(plot = cairo_plot, "cairo_returns.pdf", width = 5, height = 4)

# density
cairo_density_plot <- ggplot() +
  geom_density(data = cairo[cairo$along_distance/1000 > 1,],
               aes(y = height), color = '#E63A47', linewidth = 1) +
  geom_density(data = cairo[cairo$along_distance/1000 < 1,],
               aes(y = height), color = '#4EA699', linewidth = 1) +
  theme_classic() +
  ylim(59, 66)
  #xlim(0, 1)
#ylim(218, 226) + xlim(0, 0.5)
cairo_density_plot
ggsave(plot = cairo_density_plot, "cairo_density.pdf", width = 2, height = 5)

# subset
cairo_density_subset <- ggplot() +
  geom_density(data = cairo[cairo$along_distance/1000 > 1,],
               aes(y = height), color = '#E63A47', linewidth = 1) +
  geom_density(data = cairo[cairo$along_distance/1000 < 1,],
               aes(y = height), color = '#4EA699', linewidth = 1) +
  theme_classic() +
  ylim(59, 66) +
  coord_cartesian(ylim = c(59, 63), xlim = c(0, 0.05))
cairo_density_subset
ggsave(plot = cairo_density_subset, "cairo_density_subset.pdf", width = 2, height = 5)

# calculate stats
clear_cairo <- cairo[cairo$along_distance/1000 < 1,]
turb_cairo <- cairo[cairo$along_distance/1000 > 1,]
photon_stats(clear_cairo, turb_cairo)
ks.test(x = clear$height, y = turb$height)


###############################################################################
# lake tahoe 
###############################################################################

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
  geom_point(size = 0.5, color = '#4EA699') +
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
  geom_point(size = 0.5, color = '#4EA699') +
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
  geom_point(size = 0.5, color = '#4EA699') +
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
  geom_point(size = 0.5, color = '#4EA699') +
  labs(x = 'Along-Track Distance (km)', y = 'Height (m)', 
       title = "2023-10-04, turbidity (10-03) = 20 FNU") +
  theme_classic() +
  ylim(1860, 1878)
tahoe_plot_10_2023

(tahoe_plot_1_2022 + tahoe_plot_12_2022) / (tahoe_plot_7_2023 + tahoe_plot_10_2023)

