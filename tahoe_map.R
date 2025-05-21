library(ggplot2)
library(dplyr)
library(tidyverse)
library(maps)

# grab state data
all_states <- map_data("state")  
# make us outline
p <- ggplot() +
  geom_polygon( data=all_states, aes(x=long, y=lat, group = group), color='gray', fill="gray" )
p
# get just louisiana
louisiana <- filter(all_states, region == "louisiana")
usa <- map_data('usa')

# add louisiana to map of usa
p + geom_polygon(data = louisiana, aes(x=long, y=lat, group = group), color = 'black', fill="#729B79") +
  geom_polygon(data = usa, aes(x=long, y=lat, group=group), color = 'black', fill = NA) +
  theme_classic() +
  coord_fixed(1.3)


# make map of usa with states outlined
all_states <- map_data("state")  
p <- ggplot() +
  geom_polygon( data=all_states, aes(x=long, y=lat, group = group), color='black', fill="gray" )
p
p + geom_polygon(data = usa, aes(x=long, y=lat, group=group), color = 'black', fill = NA) +
  theme_void() +
  coord_fixed(1.3) +
  geom_rect(aes(xmin = -122, xmax = -118, ymin = 37.5, ymax = 40.5), 
            color = "black", fill = NA, linewidth = 1)


# instead of gauges, put in your own point data or river shapefile
#gauges <- read_csv() # your data here
ggplot() + 
  geom_polygon(data=all_states, aes(x=long, y=lat, group = group), color='black', fill="gray") +
  #geom_polygon(data = usa, aes(x=long, y=lat, group=group), color = 'black', fill = NA) +
  geom_sf(data = laketahoe, fill = 'lightblue') +
  #geom_point(data = gauges, aes(x = dec_long_v, y = dec_lat_va), color = 'blue') +
  theme_void() +
  coord_sf(xlim = c(-122, -118), ylim = c(37.5, 40.5))

ggplot() + 
  geom_sf(data = laketahoe, fill = 'lightblue') +
  theme_classic() +
  coord_sf(crs = 4269)

# download lakes geometries
lakes <- rnaturalearth::ne_download(scale = 50, 
                                    type = 'lakes', 
                                    category = 'physical') %>% 
  sf::st_as_sf(lakes110, crs = 4269)
laketahoe <- lakes[lakes$label == 'Lake Tahoe',]
laketahoe <- laketahoe[3,]


