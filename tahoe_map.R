###############################################################################
# Code to create maps of USA and Lake Tahoe
# Written by Molly Stroud 5/21/25
###############################################################################
require(pacman)
p_load(ggplot2, dplyr, tidyverse, maps, rnaturalearth)
###############################################################################

# grab state data
all_states <- map_data("state")
usa <- map_data("usa")
# make map of usa with states outlined and rectangle around AOI
ggplot() + 
  geom_polygon(data = usa, aes(x=long, y=lat, group=group), 
               color = 'black', fill = NA) +
  geom_polygon( data=all_states, aes(x=long, y=lat, group = group), 
                color='black', fill="gray" ) +
  theme_void() +
  coord_fixed(1.3) +
  geom_rect(aes(xmin = -122, xmax = -118, ymin = 37.5, ymax = 40.5), 
            color = "black", fill = NA, linewidth = 1)

# map zoomed in AOI with Lake Tahoe
# download Lake Tahoe
lakes <- rnaturalearth::ne_download(scale = 50, 
                                    type = 'lakes', 
                                    category = 'physical') %>% 
  sf::st_as_sf(lakes110, crs = 4269)
laketahoe <- lakes[lakes$label == 'Lake Tahoe',]
laketahoe <- laketahoe[3,]

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




