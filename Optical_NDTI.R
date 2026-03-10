###############################################################################
# Code to download HLS optical data and calculate NDTI 
# Written by Molly Stroud 3/6/26
###############################################################################
pacman::p_load(rstac, gdalcubes, sf, stars, terra, ggplot2)
# define stac url
ls = stac("https://planetarycomputer.microsoft.com/api/stac/v1")

bbox <- c(left = -100.2940,
           bottom = 53.14795,
           right = -100.2315,
           top = 53.52786)
start_date <- "2023-08-14"
end_date <- "2023-08-17"
################################################################################
# Function to create thermal stars object with specified dates and bbox
################################################################################
get_lst <- function(bbox, start_date, end_date) {
  # grab items within dates of interest
  items <- ls |>
    stac_search(collections = "hls2-s30",
                bbox = bbox,
                datetime = paste(start_date, end_date, sep="/"),
                limit = 1000) |>
    #ext_query("eo:cloud_cover" < 30) |> #filter for cloud cover
    post_request() |>
    items_sign(sign_fn = sign_planetary_computer()) |>
    items_fetch()
    # define the cube space
    cube <- cube_view(srs = "EPSG:4326",
                      extent = list(t0 = start_date, 
                                    t1 = end_date,
                                    left = bbox[1], 
                                    right = bbox[3],
                                    top = bbox[4], 
                                    bottom = bbox[2]),
                      dx = 0.00031, # 30 m resolution
                      dy = 0.00031, 
                      dt = "P1D",
                      aggregation = "median", 
                      resampling = "average")
    # create stac image collection
    col <- stac_image_collection(items$features,
                                 asset_names = c("B03", "B04", "B02"), # green red blue
                                 url_fun = identity)
    # make raster cube
    data <- raster_cube(image_collection = col, 
                        view = cube) |>
      apply_pixel(expr = "(B04 - B03)/(B04 + B03)", names = "NDTI")
    # make stars obj
    ls_stars <- st_as_stars(data)
    return(ls_stars)
}



################################################################################
# CEDAR LAKE
################################################################################
ls_stars <- get_lst(bbox, start_date, end_date)
plot(ls_stars)

r <- rast(ls_stars)
plot(r)
coords <- cbind(c(-100.2940, -100.2315), c(53.14795, 53.52786))
line <- st_linestring(coords)
line <- st_sfc(line, crs = 4326)
line_vect <- vect(line)
pts <- spatSample(line_vect, size = 200, method = "regular")
vals <- extract(r, pts)
ggplot(vals, aes(x = ID, y = lyr.1)) +
  geom_line() +
  theme_classic()
# plot density
cedar_density <- ggplot() +
  geom_density(data = cedar[cedar$along_distance/1000 < 25,],
               aes(x = along_distance), color = '#4EA699', linewidth = 1) +
  geom_density(data = cedar[cedar$along_distance/1000 > 25,],
               aes(x = along_distance), color = '#E63A47', linewidth = 1) +
  theme_classic() #+
  ylim(220, 228)# + xlim(0, 0.5)
cedar_density


# create 100 m bins of photons
bin_size <- 1000   # meters

cedar$track_bin <- floor(cedar$along_distance / bin_size)


track_points <- cedar %>%
  group_by(track_bin) %>%
  summarise(
    lon = mean(longitude),
    lat = mean(latitude)
  )

track_points <- st_as_sf(track_points, coords = c("lon","lat"), crs = 4326)

track_points$NDTI <- terra::extract(r, vect(track_points))[,2]

photon_stats <- cedar %>%
  group_by(track_bin) %>%
  summarise(
    photon_count = n(),
    max_depth = max(height, na.rm = TRUE),
    mean_depth = mean(height, na.rm = TRUE),
    depth_sd = sd(height, na.rm = TRUE),
    deep_fraction = mean(height > 1, na.rm = TRUE)
  )
data <- merge(photon_stats, st_drop_geometry(track_points), by="track_bin")
plot(data$depth_sd)
plot(data$NDTI)
cor(data$NDTI, data$depth_sd)
mod <- lm(depth_sd ~ NDTI, data=data)
summary(mod)

ggplot(data, aes(x = NDTI, y = depth_sd)) +
  geom_point() +
  theme_classic() +
  labs(y = "Depth Stdev", title = "Cedar Lake")
cor(data$NDTI, data$depth_sd)



################################################################################
# LAKE BUCHANAN
################################################################################
# create 100 m bins of photons
bin_size <- 1000   # meters

buchanan$track_bin <- floor(buchanan$along_distance / bin_size)

track_points <- buchanan %>%
  group_by(track_bin) %>%
  summarise(
    lon = mean(longitude),
    lat = mean(latitude)
  )

track_points <- st_as_sf(track_points, coords = c("lon","lat"), crs = 4326)

bbox <- c(left = -98.43690,
          bottom = 30.74567,
          right = -98.42337,
          top = 30.86624)
start_date <- "2024-09-09"
end_date <- "2024-09-11"
#buch_stars <- get_lst(bbox, start_date, end_date)

r_buch <- rast(buch_stars)
plot(r_buch)
track_points$NDTI <- terra::extract(r_buch, vect(track_points))[,3]
track_points <- track_points |>
  filter(NDTI < -0.05)
photon_stats <- buchanan %>%
  group_by(track_bin) %>%
  summarise(
    photon_count = n(),
    max_depth = max(height, na.rm = TRUE),
    mean_depth = mean(height, na.rm = TRUE),
    depth_sd = sd(height, na.rm = TRUE),
    deep_fraction = mean(height > 1, na.rm = TRUE)
  )
data <- merge(photon_stats, st_drop_geometry(track_points), by="track_bin")
data <- na.omit(data)

plot(data$depth_sd)
plot(data$NDTI)
cor(data$NDTI, data$depth_sd)
mod <- lm(depth_sd ~ NDTI, data=data)
summary(mod)

ggplot(data, aes(x = NDTI, y = depth_sd)) +
  geom_point() +
  theme_classic() +
  labs(y = "Depth Stdev", title = "Lake Buchanan")
cor(data$NDTI, data$depth_sd)



################################################################################
# AMAZON
################################################################################

# create 100 m bins of photons
bin_size <- 500   # meters

amazon$track_bin <- floor(amazon$along_distance / bin_size)

track_points <- amazon %>%
  group_by(track_bin) %>%
  summarise(
    lon = mean(longitude),
    lat = mean(latitude)
  )

track_points <- st_as_sf(track_points, coords = c("lon","lat"), crs = 4326)

bbox <- c(left = -58.78216,
          bottom = -3.409101,
          right = -58.77421,
          top = -3.329561)
start_date <- "2022-09-03"
end_date <- "2022-09-05"
#amaz_stars <- get_lst(bbox, start_date, end_date)

r_amaz <- rast(amaz_stars)
plot(r_amaz)
track_points$NDTI <- terra::extract(r_amaz, vect(track_points))[,3]
track_points <- track_points |>
  filter(NDTI > -0.05)
photon_stats <- amazon %>%
  group_by(track_bin) %>%
  summarise(
    photon_count = n(),
    max_depth = max(height, na.rm = TRUE),
    mean_depth = mean(height, na.rm = TRUE),
    depth_sd = sd(height, na.rm = TRUE),
    deep_fraction = mean(height > 1, na.rm = TRUE)
  )
data <- merge(photon_stats, st_drop_geometry(track_points), by="track_bin")
data <- na.omit(data)

plot(data$depth_sd)
plot(data$NDTI)
cor(data$NDTI, data$depth_sd)
mod <- lm(depth_sd ~ NDTI, data=data)
summary(mod)

ggplot(data, aes(x = NDTI, y = depth_sd)) +
  geom_point() +
  theme_classic() +
  labs(y = "Depth Stdev", title = "Amazon Conf")
cor(data$NDTI, data$depth_sd)


# SECOND DATE
amazon_w$track_bin <- floor(amazon_w$along_distance / bin_size)

track_points <- amazon_w %>%
  group_by(track_bin) %>%
  summarise(
    lon = mean(longitude),
    lat = mean(latitude)
  )

track_points <- st_as_sf(track_points, coords = c("lon","lat"), crs = 4326)

bbox <- c(left = -58.77592,
          bottom = -3.403489,
          right = -58.76778,
          top = -3.321951)
start_date <- "2024-08-13"
end_date <- "2024-08-15"
#amaz_w_stars <- get_lst(bbox, start_date, end_date)

r_amaz_w <- rast(amaz_w_stars)
plot(r_amaz_w)
track_points$NDTI <- terra::extract(r_amaz_w, vect(track_points))[,3]
track_points <- track_points |>
  filter(NDTI > -0.05)
photon_stats <- amazon_w %>%
  group_by(track_bin) %>%
  summarise(
    photon_count = n(),
    max_depth = max(height, na.rm = TRUE),
    mean_depth = mean(height, na.rm = TRUE),
    depth_sd = sd(height, na.rm = TRUE),
    deep_fraction = mean(height > 1, na.rm = TRUE)
  )
data <- merge(photon_stats, st_drop_geometry(track_points), by="track_bin")
data <- na.omit(data)

plot(data$depth_sd)
plot(data$NDTI)
cor(data$NDTI, data$depth_sd)
mod <- lm(depth_sd ~ NDTI, data=data)
summary(mod)

ggplot(data, aes(x = NDTI, y = depth_sd)) +
  geom_point() +
  theme_classic() +
  labs(y = "Depth Stdev", title = "Amazon Conf")
cor(data$NDTI, data$depth_sd)




################################################################################
# OHIO MISSISSIPPI
################################################################################
bin_size <- 100   # meters
cairo$track_bin <- floor(cairo$along_distance / bin_size)

track_points <- cairo %>%
  group_by(track_bin) %>%
  summarise(
    lon = mean(longitude),
    lat = mean(latitude)
  )

track_points <- st_as_sf(track_points, coords = c("lon","lat"), crs = 4326)

bbox <- c(left = -89.13566,
          bottom = 36.97817,
          right = -89.13398,
          top = 36.99219)
start_date <- "2024-04-23"
end_date <- "2024-04-25"
#cairo_stars <- get_lst(bbox, start_date, end_date)

r_cairo <- rast(cairo_stars)
plot(r_cairo)
track_points$NDTI <- terra::extract(r_cairo, vect(track_points))[,3]
track_points <- track_points |>
  filter(NDTI > -0.05)
photon_stats <- cairo %>%
  group_by(track_bin) %>%
  summarise(
    photon_count = n(),
    max_depth = max(height, na.rm = TRUE),
    mean_depth = mean(height, na.rm = TRUE),
    depth_sd = sd(height, na.rm = TRUE),
    deep_fraction = mean(height > 1, na.rm = TRUE)
  )
data <- merge(photon_stats, st_drop_geometry(track_points), by="track_bin")
data <- na.omit(data)

plot(data$depth_sd)
plot(data$NDTI)
cor(data$NDTI, data$depth_sd)
mod <- lm(depth_sd ~ NDTI, data=data)
summary(mod)

ggplot(data, aes(x = NDTI, y = depth_sd)) +
  geom_point() +
  theme_classic() +
  labs(y = "Depth Stdev", title = "Ohio Conf")
cor(data$NDTI, data$depth_sd)




###### second date
bin_size <- 100
cairo_working$track_bin <- floor(cairo_working$along_distance / bin_size)

track_points <- cairo_working %>%
  group_by(track_bin) %>%
  summarise(
    lon = mean(longitude),
    lat = mean(latitude)
  )

track_points <- st_as_sf(track_points, coords = c("lon","lat"), crs = 4326)

bbox <- c(left = -89.15264,
          bottom = 36.97464,
          right = -89.14965,
          top = 36.99957)
start_date <- "2024-07-17"
end_date <- "2024-07-19"
#cairo_w_stars <- get_lst(bbox, start_date, end_date)

r_cairo_w <- rast(cairo_w_stars)
plot(r_cairo_w)
track_points$NDTI <- terra::extract(r_cairo_w, vect(track_points))[,3]
track_points <- track_points |>
  filter(NDTI < -0.1)
photon_stats <- cairo_working %>%
  group_by(track_bin) %>%
  summarise(
    photon_count = n(),
    max_depth = max(height, na.rm = TRUE),
    mean_depth = mean(height, na.rm = TRUE),
    depth_sd = sd(height, na.rm = TRUE),
    deep_fraction = mean(height > 1, na.rm = TRUE)
  )
data <- merge(photon_stats, st_drop_geometry(track_points), by="track_bin")
data <- na.omit(data)

plot(data$depth_sd)
plot(data$NDTI)
cor(data$NDTI, data$depth_sd)
mod <- lm(depth_sd ~ NDTI, data=data)
summary(mod)

ggplot(data, aes(x = NDTI, y = depth_sd)) +
  geom_point() +
  theme_classic() +
  labs(y = "Depth Stdev", title = "Ohio Conf")
cor(data$NDTI, data$depth_sd)




