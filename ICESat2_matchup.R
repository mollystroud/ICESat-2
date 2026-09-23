###############################################################################
# ICESat-2 / HLS NDTI matchup
# Randomly selects lakes from HydroLAKES that are crossed by ICESat-2 ground
# tracks, finds overpass dates that coincide with HLS (Sentinel-2) imagery
# within +/- 3 days, downloads both, then bins ATL03 photons along-track and
# matches each bin to NDTI extracted from the matched HLS image
###############################################################################

require(pacman)
p_load(rstac, gdalcubes, sf, stars, terra, tidyverse, geosphere, IceSat2R)

# User parameters
set.seed(10)
N_LAKES      <- 100
MIN_LAKE_KM2 <- 10
DAY_WINDOW   <- 3 # +/- days between ICESat-2 overpass and HLS image
TARGET_BINS  <- 30
DT_SEARCH    <- c("2022-01-01", as.character(Sys.Date())) 
CLOUD_MAX    <- 30
MIN_MATCHUPS_PER_LAKE <- 2

TRACK_DIR <- "ICESat2_groundtracks_WesternHem"   # holds per-cycle .kmz files
GDB       <- "HydroLAKES_polys_v10.gdb/HydroLAKES_polys_v10.gdb"
GDB_LAYER <- "HydroLAKES_polys_v10"
OUT_CSV   <- "icesat2_ndti_matchup_seed50_100lakes.csv"
PHOTON_DIR <- "matchup_photons"                  # per-matchup raw ATL03 CSVs
CANDIDATE_RDS <- "lakes_candidates.rds"          # cached lake/track screen
dir.create(PHOTON_DIR, showWarnings = FALSE)

pc <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

###############################################################################
# Function 1: read ground tracks and find lakes that tracks cross
###############################################################################
read_ground_tracks <- function(track_dir) {
  files <- list.files(track_dir, pattern = "\\.kmz$", full.names = TRUE)
  stopifnot("No .kmz files found in TRACK_DIR" = length(files) > 0)

  trk_list <- lapply(files, function(f) {
    base <- tools::file_path_sans_ext(basename(f))
    cycle <- stringr::str_extract(base, "repeat[0-9]+")
    beam  <- stringr::str_extract(base, "GT[0-9][LR0-9]")
    obj <- tryCatch(
      st_zm(st_read(paste0("/vsizip/", f), quiet = TRUE)),
      error = function(e) {
        tmp <- tempfile(fileext = ".kml")
        unzip(f, exdir = tempdir(), overwrite = TRUE)
        kml <- file.path(tempdir(), list.files(tempdir(), "\\.kml$")[1])
        st_zm(st_read(kml, quiet = TRUE))
      }
    )
    obj$track_cycle <- cycle
    obj$beam        <- beam
    obj[, c("Name", "track_cycle", "beam")]
  })
  do.call(rbind, trk_list)
}

# Spatial join: which HydroLAKES polygons (>= MIN_LAKE_KM2) does a track cross?
find_lakes_with_tracks <- function(gdb, layer, tracks, min_area) {
  if (!sf_use_s2()) sf_use_s2(TRUE)

  message("Decomposing ground tracks to vertices...")
  verts <- st_cast(st_geometry(tracks), "POINT", warn = FALSE)

  big_ids <- unique(st_read(
    gdb, layer = layer, quiet = TRUE,
    query = sprintf("SELECT Hylak_id, Lake_area FROM %s WHERE Lake_area > %f",
                    layer, min_area)
  )$Hylak_id)
  message(length(big_ids), " lakes > ", min_area, " km2; screening for track crossings...")

  chunk  <- 1000
  hits   <- list()
  for (i in seq(1, length(big_ids), by = chunk)) {
    ids  <- big_ids[i:min(i + chunk - 1, length(big_ids))]
    q    <- sprintf("SELECT Hylak_id, Lake_name, Lake_area, Pour_long, Pour_lat FROM %s WHERE Hylak_id IN (%s)",
                    layer, paste(ids, collapse = ","))
    lake <- st_read(gdb, query = q, quiet = TRUE)
    lake <- st_make_valid(lake)
    lake <- st_collection_extract(lake, "POLYGON", warn = FALSE)
    m    <- st_within(verts, st_geometry(lake))        # per-vertex lake index
    ok   <- tabulate(unlist(m), nbins = nrow(lake)) > 0
    if (any(ok)) hits[[length(hits) + 1]] <- lake[ok, ]
    message(sprintf("  chunk %d-%d: %d hits", i, min(i + chunk - 1, length(big_ids)), sum(ok)))
  }
  out <- do.call(rbind, hits)
  out <- out[!duplicated(out$Hylak_id), ]
  message(nrow(out), " lakes are crossed by an ICESat-2 track.")
  out
}

###############################################################################
# Function 2: for one lake, find ICESat-2 overpasses that have HLS imagery
# within +/- x days
###############################################################################
hls_dates_for_bbox <- function(bbox) {
  cloud_max <- CLOUD_MAX
  it <- pc |>
    stac_search(collections = "hls2-s30",
                bbox = as.numeric(bbox),
                datetime = paste(DT_SEARCH, collapse = "/"),
                limit = 1000) |>
    rstac::ext_query("eo:cloud_cover" < cloud_max) |>
    post_request() |>
    items_sign(sign_fn = sign_planetary_computer()) |>
    items_fetch()
  if (length(it$features) == 0) return(as.Date(character(0)))
  sort(unique(as.Date(substr(vapply(it$features,
                                    function(x) x$properties$datetime,
                                    character(1)), 1, 10))))
}

# Find RGT/Date pairs that cross this lake, keep those with nearby HLS imagery
find_matched_overpasses <- function(lake_row,
                                    probe_step_days = 30,
                                    rgt_repeat_days = 91,
                                    max_matchups = 8) {
  bb <- st_bbox(lake_row)
  bbv <- c(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])

  probe_tracks <- function(d) {
    res <- tryCatch(
      getTracks(minx = bbv[1], miny = bbv[2], maxx = bbv[3], maxy = bbv[4],
                date = as.character(d), outputFormat = "csv",
                download_method = "curl", verbose = FALSE),
      error = function(e) NULL
    )
    if (is.null(res) || nrow(res) == 0) return(data.frame(track = integer(0),
                                                          probe_date = as.Date(character(0))))
    data.frame(track = res$track, probe_date = as.Date(d))
  }

  probes <- seq(as.Date(DT_SEARCH[1]), as.Date(DT_SEARCH[2]),
                by = probe_step_days)
  hits <- do.call(rbind, lapply(probes, probe_tracks))
  if (nrow(hits) == 0) return(NULL)

  start <- as.Date(DT_SEARCH[1]); end <- as.Date(DT_SEARCH[2])
  expanded <- do.call(rbind, lapply(seq_len(nrow(hits)), function(i) {
    rgt <- hits$track[i]; anchor <- hits$probe_date[i]
    ks <- seq(floor(as.numeric(start - anchor) / rgt_repeat_days),
              ceiling(as.numeric(end - anchor) / rgt_repeat_days))
    dates <- anchor + ks * rgt_repeat_days
    dates <- dates[dates >= start & dates <= end]
    data.frame(track = rgt, icesat_date = dates)
  }))
  expanded <- dplyr::distinct(expanded, track, icesat_date)

  hls_days <- hls_dates_for_bbox(bbv)
  if (length(hls_days) == 0) return(NULL)
  keep <- expanded[vapply(expanded$icesat_date,
                          function(d) any(abs(hls_days - d) <= DAY_WINDOW),
                          logical(1)), ]
  if (nrow(keep) == 0) return(NULL)

  keep <- keep[order(keep$icesat_date), ]
  verified_rows <- list()
  for (i in seq_len(nrow(keep))) {
    res <- tryCatch(
      getTracks(minx = bbv[1], miny = bbv[2], maxx = bbv[3], maxy = bbv[4],
                date = as.character(keep$icesat_date[i]), outputFormat = "csv",
                download_method = "curl", verbose = FALSE),
      error = function(e) NULL
    )
    if (!is.null(res) && keep$track[i] %in% res$track) {
      verified_rows[[length(verified_rows) + 1]] <- keep[i, ]
      if (length(verified_rows) >= max_matchups) break
    }
  }
  verified <- do.call(rbind, verified_rows)
  if (is.null(verified) || nrow(verified) == 0) return(NULL)

  data.frame(
    hylak_id    = lake_row$Hylak_id,
    lake_name   = lake_row$Lake_name,
    trackId     = verified$track,
    icesat_date = verified$icesat_date,
    xmin = bbv[1], ymin = bbv[2], xmax = bbv[3], ymax = bbv[4],
    stringsAsFactors = FALSE
  ) |>
    dplyr::distinct(trackId, icesat_date, .keep_all = TRUE)
}

###############################################################################
# Function 3: download ATL03 photons and HLS imagery; compute NDTI
###############################################################################
get_icesat_photons <- function(matchup) {
  dat <- get_atlas_data(
    minx = matchup$xmin, miny = matchup$ymin,
    maxx = matchup$xmax, maxy = matchup$ymax,
    date = as.character(matchup$icesat_date),
    trackId = as.character(matchup$trackId),
    beamName = c("gt1r", "gt2r", "gt3r"),
    product = "atl03", client = "portal",
    photonConfidence = c("low", "medium", "high"),
    sampling = FALSE, outputFormat = "csv",
    file_path_zip = NULL, download_method = "curl", verbose = FALSE
  )
  if (is.null(dat) || nrow(dat) < 2) return(NULL)
  dat
}

get_hls_ndti_at_points <- function(pts_sf, icesat_date, window = DAY_WINDOW) {
  bb <- st_bbox(pts_sf)
  start_date <- as.character(icesat_date - window)
  end_date   <- as.character(icesat_date + window)
  items <- pc |>
    stac_search(collections = "hls2-s30",
                bbox = as.numeric(bb),
                datetime = paste(start_date, end_date, sep = "/"),
                limit = 1000) |>
    rstac::ext_query("eo:cloud_cover" < CLOUD_MAX) |>
    post_request() |>
    items_sign(sign_fn = sign_planetary_computer()) |>
    items_fetch()
  if (length(items$features) == 0) return(NULL)

  meta <- do.call(rbind, lapply(items$features, function(f) {
    data.frame(cloud = as.numeric(f$properties$`eo:cloud_cover`),
               date  = as.Date(substr(f$properties$datetime, 1, 10)),
               b03   = f$assets$B03$href, b04 = f$assets$B04$href,
               stringsAsFactors = FALSE)
  }))
  ord <- order(meta$cloud, abs(meta$date - icesat_date))
  meta <- meta[ord, , drop = FALSE]

  pts_v <- terra::vect(pts_sf)
  for (k in seq_len(nrow(meta))) {
    out <- tryCatch({
      b03 <- terra::extract(terra::rast(meta$b03[k], vsi = TRUE), pts_v, ID = FALSE)[[1]]
      b04 <- terra::extract(terra::rast(meta$b04[k], vsi = TRUE), pts_v, ID = FALSE)[[1]]
      # HLS is scaled int16 (x 1e-4); ratio is scale-invariant as both bands share scale
      (b04 - b03) / (b04 + b03)
    }, error = function(e) NULL)
    if (!is.null(out) && any(!is.na(out))) return(out)
  }
  NULL
}

###############################################################################
# Function 4: bin photons along-track and match each bin to NDTI
###############################################################################
photons_to_ndti <- function(photons, matchup, lake_geom, lake_buffer_m = 30) {
  names(photons) <- gsub("\\.", " ", names(photons))
  colnames(photons)[colnames(photons) == "photon height"] <- "height"
  colnames(photons)[colnames(photons) == "confidence code"] <- "confidence"

  photons <- photons[photons$confidence > 3, ]
  if (nrow(photons) < 10) return(NULL)

  mask_sf <- st_as_sf(photons, coords = c("longitude", "latitude"), crs = 4326)
  over_water <- lengths(st_intersects(mask_sf, st_buffer(lake_geom, lake_buffer_m))) > 0
  photons <- photons[over_water, ]
  if (nrow(photons) < 10) return(NULL)

  photons <- mutate(photons,
                    Distance = distHaversine(cbind(longitude, latitude),
                                             cbind(lag(longitude), lag(latitude))))
  photons <- na.omit(photons)
  photons$along_distance <- cumsum(photons$Distance)

  span <- max(photons$along_distance, na.rm = TRUE)
  if (span <= 0) return(NULL)
  bin_size <- span / TARGET_BINS                 # adaptive bin size
  photons$track_bin <- floor(photons$along_distance / bin_size)

  pts_sf <- st_as_sf(photons, coords = c("longitude", "latitude"), crs = 4326)
  ndti <- get_hls_ndti_at_points(pts_sf, as.Date(matchup$icesat_date))
  if (is.null(ndti)) return(NULL)
  photons$NDTI <- ndti

  track_points <- photons |>
    group_by(track_bin) |>
    summarise(lon = mean(longitude), lat = mean(latitude), .groups = "drop")

  photon_stats <- photons |>
    group_by(track_bin) |>
    summarise(photon_count = n(),
              max_depth    = max(height, na.rm = TRUE),
              mean_depth   = mean(height, na.rm = TRUE),
              depth_sd     = sd(height,  na.rm = TRUE),
              deep_fraction = mean(height > 1, na.rm = TRUE),
              NDTI         = mean(NDTI, na.rm = TRUE),
              .groups = "drop")

  data <- merge(photon_stats, track_points, by = "track_bin")
  data <- data[complete.cases(data), ]
  if (nrow(data) == 0) return(NULL)

  data$hylak_id    <- matchup$hylak_id
  data$lake_name   <- matchup$lake_name
  data$trackId     <- matchup$trackId
  data$icesat_date <- as.character(matchup$icesat_date)
  data$bin_size_m  <- bin_size
  # align with OUT_CSV header so appended rows land in the right columns
  data <- data[, c("track_bin", "photon_count", "max_depth", "mean_depth",
                   "depth_sd", "deep_fraction", "NDTI", "hylak_id", "lake_name",
                   "trackId", "icesat_date", "bin_size_m", "lon", "lat")]
  data
}

###############################################################################
# Run everything
###############################################################################

run_matchup <- function() {
  if (file.exists(CANDIDATE_RDS)) {
    message("Loading cached candidate lakes from ", CANDIDATE_RDS)
    lakes <- readRDS(CANDIDATE_RDS)
  } else {
    tracks <- read_ground_tracks(TRACK_DIR)
    lakes  <- find_lakes_with_tracks(GDB, GDB_LAYER, tracks, MIN_LAKE_KM2)
    saveRDS(lakes, CANDIDATE_RDS)
  }
  
  lakes <- lakes[sample(nrow(lakes)), ]

  if (!file.exists(OUT_CSV)) {
    write_csv(data.frame(
      track_bin = numeric(), photon_count = numeric(), max_depth = numeric(),
      mean_depth = numeric(), depth_sd = numeric(), deep_fraction = numeric(),
      NDTI = numeric(), hylak_id = numeric(), lake_name = character(),
      trackId = numeric(), icesat_date = character(), bin_size_m = numeric(),
      lon = numeric(), lat = numeric()), OUT_CSV)
  }

  
  done_ids <- numeric(0)
  if (file.exists(OUT_CSV) && file.size(OUT_CSV) > 0) {
    prev <- suppressMessages(read_csv(OUT_CSV, show_col_types = FALSE))
    if (nrow(prev) > 0) {
      mm <- aggregate(icesat_date ~ hylak_id, data = prev,
                      FUN = function(x) length(unique(x)))
      done_ids <- mm$hylak_id[mm$icesat_date >= MIN_MATCHUPS_PER_LAKE]
    }
  }
  n_done <- length(done_ids)
  message(sprintf("Target %d lakes each with >= %d matchups; %d already complete.",
                  N_LAKES, MIN_MATCHUPS_PER_LAKE, n_done))

  for (i in seq_len(nrow(lakes))) {
    if (n_done >= N_LAKES) break
    lk <- lakes[i, ]
    if (lk$Hylak_id %in% done_ids) next
    message(sprintf("[lake %d | %d done] %s (Hylak %s)", i, n_done,
                    lk$Lake_name, lk$Hylak_id))

    matchups <- tryCatch(
      { mm <- find_matched_overpasses(lk); if (is.null(mm)) message("  (matcher returned NULL)"); mm },
      error = function(e) { message("  matcher error: ", conditionMessage(e)); NULL })
    if (is.null(matchups) || nrow(matchups) == 0) { message("  no matched overpasses"); next }

    bb <- st_bbox(lk)
    bbox <- c(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])

    n_mm_saved <- 0
    existing_matchups <- character(0)
    
    if (file.exists(OUT_CSV) && file.size(OUT_CSV) > 0) {
      prev <- suppressMessages(read_csv(OUT_CSV, show_col_types = FALSE))
      prev_lake <- prev[prev$hylak_id == lk$Hylak_id, ]
      if (nrow(prev_lake) > 0) {
        existing_matchups <- unique(paste(prev_lake$trackId, prev_lake$icesat_date))
        n_mm_saved <- length(existing_matchups)
      }
    }
    for (j in seq_len(nrow(matchups))) {
      remaining <- nrow(matchups) - j + 1
      if (n_mm_saved < MIN_MATCHUPS_PER_LAKE && n_mm_saved + remaining < MIN_MATCHUPS_PER_LAKE) break
      m <- matchups[j, ]
      if (paste(m$trackId, m$icesat_date) %in% existing_matchups) next  
      photons <- tryCatch(get_icesat_photons(m), error = function(e) NULL)
      if (is.null(photons)) next

      fname <- file.path(PHOTON_DIR, sprintf("%s_%s_%s.csv", m$hylak_id, m$trackId, m$icesat_date))
      if (!file.exists(fname)) write_csv(photons, fname)

      out <- tryCatch(photons_to_ndti(photons, m, st_geometry(lk)),
                      error = function(e) { message("  ndti error: ", conditionMessage(e)); NULL })
      if (is.null(out) || nrow(out) == 0) next

      write_csv(out, OUT_CSV, append = TRUE)
      n_mm_saved <- n_mm_saved + 1
      message(sprintf("  saved %d bins (%s, %s) [%d/%d matchups]",
                      nrow(out), m$trackId, m$icesat_date, n_mm_saved, MIN_MATCHUPS_PER_LAKE))
    }
    if (n_mm_saved >= MIN_MATCHUPS_PER_LAKE) {
      n_done <- n_done + 1
      done_ids <- c(done_ids, lk$Hylak_id)
      message(sprintf("  -> lake complete (%d/%d)", n_done, N_LAKES))
    } else if (n_mm_saved > 0) {
      message(sprintf("  (only %d matchup(s) - < %d; lake does not count)", n_mm_saved, MIN_MATCHUPS_PER_LAKE))
    }
  }
  message(sprintf("Finished with %d/%d lakes (each with >= %d matchups).",
                  n_done, N_LAKES, MIN_MATCHUPS_PER_LAKE))
  invisible(read_csv(OUT_CSV, show_col_types = FALSE))
}

#run_matchup()