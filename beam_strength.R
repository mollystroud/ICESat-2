###############################################################################
# Beam strength (strong vs weak) for ICESat-2 ATL03 beams
#
# ATL03 ATBD 7.5: beam strength is set by the observatory yaw (sc_orient), NOT
# by the ground-track number. The observatory alternates between forward and
# backward orientations; each time it flips, the strong/weak beam pairs swap
# sides:
#   sc_orient == 1 (forward) : right beams strong -> gt1r, gt2r, gt3r
#   sc_orient == 0 (backward): left  beams strong -> gt1l, gt2l, gt3l
# Flip times are from the NSIDC ICESat-2 Major Activities table.
###############################################################################

YAW_FLIPS <- data.frame(
  datetime  = as.POSIXct(c("2018-12-28 18:53:08", "2019-09-07 01:04:06",
                           "2020-05-14 01:49:03", "2021-01-15 15:17:01",
                           "2021-10-02 02:20:01", "2022-06-09 01:31:19",
                           "2023-02-09 16:59:14", "2023-10-27 13:26:50",
                           "2024-06-25 09:45:16", "2025-03-03 14:47:04",
                           "2025-11-20 00:57:40", "2026-07-21 14:08:40"), tz = "UTC"),
  sc_orient = c(0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L),
  stringsAsFactors = FALSE)

# Observatory yaw for an overpass: 1 = forward, 0 = backward, NA before launch.
# Dates are date-only stamps, so evaluate at midday UTC to stay on the correct
# side of any same-day flip.
yaw_orientation <- function(date) {
  t <- as.POSIXct(date, tz = "UTC") + 12 * 3600
  idx <- findInterval(t, YAW_FLIPS$datetime)
  ifelse(idx == 0L, NA_integer_, YAW_FLIPS$sc_orient[idx])
}

# Which side (l/r) is strong on a given overpass date
strong_side <- function(date) {
  o <- yaw_orientation(date)
  ifelse(is.na(o), NA_character_, ifelse(o == 1L, "r", "l"))
}

# Ground-track names that are strong on a given overpass date
strong_beams_for <- function(date) {
  o <- yaw_orientation(date)
  ifelse(is.na(o), NA_character_, ifelse(o == 1L, "gt*r", "gt*l"))
}

# Strength of a single beam (e.g. "gt2r") on a given overpass date
beam_strength <- function(beam, date) {
  o    <- yaw_orientation(date)
  side <- substr(beam, 4, 4)
  ifelse(is.na(o), NA_character_,
         ifelse(side == ifelse(o == 1L, "r", "l"), "strong", "weak"))
}
