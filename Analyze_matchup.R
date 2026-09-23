###############################################################################
# Analysis of ICESat-2 / HLS NDTI matchup output
###############################################################################

require(pacman)
p_load(tidyverse, broom)

OUT_CSV <- "icesat2_ndti_matchup.csv"
OUT_PDF <- "matchup_ndti_vs_photons.pdf"
CANDIDATE_RDS <- "lakes_candidates.rds"  
N_LAKES      <- 100 
MIN_BINS_PER_MATCHUP <- 10
MIN_MATCHUPS_PER_LAKE <- 2

DROP_WINTER <- FALSE
WINTER_MONTHS <- 1:3

theme_set(theme_classic(base_size = 12))

###############################################################################
# Load
###############################################################################

dat <- read_csv(OUT_CSV, show_col_types = FALSE)

# drop non-finite or out of range NDTI (|NDTI| > 1)
dropped <- sum(!is.finite(dat$NDTI) | abs(dat$NDTI) > 1, na.rm = TRUE)
if (dropped > 0) message("Dropping ", dropped, " rows with non-finite or |NDTI|>1.")
dat <- dat |> filter(!(!is.finite(NDTI) | abs(NDTI) > 1))
dat <- dat |>
  mutate(icesat_date = as.Date(icesat_date)) |>
  filter(!DROP_WINTER | !month(icesat_date) %in% WINTER_MONTHS) |>
  mutate(
    matchup_id     = paste(hylak_id, trackId, icesat_date, sep = "_"),
    photon_density = photon_count / bin_size_m * 1000  # photons per km
  )

message(sprintf("%d bins | %d matchups | %d lakes",
                nrow(dat), n_distinct(dat$matchup_id), n_distinct(dat$hylak_id)))

lakes_all <- readRDS(CANDIDATE_RDS)

set.seed(10)
lakes_shuffled <- lakes_all[sample(nrow(lakes_all)), ]

complete_ids <- dat |>
  distinct(hylak_id, trackId, icesat_date) |>
  count(hylak_id) |>
  filter(n >= MIN_MATCHUPS_PER_LAKE) |>
  arrange(match(hylak_id, lakes_shuffled$Hylak_id)) |>
  pull(hylak_id) |>
  head(N_LAKES)

dat <- dat |> filter(hylak_id %in% complete_ids)
message(sprintf("Seed-5 sample: %d bins | %d matchups | %d lakes",
                nrow(dat), n_distinct(dat$matchup_id), n_distinct(dat$hylak_id)))

###############################################################################
# bin level within each matchup
###############################################################################
matchup_cors <- dat |>
  group_by(matchup_id, hylak_id, icesat_date) |>
  filter(n() >= MIN_BINS_PER_MATCHUP) |>
  summarise(n_bins = n(),
            r = cor(NDTI, photon_count, method = "spearman"),
            .groups = "drop")

message(sprintf("\nWithin-matchup NDTI ~ photon count (Spearman), n matchups = %d:", nrow(matchup_cors)))
print(summary(matchup_cors$r))

p1 <- ggplot(matchup_cors, aes(x = r)) +
  geom_histogram(bins = 20, colour = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(title = "Within-matchup correlations: NDTI vs photon count",
       subtitle = sprintf("Spearman r per matchup (>= %d bins)", MIN_BINS_PER_MATCHUP),
       x = "Spearman r", y = "matchups")

###############################################################################
# bin level within each matchup - NDTI vs depth SD (spatial)
###############################################################################

matchup_sd_cors <- dat |>
  group_by(matchup_id, hylak_id, icesat_date) |>
  filter(n() >= MIN_BINS_PER_MATCHUP) |>
  summarise(n_bins = n(),
            r = suppressWarnings(cor(NDTI, depth_sd, method = "spearman")),
            .groups = "drop")

message(sprintf("\nWithin-matchup NDTI ~ depth SD (Spearman), n matchups = %d:",
                nrow(matchup_sd_cors)))
print(summary(matchup_sd_cors$r))

p1b <- ggplot(matchup_sd_cors, aes(x = r)) +
  geom_histogram(bins = 20, colour = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(title = "Within-matchup correlations: NDTI vs photon depth SD",
       subtitle = sprintf("Spearman r per matchup (>= %d bins)", MIN_BINS_PER_MATCHUP),
       x = "Spearman r", y = "matchups")

lake_sd_cors <- matchup_sd_cors |>
  group_by(hylak_id) |>
  summarise(n_matchups = n(),
            n_neg      = sum(r < 0, na.rm = TRUE),
            n_pos      = sum(r > 0, na.rm = TRUE),
            n_zero     = sum(r == 0, na.rm = TRUE),
            median_r   = median(r, na.rm = TRUE),
            .groups = "drop") |>
  mutate(dominant = case_when(
    median_r < 0 ~ "negative",
    median_r > 0 ~ "positive",
    TRUE         ~ "none"))

###############################################################################
# day level - one point per matchup, connected within lake
###############################################################################
matchups <- dat |>
  # mean depth standard deviation per matchup
  dplyr::group_by(hylak_id, lake_name, trackId, icesat_date, matchup_id) |>
  summarise(
    n_bins        = n(),
    bin_size_m    = first(bin_size_m),
    photons_total = sum(photon_count),
    mean_ndti     = mean(NDTI),
    mean_density  = mean(photon_density),
    mean_depth_sd = mean(depth_sd),
    .groups = "drop")

p2 <- ggplot(matchups, aes(x = mean_ndti, y = mean_depth_sd)) +
  geom_line(aes(group = hylak_id), alpha = 0.4) +
  geom_point(alpha = 0.8) +
  labs(title = "Mean NDTI vs mean depth SD, one point per matchup day",
       subtitle = "lines connect days of the same lake",
       x = "Mean NDTI",
       y = "Mean Depth SD")

###############################################################################
# within-lake co-variation
###############################################################################

within <- matchups |>
  dplyr::group_by(hylak_id) |>
  mutate(ndti_c = mean_ndti   - mean(mean_ndti),
         dens_c = log10(mean_density) - mean(log10(mean_density))) |>
  ungroup()

within_fit <- lm(ndti_c ~ dens_c, data = within)
message("\nWithin-lake regression: centred NDTI ~ centred log10(density)")
print(tidy(within_fit, conf.int = TRUE))

p3 <- ggplot(within, aes(x = dens_c, y = ndti_c)) +
  geom_hline(yintercept = 0, colour = "grey85") +
  geom_vline(xintercept = 0, colour = "grey85") +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Within-lake anomalies: photon density vs NDTI",
       subtitle = "each point is a matchup day, centred on its lake's mean",
       x = "log10 photon density anomaly",
       y = "NDTI anomaly")

# per-lake Spearman correlation across that lake's matchup days
lake_cors <- matchups |>
  # per-lake Spearman correlation between NDTI and depth SD
  dplyr::group_by(hylak_id) |>
  filter(n() >= MIN_MATCHUPS_PER_LAKE) |>
  summarise(
    n_days   = n(),
    r        = cor(mean_ndti, mean_depth_sd, method = "spearman"),
    ndti_min = min(mean_ndti), ndti_max = max(mean_ndti),
    .groups = "drop") |>
  arrange(r)

lake_counts <- lake_cors %>%
  summarise(
    total = n(),
    neg_NDTI_small_sd = sum(r > 0, na.rm = TRUE),   # more negative NDTI with smaller sd (positive r)
    neg_NDTI_large_sd = sum(r < 0, na.rm = TRUE),   # more negative NDTI with larger sd (negative r)
    no_corr = sum(r == 0, na.rm = TRUE)
  )
print(lake_counts)


message("\nPer-lake across-days correlations:")
print(lake_cors, n = Inf)

p4 <- matchups |>
  semi_join(lake_cors, by = "hylak_id") |>
  ggplot(aes(x = mean_density, y = mean_ndti)) +
  geom_line(aes(group = hylak_id), alpha = 0.4) +
  geom_point(alpha = 0.8) +
  facet_wrap(~ factor(hylak_id, levels = lake_cors$hylak_id),
             scales = "free") +
  scale_x_log10() +
  labs(title = "Day-to-day NDTI vs photon density, per lake",
       subtitle = sprintf("lakes with >= %d matchups, ordered by r",
                          MIN_MATCHUPS_PER_LAKE),
       x = "mean photons per km of water track (log scale)",
       y = "mean NDTI")

pdf(OUT_PDF, width = 8, height = 6)
print(p1); print(p1b); print(p2); print(p3); print(p4)
dev.off()
message("\nPlots written to ", OUT_PDF)

###############################################################################
# summary correlation
###############################################################################

# bin-level (spatial): pool every matchup with enough bins
sd_bin_counts <- matchup_sd_cors |>
  summarise(total = n(),
            neg = sum(r < 0, na.rm = TRUE),
            pos = sum(r > 0, na.rm = TRUE),
            zero = sum(r == 0, na.rm = TRUE))

# lake-level (spatial): a lake counts by the sign of its median matchup r
sd_lake_counts <- lake_sd_cors |>
  summarise(total = n(),
            neg = sum(dominant == "negative"),
            pos = sum(dominant == "positive"),
            none = sum(dominant == "none"))

cat("NDTI vs photon depth SD - per-bin (within-matchup) Spearman\n")
cat(sprintf("matchups = %d | bins per matchup >= %d | winter %s\n",
            sd_bin_counts$total, MIN_BINS_PER_MATCHUP,
            if (DROP_WINTER) "excluded" else "included"))
cat(sprintf("negative NDTI <-> smaller depth SD (r > 0): %d matchups\n",
            sd_bin_counts$pos))
cat(sprintf("negative NDTI <-> larger  depth SD (r < 0): %d matchups\n",
            sd_bin_counts$neg))
cat(sprintf("no correlation                (r == 0): %d matchups\n",
            sd_bin_counts$zero))
cat(sprintf("total: %d matchups\n", sd_bin_counts$total))
cat(sprintf("per lake, by sign of median matchup r: %d negative | %d positive | %d none\n",
            sd_lake_counts$neg, sd_lake_counts$pos, sd_lake_counts$none))

cat("NDTI vs photon depth SD - per-lake across-days Spearman (temporal)\n")
cat(sprintf("lakes = %d | matchups per lake >= %d | winter %s\n",
            nrow(lake_cors), MIN_MATCHUPS_PER_LAKE,
            if (DROP_WINTER) "excluded" else "included"))
cat(sprintf("negative NDTI <-> smaller depth SD (r > 0): %d lakes\n",
            lake_counts$neg_NDTI_small_sd))
cat(sprintf("negative NDTI <-> larger  depth SD (r < 0): %d lakes\n",
            lake_counts$neg_NDTI_large_sd))
cat(sprintf("no correlation                (r == 0): %d lakes\n",
            lake_counts$no_corr))
cat(sprintf("total: %d lakes\n", lake_counts$total))

list(bins = dat, matchups = matchups,
     matchup_cors = matchup_cors, matchup_sd_cors = matchup_sd_cors,
     lake_cors = lake_cors, lake_sd_cors = lake_sd_cors,
     lake_counts = lake_counts, within_fit = within_fit)

library(patchwork)
(p1 + p1b) / (p2 + p4)
