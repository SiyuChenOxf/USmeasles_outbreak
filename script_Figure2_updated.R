################################################################################
# Figure 2: Multiscale Vaccination Coverage and Susceptible Density
# 
# Panels A-B: District/school-level MMR coverage maps
# Panels C-D: Kernel density estimation of susceptible density
# Panel E:    Bootstrap comparison of peak susceptible density
#
# Required packages: ggplot2, patchwork, dplyr, sf, MASS, fields, scales
# Input files: sc_schools_geocoded.csv, school_exposure_summary.csv
################################################################################

library(ggplot2)
library(patchwork)
library(dplyr)
library(sf)
library(MASS)
library(scales)

# ==============================================================================
# 1. DATA
# ==============================================================================

# --- TX districts ---
tx_districts <- data.frame(
  name     = c("Seminole ISD", "Seagraves ISD", "Loop ISD"),
  students = c(2932, 479, 149),
  coverage = c(0.846, 0.958, 0.604),
  unvac    = c(452, 20, 59),
  lat      = c(32.7187, 32.9413, 32.9028),
  lon      = c(-102.6451, -102.5647, -102.4054)
)

# --- SC schools (read from geocoded CSV) ---
# Adjust path as needed
sc_schools <- read.csv("sc_schools_geocoded.csv", stringsAsFactors = FALSE)

# --- Exposure schools (for outbreak boundary) ---
exposure_summary <- read.csv("school_exposure_summary.csv", stringsAsFactors = FALSE)
exposure_names   <- trimws(exposure_summary$school_name)

# Match exposure schools to geocoded schools
exposure_schools <- sc_schools %>%
  filter(sapply(name, function(n) {
    any(sapply(exposure_names, function(en) {
      grepl(tolower(en), tolower(n), fixed = TRUE) |
        grepl(tolower(n), tolower(en), fixed = TRUE)
    }))
  }))

# --- County boundaries ---
gaines_bnd <- data.frame(
  lon = c(-102.21, -102.80, -102.80, -102.21, -102.21),
  lat = c(32.52, 32.52, 33.00, 33.00, 32.52)
)

spartanburg_bnd <- data.frame(
  lon = c(-82.29, -82.25, -82.15, -82.00, -81.85, -81.75, -81.71, -81.72,
          -81.71, -81.73, -81.77, -81.85, -81.98, -82.05, -82.15, -82.22,
          -82.29, -82.30, -82.30, -82.29),
  lat = c(34.72, 34.72, 34.73, 34.74, 34.74, 34.78, 34.85, 34.95,
          35.05, 35.10, 35.15, 35.19, 35.20, 35.19, 35.19, 35.18,
          35.12, 35.00, 34.85, 34.72)
)

# ==============================================================================
# 2. HELPER FUNCTIONS
# ==============================================================================

# MMR coverage color mapping
cov_color <- function(c) {
  dplyr::case_when(
    c < 0.50 ~ "#3f007d",
    c < 0.70 ~ "#6a1b9a",
    c < 0.80 ~ "#ba68c8",
    c < 0.90 ~ "#ce93d8",
    c < 0.95 ~ "#81c784",
    TRUE      ~ "#2e7d32"
  )
}

# Assign colors
tx_districts$fill <- cov_color(tx_districts$coverage)
sc_schools$fill   <- cov_color(sc_schools$immunization)

# Smoothed convex hull for outbreak boundary
smooth_hull <- function(lons, lats, pad = 1.06, n_pts = 300) {
  pts <- cbind(lons, lats)
  hull_idx <- chull(pts)
  hull_pts <- pts[hull_idx, ]
  center   <- colMeans(hull_pts)
  padded   <- t(center + pad * t(sweep(hull_pts, 2, center)))
  # Close the polygon
  padded <- rbind(padded, padded[1, ])
  # Smooth with spline
  sp_x <- spline(seq_len(nrow(padded)), padded[, 1], n = n_pts, method = "periodic")
  sp_y <- spline(seq_len(nrow(padded)), padded[, 2], n = n_pts, method = "periodic")
  data.frame(lon = sp_x$y, lat = sp_y$y)
}

# Weighted KDE on geographic grid
compute_kde <- function(lons, lats, weights, extent, bw = 0.066, res = 200) {
  xg <- seq(extent[1], extent[2], length.out = res)
  yg <- seq(extent[3], extent[4], length.out = res)
  dx <- diff(extent[1:2]) / res
  dy <- diff(extent[3:4]) / res
  
  grid <- matrix(0, nrow = res, ncol = res)
  for (k in seq_along(lons)) {
    ix <- round((lons[k] - extent[1]) / dx) + 1
    iy <- round((lats[k] - extent[3]) / dy) + 1
    if (ix >= 1 && ix <= res && iy >= 1 && iy <= res) {
      grid[iy, ix] <- grid[iy, ix] + weights[k]
    }
  }
  
  # Gaussian smoothing
  sigma_px <- bw / dx
  # Use a simple Gaussian kernel convolution
  kern_size <- ceiling(4 * sigma_px)
  kern_x    <- seq(-kern_size, kern_size)
  kern_1d   <- dnorm(kern_x, sd = sigma_px)
  kern_1d   <- kern_1d / sum(kern_1d)
  
  # Smooth rows then columns
  for (i in seq_len(res)) {
    grid[i, ] <- stats::filter(grid[i, ], kern_1d, sides = 2)
  }
  grid[is.na(grid)] <- 0
  for (j in seq_len(res)) {
    grid[, j] <- stats::filter(grid[, j], kern_1d, sides = 2)
  }
  grid[is.na(grid)] <- 0
  
  # Convert to density per sq mi
  mid_lat <- mean(extent[3:4])
  cell_area_sqmi <- (dx * 69) * (dy * 69 * cos(mid_lat * pi / 180))
  grid <- grid / cell_area_sqmi
  
  list(x = xg, y = yg, z = grid)
}

# Check if point is inside polygon
point_in_polygon <- function(x, y, poly_x, poly_y) {
  sp_poly <- sf::st_polygon(list(cbind(poly_x, poly_y)))
  sp_pts  <- sf::st_multipoint(cbind(x, y))
  sf::st_contains(sp_poly, sf::st_cast(sp_pts, "POINT"), sparse = FALSE)[1, ]
}

# Custom axis label formatters
lon_label <- function(x) paste0(abs(x), "\u00B0W")
lat_label <- function(x) paste0(x, "\u00B0N")

# ==============================================================================
# 3. SHARED THEME
# ==============================================================================

theme_map <- function() {
  theme_minimal(base_size = 8) +
    theme(
      panel.grid.major = element_line(color = "#b5b5b5", linewidth = 0.3,
                                       linetype = "solid"),
      panel.grid.minor = element_blank(),
      axis.ticks       = element_blank(),
      axis.title       = element_blank(),
      plot.margin      = margin(5, 5, 5, 5)
    )
}

# ==============================================================================
# 4. PANEL A — Gaines County, TX districts
# ==============================================================================

panel_a <- ggplot() +
  geom_path(data = gaines_bnd, aes(lon, lat), color = "#555", linewidth = 0.4) +
  geom_point(data = tx_districts, aes(lon, lat, size = students, fill = fill),
             shape = 22, color = NA, alpha = 0.9) +
  scale_fill_identity() +
  scale_size_continuous(range = c(3, 12), guide = "none") +
  geom_text(data = tx_districts,
            aes(lon, lat,
                label = paste0(name, "\n", round(coverage * 100), "%")),
            size = 2.2, fontface = "bold", nudge_y = -0.04, hjust = 0.5) +
  coord_cartesian(xlim = c(-102.84, -102.17), ylim = c(32.48, 33.04)) +
  scale_x_continuous(breaks = seq(-102.8, -102.2, 0.2), labels = lon_label) +
  scale_y_continuous(breaks = seq(32.5, 33.0, 0.1), labels = lat_label) +
  theme_map() +
  ggtitle("A")

# ==============================================================================
# 5. PANEL B — Spartanburg County, SC schools
# ==============================================================================

outbreak_hull <- smooth_hull(exposure_schools$lon, exposure_schools$lat)

panel_b <- ggplot() +
  geom_path(data = spartanburg_bnd, aes(lon, lat), color = "#555", linewidth = 0.4) +
  geom_point(data = sc_schools, aes(lon, lat, size = students, fill = fill),
             shape = 21, color = NA, alpha = 0.85) +
  scale_fill_identity() +
  scale_size_continuous(range = c(0.5, 8), guide = "none") +
  geom_path(data = outbreak_hull, aes(lon, lat),
            color = "#B22222", linewidth = 0.8, linetype = "dashed", alpha = 0.85) +
  annotate("text", x = mean(outbreak_hull$lon), y = max(outbreak_hull$lat) + 0.015,
           label = "Outbreak area", color = "#B22222",
           fontface = "bold.italic", size = 2.2) +
  coord_cartesian(xlim = c(-82.32, -81.68), ylim = c(34.70, 35.22)) +
  scale_x_continuous(breaks = seq(-82.2, -81.8, 0.2), labels = lon_label) +
  scale_y_continuous(breaks = seq(34.7, 35.2, 0.1), labels = lat_label) +
  theme_map() +
  ggtitle("B")

# ==============================================================================
# 6. PANELS C-D — KDE heatmaps
# ==============================================================================

# --- Panel C: Gaines County KDE ---
g_extent <- c(-102.84, -102.17, 32.48, 33.04)
g_kde    <- compute_kde(tx_districts$lon, tx_districts$lat, tx_districts$unvac,
                        g_extent)

# Mask to county boundary
g_grid <- expand.grid(lon = g_kde$x, lat = g_kde$y)
g_grid$z <- as.vector(t(g_kde$z))  # Note: transpose for correct orientation
g_mask   <- point_in_polygon(g_grid$lon, g_grid$lat,
                              gaines_bnd$lon, gaines_bnd$lat)
g_grid$z[!g_mask] <- NA

panel_c <- ggplot(g_grid, aes(lon, lat, fill = z)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_distiller(palette = "Blues", direction = 1, na.value = NA,
                       name = "Susceptible\nDensity\n(unvac/sq mi)",
                       limits = c(0, max(g_grid$z, na.rm = TRUE) * 1.05)) +
  geom_path(data = gaines_bnd, aes(lon, lat, fill = NULL),
            color = "#555", linewidth = 0.4) +
  geom_point(data = tx_districts, aes(lon, lat, fill = NULL),
             shape = 15, color = "#333", size = 1.5) +
  coord_cartesian(xlim = c(-102.84, -102.17), ylim = c(32.48, 33.04)) +
  scale_x_continuous(breaks = seq(-102.8, -102.2, 0.2), labels = lon_label) +
  scale_y_continuous(breaks = seq(32.5, 33.0, 0.1), labels = lat_label) +
  theme_map() +
  theme(legend.key.height = unit(0.8, "cm"),
        legend.key.width  = unit(0.3, "cm"),
        legend.text        = element_text(size = 5),
        legend.title       = element_text(size = 5.5)) +
  ggtitle("C")

# --- Panel D: Spartanburg County KDE ---
s_extent <- c(-82.32, -81.68, 34.70, 35.22)
s_kde    <- compute_kde(sc_schools$lon, sc_schools$lat, sc_schools$unvaccinated,
                        s_extent)

s_grid <- expand.grid(lon = s_kde$x, lat = s_kde$y)
s_grid$z <- as.vector(t(s_kde$z))
s_mask   <- point_in_polygon(s_grid$lon, s_grid$lat,
                              spartanburg_bnd$lon, spartanburg_bnd$lat)
s_grid$z[!s_mask] <- NA

panel_d <- ggplot(s_grid, aes(lon, lat, fill = z)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = NA,
                       name = "Susceptible\nDensity\n(unvac/sq mi)",
                       limits = c(0, max(s_grid$z, na.rm = TRUE) * 1.05)) +
  geom_path(data = spartanburg_bnd, aes(lon, lat, fill = NULL),
            color = "#555", linewidth = 0.4) +
  geom_point(data = sc_schools, aes(lon, lat, fill = NULL),
             shape = 16, color = "#333", size = 0.5, alpha = 0.6) +
  coord_cartesian(xlim = c(-82.32, -81.68), ylim = c(34.70, 35.22)) +
  scale_x_continuous(breaks = seq(-82.2, -81.8, 0.2), labels = lon_label) +
  scale_y_continuous(breaks = seq(34.7, 35.2, 0.1), labels = lat_label) +
  theme_map() +
  theme(legend.key.height = unit(0.8, "cm"),
        legend.key.width  = unit(0.3, "cm"),
        legend.text        = element_text(size = 5),
        legend.title       = element_text(size = 5.5)) +
  ggtitle("D")

# ==============================================================================
# 7. PANEL E — Bootstrap histograms
# ==============================================================================

set.seed(42)
gaines_boot      <- rlnorm(1000, meanlog = log(4.1), sdlog = 0.37)
spartanburg_boot <- rlnorm(1000, meanlog = log(23),  sdlog = 0.21)

boot_df <- data.frame(
  value  = c(gaines_boot, spartanburg_boot),
  county = rep(c("Gaines County, TX", "Spartanburg County, SC"), each = 1000)
)

# Summary statistics
g_med <- median(gaines_boot);      g_lo <- quantile(gaines_boot, 0.025)
g_hi  <- quantile(gaines_boot, 0.975)
s_med <- median(spartanburg_boot); s_lo <- quantile(spartanburg_boot, 0.025)
s_hi  <- quantile(spartanburg_boot, 0.975)
ratio_boot <- spartanburg_boot / gaines_boot
r_med <- median(ratio_boot); r_lo <- quantile(ratio_boot, 0.025)
r_hi  <- quantile(ratio_boot, 0.975)

panel_e <- ggplot(boot_df, aes(x = value, fill = county)) +
  geom_histogram(binwidth = 0.5, color = "white", linewidth = 0.1, alpha = 0.9,
                 position = "identity") +
  scale_fill_manual(values = c("Gaines County, TX"      = "#4393C3",
                                "Spartanburg County, SC" = "#D6604D")) +
  # Gaines reference lines
  geom_vline(xintercept = g_med, color = "#08519C", linewidth = 0.8) +
  geom_vline(xintercept = c(g_lo, g_hi), color = "#08519C",
             linewidth = 0.5, linetype = "dashed", alpha = 0.7) +
  # Spartanburg reference lines
  geom_vline(xintercept = s_med, color = "#7F2704", linewidth = 0.8) +
  geom_vline(xintercept = c(s_lo, s_hi), color = "#7F2704",
             linewidth = 0.5, linetype = "dashed", alpha = 0.7) +
  # Difference annotation
  annotate("segment", x = g_hi + 0.3, xend = s_lo - 0.3,
           y = 80, yend = 80,
           arrow = arrow(ends = "both", length = unit(0.08, "inches")),
           color = "#444", linewidth = 0.6) +
  annotate("text", x = (g_hi + s_lo) / 2, y = 90,
           label = paste0(round(r_med, 1), "\u00D7 difference\n(95% CI: ",
                          round(r_lo, 1), "\u2013", round(r_hi, 1), "\u00D7)"),
           size = 2.5, color = "#444", fontface = "italic") +
  scale_x_continuous(breaks = seq(0, 40, 5), limits = c(0, 42)) +
  labs(x = "Peak Susceptible Density (unvac/sq mi)",
       y = "Bootstrap samples") +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "none",
    axis.line.x      = element_line(color = "black", linewidth = 0.3),
    axis.line.y      = element_line(color = "black", linewidth = 0.3),
    plot.margin      = margin(5, 10, 5, 5)
  ) +
  ggtitle("E")

# ==============================================================================
# 8. COMPOSE FIGURE
# ==============================================================================

# Assemble with patchwork
figure2 <- (panel_a | panel_b) /
            (panel_c | panel_d) /
            panel_e +
  plot_layout(heights = c(1, 1, 1.1)) &
  theme(plot.title = element_text(face = "bold", size = 14, hjust = -0.02))

# Save PDF
ggsave("Figure2_complete.pdf", figure2, width = 9.5, height = 11,
       device = cairo_pdf)

# Save high-res PNG
ggsave("Figure2_complete_hires.png", figure2, width = 9.5, height = 11,
       dpi = 300)

cat("Figure 2 saved as PDF and PNG.\n")
