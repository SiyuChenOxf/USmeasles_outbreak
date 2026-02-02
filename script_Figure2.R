# ==============================================================================
# Figure 2: Kernel Density Estimation of Susceptible Population
# Gaines County, TX vs Spartanburg County, SC
# JAMA Research Letter
# ==============================================================================

# Load required packages
library(tidyverse)
library(readxl)
library(sf)
library(ggplot2)
library(viridis)
library(patchwork)
library(MASS)  # for kde2d

# ==============================================================================
# CONFIGURATION
# ==============================================================================

GAINES_AREA_SQMI <- 1502
SPARTANBURG_AREA_SQMI <- 819

# ==============================================================================
# LOAD SOUTH CAROLINA DATA
# ==============================================================================

cat("Loading South Carolina data...\n")

sc_raw <- read_excel("45_Day_Report_25-26_cleaned.xlsx")

spartanburg <- sc_raw %>%
  filter(str_detect(County, regex("Spart", ignore_case = TRUE))) %>%
  mutate(
    Immunizations = as.numeric(Immunizations),
    `Total Students` = as.numeric(`Total Students`)
  ) %>%
  filter(!is.na(Immunizations), !is.na(`Total Students`)) %>%
  mutate(Unvaccinated = round(`Total Students` * (1 - Immunizations)))

# City coordinates for geocoding
sc_cities <- tribble(
  ~City, ~lat, ~lon,
  "Spartanburg", 34.9496, -81.9320,
  "Duncan", 34.9382, -82.1437,

  "Boiling Springs", 35.0465, -81.9826,
  "Inman", 35.0468, -82.0901,
  "Chesnee", 35.1501, -81.8601,
  "Roebuck", 34.8768, -81.9665,
  "Woodruff", 34.7393, -82.0373,
  "Campobello", 35.1187, -82.1562,
  "Moore", 35.0165, -81.9915,
  "Lyman", 34.9482, -82.1273,
  "Cowpens", 35.0168, -81.8040,
  "Landrum", 35.1751, -82.1893,
  "Mayo", 34.87, -81.88,
  "Pacolet", 34.8982, -81.7601,
  "Pauline", 34.81, -81.85,
  "Wellford", 34.9501, -82.1001,
  "Reidville", 34.87, -82.12,
  "Greer", 34.9388, -82.2271
)

# Add coordinates with jitter
set.seed(42)
spartanburg <- spartanburg %>%
  left_join(sc_cities, by = "City") %>%
  mutate(
    # Default to Spartanburg city if city not found
    lat = ifelse(is.na(lat), 34.9496, lat),
    lon = ifelse(is.na(lon), -81.9320, lon),
    # Add jitter
    lat = lat + runif(n(), -0.035, 0.035),
    lon = lon + runif(n(), -0.035, 0.035)
  )

cat(sprintf("Spartanburg: %d schools, %d total unvaccinated\n", 
            nrow(spartanburg), sum(spartanburg$Unvaccinated)))

# ==============================================================================
# LOAD TEXAS DATA (Gaines County)
# ==============================================================================

cat("Loading Texas data...\n")

# Gaines County districts with cohort reconstruction values
gaines_districts <- tibble(
  District = c("Loop ISD", "Seminole ISD", "Seagraves ISD"),
  Students = c(145, 2366, 425),
  Coverage = c(36.5, 81.5, 94.4),  # K-8 weighted average
  lat = c(32.9282, 32.7193, 32.9443),
  lon = c(-102.4104, -102.6451, -102.5654)
) %>%
  mutate(Unvaccinated = round(Students * (1 - Coverage/100)))

cat(sprintf("Gaines: %d districts, %d total unvaccinated\n",
            nrow(gaines_districts), sum(gaines_districts$Unvaccinated)))

# ==============================================================================
# COUNTY BOUNDARIES
# ==============================================================================

# Spartanburg County boundary (approximate)
spartanburg_boundary <- matrix(c(
  -82.331, 34.784,
  -82.246, 34.737,
  -82.047, 34.698,
  -81.874, 34.713,
  -81.783, 34.837,
  -81.641, 34.895,
  -81.457, 34.839,
  -81.457, 35.044,
  -81.489, 35.117,
  -81.640, 35.164,
  -81.764, 35.180,
  -81.969, 35.187,
  -82.051, 35.189,
  -82.216, 35.196,
  -82.293, 35.195,
  -82.331, 35.060,
  -82.331, 34.784  # Close polygon
), ncol = 2, byrow = TRUE)

spartanburg_poly <- st_polygon(list(spartanburg_boundary)) %>%
  st_sfc(crs = 4326) %>%
  st_sf()

# Gaines County boundary (approximate)
gaines_boundary <- matrix(c(
  -103.065, 32.522,
  -102.211, 32.522,
  -102.211, 32.959,
  -102.525, 32.959,
  -102.525, 33.000,
  -103.065, 33.000,
  -103.065, 32.522  # Close polygon
), ncol = 2, byrow = TRUE)

gaines_poly <- st_polygon(list(gaines_boundary)) %>%
  st_sfc(crs = 4326) %>%
  st_sf()

# ==============================================================================
# WEIGHTED KDE FUNCTION
# ==============================================================================

#' Compute weighted kernel density estimation
#' @param x Vector of x coordinates (longitude)
#' @param y Vector of y coordinates (latitude)
#' @param weights Vector of weights (unvaccinated counts)
#' @param bandwidth Bandwidth for Gaussian kernel
#' @param grid_res Resolution of output grid
#' @param xlim X limits for grid
#' @param ylim Y limits for grid
#' @return List with x_grid, y_grid, and density matrix
compute_weighted_kde <- function(x, y, weights, bandwidth, grid_res = 200,
                                  xlim = NULL, ylim = NULL) {
  
  if (is.null(xlim)) xlim <- range(x) + c(-0.1, 0.1)
  if (is.null(ylim)) ylim <- range(y) + c(-0.1, 0.1)
  
  # Create grid
  x_grid <- seq(xlim[1], xlim[2], length.out = grid_res)
  y_grid <- seq(ylim[1], ylim[2], length.out = grid_res)
  
  # Initialize density matrix
  Z <- matrix(0, nrow = grid_res, ncol = grid_res)
  
  # Compute weighted KDE (Gaussian kernel)
  for (i in seq_along(x)) {
    # Distance from each grid point to this data point
    dx <- outer(x_grid, rep(1, grid_res)) - x[i]
    dy <- outer(rep(1, grid_res), y_grid) - y[i]
    dist_sq <- dx^2 + dy^2
    
    # Gaussian kernel
    kernel <- exp(-dist_sq / (2 * bandwidth^2))
    
    # Add weighted contribution
    Z <- Z + weights[i] * kernel
  }
  
  # Normalize
  Z <- Z / (2 * pi * bandwidth^2)
  
  list(x = x_grid, y = y_grid, z = Z)
}

#' Mask KDE to county boundary
#' @param kde KDE result from compute_weighted_kde
#' @param boundary_sf sf polygon object
#' @return KDE with values outside boundary set to NA
mask_kde_to_boundary <- function(kde, boundary_sf) {
  # Create grid of points
  grid_points <- expand.grid(x = kde$x, y = kde$y)
  grid_sf <- st_as_sf(grid_points, coords = c("x", "y"), crs = 4326)
  

  # Check which points are inside boundary
  inside <- st_intersects(grid_sf, boundary_sf, sparse = FALSE)[, 1]
  
  # Mask density
  Z_masked <- kde$z
  Z_masked[!matrix(inside, nrow = length(kde$x), ncol = length(kde$y))] <- NA
  
  kde$z <- Z_masked
  kde
}

# ==============================================================================
# COMPUTE KDE FOR BOTH COUNTIES
# ==============================================================================

cat("Computing KDE for Spartanburg County...\n")

# Spartanburg KDE
kde_sc <- compute_weighted_kde(
  x = spartanburg$lon,
  y = spartanburg$lat,
  weights = spartanburg$Unvaccinated,
  bandwidth = 0.04,
  grid_res = 200,
  xlim = c(-82.40, -81.40),
  ylim = c(34.65, 35.25)
)

# Mask to county boundary
kde_sc <- mask_kde_to_boundary(kde_sc, spartanburg_poly)

# Convert to unvac per sq mile (approx: 1 degree lat ≈ 69 mi, 1 degree lon ≈ 57 mi at this latitude)
deg_to_sqmi_sc <- 69 * 57
kde_sc$z_sqmi <- kde_sc$z / deg_to_sqmi_sc

cat("Computing KDE for Gaines County...\n")

# Gaines KDE
kde_g <- compute_weighted_kde(
  x = gaines_districts$lon,
  y = gaines_districts$lat,
  weights = gaines_districts$Unvaccinated,
  bandwidth = 0.08,
  grid_res = 200,
  xlim = c(-103.15, -102.10),
  ylim = c(32.45, 33.08)
)

# Mask to county boundary
kde_g <- mask_kde_to_boundary(kde_g, gaines_poly)

# Convert to unvac per sq mile
deg_to_sqmi_g <- 69 * 58
kde_g$z_sqmi <- kde_g$z / deg_to_sqmi_g

# ==============================================================================
# SUMMARY STATISTICS
# ==============================================================================

gaines_total_unvax <- sum(gaines_districts$Unvaccinated)
gaines_density_avg <- gaines_total_unvax / GAINES_AREA_SQMI
gaines_density_peak <- max(kde_g$z_sqmi, na.rm = TRUE)

sc_total_unvax <- sum(spartanburg$Unvaccinated)
sc_density_avg <- sc_total_unvax / SPARTANBURG_AREA_SQMI
sc_density_peak <- max(kde_sc$z_sqmi, na.rm = TRUE)

cat("\n========================================\n")
cat("SUMMARY STATISTICS\n")
cat("========================================\n")
cat(sprintf("Gaines County, TX:\n"))
cat(sprintf("  Total unvaccinated: %d\n", gaines_total_unvax))
cat(sprintf("  County avg density: %.2f unvac/sq mi\n", gaines_density_avg))
cat(sprintf("  Peak density: %.1f unvac/sq mi\n", gaines_density_peak))
cat(sprintf("\nSpartanburg County, SC:\n"))
cat(sprintf("  Total unvaccinated: %d\n", sc_total_unvax))
cat(sprintf("  County avg density: %.1f unvac/sq mi\n", sc_density_avg))
cat(sprintf("  Peak density: %.0f unvac/sq mi\n", sc_density_peak))
cat(sprintf("\nDensity ratio (SC/TX): %.0fx\n", sc_density_avg / gaines_density_avg))

# ==============================================================================
# CREATE FIGURE
# ==============================================================================

cat("\nCreating figure...\n")

# Convert KDE to data frames for ggplot
kde_g_df <- expand.grid(lon = kde_g$x, lat = kde_g$y) %>%
  mutate(density = as.vector(kde_g$z_sqmi))

kde_sc_df <- expand.grid(lon = kde_sc$x, lat = kde_sc$y) %>%
  mutate(density = as.vector(kde_sc$z_sqmi))

# Gaines boundary as data frame
gaines_boundary_df <- as.data.frame(gaines_boundary) %>%
  rename(lon = V1, lat = V2)

# Spartanburg boundary as data frame
spartanburg_boundary_df <- as.data.frame(spartanburg_boundary) %>%
  rename(lon = V1, lat = V2)

# Info box text
gaines_info <- sprintf(
  "Total Unvaccinated: %d\nArea: %s sq mi\n\nCounty avg: %.2f unvac/sq mi\nPeak: %.1f unvac/sq mi\n\nOutbreak: CONTAINED",
  gaines_total_unvax, format(GAINES_AREA_SQMI, big.mark = ","),
  gaines_density_avg, gaines_density_peak
)

sc_info <- sprintf(
  "Total Unvaccinated: %s\nArea: %d sq mi\n\nCounty avg: %.1f unvac/sq mi\nPeak: %.0f unvac/sq mi\n\nOutbreak: ONGOING",
  format(sc_total_unvax, big.mark = ","), SPARTANBURG_AREA_SQMI,
  sc_density_avg, sc_density_peak
)

# Panel C: Gaines County (Blue colormap)
p_gaines <- ggplot() +
  # KDE heatmap
  geom_tile(data = kde_g_df %>% filter(!is.na(density)),
            aes(x = lon, y = lat, fill = density)) +
  scale_fill_gradientn(
    colors = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", 
               "#6BAED6", "#4292C6", "#2171B5", "#084594"),
    name = "Susceptible Density\n(unvac/sq mi)",
    na.value = "white"
  ) +
  # County boundary
  geom_polygon(data = gaines_boundary_df,
               aes(x = lon, y = lat),
               fill = NA, color = "#2C3E50", linewidth = 1) +
  # District points
  geom_point(data = gaines_districts,
             aes(x = lon, y = lat),
             size = 3, color = "#2C3E50") +
  # District labels
  geom_label(data = gaines_districts,
             aes(x = lon, y = lat, 
                 label = paste0(District, "\n", Unvaccinated, " unvac")),
             size = 3, fontface = "bold",
             fill = "white", color = "#084594",
             label.padding = unit(0.2, "lines"),
             nudge_y = 0.06) +
  # Info box
  annotate("label", x = -103.0, y = 32.55, label = gaines_info,
           hjust = 0, vjust = 0, size = 3, fontface = "bold",
           fill = "#E6F3FF", color = "#084594",
           label.padding = unit(0.4, "lines")) +
  # Formatting
  coord_sf(xlim = c(-103.15, -102.10), ylim = c(32.45, 33.08)) +
  labs(title = "C. Gaines County, TX",
       subtitle = "3 Districts | 414 cases | Contained") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#084594", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", size = 11, color = "#084594", hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8)
  )

# Panel D: Spartanburg County (Red/Yellow colormap)
p_spartanburg <- ggplot() +
  # KDE heatmap
  geom_tile(data = kde_sc_df %>% filter(!is.na(density)),
            aes(x = lon, y = lat, fill = density)) +
  scale_fill_gradientn(
    colors = c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C",
               "#FD8D3C", "#FC4E2A", "#E31A1C", "#B10026"),
    name = "Susceptible Density\n(unvac/sq mi)",
    na.value = "white"
  ) +
  # County boundary
  geom_polygon(data = spartanburg_boundary_df,
               aes(x = lon, y = lat),
               fill = NA, color = "#2C3E50", linewidth = 1) +
  # School points
  geom_point(data = spartanburg,
             aes(x = lon, y = lat),
             size = 0.8, color = "#2C3E50", alpha = 0.5) +
  # Info box
  annotate("label", x = -82.33, y = 34.73, label = sc_info,
           hjust = 0, vjust = 1, size = 3, fontface = "bold",
           fill = "#FFE6E6", color = "#B10026",
           label.padding = unit(0.4, "lines")) +
  # Formatting
  coord_sf(xlim = c(-82.40, -81.40), ylim = c(34.65, 35.25)) +
  labs(title = "D. Spartanburg County, SC",
       subtitle = "93 Schools | 847+ cases | Ongoing") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#B10026", hjust = 0.5),
    plot.subtitle = element_text(face = "bold", size = 11, color = "#B10026", hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8)
  )

# Combine panels
p_combined <- p_gaines + p_spartanburg +
  plot_annotation(
    title = "Susceptible Population Density (unvac/sq mi)",
    subtitle = "Kernel Density Estimation | Blue = Contained | Red = Ongoing",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

# Save figure
ggsave("Figure2_KDE_panels_CD.png", p_combined, 
       width = 16, height = 7, dpi = 300, bg = "white")

cat("Figure saved: Figure2_KDE_panels_CD.png\n")

# ==============================================================================
# ALTERNATIVE: INDIVIDUAL PLOTS
# ==============================================================================

# Save individual panels
ggsave("Figure2_panel_C_Gaines.png", p_gaines, 
       width = 8, height = 7, dpi = 300, bg = "white")
ggsave("Figure2_panel_D_Spartanburg.png", p_spartanburg, 
       width = 8, height = 7, dpi = 300, bg = "white")

cat("Individual panels saved.\n")
cat("\nDone!\n")
