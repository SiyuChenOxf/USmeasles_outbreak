# ============================================================================
# PNAS Brief Report - Figure 1
# Single-box broken y-axis trajectory panel
# ============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)
library(grid)

# ============================================================================
# DATA
# ============================================================================

gaines_data <- data.frame(
  Level = c("Elementary\n(K-5/6)", "Middle\n(6-8)", "High\n(9-12)", "Total\nK-12"),
  Enrollment = c(1671, 831, 1058, 3560),
  Coverage = c(81.0, 86.8, 90.2, 85.1),
  Unvaccinated = c(317, 110, 104, 531),
  County = "Gaines Co., TX"
)

spartanburg_data <- data.frame(
  Level = c("Elementary\n(K-5/6)", "Middle\n(6-8)", "High\n(9-12)", "Total\nK-12"),
  Enrollment = c(27258, 13050, 16973, 57281),
  Coverage = c(86.8, 87.3, 93.1, 88.8),
  Unvaccinated = c(3606, 1656, 1164, 6426),
  County = "Spartanburg Co., SC"
)

combined_data <- rbind(gaines_data, spartanburg_data)
combined_data$Level <- factor(combined_data$Level, 
                               levels = c("Elementary\n(K-5/6)", "Middle\n(6-8)", 
                                         "High\n(9-12)", "Total\nK-12"))

gaines_trajectory <- data.frame(
  Date = as.Date(c("2025-02-05", "2025-02-11", "2025-02-14", "2025-02-18", 
                   "2025-02-21", "2025-02-25", "2025-02-28", "2025-03-04",
                   "2025-03-11", "2025-03-18", "2025-03-20", "2025-04-02",
                   "2025-04-04", "2025-04-08", "2025-04-11", "2025-04-15",
                   "2025-04-18", "2025-04-23", "2025-04-25", "2025-04-29",
                   "2025-05-06", "2025-05-13", "2025-05-16", "2025-05-23",
                   "2025-05-30", "2025-06-03", "2025-06-17", "2025-07-01")),
  New_Cases = c(6, 18, 18, 3, 12, 23, 18, 9, 49, 35, 79, 10, 35, 13, 27, 9, 
                7, 15, 7, 3, 7, 2, 1, 2, 1, 2, 2, 1),
  County = "Gaines Co., TX"
)
gaines_trajectory$Cumulative <- cumsum(gaines_trajectory$New_Cases)
gaines_trajectory$Day <- as.numeric(gaines_trajectory$Date - min(gaines_trajectory$Date))

spartanburg_trajectory <- data.frame(
  Date = as.Date(c("2025-10-02", "2025-10-15", "2025-11-01", "2025-11-15",
                   "2025-12-01", "2025-12-15", "2025-12-26", "2026-01-06",
                   "2026-01-13", "2026-01-21", "2026-01-30",
                   "2026-02-06", "2026-02-10", "2026-02-20")),
  Cumulative = c(1, 12, 45, 78, 112, 145, 156, 425, 565, 710, 847,
                 879, 890, 923),
  County = "Spartanburg Co., SC"
)
spartanburg_trajectory$Day <- as.numeric(spartanburg_trajectory$Date - min(spartanburg_trajectory$Date))

gaines_susceptibles <- 531
spartanburg_susceptibles <- 6426
sc_surpass_day <- spartanburg_trajectory$Day[which(spartanburg_trajectory$Cumulative >= 414)[1]]
unvac_ratio <- round(spartanburg_data$Unvaccinated[4] / gaines_data$Unvaccinated[4])

# ============================================================================
# COLORS
# ============================================================================

colors <- c("Gaines Co., TX" = "#2171B5",
            "Spartanburg Co., SC" = "#A0522D")

# ============================================================================
# CUSTOM Y-AXIS TRANSFORMATION (squish gap between 550 and 5800)
# ============================================================================

# Parameters for the break
break_lo <- 1000   # bottom of gap
break_hi <- 5800   # top of gap
gap_size <- 30     # visual space for the compressed gap (smaller = tighter gap)
top_scale <- 0.15  # scale factor for 5800-6800 zone (smaller = more compressed)

squish_forward <- function(y) {
  ifelse(y <= break_lo, y,
         ifelse(y <= break_hi, break_lo + (y - break_lo) / (break_hi - break_lo) * gap_size,
                break_lo + gap_size + (y - break_hi) * top_scale))
}

squish_inverse <- function(y) {
  ifelse(y <= break_lo, y,
         ifelse(y <= break_lo + gap_size, break_lo + (y - break_lo) / gap_size * (break_hi - break_lo),
                break_hi + (y - break_lo - gap_size) / top_scale))
}

squish_trans <- trans_new("squish", squish_forward, squish_inverse,
                          domain = c(0, 7000))

# Y-axis breaks and labels
y_breaks <- c(seq(0, 900, 100), 6000, 6400, 6800)
y_labels <- comma(y_breaks)

# ============================================================================
# PANEL A: Coverage by School Level
# ============================================================================

panel_a <- ggplot(combined_data, 
                  aes(x = Level, y = Coverage, fill = County)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
           width = 0.7, color = "black", linewidth = 0.3) +
  geom_hline(yintercept = 95, linetype = "dashed", color = "darkgreen", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "goldenrod3", linewidth = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Coverage)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3.8, fontface = "bold") +
  scale_fill_manual(values = colors) +
  scale_y_continuous(limits = c(75, 100), breaks = seq(75, 100, 5),
                     expand = c(0, 0), oob = rescale_none) +
  labs(title = NULL,
       x = NULL, y = "Weighted Coverage (%)", fill = NULL) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 13, color = "black"),
    axis.line = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    legend.position = "none",
  )

# ============================================================================
# PANEL B: Susceptible Population
# ============================================================================

# Calculate 75% CI on unvaccinated counts via binomial CI on coverage
z75 <- qnorm(0.875)  # 1.15 for 75% CI

combined_b <- combined_data
combined_b$Level_short <- factor(
  c("Elem", "Middle", "High", "TOTAL", "Elem", "Middle", "High", "TOTAL"),
  levels = c("Elem", "Middle", "High", "TOTAL")
)

combined_b$Coverage_prop <- combined_b$Coverage / 100
combined_b$SE_cov <- sqrt(combined_b$Coverage_prop * (1 - combined_b$Coverage_prop) / combined_b$Enrollment)
combined_b$Cov_lo <- pmax(0, combined_b$Coverage_prop - z75 * combined_b$SE_cov)
combined_b$Cov_hi <- pmin(1, combined_b$Coverage_prop + z75 * combined_b$SE_cov)
combined_b$Unvac_lo <- round(combined_b$Enrollment * (1 - combined_b$Cov_hi))
combined_b$Unvac_hi <- round(combined_b$Enrollment * (1 - combined_b$Cov_lo))

# Extract TOTAL CIs for Panel C annotations
gaines_unvac_lo <- combined_b$Unvac_lo[combined_b$County == "Gaines Co., TX" & combined_b$Level_short == "TOTAL"]
gaines_unvac_hi <- combined_b$Unvac_hi[combined_b$County == "Gaines Co., TX" & combined_b$Level_short == "TOTAL"]
sc_unvac_lo <- combined_b$Unvac_lo[combined_b$County == "Spartanburg Co., SC" & combined_b$Level_short == "TOTAL"]
sc_unvac_hi <- combined_b$Unvac_hi[combined_b$County == "Spartanburg Co., SC" & combined_b$Level_short == "TOTAL"]

cat(sprintf("\n75%% CI: Gaines TOTAL %s [%s, %s]\n", comma(531), comma(gaines_unvac_lo), comma(gaines_unvac_hi)))
cat(sprintf("75%% CI: Spartanburg TOTAL %s [%s, %s]\n", comma(6426), comma(sc_unvac_lo), comma(sc_unvac_hi)))

panel_b <- ggplot(combined_b, 
                  aes(x = Level_short, y = Unvaccinated, fill = County)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
           width = 0.7, color = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = Unvac_lo, ymax = Unvac_hi),
                position = position_dodge(width = 0.8),
                width = 0.2, linewidth = 0.6, color = "grey30") +
  geom_text(aes(label = comma(Unvaccinated), color = County, y = Unvac_hi), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3.8, fontface = "bold",
            show.legend = FALSE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_y_continuous(limits = c(0, 7500), labels = comma, expand = c(0, 0)) +
  labs(title = NULL,
       x = NULL, y = "Unvaccinated K-12 Students", fill = NULL) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 13, color = "black"),
    axis.line = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    legend.position = "none",
  )

# ============================================================================
# PANEL C: Outbreak Trajectory — SINGLE BOX, broken y-axis
# ============================================================================

# Compute transformed positions for break marks
break_y_mid <- squish_forward((break_lo + break_hi) / 2)  # mid of gap in transformed space
break_y_lo_t <- squish_forward(break_lo)
break_y_hi_t <- squish_forward(break_hi)

panel_c <- ggplot() +
  # Gaines trajectory
  geom_line(data = gaines_trajectory, 
            aes(x = Day, y = Cumulative, color = County), linewidth = 1.0) +
  geom_point(data = gaines_trajectory, 
             aes(x = Day, y = Cumulative, color = County), size = 1.8, shape = 16) +
  # Spartanburg trajectory
  geom_line(data = spartanburg_trajectory, 
            aes(x = Day, y = Cumulative, color = County), linewidth = 1.0) +
  geom_point(data = spartanburg_trajectory, 
             aes(x = Day, y = Cumulative, color = County), size = 1.8, shape = 15) +
  # Gaines susceptible line
  geom_hline(yintercept = gaines_susceptibles, 
             linetype = "dotted", color = "#2171B5", linewidth = 0.9) +
  # Spartanburg susceptible line
  geom_hline(yintercept = spartanburg_susceptibles, 
             linetype = "dotted", color = "#A0522D", linewidth = 0.9) +
  # Susceptible labels on right
  annotate("text", x = 215, y = gaines_susceptibles,
           label = sprintf("Gaines K-12 susceptibles: %s\n(75%% CI: %s\u2013%s)", 
                           comma(gaines_susceptibles), comma(gaines_unvac_lo), comma(gaines_unvac_hi)),
           color = "#2171B5", size = 3.8, hjust = 0, vjust = 0.5, fontface = "bold", lineheight = 0.9) +
  annotate("text", x = 215, y = spartanburg_susceptibles,
           label = sprintf("Spartanburg K-12 susceptibles: %s\n(75%% CI: %s\u2013%s)", 
                           comma(spartanburg_susceptibles), comma(sc_unvac_lo), comma(sc_unvac_hi)),
           color = "#A0522D", size = 3.8, hjust = 0, vjust = 0.5, fontface = "bold", lineheight = 0.9) +
  # Gaines final
  annotate("text", x = 138, y = 450,
           label = "Gaines final: 414",
           color = "#2171B5", size = 3.8, hjust = 0, fontface = "bold") +
  # Spartanburg latest
  annotate("text", x = 145, y = 923,
           label = "923+ cases (ongoing)",
           color = "#A0522D", size = 3.8, hjust = 0, vjust = -0.8, fontface = "bold") +
  # 1st death
  annotate("text", x = 22, y = 180,
           label = "1st death\n(Feb 21)",
           color = "#2171B5", size = 3.2, hjust = 0.5, fontface = "italic") +
  # 2nd death
  annotate("text", x = 48, y = 355,
           label = "2nd death\n(Apr 3)",
           color = "#2171B5", size = 3.2, hjust = 0.5, fontface = "italic") +
  # SC surpasses Gaines
  annotate("text", x = sc_surpass_day + 2, y = 310,
           label = sprintf("SC surpasses\nGaines (day %d)", sc_surpass_day),
           color = "#A0522D", size = 3.2, hjust = 0, fontface = "italic") +
  # Break marks (zigzag) — draw white rect to blank the gap, then zigzags
  # White rectangle to blank the compressed gap region
  annotate("rect", xmin = -10, xmax = 220, 
           ymin = break_lo + 5, ymax = break_hi - 5,
           fill = "white", color = NA) +
  # (Break marks drawn via grid after plot assembly)
  # Scale
  scale_color_manual(values = colors,
                     labels = c("Gaines County, TX (contained)",
                                "Spartanburg County, SC (ongoing)")) +
  scale_x_continuous(limits = c(-8, 215), breaks = seq(0, 200, 50),
                     expand = c(0, 0)) +
  scale_y_continuous(trans = squish_trans,
                     breaks = y_breaks,
                     labels = y_labels,
                     expand = c(0.01, 0)) +
  coord_cartesian(clip = "off", ylim = c(0, 6800)) +
  labs(title = NULL,
       x = "Days Since First Cases",
       y = "Cumulative Confirmed Cases",
       color = NULL) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 13, color = "black"),
    axis.line = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    legend.position = c(0.78, 0.12),
    legend.text = element_text(size = 11),
    legend.background = element_rect(fill = alpha("white", 0.9), color = NA),
    plot.margin = margin(5, 210, 5, 5)
  )

# ============================================================================
# COMBINE: A + B top, C full width below
# ============================================================================

figure1 <- (panel_a + panel_b) / panel_c +
  plot_layout(heights = c(1, 1.8)) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(face = "bold", size = 18),
      plot.margin = margin(10, 5, 5, 5)
    )
  )

# ============================================================================
# DRAW BREAK MARKS ON Y-AXIS USING GRID
# ============================================================================

# Custom function to add break marks to the saved plot
add_break_marks <- function(plot, filename, width, height, dpi = 300) {
  # Open device
  if (grepl("\\.png$", filename)) {
    png(filename, width = width, height = height, units = "in", res = dpi, bg = "white")
  } else if (grepl("\\.pdf$", filename)) {
    pdf(filename, width = width, height = height, bg = "white")
  } else if (grepl("\\.tiff$", filename)) {
    tiff(filename, width = width, height = height, units = "in", res = dpi, bg = "white")
  }
  
  print(plot)
  
  # Navigate to panel C (the third panel in the layout)
  # Add break marks at the y-axis break point
  # Position: on the left y-axis, at ~73% up from bottom of panel C
  # (500 out of 0-700 transformed range ≈ 71%)
  
  # Use viewport to draw on top of everything
  grid::pushViewport(grid::viewport(x = 0.085, y = 0.49, 
                                      width = 0.03, height = 0.025,
                                      just = c("center", "center")))
  # White background to cover axis line
  grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))
  # Two parallel diagonal lines
  grid::grid.lines(x = c(0.1, 0.9), y = c(0, 0.45), 
                    gp = grid::gpar(lwd = 3))
  grid::grid.lines(x = c(0.1, 0.9), y = c(0.55, 1), 
                    gp = grid::gpar(lwd = 3))
  grid::popViewport()
  
  dev.off()
}

# Save with break marks
add_break_marks(figure1, "/mnt/user-data/outputs/PNAS_Figure1.png", 12, 12)
add_break_marks(figure1, "/mnt/user-data/outputs/PNAS_Figure1.pdf", 12, 12)
add_break_marks(figure1, "/mnt/user-data/outputs/PNAS_Figure1.tiff", 12, 12)

print("Figure 1 saved!")

# Individual panels
ggsave("/mnt/user-data/outputs/Figure1_PanelA.png", panel_a,
       width = 6, height = 5, dpi = 300, bg = "white")
ggsave("/mnt/user-data/outputs/Figure1_PanelB.png", panel_b,
       width = 6, height = 5, dpi = 300, bg = "white")
ggsave("/mnt/user-data/outputs/Figure1_PanelC.png", panel_c,
       width = 12, height = 8, dpi = 300, bg = "white")

cat("\n=======================================================================\n")
cat("SUMMARY\n")
cat(sprintf("Gaines:      %s enroll, %s unvac, %.1f%% cov, %.1f%% attack\n",
            comma(3560), comma(531), 85.1, 414/531*100))
cat(sprintf("Spartanburg: %s enroll, %s unvac, %.1f%% cov, %.1f%% attack\n",
            comma(57281), comma(6426), 88.8, 923/6426*100))
cat(sprintf("SC has %dx more unvac; coverage +%.1f pp higher\n", unvac_ratio, 88.8-85.1))
