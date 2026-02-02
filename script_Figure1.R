# ============================================================================
# JAMA Research Letter - Figure 1
# Comparison of Measles Outbreaks: Gaines County, TX vs Spartanburg County, SC
# ============================================================================

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)

# ============================================================================
# DATA INPUT
# ============================================================================

# --- METHODOLOGY NOTES ---
# GAINES COUNTY, TX: Cohort reconstruction method
#   - Data: TX DSHS K coverage (2019-2025), 7th grade coverage (2019-2025)
#   - Enrollment: TX Education Agency (2024-2025)
#   - Method: Each grade's coverage estimated from when that cohort was in K
#     (e.g., current 5th graders → K coverage from 2019-20)
#   - For middle/high: 7th grade coverage used as proxy
#
# SPARTANBURG COUNTY, SC: Direct calculation
#   - Data: SC DHEC 45-Day Immunization Report (2025-2026)
#   - 93 schools with grade range, enrollment, and coverage
#   - Combined schools (K-8, K-12, Middle-High) allocated proportionally:
#     K-8: 67% Elementary, 33% Middle
#     K-12: 46% Elementary, 23% Middle, 31% High
#     Middle-High: 43% Middle, 57% High

# --- Gaines County, TX (from cohort reconstruction) ---
# Coverage by level derived from historical K and 7th grade data:
#   Elementary: K coverage from 2019-2025 weighted by current enrollment
#   Middle: 7th grade coverage 2024-25 as proxy
#   High: Historical 7th grade coverage when cohort was in 7th
gaines_data <- data.frame(
  Level = c("Elementary\n(K-5)", "Middle\n(6-8)", "High\n(9-12)", "Total\nK-12"),
  Enrollment = c(1668, 828, 440, 2936),
  Coverage = c(81.1, 86.7, 89.8, 84.0),
  Unvaccinated = c(315, 110, 45, 470),
  County = "Gaines County, TX"
)

# --- Spartanburg County, SC (from SC DHEC 45-Day Report) ---
# Direct calculation from school-level data (93 schools)
# Coverage: School-reported immunization rates
# Allocation: Combined schools split by grade span proportions
spartanburg_data <- data.frame(
  Level = c("Elementary\n(K-5)", "Middle\n(6-8)", "High\n(9-12)", "Total\nK-12"),
  Enrollment = c(24928, 13599, 16829, 55356),
  Coverage = c(86.3, 87.1, 93.2, 88.6),
  Unvaccinated = c(3403, 1748, 1143, 6294),
  County = "Spartanburg County, SC"
)

# NOTE: For Figure 2 coverage maps, Gaines County ISD-level coverage uses
# simple average of K and 7th grade (2024-25):
#   Loop ISD: (20.0% + 52.9%) / 2 = 36.5%
#   Seminole ISD: (77.1% + 86.0%) / 2 = 81.5%
#   Seagraves ISD: (96.8% + 92.1%) / 2 = 94.4%
# This differs slightly from cohort reconstruction totals (470 vs ~554 unvax)
# because it doesn't account for historical cohort-specific coverage.

# Combine data
combined_data <- rbind(gaines_data, spartanburg_data)
combined_data$Level <- factor(combined_data$Level, 
                               levels = c("Elementary\n(K-5)", "Middle\n(6-8)", 
                                         "High\n(9-12)", "Total\nK-12"))

# --- Outbreak trajectory data (from JHU Measles Tracking Team) ---
# Gaines County cumulative cases
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
  County = "Gaines County, TX"
)
gaines_trajectory$Cumulative <- cumsum(gaines_trajectory$New_Cases)
gaines_trajectory$Day <- as.numeric(gaines_trajectory$Date - min(gaines_trajectory$Date))

# Spartanburg County cumulative cases (from SC DHEC Dashboard)
spartanburg_trajectory <- data.frame(
  Date = as.Date(c("2025-10-02", "2025-10-15", "2025-11-01", "2025-11-15",
                   "2025-12-01", "2025-12-15", "2025-12-26", "2026-01-06",
                   "2026-01-13", "2026-01-21", "2026-01-30")),
  Cumulative = c(1, 12, 45, 78, 112, 145, 156, 425, 565, 710, 847),
  County = "Spartanburg County, SC"
)
spartanburg_trajectory$Day <- as.numeric(spartanburg_trajectory$Date - min(spartanburg_trajectory$Date))

# --- Attack rate data ---
attack_rate <- data.frame(
  County = c("Gaines County, TX", "Spartanburg County, SC"),
  Cases = c(414, 847),
  Susceptibles = c(470, 6294),
  Attack_Rate = c(414/470*100, 847/6294*100),
  Status = c("Contained", "Ongoing")
)

# ============================================================================
# COLOR SCHEME
# ============================================================================

colors <- c("Gaines County, TX" = "#E74C3C",      # Red for TX (contained)
            "Spartanburg County, SC" = "#3498DB")  # Blue for SC (ongoing)

# ============================================================================
# PANEL A: Coverage by School Level
# ============================================================================

panel_a <- ggplot(combined_data %>% filter(Level != "Total\nK-12"), 
                  aes(x = Level, y = Coverage, fill = County)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
           width = 0.7, color = "black", linewidth = 0.3) +
  geom_hline(yintercept = 95, linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
  geom_hline(yintercept = 92.9, linetype = "dotted", color = "darkred", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", Coverage)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3, fontface = "bold") +
  scale_fill_manual(values = colors) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20),
                     expand = c(0, 0)) +
  labs(title = "A. MMR Coverage by School Level",
       x = NULL, y = "Coverage (%)",
       fill = NULL) +
  annotate("text", x = 3.4, y = 96, label = "WHO target (95%)", 
           hjust = 1, size = 2.5, color = "darkgreen") +
  annotate("text", x = 3.4, y = 91, label = "Herd immunity (92.9%)", 
           hjust = 1, size = 2.5, color = "darkred") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# ============================================================================
# PANEL B: Susceptible Population by School Level
# ============================================================================

panel_b <- ggplot(combined_data %>% filter(Level != "Total\nK-12"), 
                  aes(x = Level, y = Unvaccinated, fill = County)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
           width = 0.7, color = "black", linewidth = 0.3) +
  geom_text(aes(label = comma(Unvaccinated)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3, fontface = "bold") +
  scale_fill_manual(values = colors) +
  scale_y_continuous(limits = c(0, 4000), 
                     labels = comma,
                     expand = c(0, 0)) +
  labs(title = "B. Susceptible Population by School Level",
       x = NULL, y = "Unvaccinated Students",
       fill = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# ============================================================================
# PANEL C: Attack Rate Comparison
# ============================================================================

panel_c <- ggplot(attack_rate, aes(x = County, y = Attack_Rate, fill = County)) +
  geom_bar(stat = "identity", width = 0.6, color = "black", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.1f%%", Attack_Rate)), 
            vjust = -0.5, size = 4, fontface = "bold") +
  geom_text(aes(label = sprintf("%s cases\n%s susceptibles", 
                                 comma(Cases), comma(Susceptibles)),
                y = Attack_Rate / 2), 
            size = 3, color = "white", fontface = "bold") +
  scale_fill_manual(values = colors) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                     expand = c(0, 0)) +
  labs(title = "C. Attack Rate Among K-12 Susceptibles",
       x = NULL, y = "Attack Rate (%)",
       fill = NULL) +
  annotate("text", x = 1, y = 95, label = "CONTAINED", 
           size = 3, fontface = "bold", color = "#E74C3C") +
  annotate("text", x = 2, y = 20, label = "ONGOING", 
           size = 3, fontface = "bold", color = "#3498DB") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# ============================================================================
# PANEL D: Outbreak Trajectory
# ============================================================================

# Susceptible pool lines
gaines_susceptibles <- 470
spartanburg_susceptibles <- 6294

panel_d <- ggplot() +
  # Gaines trajectory
  geom_line(data = gaines_trajectory, 
            aes(x = Day, y = Cumulative, color = County), 
            linewidth = 1.2) +
  geom_point(data = gaines_trajectory, 
             aes(x = Day, y = Cumulative, color = County), 
             size = 2) +
  # Spartanburg trajectory
  geom_line(data = spartanburg_trajectory, 
            aes(x = Day, y = Cumulative, color = County), 
            linewidth = 1.2) +
  geom_point(data = spartanburg_trajectory, 
             aes(x = Day, y = Cumulative, color = County), 
             size = 2) +
  # Susceptible pool reference lines
  geom_hline(yintercept = gaines_susceptibles, 
             linetype = "dashed", color = "#E74C3C", linewidth = 0.8, alpha = 0.7) +
  geom_hline(yintercept = spartanburg_susceptibles, 
             linetype = "dashed", color = "#3498DB", linewidth = 0.8, alpha = 0.7) +
  # Annotations
  annotate("text", x = 130, y = gaines_susceptibles + 150, 
           label = sprintf("Gaines susceptibles: %s", comma(gaines_susceptibles)),
           color = "#E74C3C", size = 3, hjust = 0) +
  annotate("text", x = 5, y = spartanburg_susceptibles - 300, 
           label = sprintf("Spartanburg susceptibles: %s", comma(spartanburg_susceptibles)),
           color = "#3498DB", size = 3, hjust = 0) +
  # Final case counts
  annotate("text", x = 150, y = 414, 
           label = "414 cases\n(final)", 
           color = "#E74C3C", size = 2.5, hjust = 0, vjust = 0.5) +
  annotate("text", x = 125, y = 847, 
           label = "847+ cases\n(ongoing)", 
           color = "#3498DB", size = 2.5, hjust = 0, vjust = 0.5) +
  scale_color_manual(values = colors) +
  scale_x_continuous(limits = c(0, 170), breaks = seq(0, 150, 30)) +
  scale_y_continuous(limits = c(0, 7000), 
                     breaks = c(0, 500, 1000, 2000, 4000, 6000),
                     labels = comma) +
  labs(title = "D. County-Level Outbreak Trajectory",
       x = "Days Since First Case",
       y = "Cumulative Cases",
       color = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

# ============================================================================
# COMBINE PANELS
# ============================================================================

# Option 1: 2x2 layout
figure1 <- (panel_a + panel_b) / (panel_c + panel_d) +
  plot_annotation(
    title = "Figure 1. Comparison of Measles Outbreaks: Gaines County, TX vs Spartanburg County, SC",
    caption = paste0(
      "Data sources: TX DSHS School Vaccination Coverage (2019-2025), TX Education Agency (2024-25), ",
      "SC DHEC 45-Day Immunization Report (2025-26),\n",
      "JHU Measles Tracking Team, SC DHEC Measles Dashboard. ",
      "Gaines County coverage estimated using cohort reconstruction method."
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.caption = element_text(size = 8, hjust = 0)
    )
  )

# Save figure
ggsave("/mnt/user-data/outputs/JAMA_Figure1.png", figure1, 
       width = 12, height = 10, dpi = 300, bg = "white")
ggsave("/mnt/user-data/outputs/JAMA_Figure1.pdf", figure1, 
       width = 12, height = 10, bg = "white")

print("Figure 1 saved!")

# ============================================================================
# ALTERNATIVE: Separate panels for flexibility
# ============================================================================

# Save individual panels
ggsave("/mnt/user-data/outputs/Figure1_PanelA.png", panel_a, 
       width = 6, height = 5, dpi = 300, bg = "white")
ggsave("/mnt/user-data/outputs/Figure1_PanelB.png", panel_b, 
       width = 6, height = 5, dpi = 300, bg = "white")
ggsave("/mnt/user-data/outputs/Figure1_PanelC.png", panel_c, 
       width = 5, height = 5, dpi = 300, bg = "white")
ggsave("/mnt/user-data/outputs/Figure1_PanelD.png", panel_d, 
       width = 7, height = 5, dpi = 300, bg = "white")

print("Individual panels saved!")

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("SUMMARY STATISTICS FOR JAMA LETTER\n")
cat("=" , rep("=", 70), "\n", sep = "")

cat("\nGAINES COUNTY, TX:\n")
cat(sprintf("  K-12 Enrollment: %s\n", comma(sum(gaines_data$Enrollment[1:3]))))
cat(sprintf("  K-12 Coverage: %.1f%%\n", gaines_data$Coverage[4]))
cat(sprintf("  K-12 Unvaccinated: %s\n", comma(gaines_data$Unvaccinated[4])))
cat(sprintf("  Outbreak Cases: 414 (contained)\n"))
cat(sprintf("  Attack Rate: %.1f%%\n", attack_rate$Attack_Rate[1]))

cat("\nSPARTANBURG COUNTY, SC:\n")
cat(sprintf("  K-12 Enrollment: %s\n", comma(sum(spartanburg_data$Enrollment[1:3]))))
cat(sprintf("  K-12 Coverage: %.1f%%\n", spartanburg_data$Coverage[4]))
cat(sprintf("  K-12 Unvaccinated: %s\n", comma(spartanburg_data$Unvaccinated[4])))
cat(sprintf("  Outbreak Cases: 847+ (ongoing)\n"))
cat(sprintf("  Attack Rate: %.1f%%\n", attack_rate$Attack_Rate[2]))

cat("\nKEY COMPARISONS:\n")
cat(sprintf("  SC has %.0fx more K-12 students\n", 
            sum(spartanburg_data$Enrollment[1:3]) / sum(gaines_data$Enrollment[1:3])))
cat(sprintf("  SC has %.0fx more unvaccinated students\n", 
            spartanburg_data$Unvaccinated[4] / gaines_data$Unvaccinated[4]))
cat(sprintf("  SC coverage is +%.1f pp higher than TX\n", 
            spartanburg_data$Coverage[4] - gaines_data$Coverage[4]))
cat(sprintf("  TX exhausted %.1f%% of susceptibles; SC has infected only %.1f%%\n",
            attack_rate$Attack_Rate[1], attack_rate$Attack_Rate[2]))
