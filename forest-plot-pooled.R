# Forest Plot for Pooled Propensity Score Results
# 
# This script creates a publication-ready forest plot from pooled PS results
# Input: CSV file with pooled results (from reanalysis-12-export_results.qmd)
# Output: Forest plot PNG file
#
# Author: Analysis Team
# Date: 2025

# Load required libraries
library(ggplot2)

# ==============================================================================
# CONFIGURATION - Edit these paths as needed
# ==============================================================================

# Input CSV file path - change this to your data location
csv_file <- "exported_results/pooled_results_main.csv"

# Output plot file path - change this to where you want the plot saved
output_file <- "forest_plot_publication.png"

# Plot dimensions
plot_width <- 10
plot_height <- 8
plot_dpi <- 300

# ==============================================================================
# LOAD DATA
# ==============================================================================

# Read the CSV file
results_df <- read.csv(csv_file, stringsAsFactors = FALSE)

# Remove any failed methods (where OR is NA)
forest_data <- results_df[!is.na(results_df$or), ]

# ==============================================================================
# FORMAT METHOD LABELS
# ==============================================================================

# Clean up method names for display
forest_data$method_label <- forest_data$method

# Replace underscores with spaces
forest_data$method_label <- gsub("_", " ", forest_data$method_label)

# Format specific method names to match publication style
forest_data$method_label <- gsub("nearest ", "Nearest Neighbor ", forest_data$method_label)
forest_data$method_label <- gsub("subclass glm", "Subclassification GLM", forest_data$method_label)
forest_data$method_label <- gsub("cbps", "CBPS", forest_data$method_label)
forest_data$method_label <- gsub("entropy", "Entropy Balancing", forest_data$method_label)
forest_data$method_label <- gsub("bart", "BART", forest_data$method_label)
forest_data$method_label <- gsub("twang gbm", "IPTW Twang GBM", forest_data$method_label)

# Capitalize abbreviations
forest_data$method_label <- gsub("glm", "GLM", forest_data$method_label)
forest_data$method_label <- gsub("gam", "GAM", forest_data$method_label)
forest_data$method_label <- gsub("gbm", "GBM", forest_data$method_label)
forest_data$method_label <- gsub("lasso", "LASSO", forest_data$method_label)
forest_data$method_label <- gsub("rpart", "RPART", forest_data$method_label)
forest_data$method_label <- gsub("mahalanobis", "Mahalanobis", forest_data$method_label)

# ==============================================================================
# SET COLORS
# ==============================================================================

# Color scheme: blue for twang_gbm (primary method), black for others
# You can customize colors here
forest_data$color <- ifelse(forest_data$method == "twang_gbm", "blue", "black")

# Optional: Add red for a reference/unadjusted model if you have one
# forest_data$color <- ifelse(forest_data$method == "unadjusted", "red", forest_data$color)

# ==============================================================================
# CREATE FOREST PLOT
# ==============================================================================

forest_plot <- ggplot(forest_data, aes(x = reorder(method_label, or), y = or, color = color)) +
  
  # Point estimates with confidence intervals
  geom_pointrange(aes(ymin = or_ci_lower, ymax = or_ci_upper), 
                  size = 0.8) +
  
  # Larger points for the estimates
  geom_point(size = 3) +
  
  # Add OR values as text labels
  geom_text(aes(label = sprintf("%.2f", or), color = color), 
            vjust = -1.2,     # Vertical adjustment (above points)
            hjust = 0.5,      # Horizontal adjustment (centered)
            size = 4) +       # Text size
  
  # Flip coordinates for horizontal forest plot
  coord_flip() +
  
  # Use the actual colors (not a legend)
  scale_color_identity() +
  
  # Y-axis (OR scale) configuration
  scale_y_continuous(
    breaks = seq(1, max(forest_data$or_ci_upper), by = 0.1),  # Tick marks every 0.1
    limits = c(min(forest_data$or_ci_lower) * 0.95,           # Add some padding
               max(forest_data$or_ci_upper) * 1.05)
  ) +
  
  # X-axis (methods) configuration
  scale_x_discrete(expand = c(0.075, 0.075)) +  # Add space at top and bottom
  
  # Theme
  theme_minimal(base_size = 14) +
  
  # Labels
  labs(
    x = element_blank(),  # No x-axis label
    y = "Odds Ratio",
    title = "Doubly Robust Propensity Score Adjusted\n Model Estimates (ATT)",
    caption = "\n\nOdds Ratio of 1.0 = No Association.
               MatchIt, WeightIt, and twang R packages are used for estimation.
               IPTW = Inverse probability of treatment weighting.
               IPTW Twang GBM results (shown in blue) reported throughout paper."
  ) +
  
  # Theme customization
  theme(
    # Title formatting
    plot.title = element_text(hjust = 0.5,           # Center title
                             face = "bold",           # Bold text
                             margin = margin(b = 15)), # Bottom margin
    
    # Remove legend (we're using direct colors)
    legend.position = "none",
    
    # Axis formatting
    axis.line = element_line(color = "gray80"),
    axis.text = element_text(color = "gray30"),
    axis.ticks = element_line(color = "gray80"),
    axis.title.x = element_text(margin = margin(t = 10)),  # Top margin for x-axis title
    
    # Caption formatting
    plot.caption = element_text(margin = margin(t = 15)),  # Top margin for caption
    
    # Overall plot margins
    plot.margin = margin(20, 20, 30, 20)  # top, right, bottom, left
  ) +
  
  # Add reference line at OR = 1 (null effect)
  geom_hline(yintercept = 1, 
             linetype = "solid", 
             color = "gray60") +
  
  # Add "Null" label next to reference line
  annotate("text", 
           x = 0.5,           # Position on x-axis (adjust as needed)
           y = 1,             # At OR = 1
           label = "Null", 
           vjust = -1, 
           hjust = 1.2, 
           color = "gray50", 
           size = 3.5)

# ==============================================================================
# DISPLAY PLOT
# ==============================================================================

# Show the plot in RStudio/R console
print(forest_plot)

# ==============================================================================
# SAVE PLOT
# ==============================================================================

# Save as PNG file
ggsave(
  filename = output_file,
  plot = forest_plot,
  width = plot_width,
  height = plot_height,
  dpi = plot_dpi
)

cat("\nForest plot saved to:", output_file, "\n")
cat("Dimensions:", plot_width, "x", plot_height, "inches at", plot_dpi, "DPI\n")

# ==============================================================================
# OPTIONAL: CREATE ALTERNATIVE VERSIONS
# ==============================================================================

# Version 2: Sorted by effect size (largest to smallest)
forest_plot_sorted <- ggplot(forest_data, aes(x = reorder(method_label, -or), y = or, color = color)) +
  geom_pointrange(aes(ymin = or_ci_lower, ymax = or_ci_upper), size = 0.8) +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.2f", or), color = color), 
            vjust = -1.2, hjust = 0.5, size = 4) +
  coord_flip() +
  scale_color_identity() +
  scale_y_continuous(breaks = seq(1, max(forest_data$or_ci_upper), by = 0.1)) +
  scale_x_discrete(expand = c(0.075, 0.075)) +
  theme_minimal(base_size = 14) +
  labs(
    x = element_blank(),
    y = "Odds Ratio",
    title = "Propensity Score Methods - Sorted by Effect Size",
    subtitle = "Largest to smallest odds ratios"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 15)),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)),
    legend.position = "none",
    axis.line = element_line(color = "gray80"),
    axis.text = element_text(color = "gray30"),
    axis.ticks = element_line(color = "gray80"),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(20, 20, 30, 20)
  ) +
  geom_hline(yintercept = 1, linetype = "solid", color = "gray60")

# Uncomment to save sorted version
# ggsave("forest_plot_sorted.png", forest_plot_sorted, width = plot_width, height = plot_height, dpi = plot_dpi)

# ==============================================================================
# CUSTOMIZATION TIPS
# ==============================================================================

# 1. To change colors:
#    - Edit the color assignment section above
#    - Example: forest_data$color <- ifelse(forest_data$p_value < 0.05, "darkgreen", "gray50")

# 2. To change which methods are highlighted:
#    - Modify the condition in: forest_data$color <- ifelse(...)

# 3. To adjust text size:
#    - Change base_size in theme_minimal()
#    - Change size in geom_text()

# 4. To change the y-axis range:
#    - Modify the limits in scale_y_continuous()

# 5. To add grid lines:
#    - Add: panel.grid.major = element_line(color = "gray90") to theme()

# 6. To change the reference line:
#    - Modify geom_hline() parameters

# 7. To add p-value stars:
#    - Create a new column: forest_data$label <- paste0(sprintf("%.2f", forest_data$or),
#                                                        ifelse(forest_data$p_value < 0.001, "***",
#                                                        ifelse(forest_data$p_value < 0.01, "**",
#                                                        ifelse(forest_data$p_value < 0.05, "*", ""))))
#    - Use this in geom_text: aes(label = label)

# ==============================================================================
# END OF SCRIPT
# ==============================================================================