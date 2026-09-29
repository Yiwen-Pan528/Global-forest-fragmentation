###############################################################################
# Title:    code for Figure 1
# Author:   [Yiwen Pan]
#
# Description:
#   This script reproduces Figure 1 (panels a–d), which visualises:
#     (a) Bivariate map of habitat amount vs. fragmentation in 2020
#     (b) Bivariate map of change trajectories (2000–2020)
#     (c) Composition of amount × fragmentation classes by ecosystem
#     (d) Global composition of change trajectories and mean FFI change
#
# Data availability:
#   The input object `data` is assumed to be loaded before running
#   this script.
#
###############################################################################


# ---------------------------------------------------------------------------
# 0. Setup
# ---------------------------------------------------------------------------

# Install packages if missing (uncomment if needed)
# install.packages(c("biscale", "sf", "ggplot2", "dplyr", "cowplot",
#                    "rnaturalearth", "rnaturalearthdata", "scales",
#                    "tidyr"))

library(biscale)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

# Set a consistent theme for all figures
theme_set(theme_minimal(base_family = "serif"))


# ===========================================================================
# Figure 1a: Bivariate map of habitat amount vs. fragmentation (2020)
# ===========================================================================

# --- 1a.1 Load global basemap ----------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")

# --- 1a.2 Classify cells into 5 × 5 bivariate classes ----------------------
fishnet_joined <- data %>%
  mutate(
    amount_group = cut(
      amount2020,
      breaks = c(0, 10, 30, 60, 90, 100),
      labels = c("0-10", "10-30", "30-60", "60-90", "90-100"),
      include.lowest = TRUE
    ),
    frag_group = cut(
      FFI2020,
      breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      labels = c("0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8", "0.8-1"),
      include.lowest = TRUE
    )
  ) %>%
  mutate(
    amount_num = as.numeric(amount_group),
    frag_num   = 6 - as.numeric(frag_group),   # reverse y-axis
    bi_class   = paste(amount_num, frag_num, sep = "-")
  )

# --- 1a.3 Build bivariate colour palette (5 × 5) ---------------------------
test_data <- expand.grid(x = 1:5, y = 1:5)

# Corner colours, clockwise from top-right:
#   top-right (high amount, high fragmentation)
#   bottom-right (low amount, high fragmentation)
#   bottom-left (low amount, low fragmentation)
#   top-left (high amount, low fragmentation)
corner_colors <- c(
  "#007b00",   # top-right
  "#d9be00",   # bottom-right
  "#EDF5ED",   # bottom-left
  "#0088d9"    # top-left
)

test_data$color <- colors2d(
  data      = test_data[, c("x", "y")],
  colors    = corner_colors,
  xtrans    = "none",
  ytrans    = "none"
)

bivariate_colors <- matrix(test_data$color, nrow = 5, byrow = TRUE)

# --- 1a.4 Plot map ----------------------------------------------------------
map_1a <- ggplot() +
  geom_sf(data = world, fill = "#A9A9A9", color = NA,
          size = 0, alpha = 0.5) +
  geom_sf(data = fishnet_joined,
          aes(fill = bi_class),
          color = NA, show.legend = FALSE) +
  scale_fill_manual(values = bivariate_colors) +
  theme_void()

# --- 1a.5 Build legend ------------------------------------------------------
legend_data_1a <- expand.grid(amount_num = 1:5, frag_num = 1:5) %>%
  mutate(bi_class = paste(amount_num, frag_num, sep = "-"))

legend_1a <- ggplot(legend_data_1a) +
  geom_tile(aes(x = amount_num, y = frag_num, fill = bi_class)) +
  scale_fill_manual(values = bivariate_colors) +
  scale_x_continuous(breaks = 1:5, labels = rep("", 5)) +
  scale_y_continuous(breaks = 1:5, labels = rep("", 5)) +
  labs(x = "Habitat amount", y = "Fragmentation") +
  coord_fixed(ratio = 1) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid      = element_blank(),
    axis.title      = element_text(size = 20, family = "Times New Roman")
  )


# ===========================================================================
# Figure 1b: Bivariate map of change trajectories (2000–2020)
# ===========================================================================

# --- 1b.1 Compute relative changes -----------------------------------------
fishnet_joined <- fishnet_joined %>%
  mutate(
    area_relative_change = (area2020 - area2000) / area2000 * 100,
    FFI_change           = FFI2020 - FFI2000
  )

# --- 1b.2 Classify change trajectories -------------------------------------
# A ±1% threshold defines "stable" habitat amount.
fishnet_joined <- fishnet_joined %>%
  mutate(
    habitatstatus = case_when(
      area_relative_change >= -1 & area_relative_change <= 1 ~ "stable",
      area_relative_change >   1 & FFI_change > 0 ~ "amount_up_FFI_up",
      area_relative_change >   1 & FFI_change < 0 ~ "amount_up_FFI_down",
      area_relative_change <  -1 & FFI_change > 0 ~ "amount_down_FFI_up",
      area_relative_change <  -1 & FFI_change < 0 ~ "amount_down_FFI_down",
      TRUE ~ NA_character_
    )
  )

# --- 1b.3 Split into trajectory groups -------------------------------------
fishnet_stable <- fishnet_joined %>%
  filter(habitatstatus == "stable") %>%
  mutate(bi_class = "stable")

fishnet_amount_up_FFI_up <- fishnet_joined %>%
  filter(habitatstatus == "amount_up_FFI_up")

fishnet_amount_up_FFI_down <- fishnet_joined %>%
  filter(habitatstatus == "amount_up_FFI_down")

fishnet_amount_down_FFI_up <- fishnet_joined %>%
  filter(habitatstatus == "amount_down_FFI_up")

fishnet_amount_down_FFI_down <- fishnet_joined %>%
  filter(habitatstatus == "amount_down_FFI_down")

# --- 1b.4 Define an 8 × 8 bivariate palette --------------------------------
test_data_8 <- expand.grid(x = 1:8, y = 1:8)

# Corner colours, clockwise from top-right
corner_colors_8 <- c(
  "#addd8e",   # top-right
  "#006600",   # bottom-right
  "#FFCC00",   # bottom-left
  "#CC0000"    # top-left
)

# Bilinear interpolation in RGB space
colors2d <- function(data, colors, xtrans = "none", ytrans = "none") {
  x_vals <- data[, 1]
  y_vals <- data[, 2]
  x_norm <- (x_vals - min(x_vals)) / (max(x_vals) - min(x_vals))
  y_norm <- (y_vals - min(y_vals)) / (max(y_vals) - min(y_vals))
  
  col_rgb <- lapply(colors, function(c) col2rgb(c) / 255)
  
  result <- vector("character", length(x_norm))
  for (i in seq_along(x_norm)) {
    w_top    <- y_norm[i]
    w_bottom <- 1 - y_norm[i]
    w_right  <- x_norm[i]
    w_left   <- 1 - x_norm[i]
    
    r <- col_rgb[[1]][1] * w_right * w_top +
      col_rgb[[2]][1] * w_right * w_bottom +
      col_rgb[[3]][1] * w_left  * w_bottom +
      col_rgb[[4]][1] * w_left  * w_top
    g <- col_rgb[[1]][2] * w_right * w_top +
      col_rgb[[2]][2] * w_right * w_bottom +
      col_rgb[[3]][2] * w_left  * w_bottom +
      col_rgb[[4]][2] * w_left  * w_top
    b <- col_rgb[[1]][3] * w_right * w_top +
      col_rgb[[2]][3] * w_right * w_bottom +
      col_rgb[[3]][3] * w_left  * w_bottom +
      col_rgb[[4]][3] * w_left  * w_top
    
    result[i] <- rgb(r, g, b)
  }
  result
}

test_data_8$color <- colors2d(
  data   = test_data_8[, c("x", "y")],
  colors = corner_colors_8
)

bivariate_colors_8x8 <- matrix(test_data_8$color, nrow = 8, byrow = TRUE)

# --- 1b.5 Assign 8 × 8 bi_class to each trajectory group -------------------
# Note: quantile breaks are computed within each trajectory group, so the
# 8 × 8 grid represents relative position within a trajectory, not absolute
# change magnitude. This is intentional: it emphasises within-trajectory
# variation.

assign_bi_class <- function(df, amount_labels, frag_labels) {
  df %>%
    mutate(
      amount_group = cut(
        area_relative_change,
        breaks = quantile(area_relative_change,
                          probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
        labels = amount_labels, include.lowest = TRUE
      ),
      frag_group = cut(
        FFI_change,
        breaks = quantile(FFI_change,
                          probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
        labels = frag_labels, include.lowest = TRUE
      )
    ) %>%
    mutate(
      amount_num = as.character(amount_group),
      frag_num   = as.character(frag_group),
      bi_class   = paste(amount_num, frag_num, sep = "-")
    )
}

fishnet_amount_up_FFI_up     <- assign_bi_class(fishnet_amount_up_FFI_up,
                                                c("5", "6", "7", "8"),
                                                c("5", "6", "7", "8"))
fishnet_amount_up_FFI_down   <- assign_bi_class(fishnet_amount_up_FFI_down,
                                                c("5", "6", "7", "8"),
                                                c("1", "2", "3", "4"))
fishnet_amount_down_FFI_up   <- assign_bi_class(fishnet_amount_down_FFI_up,
                                                c("1", "2", "3", "4"),
                                                c("5", "6", "7", "8"))
fishnet_amount_down_FFI_down <- assign_bi_class(fishnet_amount_down_FFI_down,
                                                c("1", "2", "3", "4"),
                                                c("1", "2", "3", "4"))

# --- 1b.6 Combine all trajectory groups ------------------------------------
fishnet_all <- bind_rows(
  fishnet_stable,
  fishnet_amount_up_FFI_up,
  fishnet_amount_up_FFI_down,
  fishnet_amount_down_FFI_up,
  fishnet_amount_down_FFI_down
)

# --- 1b.7 Build colour lookup table ----------------------------------------
color_lookup <- c()
for (i in 1:8) {
  for (j in 1:8) {
    color_lookup[paste(i, j, sep = "-")] <- bivariate_colors_8x8[j, i]
  }
}
color_lookup["stable"] <- "#0066CC"

# Check for missing colours and assign grey as fallback
unique_classes  <- unique(fishnet_all$bi_class)
missing_colors  <- unique_classes[!unique_classes %in% names(color_lookup)]
if (length(missing_colors) > 0) {
  warning("Missing bi_class values in colour lookup: ",
          paste(missing_colors, collapse = ", "))
  for (cls in missing_colors) color_lookup[cls] <- "#CCCCCC"
}

# --- 1b.8 Plot map ----------------------------------------------------------
map_1b <- ggplot() +
  geom_sf(data = world, fill = "#A9A9A9", color = NA, alpha = 0.5) +
  geom_sf(data = fishnet_all, aes(fill = bi_class),
          color = NA, size = 0) +
  scale_fill_manual(values = color_lookup, na.value = "transparent") +
  coord_sf() +
  theme_void() +
  theme(
    plot.title     = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

# --- 1b.9 Legend ------------------------------------------------------------
legend_data_1b <- expand.grid(amount_num = 1:8, frag_num = 1:8) %>%
  mutate(bi_class = paste(amount_num, frag_num, sep = "-"))

legend_1b <- ggplot(legend_data_1b) +
  geom_tile(aes(x = amount_num, y = frag_num, fill = bi_class)) +
  scale_fill_manual(values = color_lookup[1:64]) +
  scale_x_continuous(breaks = 1:8, labels = rep("", 8)) +
  scale_y_continuous(breaks = 1:8, labels = rep("", 8)) +
  labs(x = "Habitat amount", y = "Fragmentation") +
  coord_fixed(ratio = 1) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid      = element_blank(),
    axis.title      = element_text(size = 20, family = "serif")
  )


# ===========================================================================
# Figure 1c: Composition of amount × fragmentation classes by ecosystem
# ===========================================================================

# --- 1c.1 Define habitat-amount classes ------------------------------------
plotdata <- data %>%
  mutate(
    amountlevel = case_when(
      amount2020 <  10                      ~ "<10%",
      amount2020 >= 10 & amount2020 < 30    ~ "10-30%",
      amount2020 >= 30 & amount2020 < 60    ~ "30-60%",
      amount2020 >= 60 & amount2020 < 90    ~ "60-90%",
      amount2020 >= 90                      ~ ">90%",
      TRUE ~ NA_character_
    ),
    amountlevel = factor(amountlevel,
                         levels = c("<10%", "10-30%", "30-60%",
                                    "60-90%", ">90%"))
  )

# --- 1c.2 Compute relative changes -----------------------------------------
plotdata <- plotdata %>%
  mutate(
    area_relative_change = (area2020 - area2000) / area2000 * 100,
    FFI_change           = FFI2020 - FFI2000
  )

# --- 1c.3 Classify change trajectories -------------------------------------
plotdata <- plotdata %>%
  mutate(
    habitatstatus = case_when(
      area_relative_change >= -1 & area_relative_change <= 1 ~ "stable",
      area_relative_change >   1 & FFI_change > 0 ~ "amount_up_FFI_up",
      area_relative_change >   1 & FFI_change < 0 ~ "amount_up_FFI_down",
      area_relative_change <  -1 & FFI_change > 0 ~ "amount_down_FFI_up",
      area_relative_change <  -1 & FFI_change < 0 ~ "amount_down_FFI_down",
      TRUE ~ NA_character_
    )
  )

# --- 1c.4 Classify fragmentation (FFI) ----------------------
plotdata <- plotdata %>%
  mutate(
    FFI_quantile20 = case_when(
      FFI2020 <  0.2                    ~ "Q1",
      FFI2020 >= 0.2 & FFI2020 < 0.4    ~ "Q2",
      FFI2020 >= 0.4 & FFI2020 < 0.6    ~ "Q3",
      FFI2020 >= 0.6 & FFI2020 < 0.8    ~ "Q4",
      FFI2020 >= 0.8                    ~ "Q5",
      TRUE ~ NA_character_
    ),
    FFI_quantile20 = factor(FFI_quantile20,
                            levels = c("Q5", "Q4", "Q3", "Q2", "Q1"))
  )

# --- 1c.5 Cross-classify amount × FFI --------------------------------------
plotdata <- plotdata %>%
  mutate(combined_class = paste(amountlevel, FFI_quantile20, sep = "_"))

# --- 1c.6 Global proportions ------------------------------------------------
overall_prop <- plotdata %>%
  count(combined_class) %>%
  mutate(
    proportion = n / sum(n) * 100,
    percentage = round(proportion, 2)
  ) %>%
  arrange(desc(proportion)) %>%
  rename(count = n) %>%
  mutate(ecosystems = "Global")

# --- 1c.7 Per-ecosystem proportions ----------------------------------------
ecosystem_prop <- plotdata %>%
  group_by(ecosystems, combined_class) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(ecosystems) %>%
  mutate(
    proportion = count / sum(count) * 100,
    percentage = round(proportion, 2)
  ) %>%
  ungroup() %>%
  arrange(ecosystems, desc(proportion))

combined_prop <- bind_rows(overall_prop, ecosystem_prop) %>%
  mutate(
    ecosystems = case_when(
      ecosystems == "boreal"         ~ "Boreal",
      ecosystems == "cool temperate" ~ "Cool temperate",
      ecosystems == "polar"          ~ "Polar",
      ecosystems == "sub tropical"   ~ "Subtropical",
      ecosystems == "tropical"       ~ "Tropical",
      ecosystems == "warm temperate" ~ "Warm temperate",
      TRUE ~ ecosystems
    )
  ) %>%
  filter(ecosystems != "Polar")

# --- 1c.8 Order factors and insert a gap after "Global" --------------------
desired_order <- c("Global", "Tropical", "Subtropical",
                   "Warm temperate", "Cool temperate", "Boreal")

all_classes <- c(
  "<10%_Q1", "<10%_Q2", "<10%_Q3", "<10%_Q4", "<10%_Q5",
  "10-30%_Q1", "10-30%_Q2", "10-30%_Q3", "10-30%_Q4", "10-30%_Q5",
  "30-60%_Q1", "30-60%_Q2", "30-60%_Q3", "30-60%_Q4", "30-60%_Q5",
  "60-90%_Q1", "60-90%_Q2", "60-90%_Q3", "60-90%_Q4", "60-90%_Q5",
  ">90%_Q1",   ">90%_Q2",   ">90%_Q3",   ">90%_Q4",   ">90%_Q5"
)

combined_prop <- combined_prop %>%
  mutate(
    ecosystems     = factor(ecosystems, levels = desired_order),
    combined_class = factor(combined_class, levels = all_classes)
  )

# Position mapping: Global at 1, gap at 2, ecosystems from 3 onward
position_mapping <- data.frame(
  ecosystems = desired_order,
  position   = c(1, 3, 4, 5, 6, 7)
)

combined_prop <- combined_prop %>%
  left_join(position_mapping, by = "ecosystems")

# --- 1c.9 Plot --------------------------------------------------------------
p1c <- ggplot(combined_prop,
              aes(x = position, y = percentage, fill = combined_class)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(
    breaks = position_mapping$position,
    labels = position_mapping$ecosystems,
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_fill_manual(
    name   = "Amount_FFI",
    values = c(
      "<10%_Q1"   = "#0088D9", "<10%_Q2"   = "#3BA3DE",
      "<10%_Q3"   = "#77BFE3", "<10%_Q4"   = "#B2DAE8",
      "<10%_Q5"   = "#EDF5ED",
      "10-30%_Q1" = "#0085A3", "10-30%_Q2" = "#3A9DA6",
      "10-30%_Q3" = "#74B6AA", "10-30%_Q4" = "#AECFAE",
      "10-30%_Q5" = "#E8E7B2",
      "30-60%_Q1" = "#00826D", "30-60%_Q2" = "#39986F",
      "30-60%_Q3" = "#72AE72", "30-60%_Q4" = "#AAC474",
      "30-60%_Q5" = "#E3DA77",
      "60-90%_Q1" = "#007E36", "60-90%_Q2" = "#389238",
      "60-90%_Q3" = "#6FA539", "60-90%_Q4" = "#A7B83A",
      "60-90%_Q5" = "#DECC3B",
      ">90%_Q1"   = "#007B00", ">90%_Q2"   = "#368C00",
      ">90%_Q3"   = "#6D9D00", ">90%_Q4"   = "#A3AD00",
      ">90%_Q5"   = "#D9BE00"
    )
  ) +
  labs(y = "Cumulative percentage", x = NULL) +
  theme_minimal() +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1,
                                      family = "serif", color = "black",
                                      size = 19),
    axis.text.y        = element_text(family = "serif", color = "black",
                                      size = 19),
    axis.title.y       = element_text(family = "serif", color = "black",
                                      size = 19, margin = margin(r = 10)),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.background   = element_blank(),
    axis.line          = element_line(color = "black"),
    axis.ticks         = element_line(color = "black"),
    axis.ticks.length  = unit(-0.15, "cm"),
    strip.background   = element_blank(),
    strip.text         = element_blank(),
    legend.position    = "right",
    legend.title       = element_text(family = "serif", color = "black",
                                      size = 16, face = "bold"),
    legend.text        = element_text(family = "serif", color = "black",
                                      size = 12),
    text               = element_text(family = "serif", color = "black")
  )


# ===========================================================================
# Figure 1d: Global composition of change trajectories and mean FFI change
# ===========================================================================

# --- 1d.1 Global composition (stacked bar) ---------------------------------
plotdata <- plotdata %>%
  mutate(habitatstatus = case_when(
    area_relative_change >= -1 & area_relative_change <= 1 ~ "stable",
    area_relative_change >   1 & FFI_change > 0 ~ "amount_up_FFI_up",
    area_relative_change >   1 & FFI_change < 0 ~ "amount_up_FFI_down",
    area_relative_change <  -1 & FFI_change > 0 ~ "amount_down_FFI_up",
    area_relative_change <  -1 & FFI_change < 0 ~ "amount_down_FFI_down",
    TRUE ~ NA_character_
  )) %>%
  mutate(group = "Global")

p1d_stack <- ggplot(plotdata, aes(x = group, fill = habitatstatus)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c(
    "stable"               = "#0066CC",
    "amount_down_FFI_up"   = "#CC0000",
    "amount_down_FFI_down" = "#FFCC00",
    "amount_up_FFI_up"     = "#addd8e",
    "amount_up_FFI_down"   = "#006600"
  )) +
  labs(y = "Cumulative percentage", x = NULL) +
  theme_minimal() +
  theme(
    axis.text.x       = element_blank(),
    axis.text.y       = element_text(family = "serif", color = "black",
                                     size = 15),
    axis.title.y      = element_text(family = "serif", color = "black",
                                     size = 15, margin = margin(l = 10)),
    axis.title.x      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    panel.background  = element_blank(),
    axis.line         = element_blank(),
    axis.line.y       = element_line(color = "black"),
    axis.ticks.x      = element_blank(),
    axis.ticks.y      = element_line(color = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    legend.position   = "none",
    text              = element_text(family = "Times New Roman",
                                     color = "black")
  )

# --- 1d.2 Summary statistics for FFI change --------------------------------
summary_stats_original <- plotdata %>%
  filter(!is.na(habitatstatus)) %>%
  filter(is.finite(area_relative_change)) %>%
  filter(is.finite(FFI_change)) %>%
  group_by(habitatstatus) %>%
  summarise(
    area_mean = mean(abs(area_relative_change), na.rm = TRUE),
    area_sd   = sd(abs(area_relative_change),   na.rm = TRUE),
    area_se   = area_sd / sqrt(n()),
    FFI_mean  = mean(abs(FFI_change),           na.rm = TRUE),
    FFI_sd    = sd(abs(FFI_change),             na.rm = TRUE),
    FFI_se    = FFI_sd / sqrt(n()),
    n         = n(),
    .groups   = "drop"
  ) %>%
  mutate(
    habitatstatus = factor(habitatstatus,
                           levels = c("stable",
                                      "amount_up_FFI_up",
                                      "amount_up_FFI_down",
                                      "amount_down_FFI_up",
                                      "amount_down_FFI_down"))
  )

plot_long_filtered <- summary_stats_original %>%
  pivot_longer(cols = c(area_mean, FFI_mean),
               names_to = "variable", values_to = "mean") %>%
  mutate(
    sd = ifelse(variable == "area_mean", area_se, FFI_se),
    variable_label = ifelse(variable == "area_mean",
                            "Relative area change (%)", "FFI change")
  ) %>%
  filter(variable_label == "FFI change")

# --- 1d.3 Bar plot with error bars -----------------------------------------
habitat_colors <- c(
  "stable"               = "#0066CC",
  "amount_down_FFI_up"   = "#CC0000",
  "amount_down_FFI_down" = "#FFCC00",
  "amount_up_FFI_up"     = "#addd8e",
  "amount_up_FFI_down"   = "#006600"
)

p1d_bar <- ggplot(plot_long_filtered,
                  aes(x = habitatstatus, y = mean, fill = habitatstatus)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.1, size = 0.4) +
  scale_fill_manual(values = habitat_colors) +
  labs(y = "FFI change") +
  theme_minimal() +
  theme(
    axis.text.x       = element_text(angle = 45, hjust = 1,
                                     family = "serif", color = "black",
                                     size = 15),
    axis.text.y       = element_text(family = "serif", color = "black",
                                     size = 15),
    axis.title.y      = element_text(family = "serif", color = "black",
                                     size = 15),
    axis.title.x      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    panel.background  = element_rect(fill = "white"),
    axis.line         = element_line(color = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    axis.ticks        = element_line(color = "black", size = 0.5),
    legend.position   = "none"
  )


###############################################################################
# End of script
###############################################################################