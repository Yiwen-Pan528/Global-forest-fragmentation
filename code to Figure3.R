
###############################################################################
# Title:    code for Figure 3
# Author:   [Yiwen Pan]
#
# Description:
#   This script reproduces Figure 3, which shows the mean effect of
#   fragmentation (SFI coefficient) on two NPP responses
#   (NPP total and NPP stability) across forest change trajectories:
#     (a) simplified: stable vs. dynamic
#     (b) detailed:   comparison for four dynamics
#                     (amount up/down × FFI up/down)
#
# Input:
#   `data` is assumed to be loaded before running this script:
#     response       : dependent variable name ("NPP_total", "NPP_stability")
#     habitatstatus  : forest change trajectories
#                      ("stable", "dynamic", "amount_up_FFI_up",
#                       "amount_up_FFI_down", "amount_down_FFI_up",
#                       "amount_down_FFI_down")
#     SFI_coef       : coefficient of fragmentation effect
#
###############################################################################


# ---------------------------------------------------------------------------
# 0. Setup
# ---------------------------------------------------------------------------

# Install packages if missing (uncomment if needed)
# install.packages(c("dplyr", "ggplot2"))

library(dplyr)
library(ggplot2)

# Set a consistent theme for all figures
theme_set(theme_bw(base_family = "serif"))


# ===========================================================================
# Figure 3a: Simplified trajectories (stable vs. dynamic)
# ===========================================================================

# --- 3a.1 Summarise mean and SD of the SFI coefficient ---------------------
result_3a <- data %>%
  group_by(response, habitatstatus) %>%
  summarise(
    mean_value = mean(SFI_coef, na.rm = TRUE),
    sd_value   = sd(SFI_coef,   na.rm = TRUE),
    n          = sum(!is.na(SFI_coef)),
    .groups    = "drop"
  )

# --- 3a.2 Order factors for plotting ---------------------------------------
result_3a <- result_3a %>%
  mutate(
    habitatstatus = factor(habitatstatus,
                           levels = c("stable", "dynamic")),
    response      = factor(response,
                           levels = c("NPP_total", "NPP_stability"),
                           labels = c("NPP(Total)", "NPP(Stability)"))
  )

# --- 3a.3 Plot --------------------------------------------------------------
plot_3a <- ggplot(result_3a,
                  aes(x = habitatstatus, y = mean_value, fill = response)) +
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.8) +
  geom_errorbar(
    aes(ymin = mean_value - sd_value,
        ymax = mean_value + sd_value),
    position = position_dodge(0.8),
    width = 0.2, linewidth = 0.5, color = "black"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray40", linewidth = 0.6) +
  labs(
    y    = "Effect of fragmentation",
    fill = "Response"
  ) +
  scale_fill_manual(
    values = c("NPP(Stability)" = "#0066CC",
               "NPP(Total)"     = "#339900")
  ) +
  theme_bw() +
  theme(
    text             = element_text(family = "serif"),
    axis.text.x      = element_blank(),
    axis.text.y      = element_text(size = 12, color = "black"),
    axis.title.x     = element_blank(),
    axis.title.y     = element_text(size = 13, face = "bold"),
    legend.text      = element_text(size = 13, face = "bold"),
    legend.title     = element_blank(),
    legend.position  = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot_3a


# ===========================================================================
# Figure 3b: Detailed trajectories (four dynamics)
# ===========================================================================

# --- 3b.1 Summarise mean and SD of the SFI coefficient ---------------------
result_3b <- data %>%
  group_by(response, habitatstatus) %>%
  summarise(
    mean_value = mean(SFI_coef, na.rm = TRUE),
    sd_value   = sd(SFI_coef,   na.rm = TRUE),
    n          = sum(!is.na(SFI_coef)),
    .groups    = "drop"
  )

# --- 3b.2 Order factors for plotting ---------------------------------------
result_3b <- result_3b %>%
  mutate(
    habitatstatus = factor(
      habitatstatus,
      levels = c("stable",
                 "amount_up_FFI_up",
                 "amount_up_FFI_down",
                 "amount_down_FFI_up",
                 "amount_down_FFI_down")
    ),
    response = factor(
      response,
      levels = c("NPP_total", "NPP_stability"),
      labels = c("NPP(Total)", "NPP(Stability)")
    )
  )

# --- 3b.3 Plot --------------------------------------------------------------
plot_3b <- ggplot(result_3b,
                  aes(x = habitatstatus, y = mean_value, fill = response)) +
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.8) +
  geom_errorbar(
    aes(ymin = mean_value - sd_value,
        ymax = mean_value + sd_value),
    position = position_dodge(0.8),
    width = 0.2, linewidth = 0.5, color = "black"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray40", linewidth = 0.6) +
  labs(
    y    = "Effect of fragmentation",
    fill = "Response"
  ) +
  scale_fill_manual(
    values = c("NPP(Stability)" = "#0066CC",
               "NPP(Total)"     = "#339900")
  ) +
  theme_bw() +
  theme(
    text                = element_text(family = "serif"),
    axis.text.x         = element_blank(),
    axis.text.y         = element_text(size = 12, color = "black"),
    axis.title.x        = element_blank(),
    axis.title.y        = element_text(size = 13, face = "bold"),
    legend.text         = element_text(size = 13, face = "bold"),
    legend.title        = element_blank(),
    legend.position     = "top",
    panel.grid.major    = element_blank(),
    panel.grid.minor    = element_blank(),
    panel.background    = element_rect(fill = "transparent", color = NA),
    plot.background     = element_rect(fill = "transparent", color = NA),
    legend.background   = element_rect(fill = "transparent", color = NA),
    legend.key          = element_rect(fill = "transparent", color = NA)
  )

plot_3b


###############################################################################
# End of script
###############################################################################

