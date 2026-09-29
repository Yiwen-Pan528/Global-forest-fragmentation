
###############################################################################
# Title:    code for Figure 2
# Author:   [Yiwen Pan]
#
# Description:
#   This script reproduces Figure 2, which shows the mean effect of
#   fragmentation (FFI coefficient) on two NPP responses
#   (NPP total and NPP stability) across six forest amount levels
#   (Global, <10%, 10-30%, 30-60%, 60-90%, >90%).
#
# Input:
#   The input object `amountlevel` is assumed to be loaded before running this script:
#     depend_var   : dependent variable name
#     FFI_coef     : coefficient of fragmentation effect
#     FFI_p        : p-value of FFI coefficient
#     max_distance : lag distance of spatial error models
#     amountlevel  : forest amount level
#
###############################################################################


# ---------------------------------------------------------------------------
# 0. Setup
# ---------------------------------------------------------------------------

# Install packages if missing (uncomment if needed)
# install.packages(c("dplyr", "ggplot2", "tidyr"))

library(dplyr)
library(ggplot2)

# Set a consistent theme for all figures
theme_set(theme_bw(base_family = "serif"))


# ===========================================================================
# Figure 2: Effect of fragmentation on NPP across forest amount levels
# ===========================================================================

# --- 2.1 Subset and rename dependent variables -----------------------------
amountlevel_npp <- amountlevel %>%
  dplyr::select(depend_var, FFI_coef, FFI_p, max_distance, amountlevel) %>%
  filter(depend_var %in% c("Npp_total20", "NppTotal_stability")) %>%
  filter(amountlevel %in% c("Global", "<10%", "10-30%", "30-60%",
                            "60-90%", ">90%")) %>%
  mutate(
    response = case_when(
      depend_var == "Npp_total20"        ~ "NPP_total20",
      depend_var == "NppTotal_stability" ~ "NPP_stability",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(response))   # drop any rows that did not match

# --- 2.2 Summarise mean and SD of the FFI coefficient ----------------------
result <- amountlevel_npp %>%
  group_by(response, amountlevel) %>%
  summarise(
    mean_value = mean(FFI_coef, na.rm = TRUE),
    sd_value   = sd(FFI_coef,   na.rm = TRUE),
    n          = sum(!is.na(FFI_coef)),
    .groups    = "drop"
  )

# --- 2.3 Order factors for plotting ----------------------------------------
result <- result %>%
  mutate(
    amountlevel = factor(
      amountlevel,
      levels = c("Global", "<10%", "10-30%", "30-60%", "60-90%", ">90%")
    ),
    response = factor(
      response,
      levels = c("NPP_total20", "NPP_stability"),
      labels = c("NPP(Total)", "NPP(Stability)")
    )
  )

# --- 2.4 Plot ---------------------------------------------------------------
plot_2 <- ggplot(result,
                 aes(x = amountlevel, y = mean_value, fill = response)) +
  # Shaded background: Global vs. the five amount strata
  annotate("rect",
           xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf,
           alpha = 0.3, fill = "gray70") +
  annotate("rect",
           xmin = 1.5, xmax = 5.5, ymin = -Inf, ymax = Inf,
           alpha = 0.1, fill = "gray90") +
  # Reference line at zero effect
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray40", linewidth = 0.6) +
  # Separator between Global and the amount strata
  geom_vline(xintercept = 1.5, linetype = "dashed",
             color = "red", linewidth = 1) +
  # Bars and error bars
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.8) +
  geom_errorbar(
    aes(ymin = mean_value - sd_value,
        ymax = mean_value + sd_value),
    position = position_dodge(0.8),
    width = 0.2, linewidth = 0.5, color = "black"
  ) +
  labs(
    x    = "Amount level",
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
    axis.text        = element_text(size = 10, color = "black"),
    axis.title.y     = element_text(size = 11, face = "bold"),
    axis.title.x     = element_blank(),
    legend.text      = element_text(size = 11),
    legend.title     = element_blank(),
    legend.position  = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot_2


###############################################################################
# End of script
###############################################################################

