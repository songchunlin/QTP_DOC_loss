# DOC concentration and other changes ============================================
## Overall changes--------
library(tidyverse)
library(ggplotify)  # ggplotGrob/as.grob for insets
library(grid)
library(patchwork)  # assemble panels & collect legend

DOC_change  <- "~/Documents/Papers/DOC_degradation/Data/DOC_change.csv"
outdir      <- "~/Documents/Papers/DOC_degradation/Data/Output"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

raw <- readr::read_csv(DOC_change, show_col_types = FALSE)

# Facet order (rows) & legend order
indices_to_plot <- c("DOC", "SUVA254", "SR", "FI", "HIX", "BIX")
season_levels   <- c("Spring", "Summer", "Autumn")
tick_days       <- c(0, 1, 2, 3, 7, 14, 28)

# Normalize experiment labels and set desired column order (accept "Bio+Photo")
raw <- raw %>%
  mutate(
    Experiment = dplyr::recode(Experiment, "Bio+Photo" = "Bio-Photo",
                               .default = as.character(Experiment))
  )

exp_order <- c("Bio", "Photo", "Bio-Photo")

raw <- raw %>%
  mutate(
    Season          = factor(Season, levels = season_levels),
    Experiment      = factor(Experiment, levels = exp_order),
    Incubation_days = as.numeric(Incubation_days)
  )

# Select + reshape
idx_cols  <- as.vector(rbind(indices_to_plot, paste0(indices_to_plot, "_sd")))
keep_cols <- c("Experiment", "Incubation_days", "Year", "Season")
raw_sel   <- raw %>% select(any_of(c(keep_cols, idx_cols)))

long <- raw_sel %>%
  pivot_longer(
    cols          = setdiff(names(raw_sel), keep_cols),
    names_to      = c("Index", "Measure"),
    names_pattern = paste0("^(", paste(indices_to_plot, collapse = "|"), ")(?:_(sd))?$"),
    values_to     = "val",
    values_drop_na = FALSE
  ) %>%
  mutate(Measure = replace_na(na_if(Measure, ""), "value")) %>%
  pivot_wider(names_from = Measure, values_from = val, values_fill = NA_real_) %>%
  rename(Value = value, SD = sd) %>%
  mutate(Index = factor(Index, levels = indices_to_plot)) %>%
  arrange(Experiment, Index, Season, Incubation_days)

# % change day 0 -> last day (per Experiment × Index × Season)
change_df <- long %>%
  filter(!is.na(Value)) %>%
  group_by(Experiment, Index, Season) %>%
  summarise(
    v0 = Value[which.min(Incubation_days)],
    vN = Value[which.max(Incubation_days)],
    .groups = "drop"
  ) %>%
  mutate(
    pct_change = 100 * (vN - v0) / v0,
    pct_lab    = sprintf("%+.1f%%", pct_change)
  )

# Global y-limits per Index (same across experiments; +5% padding)
ylim_tbl <- long %>%
  group_by(Index) %>%
  summarise(
    ymin = min(Value, na.rm = TRUE),
    ymax = max(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pad  = 0.05 * (ymax - ymin + 1e-12),
    ymin = ymin - pad,
    ymax = ymax + pad
  )

# Theme (no grid; black borders/ticks/axis text)
base_theme <- theme_minimal(base_size = 11) +
  theme(
    text = element_text(family = "Helvetica Light", face = "plain"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.ticks       = element_line(color = "black", linewidth = 0.4),
    axis.text        = element_text(color = "black"),
    axis.title       = element_text(color = "black")
  )

# All 18 insets for percentage change
# Fractions 0..1 within each panel (0 = left/bottom, 1 = right/top).
inset_pos18 <- tribble(
  ~Index,    ~Experiment, ~x_left, ~x_right, ~y_bottom, ~y_top,
  "DOC",     "Bio",        0.45,     1.05,     0.66,      1.06,
  "DOC",     "Photo",      0.45,     1.05,     0.66,      1.06,
  "DOC",     "Bio-Photo",  0.45,     1.05,     0.66,      1.06,
  "SUVA254", "Bio",        0.45,     1.05,     -0.06,      0.34,
  "SUVA254", "Photo",      0.45,     1.05,     -0.06,      0.34,
  "SUVA254", "Bio-Photo",  0.45,     1.05,     -0.06,      0.34,
  "SR",      "Bio",        0.45,     1.05,     0.58,      0.98,
  "SR",      "Photo",      0.45,     1.05,     0.04,      0.44,
  "SR",      "Bio-Photo",  0.45,     1.05,     0.04,      0.44,
  "FI",      "Bio",        0.45,     1.05,     -0.06,      0.34,
  "FI",      "Photo",      0.45,     1.05,     0.66,      1.06,
  "FI",      "Bio-Photo",  0.45,     1.05,     0.66,      1.06,
  "HIX",     "Bio",        0.45,     1.05,     0.01,      0.41,
  "HIX",     "Photo",      0.45,     1.05,     0.66,      1.06,
  "HIX",     "Bio-Photo",  0.45,     1.05,     0.66,      1.06,
  "BIX",     "Bio",        0.45,     1.05,     0.58,      0.98,
  "BIX",     "Photo",      0.45,     1.05,     -0.06,      0.34,
  "BIX",     "Bio-Photo",  0.45,     1.05,     -0.06,      0.34
)

# Tiny bar-chart inset for one panel (NO border)
season_colors <- c(
  "Spring" = "#6BA3C9", 
  "Summer" = "#D8C27A", 
  "Autumn" = "#C04F15"
)

make_inset_grob <- function(df_idx) {
  p <- ggplot(df_idx, aes(x = Season, y = pct_change, fill = Season)) +
    geom_col(width = 0.5) +
    geom_text(aes(label = pct_lab, color = Season),
              vjust = ifelse(df_idx$pct_change >= 0, -0.3, 1.2),
              size = 2.5) +
    #scale_fill_brewer(palette = "Set1", drop = FALSE) +
    #scale_color_brewer(palette = "Set1", drop = FALSE) +  # sync text color with fill
    scale_color_manual(values = season_colors, drop = FALSE) +
    scale_fill_manual(values = season_colors, drop = FALSE) +
    coord_cartesian(clip = "off") +
    theme_void(base_size = 9) +
    theme(legend.position = "none")
  ggplotGrob(p)
}

# Axis title helper (units for DOC and SUVA254 only)
index_y_label <- function(index) {
  if (index == "DOC")      return(expression("DOC (mg L"^-1*")"))
  if (index == "SUVA254")  return(expression("SUVA"[254]*" (L mg C"^-1*" m"^-1*")"))
  if (index == "SR")  return(expression("S"[R]))
  return(index)  # others show no y-title to save space unless in left column
}

# Single panel (line/points/errorbars + inset), no panel title
make_panel_plot <- function(df_idx, chg_idx, index_name, exp_name,
                            show_x_title = FALSE, show_y_title = FALSE) {
  
  yl <- ylim_tbl %>% filter(Index == index_name)
  x_title <- if (show_x_title) "Incubation days" else NULL
  y_title <- if (show_y_title) index_y_label(index_name) else NULL
  
  df_line <- df_idx %>% filter(!is.na(Value))
  
  p <- ggplot(
    df_line,
    aes(x = Incubation_days, y = Value, color = Season, group = Season)
  ) +
    geom_line(linetype = "dotted", linewidth = 0.6, na.rm = TRUE) +
    geom_point(size = 2, na.rm = TRUE) +
    geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                  width = 0.1, alpha = 0.5, na.rm = TRUE) +
    #scale_color_brewer(palette = "Set1", drop = FALSE) +
    scale_color_manual(values = season_colors, drop = FALSE) +
    scale_x_continuous(breaks = tick_days, minor_breaks = NULL) +
    scale_y_continuous(limits = c(yl$ymin, yl$ymax)) +
    labs(x = x_title, y = y_title) +
    base_theme +
    theme(legend.position = "none")
  
  # Insert per-panel (Index × Experiment) placement
  pos_row <- inset_pos18 %>% filter(Index == index_name, Experiment == exp_name)
  if (nrow(pos_row) != 1) {
    stop("Inset position missing/duplicated for panel: ", index_name, " × ", exp_name)
  }
  
  x_min <- min(tick_days); x_max <- max(tick_days)
  y_min <- yl$ymin;       y_max <- yl$ymax
  
  xmin  <- x_min + pos_row$x_left  * (x_max - x_min)
  xmax  <- x_min + pos_row$x_right * (x_max - x_min)
  ymin  <- y_min + pos_row$y_bottom* (y_max - y_min)
  ymax  <- y_min + pos_row$y_top   * (y_max - y_min)
  
  inset_grob <- make_inset_grob(chg_idx)
  
  p + annotation_custom(grob = inset_grob, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}

# Column headers
header_plot <- function(label_text) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = label_text, fontface = "bold") +
    coord_cartesian(xlim = c(0,1), ylim = c(0,1), clip = "off") +
    theme_void(base_size = 12)
}
col_headers <- list(
  header_plot("Biodegradation"),
  header_plot("Photodegradation"),
  header_plot("Bio-photodegradation")
)
header_row <- wrap_plots(col_headers, ncol = 3)

# Build all 18 panels (x-title only bottom row; y-title only left column)
all_panels <- list()
for (idx in indices_to_plot) {
  for (exp_name in exp_order) {
    df_panel  <- long      %>% filter(Index == idx, Experiment == exp_name)
    chg_panel <- change_df %>% filter(Index == idx, Experiment == exp_name)
    
    show_x <- (idx == tail(indices_to_plot, 1))  # bottom row only
    show_y <- (exp_name == "Bio")                # left column only
    
    if (nrow(df_panel) == 0) {
      x_title <- if (show_x) "Incubation days" else NULL
      y_title <- if (show_y) index_y_label(idx) else NULL
      p_blank <- ggplot() + theme_void() +
        theme(panel.border = element_rect(color="black", fill=NA, linewidth=0.6)) +
        labs(x = x_title, y = y_title)
      all_panels <- append(all_panels, list(p_blank))
    } else {
      p <- make_panel_plot(df_panel, chg_panel, idx, exp_name,
                           show_x_title = show_x, show_y_title = show_y)
      all_panels <- append(all_panels, list(p))
    }
  }
}

# 6x3 grid + legend at bottom
grid_6x3 <- wrap_plots(all_panels, ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")

# Stack headers above the grid
final_plot <- header_row / grid_6x3 + plot_layout(heights = c(0.5, 16))

# Save
outfile_base <- file.path(outdir, "DOC_indices_6x3_with_manual_insets_v2")
ggsave(paste0(outfile_base, ".png"), final_plot, width = 7.5, height = 12.5, dpi = 600)
ggsave(paste0(outfile_base, ".pdf"), final_plot, width = 7.5, height = 12.5, device = cairo_pdf)

## Overall changes version 2--------

DOC_change  <- "~/Documents/Papers/DOC_degradation/Data/DOC_change.csv"
outdir      <- "~/Documents/Papers/DOC_degradation/Data/Output"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

raw <- readr::read_csv(DOC_change, show_col_types = FALSE)

# Facet order (rows) & legend order
indices_to_plot <- c("DOC", "SUVA254", "SR", "FI", "HIX", "BIX")
season_levels   <- c("Spring", "Summer", "Autumn")
tick_days       <- c(0, 1, 2, 3, 7, 14, 28)
index_labeller <- function(x) {
  ifelse(x == "SR", "S[R]", x)
}

# Normalize experiment labels and set desired column order (accept "Bio+Photo")
raw <- raw %>%
  mutate(
    Experiment = dplyr::recode(Experiment, "Bio+Photo" = "Bio-Photo",
                               .default = as.character(Experiment))
  )

exp_order <- c("Bio", "Photo", "Bio-Photo")

raw <- raw %>%
  mutate(
    Season          = factor(Season, levels = season_levels),
    Experiment      = factor(Experiment, levels = exp_order),
    Incubation_days = as.numeric(Incubation_days)
  )

# Select + reshape
idx_cols  <- as.vector(rbind(indices_to_plot, paste0(indices_to_plot, "_sd")))
keep_cols <- c("Experiment", "Incubation_days", "Year", "Season")
raw_sel   <- raw %>% select(any_of(c(keep_cols, idx_cols)))

long <- raw_sel %>%
  pivot_longer(
    cols          = setdiff(names(raw_sel), keep_cols),
    names_to      = c("Index", "Measure"),
    names_pattern = paste0("^(", paste(indices_to_plot, collapse = "|"), ")(?:_(sd))?$"),
    values_to     = "val",
    values_drop_na = FALSE
  ) %>%
  mutate(Measure = replace_na(na_if(Measure, ""), "value")) %>%
  pivot_wider(names_from = Measure, values_from = val, values_fill = NA_real_) %>%
  rename(Value = value, SD = sd) %>%
  mutate(Index = factor(Index, levels = indices_to_plot)) %>%
  arrange(Experiment, Index, Season, Incubation_days)

# % change day 0 -> last day (per Experiment × Index × Season)
change_df <- long %>%
  filter(!is.na(Value)) %>%
  group_by(Experiment, Index, Season) %>%
  summarise(
    v0 = Value[which.min(Incubation_days)],
    vN = Value[which.max(Incubation_days)],
    .groups = "drop"
  ) %>%
  mutate(
    pct_change = 100 * (vN - v0) / v0,
    pct_lab    = sprintf("%+.1f%%", pct_change)
  )

# Global y-limits per Index (same across experiments; +5% padding)
ylim_tbl <- long %>%
  group_by(Index) %>%
  summarise(
    ymin = min(Value, na.rm = TRUE),
    ymax = max(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pad  = 0.05 * (ymax - ymin + 1e-12),
    ymin = ymin - pad,
    ymax = ymax + pad
  )

# Theme (no grid; black borders/ticks/axis text)
base_theme <- theme_minimal(base_size = 11) +
  theme(
    text = element_text(family = "Helvetica Light", face = "plain"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.15),
    axis.ticks       = element_line(color = "black", linewidth = 0.15),
    axis.text        = element_text(color = "black"),
    axis.title       = element_text(color = "black")
  )

# Inset placement table (used ONLY for DOC and SUVA254 now)
inset_pos18 <- tribble(
  ~Index,    ~Experiment, ~x_left, ~x_right, ~y_bottom, ~y_top,
  "DOC",     "Bio",        0.45,     1.05,     0.66,      1.06,
  "DOC",     "Photo",      0.45,     1.05,     0.66,      1.06,
  "DOC",     "Bio-Photo",  0.45,     1.05,     0.66,      1.06,
  "SUVA254", "Bio",        0.45,     1.05,     -0.06,      0.34,
  "SUVA254", "Photo",      0.45,     1.05,     -0.06,      0.34,
  "SUVA254", "Bio-Photo",  0.45,     1.05,     -0.06,      0.34
)

# Tiny bar-chart inset for one panel (NO border)
season_colors <- c(
  "Spring" = "#6BA3C9",
  "Summer" = "#D8C27A",
  "Autumn" = "#C04F15"
)

make_inset_grob <- function(df_idx) {
  p <- ggplot(df_idx, aes(x = Season, y = pct_change, fill = Season)) +
    geom_col(width = 0.5) +
    geom_text(aes(label = pct_lab, color = Season),
              vjust = ifelse(df_idx$pct_change >= 0, -0.3, 1.2),
              size = 2.5) +
    scale_color_manual(values = season_colors, drop = FALSE) +
    scale_fill_manual(values = season_colors, drop = FALSE) +
    coord_cartesian(clip = "off") +
    theme_void(base_size = 9) +
    theme(legend.position = "none")
  ggplotGrob(p)
}

# Axis title helper (units for DOC and SUVA254 only)
index_y_label <- function(index) {
  if (index == "DOC")      return(expression("DOC (mg L"^-1*")"))
  if (index == "SUVA254")  return(expression("SUVA"[254]*" (L mg C"^-1*" m"^-1*")"))
  return(index)
}

# Time-series panel with inset (used ONLY for DOC and SUVA254)
make_panel_plot <- function(df_idx, chg_idx, index_name, exp_name,
                            show_x_title = FALSE, show_y_title = FALSE) {
  
  yl <- ylim_tbl %>% filter(Index == index_name)
  x_title <- if (show_x_title) "Incubation days" else NULL
  y_title <- if (show_y_title) index_y_label(index_name) else NULL
  
  df_line <- df_idx %>% filter(!is.na(Value))
  
  p <- ggplot(
    df_line,
    aes(x = Incubation_days, y = Value, color = Season, group = Season)
  ) +
    geom_line(linetype = "dotted", linewidth = 0.6, na.rm = TRUE) +
    geom_point(size = 2, na.rm = TRUE) +
    geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                  width = 0.1, alpha = 0.5, na.rm = TRUE) +
    scale_color_manual(values = season_colors, drop = FALSE) +
    scale_x_continuous(breaks = tick_days, minor_breaks = NULL, labels= c(0,"","",3,7,14,28)) +
    scale_y_continuous(limits = c(yl$ymin, yl$ymax)) +
    labs(x = x_title, y = y_title) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "none",
      text = element_text(family = "Helvetica Light", face = "plain"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.3),
      axis.ticks       = element_line(color = "black", linewidth = 0.3),
      axis.text        = element_text(color = "black"),
      axis.title       = element_text(color = "black")
    )
  
  pos_row <- inset_pos18 %>% filter(Index == index_name, Experiment == exp_name)
  if (nrow(pos_row) != 1) stop("Inset position missing/duplicated for: ", index_name, " × ", exp_name)
  
  x_min <- min(tick_days); x_max <- max(tick_days)
  y_min <- yl$ymin;       y_max <- yl$ymax
  
  xmin  <- x_min + pos_row$x_left  * (x_max - x_min)
  xmax  <- x_min + pos_row$x_right * (x_max - x_min)
  ymin  <- y_min + pos_row$y_bottom* (y_max - y_min)
  ymax  <- y_min + pos_row$y_top   * (y_max - y_min)
  
  inset_grob <- make_inset_grob(chg_idx)
  
  p + annotation_custom(grob = inset_grob, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}

# Column headers
header_plot <- function(label_text) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = label_text) +
    coord_cartesian(xlim = c(0,1), ylim = c(0,1), clip = "off") +
    theme_void(base_size = 12)
}
col_headers <- list(
  header_plot("Biodegradation"),
  header_plot("Photodegradation"),
  header_plot("Bio-photodegradation")
)
header_row <- wrap_plots(col_headers, ncol = 3)

# Manual fixed y-limits per Index (applied to all three Experiment columns)
manual_lim_df <- tibble(
  Index = factor(c("SR","FI","HIX","BIX"), levels = c("SR","FI","HIX","BIX")),
  ymin  = c(-30, -20, -40,  -10),
  ymax  = c(100,   5,   10,  45)
)


# NEW: one "inset-only" panel per Experiment containing SR/FI/HIX/BIX % changes
make_pctonly_panel <- function(exp_name, show_legend = FALSE) {
  
  df <- change_df %>%
    filter(Experiment == exp_name, Index %in% c("SR", "FI", "HIX", "BIX")) %>%
    mutate(
      Index  = factor(Index, levels = c("SR", "FI", "HIX", "BIX")),
      Season = factor(Season, levels = season_levels)
    )
  
  # For facet-specific limits via geom_blank
  lim_df <- manual_lim_df
  
  # (optional) per-index tick breaks via faceting trick
  
  ggplot(df, aes(x = Season, y = pct_change, fill = Season)) +
    geom_hline(yintercept = 0, linewidth = 0.2, color = "grey50") +
    geom_col(width = 0.55) +
    geom_text(
      aes(label = pct_lab, color = Season),
      vjust = ifelse(df$pct_change >= 0, -0.25, 1.15),
      size = 2.4
    ) +
    # FORCE each facet's y-range (Index-specific) 
  geom_blank(
    data = lim_df,
    inherit.aes = FALSE,
    aes(y = ymin, Index = Index)
  ) +
    geom_blank(
      data = lim_df,
      inherit.aes = FALSE,
      aes(y = ymax, Index = Index)
    ) +
    facet_wrap(
      ~ Index, ncol = 1, scales = "free_y",
      labeller = as_labeller(
        function(x) ifelse(x == "SR", "S[R]", x),
        default = label_parsed
      )
    ) +
    scale_fill_manual(values = season_colors, drop = FALSE) +
    scale_color_manual(values = season_colors, drop = FALSE) +
    
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = "% change (day 0 to day 28)") +
    base_theme +
    theme(
      legend.position = if (show_legend) "bottom" else "none",
      legend.box = "horizontal",
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.margin = margin(6, 10, 14, 10)
    )
}

# Build only the panels you want:
# Rows a–b: DOC + SUVA254 time series (2×3)
# Row c–f: inset-only summaries merged into one row (1×3)

# Row a: DOC (3 panels)
row_DOC <- wrap_plots(
  lapply(exp_order, function(exp_name) {
    df_panel  <- long      %>% filter(Index == "DOC", Experiment == exp_name)
    chg_panel <- change_df %>% filter(Index == "DOC", Experiment == exp_name)
    
    make_panel_plot(df_panel, chg_panel, "DOC", exp_name,
                    show_x_title = FALSE,
                    show_y_title = (exp_name == "Bio"))
  }),
  ncol = 3
)

# Row b: SUVA254 (3 panels)
row_SUVA <- wrap_plots(
  lapply(exp_order, function(exp_name) {
    df_panel  <- long      %>% filter(Index == "SUVA254", Experiment == exp_name)
    chg_panel <- change_df %>% filter(Index == "SUVA254", Experiment == exp_name)
    
    make_panel_plot(df_panel, chg_panel, "SUVA254", exp_name,
                    show_x_title = TRUE,                 # <- change here
                    show_y_title = (exp_name == "Bio"))
  }),
  ncol = 3
)

# Row (merged c–f): SR/FI/HIX/BIX percent-only (3 panels)
row_pctonly <- wrap_plots(
  list(
    make_pctonly_panel("Bio",       show_legend = FALSE),
    make_pctonly_panel("Photo",     show_legend = FALSE),
    make_pctonly_panel("Bio-Photo", show_legend = FALSE)
  ),
  ncol = 3,
  guides = "collect"
) + theme(
  legend.position = "none")

# Combine into final layout: headers + (DOC row + SUVA row + pctonly row)
grid_new <- row_DOC / row_SUVA / row_pctonly +
  plot_layout(heights = c(1, 1, 2.4))

final_plot <- header_row / grid_new + plot_layout(heights = c(0.45, 10))

# Save
outfile_base <- file.path(outdir, "DOC_indices_ab_plus_pctonly_SR_FI_HIX_BIX-1")
ggsave(paste0(outfile_base, ".png"), final_plot, width = 7, height = 9.0, dpi = 600)
ggsave(paste0(outfile_base, ".pdf"), final_plot, width = 7, height = 9.0, device = cairo_pdf)

## Relative abundance of C1, C2, & C3------
# Basic setup
exp_order <- c("Bio", "Photo", "Bio-Photo")
season_levels <- c("Spring", "Summer", "Autumn")
tick_days <- c(0, 1, 2, 3, 7, 14, 28)

raw <- raw %>%
  mutate(
    Experiment = recode(Experiment, "Bio+Photo" = "Bio-Photo"),
    Experiment = factor(Experiment, levels = exp_order),
    Season = factor(Season, levels = season_levels),
    Incubation_days = as.numeric(Incubation_days)
  )

# Check that C1, C2, C3 exist
if (!all(c("C1", "C2", "C3") %in% names(raw))) {
  stop("C1, C2, and C3 columns are required in DOC_change.csv")
}

# Reshape for plotting
# include any extra days that might be present so they don't become NA
day_levels <- sort(unique(c(tick_days, raw$Incubation_days)))

c_long <- raw %>%
  select(Experiment, Season, Incubation_days, C1, C2, C3) %>%
  pivot_longer(c(C1, C2, C3), names_to = "Component", values_to = "Abundance") %>%
  mutate(
    Component = factor(trimws(toupper(Component)), levels = c("C1","C2","C3")),
    Abundance_clean = pmax(Abundance, 0)  # floor negatives at 0
  ) %>%
  group_by(Experiment, Season, Incubation_days) %>%
  mutate(
    total = sum(Abundance_clean, na.rm = TRUE),
    Rel_abundance = if_else(total > 0, 100 * Abundance_clean / total, 0)
  ) %>%
  ungroup() %>%
  mutate(
    Rel_abundance = pmin(pmax(Rel_abundance, 0), 100),  # clamp to [0,100]
    Incu_f  = factor(Incubation_days, levels = day_levels, ordered = TRUE),
    pct_lab = if_else(Rel_abundance > 0, sprintf("%.1f", Rel_abundance), "")
  )

# Colors

component_colors <- c(
  C1 = "#EBD8B7",  # light beige
  C2 = "#C69C72",  # tan
  C3 = "#A6611A"   # dark umber
)

# Horizontal stacked % bar plot with labels
p <- ggplot(
  c_long,
  aes(x = Rel_abundance, y = Incu_f, fill = Component)
) +
  geom_col(position = "stack", color = "black", linewidth = 0.2) +
  # % labels centered within each stacked segment (black text)
  geom_text(
    aes(label = ifelse(Rel_abundance > 0 & !is.na(Component),
                       sprintf("%.1f", Rel_abundance), "")),
    position = position_stack(vjust = 0.5),
    color = "black", size = 2.6
  )+
  scale_fill_manual(values = component_colors, drop = FALSE, na.translate = FALSE) +
  facet_grid(
    Season ~ Experiment,
    scales = "free_y"
  ) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                     expand = expansion(mult = c(0, 0.02)))+
  labs(
    x = "Relative abundance (%)",
    y = "Incubation days",
    fill = "Component"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    text = element_text(family = "Helvetica Light"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.0, "lines"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    strip.text.x = element_blank(),         # drop default column labels
    strip.text.y = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    legend.position = "bottom"
  )

# Column headers
header_plot <- function(label_text) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = label_text, fontface = "bold") +
    coord_cartesian(xlim = c(0,1), ylim = c(0,1), clip = "off") +
    theme_void(base_size = 12)
}
col_headers <- list(
  header_plot("Biodegradation"),
  header_plot("Photodegradation"),
  header_plot("Bio-photodegradation")
)
header_row <- wrap_plots(col_headers, ncol = 3)

# Combine header + plot
final_plot <- header_row / p + plot_layout(heights = c(0.45, 9))

# Save
outfile_base <- file.path(outdir, "C_components_relative_abundance_horizontal")
ggsave(paste0(outfile_base, ".png"), final_plot, width = 6, height = 6, dpi = 600)
ggsave(paste0(outfile_base, ".pdf"), final_plot, width = 6, height = 6,
       device = cairo_pdf)
print(final_plot)


# DOM change with FT-ICR MS ============================================
# FT-ICR MS: T0→T28 formula reactivity (Δlog2)
# Seasons: Spring, Summer | Methods: Bio, Photo, BioPhoto
# Van Krevelen + class mean Δlog2 (faceted)

## Read data & prepare VK table with % change --------
# Setup
setwd("~/Documents/Papers/DOC_degradation")

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(scales)
  library(ggpubr)
  library(ggvenn) # If not installed: install.packages("ggvenn")
  # library(ComplexUpset) # Uncomment if you use the UpSet plot section
})

# Defined Directories
FT_dir  <- "~/Documents/Papers/DOC_degradation/Data/FT"
out_dir <- "~/Documents/Papers/DOC_degradation/Data/Output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# helpers functions
pick_col <- function(df, aliases, required = TRUE) {
  hit <- aliases[aliases %in% names(df)]
  if (length(hit)) return(hit[1])
  if (required) stop("Required column not found. Tried: ", paste(aliases, collapse=", "))
  NA_character_
}

load_one <- function(path, label) {
  raw <- readr::read_csv(path, show_col_types = FALSE)
  f_col  <- pick_col(raw, c("sumFormula","Formula","formula","mf"))
  I_col  <- pick_col(raw, c("relativeintens","ObservedIntens","Intensity","intensity","abundance"))
  hc_col <- pick_col(raw, c("HCraito","H_C","H_C_ratio"), required = FALSE)
  oc_col <- pick_col(raw, c("OCraito","O_C","O_C_ratio"), required = FALSE)
  df <- raw %>% dplyr::select(
    Formula = !!sym(f_col),
    RelInt  = !!sym(I_col),
    HCraito = dplyr::all_of(na.omit(hc_col)),
    OCraito = dplyr::all_of(na.omit(oc_col)),
    dplyr::any_of(c("C","H","O","N","S","ObservedM_z","aimod","dbe","nosc"))
  )
  for (el in c("C","H","O","N","S")) if (el %in% names(df)) df[[el]] <- suppressWarnings(as.numeric(df[[el]]))
  df$Condition <- label
  df
}

vk_from_elements_or_ratios <- function(df, hc_hint = c("HCraito","H_C","H_C_ratio"),
                                       oc_hint = c("OCraito","O_C","O_C_ratio")) {
  hc_hit <- hc_hint[hc_hint %in% names(df)][1]
  oc_hit <- oc_hint[oc_hint %in% names(df)][1]
  df %>%
    mutate(
      O_C = dplyr::coalesce(if (!is.na(oc_hit)) .data[[oc_hit]] else NA_real_,
                            if (all(c("O","C") %in% names(df))) suppressWarnings(as.numeric(O)/as.numeric(C)) else NA_real_),
      H_C = dplyr::coalesce(if (!is.na(hc_hit)) .data[[hc_hit]] else NA_real_,
                            if (all(c("H","C") %in% names(df))) suppressWarnings(as.numeric(H)/as.numeric(C)) else NA_real_)
    ) %>%
    filter(is.finite(O_C), is.finite(H_C))
}

merge_pair <- function(file_T0, file_T28, season, method) {
  d0  <- load_one(file_T0, "T0")
  d28 <- load_one(file_T28, "T28")
  
  merged <- inner_join(
    d0 %>% rename(RelInt_T0 = RelInt, HCraito0 = HCraito, OCraito0 = OCraito,
                  C0 = C, H0 = H, O0 = O, N0 = N, S0 = S),
    d28 %>% rename(RelInt_T28 = RelInt, HCraito28 = HCraito, OCraito28 = OCraito,
                   C28 = C, H28 = H, O28 = O, N28 = N, S28 = S),
    by = "Formula"
  ) %>%
    mutate(
      delta_log2 = log2((RelInt_T28 + 1e-9) / (RelInt_T0 + 1e-9)),
      O_C = dplyr::coalesce(OCraito0, O0 / C0),
      H_C = dplyr::coalesce(HCraito0, H0 / C0),
      # Class from T0 composition (use 28 if needed)
      Class = case_when(
        coalesce(N0, 0) > 0 & coalesce(S0, 0) > 0 ~ "CHONS",
        coalesce(S0, 0) > 0 ~ "CHOS",
        coalesce(N0, 0) > 0 ~ "CHON",
        TRUE ~ "CHO"
      ),
      Season = season,
      Method = method
    )
  
  # Keep only finite ratios
  merged %>% filter(is.finite(O_C), is.finite(H_C))
}

# file map (Bio, Photo, BioPhoto; Spring & Summer)
pairs_all <- tribble(
  ~Season,  ~Method,    ~file_T0,                                    ~file_T28,
  "Spring", "Bio",      "2024_Spring_Bio_T0.csv",      "2024_Spring_Bio_T28.csv",
  "Spring", "Photo",    "2024_Spring_Photo_T0.csv",    "2024_Spring_Photo_T28.csv",
  "Spring", "BioPhoto", "2024_Spring_BioPhoto_T0.csv", "2024_Spring_BioPhoto_T28.csv",
  "Summer", "Bio",      "2024_Summer_Bio_T0.csv",      "2024_Summer_Bio_T28.csv",
  "Summer", "Photo",    "2024_Summer_Photo_T0.csv",    "2024_Summer_Photo_T28.csv",
  "Summer", "BioPhoto", "2024_Summer_BioPhoto_T0.csv", "2024_Summer_BioPhoto_T28.csv"
) %>%
  mutate(file_T0  = file.path(FT_dir, file_T0),
         file_T28 = file.path(FT_dir, file_T28))

# build combined VK table with % change for all methods  (keeps T0-only and T28-only)
vk_pct_df <- purrr::pmap_dfr(pairs_all, function(Season, Method, file_T0, file_T28) {
  d0  <- load_one(file_T0,  "T0")  %>% vk_from_elements_or_ratios() %>%
    transmute(Formula, O_C0 = O_C, H_C0 = H_C, RelInt_T0  = RelInt, N0 = N, S0 = S, mz0 = ObservedM_z, AImod0 = aimod, DBE0 = dbe, NOSC0 = nosc)
  d28 <- load_one(file_T28, "T28") %>% vk_from_elements_or_ratios() %>%
    transmute(Formula, O_C28 = O_C, H_C28 = H_C, RelInt_T28 = RelInt, N28 = N, S28 = S, mz28 = ObservedM_z, AImod28 = aimod, DBE28 = dbe, NOSC28 = nosc)
  
  full_join(d0, d28, by = "Formula", relationship = "many-to-many") %>%
    mutate(
      # O/C, H/C for plotting
      O_C = coalesce(O_C0, O_C28),
      H_C = coalesce(H_C0, H_C28),
      mz0    = mz0,
      AImod0 = AImod0,
      DBE0   = DBE0,
      NOSC0  = NOSC0,
      mz28    = mz28,
      AImod28 = AImod28,
      DBE28   = DBE28,
      NOSC28  = NOSC28,
      # intensities (treat missing as 0)
      RelInt_T0  = replace_na(RelInt_T0,  0),
      RelInt_T28 = replace_na(RelInt_T28, 0),
      present0   = RelInt_T0  > 0,
      present28  = RelInt_T28 > 0,
      # % change (−100%..+100%)
      pct_change = case_when(
        RelInt_T0 == 0 & RelInt_T28 > 0  ~  1,
        RelInt_T0 == 0 & RelInt_T28 == 0 ~  0,
        TRUE ~ (RelInt_T28 - RelInt_T0) / RelInt_T0
      ),
      pct_change = scales::squish(pct_change, range = c(-1, 1)),
      # Class using N/S presence (prefer T0, fallback to T28)
      N_any = coalesce(N0, N28, 0),
      S_any = coalesce(S0, S28, 0),
      Class = case_when(
        coalesce(N_any,0) > 0 & coalesce(S_any,0) > 0 ~ "CHONS",
        coalesce(S_any,0) > 0                         ~ "CHOS",
        coalesce(N_any,0) > 0                         ~ "CHON",
        TRUE                                          ~ "CHO"
      ),
      Season = Season,
      Method = Method
    ) %>%
    select(Formula, O_C, H_C, mz0, mz28, AImod0, AImod28, DBE0, DBE28, NOSC0, NOSC28, 
           RelInt_T0, RelInt_T28, present0, present28,
           pct_change, Class, Season, Method)
})

vk_pct_df <- vk_pct_df %>%
  mutate(
    Season = factor(Season, levels = c("Spring","Summer")),
    Method = factor(Method, levels = c("Bio","Photo","BioPhoto"),
                    labels = c("Biodegradation","Photodegradation","Bio-photodegradation")),
    Class  = factor(Class, levels = c("CHO","CHON","CHOS","CHONS"))
  )

nrow(vk_pct_df) # 35751

readr::write_csv(vk_pct_df, file.path(out_dir, "vk_pct_df4.csv"))

## Read data in stupid ways --------
spring_bio_T0 <- read_csv(file.path(FT_dir, "2024_Spring_Bio_T0.csv")) %>% mutate(Season = "Spring", Method = "Bio", Time = "T0")

spring_bio_T28 <- read_csv(file.path(FT_dir, "2024_Spring_Bio_T28.csv")) %>% mutate(Season = "Spring", Method = "Bio", Time = "T28")

spring_photo_T0 <- read_csv(file.path(FT_dir, "2024_Spring_Photo_T0.csv")) %>% mutate(Season = "Spring", Method = "Photo", Time = "T0")

spring_photo_T28 <- read_csv(file.path(FT_dir, "2024_Spring_Photo_T28.csv")) %>% mutate(Season = "Spring", Method = "Photo", Time = "T28")

spring_biophoto_T0 <- read_csv(file.path(FT_dir, "2024_Spring_BioPhoto_T0.csv")) %>% mutate(Season = "Spring", Method = "BioPhoto", Time = "T0")

spring_biophoto_T28 <- read_csv(file.path(FT_dir, "2024_Spring_BioPhoto_T28.csv")) %>% mutate(Season = "Spring", Method = "BioPhoto", Time = "T28")

summer_bio_T0 <- read_csv(file.path(FT_dir, "2024_Summer_Bio_T0.csv")) %>% mutate(Season = "Summer", Method = "Bio", Time = "T0")

summer_bio_T28 <- read_csv(file.path(FT_dir, "2024_Summer_Bio_T28.csv")) %>% mutate(Season = "Summer", Method = "Bio", Time = "T28")

summer_photo_T0 <- read_csv(file.path(FT_dir, "2024_Summer_Photo_T0.csv")) %>% mutate(Season = "Summer", Method = "Photo", Time = "T0")

summer_photo_T28 <- read_csv(file.path(FT_dir, "2024_Summer_Photo_T28.csv")) %>% mutate(Season = "Summer", Method = "Photo", Time = "T28")

summer_biophoto_T0 <- read_csv(file.path(FT_dir, "2024_Summer_BioPhoto_T0.csv")) %>% mutate(Season = "Summer", Method = "BioPhoto", Time = "T0")

summer_biophoto_T28 <- read_csv(file.path(FT_dir, "2024_Summer_BioPhoto_T28.csv")) %>% mutate(Season = "Summer", Method = "BioPhoto", Time = "T28")


ft_all <- bind_rows(
  spring_bio_T0,
  spring_bio_T28,
  spring_photo_T0,
  spring_photo_T28,
  spring_biophoto_T0,
  spring_biophoto_T28,
  summer_bio_T0,
  summer_bio_T28,
  summer_photo_T0,
  summer_photo_T28,
  summer_biophoto_T0,
  summer_biophoto_T28
)

# Spring — Bio join
spring_bio_join <- full_join(spring_bio_T0  %>% select(-Time), spring_bio_T28 %>% select(-Time), by = "sumFormula", suffix = c("_T0", "_T28"))

# Spring — Photo join
spring_photo_join <- full_join(spring_photo_T0  %>% select(-Time), spring_photo_T28 %>% select(-Time), by = "sumFormula", suffix = c("_T0", "_T28"))

# Spring — BioPhoto join
spring_biophoto_join <- full_join(spring_biophoto_T0  %>% select(-Time), spring_biophoto_T28 %>% select(-Time), by = "sumFormula", suffix = c("_T0", "_T28"))

# Summer — Bio join
summer_bio_join <- full_join(summer_bio_T0  %>% select(-Time), summer_bio_T28 %>% select(-Time), by = "sumFormula", suffix = c("_T0", "_T28"))

# Summer — Photo join
summer_photo_join <- full_join(summer_photo_T0  %>% select(-Time), summer_photo_T28 %>% select(-Time), by = "sumFormula", suffix = c("_T0", "_T28"))

# Summer — BioPhoto join
summer_biophoto_join <- full_join(summer_biophoto_T0  %>% select(-Time), summer_biophoto_T28 %>% select(-Time), by = "sumFormula", suffix = c("_T0", "_T28"))

ft_paired_all <- bind_rows(
  spring_bio_join,
  spring_photo_join,
  spring_biophoto_join,
  summer_bio_join,
  summer_photo_join,
  summer_biophoto_join
)
nrow(ft_paired_all) # 35751, same as vk_pct_df

## Δ formula counts by class (T28 − T0), faceted + % change labels ----------
# helper: assign class from element counts (works even if N/S missing)
assign_class <- function(df, Ns = "N", Ss = "S") {
  Ncol <- if (Ns %in% names(df)) Ns else NA_character_
  Scol <- if (Ss %in% names(df)) Ss else NA_character_
  df %>%
    mutate(
      Ntmp = if (!is.na(Ncol)) suppressWarnings(as.numeric(.data[[Ncol]])) else 0,
      Stmp = if (!is.na(Scol)) suppressWarnings(as.numeric(.data[[Scol]])) else 0,
      Class = dplyr::case_when(
        Ntmp > 0 & Stmp > 0 ~ "CHONS",
        Stmp > 0            ~ "CHOS",
        Ntmp > 0            ~ "CHON",
        TRUE                ~ "CHO"
      )
    ) %>%
    select(-Ntmp, -Stmp)
}

# compute counts per class BEFORE and AFTER for each Season × Method

count_pair <- function(file_T0, file_T28, season, method) {
  d0  <- load_one(file_T0, "T0")  %>% assign_class()
  d28 <- load_one(file_T28, "T28") %>% assign_class()
  
  c0 <- d0  %>% distinct(Formula, Class) %>% count(Class, name = "T0_count")
  c28 <- d28 %>% distinct(Formula, Class) %>% count(Class, name = "T28_count")
  
  full_join(c0, c28, by = "Class") %>%
    mutate(
      T0_count  = replace_na(T0_count, 0L),
      T28_count = replace_na(T28_count, 0L),
      DeltaCount = T28_count - T0_count,
      # % change; treat T0=0 specially:
      #  - T0=0 & T28>0  -> Inf  (label as "new")
      #  - T0=0 & T28=0  -> 0%
      pct_change = case_when(
        T0_count == 0 & T28_count > 0  ~ Inf,
        T0_count == 0 & T28_count == 0 ~ 0,
        TRUE ~ 100 * (T28_count - T0_count) / T0_count
      ),
      pct_label = case_when(
        is.infinite(pct_change)        ~ "new",
        TRUE ~ paste0(if_else(pct_change > 0, "+", ""),
                      sprintf("%.0f%%", pct_change))
      ),
      Season = season,
      Method = method
    ) %>%
    arrange(Class)
}

counts_df <- purrr::pmap_dfr(
  list(pairs_all$file_T0, pairs_all$file_T28, pairs_all$Season, pairs_all$Method),
  ~ count_pair(..1, ..2, ..3, ..4)
)

# save table for record
readr::write_csv(counts_df, file.path(out_dir, "FormulaCount_Delta_by_Season_Method_Class2.csv"))

# bar plot: Δ formula count per class (T28 − T0), faceted Season × Method
p_bar <- ggplot(counts_df, aes(x = Class, y = DeltaCount, fill = Class)) +
  geom_col(width = 0.65, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(Season ~ Method) +
  labs(x = "", y = expression(Delta~"formula count (T28 − T0)")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6),
    axis.ticks = element_line(colour = "black"),
    axis.text  = element_text(colour = "black"),
    axis.title = element_text(colour = "black"),
    plot.margin = margin(2,2,2,2)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.00, 0.02)))


## Bar plot: Δ formula count per class with % change annotations ----------

# Add an "Overall" column per Season × Method
overall_df <- counts_df %>%
  dplyr::group_by(Season, Method) %>%
  dplyr::summarise(
    T0_count  = sum(T0_count,  na.rm = TRUE),
    T28_count = sum(T28_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Class = "Overall",
    DeltaCount = T28_count - T0_count,
    pct_change = dplyr::case_when(
      T0_count == 0 & T28_count > 0  ~ Inf,
      T0_count == 0 & T28_count == 0 ~ 0,
      TRUE ~ 100 * (T28_count - T0_count) / T0_count
    ),
    pct_label = dplyr::case_when(
      is.infinite(pct_change) ~ "new",
      TRUE ~ paste0(dplyr::if_else(pct_change > 0, "+", ""),
                    sprintf("%.0f%%", pct_change))
    )
  )

# Bind class-level rows with the overall rows
counts_all <- dplyr::bind_rows(counts_df, overall_df) %>%
  dplyr::mutate(
    # Put "Overall" at the end of the x-axis
    Class = factor(Class, levels = c("CHO","CHON","CHOS","CHONS","Overall")),
    # Label position just above the taller bar
    label_y = pmax(T0_count, T28_count)
  )

# Overlapping bars: T0 hollow (outline) + T28 filled, with % labels
p_bar2 <- ggplot(counts_all, aes(x = Class)) +
  # T0 (hollow)
  geom_col(aes(y = T0_count),
           width = 0.65, fill = NA, color = "black", linewidth = 0.3) +
  # T28 (filled, slightly narrower)
  geom_col(aes(y = T28_count, fill = Class),
           width = 0.5, color = "black", linewidth = 0.3) +
  # % change annotation above the taller of the two bars
  geom_text(aes(y = label_y, label = pct_label),
            vjust = -0.3, size = 3) +
  facet_grid(Season ~ Method) +
  labs(x = "", y = "Formula count (T0 hollow; T28 filled)") +
  theme_minimal(base_size = 12) +
  scale_fill_manual(values = c(CHO="#7fc97f", CHON="#beaed4", CHOS="#fdc086", CHONS="#386cb0", Overall="grey80")) +
  theme(
    text = element_text(family = "Helvetica Light", face = "plain"),
    legend.position = "none",
    panel.grid      = element_blank(),
    panel.border    = element_rect(colour = "black", fill = NA, linewidth = 0.4),
    axis.ticks      = element_line(colour = "black"),
    axis.text       = element_text(colour = "black"),
    axis.title      = element_text(colour = "black"),
    plot.margin     = margin(2,2,2,2)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.00, 0.12))) +
  coord_cartesian(clip = "off")

# Build VK overlay data: stack T0 & T28 per pair
vk_overlay_pair <- function(file_T0, file_T28, season, method) {
  d0  <- load_one(file_T0,  "T0")
  d28 <- load_one(file_T28, "T28")
  
  # Compute O/C and H/C for each table (use provided ratios if present; else from elements)
  prep <- function(df) {
    # detect optional ratio columns
    hc_col <- c("HCraito","H_C","H_C_ratio")
    oc_col <- c("OCraito","O_C","O_C_ratio")
    hc_hit <- hc_col[hc_col %in% names(df)][1]
    oc_hit <- oc_col[oc_col %in% names(df)][1]
    
    df %>%
      mutate(
        O_C = dplyr::coalesce(if (!is.na(oc_hit)) .data[[oc_hit]] else NA_real_,
                              if (all(c("O","C") %in% names(df))) suppressWarnings(as.numeric(O)/as.numeric(C)) else NA_real_),
        H_C = dplyr::coalesce(if (!is.na(hc_hit)) .data[[hc_hit]] else NA_real_,
                              if (all(c("H","C") %in% names(df))) suppressWarnings(as.numeric(H)/as.numeric(C)) else NA_real_)
      ) %>%
      transmute(Formula, O_C, H_C, Condition = Condition)
  }
  
  bind_rows(prep(d0), prep(d28)) %>%
    filter(is.finite(O_C), is.finite(H_C)) %>%
    mutate(Season = season, Method = method)
}

vk_overlay_df <- purrr::pmap_dfr(
  list(pairs_all$file_T0, pairs_all$file_T28, pairs_all$Season, pairs_all$Method),
  ~ vk_overlay_pair(..1, ..2, ..3, ..4)
)

# (Optional) DOM composition boxes to overlay on every facet
# dom_boxes <- tibble::tribble(
#   ~label,                ~xmin, ~xmax, ~ymin, ~ymax,
#   "Lipid-like",            0.00,  0.30,  1.50,  2.00,
#   "Protein-like",          0.30,  0.67,  1.50,  2.20,
#   "Carbohydrate-like",     0.67,  1.20,  1.50,  2.40,
#   "Lignin-like",           0.10,  0.67,  0.70,  1.50,
#   "Tannin-like",           0.67,  1.20,  0.50,  1.50,
#   "Unsat. hydrocarbons",   0.00,  0.10,  0.70,  1.50,
#   "Condensed aromatics",   0.00,  0.67,  0.20,  0.70
# )

# VK overlay plot: T0 vs T28 in different colors, faceted
p_vk_overlay <- ggplot(vk_overlay_df, aes(x = O_C, y = H_C)) +
  #geom_point(alpha = 0.65, size = 1.1) +
  geom_point(aes(fill = Condition), color = "black", shape = 21,
             size = 1.8, stroke = 0.25) +
  facet_grid(Season ~ Method) +
  scale_fill_manual(values = c(T0 = alpha("#1f77b4", 0.60), T28 = alpha("#d62728", 0.60)), name = NULL) +
  labs(x = "O/C", y = "H/C", color = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.spacing = unit(1, "lines"),
    text = element_text(family = "Helvetica Light", face = "plain"),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6),
    axis.ticks = element_line(colour = "black"),
    axis.text  = element_text(colour = "black"),
    axis.title = element_text(colour = "black"),
    legend.position = "bottom",
    plot.margin = margin(2,2,2,2)
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01)))


combined_vk_counts <- p_vk_overlay / p_bar2 +
  plot_layout(heights = c(3, 2), guides = "collect") +
  theme(
    legend.position   = "bottom",
    legend.margin     = margin(0,0,0,0),
    legend.box.margin = margin(0,0,0,0),
    plot.margin       = margin(2,2,2,2),
    panel.spacing     = unit(6, "pt")
  )

ggsave(file.path(out_dir, "VK_overlay_T0vsT28_shapes_plus_CountBars.png"),
       combined_vk_counts, width = 8, height = 9, dpi = 300)
ggsave(file.path(out_dir, "VK_overlay_T0vsT28_shapes_plus_CountBars.pdf"),
       combined_vk_counts, width = 8, height = 9, device = cairo_pdf)

print(combined_vk_counts)


## Van Krevelen (VK) — 3 columns × 4 rows ----------
# Columns: Biodegradation | Bio-photodegradation | Photodegradation
# Rows:    Spring–Before, Spring–After, Summer–Before, Summer–After
# Leftmost column: row annotations


# color palette
class_cols <- c(CHO="#7fc97f", CHON="#beaed4", CHOS="#fdc086", CHONS="#386cb0")

# prep + classify
vk_prep_table <- function(file) {
  df <- load_one(file, "tmp")
  hc_hit <- c("HCraito","H_C","H_C_ratio")[c("HCraito","H_C","H_C_ratio") %in% names(df)][1]
  oc_hit <- c("OCraito","O_C","O_C_ratio")[c("OCraito","O_C","O_C_ratio") %in% names(df)][1]
  df %>%
    mutate(
      O_C = dplyr::coalesce(if (!is.na(oc_hit)) .data[[oc_hit]] else NA_real_,
                            if (all(c("O","C") %in% names(df))) suppressWarnings(as.numeric(O)/as.numeric(C)) else NA_real_),
      H_C = dplyr::coalesce(if (!is.na(hc_hit)) .data[[hc_hit]] else NA_real_,
                            if (all(c("H","C") %in% names(df))) suppressWarnings(as.numeric(H)/as.numeric(C)) else NA_real_)
    ) %>%
    filter(is.finite(O_C), is.finite(H_C)) %>%
    select(Formula, O_C, H_C, C, H, O, N, S)
}

classify_tbl <- function(df) {
  df %>%
    mutate(
      Nnum = suppressWarnings(as.numeric(N)),
      Snum = suppressWarnings(as.numeric(S)),
      Class = case_when(
        coalesce(Nnum,0) > 0 & coalesce(Snum,0) > 0 ~ "CHONS",
        coalesce(Snum,0) > 0                         ~ "CHOS",
        coalesce(Nnum,0) > 0                         ~ "CHON",
        TRUE                                         ~ "CHO"
      )
    ) %>% select(-Nnum,-Snum)
}

method_full <- function(m) dplyr::recode(m,
                                         Bio = "Biodegradation",
                                         Photo = "Photodegradation",
                                         BioPhoto = "Bio-photodegradation"
)

# fetch one table
get_tbl_for <- function(season, method, when = c("T0","T28")) {
  when <- match.arg(when)
  row <- pairs_all %>% filter(Season == season, Method == method)
  stopifnot(nrow(row) == 1)
  file <- if (when == "T0") row$file_T0 else row$file_T28
  vk_prep_table(file) %>% classify_tbl()
}

# VK scatter
vk_scatter <- function(tbl, show_col_header = NULL) {
  ggplot(tbl, aes(x=O_C, y=H_C)) +
    geom_point(aes(fill = Class), shape = 21, size = 1.8, stroke = 0.25, color = "black") +
    scale_fill_manual(values = alpha(class_cols, 0.6), drop = FALSE, name = "Class") +
    labs(x="O/C", y="H/C", title = show_col_header) +
    theme_minimal(base_size=11) +
    theme(
      text = element_text(family = "Helvetica Light", face = "plain"),
      panel.grid   = element_blank(),
      panel.border = element_rect(color="black", fill=NA, linewidth=0.5),
      axis.ticks   = element_line(color="black"),
      axis.text    = element_text(color="black"),
      axis.title   = element_text(color="black"),
      plot.title   = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom",
      plot.margin  = margin(2,2,2,2)
    ) +
    scale_x_continuous(expand = expansion(mult=c(0.01,0.01))) +
    scale_y_continuous(expand = expansion(mult=c(0.01,0.01)))
}

# row labels (far-left column)
row_label_plot <- function(label) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = label, fontface = "bold", size = 4.2, angle = 90) +
    coord_cartesian(xlim=c(0,1), ylim=c(0,1), clip="off") +
    theme_void()
}

# build VK grid
methods_order <- c("Bio","BioPhoto","Photo")
col_headers   <- method_full(methods_order)
row_defs <- tribble(
  ~Season,  ~When, ~RowLabel,
  "Spring", "T0",  "Spring — Before",
  "Spring", "T28", "Spring — After",
  "Summer", "T0",  "Summer — Before",
  "Summer", "T28", "Summer — After"
)

plots <- list()
for (r in seq_len(nrow(row_defs))) {
  row_label <- row_label_plot(row_defs$RowLabel[r])
  row_plots <- list(row_label)
  
  for (c in seq_along(methods_order)) {
    se  <- row_defs$Season[r]
    wh  <- row_defs$When[r]
    me  <- methods_order[c]
    tbl <- get_tbl_for(se, me, wh)
    show_col <- if (r == 1) col_headers[c] else NULL
    row_plots[[length(row_plots)+1]] <- vk_scatter(tbl, show_col_header = show_col)
  }
  
  # combine row: label | 3 VK panels
  plots[[r]] <- wrap_plots(row_plots, ncol = 4, widths = c(0.12, 1, 1, 1))
}

# combine all rows vertically
plots2 <- lapply(plots, function(p) p + theme(legend.position = "none"))

vk_grid <- wrap_plots(plots2, ncol = 1, guides = "collect") +
  plot_annotation(
    theme = theme(
      legend.position   = "bottom",
      legend.box        = "horizontal",
      legend.direction  = "horizontal",
      legend.margin     = margin(0,0,0,0),
      legend.box.margin = margin(0,0,0,0),
      panel.spacing     = unit(6, "pt")
    )
  )

# save
ggsave(file.path(out_dir, "VK_3col_4row_with_left_labels2.png"),
       vk_grid, width = 8, height = 10, dpi = 600)
ggsave(file.path(out_dir, "VK_3col_4row_with_left_labels2.pdf"),
       vk_grid, width = 8, height = 10, device = cairo_pdf)

##  Combined Plot 2: bars % =======================

count_pair_pct <- function(file_T0, file_T28, season, method) {
  d0  <- load_one(file_T0, "T0")  %>% classify_tbl()
  d28 <- load_one(file_T28, "T28") %>% classify_tbl()
  
  c0  <- d0  %>% distinct(Formula, Class) %>% count(Class, name="T0_count")
  c28 <- d28 %>% distinct(Formula, Class) %>% count(Class, name="T28_count")
  
  full_join(c0, c28, by="Class") %>%
    mutate(
      T0_count  = replace_na(T0_count, 0L),
      T28_count = replace_na(T28_count, 0L),
      T0_pct  = if (sum(T0_count)>0) 100*T0_count/sum(T0_count) else 0,
      T28_pct = if (sum(T28_count)>0) 100*T28_count/sum(T28_count) else 0,
      Season = season, Method = method
    )
}

counts_pct <- purrr::pmap_dfr(
  list(pairs_all$file_T0, pairs_all$file_T28, pairs_all$Season, pairs_all$Method),
  ~ count_pair_pct(..1, ..2, ..3, ..4)
)

# Only classes (remove "Overall")
counts_pct_all <- counts_pct %>%
  mutate(Class = factor(Class, levels=c("CHO","CHON","CHOS","CHONS")),
         label_y = pmax(T0_pct, T28_pct))

p_bar_pct <- ggplot(counts_pct_all, aes(x = Class)) +
  geom_col(aes(y = T0_pct), width = 0.65, fill = NA, color = "black", linewidth = 0.3) +
  geom_col(aes(y = T28_pct, fill = Class), width = 0.5, color = "black", linewidth = 0.3) +
  facet_grid(Season ~ Method) +
  scale_fill_manual(values = class_cols, drop = FALSE) +
  labs(x = "", y = "Relative abundance (%)") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    axis.ticks = element_line(colour = "black"),
    axis.text  = element_text(colour = "black"),
    axis.title = element_text(colour = "black"),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(3,3,3,3)
  ) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0,60,20),
                     expand = expansion(mult=c(0,0.05)))

ggsave(file.path(out_dir, "ClassRelativeAbundance_T0hollow_T28filled.png"),
       p_bar_pct, width = 9, height = 6, dpi = 300)
ggsave(file.path(out_dir, "ClassRelativeAbundance_T0hollow_T28filled.pdf"),
       p_bar_pct, width = 9, height = 6, device = cairo_pdf)

## Van Krevelen colored by 28-day % change (Bio, Photo, Bio-Photo; 6 panels) ======== 

# plot: 2×3 facets (rows = Season, cols = Method)
shape_vals <- c(CHO = 21, CHON = 22, CHOS = 24, CHONS = 23)

vk_neg <- vk_pct_df %>% filter(pct_change < 0)
vk_pos <- vk_pct_df %>% filter(pct_change >= 0)

p_vk_change <- ggplot() +
  geom_point(data = vk_pos,
             aes(x = O_C, y = H_C, fill = pct_change, shape = Class),
             size = 1.6, stroke = 0.25, color = "black", alpha = 0.7) +
  geom_point(data = vk_neg,
             aes(x = O_C, y = H_C, fill = pct_change, shape = Class),
             size = 1.6, stroke = 0.25, color = "black", alpha = 0.7) +
  scale_shape_manual(values = shape_vals, name = NULL) + 
  scale_fill_gradient2(
    low = "#2c7fb8", mid = "#bdbdbd", high = "#d7301f",
    midpoint = 0, limits = c(-1, 1),
    name = expression(Delta~"Change"),
    labels = scales::label_percent(accuracy = 1)
  ) +
  facet_grid(Season ~ Method) +
  labs(x = "O/C", y = "H/C") +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Helvetica Light", face = "plain"),
    panel.grid   = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.ticks   = element_line(color = "black"),
    axis.text    = element_text(color = "black"),
    axis.title   = element_text(color = "black"),
    strip.text   = element_text(face = "bold"),
    # two legends (fill gradient + shape); keep them stacked inside plot
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.key.width  = unit(2, "lines"),
    legend.title = element_text(vjust = 0.8, face = "bold"),
    panel.spacing = unit(1.5, "lines"),
    plot.margin  = margin(6, 6, 6, 6)
  ) +
  scale_x_continuous(limits = c(0, 1.5), expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(limits = c(0, 2.5), expand = expansion(mult = c(0.01, 0.01)))

vk_four_groups <- function(p_vk_change, col = "grey20") {
  p_vk_change +
    geom_hline(yintercept = 2.0, linetype = "dashed",
               linewidth = 0.35, color = col, inherit.aes = FALSE) +
    # Aliphatics: horizontal boundary around H/C ~ 1.5
    geom_hline(yintercept = 1.50, linetype = "dashed",
               linewidth = 0.35, color = col, inherit.aes = FALSE) +
    # HUP & PPh: gently declining lines (adjust intercepts if needed)
    geom_abline(intercept = 1.1, slope = -0.2, linetype = "dashed",
                linewidth = 0.35, color = col, inherit.aes = FALSE) +  # HUP
    geom_abline(intercept = 0.75, slope = -0.20, linetype = "dashed",
                linewidth = 0.35, color = col, inherit.aes = FALSE)    # PPh
}

p_vk_change <- vk_four_groups(p_vk_change)


# save
ggsave(file.path(out_dir, "VK_pct_change_all_methods_shapesv3.png"),
       p_vk_change, width = 7.15, height = 5, dpi = 600)
ggsave(file.path(out_dir, "VK_pct_change_all_methods_shapesv3.pdf"),
       p_vk_change, width = 7.15, height = 5, device = cairo_pdf)

dom_cols <- c(
  Aliphatics = "#A6C36F",
  HUPs       = "#4C9F70",
  PPh        = "#D99E6A",
  CA         = "#7C70A4",
  Other      = "#B4B8C5"
)

vk_dom <- vk_pct_df %>%
  mutate(
    AImod = coalesce(AImod0, AImod28),
    dom_group = case_when(
      # 1) Condensed aromatics (CA; combustion-derived); 
      AImod > 0.66 ~ "CA",
      # 2) Polyphenolic-like (PPh; vascular plant–derived);
      AImod > 0.50 & AImod <= 0.66 ~ "PPh",
      # 3) Highly unsaturated & phenolic (HUPs);
      AImod <= 0.50 & H_C < 1.5 ~ "HUPs",
      # 4) Aliphatic-like;
      H_C >= 1.5 & H_C <= 2.0 ~ "Aliphatics",
      # everything else (sugar-like, peptide-like, weird edge cases…)
      TRUE ~ "Other"
    ),
    dom_group = factor(dom_group, levels = c("Aliphatics", "HUPs", "PPh", "CA", "Other"))
  )

vk_neg2 <- vk_dom %>% filter(pct_change < 0)
vk_pos2 <- vk_dom %>% filter(pct_change >= 0)



vk_pos2 <- vk_pos2 %>%
  mutate(alpha_val = scales::rescale(pct_change, to = c(0.2, 1), from = c(-1, 1)))

vk_neg2 <- vk_neg2 %>%
  mutate(alpha_val = scales::rescale(pct_change, to = c(0.2, 1), from = c(-1, 1)))

p_vk_change2 <- ggplot() +
  geom_point(
    data = vk_pos2,
    aes(
      x = O_C, y = H_C,
      color = dom_group,
      alpha = (pct_change + 1) / 2   # rescale pct_change → [0,1]
    ),
    size = 1.5,
    stroke = 0.25, shape = 20
  ) +
  geom_point(
    data = vk_neg2,
    aes(
      x = O_C, y = H_C,
      color = dom_group,
      alpha = (pct_change + 1) / 2
    ),
    size = 1.5,
    stroke = 0.25, shape = 20
  ) +
  scale_color_manual(values = dom_cols, name = NULL) +
  scale_alpha_continuous(
    name = expression(Delta~"Change"),
    limits = c(0, 1),
    range  = c(0.1, 1),   # adjust visibility, avoid fully invisible points
    labels = function(x) scales::percent(2 * x - 1, accuracy = 1)
  ) +
  facet_grid(Season ~ Method) +
  labs(x = "O/C", y = "H/C") +
  theme_minimal(base_size = 12) +
  guides(
    alpha = guide_legend(
      override.aes = list(size = 4), 
      title.position = "left",
      title.hjust = 0.5,
      byrow = TRUE 
    ),
    color = guide_legend(
      override.aes = list(size = 4),
      title.position = "left",
      title.hjust = 0.5,
      byrow = TRUE 
    )
  ) +
  theme(
    text = element_text(family = "Helvetica Light"),
    panel.grid   = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.ticks   = element_line(color = "black"),
    axis.text    = element_text(color = "black"),
    axis.title   = element_text(color = "black"),
    strip.text   = element_text(face = "bold"),
    panel.spacing = unit(1, "lines"),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0), 
    legend.box.margin = margin(t = -10, r = 0, b = 0, l = 0, unit = "pt")
  )

ggsave(file.path(out_dir, "VK_pct_change_all_methods_colors2.png"),
       p_vk_change2, width = 7.5, height = 5, dpi = 600)
ggsave(file.path(out_dir, "VK_pct_change_all_methods_colors2.pdf"),
       p_vk_change2, width = 7.5, height = 5, device = cairo_pdf)



p_vk_change3 <- ggplot(vk_dom, aes(O_C, H_C, z = pct_change)) +
  stat_summary_hex(
    fun = mean,
    bins = 40
  ) +
  facet_grid(Season ~ Method) +
  scale_fill_gradient2(
    low = "#2166ac",
    mid = "white",
    high = "#b2182b",
    midpoint = 0,
    name = expression(~Delta~"Change"),
    labels = function(x) scales::percent(x)
  ) +
  labs(x = "O/C", y = "H/C") +
  theme_minimal(base_size = 12) +
  guides(
    alpha = guide_legend(
      override.aes = list(size = 4), 
      title.position = "left",
      title.hjust = 0.5,
      byrow = TRUE 
    ),
    color = guide_legend(
      override.aes = list(size = 4),
      title.position = "left",
      title.hjust = 0.5,
      byrow = TRUE 
    )
  ) +
  theme(
    text = element_text(family = "Helvetica Light", face = "plain"),
    panel.grid   = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.ticks   = element_line(color = "black"),
    axis.text    = element_text(color = "black"),
    axis.title   = element_text(color = "black"),
    strip.text   = element_text(face = "bold"),
    # two legends (fill gradient + shape); keep them stacked inside plot
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.key.width  = unit(2, "lines"),
    legend.title = element_text(vjust = 0.8, face = "bold"),
    panel.spacing = unit(1.5, "lines"),
    plot.margin  = margin(6, 6, 6, 6)
  ) +
  scale_x_continuous(limits = c(0, 1.5), expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(limits = c(0, 2.5), expand = expansion(mult = c(0.01, 0.01)))

vk_four_groups3 <- function(p, col = "grey20") {
  p +
    geom_hline(yintercept = 2.0, linetype = "dashed",
               linewidth = 0.35, color = col, inherit.aes = FALSE) +
    geom_hline(yintercept = 1.50, linetype = "dashed",
               linewidth = 0.35, color = col, inherit.aes = FALSE) +
    geom_abline(intercept = 1.1, slope = -0.2, linetype = "dashed",
                linewidth = 0.35, color = col, inherit.aes = FALSE) +
    geom_abline(intercept = 0.75, slope = -0.20, linetype = "dashed",
                linewidth = 0.35, color = col, inherit.aes = FALSE)
}

p_vk_change3 <- vk_four_groups3(p_vk_change3)

ggsave(file.path(out_dir, "VK_pct_change_all_methods_colors2-3.png"),
       p_vk_change3, width = 7, height = 5.5, dpi = 600)
ggsave(file.path(out_dir, "VK_pct_change_all_methods_colors2-3.pdf"),
       p_vk_change3, width = 7, height = 5.5, device = cairo_pdf)

## AIMOD histograms: Before (T0) vs After (T28) in a 2×3 grid--------
# Rows = Season (Spring, Summer); Cols = Biodeg, Photodeg, Bio-photodeg

# build a long table with both T0 & T28 (no joins needed)
aimod_long <- purrr::pmap_dfr(pairs_all, function(Season, Method, file_T0, file_T28) {
  bind_rows(
    load_one(file_T0,  "Before"),
    load_one(file_T28, "After")
  ) %>%
    mutate(Season = Season, Method = Method)
})
# build a long table with both T0 & T28 (no joins needed)
aimod_long <- purrr::pmap_dfr(pairs_all, function(Season, Method, file_T0, file_T28) {
  bind_rows(
    load_one(file_T0,  "Before"),
    load_one(file_T28, "After")
  ) %>%
    mutate(Season = Season, Method = Method)
})

aimod_long <- aimod_long %>%
  mutate(
    Season = factor(Season, levels = c("Spring","Summer")),
    Method = factor(Method, levels = c("Bio","Photo","BioPhoto"),
                    labels = c("Biodegradation","Photodegradation","Bio-photodegradation")),
    Condition = factor(Condition, levels = c("Before","After"))
  )

# plot: overlaid histograms + density curves per Season×Method 
p_histdens <- ggplot(aimod_long, aes(x = aimod)) +
  # histogram pillars, scaled as density
  geom_histogram(
    aes(y = after_stat(density), color = Condition, fill="grey95"),
    binwidth = 0.02,
    #color    = "black",
    alpha    = 0.6,
    position = "identity",
    linewidth = 0.2
  ) +
  # smooth density curves
  geom_density(
    aes(color = Condition, fill = Condition),
    alpha = 0.4,
    linewidth = 0.8,
    adjust = 1
  ) +
  facet_grid(Season ~ Method) +
  scale_fill_manual(values = c(Before = "#1f77b4", After = "#d62728")) +
  scale_color_manual(values = c(Before = "#1f77b4", After = "#d62728")) +
  labs(
    x    = expression("AI"[mod]),
    y    = "Density",
    fill = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Helvetica Light", face = "plain"),
    panel.grid   = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.ticks   = element_line(color = "black"),
    axis.text    = element_text(color = "black"),
    axis.title   = element_text(color = "black"),
    strip.text   = element_text(face = "bold"),
    legend.position = "bottom",
    panel.spacing = unit(1.0, "lines")
  ) +
  scale_x_continuous(limits = c(0, 1.2), expand = expansion(mult = c(0, 0.01)))

ggsave(file.path(out_dir, "AIMOD_hist_density_before_after_2x3.png"),
       p_histdens, width = 10, height = 7, dpi = 600)
ggsave(file.path(out_dir, "AIMOD_hist_density_before_after_2x3.pdf"),
       p_histdens, width = 10, height = 7, device = cairo_pdf)


## Recalcitrant formulae 3×2 VK plot (cols = methods, rows = seasons) ----------

# build vk_pct_df if missing (keeps T0-only & T28-only) 
if (!exists("vk_pct_df")) {
  vk_pct_df <- purrr::pmap_dfr(pairs_all, function(Season, Method, file_T0, file_T28) {
    d0  <- load_one(file_T0,  "T0")  %>% vk_from_elements_or_ratios() %>%
      transmute(Formula, O_C0 = O_C, H_C0 = H_C, RelInt_T0  = RelInt)
    d28 <- load_one(file_T28, "T28") %>% vk_from_elements_or_ratios() %>%
      transmute(Formula, O_C28 = O_C, H_C28 = H_C, RelInt_T28 = RelInt)
    
    # collapse duplicates per Formula (if any)
    d0  <- d0  %>% group_by(Formula) %>% summarise(O_C0 = mean(O_C0, na.rm=TRUE),
                                                   H_C0 = mean(H_C0, na.rm=TRUE),
                                                   RelInt_T0 = sum(RelInt_T0, na.rm=TRUE), .groups="drop")
    d28 <- d28 %>% group_by(Formula) %>% summarise(O_C28 = mean(O_C28, na.rm=TRUE),
                                                   H_C28 = mean(H_C28, na.rm=TRUE),
                                                   RelInt_T28 = sum(RelInt_T28, na.rm=TRUE), .groups="drop")
    
    full_join(d0, d28, by = "Formula") %>%
      mutate(
        O_C = coalesce(O_C0, O_C28),
        H_C = coalesce(H_C0, H_C28),
        RelInt_T0  = replace_na(RelInt_T0,  0),
        RelInt_T28 = replace_na(RelInt_T28, 0),
        present0   = RelInt_T0  > 0,
        present28  = RelInt_T28 > 0,
        pct_change = case_when(
          RelInt_T0 == 0 & RelInt_T28 > 0  ~  1,
          RelInt_T0 == 0 & RelInt_T28 == 0 ~  0,
          TRUE ~ (RelInt_T28 - RelInt_T0) / RelInt_T0
        ),
        pct_change = scales::squish(pct_change, range = c(-1, 1)),
        Season = Season,
        Method = factor(Method, levels = c("Bio","Photo","BioPhoto"),
                        labels = c("Biodegradation","Photodegradation","Bio-photodegradation"))
      ) %>%
      select(Formula, O_C, H_C, RelInt_T0, RelInt_T28, present0, present28,
             pct_change, Season, Method)
  }) %>%
    mutate(Season = factor(Season, levels = c("Spring","Summer")))
}

# choose tolerance for "no change"
tau <- 0.05   # e.g., |Δ| ≤ 2% regarded as resistant; adjust as needed

resistant_df <- vk_pct_df %>%
  filter(present0, present28, is.finite(pct_change), abs(pct_change) <= tau)

# (optional) counts per panel to annotate
counts <- resistant_df %>%
  count(Season, Method, name = "n")

# 3×2 VK plot (cols = methods, rows = seasons)
p_resist <- ggplot(resistant_df, aes(O_C, H_C)) +
  geom_point(shape = 21, size = 1.7, stroke = 0.25,
             fill = alpha("#f5c147", 0.6), color = "black") +
  facet_grid(Season ~ Method) +
  labs(
    x = "O/C", y = "H/C",
    title = paste0("Resistant compound (|Δ intensity| ≤ ", percent(tau), ")")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid   = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.ticks   = element_line(color = "black"),
    axis.text    = element_text(color = "black"),
    axis.title   = element_text(color = "black"),
    strip.text   = element_text(face = "bold"),
    plot.title   = element_text(face = "bold"),
    panel.spacing = unit(1.2, "lines"),
    plot.margin  = margin(6,6,6,6)
  ) +
  scale_x_continuous(limits = c(0, 1.2), expand = expansion(mult = c(0.01,0.01))) +
  scale_y_continuous(limits = c(0, 2.5), expand = expansion(mult = c(0.01,0.01)))

# add simple n labels (top-left of each panel)
p_resist <- p_resist +
  geom_text(
    data = counts,
    aes(x = 0.06, y = 2.4, label = paste0("n = ", n)),
    inherit.aes = FALSE, size = 3.3, hjust = 0
  )

ggsave(file.path(out_dir, "VK_resistant_3x2-1.png"),
       p_resist, width = 8.1, height = 5.4, dpi = 600)
ggsave(file.path(out_dir, "VK_resistant_3x2-1.pdf"),
       p_resist, width = 8.1, height = 5.4, device = cairo_pdf)

## Spring and summer DOC degradation differences----------
vk_season <- vk_pct_df %>%
  select(Method, Formula, Season, O_C, H_C, pct_change) %>%
  group_by(Method, Formula, Season) %>%
  summarise(
    pct = mean(pct_change, na.rm = TRUE),
    O_C = mean(O_C, na.rm = TRUE),
    H_C = mean(H_C, na.rm = TRUE),
    .groups = "drop"
  )

# 2) Pivot wider to Summer / Spring columns, then subtract
vk_diff <- vk_season %>%
  pivot_wider(names_from = Season, values_from = pct) %>%
  mutate(diff = Summer - Spring)   # <-- Summer minus Spring

write_csv(vk_diff, file.path(out_dir, "VK_diff2.csv"))

vk_diff_clean <- vk_diff %>% filter(!is.na(diff))

p_diff <- ggplot(vk_diff_clean, aes(x = O_C, y = H_C, fill = diff)) +
  geom_point(shape = 21, size = 1.6, stroke = 0.25, color = "grey20", alpha = 0.8)  +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, limits = c(-2, 2),
    oob = scales::squish,             # keeps within range
    name = expression("Degradability differences (Summer – Spring)"),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  facet_wrap(~ Method, ncol = 3) +
  labs(x = "O/C", y = "H/C") +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Helvetica Light"),
    panel.spacing = unit(1.5, "lines"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.ticks = element_line(color = "black"),
    strip.text = element_text(face = "bold"),
    plot.margin = margin(t = 0.1, r = 0.5, b = 0.1, l = 0.5, unit = "cm"),
    legend.title = element_text(vjust = 0.8, face = "bold"),
    legend.key.height = unit(0.5, "cm"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  )  + scale_x_continuous(limits = c(0, 1.2), expand = expansion(mult = 0.01)) +
  scale_y_continuous(limits = c(0, 2.5), expand = expansion(mult = 0.01))

vk_4groups_diff <- function(p_diff, col = "grey20") {
  p_diff +
    geom_hline(yintercept = 2.0, linetype = "dashed",
               linewidth = 0.35, color = col, inherit.aes = FALSE) +
    # Aliphatics: horizontal boundary around H/C ~ 1.5
    geom_hline(yintercept = 1.50, linetype = "dashed",
               linewidth = 0.35, color = col, inherit.aes = FALSE) +
    # HUP & PPh: gently declining lines (adjust intercepts if needed)
    geom_abline(intercept = 1.1, slope = -0.2, linetype = "dashed",
                linewidth = 0.35, color = col, inherit.aes = FALSE) +  # HUP
    geom_abline(intercept = 0.75, slope = -0.20, linetype = "dashed",
                linewidth = 0.35, color = col, inherit.aes = FALSE)    # PPh
}

p_diff2 <- vk_4groups_diff(p_diff)

# Save figure
ggsave(file.path(out_dir, "VK_diff_Summer_vs_Spring2.png"),
       p_diff, width = 9, height = 4, dpi = 600)
ggsave(file.path(out_dir, "VK_diff_Summer_vs_Spring2.pdf"),
       p_diff, width = 9, height = 4, device = cairo_pdf)

## Density plots of % change by Season and Method--------
# Assuming vk_pct_df is already built (with Season, Method, O_C, H_C, pct_change)
# Colors (feel free to swap)
season_cols <- c(Spring = "#6BA3C9", Summer = "#D8C27A")

p_den <- ggplot(vk_pct_df %>% mutate(Method = factor(Method, levels = c("Biodegradation", "Photodegradation", "Bio-photodegradation"))), aes(x = pct_change, fill = Season)) +
  geom_density(alpha = 0.35, color = "black", linewidth = 0.3, adjust = 1) +
  facet_wrap(~ Method, nrow = 1) +
  scale_fill_manual(values = season_cols, drop = FALSE) + 
  #scale_y_continuous(trans = "log10") +
  scale_x_continuous(limits = c(-1, 1),
                     breaks = seq(-1, 1, by = 0.5),
                     labels = label_percent(accuracy = 1)) +
  labs(
    title = NULL,
    x = expression("Relative intensity change ("*Delta*"% )"),
    y = "Density",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    # strips: keep text, remove borders/background
    text = element_text(family = "Helvetica Light"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    # 4-sided panel border
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black"),
    axis.text  = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    # legend inside upper-middle of the middle panel (approx.)
    panel.spacing = unit(1.2, "lines"),
    legend.position = c(0.52, 0.9),   # centered horizontally; near top
    legend.justification = c(0.5, 1),
    legend.direction = "horizontal",
    legend.title = element_text(face = "bold"),
    legend.key.width = unit(0.5, "cm"),
    plot.margin = margin(8, 12, 8, 12)
  ) +
  guides(fill = guide_legend(title.position = "top", nrow = 1)) 

ggsave(file.path(out_dir, "Density_Season_Method2.png"),
       p_den, width = 9, height = 3.6, dpi = 600)
ggsave(file.path(out_dir, "Density_Season_Method2.pdf"),
       p_den, width = 9, height = 3.6, device = cairo_pdf)

## Statistical comparisons between molecular classes--------


#  Ensure Class exists 
if (!"Class" %in% names(vk_pct_df)) {
  vk_pct_df <- vk_pct_df %>%
    mutate(
      Nnum = suppressWarnings(as.numeric(N)),
      Snum = suppressWarnings(as.numeric(S)),
      Class = case_when(
        coalesce(Nnum, 0) > 0 & coalesce(Snum, 0) > 0 ~ "CHONS",
        coalesce(Snum, 0) > 0                         ~ "CHOS",
        coalesce(Nnum, 0) > 0                         ~ "CHON",
        TRUE                                          ~ "CHO"
      )
    ) %>%
    select(-Nnum, -Snum)
}

vk_pct_df <- vk_pct_df %>%
  mutate(
    Season = factor(Season, levels = c("Spring","Summer")),
    Method = factor(Method, levels = c("Biodegradation","Photodegradation","Bio-photodegradation"))
  )


# Means for the columns

class_summary <- vk_pct_df %>%
  group_by(Season, Method, Class) %>%
  summarise(mean_change = mean(pct_change, na.rm = TRUE),
            n = dplyr::n(), .groups = "drop")

overall_summary <- vk_pct_df %>%
  group_by(Season, Method) %>%
  summarise(mean_change = mean(pct_change, na.rm = TRUE),
            n = dplyr::n(), .groups = "drop") %>%
  mutate(Class = "All")

class_plot_df <- bind_rows(class_summary, overall_summary) %>%
  mutate(Class = factor(Class, levels = c("CHO","CHON","CHOS","CHONS","All")))


# Fisher + permutation tests per Method×Class
# Create LossFlag for Fisher (complete loss)
vk_test_df <- vk_pct_df %>%
  mutate(
    LossFlag = pct_change <= -0.999,
    Class = factor(Class, levels = c("CHO","CHON","CHOS","CHONS"))
  )

# Fisher's exact test on loss fraction
fisher_core <- vk_test_df %>%
  filter(Class %in% c("CHO","CHON","CHOS","CHONS")) %>%
  group_by(Method, Class) %>%
  summarise(
    loss_frac_spring = mean(LossFlag[Season == "Spring"], na.rm = TRUE),
    loss_frac_summer = mean(LossFlag[Season == "Summer"], na.rm = TRUE),
    p_Fisher = tryCatch(
      {
        tab <- table(Season, LossFlag)
        fisher.test(tab)$p.value
      },
      error = function(e) NA_real_
    ),
    .groups = "drop"
  )

fisher_all <- vk_test_df %>%
  mutate(Class = "All") %>%
  group_by(Method, Class) %>%
  summarise(
    loss_frac_spring = mean(LossFlag[Season == "Spring"], na.rm = TRUE),
    loss_frac_summer = mean(LossFlag[Season == "Summer"], na.rm = TRUE),
    p_Fisher = tryCatch(
      {
        tab <- table(Season, LossFlag)
        fisher.test(tab)$p.value
      },
      error = function(e) NA_real_
    ),
    .groups = "drop"
  )

fisher_res <- bind_rows(fisher_core, fisher_all) %>%
  mutate(
    Class = factor(Class, levels = c("CHO","CHON","CHOS","CHONS","All"))
  )

# Permutation test on partial losses (pct_change > -1)
perm_test <- function(x, g, n_perm = 5000) {
  observed <- mean(x[g=="Summer"], na.rm = TRUE) - mean(x[g=="Spring"], na.rm = TRUE)
  sims <- replicate(n_perm, {
    shuffled <- sample(g)
    mean(x[shuffled=="Summer"], na.rm = TRUE) - mean(x[shuffled=="Spring"], na.rm = TRUE)
  })
  (sum(abs(sims) >= abs(observed)) + 1) / (n_perm + 1)
}

# helper for one subgroup
perm_by_group <- function(subdf) {
  d <- subdf %>% filter(pct_change > -0.999)  # partial-change only
  
  # need both seasons with at least 2 obs
  if (sum(d$Season == "Spring") < 2 || sum(d$Season == "Summer") < 2) {
    return(tibble(
      mean_spring = NA_real_,
      mean_summer = NA_real_,
      p_Perm = NA_real_
    ))
  }
  
  p <- tryCatch(
    perm_test(d$pct_change, d$Season, n_perm = 5000),
    error = function(e) NA_real_
  )
  
  tibble(
    mean_spring = mean(d$pct_change[d$Season=="Spring"], na.rm = TRUE),
    mean_summer = mean(d$pct_change[d$Season=="Summer"], na.rm = TRUE),
    p_Perm = p
  )
}

perm_core <- vk_pct_df %>%
  filter(Class %in% c("CHO","CHON","CHOS","CHONS")) %>%
  group_by(Method, Class) %>%
  group_modify(~ perm_by_group(.x)) %>%
  ungroup()

perm_all <- vk_pct_df %>%
  mutate(Class = "All") %>%
  group_by(Method, Class) %>%
  group_modify(~ perm_by_group(.x)) %>%
  ungroup()

perm_res <- bind_rows(perm_core, perm_all) %>%
  mutate(
    Class = factor(Class, levels = c("CHO","CHON","CHOS","CHONS","All"))
  )

# Combine Fisher + Permutation into one stats table
stats_all <- full_join(fisher_res, perm_res,
                       by = c("Method","Class")) %>%
  mutate(
    # stars for each test
    label_F = case_when(
      is.na(p_Fisher)      ~ "F: ns",
      p_Fisher < 0.001     ~ "F: ***",
      p_Fisher < 0.01      ~ "F: **",
      p_Fisher < 0.05      ~ "F: *",
      TRUE                 ~ "F: ns"
    ),
    label_P = case_when(
      is.na(p_Perm)        ~ "P: ns",
      p_Perm < 0.001       ~ "P: ***",
      p_Perm < 0.01        ~ "P: **",
      p_Perm < 0.05        ~ "P: *",
      TRUE                 ~ "P: ns"
    ),
    # combined two-line label
    label = paste(label_F, label_P, sep = "\n"),
    Class = factor(Class, levels = c("CHO","CHON","CHOS","CHONS","All"))
  )


yrange <- class_plot_df %>%
  group_by(Method) %>%
  summarise(
    ymin = min(mean_change, na.rm = TRUE),
    ymax = max(mean_change, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(y_pos = ymax + 0.18 * (ymax - ymin + 1e-6))  # a bit of headroom

stats_all <- stats_all %>%
  left_join(yrange, by = "Method") %>%
  mutate(
    x_id   = as.numeric(Class),
    group1 = x_id - 0.25,  # Spring bar
    group2 = x_id + 0.25   # Summer bar
  )

# Slight Method-specific tweaks (optional)
stats_all <- stats_all %>%
  mutate(
    y_pos = case_when(
      Method == "Biodegradation"       ~ y_pos - 0.05,
      Method == "Photodegradation"     ~ y_pos + 0.00,
      Method == "Bio-photodegradation" ~ y_pos + 0.05,
      TRUE ~ y_pos
    )
  )


season_cols <- c(Spring = "#6BA3C9", Summer = "#D8C27A")


p_class <- ggplot(class_plot_df, aes(x = Class, y = mean_change, fill = Season)) +
  geom_col(alpha = 0.5, position = position_dodge(width = 0.7), color = "black", width = 0.6, linewidth = 0.2) +
  facet_wrap(~ Method, ncol = 3) +
  scale_fill_manual(values = season_cols) +
  labs(y = "Mean relative change (%)", x = NULL, fill = NULL) +
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "Helvetica Light"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.text = element_text(color = "black"),
    legend.position = "top"
    #legend.position = "none"
    
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) +
  coord_cartesian(clip = "off")

# significance bars (both tests encoded in the label)
p_class <- p_class +
  ggpubr::stat_pvalue_manual(
    data       = stats_all,
    label      = "label",
    xmin       = "group1",
    xmax       = "group2",
    y.position = "y_pos",
    tip.length = 0.01,
    size       = 3.5,
    bracket.size = 0.3,
    inherit.aes = FALSE
  )

p_class


ggsave(file.path(out_dir, "Class_mean_change_with_Fisher_and_Perm2.png"),
       p_class, width = 10, height = 4.8, dpi = 600)
ggsave(file.path(out_dir, "Class_mean_change_with_Fisher_and_Perm2.pdf"),
       p_class, width = 10, height = 4.8, device = cairo_pdf)


## Statistical comparisons between DOM groups--------
vk_pct_df <- read_csv(file.path(out_dir, "vk_pct_df4.csv"))
vk_pct_df <- vk_pct_df %>%
  mutate(
    AImod = coalesce(AImod0, AImod28),
    dom_group = case_when(
      AImod > 0.66 ~ "CA",
      AImod > 0.50 & AImod <= 0.66 ~ "PPh",
      AImod <= 0.50 & H_C < 1.5 ~ "HUPs",
      H_C >= 1.5 & H_C <= 2.0 ~ "Aliphatics",
      TRUE ~ "Other"
    ),
    dom_group = factor(dom_group,
                       levels = c("Aliphatics","HUPs","PPh","CA","Other"))
  ) %>% mutate(
    delta_int = RelInt_T28 - RelInt_T0,
    Season = factor(Season, levels = c("Spring","Summer")),
    Method = factor(Method,
                    levels = c("Biodegradation","Photodegradation","Bio-photodegradation"))
  )

dom_summary <- vk_pct_df %>%
  group_by(Season, Method, dom_group) %>%
  summarise(
    mean_change = mean(pct_change, na.rm = TRUE),
    sum_delta = sum(delta_int, na.rm = TRUE),
    mean_delta = mean(delta_int, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

overall_dom_summary <- vk_pct_df %>%
  group_by(Season, Method) %>%
  summarise(
    mean_change = mean(pct_change, na.rm = TRUE),
    sum_delta = sum(delta_int, na.rm = TRUE),
    mean_delta = mean(delta_int, na.rm = TRUE),
    n = n(),
    dom_group = "All",
    .groups = "drop"
  )

dom_plot_df <- bind_rows(dom_summary, overall_dom_summary) %>%
  mutate(dom_group = factor(dom_group,
                            levels = c("Aliphatics","HUPs","PPh","CA","Other","All")))

vk_dom_test <- vk_pct_df %>%
  mutate(
    LossFlag = pct_change <= -0.999,
    dom_group = factor(dom_group,
                       levels = c("Aliphatics","HUPs","PPh","CA","Other"))
  )

# Fisher per dom_group × Method
fisher_core <- vk_dom_test %>%
  group_by(Method, dom_group) %>%
  summarise(
    loss_frac_spring = mean(LossFlag[Season=="Spring"], na.rm = TRUE),
    loss_frac_summer = mean(LossFlag[Season=="Summer"], na.rm = TRUE),
    p_Fisher = tryCatch({
      tab <- table(Season, LossFlag)
      fisher.test(tab)$p.value
    }, error = function(e) NA_real_),
    .groups = "drop"
  )

fisher_all <- vk_dom_test %>%
  mutate(dom_group = "All") %>%
  group_by(Method, dom_group) %>%
  summarise(
    loss_frac_spring = mean(LossFlag[Season=="Spring"], na.rm = TRUE),
    loss_frac_summer = mean(LossFlag[Season=="Summer"], na.rm = TRUE),
    p_Fisher = tryCatch({
      tab <- table(Season, LossFlag)
      fisher.test(tab)$p.value
    }, error = function(e) NA_real_),
    .groups = "drop"
  )

fisher_dom <- bind_rows(fisher_core, fisher_all) %>%
  mutate(
    dom_group = factor(dom_group,
                       levels = c("Aliphatics","HUPs","PPh","CA","Other","All"))
  )

perm_by_group <- function(subdf) {
  dd <- subdf 
  if (sum(dd$Season=="Spring") < 2 | sum(dd$Season=="Summer") < 2)
    return(tibble(mean_spring=NA_real_,
                  mean_summer=NA_real_,
                  p_Perm=NA_real_))
  
  observed <- mean(dd$delta_int[dd$Season=="Summer"]) -
    mean(dd$delta_int[dd$Season=="Spring"])
  
  sims <- replicate(5000, {
    s <- sample(dd$Season)
    mean(dd$delta_int[s=="Summer"]) -
      mean(dd$delta_int[s=="Spring"])
  })
  
  p <- (sum(abs(sims) >= abs(observed)) + 1) / (5000 + 1)
  
  tibble(
    mean_spring = mean(dd$delta_int[dd$Season=="Spring"]),
    mean_summer = mean(dd$delta_int[dd$Season=="Summer"]),
    p_Perm = p
  )
}

perm_core <- vk_pct_df %>%
  group_by(Method, dom_group) %>%
  group_modify(~ perm_by_group(.x)) %>%
  ungroup()

perm_all <- vk_pct_df %>%
  mutate(dom_group = "All") %>%
  group_by(Method, dom_group) %>%
  group_modify(~ perm_by_group(.x)) %>%
  ungroup()

perm_dom <- bind_rows(perm_core, perm_all) %>%
  mutate(
    dom_group = factor(dom_group,
                       levels = c("Aliphatics","HUPs","PPh","CA","Other","All"))
  )

stats_dom <- full_join(fisher_dom, perm_dom,
                       by = c("Method","dom_group")) %>%
  mutate(
    label_F = case_when(
      is.na(p_Fisher) ~ "F: ns",
      p_Fisher < 0.001 ~ "F: ***",
      p_Fisher < 0.01  ~ "F: **",
      p_Fisher < 0.05  ~ "F: *",
      TRUE ~ "F: ns"
    ),
    label_P = case_when(
      is.na(p_Perm) ~ "P: ns",
      p_Perm < 0.001 ~ "P: ***",
      p_Perm < 0.01  ~ "P: **",
      p_Perm < 0.05  ~ "P: *",
      TRUE ~ "P: ns"
    ),
    label = paste(label_F, label_P, sep="\n")
  )

yrange_dom <- dom_plot_df %>%
  group_by(Method) %>%
  summarise(ymin=min(sum_delta), ymax=max(sum_delta), .groups="drop") %>%
  mutate(y_pos = ymax + 0.18 * (ymax - ymin + 1e-6))

stats_dom <- stats_dom %>%
  left_join(yrange_dom, by="Method") %>%
  mutate(
    x_id = as.numeric(dom_group),
    group1 = x_id - 0.25,
    group2 = x_id + 0.25
  )


season_cols <- c(Spring="#6BA3C9", Summer="#D8C27A")

p_dom <- ggplot(dom_plot_df, aes(x = dom_group, y = sum_delta, fill = Season)) +
  geom_col(alpha = 0.55,
           position = position_dodge(width=0.7),
           color="black", width=0.6, linewidth=0.2) +
  facet_wrap(~ Method, ncol=3) +
  scale_fill_manual(values=season_cols) +
  labs(y="Cumulative intensity", x=NULL, fill=NULL) +
  theme_bw(base_size=12) +
  theme(
    text = element_text(family = "Helvetica Light"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.spacing = unit(1.5, "lines"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.ticks = element_line(color = "black"),
    strip.text = element_text(face = "bold"),
    strip.background=element_blank(),
    plot.margin = margin(t = 0.1, r = 0.5, b = 0.1, l = 0.5, unit = "cm"),
    legend.title = element_text(vjust = 0.8, face = "bold"),
    legend.key.height = unit(0.3, "cm"),
    legend.key.width = unit(0.3, "cm"),
    legend.position = c(0.5,0.9),
    legend.direction = "horizontal",
    legend.box = "horizontal"
  )+
  scale_y_continuous(expand=expansion(mult=c(0.05,0.25))) 

# Add significance labels
p_dom <- p_dom +
  ggpubr::stat_pvalue_manual(
    data=stats_dom,
    label="label",
    xmin="group1", xmax="group2",
    y.position="y_pos",
    tip.length=0.01,
    size=3.5,
    bracket.size=0.3,
    inherit.aes=FALSE,
    family="Helvetica Light"
  )

p_dom

ggsave(file.path(out_dir, "Group_sum_intensity_with_Fisher_and_Perm2.png"),
       p_dom, width = 10, height = 4.8, dpi = 600)
ggsave(file.path(out_dir, "Group_sum_intensity_with_Fisher_and_Perm2.pdf"),
       p_dom, width = 10, height = 4.8, device = cairo_pdf)


## Combined 3 season comparison figures--------

p_all <- p_den / p_class / p_diff +
  plot_annotation(
    tag_levels = "a",       # produces (a), (b), (c)
    tag_prefix = "",       # optional
    tag_suffix = ""
  ) +
  theme(plot.tag = element_text(face = "bold", size = 12), plot.margin = margin(1, 4, 1, 1) )  # style the labels

ggsave(file.path(out_dir, "combined_seasonal_compare2.png"),
       p_all, width = 8, height = 10, dpi = 600)
ggsave(file.path(out_dir, "combined_seasonal_compare2.pdf"),
       p_all, width = 8, height = 10, device = cairo_pdf)

p_all2 <- p_dom / p_diff2 +
  plot_annotation(
    tag_levels = "a",       # produces (a), (b), (c)
    tag_prefix = "",       # optional
    tag_suffix = ""
  ) +
  theme(plot.tag = element_text(face = "bold", size = 12), plot.margin = margin(1, 4, 1, 1) )  # style the labels

ggsave(file.path(out_dir, "combined_seasonal_compare2-5.png"),
       p_all2, width = 8, height = 7.5, dpi = 600)
ggsave(file.path(out_dir, "combined_seasonal_compare2-5.pdf"),
       p_all2, width = 8, height = 7.5, device = cairo_pdf)

## Venn diagrams of degraded formulae by experiment---------
# Define "degraded" DOM
vk_pct_df <- read_csv(file.path(out_dir, "vk_pct_df4.csv"))

if (!all(c("present0","present28","pct_change") %in% names(vk_pct_df))) {
  vk_pct_df <- vk_pct_df %>%
    mutate(
      RelInt_T0  = replace_na(RelInt_T0,  0),
      RelInt_T28 = replace_na(RelInt_T28, 0),
      present0   = RelInt_T0  > 0,
      present28  = RelInt_T28 > 0,
      pct_change = case_when(
        RelInt_T0 == 0 & RelInt_T28 > 0  ~  1,                       # new
        RelInt_T0 == 0 & RelInt_T28 == 0 ~  0,                       # both 0
        TRUE ~ (RelInt_T28 - RelInt_T0) / RelInt_T0                  # relative change
      )
    )
}

# loss threshold (e.g. 50% loss = -0.5):
tau <- -0.5  

vk_loss <- vk_pct_df %>%
  mutate(
    Method = factor(Method,
                    levels = c("Biodegradation","Photodegradation","Bio-photodegradation")),
    Season = factor(Season, levels = c("Spring","Summer")),
    degraded = present0 & (!present28 | pct_change <= tau)
  ) %>%
  filter(degraded)

#  Build sets for each season

make_sets <- function(season_name) {
  vk_loss %>%
    filter(Season == season_name) %>%
    select(Formula, Method) %>%
    distinct() %>%
    group_split(Method) %>%
    setNames(levels(vk_loss$Method)) %>%
    lapply(\(df) df$Formula)
}

spring_sets <- make_sets("Spring")
summer_sets <- make_sets("Summer")

spring_sets_df <- stack(spring_sets)
colnames(spring_sets_df) <- c("Formula", "Method")
write.table(
  spring_sets_df,
  file = file.path(out_dir, "spring_sets_all_entries.txt"),
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

summer_sets_df <- stack(summer_sets)
colnames(summer_sets_df) <- c("Formula", "Method")
write.table(
  summer_sets_df,
  file = file.path(out_dir, "summer_sets_all_entries.txt"),
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)
# Plot Venn diagrams

cols_venn <- c(
  "Biodegradation"      = "#7fc97f",
  "Photodegradation"    = "#fdc086",
  "Bio-photodegradation"= "#386cb0"
)

p_venn_spring <- ggvenn(
  spring_sets,
  fill_color  = cols_venn,
  stroke_color = "black",
  set_name_size = 4,
  show_percentage = FALSE
) +
  ggtitle("Degraded formulae — Spring") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    text = element_text(family = "Helvetica Light")
  )

p_venn_summer <- ggvenn(
  summer_sets,
  fill_color  = cols_venn,
  stroke_color = "black",
  set_name_size = 4,
  show_percentage = FALSE
) +
  ggtitle("Degraded formulae — Summer") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    text = element_text(family = "Helvetica Light")
  )

# side-by-side layout
p_venn <- p_venn_spring + p_venn_summer + plot_layout(nrow = 1)

ggsave(file.path(out_dir, "Venn_Degraded_Spring_Summer.png"),
       p_venn, width = 8, height = 5, dpi = 600)
ggsave(file.path(out_dir, "Venn_Degraded_Spring_Summer.pdf"),
       p_venn, width = 8, height = 5, device = cairo_pdf)

p_venn

# Export CSVs of overlaps (which formulas are degraded in which experiments)

# vk_loss has: Season, Method, Formula (only degraded ones)
# Build a wide table: one row per Formula × Season, columns for each experiment

overlap_df <- vk_loss %>%
  select(Season, Method, Formula) %>%
  distinct() %>%
  mutate(flag = 1L) %>%
  tidyr::pivot_wider(
    names_from  = Method,
    values_from = flag,
    values_fill = 0L
  ) %>%
  # how many experiments degraded this formula?
  mutate(
    n_experiments = `Biodegradation` +
      `Photodegradation` +
      `Bio-photodegradation`
  )

# Write per-season CSVs
spring_overlap <- overlap_df %>% filter(Season == "Spring")
summer_overlap <- overlap_df %>% filter(Season == "Summer")

readr::write_csv(spring_overlap, file.path(out_dir, "Degraded_overlap_Spring.csv"))
readr::write_csv(summer_overlap, file.path(out_dir, "Degraded_overlap_Summer.csv"))
readr::write_csv(overlap_df, file.path(out_dir, "Degraded_overlap_AllSeasons.csv"))


## Analyze DOM group-specific losses after incubation--------
vk_dom <- vk_pct_df %>%
  mutate(
    AImod = coalesce(AImod0, AImod28),
    dom_group = case_when(
      # 1) Condensed aromatics (CA; combustion-derived); 
      AImod > 0.66 ~ "CA",
      # 2) Polyphenolic-like (PPh; vascular plant–derived);
      AImod > 0.50 & AImod <= 0.66 ~ "PPh",
      # 3) Highly unsaturated & phenolic (HUPs);
      AImod <= 0.50 & H_C < 1.5 ~ "HUPs",
      # 4) Aliphatic-like;
      H_C >= 1.5 & H_C <= 2.0 ~ "Aliphatics",
      # everything else (sugar-like, peptide-like, weird edge cases…)
      TRUE ~ "Other"
    ),
    dom_group = factor(dom_group, levels = c("Aliphatics", "HUPs", "PPh", "CA", "Other"))
  )

# threshold for strong loss (-0.95 for 95% loss)
tau <- -0.95

dom_summary <- vk_dom %>%
  group_by(Season, Method, dom_group) %>%
  summarise(
    n_total   = n(),
    n_lost    = sum(present0 & (!present28 | pct_change <= -1), na.rm = TRUE),
    frac_lost = n_lost / n_total,
    #Weighted loss index (WLI)
    WLI = n_lost / sum(n_lost),
    mean_change   = mean(pct_change, na.rm = TRUE),
    #sd_change = sd(pct_change, na.rm = TRUE),
    median_change = median(pct_change, na.rm = TRUE),
    sum_RelInt_change   = sum(RelInt_T28-RelInt_T0, na.rm = TRUE),
    sum_RelInt_gain = sum(pmax(RelInt_T28 - RelInt_T0, 0), na.rm = TRUE),  # ≥ 0
    sum_RelInt_loss = sum(pmin(RelInt_T28 - RelInt_T0, 0), na.rm = TRUE),  # ≤ 0
    .groups = "drop"
  ) %>%
  group_by(Season, Method) %>%
  mutate(WLI = n_lost / sum(n_lost, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Season, Method, dom_group)

View(dom_summary)

readr::write_csv(dom_summary, file.path(out_dir, "compound_loss_preferencev3.csv"))

# SPLIT INTO PRODUCED / DEGRADED
dom_summary_long <- dom_summary %>%
  select(Season, Method, dom_group, sum_RelInt_gain, sum_RelInt_loss) %>%
  pivot_longer(
    cols = c(sum_RelInt_gain, sum_RelInt_loss),
    names_to  = "change_type",
    values_to = "sum_RelInt"
  ) %>%
  mutate(
    change_type = dplyr::recode(
      change_type,
      "sum_RelInt_gain"  = "Produced",
      "sum_RelInt_loss"  = "Degraded"
    ),
    change_type = factor(change_type, levels = c("Degraded", "Produced"))
  )

dom_cols <- c(
  Aliphatics = "#A6C36F",   # soft olive-green
  HUPs      = "#4C9F70",   # deep natural green
  PPh       = "#D99E6A",   # warm sand
  CA        = "#7C70A4",   # dusty purple
  Other     = "#B4B8C5"    # cool grey
)


p_frac_lost <- ggplot(dom_summary, aes(x = dom_group, y = frac_lost, fill = dom_group)) +
  geom_col(color = "black") +
  facet_grid(Season ~ Method) +
  scale_fill_manual(values = dom_cols) +
  labs(
    x = "DOM groups",
    y = "Fraction of formulae lost",
    fill = "Group"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks   = element_line(color = "black"),
    axis.text    = element_text(color = "black"),
    axis.title   = element_text(color = "black"),
    legend.position = "none"
  )
p_frac_lost

ggsave(file.path(out_dir, "compound_lost_by_fractionv3.png"),
       p_frac_lost, width = 9, height = 6, dpi = 600)
ggsave(file.path(out_dir, "compound_lost_by_fractionv3.pdf"),
       p_frac_lost, width = 9, height = 6, device = cairo_pdf)


p_wli <- ggplot(dom_summary, aes(x = dom_group, y = WLI, fill = dom_group)) +
  geom_col(color = "black", linewidth=0.3) +
  facet_grid(Season ~ Method) +
  scale_fill_manual(values = dom_cols) +
  labs(
    x = NULL,
    y = "Weighted formulae lost fraction",
    fill = "Group"
  ) +
  theme_bw(base_size = 11) +
  theme(
    #text = element_text(family = "Helvetica Light"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks   = element_line(color = "black"),
    axis.text    = element_text(color = "black"),
    axis.title   = element_text(color = "black"),
    legend.position = "none"
  )
p_wli

ggsave(file.path(out_dir, "compound_lost_by_weighted_fraction2.png"),
       p_wli, width = 6.9, height = 4.6, dpi = 600)
ggsave(file.path(out_dir, "compound_lost_by_weighted_fraction2.pdf"),
       p_wli, width = 6.9, height = 4.6, device = cairo_pdf)


p_mean_change <- ggplot(dom_summary, aes(x = dom_group, y = mean_change, fill = dom_group)) +
  geom_col(color = "black",linewidth=0.3) +
  #geom_hline(yintercept = 0, linetype = "dotted",linewidth=0.3) +
  facet_grid(Season ~ Method) +
  scale_fill_manual(values = dom_cols) +
  labs(
    x = "DOM groups",
    y = "Mean relative change after incubation",
    fill = "Group"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks   = element_line(color = "black"),
    axis.text    = element_text(color = "black"),
    axis.title   = element_text(color = "black"),
    legend.position = "none"
  )
p_mean_change

ggsave(file.path(out_dir, "compound_mean_change v3.png"),
       p_mean_change, width = 6.9, height = 4.6, dpi = 600)
ggsave(file.path(out_dir, "compound_mean_change v3.pdf"),
       p_mean_change, width = 6.9, height = 4.6, device = cairo_pdf)


p_sum_RelInt_change <- ggplot(dom_summary, aes(x = dom_group, y = sum_RelInt_change, fill = dom_group)) +
  geom_col(color = "black",linewidth=0.3) +
  #geom_hline(yintercept = 0, linetype = "dotted",linewidth=0.3) +
  facet_grid(Season ~ Method) +
  scale_fill_manual(values = dom_cols) +
  labs(
    x = "DOM groups",
    y = expression("Commulative "*Delta*"intensity after incubation"),
    fill = "Group"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks   = element_line(color = "black"),
    axis.text    = element_text(color = "black"),
    axis.title   = element_text(color = "black"),
    legend.position = "none"
  )
p_sum_RelInt_change

ggsave(file.path(out_dir, "compound_sum_intensity_change2.png"),
       p_sum_RelInt_change, width = 6.9, height = 4.6, dpi = 600)
ggsave(file.path(out_dir, "compound_sum_intensity_change2.pdf"),
       p_sum_RelInt_change, width = 6.9, height = 4.6, device = cairo_pdf)

# New plot: separate produced vs degraded
library(ggpattern)

p_sum_RelInt_gain_loss <- ggplot(
  dom_summary_long,
  aes(x = dom_group, y = sum_RelInt)) +
  geom_col_pattern(
    aes(fill = dom_group, pattern = change_type),
    color = "black",
    linewidth = 0.3,
    pattern_fill      = "black",
    pattern_colour    = "black",
    pattern_angle     = 45,             # 45° diagonal
    pattern_density   = 0.2,            # adjust line density
    pattern_spacing   = 0.02,           # spacing of stripes
    pattern_size      = 0.01,            # thickness of stripes
  ) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.3) +
  facet_grid(Season ~ Method) +
  scale_fill_manual(values = dom_cols, name = NULL) +
  scale_pattern_manual( values = c("Produced" = "stripe", "Degraded" = "none"), guide = "none") +
  labs(
    x = NULL,
    y = expression("Cumulative intensity")
  ) +
  theme_bw(base_size = 11) +
  theme(
    text            = element_text(family = "Helvetica Light"),
    panel.grid      = element_blank(),
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    axis.ticks      = element_line(color = "black"),
    axis.text       = element_text(color = "black"),
    axis.title      = element_text(color = "black"),
    legend.position = "none"
  )

p_sum_RelInt_gain_loss

ggsave(file.path(out_dir, "compound_sum_intensity_gain_loss3.png"),
       p_sum_RelInt_gain_loss, width = 7, height = 4.8, dpi = 600)
ggsave(file.path(out_dir, "compound_sum_intensity_gain_loss3.pdf"),
       p_sum_RelInt_gain_loss, width = 7, height = 4.8, device = cairo_pdf)

# DOC compound change pie charts scaled to concentration
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggforce)
library(readr)

# 1) Load DOC concentration table
doc_df <- read_csv("~/Documents/Papers/DOC_degradation/Data/DOC_change_FHS3.csv") %>%
  rename(DOC = `DOC_mgL-1`) %>%
  mutate(
    TimePoint = factor(TimePoint, levels = c("T0", "T28")),
    Method    = factor(Method)
  )

# 2) Summarise FT-ICR DOM intensity by group (T0 & T28)
dom_pie <- vk_dom %>%
  group_by(Season, Method, dom_group) %>%
  summarise(
    sum_T0  = sum(RelInt_T0,  na.rm = TRUE),
    sum_T28 = sum(RelInt_T28, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols      = c(sum_T0, sum_T28),
    names_to  = "TimePoint",
    values_to = "sum_RelInt"
  ) %>%
  mutate(
    TimePoint = dplyr::recode(TimePoint, "sum_T0" = "T0", "sum_T28" = "T28"),
    TimePoint = factor(TimePoint, levels = c("T0", "T28")),
    dom_group = factor(dom_group,
                       levels = c("Aliphatics", "HUPs", "PPh", "CA", "Other"))
  )

# 3) Join DOC concentration into the FT-ICR summary
dom_pie <- dom_pie %>%
  left_join(doc_df, by = c("Season", "Method", "TimePoint"))

# 4) Get slice proportions FROM RelInt ONLY (no DOC scaling of intensities)
dom_pie <- dom_pie %>%
  group_by(Season, Method, TimePoint) %>%
  mutate(
    total_int  = sum(sum_RelInt, na.rm = TRUE),
    prop       = sum_RelInt / total_int,
    end_angle  = 2 * pi * cumsum(prop),
    start_angle = dplyr::lag(end_angle, default = 0)
  ) %>%
  ungroup()

# 5) GLOBAL pie radius scaling (area ∝ DOC)
max_DOC <- max(dom_pie$DOC, na.rm = TRUE)

dom_pie <- dom_pie %>%
  mutate(
    # area ∝ DOC → r ∝ sqrt(DOC)
    radius      = sqrt(DOC / max_DOC),
    # explicit x positions for pies inside each facet
    x_center    = if_else(TimePoint == "T0", -1, 1),
    # tweak factor to control visual size
    radius_plot = radius * 1.1
  )

# 6) Labels (using prop already in dom_pie)
dom_pie_labels <- dom_pie %>%
  mutate(
    pct       = prop * 100,
    mid_angle = (start_angle + end_angle) / 2,
    adj_angle = -mid_angle + pi/2,
    lab_r     = radius_plot * 0.65,
    lab_x     = x_center + lab_r * cos(adj_angle),
    lab_y     = 0        + lab_r * sin(adj_angle),
    lab_txt   = paste0(round(pct), "%")
  ) %>%
  filter(pct >= 4)   # optional: hide tiny slices

# 7) PLOT: pie area scaled to DOC, slice fractions from RelInt
p_scaled_DOC_pies <- ggplot(dom_pie) +
  geom_arc_bar(
    aes(
      x0   = x_center,
      y0   = 0,
      r0   = 0,
      r    = radius_plot,
      start = start_angle,
      end   = end_angle,
      fill  = dom_group
    ),
    color = "black",
    linewidth = 0.3
  ) +
  geom_text(
    data = dom_pie_labels,
    aes(x = lab_x, y = lab_y, label = lab_txt),
    size   = 2,
    color  = "black",
    family = "Helvetica Light"
  ) +
  coord_fixed(xlim = c(-2, 2), ylim = c(-1.2, 1.2)) +
  facet_grid(Season ~ Method) +
  scale_fill_manual(
    values = c(
      Aliphatics = "#A6C36F",   # soft olive-green
      HUPs      = "#4C9F70",    # deep natural green
      PPh       = "#D99E6A",    # warm sand
      CA        = "#7C70A4",    # dusty purple
      Other     = "#B4B8C5"     # cool grey
    ),
    name = NULL
  ) +
  labs(
    x = NULL, y = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    text            = element_text(family = "Helvetica Light"),
    panel.grid      = element_blank(),
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.text       = element_blank(),
    axis.ticks      = element_blank(),
    legend.position = "bottom"
  )

p_scaled_DOC_pies


ggsave(file.path(out_dir, "p_scaled_DOC_pies2.png"),
       p_scaled_DOC_pies, width = 7, height = 3.8, dpi = 600)
ggsave(file.path(out_dir, "p_scaled_DOC_pies2.pdf"),
       p_scaled_DOC_pies, width = 7, height = 3.8, device = cairo_pdf)


# dom_summary columns:
# Season, Method, dom_group, n_total, n_lost

# 1) Add remaining formulas
dom_long <- dom_summary %>%
  mutate(remaining = n_total - n_lost) %>%
  pivot_longer(
    cols = c(n_total, remaining),
    names_to  = "State",
    values_to = "count"
  ) %>%
  mutate(
    State = recode(State,
                   n_total  = "Original",
                   remaining = "Remaining"),
    State = factor(State, levels = c("Original", "Remaining")),
    Method = factor(Method,
                    levels = c("Biodegradation", "Photodegradation", "Bio-photodegradation"))
  )

# 2) Color palette for DOM groups (customize as you like)
dom_cols <- c(
  Aliphatics = "#A6C36F",   # soft olive-green
  HUPs      = "#4C9F70",   # deep natural green
  PPh       = "#D99E6A",   # warm sand
  CA        = "#7C70A4",   # dusty purple
  Other     = "#B4B8C5"    # cool grey
)

# 3) One large plot:
#   - Facet rows = Season (Spring / Summer)
#   - Facet cols = Method (Bio / Photo / Bio-Photo)
#   - In each facet: 2 bars (Original, Remaining) stacked by dom_group
p_dom_bars <- ggplot(dom_long,
                     aes(x = State, y = count, fill = dom_group)) +
  geom_col(color = "black", width = 0.7, linewidth = 0.3) +
  facet_grid(Season ~ Method) +
  scale_fill_manual(values = dom_cols, name = "DOM group") +
  labs(
    x = NULL,
    y = "Number of molecular formulae",
    title = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    text          = element_text(family = "Helvetica Light"),
    panel.grid    = element_blank(),
    panel.border  = element_rect(color = "black", fill = NA, linewidth = 0.6),
    strip.background = element_blank(),
    strip.text    = element_text(face = "bold"),
    axis.text.x   = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom",
    legend.box    = "vertical",
    legend.margin = margin(t = -3, r = 5, b = 3, l = 5)
  )
p_dom_bars


ggsave(file.path(out_dir, "DOM_original_vs_remaining_by_group_Spring_Summer2.png"),
       p_dom_bars, width = 8, height = 5, dpi = 300)
ggsave(file.path(out_dir, "DOM_original_vs_remaining_by_group_Spring_Summer2.pdf"),
       p_dom_bars, width = 8, height = 5, device = cairo_pdf)



library(tidyverse)

# dom_summary: Season, Method, dom_group, n_total, n_lost

# 1) Decompose Original vs Remaining into stacked parts
dom_parts <- dom_summary %>%
  mutate(remaining = n_total - n_lost) %>%
  # Remaining bar: two parts (remaining + lost)
  pivot_longer(
    cols = c(remaining, n_lost),
    names_to  = "Part",
    values_to = "count"
  ) %>%
  mutate(
    State = "Remaining",
    Part  = recode(Part,
                   remaining = "Remaining",
                   n_lost    = "Lost")
  )

# Original bar: one part = total
orig_parts <- dom_summary %>%
  transmute(
    Season, Method, dom_group,
    State = "Original",
    Part  = "Total",
    count = n_total
  )

plot_df <- bind_rows(orig_parts, dom_parts) %>%
  mutate(
    State  = factor(State, levels = c("Original", "Remaining")),
    Method = factor(Method,
                    levels = c("Biodegradation","Photodegradation","Bio-photodegradation")),
    Part   = factor(Part, levels = c("Remaining","Lost","Total"))
  )

# 2) DOM group colours
dom_cols <- c(
  Aliphatics = "#A6C36F",
  HUPs       = "#4C9F70",
  PPh        = "#D99E6A",
  CA         = "#7C70A4",
  Other      = "#B4B8C5"
)

# 3) Plot: Remaining bar = remaining (solid) + lost (dashed, semi-transparent)
p_dom_bars2 <- ggplot(
  plot_df,
  aes(x = State, y = count, fill = dom_group,
      alpha = Part, linetype = Part)
) +
  geom_col(width = 0.7, color = "black", linewidth=0.3) +
  facet_grid(Season ~ Method) +
  scale_fill_manual(values = dom_cols, name = "DOM group") +
  scale_alpha_manual(
    values = c(Remaining = 1, Lost = 0.4, Total = 1),
    guide = "none"
  ) +
  scale_linetype_manual(
    values = c(Remaining = "solid", Lost = "dashed", Total = "solid"),
    guide = "none"
  ) +
  labs(
    x = NULL,
    y = "Number of formulae",
    title = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    text           = element_text(family = "Helvetica Light"),
    panel.grid     = element_blank(),
    panel.border   = element_rect(color = "black", fill = NA, linewidth = 0.6),
    strip.background = element_blank(),
    strip.text     = element_text(face = "bold"),
    axis.text.x    = element_text(color = "black"),
    legend.position = "bottom",
    legend.box     = "vertical"
  )

p_dom_bars2

ggsave(file.path(out_dir, "DOM_original_vs_remaining_by_group_show_lost2.png"),
       p_dom_bars2, width = 6.9, height = 4.6, dpi = 600)
ggsave(file.path(out_dir, "DOM_original_vs_remaining_by_group_show_lost2.pdf"),
       p_dom_bars2, width = 6.9, height = 4.6, device = cairo_pdf)


## Identify Bio-produced or Photo-produced & used in BioPhoto------
## 1) Split by method 
vk_bio <- vk_pct_df %>%
  filter(Method == "Biodegradation")

vk_photo <- vk_pct_df %>%
  filter(Method == "Photodegradation")

vk_biophoto <- vk_pct_df %>%
  filter(Method == "Bio-photodegradation")

## 2) Identify photo-produced formulas (present only at T28 in Photo) 
bio_produced <- vk_bio %>%
  filter(!present0 & present28) %>%   # produced during photodegradation
  select(Season, Formula, O_C, H_C)

photo_produced <- vk_photo %>%
  filter(!present0 & present28) %>%   # produced during photodegradation
  select(Season, Formula, O_C, H_C)

biophoto_produced <- vk_biophoto %>%
  filter(!present0 & present28) %>%   # produced during photodegradation
  select(Season, Formula, O_C, H_C)

## 3) Join with BioPhoto to see which of those are utilized 

photo_prod_biophoto <- photo_produced %>%
  left_join(
    vk_biophoto %>%
      select(
        Season, Formula,
        RelInt_T0_BioP   = RelInt_T0,
        RelInt_T28_BioP  = RelInt_T28,
        pct_change_BioP  = pct_change,
        present0_BioP    = present0,
        present28_BioP   = present28
      ),
    by = c("Season", "Formula")
  ) %>%
  mutate(
    # define "utilized in BioPhoto" (you can tweak this criterion):
    used_in_BioPhoto = !is.na(pct_change_BioP) &
      pct_change_BioP < 0 &          # decreased in BioPhoto
      present0_BioP # observed at t0
  )

bio_prod_biophoto <- bio_produced %>%
  left_join(
    vk_biophoto %>%
      select(
        Season, Formula,
        RelInt_T0_BioP   = RelInt_T0,
        RelInt_T28_BioP  = RelInt_T28,
        pct_change_BioP  = pct_change,
        present0_BioP    = present0,
        present28_BioP   = present28
      ),
    by = c("Season", "Formula")
  ) %>%
  mutate(
    # define "utilized in BioPhoto" (you can tweak this criterion):
    used_in_BioPhoto = !is.na(pct_change_BioP) &
      pct_change_BioP < 0 &          # decreased in BioPhoto
      present0_BioP # observed at t0
  )

## 4) Split produced sets into "produced-only" vs "produced & used" 

photo_used <- photo_prod_biophoto %>%
  filter(used_in_BioPhoto) %>%
  mutate(group = "Photo-produced & used in BioPhoto")

photo_only <- photo_prod_biophoto %>%
  filter(!used_in_BioPhoto) %>%
  mutate(group = "Photo-produced")

bio_used <- bio_prod_biophoto %>%
  filter(used_in_BioPhoto) %>%
  mutate(group = "Bio-produced & used in BioPhoto")

bio_only <- bio_prod_biophoto %>%
  filter(!used_in_BioPhoto) %>%
  mutate(group = "Bio-produced")

## 5) Background formulas (same for both panels) 

vk_bg <- vk_pct_df %>%
  distinct(Season, Formula, O_C, H_C) %>%
  filter(is.finite(O_C), is.finite(H_C))

## 6) Build panel-wise plotting data 

# Photodegradation panels: background + photo-produced
vk_photo_panel <- bind_rows(
  vk_bg %>%
    mutate(Source = "Photodegradation",
           group  = "Background"),
  photo_only %>%
    transmute(Season, O_C, H_C,
              Source = "Photodegradation",
              group  = "Photo-produced"),
  photo_used %>%
    transmute(Season, O_C, H_C,
              Source = "Photodegradation",
              group  = "Photo-produced & used in BioPhoto")
)

# Biodegradation panels: background + bio-produced
vk_bio_panel <- bind_rows(
  vk_bg %>%
    mutate(Source = "Biodegradation",
           group  = "Background"),
  bio_only %>%
    transmute(Season, O_C, H_C,
              Source = "Biodegradation",
              group  = "Bio-produced"),
  bio_used %>%
    transmute(Season, O_C, H_C,
              Source = "Biodegradation",
              group  = "Bio-produced & used in BioPhoto")
)

# Combine for plotting
vk_panel <- bind_rows(vk_photo_panel, vk_bio_panel) %>%
  filter(is.finite(O_C), is.finite(H_C)) %>%
  mutate(
    Source = factor(Source,
                    levels = c("Photodegradation","Biodegradation")),
    group = factor(
      group,
      levels = c(
        "Background",
        "Photo-produced","Photo-produced & used in BioPhoto",
        "Bio-produced","Bio-produced & used in BioPhoto"
      )
    )
  )

# Count number of points per category in each facet
vk_counts <- vk_panel %>% 
  filter(group != "Background")  %>%
  count(Season, Source, group) %>%
  group_by(Season, Source) %>%
  mutate(
    ypos = 2.45 - 0.15 * (row_number() - 1),   # stack labels top → bottom
    xpos = 1.2                                # left margin position
  ) %>%
  ungroup()


## 7) Van Krevelen diagram: 4 panels (Season × Source) 
vk_panel_nobg <- vk_panel %>%
  filter(group != "Background")

p_vk_prod_used_4panel <- ggplot(vk_panel_nobg,
                                aes(x = O_C, y = H_C, color = group)) +
  geom_point(size = 1.5, stroke = 0.15, alpha = 0.75) +
  facet_grid(Season ~ Source) +
  scale_color_manual(
    values = c(
      "Background"                         = "grey85",
      "Photo-produced"                     = "#fdbb84",
      "Photo-produced & used in BioPhoto"  = "#d7301f",
      "Bio-produced"                       = "#b6e7ec",
      "Bio-produced & used in BioPhoto"    = "#063970"
    ),
    labels = c(
      "Photo-produced"                         = "Photo-produced",
      "Photo-produced & used in BioPhoto"      = "Photo-produced & degraded in Bio-photodegradation",
      "Bio-produced"                            = "Bio-produced",
      "Bio-produced & used in BioPhoto"         = "Bio-produced & degraded in Bio-photodegradation"
    ),
    name = NULL
  ) +
  geom_text(
    data = vk_counts,
    aes(x = xpos, y = ypos, label = n, color = group),
    size = 3,
    fontface = "bold",
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  labs(
    x = "O/C",
    y = "H/C",
    title = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    text            = element_text(family = "Helvetica Light"),
    panel.grid      = element_blank(),
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.spacing = unit(1, "lines"),
    strip.background= element_blank(),
    strip.text      = element_text(face = "bold"),
    axis.text       = element_text(color = "black"),
    axis.title      = element_text(color = "black"),
    legend.position = "bottom",
    legend.margin = margin(t = -5, r = 0, b = 0, l = 0)
  ) +
  scale_x_continuous(limits = c(0, 1.5), expand = expansion(mult = 0.01)) +
  scale_y_continuous(limits = c(0, 2.5), expand = expansion(mult = 0.01))

p_vk_prod_used_4panel

ggsave(file.path(out_dir, "VK_diagram_produced_used_4panel2.png"),
       p_vk_prod_used_4panel, width = 6, height = 5, dpi = 600)
ggsave(file.path(out_dir, "VK_diagram_produced_used_4panel2.pdf"),
       p_vk_prod_used_4panel, width = 6, height = 5, device = cairo_pdf)

## Identify Bio-produced or Photo-produced & used in Photo-degradation or Bio-degradation------
vk_bio <- vk_dom %>%
  filter(Method == "Biodegradation")

vk_photo <- vk_dom %>%
  filter(Method == "Photodegradation")

bio_produced <- vk_bio %>%
  filter(!present0 & present28) %>%   # produced during photodegradation
  select(Season, Formula, O_C, H_C)

photo_produced <- vk_photo %>%
  filter(!present0 & present28) %>%   # produced during photodegradation
  select(Season, Formula, O_C, H_C)

bio_prod_used_in_photo <- bio_produced %>%
  left_join(
    vk_photo %>%
      select(
        Season, Formula, Class, dom_group,
        RelInt_T0_Photo  = RelInt_T0,
        RelInt_T28_Photo = RelInt_T28,
        pct_change_Photo = pct_change,
        present0_Photo   = present0,
        present28_Photo  = present28
      ),
    by = c("Season", "Formula")
  ) %>%
  mutate(
    used_in_Photo = !is.na(pct_change_Photo) &
      present0_Photo &               # must be present at Photo T0
      pct_change_Photo < 0           # sunlight degrades it
  )

photo_prod_used_in_bio <- photo_produced %>%
  left_join(
    vk_bio %>%
      select(
        Season, Formula, Class, dom_group, 
        RelInt_T0_Bio  = RelInt_T0,
        RelInt_T28_Bio = RelInt_T28,
        pct_change_Bio = pct_change,
        present0_Bio   = present0,
        present28_Bio  = present28
      ),
    by = c("Season", "Formula")
  ) %>%
  mutate(
    used_in_Bio = !is.na(pct_change_Bio) &
      present0_Bio &                    # present at Bio T0 → available to microbes
      pct_change_Bio < 0                # consumed / degraded during Bio
  )

plot_photo_to_bio <- bind_rows(
  photo_prod_used_in_bio %>%
    filter(!used_in_Bio) %>%
    mutate(group = "Photo-produced",
           Panel = "Photo-produced & bio-degraded"),
  
  photo_prod_used_in_bio %>%
    filter(used_in_Bio) %>%
    mutate(group = "Photo-produced & bio-degraded",
           Panel = "Photo-produced & bio-degraded")
)

plot_bio_to_photo <- bind_rows(
  bio_prod_used_in_photo %>%
    filter(!used_in_Photo) %>%
    mutate(group = "Bio-produced",
           Panel = "Bio-produced & photo-degraded"),
  
  bio_prod_used_in_photo %>%
    filter(used_in_Photo) %>%
    mutate(group = "Bio-produced & photo-degraded",
           Panel = "Bio-produced & photo-degraded")
)

vk_cross <- bind_rows(plot_photo_to_bio, plot_bio_to_photo)
vk_cross$group <- factor(
  vk_cross$group,
  levels = c("Photo-produced",
             "Photo-produced & bio-degraded",
             "Bio-produced",
             "Bio-produced & photo-degraded")
)

vk_cross$Panel <- factor(
  vk_cross$Panel,
  levels = c("Photo-produced & bio-degraded", "Bio-produced & photo-degraded")
)

p_cross <- ggplot(vk_cross, aes(x = O_C, y = H_C, color = group)) +
  geom_point(size = 1.5, alpha = 0.7, stroke = 0.15) +
  facet_grid(Season ~ Panel) +
  scale_color_manual(
    values = c(
      "Photo-produced"                 = "#fdbb84",
      "Photo-produced & bio-degraded"   = "#d7301f",
      "Bio-produced"                   = "#6BA3C9",
      "Bio-produced & photo-degraded"   = "#08589e"
    ),
    labels = c("Photo-produced", "Photo-produced & \nbio-degraded", "Bio-produced", "Bio-produced & \nphoto-degraded"),
    name = NULL
  ) +
  labs(
    x = "O/C", y = "H/C",
    title = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    text = element_text(family = "Helvetica Light"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", linewidth = 0.6),
    legend.position = "right",
    axis.text    = element_text(color = "black"),
    legend.margin = margin(t = -5, r = 0, b = 0, l = 0),
    legend.key.spacing.y = unit(1, 'cm')
  ) +
  guides(color = guide_legend(nrow = 4, byrow = TRUE)) +
  scale_x_continuous(limits = c(0, 1.5)) +
  scale_y_continuous(limits = c(0, 2.5))

p_cross

vk_four_groups3 <- function(p, col = "grey20") {
  p +
    geom_hline(yintercept = 2.0, linetype = "dashed",
               linewidth = 0.35, color = col, inherit.aes = FALSE) +
    geom_hline(yintercept = 1.50, linetype = "dashed",
               linewidth = 0.35, color = col, inherit.aes = FALSE) +
    geom_abline(intercept = 1.1, slope = -0.2, linetype = "dashed",
                linewidth = 0.35, color = col, inherit.aes = FALSE) +
    geom_abline(intercept = 0.75, slope = -0.20, linetype = "dashed",
                linewidth = 0.35, color = col, inherit.aes = FALSE)
}

p_cross2 <- vk_four_groups3(p_cross)

library(dplyr)
library(ggplot2)


# Photo-produced → used in Bio
photo_dom_counts <- photo_prod_used_in_bio %>%
  filter(!is.na(dom_group)) %>%
  mutate(
    Panel = "Photo-produced & bio-degraded",
    group = if_else(
      used_in_Bio,
      "Photo-produced & degraded in Bio",
      "Photo-produced only"
    )
  ) %>%
  count(Season, Panel, dom_group, group, name = "n")

# Bio-produced → used in Photo
bio_dom_counts <- bio_prod_used_in_photo %>%
  filter(!is.na(dom_group)) %>%
  mutate(
    Panel = "Bio-produced & photo-degraded",
    group = if_else(
      used_in_Photo,
      "Bio-produced & degraded in Photo",
      "Bio-produced only"
    )
  ) %>%
  count(Season, Panel, dom_group, group, name = "n")

# Combine
cross_dom_counts <- bind_rows(photo_dom_counts, bio_dom_counts) %>%
  mutate(
    dom_group = factor(dom_group,
                       levels = c("Aliphatics","HUPs","PPh","CA","Other")),
    Panel = factor(Panel,
                   levels = c("Photo-produced & bio-degraded","Bio-produced & photo-degraded")),
    group = factor(
      group,
      levels = c(
        "Photo-produced only",
        "Photo-produced & degraded in Bio",
        "Bio-produced only",
        "Bio-produced & degraded in Photo"
      )
    )
  )


group_cols <- c(
  "Photo-produced only"              = "#fdbb84",
  "Photo-produced & degraded in Bio" = "#d7301f",
  "Bio-produced only"                = "#6BA3C9",
  "Bio-produced & degraded in Photo" = "#08589e"
)

group_levels <- names(group_cols)

p_dom_cross <- ggplot(
  cross_dom_counts %>%
    mutate(group = factor(group, levels = group_levels)),
  aes(x = dom_group, y = n, fill = group)
) +
  geom_col(color = "black", width = 0.7, linewidth = 0.2) +
  facet_grid(Season ~ Panel, scales = "free_y") +
  scale_fill_manual(
    values = group_cols,
    breaks = group_levels,  # guarantees legend order
    labels = c(
      "Photo-produced",
      "Photo-produced &\nbio-degraded",
      "Bio-produced",
      "Bio-produced &\nphoto-degraded"
    ),
    name = NULL
  ) +
  labs(x = NULL, y = "Number of formulae") +
  theme_bw(base_size = 11) +
  theme(
    text = element_text(family = "Helvetica Light"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", linewidth = 0.6),
    legend.position = "right",
    axis.text = element_text(color = "black"),
    legend.margin = margin(t = -5, r = 0, b = 0, l = 0),
    legend.key.spacing.y = unit(1, "cm")
  ) +
  guides(fill = guide_legend(nrow = 4, byrow = TRUE))

p_dom_cross

p_bio_photo_enhance <- p_cross2 / p_dom_cross +
  plot_annotation(
    tag_levels = "a",       # produces (a), (b)
    tag_prefix = "",       # optional
    tag_suffix = ""
  ) 

ggsave(file.path(out_dir, "p_bio_photo_enhance3-2.png"),
       p_bio_photo_enhance, width = 7.2, height = 9, dpi = 600)
ggsave(file.path(out_dir, "p_bio_photo_enhance3-2.pdf"),
       p_bio_photo_enhance, width =7.2, height = 9, device = cairo_pdf)

## Compare molecular weight (m/z) of produced compounds at T28 vs original compounds at T0 --------
out_dir <- "~/Documents/Papers/DOC_degradation/Data/Output"
vk_path <- file.path(out_dir, "vk_pct_df4.csv")

vk <- read_csv(vk_path, show_col_types = FALSE)

# DOM group assignment rule (same thresholds you used) 
assign_dom_group <- function(AImod, H_C) {
  dplyr::case_when(
    AImod > 0.66 ~ "CA",
    AImod > 0.50 & AImod <= 0.66 ~ "PPh",
    AImod <= 0.50 & H_C < 1.5 ~ "HUPs",
    H_C >= 1.5 & H_C <= 2.0 ~ "Aliphatics",
    TRUE ~ "Other"
  )
}

# keep Bio/Photo only (robust to naming variants)
vk2 <- vk %>%
  mutate(
    Method2 = case_when(
      grepl("^Bio",  Method, ignore.case = TRUE) & !grepl("Photo", Method, ignore.case = TRUE) ~ "Biodegradation",
      grepl("^Photo", Method, ignore.case = TRUE) ~ "Photodegradation",
      # if your Method is already full names, keep them:
      Method %in% c("Biodegradation","Photodegradation") ~ Method,
      TRUE ~ as.character(Method)
    )
  ) %>%
  filter(Method2 %in% c("Biodegradation","Photodegradation")) %>%
  mutate(
    Method2 = factor(Method2, levels = c("Biodegradation","Photodegradation"))
  )

# build two pools: T0 (original) and Produced at T28
t0_pool <- vk2 %>%
  filter((present0 %in% TRUE) & (present28 %in% FALSE)) %>%
  transmute(
    Season,
    Method = Method2,
    Formula,
    TimeClass = "T0 (original)",
    MW = as.numeric(mz0),
    dom_group = assign_dom_group(as.numeric(AImod0), as.numeric(H_C))
  )

prod_pool <- vk2 %>%
  filter((present0 %in% FALSE) & (present28 %in% TRUE)) %>%
  transmute(
    Season,
    Method = Method2,
    Formula,
    TimeClass = "T28 (produced)",
    MW = as.numeric(mz28),
    dom_group = assign_dom_group(as.numeric(AImod28), as.numeric(H_C))
  )

plot_df <- bind_rows(t0_pool, prod_pool) %>%
  filter(dom_group %in% c("Aliphatics","HUPs","PPh","CA")) %>%
  mutate(
    dom_group = factor(dom_group, levels = c("Aliphatics","HUPs","PPh","CA")),
    TimeClass = factor(TimeClass, levels = c("T0 (original)", "T28 (produced)"))
  ) %>%
  filter(is.finite(MW))

# boxplot: compare MW at T0 vs Produced at T28, within each DOM group & method 
p_mw <- ggplot(plot_df %>% filter(dom_group %in% c("Aliphatics", "HUPs")), aes(x = TimeClass, y = MW, fill = TimeClass)) +
  geom_boxplot(
    width = 0.65,
    outlier.shape = 21,
    outlier.size = 1.6,
    outlier.stroke = 0.2,
    outlier.alpha = 0.6,
    color = "black",
    linewidth = 0.3
  ) +  
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,          # diamond
    size = 2.8,
    fill = "white",
    color = "black",
    position = position_dodge(width = 0.7)
  ) +
  facet_grid(dom_group ~ Method, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "T0 (original)"  = "grey85",
      "T28 (produced)" = "#fdbb84"
    )) +
  labs(x = NULL, y = "m/z") +
  theme_minimal(base_size = 12) + ylim(100, 860) +
  theme(
    text = element_text(family = "Helvetica Light"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.ticks = element_line(color = "black"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.position = "none"
  )

p_mw_sig <- p_mw +
  stat_compare_means(
    comparisons = list(c("T0 (original)", "T28 (produced)")),
    method = "wilcox.test",
    label = "p.signif",   # shows ***, **, *, ns
    tip.length = 0.01,
    color = "grey20"
  )

p_mw_sig

p_mw
ggsave(file.path(out_dir, "Boxplot_MW_DOMgroup_T0_vs_ProducedT28_BioPhoto.png"),
       p_mw_sig, width = 5, height = 5, dpi = 600)
ggsave(file.path(out_dir, "Boxplot_MW_DOMgroup_T0_vs_ProducedT28_BioPhoto.pdf"),
       p_mw_sig, width = 5, height = 5, device = cairo_pdf)

p_mw_season <- ggplot(
  plot_df %>%
    mutate(
      Season = factor(Season, levels = c("Spring", "Summer")),
      TimeClass = factor(TimeClass, levels = c("T0 (original)", "T28 (produced)"))
    ),
  aes(x = Season, y = MW, fill = TimeClass)
) +
  geom_boxplot(
    position = position_dodge(width = 0.7),
    width = 0.6,
    outlier.shape = 21,
    outlier.size = 1.6,
    color = "black",
    linewidth = 0.5
  ) + 
  stat_summary(
    aes(group = interaction(Season, TimeClass)),
    fun = mean,
    geom = "point",
    shape = 23,          # diamond
    size = 2.8,
    fill = "white",
    color = "black",
    position = position_dodge(width = 0.7)
  ) +
  facet_grid(dom_group ~ Method, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "T0 (original)"  = "#9e9e9e",
      "T28 (produced)" = "#e41a1c"
    )
  ) +
  labs(
    x = NULL,
    y = "Molecular weight (m/z)",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.position = "top"
  )

p_mw_season

## Check synergistic or competition of aliphatics between Bio and Photo in BioPhoto------

## Aliphatic subset
vk_aliph <- vk_pct_df %>%
  filter(dom_group == "Aliphatics") %>%
  select(Season, Method, Formula, RelInt_T0, RelInt_T28, pct_change)

## Collapse any duplicates (Season × Method × Formula)
vk_aliph_dedup <- vk_aliph %>%
  group_by(Season, Method, Formula) %>%
  summarise(
    RelInt_T0  = sum(RelInt_T0,  na.rm = TRUE),
    RelInt_T28 = sum(RelInt_T28, na.rm = TRUE),
    pct_change = mean(pct_change, na.rm = TRUE),
    .groups    = "drop"
  )

## Wide: Bio, Photo, BioPhoto columns
vk_aliph_wide <- vk_aliph_dedup %>%
  mutate(Method = as.character(Method)) %>%
  pivot_wider(
    names_from  = Method,
    values_from = c(RelInt_T0, RelInt_T28, pct_change),
    names_glue  = "{Method}_{.value}"
  )

## Per-formula intensity changes (negative = loss)
vk_aliph_int <- vk_aliph_wide %>%
  mutate(
    dI_Bio      = Biodegradation_RelInt_T28      - Biodegradation_RelInt_T0,
    dI_Photo    = Photodegradation_RelInt_T28    - Photodegradation_RelInt_T0,
    dI_BioPhoto = `Bio-photodegradation_RelInt_T28` - `Bio-photodegradation_RelInt_T0`
  )

tol <- 1e-3   # small tolerance to ignore tiny numerical differences

vk_aliph_tag <- vk_aliph_int %>%
  mutate(
    # expected loss if BioPhoto is at least as strong as the best single process
    dI_expected = pmin(dI_Bio, dI_Photo, na.rm = TRUE),
    comp_index  = dI_BioPhoto - dI_expected,
    interaction = case_when(
      !is.finite(comp_index)            ~ NA_character_,
      comp_index >  tol                 ~ "Competitive",   # BioPhoto loses LESS
      comp_index < -tol                 ~ "Synergistic",   # BioPhoto loses MORE
      TRUE                              ~ "Neutral"
    )
  )

vk_aliph_tag <- vk_aliph_tag %>%
  mutate(active = (dI_Bio < 0 | dI_Photo < 0 | dI_BioPhoto < 0)) %>%
  filter(active)

synergy_comp_summary <- vk_aliph_tag %>%
  filter(!is.na(interaction)) %>%
  count(Season, interaction, name = "n") %>%
  group_by(Season) %>%
  mutate(
    total = sum(n),
    frac  = n / total
  ) %>%
  ungroup()

synergy_comp_summary


## Comparison of Spring vs Summer raw DOC compound-------

spring <- load_one(file.path(FT_dir,"2024_Spring_Bio_T0.csv"), "Spring")
summer <- load_one(file.path(FT_dir,"2024_Summer_Bio_T0.csv"), "Summer")

bio_T0 <- bind_rows(spring, summer) %>%
  mutate(
    Class = case_when(
      N > 0 & S > 0 ~ "CHONS",
      S > 0         ~ "CHOS",
      N > 0         ~ "CHON",
      TRUE          ~ "CHO"
    )
  )

bio_T0 <- bio_T0 %>%
  mutate(
    O_C = O/C,
    H_C = H/C,
    AImod = AImod,
    dom_group = case_when(
      # 1) Condensed aromatics (CA; combustion-derived); GBC + JGR: AImod > 0.66
      AImod > 0.66 ~ "CA",
      # 2) Polyphenolic-like (PPh; vascular plant–derived); GBC / NC / JGR: 0.5 < AImod ≤ 0.66
      AImod > 0.50 & AImod <= 0.66 ~ "PPh",
      # 3) Highly unsaturated & phenolic (HUPs); GBC / NC: AImod ≤ 0.5, H/C < 1.5, O/C ≤ 0.9
      AImod <= 0.50 & H_C < 1.5 & O_C <= 0.9 ~ "HUPs",
      # 4) Aliphatic-like; GBC / JGR: 1.5 ≤ H/C ≤ 2.0 (and not already assigned above)
      H_C >= 1.5 & H_C <= 2.0 & O_C <= 0.9 ~ "Aliphatics",
      # everything else (sugar-like, peptide-like, weird edge cases…)
      TRUE ~ "Other"
    ),
    dom_group = factor(dom_group, levels = c("Aliphatics", "HUPs", "PPh", "CA", "Other"))
  )

test_class <- bio_T0 %>%
  group_by(Class) %>%
  summarise(
    p = wilcox.test(RelInt ~ Season)$p.value,
    .groups="drop"
  )

test_dom_group <- bio_T0 %>%
  group_by(dom_group) %>%
  summarise(
    p = wilcox.test(RelInt ~ Season)$p.value,
    .groups="drop"
  )

class_summary <- bio_T0 %>%
  group_by(Season, Class) %>%
  summarise(
    mean_int = mean(RelInt, na.rm = TRUE),
    .groups="drop"
  )

stats_class <- test_class %>%
  mutate(
    label = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ "ns"
    ),
    group1 = as.numeric(factor(Class)) - 0.2,   # left bar (Spring)
    group2 = as.numeric(factor(Class)) + 0.2,   # right bar (Summer)
    y.position = max(class_summary$mean_int) * 1.1
  )

p_class_T0 <- ggplot(class_summary, aes(x = Class, y = mean_int, fill = Season)) +
  geom_col(position = position_dodge(width = 0.7), color = "black", width = 0.6) +
  scale_fill_manual(values = c(Spring = "#6BA3C9", Summer = "#D8C27A")) +
  ggpubr::stat_pvalue_manual(
    data       = stats_class,
    label      = "label",
    xmin       = "group1",
    xmax       = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    size       = 4,
    bracket.size = 0.3,
    inherit.aes = FALSE   # <- THIS avoids the 'Season' issue
  ) +
  labs(
    y = "Mean relative abundance",
    x = "Formula class",
    title = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Helvetica Light", face = "plain"),
    panel.grid   = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.ticks   = element_line(color = "black"),
    axis.text    = element_text(color = "black"),
    axis.title   = element_text(color = "black")
  )
p_class_T0

dg_summary <- bio_T0 %>%
  group_by(Season, dom_group) %>%
  summarise(mean_int = mean(RelInt), .groups="drop")

stats_dg <- test_dom_group %>% mutate(
  label = case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ "ns"
  )
)

stats_dg_annot <- stats_dg %>%
  mutate(
    label = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ "ns"
    ),
    group1    = as.numeric(factor(dom_group)) - 0.2,  # Spring bar
    group2    = as.numeric(factor(dom_group)) + 0.2,  # Summer bar
    y.position = max(dg_summary$mean_int, na.rm = TRUE) * 1.1
  )

p_dg_T0 <- ggplot(dg_summary, aes(x = dom_group, y = mean_int, fill = Season)) +
  geom_col(position = position_dodge(width = 0.7), color = "black", width = 0.6) +
  scale_fill_manual(values = c(Spring = "#6BA3C9", Summer = "#D8C27A")) +
  ggpubr::stat_pvalue_manual(
    data       = stats_dg_annot,
    label      = "label",
    xmin       = "group1",
    xmax       = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    size       = 4,
    bracket.size = 0.3,
    inherit.aes = FALSE   # <- important: don't try to use Season etc.
  ) +
  labs(
    y = "Mean relative abundance",
    x = "DOM groups",
    title = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Helvetica Light", face = "plain"),
    panel.grid   = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.ticks   = element_line(color = "black"),
    axis.text    = element_text(color = "black"),
    axis.title   = element_text(color = "black")
  )
p_dg_T0



p_combined <- (p_class_T0 + p_dg_T0) +
  plot_layout(ncol = 1, guides = "collect") &
  theme(legend.position = "bottom", legend.box = "horizontal")

ggsave(file.path(out_dir, "Class_DOMgroup_combinedv2.png"),
       p_combined, width = 7, height = 7, dpi = 600)
ggsave(file.path(out_dir, "Class_DOMgroup_combinedv2.pdf"),
       p_combined, width = 7, height = 7, device = cairo_pdf)

p_vk_T0 <- ggplot(bio_T0, aes(O_C, H_C, color = Season)) +
  geom_point(alpha=0.6, size=1.1) +
  scale_color_manual(values=c(Spring = "#6BA3C9", Summer = "#D8C27A")) +
  facet_wrap(~Season) +
  theme_bw() +
  labs(title="Van Krevelen — Spring vs Summer (Bio T0)")

bio_T0 <- bio_T0 %>%
  mutate(Season = factor(Season, levels = c("Spring", "Summer")))

season_cols <- c(Spring = "#6BA3C9", Summer = "#D8C27A")

p_vk_overlap <- ggplot(bio_T0, aes(x = O_C, y = H_C)) +
  geom_point(aes(color = Season),
             alpha = 0.6, size = 1, stroke = 0.2) +
  scale_color_manual(values = season_cols) +
  labs(
    x = "O/C",
    y = "H/C",
    color = NULL,
    title = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    text          = element_text(family = "Helvetica Light"),
    panel.grid    = element_blank(),
    panel.border  = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.ticks    = element_line(color = "black"),
    axis.text     = element_text(color = "black"),
    axis.title    = element_text(color = "black"),
    legend.position = c(0.85, 0.9)
  ) +
  scale_x_continuous(limits = c(0, 1.5), expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(limits = c(0, 2.5), expand = expansion(mult = c(0.01, 0.01)))

ggsave(file.path(out_dir, "VK_overlap_Spring_Summer_Bio_T0.png"),
       p_vk_overlap, width = 4, height = 4, dpi = 600)
ggsave(file.path(out_dir, "VK_overlap_Spring_Summer_Bio_T0.pdf"),
       p_vk_overlap, width = 4, height = 4, device = cairo_pdf)

## Boxplot for AImod, DBE, NOSC at T0 and T28------
vk_box <- vk_pct_df %>%
  # remove extremely large / erroneous AImod values
  filter(
    is.na(AImod0)  | AImod0  <= 10,
    is.na(AImod28) | AImod28 <= 10
  ) %>%
  select(Season, Method,
         AImod0, AImod28,
         DBE0,  DBE28,
         NOSC0, NOSC28) %>%
  pivot_longer(
    cols = c(AImod0, AImod28, DBE0, DBE28, NOSC0, NOSC28),
    names_to   = c("Metric", "TimePoint"),
    names_pattern = "(AImod|DBE|NOSC)(0|28)",
    values_to  = "value"
  ) %>%
  mutate(
    TimePoint = if_else(TimePoint == "0", "T0", "T28"),
    TimePoint = factor(TimePoint, levels = c("T0", "T28")),
    Metric    = factor(Metric, levels = c("AImod","DBE","NOSC")),
    Season    = factor(Season, levels = c("Spring","Summer")),
    Method    = factor(Method,
                       levels = c("Biodegradation",
                                  "Photodegradation",
                                  "Bio-photodegradation"))
  ) %>%
  filter(!is.na(value))

# Median at T0 and T28 + Δmedian + y-position for labels
# Wilcoxon is sensitive to distribution shifts, not differences in means. Even if the box pair looks identical, the algorithm detects: small consistent changes in quantiles, tiny skew shifts, and slight broadening or narrowing. These produce extremely small p-values with large n. #
eff_stats <- vk_box %>%
  group_by(Metric, Season, Method, TimePoint) %>%
  summarise(med = median(value, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from  = TimePoint,
    values_from = med,
    names_prefix = "med_"
  ) %>%
  mutate(delta = med_T28 - med_T0,
         label = sprintf("\u0394median = %.2f", delta))

metric_ypos <- vk_box %>%
  group_by(Metric) %>%
  summarise(y_pos = max(value, na.rm = TRUE) * 1.05, .groups = "drop")

eff_stats <- eff_stats %>%
  left_join(metric_ypos, by = "Metric")


tp_cols <- c("T0" = "#d9d9d9",  # light grey
             "T28" = "#3182bd") # muted blue


p_vk_violin <- ggplot(
  vk_box,
  aes(x = Method, y = value, fill = TimePoint)
) +
  geom_violin(
    position = position_dodge(width = 0.8),
    trim = TRUE,
    alpha = 0.6,
    color = "black",
    linewidth = 0.2
  ) +
  geom_boxplot(
    width = 0.22,
    position = position_dodge(width = 0.8),
    outlier.shape = NA,
    color = "black",
    alpha = 0.9,
    linewidth = 0.25
  ) +
  stat_summary(
    fun = median,
    geom = "crossbar",
    position = position_dodge(width = 0.8),
    width = 0.4,
    color = "black",
    fatten = 0
  ) +
  facet_grid(
    Metric ~ Season,
    scales = "free_y", switch = "y",
    labeller = labeller(Metric = label_value)   # y-axis strip labels
  ) +
  scale_fill_manual(values = tp_cols, name = NULL) +
  scale_color_manual(values = tp_cols, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  geom_text(
    data = eff_stats,
    aes(x = Method, y = y_pos, label = label),
    inherit.aes = FALSE,
    size = 3,
    fontface = "plain",
    family = "Helvetica Light",
    vjust = 0
  ) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "Helvetica Light", face = "plain"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(color = "black", linewidth = 0.6),
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold"),
    strip.placement = "outside",
    axis.text.x      = element_text(angle = 30, hjust = 1),
    axis.text        = element_text(color = "black"),
    legend.position  = "bottom",
    legend.margin    = margin(t = -5)
  )

p_vk_violin

ggsave(file.path(out_dir, "Violin_AImod_DBE_NOSC.png"),
       p_vk_violin, width = 7, height = 7, dpi = 600)
ggsave(file.path(out_dir, "Violin_AImod_DBE_NOSC.pdf"),
       p_vk_violin, width = 7, height = 7, device = cairo_pdf)



## FT-ICR MS DOM-group + DOM-class summaries------
suppressPackageStartupMessages({
  library(tidyverse)
})

FT_dir  <- "~/Documents/Papers/DOC_degradation/Data/FT"
out_dir <- "~/Documents/Papers/DOC_degradation/Data/Output"

FT_dir  <- path.expand(FT_dir)
out_dir <- path.expand(out_dir)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

### 1) file list
file_names <- c(
  "2024_Spring_Bio_T0.csv",
  "2024_Summer_BioPhoto_T28.csv",
  "2024_Summer_Photo_T28.csv",
  "2024_Summer_BioPhoto_T0.csv",
  "2024_Summer_Photo_T0.csv",
  "2024_Summer_Bio_T28.csv",
  "2024_Summer_Bio_T0.csv",
  "2024_Spring_BioPhoto_T28.csv",
  "2024_Spring_Photo_T28.csv",
  "2024_Spring_BioPhoto_T0.csv",
  "2024_Spring_Photo_T0.csv",
  "2024_Spring_Bio_T28.csv"
)

files <- tibble(
  file = file_names,
  path = file.path(FT_dir, file_names)
)

missing <- files %>% filter(!file.exists(path))
if (nrow(missing) > 0) {
  stop("Missing files:\n", paste0(" - ", missing$path, collapse = "\n"))
}

### helpers
pick_col <- function(df, candidates, required = TRUE) {
  nm <- names(df)
  hit <- intersect(candidates, nm)
  if (length(hit) == 0) {
    if (required) {
      stop(
        "Required column not found. Tried: ",
        paste(candidates, collapse = ", "),
        "\nAvailable columns: ",
        paste(nm, collapse = ", ")
      )
    } else return(NA_character_)
  }
  hit[1]
}

# DOM group rule (exactly as you specified)
assign_dom_group <- function(AImod, H_C) {
  dplyr::case_when(
    AImod > 0.66 ~ "CA",
    AImod > 0.50 & AImod <= 0.66 ~ "PPh",
    AImod <= 0.50 & H_C < 1.5 ~ "HUPs",
    H_C >= 1.5 & H_C <= 2.0 ~ "Aliphatics",
    TRUE ~ "Other"
  )
}

# NEW: elemental DOM class (CHO/CHON/CHOS/CHONS)
assign_dom_class <- function(N, S) {
  # Treat NA as 0 (safer)
  N <- replace_na(N, 0)
  S <- replace_na(S, 0)
  
  dplyr::case_when(
    (N > 0) & (S > 0)  ~ "CHONS-only",
    (N > 0) & (S == 0) ~ "CHON-only",
    (N == 0) & (S > 0) ~ "CHOS-only",
    TRUE               ~ "CHO-only"
  )
}

# Deduplicate by Formula within each file: sum RA; keep first for other metrics
dedup_by_formula <- function(df) {
  df %>%
    group_by(Formula) %>%
    summarise(
      RA    = sum(RA, na.rm = TRUE),
      MW    = dplyr::first(MW),
      NOSC  = dplyr::first(NOSC),
      AImod = dplyr::first(AImod),
      H_C   = dplyr::first(H_C),
      N     = dplyr::first(N),
      S     = dplyr::first(S),
      .groups = "drop"
    )
}

# Parse Season / Method / Time from filename
parse_meta <- function(file) {
  m <- stringr::str_match(file, "^\\d{4}_(Spring|Summer)_(BioPhoto|Bio|Photo)_(T0|T28)\\.csv$")
  if (any(is.na(m))) stop("Filename does not match expected pattern: ", file)
  tibble(Season = m[2], Method = m[3], Time = m[4])
}

###  2) Read + standardize one file
read_one <- function(path) {
  df <- readr::read_csv(path, show_col_types = FALSE)
  
  # Original column names
  fcol    <- pick_col(df, c("sumFormula"))
  racol   <- pick_col(df, c("relativeintens"))
  mwcol   <- pick_col(df, c("avemass"))
  aicol   <- pick_col(df, c("aimod"))
  nosccol <- pick_col(df, c("nosc"))
  hccol   <- pick_col(df, c("HCraito"))
  ncol    <- pick_col(df, c("N"))
  scol    <- pick_col(df, c("S"))
  mzcol   <- pick_col(df, c("ObservedM_z"), required = FALSE)
  
  out <- df %>%
    transmute(
      Formula = as.character(.data[[fcol]]),
      RA      = as.numeric(.data[[racol]]),
      MW      = as.numeric(.data[[mwcol]]),     # sample-level constant (may be overwritten if recalculated)
      NOSC    = as.numeric(.data[[nosccol]]),
      AImod   = as.numeric(.data[[aicol]]),
      H_C     = as.numeric(.data[[hccol]]),
      N       = as.numeric(.data[[ncol]]),
      S       = as.numeric(.data[[scol]]),
      ObservedM_z = if (!is.na(mzcol)) as.numeric(.data[[mzcol]]) else NA_real_
    ) %>%
    filter(!is.na(Formula), Formula != "") %>%
    mutate(RA = replace_na(RA, 0))
  
  # Deduplicate by Formula (sums RA, keeps first for other fields)
  out <- dedup_by_formula(out)
  
  # Re-normalize RA to sum to 100 for this file
  if (sum(out$RA, na.rm = TRUE) > 0) {
    out <- out %>% mutate(RA = RA / sum(RA, na.rm = TRUE) * 100)
  }
  
  # Optional: recalc sample-level average mass from observed m/z
  recalc_avemass <- TRUE
  if (recalc_avemass && any(is.finite(out$ObservedM_z))) {
    mw_recalc <- stats::weighted.mean(out$ObservedM_z, w = out$RA, na.rm = TRUE)
    out <- out %>% mutate(MW = mw_recalc)
  }
  
  # Add dom_group + dom_class
  out %>%
    mutate(
      dom_group = assign_dom_group(AImod, H_C),
      dom_class = assign_dom_class(N, S)
    )
}

### 3) Summarize one file 
summarise_one <- function(df) {
  df <- df %>% filter(is.finite(RA), RA >= 0)
  
  n_formulae <- n_distinct(df$Formula)
  
  # %RA by dom_group
  ra_by_group <- df %>%
    group_by(dom_group) %>%
    summarise(RA_sum = sum(RA, na.rm = TRUE), .groups = "drop") %>%
    mutate(pct_RA = if (sum(RA_sum, na.rm = TRUE) > 0) RA_sum / sum(RA_sum, na.rm = TRUE) * 100 else 0) %>%
    select(dom_group, pct_RA) %>%
    pivot_wider(names_from = dom_group, values_from = pct_RA) %>%
    mutate(
      CA         = coalesce(CA, 0),
      PPh        = coalesce(PPh, 0),
      HUPs       = coalesce(HUPs, 0),
      Aliphatics = coalesce(Aliphatics, 0),
      Other      = coalesce(Other, 0)
    )
  
  # NEW: %RA by dom_class
  ra_by_class <- df %>%
    group_by(dom_class) %>%
    summarise(RA_sum = sum(RA, na.rm = TRUE), .groups = "drop") %>%
    mutate(pct_RA = if (sum(RA_sum, na.rm = TRUE) > 0) RA_sum / sum(RA_sum, na.rm = TRUE) * 100 else 0) %>%
    select(dom_class, pct_RA) %>%
    pivot_wider(names_from = dom_class, values_from = pct_RA) %>%
    mutate(
      `CHO-only`   = coalesce(`CHO-only`, 0),
      `CHON-only`  = coalesce(`CHON-only`, 0),
      `CHOS-only`  = coalesce(`CHOS-only`, 0),
      `CHONS-only` = coalesce(`CHONS-only`, 0)
    )
  
  wmean <- function(x, w) {
    if (all(is.na(x))) return(NA_real_)
    stats::weighted.mean(x, w = w, na.rm = TRUE)
  }
  
  tibble(
    n_formulae = n_formulae,
    avg_MW     = wmean(df$MW,   df$RA),
    avg_NOSC   = wmean(df$NOSC, df$RA),
    avg_AImod  = wmean(df$AImod, df$RA)
  ) %>%
    bind_cols(ra_by_group) %>%
    bind_cols(ra_by_class)
}

### run: per-file summaries with proper object names
summaries <- files %>%
  mutate(meta = map(file, parse_meta)) %>%
  unnest(meta) %>%
  mutate(
    key = paste(Season, Method, Time, sep = "_"),
    data = map(path, read_one),
    summary = map(data, summarise_one)
  )

# Named list objects (one row each): Spring_Bio_T0, ...
summary_list <- summaries %>%
  select(key, summary) %>%
  deframe()

list2env(summary_list, envir = .GlobalEnv)

# Long combined table
summary_long <- summaries %>%
  select(Season, Method, Time, key, summary) %>%
  unnest(summary) %>%
  mutate(
    across(c(CA, PPh, HUPs, Aliphatics, Other,
             `CHO-only`, `CHON-only`, `CHOS-only`, `CHONS-only`), ~ round(.x, 2)),
    across(c(avg_MW, avg_NOSC, avg_AImod), ~ round(.x, 3))
  ) %>%
  arrange(Method, Season, factor(Time, levels = c("T0","T28")))

### 4) Combine into three method tables and export
tab_bio <- summary_long %>%
  filter(Method == "Bio") %>%
  select(Season, Time, n_formulae,
         CA, PPh, HUPs, Aliphatics, Other,
         `CHO-only`, `CHON-only`, `CHOS-only`, `CHONS-only`,
         avg_MW, avg_NOSC, avg_AImod)

tab_photo <- summary_long %>%
  filter(Method == "Photo") %>%
  select(Season, Time, n_formulae,
         CA, PPh, HUPs, Aliphatics, Other,
         `CHO-only`, `CHON-only`, `CHOS-only`, `CHONS-only`,
         avg_MW, avg_NOSC, avg_AImod)

tab_biophoto <- summary_long %>%
  filter(Method == "BioPhoto") %>%
  select(Season, Time, n_formulae,
         CA, PPh, HUPs, Aliphatics, Other,
         `CHO-only`, `CHON-only`, `CHOS-only`, `CHONS-only`,
         avg_MW, avg_NOSC, avg_AImod)

# Export CSVs
readr::write_csv(tab_bio,      file.path(out_dir, "FTICR_DOMgroup_DOMclass_summary_Bio.csv"))
readr::write_csv(tab_photo,    file.path(out_dir, "FTICR_DOMgroup_DOMclass_summary_Photo.csv"))
readr::write_csv(tab_biophoto, file.path(out_dir, "FTICR_DOMgroup_DOMclass_summary_BioPhoto.csv"))

# Optional: export full long table
readr::write_csv(summary_long, file.path(out_dir, "FTICR_DOMgroup_DOMclass_summary_ALL.csv"))

message("Done. Exported to: ", out_dir)
