# install.packages(c("tidyverse", "grid"))
library(tidyverse)
library(grid)

# -----------------------------
# 1. Example data
# Replace this section with your real data
# Required columns:
#   species
#   shannon_ratio
# -----------------------------

set.seed(1)

species_order <- c("EL", "CA", "BU", "PV", "CH", "CS", "PC", "DP", "BT")
n_values <- c(23, 21, 24, 26, 27, 25, 24, 23, 25)

means <- c(0.95, 0.80, 0.55, 0.50, 0.60, 0.55, 0.65, 0.60, 0.45)
sds   <- c(0.32, 0.35, 0.35, 0.36, 0.35, 0.36, 0.35, 0.36, 0.35)

df <- map2_dfr(species_order, seq_along(species_order), function(sp, i) {
  tibble(
    species = sp,
    shannon_ratio = pmax(0, pmin(1.65, rnorm(n_values[i], means[i], sds[i])))
  )
})

df <- df %>%
  mutate(
    species = factor(species, levels = species_order),
    x = as.numeric(species)
  )

# -----------------------------
# 2. Colors
# -----------------------------

pal <- c(
  "EL" = "#d95f76",
  "CA" = "#f26c4f",
  "BU" = "#b7df4a",
  "PV" = "#1b8a3a",
  "CH" = "#e85aa0",
  "CS" = "#e0b34f",
  "PC" = "#e6a63a",
  "DP" = "#a65cc9",
  "BT" = "#4db6a2"
)

# -----------------------------
# 3. Per-community significance stars
# Edit this table for your real annotations
# -----------------------------

star_df <- tribble(
  ~species, ~y,    ~label, ~col,
  "EL",     1.68,  "***",  "#d95f76",
  "EL",     1.80,  "***",  "#1b8a3a",
  "EL",     1.92,  "***",  "#4db6a2",
  "EL",     2.04,  "***",  "#e85aa0",
  "EL",     2.16,  "***",  "#b7df4a",
  
  "CA",     1.68,  "*",    "#e0b34f",
  "CA",     1.82,  "*",    "#4db6a2",
  "CA",     1.96,  "**",   "#1b8a3a",
  "CA",     2.10,  "***",  "#d95f76",
  
  "BU",     1.12,  "**",   "#b7df4a",
  
  "PV",     1.55,  "***",  "#1b8a3a",
  "PV",     1.68,  "***",  "#d95f76",
  
  "CH",     1.28,  "***",  "#e85aa0",
  "CH",     1.42,  "***",  "#e0b34f",
  
  "CS",     1.55,  "*",    "#e0b34f",
  "CS",     1.70,  "**",   "#d95f76",
  
  "PC",     1.35,  "**",   "#e6a63a",
  "PC",     1.50,  "***",  "#d95f76",
  
  "DP",     1.20,  "***",  "#a65cc9",
  
  "BT",     1.62,  "**",   "#4db6a2",
  "BT",     1.75,  "**",   "#d95f76"
) %>%
  mutate(
    species = factor(species, levels = species_order),
    x = as.numeric(species)
  )

# -----------------------------
# 4. Horizontal dashed comparison lines
# Edit xmin/xmax/y/label as needed
# -----------------------------

comp_df <- tribble(
  ~xmin, ~xmax, ~y,    ~label,
  1,     4,     1.92,  "CD-PV-EL",
  2,     6,     1.66,  "CD-CA-CS",
  1,     9,     1.64,  "CD-BT-EL",
  2,     9,     1.43,  "CD-CA-BT"
)

# -----------------------------
# 5. Plot
# -----------------------------

p <- ggplot(df, aes(x = x, y = shannon_ratio)) +
  
  # boxplots
  geom_boxplot(
    aes(fill = species),
    width = 0.62,
    outlier.shape = NA,
    color = "gray45",
    alpha = 0.65,
    linewidth = 0.45
  ) +
  
  # jittered points
  geom_jitter(
    aes(color = species),
    width = 0.16,
    size = 1.8,
    alpha = 0.8
  ) +
  
  # zero reference
  geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
  
  # comparison dashed horizontal lines
  geom_segment(
    data = comp_df,
    aes(x = xmin, xend = xmax, y = y, yend = y),
    inherit.aes = FALSE,
    linetype = "dotted",
    linewidth = 0.45,
    color = "black"
  ) +
  
  # small vertical ends for comparison lines
  geom_segment(
    data = comp_df,
    aes(x = xmin, xend = xmin, y = y - 0.04, yend = y + 0.04),
    inherit.aes = FALSE,
    linetype = "dotted",
    linewidth = 0.45,
    color = "black"
  ) +
  geom_segment(
    data = comp_df,
    aes(x = xmax, xend = xmax, y = y - 0.04, yend = y + 0.04),
    inherit.aes = FALSE,
    linetype = "dotted",
    linewidth = 0.45,
    color = "black"
  ) +
  
  # labels for comparison lines
  geom_text(
    data = comp_df,
    aes(x = (xmin + xmax) / 2, y = y + 0.035, label = label),
    inherit.aes = FALSE,
    size = 3.0,
    color = "black"
  ) +
  
  # significance stars
  geom_text(
    data = star_df,
    aes(x = x, y = y, label = label, color = col),
    inherit.aes = FALSE,
    size = 3.6,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  # right-side double arrow
  annotate(
    "segment",
    x = 10.0, xend = 10.0,
    y = 0.35, yend = 1.60,
    arrow = arrow(
      length = unit(0.13, "inches"),
      ends = "both",
      type = "closed"
    ),
    linewidth = 0.5
  ) +
  
  annotate("text", x = 10.28, y = 1.48, label = "Coexistence", hjust = 0, size = 3.4) +
  annotate("text", x = 10.28, y = 0.98, label = expression(Delta*H), hjust = 0, size = 4.2) +
  annotate("text", x = 10.28, y = 0.48, label = "Competitive\nexclusion", hjust = 0, size = 3.4) +
  
  # n labels below x axis
  annotate(
    "text",
    x = 1:9,
    y = -0.18,
    label = n_values,
    size = 3.0
  ) +
  annotate(
    "text",
    x = 0.35,
    y = -0.18,
    label = "n=",
    size = 3.0,
    hjust = 0
  ) +
  
  # title-like label
  annotate(
    "text",
    x = 2.0,
    y = 2.22,
    label = expression("Coexistence " * (Delta*H)),
    hjust = 0,
    size = 4.2,
    fontface = "bold"
  ) +
  
  # panel letter
  annotate(
    "text",
    x = -0.1,
    y = 2.25,
    label = "E",
    size = 8,
    fontface = "bold"
  ) +
  
  scale_x_continuous(
    breaks = 1:9,
    labels = species_order,
    limits = c(0.5, 10.8)
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 2.0, 0.5),
    limits = c(-0.25, 2.35),
    expand = expansion(mult = c(0, 0))
  ) +
  
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  
  labs(
    x = "Communities containing specific species",
    y = "Shannon at last / initial time point"
  ) +
  
  theme_classic(base_size = 11) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 10, margin = margin(t = 15)),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.line = element_line(linewidth = 0.5, color = "gray30"),
    plot.margin = margin(15, 90, 25, 30)
  ) +
  
  coord_cartesian(clip = "off")

p
