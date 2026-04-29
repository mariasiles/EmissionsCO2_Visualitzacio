# ============================================================
# Diverging Bar Chart ESTÀTIC: Canvi CO2 per càpita 2000 vs 2019
# Sortida: PNG amb ggplot2
# ============================================================
library(tidyverse)
library(countrycode)

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

YEAR_INI <- 2000
YEAR_FI  <- 2019
N_PER_COSTAT <- 18

# Emissions per càpita als dos anys
d <- df |>
  filter(Year %in% c(YEAR_INI, YEAR_FI),
         !is.na(Code), !is.na(co2_prod_pc), co2_prod_pc > 0,
         !Code %in% c("OWID_WRL","OWID_KOS","OWID_EUR","OWID_AFR",
                      "OWID_ASI","OWID_NAM","OWID_OCE","OWID_SAM")) |>
  select(Entity, Code, Year, co2_prod_pc) |>
  pivot_wider(names_from = Year, values_from = co2_prod_pc,
              names_prefix = "y") |>
  filter(!is.na(y2000), !is.na(y2019)) |>
  mutate(
    canvi_abs = y2019 - y2000,
    canvi_pct = (y2019 - y2000) / y2000 * 100,
    continent = countrycode(Code, "iso3c", "continent")
  ) |>
  filter(!is.na(continent))

# Top 18 que MÉS redueixen (absolut) + Top 18 que MÉS augmenten (absolut)
top_up   <- d |> slice_max(canvi_abs, n = N_PER_COSTAT)
top_down <- d |> slice_min(canvi_abs, n = N_PER_COSTAT)

plot_d <- bind_rows(top_up, top_down) |>
  arrange(canvi_abs) |>
  mutate(
    Entity   = factor(Entity, levels = unique(Entity)),
    grup     = ifelse(canvi_abs < 0, "Redueix", "Augmenta"),
    label    = paste0(ifelse(canvi_abs >= 0, "+", ""),
                      sprintf("%.1f", canvi_abs), " t")
  )

# Gràfic
p <- ggplot(plot_d, aes(x = canvi_abs, y = Entity, fill = grup)) +
  geom_col(width = 0.75, color = "white", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "#222", linewidth = 0.6) +
  geom_text(aes(label = label,
                hjust = ifelse(canvi_abs >= 0, -0.15, 1.15)),
            size = 3.2, fontface = "bold", color = "#222") +
  scale_fill_manual(values = c("Redueix" = "#2ECC71",
                                "Augmenta" = "#E74C3C")) +
  scale_x_continuous(
    labels   = function(x) paste0(ifelse(x >= 0, "+", ""), x, " t"),
    expand   = expansion(mult = c(0.20, 0.20))
  ) +
  labs(
    title    = "Qui ha reduït i qui ha disparat les seves emissions?",
    subtitle = paste0("Diferència absoluta de CO₂ per càpita entre ",
                      YEAR_INI, " i ", YEAR_FI,
                      " · tones per persona · ", nrow(plot_d), " països més extrems"),
    x        = "Canvi en tones CO₂ per càpita",
    y        = NULL,
    caption  = "Font: master_dataset · Verd = redueix · Vermell = augmenta"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 15,
                                     margin = margin(b = 4)),
    plot.subtitle    = element_text(color = "#555", size = 10,
                                     margin = margin(b = 14)),
    plot.caption     = element_text(color = "#888", size = 8,
                                     margin = margin(t = 10)),
    axis.text.y      = element_text(size = 9.5, color = "#222"),
    axis.text.x      = element_text(color = "#555"),
    axis.title.x     = element_text(margin = margin(t = 8), color = "#555"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "#eaeaea"),
    legend.position    = "none",
    plot.margin        = margin(15, 25, 12, 10),
    plot.background    = element_rect(fill = "white", color = NA)
  )

# Guardar
dir.create("grafics", showWarnings = FALSE)
out_path <- "grafics/diverging_bar.png"
ggsave(out_path, p, width = 10, height = 9, dpi = 300, bg = "white")
cat("Diverging bar PNG guardat a", out_path, "\n")
