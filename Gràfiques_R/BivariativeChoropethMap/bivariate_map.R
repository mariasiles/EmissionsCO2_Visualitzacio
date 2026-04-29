# ============================================================
# Bivariate choropleth: CO2 per capita x HDI (any 2019)
# Versio sense 'sf' - usa map_data de ggplot2
# Identifica 4 perfils: occidental | petroliers | sostenibles | pobresa
# ============================================================
library(tidyverse)
library(maps)
library(patchwork)
library(countrycode)

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

dat <- df |>
  filter(Year == 2019, !is.na(Code), !is.na(co2_prod_pc), !is.na(hdi)) |>
  select(Code, Entity, co2_prod_pc, hdi)

cat("Paisos 2019:", nrow(dat), "\n")

# Categoritzem en 3 nivells
dat <- dat |>
  mutate(
    co2_cat = cut(co2_prod_pc,
                  breaks  = c(-Inf, 2, 7, Inf),
                  labels  = c("baix", "mig", "alt")),
    hdi_cat = cut(hdi,
                  breaks  = c(-Inf, 0.65, 0.80, Inf),
                  labels  = c("baix", "mig", "alt")),
    bi_class = paste0(hdi_cat, "_", co2_cat)
  )

# Paleta bivariada 3x3 (Stevens scheme)
bi_pal <- c(
  "baix_baix" = "#e8e8e8",
  "baix_mig"  = "#b8d6be",
  "baix_alt"  = "#73ae80",
  "mig_baix"  = "#b5c0da",
  "mig_mig"   = "#90b2b3",
  "mig_alt"   = "#5a9178",
  "alt_baix"  = "#6c83b5",
  "alt_mig"   = "#567994",
  "alt_alt"   = "#2a5a5b"
)

# --- Mapa ---
world <- map_data("world")
world <- world |>
  mutate(iso3 = countrycode(region, origin = "country.name",
                             destination = "iso3c", warn = FALSE))

mapa_df <- world |>
  left_join(dat, by = c("iso3" = "Code"))

p_mapa <- ggplot(mapa_df,
                 aes(x = long, y = lat, group = group, fill = bi_class)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_manual(values = bi_pal, na.value = "grey92", guide = "none") +
  coord_fixed(1.4, ylim = c(-58, 85)) +
  labs(
    title    = "Perfil de sostenibilitat global (2019)",
    subtitle = "Cada pais segons HDI (desenvolupament) i CO2 per capita (impacte climatic)",
    caption  = "Font: Our World in Data | Bivariate scheme adaptat de Joshua Stevens"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(color = "grey30", size = 10, hjust = 0.5,
                                  margin = margin(b = 10)),
    plot.caption  = element_text(color = "grey50", size = 8, hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA)
  )

# --- Llegenda 3x3 ---
legend_df <- expand.grid(
  hdi_cat = c("baix", "mig", "alt"),
  co2_cat = c("baix", "mig", "alt")
) |>
  mutate(bi_class = paste0(hdi_cat, "_", co2_cat),
         x = as.numeric(hdi_cat),
         y = as.numeric(co2_cat))

p_leg <- ggplot(legend_df, aes(x = x, y = y, fill = bi_class)) +
  geom_tile(color = "white", linewidth = 1) +
  scale_fill_manual(values = bi_pal, guide = "none") +
  scale_x_continuous(breaks = 1:3,
                     labels = c("HDI\nbaix", "HDI\nmig", "HDI\nalt"),
                     expand = c(0, 0), position = "top") +
  scale_y_continuous(breaks = 1:3,
                     labels = c("CO2\nbaix", "CO2\nmig", "CO2\nalt"),
                     expand = c(0, 0)) +
  coord_fixed() +
  labs(title = "Llegenda") +
  theme_minimal(base_size = 9) +
  theme(
    plot.title       = element_text(size = 9, face = "bold", hjust = 0.5),
    axis.title       = element_blank(),
    axis.text        = element_text(color = "grey25", size = 8),
    panel.grid       = element_blank(),
    plot.background  = element_rect(fill = "white", color = "grey80")
  )

final <- p_mapa + inset_element(p_leg, left = 0.01, bottom = 0.02,
                                 right = 0.22, top = 0.32)

dir.create("grafics", showWarnings = FALSE)
ggsave("grafics/bivariate_map.png", final, width = 14, height = 7.5, dpi = 160)
cat("Mapa bivariat guardat a grafics/bivariate_map.png\n")

# Resum
cat("\n--- Distribucio per perfil ---\n")
dat |> count(bi_class, sort = TRUE) |> print(n = Inf)

cat("\n--- ALT HDI + BAIX CO2 (model sostenible) ---\n")
dat |> filter(bi_class == "alt_baix") |> arrange(desc(hdi)) |>
  select(Entity, hdi, co2_prod_pc) |> head(15) |> print()

cat("\n--- ALT HDI + ALT CO2 (model occidental insostenible) ---\n")
dat |> filter(bi_class == "alt_alt") |> arrange(desc(co2_prod_pc)) |>
  select(Entity, hdi, co2_prod_pc) |> head(15) |> print()

cat("\n--- HDI BAIX/MIG + ALT CO2 (petroliers) ---\n")
dat |> filter(bi_class %in% c("baix_alt", "mig_alt")) |>
  arrange(desc(co2_prod_pc)) |>
  select(Entity, hdi, co2_prod_pc) |> head(10) |> print()
