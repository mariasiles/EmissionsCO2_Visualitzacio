# ============================================================
# Bivariate choropleth: CO2 per capita x HDI (any 2019)
# Versio sense 'sf' - usa map_data de ggplot2
# Identifica 4 perfils: occidental | petroliers | sostenibles | pobresa
# ============================================================
library(tidyverse)
library(maps)
library(countrycode)

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

# Filtrar datos 2019
dat <- df |>
  filter(Year == 2019, !is.na(Code), !is.na(co2_prod_pc), !is.na(hdi)) |>
  mutate(
    co2_cat = cut(co2_prod_pc, c(-Inf, 2, 7, Inf), c("baix","mig","alt")),
    hdi_cat = cut(hdi, c(-Inf, 0.65, 0.80, Inf), c("baix","mig","alt")),
    bi_class = paste0(hdi_cat, "_", co2_cat)
  ) |>
  select(Code, bi_class)

# Mapa base
world <- map_data("world") |>
  mutate(iso3 = countrycode(region, "country.name", "iso3c"))

mapa_df <- world |>
  left_join(dat, by = c("iso3" = "Code"))

# Plot mapa
ggplot(mapa_df, aes(long, lat, group = group, fill = bi_class)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  coord_fixed(1.3) +
  scale_fill_brewer(palette = "Set3", na.value = "grey90") +
  theme_void() +
  labs(title = "HDI vs CO2 (2019)")
