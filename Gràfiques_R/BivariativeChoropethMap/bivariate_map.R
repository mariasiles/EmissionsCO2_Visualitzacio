library(tidyverse)
library(countrycode)

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

bi_pal <- c(baix_baix="#e8e8e8", baix_mig="#b8d6be", baix_alt="#73ae80",
            mig_baix="#b5c0da", mig_mig="#90b2b3", mig_alt="#5a9178",
            alt_baix="#6c83b5", alt_mig="#567994", alt_alt="#2a5a5b")

dat <- df |>
  filter(Year==2019, !is.na(Code), !is.na(co2_prod_pc), !is.na(hdi)) |>
  mutate(co2_cat = cut(co2_prod_pc, c(-Inf,2,7,Inf), c("baix","mig","alt")),
         hdi_cat = cut(hdi, c(-Inf,.65,.80,Inf), c("baix","mig","alt")),
         bi_class = paste0(hdi_cat,"_",co2_cat))

map_data("world") |>
  mutate(iso3 = countrycode(region,"country.name","iso3c", warn=FALSE)) |>
  left_join(dat |> select(Code, bi_class), by=c("iso3"="Code")) |>
  ggplot(aes(long, lat, group=group, fill=bi_class)) +
  geom_polygon(color="white", linewidth=0.1) +
  scale_fill_manual(values=bi_pal, na.value="grey92", guide="none") +
  coord_fixed(1.4, ylim=c(-58,85)) +
  labs(title="HDI x CO2 per capita (2019)") +
  theme_void()

ggsave("grafics/bivariate_map.png", width=14, height=7, dpi=150, bg="white")
