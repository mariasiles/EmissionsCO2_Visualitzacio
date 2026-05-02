library(tidyverse)
library(ggcorrplot)

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

regions <- c("World","Europe","Asia","Africa","North America",
             "South America","Oceania","European Union",
             "High-income countries","Low-income countries","Middle-income countries")

dades <- df |>
  filter(Year == 2019, !Entity %in% regions) |>
  select(gdp_pc, hdi, gini, life_exp, co2_prod_pc, exports_share_gdp) |>
  drop_na()

names(dades) <- c("PIB pc","HDI","Gini","Esp. Vida","CO2 pc","Export/PIB")

mat_cor <- cor(dades, method = "spearman", use = "pairwise.complete.obs")

p <- ggcorrplot(
  mat_cor,
  type     = "lower",
  lab      = TRUE,
  lab_size = 4,
  digits   = 2,
  colors   = c("#C0392B", "white", "#2980B9"),
  outline.color = "white",
  tl.cex   = 11,
  tl.col   = "grey20"
) +
  labs(
    title   = "Correlograma de Spearman entre variables (2019)",
    caption = "Triangle inferior. Valors: coeficient r de Spearman."
  ) +
  theme(
    plot.title   = element_text(face = "bold", size = 13),
    plot.caption = element_text(color = "grey40", face = "italic")
  )

if (!dir.exists("grafics")) dir.create("grafics")
ggsave("grafics/correlograma.png", plot = p, width = 8, height = 7, dpi = 150)
cat("Grafic guardat a grafics/correlograma.png\n")
