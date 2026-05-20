library(tidyverse)
library(ggrepel)
library(countrycode)

# ============================================================
# Dades
# ============================================================
df <- read_csv("master_dataset.csv", show_col_types = FALSE)

vars <- c("gdp_pc","hdi","gini","life_exp",
          "co2_prod_pc","pct_lowcarbon",
          "carbon_intensity","exports_share_gdp")

etiq <- c(
  gdp_pc = "PIB per capita",
  hdi = "HDI",
  gini = "Gini",
  life_exp = "Esperança vida",
  co2_prod_pc = "CO2 pc",
  pct_lowcarbon = "% netes",
  carbon_intensity = "Intensitat carboni",
  exports_share_gdp = "Exportacions"
)

# ============================================================
# Preparació dades 2019
# ============================================================
gini_recent <- df |>
  filter(Year >= 2010, !is.na(gini)) |>
  group_by(Code) |>
  slice_max(Year, n = 1) |>
  ungroup() |>
  select(Code, gini)

df2019 <- df |>
  filter(Year == 2019) |>
  select(Code, Entity, all_of(vars)) |>
  left_join(gini_recent, by = "Code") |>
  drop_na()

df2019$continent <- countrycode(df2019$Code, "iso3c", "continent")

df2019 <- df2019 |> filter(!is.na(continent))

# ============================================================
# PCA
# ============================================================
mat <- scale(df2019[, vars])
pca <- prcomp(mat)

scores <- as.data.frame(pca$x[,1:2])
scores$continent <- df2019$continent
scores$Entity <- df2019$Entity

loadings <- as.data.frame(pca$rotation[,1:2])
loadings$variable <- rownames(loadings)
loadings$label <- etiq[loadings$variable]

# Escalat simple per visualització
loadings[,1:2] <- loadings[,1:2] * 4

var_exp <- round(summary(pca)$importance[2,1:2] * 100,1)

# ============================================================
# Plot
# ============================================================
ggplot() +
  geom_point(data = scores,
             aes(PC1, PC2, color = continent), alpha = 0.6) +
  geom_segment(data = loadings,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2,"cm")),
               color = "black") +
  geom_text(data = loadings,
            aes(PC1, PC2, label = label),
            size = 3) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  labs(
    title = "PCA sostenibilitat (2019)",
    x = paste0("PC1 (", var_exp[1], "%)"),
    y = paste0("PC2 (", var_exp[2], "%)")
  )

ggsave("grafics/pca.png", width = 10, height = 7)
