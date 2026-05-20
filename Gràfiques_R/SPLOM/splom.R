library(tidyverse)
library(GGally)
library(countrycode)

set.seed(42)

# ============================================================
# Dades
# ============================================================
df <- read_csv("master_dataset.csv", show_col_types = FALSE)

vars <- c("co2_prod_pc","gdp_pc","hdi","life_exp","gini")

nice <- c(
  co2_prod_pc = "CO2 pc",
  gdp_pc = "GDP pc",
  hdi = "HDI",
  life_exp = "Esp vida",
  gini = "Gini"
)

# ============================================================
# Gini (últim valor disponible)
# ============================================================
gini_recent <- df |>
  filter(Year >= 2010, !is.na(gini)) |>
  group_by(Code) |>
  slice_max(Year, n = 1) |>
  ungroup() |>
  select(Code, gini)

# ============================================================
# Dataset 2019 net
# ============================================================
df2019 <- df |>
  filter(Year == 2019) |>
  select(Code, all_of(vars)) |>
  drop_na(co2_prod_pc, gdp_pc, hdi, life_exp) |>
  left_join(gini_recent, by = "Code") |>
  mutate(gini = coalesce(gini, median(gini, na.rm = TRUE)))

# ============================================================
# Grup CO2 (quartils)
# ============================================================
df2019$tier <- cut(
  df2019$co2_prod_pc,
  breaks = quantile(df2019$co2_prod_pc, na.rm = TRUE),
  labels = c("Baix","Mig","Alt","Molt alt"),
  include.lowest = TRUE
)

# ============================================================
# Log per estabilitzar distribucions
# ============================================================
df2019 <- df2019 |>
  mutate(
    co2_prod_pc = log10(co2_prod_pc + 1),
    gdp_pc = log10(gdp_pc)
  )

dat <- df2019 |> select(all_of(vars), tier)
names(dat)[1:length(vars)] <- nice[vars]

# ============================================================
# SPLOM simple
# ============================================================
colors <- c("Baix"="#2ECC71","Mig"="#F1C40F",
            "Alt"="#E67E22","Molt alt"="#C0392B")

p <- ggpairs(
  dat,
  columns = 1:length(vars),
  aes(color = tier),
  lower = list(continuous = wrap("smooth_lm", se = FALSE)),
  diag  = list(continuous = "densityDiag"),
  upper = list(continuous = wrap("cor", size = 3))
) +
  scale_color_manual(values = colors) +
  theme_minimal() +
  labs(title = "SPLOM - relacions entre variables (2019)")

ggsave("grafics/splom.png", p, width = 12, height = 10, dpi = 150)
