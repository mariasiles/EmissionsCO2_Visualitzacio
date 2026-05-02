library(tidyverse)
library(GGally)
library(countrycode)

set.seed(42)

# ============================================================
# SPLOM (scatter plot matrix) sobre les variables del LDA
# Color = quartil de CO2 per capita 2019
# ============================================================

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

vars <- c("co2_prod_pc","gdp_pc","hdi","life_exp","gini")
nice <- c("co2_prod_pc"="CO2 pc",
          "gdp_pc"="GDP pc",
          "hdi"="HDI",
          "life_exp"="Esp. vida",
          "gini"="Gini")

# Imputacio gini (igual que pca/lda)
gini_recent <- df |>
  filter(Year >= 2010, Year <= 2019, !is.na(gini), !is.na(Code)) |>
  group_by(Code) |>
  slice_max(Year, n = 1, with_ties = FALSE) |>
  ungroup() |>
  dplyr::select(Code, gini_imp = gini)

vars_strict <- setdiff(vars, "gini")
df2019 <- df |>
  filter(Year == 2019, !is.na(Code), !is.na(co2_prod_pc)) |>
  drop_na(all_of(vars_strict), Code) |>
  left_join(gini_recent, by = "Code") |>
  mutate(gini = coalesce(gini, gini_imp)) |>
  dplyr::select(-gini_imp)

# Continent
df2019$region_raw <- countrycode(df2019$Code, "iso3c", "region", warn = FALSE)
map_region <- function(r) {
  if (is.na(r)) return(NA_character_)
  if (r == "Sub-Saharan Africa") return("Africa")
  if (r %in% c("Northern Africa","Middle East & North Africa","Western Asia")) return("Orient Mitja")
  if (r == "Europe & Central Asia") return("Europa")
  if (r %in% c("East Asia & Pacific","South Asia")) return("Asia")
  if (r == "Latin America & Caribbean") return("Am. Llatina")
  if (r == "North America") return("Am. Nord")
  return(NA_character_)
}
df2019$continent <- sapply(df2019$region_raw, map_region)
df2019$continent[df2019$Code %in% c("EGY","LBY","DZA","TUN","MAR","SDN")]  <- "Africa"
df2019$continent[df2019$Code %in% c("USA","CAN","MEX")]                     <- "Am. Nord"
df2019$continent[df2019$Code %in% c("SAU","ARE","QAT","KWT","OMN","BHR",
                                     "IRN","IRQ","ISR","JOR","LBN","SYR","YEM","TUR")] <- "Orient Mitja"

df2019 <- df2019 |>
  group_by(region_raw) |>
  mutate(gini = ifelse(is.na(gini), median(gini, na.rm = TRUE), gini)) |>
  ungroup() |>
  mutate(gini = ifelse(is.na(gini), median(gini, na.rm = TRUE), gini)) |>
  drop_na(all_of(vars))

brks <- quantile(df2019$co2_prod_pc, probs = c(0,.25,.5,.75,1), na.rm = TRUE)
df2019$tier <- cut(df2019$co2_prod_pc, breaks = brks,
                   include.lowest = TRUE,
                   labels = c("Baix","Mig","Alt","Molt alt"))

# Log per skew fort
df2019 <- df2019 |>
  mutate(co2_prod_pc      = log10(co2_prod_pc + 0.01),
         gdp_pc           = log10(gdp_pc),
         carbon_intensity = log10(carbon_intensity + 0.01))

dat <- df2019 |> dplyr::select(all_of(vars), tier)
names(dat)[1:length(vars)] <- nice[vars]

cat("N =", nrow(dat), "\n")

colors_cont <- c("Baix"="#2ECC71","Mig"="#F1C40F","Alt"="#E67E22","Molt alt"="#C0392B")

# ============================================================
# Custom panels per fer-ho mes net
# ============================================================
lower_fn <- function(data, mapping, ...) {
  ggplot(data, mapping) +
    geom_point(alpha = 0.6, size = 1.3) +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE,
                color = "grey20", linewidth = 0.5, linetype = "dashed")
}

diag_fn <- function(data, mapping, ...) {
  ggplot(data, mapping) +
    geom_density(alpha = 0.55, linewidth = 0.4)
}

upper_fn <- function(data, mapping, ...) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  r_glob <- cor(x, y, use = "pairwise.complete.obs")
  grp <- data$tier
  r_by <- tapply(seq_along(x), grp, function(idx)
    cor(x[idx], y[idx], use = "pairwise.complete.obs"))
  txt <- paste0(
    sprintf("global r = %.2f", r_glob), "\n",
    paste(sprintf("%s: %.2f", names(r_by), r_by), collapse = "\n")
  )
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = txt, size = 2.8) +
    theme_void() +
    xlim(0, 1) + ylim(0, 1)
}

p <- ggpairs(
  dat,
  columns = 1:length(vars),
  mapping = aes(color = tier, fill = tier),
  lower   = list(continuous = lower_fn),
  diag    = list(continuous = diag_fn),
  upper   = list(continuous = upper_fn),
  progress = FALSE
) +
  scale_color_manual(values = colors_cont) +
  scale_fill_manual(values  = colors_cont) +
  labs(
    title    = "SPLOM - Relacions parell-a-parell entre variables socioeconomiques i CO2",
    subtitle = paste0("N = ", nrow(dat),
                      " paisos (2019) | color = quartil CO2 per capita | ",
                      "gdp_pc, co2_pc, intens. C en log10"),
    caption  = "Diagonal: densitat per grup. Sota: scatter + recta global. Sobre: correlacio global i per grup."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey30"),
    plot.caption  = element_text(color = "grey40", face = "italic"),
    strip.text    = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

if (!dir.exists("grafics")) dir.create("grafics")
ggsave("grafics/splom.png", plot = p, width = 14, height = 12, dpi = 150)
cat("Grafic guardat a grafics/splom.png\n")
