library(tidyverse)
library(ggrepel)
library(countrycode)

# ============================================================
# Carrega dades reals del master_dataset
# ============================================================
df <- read_csv("master_dataset.csv", show_col_types = FALSE)

vars <- c("gdp_pc","hdi","gini","life_exp",
          "co2_prod_pc",
          "pct_lowcarbon","carbon_intensity","exports_share_gdp")

# Etiquetes llegibles
etiq <- c(
  "gdp_pc"            = "PIB per capita",
  "hdi"               = "Des. Huma (HDI)",
  "gini"              = "Desigualtat (Gini)",
  "life_exp"          = "Esperanca vida",
  "co2_prod_pc"       = "CO2 emissions pc",
  "pct_lowcarbon"     = "% Energies netes",
  "carbon_intensity"  = "Intensitat carboni",
  "exports_share_gdp" = "Exportacions"
)

# --- Per al Gini, agafem ultim valor disponible per pais (2010-2019) ---
gini_recent <- df |>
  filter(Year >= 2010, Year <= 2019, !is.na(gini), !is.na(Code)) |>
  group_by(Code) |>
  slice_max(Year, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(Code, gini_imp = gini)

# --- Filtrem any 2019 i seleccionem variables completes ---
vars_strict <- setdiff(vars, "gini")  # Gini el tractem a part
df2019 <- df |>
  filter(Year == 2019, !is.na(Code)) |>
  drop_na(all_of(vars_strict), Code) |>
  left_join(gini_recent, by = "Code") |>
  mutate(gini = coalesce(gini, gini_imp)) |>
  select(-gini_imp)

# --- Mapeig Code -> continent (6 regions custom) ---
df2019$region_raw <- countrycode(df2019$Code, "iso3c", "region",
                                  warn = FALSE)
df2019 <- df2019 |> filter(!is.na(region_raw))

map_region <- function(r, code) {
  if (r %in% c("Sub-Saharan Africa")) return("Africa")
  if (r %in% c("Northern Africa","Middle East & North Africa","Western Asia")) return("Orient Mitja")
  if (r %in% c("Europe & Central Asia") || code %in% c("RUS","UKR","BLR","MDA","TUR")) return("Europa")
  if (r == "North America" || code %in% c("USA","CAN")) return("America Nord")
  if (r %in% c("Latin America & Caribbean")) return("America Llatina")
  return("Asia")
}
df2019$continent <- mapply(map_region, df2019$region_raw, df2019$Code)

# Reclassificacio fina (paisos especifics)
df2019$continent[df2019$Code %in% c("EGY","LBY","DZA","TUN","MAR","SDN")] <- "Africa"
df2019$continent[df2019$Code %in% c("SAU","ARE","QAT","KWT","OMN","BHR","IRN","IRQ","ISR","JOR","LBN","SYR","YEM","TUR")] <- "Orient Mitja"
df2019$continent[df2019$Code %in% c("USA","CAN","MEX")] <- "America Nord"

# --- Treiem Orient Mitja per claredat ---
df2019 <- df2019 |> filter(continent != "Orient Mitja")

# --- Imputacio Gini: mediana del continent per als que encara no en tenen ---
df2019 <- df2019 |>
  group_by(continent) |>
  mutate(gini = ifelse(is.na(gini), median(gini, na.rm = TRUE), gini)) |>
  ungroup()
# Si algun continent no te cap Gini, omplim amb mediana global
df2019$gini[is.na(df2019$gini)] <- median(df2019$gini, na.rm = TRUE)

cat("Paisos amb dades completes:", nrow(df2019), "\n")
print(table(df2019$continent))

# --- PCA ---
mat <- scale(df2019[, vars])
pca <- prcomp(mat, center = FALSE, scale. = FALSE)
var_exp <- round(summary(pca)$importance[2, 1:4] * 100, 1)
cat("PC1 =", var_exp[1], "%, PC2 =", var_exp[2], "%, total =", var_exp[1]+var_exp[2], "%\n")

# --- Scores i loadings ---
score_scale  <- 1.0
scale_factor <- 4.5
scores <- as.data.frame(pca$x[, 1:2] * score_scale)
names(scores) <- c("PC1","PC2")
scores$continent <- df2019$continent
scores$Entity    <- df2019$Entity

loadings <- as.data.frame(pca$rotation[, 1:2] * scale_factor)
names(loadings) <- c("PC1","PC2")
loadings$variable <- rownames(pca$rotation)
loadings$label    <- etiq[loadings$variable]

# --- Correccio de signe ---
if (mean(scores$PC1[scores$continent == "Europa"], na.rm = TRUE) < 0) {
  scores$PC1   <- -scores$PC1
  loadings$PC1 <- -loadings$PC1
}
if (mean(scores$PC2[scores$continent == "America Llatina"], na.rm = TRUE) > 0) {
  scores$PC2   <- -scores$PC2
  loadings$PC2 <- -loadings$PC2
}

# --- Detectar i separar fletxes superposades (jitter angular nomes visual) ---
# Les fletxes molt correlacionades (PIB, HDI, Esperança vida, % Energies netes)
# apunten quasi al mateix angle. Aplico un petit ventall (+/- pocs graus)
# sobre la posicio de la fletxa per fer-les visualment distingibles.
# IMPORTANT: aixo nomes afecta la presentacio del biplot, no el PCA real.
rotate_xy <- function(x, y, deg) {
  th <- deg * pi / 180
  list(x = x * cos(th) - y * sin(th),
       y = x * sin(th) + y * cos(th))
}
angle_offset <- c(
  "gdp_pc"           = 11,
  "hdi"              =  0,
  "life_exp"         = -10,
  "pct_lowcarbon"    =  5,
  "co2_prod_pc"      =  0,
  "carbon_intensity" =  0,
  "exports_share_gdp"= 18,
  "gini"             =  0
)
for (v in names(angle_offset)) {
  i <- which(loadings$variable == v)
  if (length(i) == 1 && angle_offset[v] != 0) {
    r <- rotate_xy(loadings$PC1[i], loadings$PC2[i], angle_offset[v])
    loadings$PC1[i] <- r$x
    loadings$PC2[i] <- r$y
  }
}

# --- Colors ---
colors_cont <- c(
  "Africa"          = "#27AE60",
  "Europa"          = "#2980B9",
  "Asia"            = "#E67E22",
  "America Nord"    = "#C0392B",
  "America Llatina" = "#8E44AD"
)
scores <- scores |> filter(continent %in% names(colors_cont))

# --- Centroides ---
centroids <- aggregate(cbind(PC1, PC2) ~ continent, data = scores, FUN = mean)

# --- El.lipse manual per America Nord (nomes 3 paisos: USA, CAN, MEX,
# stat_ellipse no la dibuixa amb < 4 punts). Mateixa parametrizacio que
# stat_ellipse type="norm" (chi-quadrat 2 g.l. al nivell 0.80).
ellipse_norm <- function(pts, level = 0.80, n = 100, reg = 0) {
  mu <- colMeans(pts)
  S  <- cov(pts)
  if (reg > 0) S <- S + diag(reg, 2)   # regularitzacio: evita el.lipse degenerada
  if (any(!is.finite(S)) || det(S) <= 0) return(NULL)
  rad <- sqrt(qchisq(level, df = 2))
  th  <- seq(0, 2 * pi, length.out = n)
  circ <- cbind(cos(th), sin(th)) * rad
  ev  <- eigen(S)
  A   <- ev$vectors %*% diag(sqrt(ev$values))
  ell <- t(A %*% t(circ)) + matrix(mu, n, 2, byrow = TRUE)
  data.frame(PC1 = ell[,1], PC2 = ell[,2])
}
amn_ell <- NULL
amn_pts <- scores[scores$continent == "America Nord", c("PC1","PC2")]
if (nrow(amn_pts) >= 2) {
  # Amb nomes 3 paisos (USA, CAN, MEX) la cov surt aplanada;
  # apliquem regularitzacio per obtenir una el.lipse de mida raonable
  # centrada al centroide real (no es desplaca, nomes es regularitza la forma).
  amn_ell <- ellipse_norm(as.matrix(amn_pts), level = 0.80, reg = 0.6)
  if (!is.null(amn_ell)) amn_ell$continent <- "America Nord"
}

# --- Plot ---
p <- ggplot() +
  geom_hline(yintercept = 0, color = "grey78", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey78", linewidth = 0.4) +
  # El.lipses de confianca per continent (forma natural de les dades)
  stat_ellipse(
    data = scores,
    aes(x = PC1, y = PC2, fill = continent, color = continent),
    type = "norm", level = 0.80,
    geom = "polygon", alpha = 0.15, linewidth = 0.9, linetype = "dashed"
  ) +
  { if (!is.null(amn_ell)) geom_polygon(
      data = amn_ell,
      aes(x = PC1, y = PC2, fill = continent, color = continent),
      alpha = 0.15, linewidth = 0.9, linetype = "dashed"
    ) } +
  geom_point(
    data = scores,
    aes(x = PC1, y = PC2, color = continent),
    alpha = 0.55, size = 1.6
  ) +
  geom_segment(
    data = loadings,
    aes(x = 0, y = 0, xend = PC1, yend = PC2),
    arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
    color = "grey15", linewidth = 0.6, alpha = 0.9
  ) +
  geom_text(
    data          = loadings,
    aes(x = PC1, y = PC2, label = label,
        hjust = ifelse(PC1 >= 0, -0.08, 1.08),
        vjust = ifelse(PC2 >= 0, -0.3, 1.3)),
    color         = "black",
    size          = 3.6,
    fontface      = "bold"
  ) +
  scale_color_manual(values = colors_cont, name = "Continent") +
  scale_fill_manual(values  = colors_cont, name = "Continent") +
  labs(
    title    = "Biplot PCA - Perfils de sostenibilitat mundial (2019)",
    subtitle = paste0("PC1 (", var_exp[1], "%) + PC2 (", var_exp[2], "%) = ",
                      var_exp[1] + var_exp[2], "% de la variancia total  |  ",
                      "Dades reals: master_dataset (any 2019)"),
    x = paste0("<-- Menys desenvolupat   |   PC1 (", var_exp[1], "%)   |   Mes desenvolupat -->"),
    y = paste0("<-- Energia neta   |   PC2 (", var_exp[2], "%)   |   Alta emissio / carboni -->")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "right",
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(color = "grey35", size = 8.5),
    axis.title       = element_text(size = 9, color = "grey30"),
    panel.grid.major = element_line(color = "grey93"),
    panel.grid.minor = element_blank()
  )

ggsave("grafics/pca_biplot.png", plot = p, width = 12, height = 8, dpi = 150)
cat("Grafic guardat a grafics/pca_biplot.png\n")
