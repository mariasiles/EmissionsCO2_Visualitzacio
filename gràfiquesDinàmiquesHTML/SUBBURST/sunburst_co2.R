# ============================================================
# Sunburst jerarquic: Continent -> Pais (top emissors 2019)
# Visualitza la PROPORCIO d'emissions globals
# Es pot fer clic per fer drill-down
# Sortida: HTML interactiu
# ============================================================
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(countrycode)

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

# Top emissors 2019
sb <- df |>
  filter(Year == 2019, !is.na(Code), !is.na(co2_prod), co2_prod > 0,
         !Code %in% c("OWID_WRL")) |>
  mutate(
    co2_gt    = co2_prod / 1e9,
    continent = countrycode(Code, origin = "iso3c", destination = "continent"),
    region    = countrycode(Code, origin = "iso3c", destination = "region")
  ) |>
  filter(!is.na(continent)) |>
  mutate(
    continent_cat = case_when(
      continent == "Americas" & region == "North America" ~ "America Nord",
      continent == "Americas" ~ "America Llatina",
      continent == "Europe"   ~ "Europa",
      continent == "Asia"     ~ "Asia",
      continent == "Africa"   ~ "Africa",
      continent == "Oceania"  ~ "Oceania"
    )
  )

# Agreguem paisos petits ("Altres") per evitar saturacio
threshold <- 0.05  # Gt
sb <- sb |>
  group_by(continent_cat) |>
  mutate(group_label = ifelse(co2_gt >= threshold, Entity,
                               paste0("Altres ", continent_cat))) |>
  ungroup() |>
  group_by(continent_cat, group_label) |>
  summarise(co2_gt = sum(co2_gt), .groups = "drop")

# --- Construim taula jerarquica per sunburst ---
# Nivell 0: total mon
# Nivell 1: continents
# Nivell 2: paisos (o "Altres")

total_mon <- sum(sb$co2_gt)

continents_df <- sb |>
  group_by(continent_cat) |>
  summarise(co2_gt = sum(co2_gt), .groups = "drop")

# Format Plotly sunburst
nodes <- bind_rows(
  tibble(
    ids     = "Mon",
    labels  = paste0("Mon\n", round(total_mon, 1), " Gt"),
    parents = "",
    values  = total_mon
  ),
  continents_df |> transmute(
    ids     = continent_cat,
    labels  = paste0(continent_cat, "\n", round(co2_gt, 1), " Gt"),
    parents = "Mon",
    values  = co2_gt
  ),
  sb |> transmute(
    ids     = paste0(continent_cat, "_", group_label),
    labels  = paste0(group_label, "\n", round(co2_gt, 2), " Gt"),
    parents = continent_cat,
    values  = co2_gt
  )
)

# Colors per continent (heredats pels seus paisos)
pal_cont <- c(
  "Africa"          = "#E69F00",
  "America Llatina" = "#56B4E9",
  "America Nord"    = "#0072B2",
  "Asia"            = "#D55E00",
  "Europa"          = "#009E73",
  "Oceania"         = "#CC79A7"
)

# Assignem color a cada node segons continent del seu pare
nodes <- nodes |>
  mutate(
    cont = case_when(
      ids == "Mon" ~ "Mon",
      ids %in% names(pal_cont) ~ ids,
      TRUE ~ str_extract(ids, paste(names(pal_cont), collapse = "|"))
    ),
    color = case_when(
      ids == "Mon" ~ "#444444",
      TRUE ~ unname(pal_cont[cont])
    )
  )

cat("Nodes totals:", nrow(nodes),
    " | Continents:", n_distinct(continents_df$continent_cat),
    " | CO2 mundial 2019:", round(total_mon, 2), "Gt\n")

# --- Sunburst ---
fig <- plot_ly(
  type    = "sunburst",
  ids     = nodes$ids,
  labels  = nodes$labels,
  parents = nodes$parents,
  values  = nodes$values,
  branchvalues = "total",
  marker  = list(colors = nodes$color, line = list(color = "white", width = 1.5)),
  textfont = list(size = 14, color = "white", family = "Arial Black"),
  hovertemplate = "<b>%{label}</b><br>%{value:.2f} Gt CO2<br>%{percentParent:.1%} del seu grup<extra></extra>",
  insidetextorientation = "radial"
) |>
  layout(
    title = list(
      text = "<b>Distribucio mundial d'emissions de CO2 (2019)</b><br><sub>Clica un continent per fer drill-down | Total: 35.84 Gt</sub>",
      x = 0.5, xanchor = "center"
    ),
    margin = list(t = 90, b = 20, l = 20, r = 20),
    paper_bgcolor = "white"
  )

dir.create("grafics", showWarnings = FALSE)
out_path <- "grafics/sunburst_co2.html"
saveWidget(fig, file = out_path, selfcontained = FALSE,
           title = "Sunburst CO2 mundial 2019",
           libdir = "sunburst_co2_files")

cat("Sunburst guardat a", out_path, "\n")

# Resum top 10 paisos
cat("\n--- Top 10 emissors 2019 ---\n")
sb |>
  filter(!str_starts(group_label, "Altres")) |>
  arrange(desc(co2_gt)) |>
  mutate(pct_mundial = round(co2_gt / total_mon * 100, 1)) |>
  head(10) |>
  print()
