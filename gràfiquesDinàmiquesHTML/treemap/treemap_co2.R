library(tidyverse)
library(plotly)
library(htmlwidgets)
library(countrycode)

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

pal <- c(
  "Asia" = "#E63946", "America Nord" = "#2A9D8F",
  "Europa" = "#F4A261", "America Llatina" = "#457B9D",
  "Africa" = "#9D4EDD", "Oceania" = "#6A994E"
)

dat <- df |>
  filter(Year == 2019, co2_prod > 0, Code != "OWID_WRL") |>
  mutate(
    co2_mt = co2_prod / 1e6,
    continent = countrycode(Code, "iso3c", "continent"),
    region = countrycode(Code, "iso3c", "region"),
    continent_cat = case_when(
      continent == "Americas" & region == "North America" ~ "America Nord",
      continent == "Americas" ~ "America Llatina",
      continent == "Europe" ~ "Europa",
      continent == "Asia" ~ "Asia",
      continent == "Africa" ~ "Africa",
      continent == "Oceania" ~ "Oceania"
    )
  ) |>
  drop_na(continent_cat)

total <- sum(dat$co2_mt)

# --- CONTINENTS ---
cont <- dat |>
  group_by(continent_cat) |>
  summarise(val = sum(co2_mt), .groups = "drop") |>
  mutate(
    ids = continent_cat,
    parents = "",
    color = pal[continent_cat],
    label = sprintf(
      "<b>%s</b><br>%.2f Gt · %.1f%%",
      continent_cat, val/1000, val / total * 100
    )
  )

# --- COUNTRIES ---
countries <- dat |>
  mutate(
    val = co2_mt,
    ids = paste(continent_cat, Entity),
    parents = continent_cat,
    color = pal[continent_cat],
    pct = co2_mt / total * 100,
    label = case_when(
      pct >= 1.5 ~ sprintf("<b>%s</b><br>%.0f Mt · %.1f%%", Entity, co2_mt, pct),
      pct >= 0.4 ~ sprintf("<b>%s</b> %.1f%%", Entity, pct),
      TRUE ~ ""
    ),
    hover = sprintf("<b>%s</b><br>%.1f Mt · %.2f%%", Entity, co2_mt, pct)
  ) |>
  select(ids, parents, val, color, label, hover)

nodes <- bind_rows(
  cont |> transmute(ids, parents, val, color, label, hover = label),
  countries |> transmute(ids, parents, val, color, label, hover)
)

# --- PLOT ---
fig <- plot_ly(
  type = "treemap",
  ids = nodes$ids,
  labels = nodes$label,
  parents = nodes$parents,
  values = nodes$val,
  text = nodes$hover,
  marker = list(colors = nodes$color, line = list(color = "white", width = 2)),
  hovertemplate = "%{text}<extra></extra>",
  textinfo = "label",
  textfont = list(family = "Arial Black", color = "white"),
  pathbar = list(visible = TRUE),
  tiling = list(packing = "squarify")
) |>
  layout(
    title = paste0("CO2 emissions 2019 — Total: ", round(total/1000, 2), " Gt"),
    margin = list(t = 60)
  )

dir.create("grafics", showWarnings = FALSE)

saveWidget(
  fig,
  "grafics/treemap_co2.html",
  selfcontained = FALSE
)

# TOP 10
dat |>
  arrange(desc(co2_mt)) |>
  mutate(pct = co2_mt / total * 100) |>
  select(Entity, continent_cat, co2_mt, pct) |>
  head(10) |>
  print()
