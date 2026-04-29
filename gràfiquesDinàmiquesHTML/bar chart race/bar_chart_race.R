# ============================================================
# Bar chart race: Top 15 paisos emissors de CO2 (1990-2019)
# Format viral tipus "Flourish race"
# Sortida: HTML interactiu amb slider/play
# ============================================================
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(countrycode)

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

TOP_N <- 15

# --- Preparem dades: rang per any ---
race <- df |>
  filter(Year >= 1990, Year <= 2019,
         !is.na(Code), !is.na(co2_prod), co2_prod > 0,
         !Code %in% c("OWID_WRL", "OWID_KOS")) |>
  mutate(
    co2_gt    = co2_prod / 1e9,
    continent = countrycode(Code, origin = "iso3c", destination = "continent"),
    region    = countrycode(Code, origin = "iso3c", destination = "region"),
    Year      = as.integer(Year)
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

# Top N per any segons emissions
race_top <- race |>
  group_by(Year) |>
  slice_max(co2_gt, n = TOP_N, with_ties = FALSE) |>
  arrange(Year, desc(co2_gt)) |>
  mutate(rank = row_number(),
         label = paste0(Entity, "  ", round(co2_gt, 2), " Gt")) |>
  ungroup()

cat("Anys:", min(race_top$Year), "-", max(race_top$Year),
    " | Paisos unics al top:", n_distinct(race_top$Code), "\n")

# --- Paleta per continent ---
pal <- c(
  "Africa"          = "#E69F00",
  "America Llatina" = "#56B4E9",
  "America Nord"    = "#0072B2",
  "Asia"            = "#D55E00",
  "Europa"          = "#009E73",
  "Oceania"         = "#CC79A7"
)

# Reordenem perque rank=1 quedi a dalt (y invertit)
race_top <- race_top |>
  mutate(y_pos = TOP_N + 1 - rank)  # rank 1 -> y=15 (dalt)

xmax <- max(race_top$co2_gt) * 1.15

# --- Bar chart race ---
fig <- plot_ly(
  data       = race_top,
  type       = "bar",
  orientation = "h",
  x          = ~co2_gt,
  y          = ~y_pos,
  frame      = ~Year,
  ids        = ~Code,
  text       = ~label,
  textposition = "outside",
  textfont   = list(size = 13, color = "#222"),
  hovertemplate = paste0(
    "<b>%{customdata}</b><br>",
    "Emissions: %{x:.2f} Gt CO2<br>",
    "Rang: #%{meta}<extra></extra>"
  ),
  customdata = ~Entity,
  meta       = ~rank,
  color      = ~continent_cat,
  colors     = pal,
  marker     = list(line = list(color = "white", width = 1))
) |>
  layout(
    title = list(
      text = paste0("<b>Top ", TOP_N, " paisos emissors de CO2 (1990-2019)</b>",
                    "<br><sub>Bar chart race | Mira com la Xina avanca als EUA cap al 2006</sub>"),
      x = 0.5, xanchor = "center"
    ),
    xaxis = list(
      title         = "Emissions de CO2 (Gigatones / any)",
      range         = c(0, xmax),
      gridcolor     = "grey85",
      zerolinecolor = "grey60"
    ),
    yaxis = list(
      title         = "",
      tickmode      = "array",
      tickvals      = 1:TOP_N,
      ticktext      = rep("", TOP_N),
      showgrid      = FALSE,
      range         = c(0.3, TOP_N + 0.7)
    ),
    showlegend    = TRUE,
    legend        = list(title = list(text = "<b>Continent</b>"),
                         bgcolor = "rgba(255,255,255,0.85)"),
    paper_bgcolor = "white",
    plot_bgcolor  = "#fafafa",
    margin        = list(t = 90, b = 60, l = 30, r = 30),
    bargap        = 0.18
  ) |>
  animation_opts(
    frame      = 700,
    transition = 400,
    redraw     = TRUE,
    easing     = "cubic-in-out"
  ) |>
  animation_slider(
    currentvalue = list(
      prefix = "Any: ",
      font   = list(size = 22, color = "#1a1a1a"),
      xanchor = "right"
    )
  ) |>
  animation_button(label = "Play")

# --- Guardar ---
dir.create("grafics", showWarnings = FALSE)
out_path <- "grafics/bar_chart_race.html"
saveWidget(fig, file = out_path, selfcontained = TRUE,
           title = "Bar chart race - CO2 1990-2019")

cat("Bar chart race guardat a", out_path, "\n")

# Resum: any en que Xina supera EUA
xina_eua <- race |>
  filter(Code %in% c("CHN", "USA")) |>
  select(Year, Code, co2_gt) |>
  pivot_wider(names_from = Code, values_from = co2_gt) |>
  mutate(diff = CHN - USA)
print(xina_eua, n = 30)
