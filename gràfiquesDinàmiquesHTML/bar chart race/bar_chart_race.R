library(tidyverse)
library(plotly)
library(htmlwidgets)
library(countrycode)

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

TOP_N <- 15

# --- Datos base ---
race <- df |>
  filter(
    Year >= 1990, Year <= 2019,
    co2_prod > 0,
    !Code %in% c("OWID_WRL", "OWID_KOS")
  ) |>
  mutate(
    co2_gt = co2_prod / 1e9,
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

# --- Top N per any ---
top <- race |>
  group_by(Year) |>
  slice_max(co2_gt, n = TOP_N) |>
  arrange(Year, desc(co2_gt)) |>
  mutate(
    rank = row_number(),
    y = TOP_N + 1 - rank,
    label = paste(Entity, round(co2_gt, 2), "Gt")
  )

xmax <- max(top$co2_gt) * 1.1

pal <- c(
  Africa = "#E69F00",
  "America Llatina" = "#56B4E9",
  "America Nord" = "#0072B2",
  Asia = "#D55E00",
  Europa = "#009E73",
  Oceania = "#CC79A7"
)

fig <- plot_ly(
  top,
  type = "bar",
  orientation = "h",
  x = ~co2_gt,
  y = ~y,
  frame = ~Year,
  color = ~continent_cat,
  colors = pal,
  text = ~label,
  textposition = "outside",
  ids = ~Code
) |>
  layout(
    title = paste0("Top ", TOP_N, " emisores CO2 (1990–2019)"),
    xaxis = list(range = c(0, xmax)),
    yaxis = list(title = "", showticklabels = FALSE),
    bargap = 0.2
  ) |>
  animation_opts(frame = 600, transition = 300)

dir.create("grafics", showWarnings = FALSE)
saveWidget(fig, "grafics/bar_chart_race.html", selfcontained = TRUE)

# --- Extra simple insight ---
race |>
  filter(Code %in% c("CHN", "USA")) |>
  group_by(Year, Code) |>
  summarise(co2 = sum(co2_gt), .groups = "drop") |>
  pivot_wider(names_from = Code, values_from = co2) |>
  mutate(diff = CHN - USA) |>
  print(n = 20)
