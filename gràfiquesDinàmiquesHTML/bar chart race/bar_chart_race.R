library(tidyverse)
library(plotly)
library(htmlwidgets)
library(countrycode)

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

race <- df |>
  filter(Year >= 1990, Year <= 2019, !is.na(Code), co2_prod > 0) |>
  mutate(co2_gt = co2_prod / 1e9, continent = countrycode(Code,"iso3c","continent")) |>
  filter(!is.na(continent)) |>
  group_by(Year) |>
  slice_max(co2_gt, n = 15, with_ties = FALSE) |>
  mutate(rank = row_number()) |>
  ungroup()

fig <- plot_ly(race, type="bar", orientation="h",
               x=~co2_gt, y=~(16-rank), frame=~Year, ids=~Code,
               text=~Entity, textposition="outside", color=~continent) |>
  layout(title="Top 15 emissors CO2 (1990-2019)",
         yaxis=list(showticklabels=FALSE)) |>
  animation_opts(frame=700, transition=400) |>
  animation_slider() |> animation_button(label="Play")

saveWidget(fig, "grafics/bar_chart_race.html", selfcontained=TRUE)
