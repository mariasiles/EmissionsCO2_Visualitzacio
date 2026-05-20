library(tidyverse)
library(plotly)
library(htmlwidgets)
library(countrycode)

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

sb <- df |>
  filter(Year==2019, !is.na(Code), co2_prod>0, Code!="OWID_WRL") |>
  mutate(co2_gt = co2_prod/1e9,
         cont = countrycode(Code,"iso3c","continent"),
         reg  = countrycode(Code,"iso3c","region")) |>
  filter(!is.na(cont)) |>
  mutate(continent = case_when(
    cont=="Americas" & reg=="North America" ~ "America Nord",
    cont=="Americas" ~ "America Llatina", TRUE ~ cont)) |>
  group_by(continent) |>
  mutate(label = ifelse(co2_gt>=0.05, Entity, paste0("Altres ",continent))) |>
  group_by(continent, label) |>
  summarise(co2_gt=sum(co2_gt), .groups="drop")

cont <- sb |> group_by(continent) |> summarise(co2_gt=sum(co2_gt))
total <- sum(sb$co2_gt)

nodes <- bind_rows(
  tibble(ids="Mon", labels=paste0("Mon\n",round(total,1)," Gt"), parents="", values=total),
  cont |> transmute(ids=continent, labels=paste0(continent,"\n",round(co2_gt,1)," Gt"), parents="Mon", values=co2_gt),
  sb   |> transmute(ids=paste0(continent,"_",label), labels=label, parents=continent, values=co2_gt)
)

plot_ly(type="sunburst", ids=nodes$ids, labels=nodes$labels,
        parents=nodes$parents, values=nodes$values, branchvalues="total") |>
  layout(title="Distribucio CO2 mundial (2019)") |>
  saveWidget("grafics/sunburst_co2.html", selfcontained=TRUE)
