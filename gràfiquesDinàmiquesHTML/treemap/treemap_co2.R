library(tidyverse)
library(plotly)
library(htmlwidgets)
library(countrycode)

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

dat <- df |>
  filter(Year==2019, !is.na(Code), co2_prod>0, Code!="OWID_WRL") |>
  mutate(co2_mt = co2_prod/1e6,
         cont = countrycode(Code,"iso3c","continent"),
         reg  = countrycode(Code,"iso3c","region")) |>
  filter(!is.na(cont)) |>
  mutate(continent = case_when(
    cont=="Americas" & reg=="North America" ~ "America Nord",
    cont=="Americas" ~ "America Llatina", TRUE ~ cont))

nodes <- bind_rows(
  dat |> group_by(continent) |> summarise(co2_mt=sum(co2_mt)) |>
    transmute(ids=continent, labels=continent, parents="", values=co2_mt),
  dat |> transmute(ids=paste0(continent,"_",Entity), labels=Entity,
                   parents=continent, values=co2_mt)
)

plot_ly(type="treemap", ids=nodes$ids, labels=nodes$labels,
        parents=nodes$parents, values=nodes$values, branchvalues="total",
        textinfo="label+percent parent") |>
  layout(title="Qui emet mes CO2? (2019)") |>
  saveWidget("grafics/treemap_co2.html", selfcontained=TRUE)
