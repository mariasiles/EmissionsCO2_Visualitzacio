library(tidyverse)
library(plotly)
library(htmlwidgets)

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

dat <- df |>
  filter(Year==2019, !is.na(Code)) |>
  select(co2_prod_pc, gdp_pc, hdi, life_exp, gini) |> drop_na() |>
  mutate(co2_q = cut(co2_prod_pc, quantile(co2_prod_pc, c(0,.25,.5,.75,1), na.rm=TRUE),
                     labels=c("Baix","Mig","Alt","Molt alt"), include.lowest=TRUE))

plot_ly(dat, type="splom",
  dimensions=list(
    list(label="CO2 pc",   values=~log10(co2_prod_pc+.01)),
    list(label="PIB pc",   values=~log10(gdp_pc)),
    list(label="HDI",      values=~hdi),
    list(label="Esp.Vida", values=~life_exp),
    list(label="Gini",     values=~gini)),
  color=~co2_q,
  colors=c(Baix="#2ECC71",Mig="#F1C40F",Alt="#E67E22","Molt alt"="#C0392B"),
  marker=list(size=4, opacity=0.6)) |>
  layout(title="SPLOM variables socioeconomiques i CO2 (2019)") |>
  saveWidget("grafics/splom.html", selfcontained=TRUE)
