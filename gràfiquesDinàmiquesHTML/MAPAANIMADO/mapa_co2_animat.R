library(tidyverse)
library(plotly)
library(htmlwidgets)

df <- read_csv("master_dataset.csv", show_col_types=FALSE)

mapa <- df |>
  filter(Year>=1990, Year<=2019, !is.na(Code), !is.na(co2_prod_pc), co2_prod_pc<80) |>
  mutate(Year=as.integer(Year), co2_log=log10(co2_prod_pc+0.1))

plot_ly(mapa, type="choropleth", locations=~Code, z=~co2_log,
        text=~paste0(Entity,"\n",round(co2_prod_pc,1)," t/cap"),
        hoverinfo="text", frame=~Year,
        colorscale=list(c(0,"#1a3d6e"),c(0.5,"#ffffbf"),c(1,"#67000d")),
        zmin=log10(0.1), zmax=log10(30)) |>
  layout(title="CO2 per capita (1990-2019)",
         geo=list(projection=list(type="natural earth"), showframe=FALSE)) |>
  animation_opts(frame=800, transition=0, redraw=TRUE) |>
  animation_slider() |> animation_button(label="Play") |>
  saveWidget("grafics/mapa_co2_animat.html", selfcontained=FALSE, libdir="mapa_files")
