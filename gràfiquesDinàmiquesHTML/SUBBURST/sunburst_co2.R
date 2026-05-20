library(tidyverse)
library(plotly)

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

# ============================================================
# Dades 2019
# ============================================================
d <- df |>
  filter(Year == 2019, !is.na(Code), !is.na(co2_prod)) |>
  mutate(
    co2_gt = co2_prod / 1e9
  )

# Continent (simple)
d$continent <- countrycode::countrycode(
  d$Code, "iso3c", "continent"
)

d <- d |> filter(!is.na(continent))

# Agrupem països petits com "Altres"
d <- d |>
  group_by(continent) |>
  mutate(group = ifelse(co2_gt < 0.05, "Altres", Entity)) |>
  summarise(co2_gt = sum(co2_gt), .by = c(continent, group))

# ============================================================
# Taula jeràrquica sunburst
# ============================================================
total <- sum(d$co2_gt)

nodes <- bind_rows(
  tibble(ids="Mon", labels="Mon", parents="", values=total),

  d |> group_by(continent) |>
    summarise(values=sum(co2_gt)) |>
    transmute(
      ids = continent,
      labels = continent,
      parents = "Mon",
      values = values
    ),

  d |> transmute(
    ids = paste(continent, group, sep = "-"),
    labels = group,
    parents = continent,
    values = co2_gt
  )
)

# ============================================================
# Plot
# ============================================================
plot_ly(
  type = "sunburst",
  ids = nodes$ids,
  labels = nodes$labels,
  parents = nodes$parents,
  values = nodes$values,
  branchvalues = "total"
)
