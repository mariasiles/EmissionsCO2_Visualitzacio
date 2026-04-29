# ============================================================
# Mapa coroplètic animat de CO2 per càpita (1990-2019)
# Sortida: HTML interactiu amb slider d'anys i botó play
# ============================================================
library(tidyverse)
library(plotly)
library(htmlwidgets)

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

# Filtrem 1990-2019, només files amb codi ISO3 i co2 per capita
mapa <- df |>
  filter(Year >= 1990, Year <= 2019,
         !is.na(Code), !is.na(co2_prod_pc),
         co2_prod_pc >= 0, co2_prod_pc < 80) |>
  select(Entity, Code, Year, co2_prod_pc, population, gdp_pc, hdi) |>
  mutate(Year = as.integer(Year),
         # Escala logaritmica -> molt mes contrast visual
         co2_log = log10(co2_prod_pc + 0.1)) |>
  arrange(Year, Code)

cat("Files:", nrow(mapa), " | Paisos unics:", n_distinct(mapa$Code),
    " | Anys:", min(mapa$Year), "-", max(mapa$Year), "\n")

# --- Tooltip personalitzat ---
mapa <- mapa |>
  mutate(
    hover = paste0(
      "<b>", Entity, "</b><br>",
      "Any: ", Year, "<br>",
      "CO2 pc: ", round(co2_prod_pc, 2), " t<br>",
      "PIB pc: ", ifelse(is.na(gdp_pc), "n/d", paste0(round(gdp_pc), " $")), "<br>",
      "HDI: ", ifelse(is.na(hdi), "n/d", round(hdi, 2))
    )
  )

# --- Mapa coroplètic animat ---
fig <- plot_ly(
  data       = mapa,
  type       = "choropleth",
  locations  = ~Code,
  z          = ~co2_log,
  text       = ~hover,
  hoverinfo  = "text",
  frame      = ~Year,
  colorscale = list(
    c(0,    "#1a3d6e"),  # blau fosc (~0.1 t)
    c(0.20, "#2c7bb6"),
    c(0.40, "#abd9e9"),  # ~0.5 t
    c(0.55, "#ffffbf"),  # ~2 t
    c(0.70, "#fdae61"),  # ~5 t
    c(0.85, "#d7301f"),  # ~10 t
    c(1,    "#67000d")   # vermell fosc (~30+ t)
  ),
  zmin       = log10(0.1),   # ~ -1
  zmax       = log10(30),    # ~ 1.48
  colorbar   = list(
    title    = "CO2 t/cap",
    thickness = 18,
    len      = 0.7,
    tickvals = log10(c(0.1, 0.5, 1, 2, 5, 10, 20)),
    ticktext = c("0.1", "0.5", "1", "2", "5", "10", "20")
  ),
  marker     = list(line = list(color = "white", width = 0.4))
) |>
  layout(
    title = list(
      text = "<b>Emissions de CO2 per capita (1990-2019)</b><br><sub>Font: Our World in Data | Producció</sub>",
      x = 0.5, xanchor = "center"
    ),
    geo = list(
      projection      = list(type = "natural earth"),
      showcountries   = TRUE,
      countrycolor    = "grey85",
      showcoastlines  = TRUE,
      coastlinecolor  = "grey60",
      showframe       = FALSE,
      showland        = TRUE,
      landcolor       = "grey95",
      showocean       = TRUE,
      oceancolor      = "#eaf3fa",
      bgcolor         = "white"
    ),
    paper_bgcolor = "white",
    margin        = list(t = 80, b = 40, l = 0, r = 0)
  ) |>
  animation_opts(
    frame      = 800,    # ms per frame
    transition = 0,
    redraw     = TRUE,   # IMPRESCINDIBLE per choropleth
    mode       = "immediate"
  ) |>
  animation_slider(
    currentvalue = list(
      prefix = "Any: ",
      font   = list(size = 16, color = "#222")
    )
  ) |>
  animation_button(
    label = "Play"
  )

# --- Guardar HTML autocontingut ---
dir.create("grafics", showWarnings = FALSE)
out_path <- "grafics/mapa_co2_animat.html"
saveWidget(fig, file = out_path, selfcontained = FALSE,
           title = "Emissions CO2 per capita 1990-2019",
           libdir = "mapa_co2_animat_files")

cat("Mapa interactiu guardat a", out_path, "\n")
