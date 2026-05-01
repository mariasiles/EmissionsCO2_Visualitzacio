# ============================================================
# CO2 Sources & Sinks interactiu — estil 4C Carbon Explorer
# Sources: OWID CO2 data (fossil + land use)
# Sinks: partició científica GCB (ocean 26%, land 29%, atm 45%)
# Sortida: grafics/sources_sinks_co2.html
# ============================================================
library(tidyverse)
library(plotly)
library(htmlwidgets)

# ── Descarrega OWID CO2 complet (té més columnes que master) ─
url_owid <- "https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv"
tmp <- tempfile(fileext = ".csv")
download.file(url_owid, tmp, mode = "wb", quiet = TRUE)
raw <- read_csv(tmp, show_col_types = FALSE)

# ── Filtra agregat mundial ────────────────────────────────────
gcb <- raw |>
  filter(country == "World", year >= 1850, !is.na(co2)) |>
  transmute(
    Year     = year,
    # MtCO2 → GtC: /1000 (Mt→Gt) i /3.664 (CO2→C)
    fossil   = co2 / 1000 / 3.664,
    land_use = replace_na(land_use_change_co2 / 1000 / 3.664, 0)
  ) |>
  mutate(
    total_source = fossil + land_use,
    # Partició de sinks basada en proporcions GCB 2023
    # (Friedlingstein et al.): ocean ~26%, land ~29%, atm ~45%
    ocean_sink_neg    = -(total_source * 0.256),
    land_sink_neg     = -(total_source * 0.295),
    atm_growth_neg    = -(total_source * 0.450),
    # Línia "Budget Imbalance": traça acumulada dels sinks
    # (com a l'original, va d'alt a la dreta cap a baix)
    budget_imbalance  = -(total_source - fossil * 0.02)
  )

# ── Colors fidels a l'original ───────────────────────────────
COL_FOSSIL  <- "#F5C518"
COL_LANDUSE <- "#8CC84B"
COL_OCEAN   <- "#5FB8C8"
COL_LAND    <- "#E8793A"
COL_ATM     <- "#7FB3A0"
COL_IMBAL   <- "#2C3E50"
BG          <- "#FFFFFF"
COL_TXT     <- "#1A252F"
COL_AXIS    <- "#5D6D7E"

# ── Figura plotly ─────────────────────────────────────────────
fig <- plot_ly(gcb, x = ~Year) |>

  # ── SOURCES (Land-use a la base, Fossil a sobre) ──
  add_trace(
    name = "Land-use Change", y = ~land_use,
    type = "bar",
    marker = list(color = COL_LANDUSE, line = list(width = 0)),
    hovertemplate = "<b>Land-use Change</b><br>%{x}: %{y:.2f} GtC/yr<extra></extra>"
  ) |>
  add_trace(
    name = "Fossil Sources", y = ~fossil,
    type = "bar",
    marker = list(color = COL_FOSSIL, line = list(width = 0)),
    hovertemplate = "<b>Fossil Sources</b><br>%{x}: %{y:.2f} GtC/yr<extra></extra>"
  ) |>

  # ── SINKS (valors negatius) ──
  add_trace(
    name = "Ocean Sink", y = ~ocean_sink_neg,
    type = "bar",
    marker = list(color = COL_OCEAN, line = list(width = 0)),
    hovertemplate = "<b>Ocean Sink</b><br>%{x}: %{y:.2f} GtC/yr<extra></extra>"
  ) |>
  add_trace(
    name = "Land Sink", y = ~land_sink_neg,
    type = "bar",
    marker = list(color = COL_LAND, line = list(width = 0)),
    hovertemplate = "<b>Land Sink</b><br>%{x}: %{y:.2f} GtC/yr<extra></extra>"
  ) |>
  add_trace(
    name = "Atmospheric Growth", y = ~atm_growth_neg,
    type = "bar",
    marker = list(color = COL_ATM, line = list(width = 0)),
    hovertemplate = "<b>Atmospheric Growth</b><br>%{x}: %{y:.2f} GtC/yr<extra></extra>"
  ) |>

  # ── Layout ────────────────────────────────────────────────
  layout(
    barmode = "relative",
    bargap  = 0.05,
    title = list(
      text = "<b>CO₂ EMISSIONS — Sources & Sinks</b><br><sup>Fossil + Land Use Change vs. Ocean, Land & Atmospheric absorption</sup>",
      font = list(color = COL_TXT, size = 17, family = "Arial"),
      x = 0.04, xanchor = "left"
    ),
    plot_bgcolor  = BG,
    paper_bgcolor = BG,
    xaxis = list(
      title     = list(text = "Year", font = list(color = COL_AXIS, size = 12)),
      tickfont  = list(color = COL_AXIS, size = 11),
      gridcolor = "rgba(0,0,0,0.07)",
      zeroline  = FALSE,
      range     = c(1850, 2023),
      dtick     = 20
    ),
    yaxis = list(
      title     = list(text = "GtC / year", font = list(color = COL_AXIS, size = 12)),
      tickfont  = list(color = COL_AXIS, size = 11),
      tickformat = ".0f",
      gridcolor = "rgba(0,0,0,0.07)",
      zeroline  = TRUE,
      zerolinecolor = "rgba(0,0,0,0.45)",
      zerolinewidth = 1.5,
      range     = c(-12, 12),
      dtick     = 2
    ),
    legend = list(
      orientation = "h",
      x = 0, y = -0.16,
      font        = list(color = COL_TXT, size = 11),
      bgcolor     = "transparent",
      traceorder  = "normal"
    ),
    annotations = list(
      list(x = 0.02, y = 0.97, xref = "paper", yref = "paper",
           text = "CO₂ SOURCES", showarrow = FALSE,
           font = list(color = COL_TXT, size = 13, family = "Arial Black"),
           xanchor = "left"),
      list(x = 0.02, y = 0.10, xref = "paper", yref = "paper",
           text = "CO₂ SINKS", showarrow = FALSE,
           font = list(color = COL_TXT, size = 13, family = "Arial Black"),
           xanchor = "left"),
      list(x = 1, y = -0.22, xref = "paper", yref = "paper",
           text = "Source: Our World in Data / GCB 2023 · Sinks: GCB partition ratios (Friedlingstein et al.)",
           showarrow = FALSE, font = list(color = "#95A5A6", size = 9),
           xanchor = "right")
    ),
    margin    = list(t = 80, b = 130, l = 65, r = 30),
    hovermode = "x unified"
  ) |>
  config(
    displayModeBar = TRUE,
    modeBarButtonsToRemove = c("lasso2d", "select2d"),
    displaylogo = FALSE
  )

# ── Exportació (sense pandoc) ─────────────────────────────────
saveWidget(fig, "grafics/sources_sinks_co2.html",
           selfcontained = FALSE,
           libdir = "sources_sinks_co2_files",
           title = "CO2 Sources & Sinks")

message("✓ Desat: grafics/sources_sinks_co2.html")
