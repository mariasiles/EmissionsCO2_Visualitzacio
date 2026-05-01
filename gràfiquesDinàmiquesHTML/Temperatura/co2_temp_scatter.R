# ============================================================
# CO2 Concentration vs Temperature Anomaly — estil 4C Carbon
# Dades: CO2 atmosfèric (ppm) i anomalia de temperatura (°C)
# Sortida: grafics/co2_temp_scatter.html
# ============================================================
library(tidyverse)
library(plotly)
library(htmlwidgets)

# ── CO2 atmosfèric: NOAA Mauna Loa (1959+) + Law Dome ice core (1850-1958) ──
url_noaa <- "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_mlo.csv"
co2_mlo <- read_csv(url_noaa, comment = "#", show_col_types = FALSE) |>
  rename(Year = year, co2_ppm = mean) |>
  select(Year, co2_ppm)

# Ice core (Law Dome / Etheridge et al.) — valors anuals interpolats 1850-1958
ice <- tibble(
  Year    = 1850:1958,
  co2_ppm = approx(
    x = c(1850, 1860, 1870, 1880, 1890, 1900, 1910, 1920,
          1930, 1940, 1950, 1958),
    y = c(285.2, 286.0, 287.2, 290.5, 294.4, 295.7, 299.7, 303.0,
          306.8, 310.3, 311.3, 315.3),
    xout = 1850:1958
  )$y
)
co2 <- bind_rows(ice, co2_mlo) |> arrange(Year) |> distinct(Year, .keep_all = TRUE)

# ── Anomalia de temperatura: HadCRUT5 anual global ───────────
url_had <- "https://www.metoffice.gov.uk/hadobs/hadcrut5/data/HadCRUT.5.0.2.0/analysis/diagnostics/HadCRUT.5.0.2.0.analysis.summary_series.global.annual.csv"
had <- read_csv(url_had, show_col_types = FALSE)
temp <- had |>
  rename(Year = 1, temp_anom = 2) |>
  select(Year, temp_anom)

# ── Combinació ───────────────────────────────────────────────
df <- inner_join(co2, temp, by = "Year") |>
  filter(Year >= 1850, !is.na(co2_ppm), !is.na(temp_anom))

# ── Anotacions clau (posició de l'etiqueta en coords de dades) ──
events <- tribble(
  ~Year, ~text,                                                                ~lbl_y,
  1850, "1850",                                                                 1.22,
  1856, "1856\nEunice Foote identified\nCO₂ as greenhouse gas",                 1.05,
  1890, "1890\nGlobal Warming theory\nwas presented by\nArrhenius",             0.78,
  1900, "1900\nSea level rising +22cm\nsince 1900",                             0.50,
  1944, "1944",                                                                 0.42,
  1950, "1950",                                                                -0.30,
  1971, "1971\nOcean warming +383Zj\nsince 1971",                               0.18,
  1990, "1990\nArtic sea ice melting -43%\nin summer area since\n1980",        -0.20,
  1998, "1998",                                                                 0.72,
  2016, "2016\nGlobal temperature\nincreasing +1.2°C above\npre-industrial",    0.40,
  2021, "2021",                                                                 0.55
) |>
  left_join(df, by = "Year")

# Color de cada etiqueta: fosc per a anys freds (sempre visible sobre fons blanc)
# i transició a vermell intens per als anys càlids
ramp_fn <- colorRamp(
  c("#1A252F", "#1A252F", "#1A252F",
    "#C0633E", "#B53A2A", "#A31818", "#8B1A1A", "#6B0E0E")
)
norm_vals <- pmin(pmax((events$temp_anom - (-0.5)) / (1.3 - (-0.5)), 0), 1)
rgb_mat   <- ramp_fn(norm_vals)
events$lbl_color <- sprintf("#%02X%02X%02X",
                             round(rgb_mat[, 1]),
                             round(rgb_mat[, 2]),
                             round(rgb_mat[, 3]))

# ── Colors (versió fons blanc) ───────────────────────────────
BG       <- "#FFFFFF"
COL_LINE <- "#7F8C8D"
COL_TXT  <- "#1A252F"
COL_DASH <- "#95A5A6"

# Escala de color (blau-blanc-vermell) segons temperature anomaly
colors_seq <- colorRampPalette(c("#5DA8C4", "#FFFFFF",
                                  "#F5C2A8", "#E15F4C", "#7E0E0E"))(100)

# ── Figura plotly ─────────────────────────────────────────────
fig <- plot_ly() |>

  # Línia de tendència (regressió lineal)
  add_lines(
    data = df,
    x = ~co2_ppm,
    y = ~fitted(lm(temp_anom ~ co2_ppm, data = df)),
    line = list(color = COL_LINE, width = 1.2),
    name = "Trend",
    hoverinfo = "skip",
    showlegend = FALSE
  ) |>

  # Punts de dispersió
  add_markers(
    data = df,
    x = ~co2_ppm, y = ~temp_anom,
    marker = list(
      color = ~temp_anom,
      colorscale = list(
        c(0,    "#A9CCDC"),
        c(0.18, "#DDEAF1"),
        c(0.28, "#FFFFFF"),
        c(0.40, "#F8DCC9"),
        c(0.55, "#F5C2A8"),
        c(0.70, "#E88A6F"),
        c(0.85, "#E15F4C"),
        c(1.00, "#7E0E0E")
      ),
      cmin = -0.5, cmax = 1.3,
      size = 8,
      line = list(width = 0.6, color = "#1A252F"),
      colorbar = list(
        title       = list(text = "+1.2°C",
                           font = list(color = COL_TXT, size = 11)),
        tickfont    = list(color = COL_TXT, size = 10),
        tickmode    = "array",
        tickvals    = c(-0.2, 0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2),
        ticktext    = c("-0.2", "0.0", "0.2", "0.4", "0.6", "0.8", "1.0", "+1.2°C"),
        outlinecolor = "rgba(0,0,0,0)",
        thickness   = 18,
        len         = 0.85,
        x           = 1.02
      )
    ),
    text = ~paste0("<b>", Year, "</b><br>",
                   "CO₂: ", round(co2_ppm, 1), " ppm<br>",
                   "ΔT: ", round(temp_anom, 2), " °C"),
    hoverinfo = "text",
    name = "Year",
    showlegend = FALSE
  ) |>

  # Línia "Pre-industrial level"
  add_segments(
    x = 280, xend = 420, y = 0, yend = 0,
    line = list(color = COL_DASH, width = 1, dash = "dot"),
    showlegend = FALSE,
    hoverinfo = "skip"
  ) |>

  # Layout
  layout(
    title = list(
      text = "<b>Temperature change<br>relative to pre-industrial period</b>",
      font = list(color = COL_TXT, size = 18, family = "Arial"),
      x = 0.75, xanchor = "center", y = 0.96
    ),
    plot_bgcolor  = BG,
    paper_bgcolor = BG,
    xaxis = list(
      title     = list(text = "<b>CO₂ Concentration</b>",
                       font = list(color = COL_TXT, size = 13)),
      tickfont  = list(color = COL_TXT, size = 11),
      ticksuffix= " ppm",
      gridcolor = "rgba(0,0,0,0.07)",
      zeroline  = FALSE,
      range     = c(278, 425),
      dtick     = 20
    ),
    yaxis = list(
      title     = list(text = "", font = list(color = COL_TXT)),
      tickfont  = list(color = COL_TXT, size = 11),
      ticksuffix = "",
      gridcolor = "rgba(0,0,0,0.07)",
      zeroline  = FALSE,
      range     = c(-0.55, 1.35),
      dtick     = 0.2
    ),
    annotations = list(
      list(x = 418, y = 0.04, xref = "x", yref = "y",
           text = "Pre-industrial level",
           showarrow = FALSE,
           font = list(color = COL_TXT, size = 12),
           xanchor = "right")
    ),
    margin    = list(t = 130, b = 70, l = 60, r = 110),
    hovermode = "closest",
    showlegend = FALSE
  ) |>
  config(displaylogo = FALSE,
         modeBarButtonsToRemove = c("lasso2d", "select2d"))

# ── Afegim anotacions una a una (color individual i font gran) ──
for (i in seq_len(nrow(events))) {
  ev <- events[i, ]
  fig <- fig |> add_annotations(
    x = ev$co2_ppm, y = ev$temp_anom,
    ax = ev$co2_ppm, ay = ev$lbl_y,
    axref = "x", ayref = "y",
    text = ev$text,
    showarrow = TRUE,
    arrowcolor = COL_DASH, arrowwidth = 0.8, arrowhead = 0,
    xanchor = "left", yanchor = "bottom",
    font = list(color = ev$lbl_color, size = 13, family = "Arial"),
    align = "left",
    bgcolor = "rgba(0,0,0,0)"
  )
}

# ── Exportació ────────────────────────────────────────────────
saveWidget(fig, "grafics/co2_temp_scatter.html",
           selfcontained = FALSE,
           libdir = "co2_temp_scatter_files",
           title = "CO2 vs Temperature")

message("✓ Desat: grafics/co2_temp_scatter.html")
