library(tidyverse)
library(ggtext)
library(patchwork)
library(scales)
library(ggrepel)

# ============================================================
# Mix electric Espanya vs Suecia (2023) - diagrames circulars
# Categories agrupades per llegibilitat
# ============================================================

mix <- tribble(
  ~pais,     ~font,           ~pct,
  "Espanya", "Eòlica",        10.5,
  "Espanya", "Solar",         14.0,
  "Espanya", "Hidràulica",     7.0,
  "Espanya", "Nuclear",       20.3,
  "Espanya", "Gas / Cogen.",  15.2,

  "Suècia",  "Hidràulica",    42.0,
  "Suècia",  "Eòlica",        22.0,
  "Suècia",  "Biomassa",       7.0,
  "Suècia",  "Nuclear",       22.0
)

ordre_font <- c("Hidràulica","Eòlica","Solar","Biomassa",
                "Nuclear","Gas / Cogen.")
mix$font <- factor(mix$font, levels = ordre_font)

paleta <- c(
  "Hidràulica"   = "#3498DB",
  "Eòlica"       = "#5DADE2",
  "Solar"        = "#F4D35E",
  "Biomassa"     = "#52BE80",
  "Nuclear"      = "#9B59B6",
  "Gas / Cogen." = "#E67E22"
)

fer_pie <- function(dades_pais, titol, color_titol, ajustos = NULL) {
  dades_pais <- dades_pais |>
    arrange(desc(font)) |>
    mutate(ymax = cumsum(pct),
           ymin = lag(ymax, default = 0),
           ymid = (ymax + ymin)/2,
           etiqueta = paste0(font, " ", pct, "%"),
           x_lab = 2.45,
           y_lab = ymid)

  # Aplica ajustos manuals (font -> x_lab i y_offset angular)
  if (!is.null(ajustos)) {
    for (i in seq_len(nrow(ajustos))) {
      idx <- which(as.character(dades_pais$font) == ajustos$font[i])
      if (length(idx) > 0) {
        if (!is.na(ajustos$x_lab[i]))
          dades_pais$x_lab[idx] <- ajustos$x_lab[i]
        if (!is.na(ajustos$y_offset[i]))
          dades_pais$y_lab[idx] <- dades_pais$ymid[idx] + ajustos$y_offset[i]
      }
    }
  }

  ggplot(dades_pais, aes(x = 1, y = pct, fill = font)) +
    geom_col(width = 2, color = "white", linewidth = 1) +
    coord_polar(theta = "y", start = 0, clip = "off") +
    scale_fill_manual(values = paleta, guide = "none") +
    # Etiquetes clarament fora del cercle (radi exterior real = 2)
    geom_text(data = dades_pais,
              aes(x = x_lab, y = y_lab, label = etiqueta),
              color = "grey15", fontface = "bold",
              size = 4, inherit.aes = FALSE) +
    expand_limits(x = c(0, 3.2)) +
    labs(title = titol) +
    theme_void(base_size = 12) +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      plot.title       = element_text(face = "bold", size = 18,
                                      color = color_titol,
                                      hjust = 0.5,
                                      margin = margin(b = 12)),
      plot.margin      = margin(10, 10, 10, 10)
    )
}

mix_esp <- mix |> filter(pais == "Espanya")
mix_swe <- mix |> filter(pais == "Suècia")

ajustos_esp <- tribble(
  ~font,           ~x_lab, ~y_offset,
  "Eòlica",        2.95,    0,        # mes a l'esquerra
  "Gas / Cogen.",  2.95,    +5,       # mes a la dreta i avall
  "Nuclear",       2.75,    -3        # una mica mes a la dreta
)

ajustos_swe <- tribble(
  ~font,           ~x_lab, ~y_offset,
  "Hidràulica",    2.85,    0,        # mes a l'esquerra
  "Biomassa",      2.85,    0         # mes a la dreta
)

p_esp <- fer_pie(mix_esp, "ESPANYA", "#D4A017", ajustos_esp)
p_swe <- fer_pie(mix_swe, "SUÈCIA",  "#27AE60", ajustos_swe)

# Subtotals per al subtitol
fos_esp <- mix_esp |> filter(font == "Gas / Cogen.") |> pull(pct) |> sum()
fos_swe <- 0

final <- (p_esp | p_swe) +
  plot_annotation(
    title = "DIAGRAMA DE SECTORS: ESPANYA vs SUÈCIA",
    subtitle = sprintf(
      "<b style='color:#27AE60'>Suècia ja no genera amb fòssils</b> mentre <b style='color:#C0392B'>Espanya encara té el %.0f%%</b> de generació amb gas i cogeneració",
      fos_esp),
    caption = "Font: Red Eléctrica España (REE), Energimyndigheten (Suècia), Ember Climate. Categories agrupades.",
    theme = theme(
      plot.background  = element_rect(fill = "white", color = NA),
      plot.title       = element_text(face = "bold", size = 18,
                                      color = "#1A1A1A",
                                      hjust = 0,
                                      margin = margin(t = 4, b = 6)),
      plot.subtitle    = element_markdown(color = "grey25", size = 12,
                                          hjust = 0,
                                          margin = margin(b = 12)),
      plot.caption     = element_text(color = "grey45", size = 8.5,
                                      hjust = 0,
                                      margin = margin(t = 8)),
      plot.margin      = margin(16, 22, 14, 18)
    )
  )

if (!dir.exists("grafics")) dir.create("grafics")
ggsave("grafics/mix_electric_esp_swe.png", plot = final,
       width = 13, height = 8, dpi = 160, bg = "white")
cat("Grafic guardat a grafics/mix_electric_esp_swe.png\n")