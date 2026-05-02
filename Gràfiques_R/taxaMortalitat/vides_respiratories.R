library(tidyverse)
library(scales)
library(ggtext)

# ============================================================
# Vides salvables: mortalitat respiratoria cronica
# Espanya vs Suecia. Si Espanya tingues la taxa sueca,
# quantes morts s'evitarien?
# ============================================================

resp <- read_csv("chronic-respiratory-diseases-death-rate-who-mdb.csv",
                 show_col_types = FALSE) |>
  setNames(c("Entity","Code","Year","death_rate"))   # per si l'header es estrany

# Si la lectura ha desplaçat noms, ho corregim
if (!"death_rate" %in% names(resp)) {
  resp <- read_csv("chronic-respiratory-diseases-death-rate-who-mdb.csv",
                   show_col_types = FALSE)
  names(resp)[ncol(resp)] <- "death_rate"
}

dades <- resp |>
  filter(Code %in% c("ESP","SWE"), Year >= 1990) |>
  select(pais = Entity, Year, death_rate)

ult_any <- max(dades$Year)
esp_now <- dades$death_rate[dades$pais == "Spain"  & dades$Year == ult_any]
swe_now <- dades$death_rate[dades$pais == "Sweden" & dades$Year == ult_any]

# Poblacio Espanya 2023
pop_esp <- 48.5e6

# Morts evitables per any (taxa = morts per 100.000 habitants)
diff_rate    <- esp_now - swe_now
morts_anual  <- diff_rate * pop_esp / 1e5

# Acumulat 2024-2050 (assumint diferencia constant — escenari conservador)
anys_proj    <- 2050 - ult_any
morts_acum   <- round(morts_anual * anys_proj)

cat(sprintf("Espanya %d: %.1f morts/100k\n", ult_any, esp_now))
cat(sprintf("Suecia %d:  %.1f morts/100k\n", ult_any, swe_now))
cat(sprintf("Diferencia: %.1f morts/100k = %.0f morts/any a Espanya\n",
            diff_rate, morts_anual))
cat(sprintf("Acumulat %d-2050: %d vides\n", ult_any+1, morts_acum))

# ============================================================
# Paleta clara (fons blanc)
# ============================================================
bg       <- "white"
grid_col <- "grey90"
text_col <- "#1A1A1A"
sub_col  <- "grey40"
col_esp  <- "#D4A017"   # groc fosc per llegibilitat sobre blanc
col_swe  <- "#27AE60"   # verd
col_gap  <- "#C0392B"   # vermell fosc

# Posicio del big number al mig
mid_y <- (mean(dades$death_rate[dades$pais == "Spain"]) +
          mean(dades$death_rate[dades$pais == "Sweden"])) / 2

p <- ggplot() +
  # Banda entre les dues corbes (vides perdudes)
  geom_ribbon(data = dades |>
                pivot_wider(names_from = pais, values_from = death_rate) |>
                rename(esp = Spain, swe = Sweden),
              aes(x = Year, ymin = swe, ymax = esp),
              fill = col_gap, alpha = 0.18) +

  # Linies historiques
  geom_line(data = dades |> filter(pais == "Spain"),
            aes(Year, death_rate),
            color = col_esp, linewidth = 1.6) +
  geom_line(data = dades |> filter(pais == "Sweden"),
            aes(Year, death_rate),
            color = col_swe, linewidth = 1.6) +

  # Punts finals
  geom_point(aes(x = ult_any, y = esp_now), color = col_esp, size = 4.5) +
  geom_point(aes(x = ult_any, y = swe_now), color = col_swe, size = 4.5) +
  annotate("text", x = ult_any + 0.3, y = esp_now,
           label = sprintf("Espanya\n%.1f", esp_now),
           color = col_esp, fontface = "bold", size = 4, hjust = 0,
           lineheight = 0.9) +
  annotate("text", x = ult_any + 0.3, y = swe_now,
           label = sprintf("Suècia\n%.1f", swe_now),
           color = col_swe, fontface = "bold", size = 4, hjust = 0,
           lineheight = 0.9) +

  scale_x_continuous(breaks = seq(1990, 2025, 5),
                     limits = c(1990, ult_any + 4),
                     expand = c(0.005, 0)) +
  scale_y_continuous(limits = c(0, 60),
                     breaks = seq(0, 60, 10),
                     expand = c(0, 0)) +
  labs(
    title = "TAXA DE MORTALITAT PER MALALTIES RESPIRATÒRIES CRÒNIQUES",
    subtitle = sprintf(
      "Espanya podria salvar ~<b style='color:%s'>%s vides cada any</b> si tingués la taxa de Suècia (acumulat fins 2050: ~<b style='color:%s'>%s vides</b>)",
      col_gap, format(round(morts_anual), big.mark = "."),
      col_gap, format(morts_acum, big.mark = ".")),
    x = NULL,
    y = "Morts per 100.000 habitants",
    caption = "Diferència de mortalitat associada — entre altres factors — a la qualitat de l'aire i les polítiques de descarbonització.<br>Font: WHO Mortality Database via Our World in Data. Població Espanya: 48.5 M (INE 2023)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = bg, color = NA),
    panel.background = element_rect(fill = bg, color = NA),
    panel.grid.major = element_line(color = grid_col, linewidth = 0.35),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 17,
                                    color = "#1A1A1A",
                                    margin = margin(b = 6)),
    plot.subtitle    = element_markdown(color = text_col, size = 11.5,
                                        margin = margin(b = 14)),
    plot.caption     = element_markdown(color = sub_col, size = 8.5,
                                        hjust = 0, lineheight = 1.3,
                                        margin = margin(t = 12)),
    axis.title.y     = element_markdown(color = text_col, size = 10,
                                        margin = margin(r = 5)),
    axis.text        = element_text(color = text_col, size = 10),
    axis.ticks       = element_line(color = grid_col),
    plot.margin      = margin(18, 22, 14, 18)
  )

if (!dir.exists("grafics")) dir.create("grafics")
ggsave("grafics/vides_respiratories.png",
       plot = p, width = 13.5, height = 7.5, dpi = 160, bg = bg)
cat("Grafic guardat a grafics/vides_respiratories.png\n")