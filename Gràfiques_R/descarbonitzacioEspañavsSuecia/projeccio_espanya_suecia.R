library(tidyverse)
library(scales)
library(ggtext)

# ============================================================
# Projeccio Espanya: 'Si seguim aixi' vs 'Si fem com Suecia'
# Estil dashboard/infografia
# ============================================================

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

serie <- df |>
  filter(Code %in% c("ESP","SWE"), Year >= 1970, !is.na(co2_prod_pc)) |>
  select(pais = Entity, Year, co2_pc = co2_prod_pc)

ult_any <- max(serie$Year)
target  <- serie |> filter(pais == "Sweden", Year == ult_any) |> pull(co2_pc)
y0      <- serie |> filter(pais == "Spain",  Year == ult_any) |> pull(co2_pc)

# Pendents
fit_inercia <- lm(co2_pc ~ Year,
                  data = serie |> filter(pais == "Spain", Year >= 2010))
fit_suecia  <- lm(co2_pc ~ Year,
                  data = serie |> filter(pais == "Sweden",
                                         Year >= 1970, Year <= 1990))
sl_inercia <- coef(fit_inercia)[2]
sl_suecia  <- coef(fit_suecia)[2]

# Sigma residual per generar bandes d'incertesa
sg_inercia <- summary(fit_inercia)$sigma
sg_suecia  <- summary(fit_suecia)$sigma

# Projeccio amb banda d'incertesa (creixent en t)
set.seed(7)
mk_proj <- function(sl, sg, etiqueta) {
  yrs <- ult_any:2050
  dt  <- yrs - ult_any
  mu  <- y0 + sl * dt
  # Soroll realista (autocorrelat suau) per simular pujades i baixades
  noise <- numeric(length(yrs))
  for (i in seq_along(yrs)) {
    if (i == 1) noise[i] <- 0
    else noise[i] <- 0.55 * noise[i - 1] + rnorm(1, 0, abs(sg) * 0.18)
  }
  co2 <- mu + noise
  se  <- sg * sqrt(dt)
  tibble(
    Year     = yrs,
    co2_pc   = pmax(co2, 0),
    lo       = pmax(mu - 1.0 * se, 0),
    hi       = pmax(mu + 1.0 * se, 0),
    escenari = etiqueta
  )
}
proj_in <- mk_proj(sl_inercia, sg_inercia, "Si seguim així")
proj_sw <- mk_proj(sl_suecia,  sg_suecia,  "Si fem com Suècia")

# Tallem la projeccio quan toca el target (no extrapolem mes enlla)
# i fixem l'ultim punt exactament sobre la linia objectiu
tallar <- function(p, sl) {
  i <- which(p$co2_pc <= target)[1]
  if (is.na(i)) return(p)
  out <- p[1:i, ]
  out$co2_pc[nrow(out)] <- target
  out
}
proj_in_t <- tallar(proj_in, sl_inercia)
proj_sw_t <- tallar(proj_sw, sl_suecia)

yr_in <- proj_in_t$Year[nrow(proj_in_t)]
yr_sw <- proj_sw_t$Year[nrow(proj_sw_t)]

# Punt final ja esta exactament sobre el target
last_in <- tail(proj_in_t, 1)
last_sw <- tail(proj_sw_t, 1)

hist_esp <- serie |> filter(pais == "Spain")
hist_swe <- serie |> filter(pais == "Sweden")

# ============================================================
# Paleta tipus dashboard
# ============================================================
bg       <- "#0E2A33"   # fons fosc teal
grid_col <- "#1F4854"
text_col <- "#D6E7EA"
sub_col  <- "#9FB8BE"
hist_esp_col <- "#F4D35E"   # groc per Espanya historic (bonic sobre fons fosc)
hist_swe_col <- "#7DD3C0"   # mint per Suecia
inercia_col  <- "#FF6B6B"   # vermell-coral
suecia_col   <- "#3DDC97"   # verd brillant
target_col   <- "#A2D5DC"

p <- ggplot() +
  # Bandes d'incertesa
  geom_ribbon(data = proj_in_t,
              aes(Year, ymin = lo, ymax = hi),
              fill = inercia_col, alpha = 0.20) +
  geom_ribbon(data = proj_sw_t,
              aes(Year, ymin = lo, ymax = hi),
              fill = suecia_col, alpha = 0.20) +

  # Historic Suecia (referencia tenue)
  geom_line(data = hist_swe, aes(Year, co2_pc),
            color = hist_swe_col, linewidth = 1, alpha = 0.65) +

  # Historic Espanya (linia principal)
  geom_line(data = hist_esp, aes(Year, co2_pc),
            color = hist_esp_col, linewidth = 1.4) +

  # Best estimate (linies discontinues centrals)
  geom_line(data = proj_in_t,
            aes(Year, co2_pc),
            color = inercia_col, linewidth = 1, linetype = "22") +
  geom_line(data = proj_sw_t,
            aes(Year, co2_pc),
            color = suecia_col,  linewidth = 1, linetype = "22") +

  # Punts finals + etiquetes
  geom_point(data = last_in,  aes(Year, co2_pc),
             color = inercia_col, size = 4) +
  geom_point(data = last_sw,  aes(Year, co2_pc),
             color = suecia_col,  size = 4) +

  annotate("text", x = last_in$Year + 0.5, y = last_in$co2_pc + 0.45,
           label = sprintf("Inèrcia\n%d", last_in$Year),
           hjust = 0, color = inercia_col, fontface = "bold",
           size = 4, lineheight = 0.9) +
  annotate("text", x = last_sw$Year + 0.5, y = last_sw$co2_pc - 0.55,
           label = sprintf("Com Suècia\n%d", last_sw$Year),
           hjust = 0, color = suecia_col, fontface = "bold",
           size = 4, lineheight = 0.9) +

  # Target
  geom_hline(yintercept = target, color = target_col,
             linetype = "11", linewidth = 0.5, alpha = 0.8) +
  annotate("text", x = 2001, y = target + 0.32,
           label = sprintf("Objectiu Suècia 2026: %.1f t/pers", target),
           hjust = 0, color = target_col,
           size = 3.4, fontface = "italic") +

  # Linies "Historic | Projecció"
  geom_vline(xintercept = ult_any, color = sub_col,
             linetype = "33", linewidth = 0.45) +
  annotate("text", x = ult_any - 0.5, y = 9.0,
           label = "Històric",
           hjust = 1, color = sub_col, fontface = "italic", size = 3.5) +
  annotate("text", x = ult_any + 0.5, y = 9.0,
           label = "Projeccions",
           hjust = 0, color = sub_col, fontface = "italic", size = 3.5) +

  # Etiquetes paisos
  annotate("text", x = 2002, y = hist_esp$co2_pc[hist_esp$Year == 2002] + 0.6,
           label = "ESPANYA",
           color = hist_esp_col, fontface = "bold", size = 4.2, hjust = 0) +
  annotate("text", x = 2002, y = hist_swe$co2_pc[hist_swe$Year == 2002] - 0.6,
           label = "SUÈCIA",
           color = hist_swe_col, fontface = "bold", size = 4.2, hjust = 0) +

  scale_x_continuous(breaks = seq(2000, 2040, 5),
                     limits = c(2000, 2042),
                     expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(0, 9.5),
                     breaks = seq(0, 9, 1),
                     expand = c(0, 0),
                     labels = function(x) paste0(x, " t")) +
  labs(
    title    = "QUANT TRIGARÀ ESPANYA A EMETRE COM SUÈCIA?",
    subtitle = sprintf(
      "Adoptant el ritme de la transició nuclear sueca, Espanya arribaria el <b style='color:%s'>%d</b> — <b style='color:%s'>%d anys abans</b> que amb el ritme actual (<b style='color:%s'>%d</b>)",
      suecia_col, last_sw$Year, suecia_col, last_in$Year - last_sw$Year, inercia_col, last_in$Year),
    x = NULL,
    y = "CO<sub>2</sub> per càpita",
    caption = sprintf(
      "Inèrcia: regressió Espanya 2010-%d (%.3f t/any). 'Com Suècia': regressió Suècia 1970-1990 (%.3f t/any) aplicada des de %d.<br>Bandes = ±1σ de la regressió. Projeccions tallades en arribar al nivell suec actual.<br>Font: Our World in Data — emissions de producció de CO<sub>2</sub>.",
      ult_any, sl_inercia, sl_suecia, ult_any)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = bg, color = NA),
    panel.background = element_rect(fill = bg, color = NA),
    panel.grid.major = element_line(color = grid_col, linewidth = 0.35),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 18,
                                    color = "#7DD3C0",
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
    plot.margin      = margin(18, 20, 14, 18)
  )

if (!dir.exists("grafics")) dir.create("grafics")
ggsave("grafics/projeccio_espanya_suecia.png",
       plot = p, width = 13.5, height = 7.5, dpi = 160, bg = bg)

cat(sprintf("Pendent inercia: %.3f | Pendent Suecia transicio: %.3f\n",
            sl_inercia, sl_suecia))
cat(sprintf("Arribada inercia: %d | Arribada com-Suecia: %d | Diferencia: %d anys\n",
            last_in$Year, last_sw$Year, last_in$Year - last_sw$Year))
cat("Grafic guardat a grafics/projeccio_espanya_suecia.png\n")
