library(tidyverse)
library(scales)
library(ggtext)

# ============================================================
# Comparativa per sector ESP vs SWE
# Missatge: "On falla cada pais? Espanya falla mes en us que en electricitat"
# ============================================================

sec <- read_csv("ghg-emissions-by-sector.csv", show_col_types = FALSE)
pob <- read_csv("population.csv", show_col_types = FALSE)
names(pob)[ncol(pob)] <- "population"

ANY <- 2021
esp_pop <- pob |> filter(Code == "ESP", Year == ANY) |> pull(population) |> head(1)
swe_pop <- pob |> filter(Code == "SWE", Year == ANY) |> pull(population) |> head(1)

dades <- sec |>
  filter(Code %in% c("ESP","SWE"), Year == ANY) |>
  pivot_longer(-c(Entity, Code, Year),
               names_to = "sector", values_to = "co2_t") |>
  mutate(
    pop = ifelse(Code == "ESP", esp_pop, swe_pop),
    kg_pc = co2_t / pop * 1000
  )

# Agrupem sectors similars + traduïm
agrupar <- function(s) {
  case_when(
    s == "Transport"                                 ~ "Transport",
    s == "Aviation and shipping"                     ~ "Aviació i marítim",
    s == "Electricity and heat"                      ~ "Electricitat i calor",
    s == "Buildings"                                 ~ "Edificis (calefacció)",
    s %in% c("Industry","Manufacturing and construction") ~ "Indústria",
    s == "Agriculture"                               ~ "Agricultura",
    s %in% c("Waste","Fugitive emissions","Other fuel combustion") ~ "Residus i altres",
    s == "Land-use change and forestry"              ~ NA_character_,   # exclos
    TRUE ~ NA_character_
  )
}

dades <- dades |>
  mutate(grup = agrupar(sector)) |>
  filter(!is.na(grup)) |>
  group_by(Entity, grup) |>
  summarise(kg_pc = sum(kg_pc, na.rm = TRUE), .groups = "drop")

# Ordenem sectors pel valor d'Espanya
ordre <- dades |>
  filter(Entity == "Spain") |>
  arrange(kg_pc) |>
  pull(grup)
dades$grup <- factor(dades$grup, levels = ordre)

# Calcul gap percentatge per anotar
gap <- dades |>
  pivot_wider(names_from = Entity, values_from = kg_pc) |>
  rename(esp = Spain, swe = Sweden) |>
  mutate(
    diff   = esp - swe,
    ratio  = esp / swe,
    txt    = case_when(
      ratio > 1.5  ~ sprintf("ESP %.1fx", ratio),
      ratio < 0.7  ~ sprintf("ESP %.1fx", ratio),
      TRUE         ~ sprintf("ESP %+.0f%%", (esp - swe)/swe * 100)
    )
  )

col_esp <- "#D4A017"
col_swe <- "#27AE60"

p <- ggplot(dades, aes(x = kg_pc, y = grup, fill = Entity)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(aes(label = sprintf("%.0f", kg_pc)),
            position = position_dodge(width = 0.75),
            hjust = -0.15, size = 3.6, fontface = "bold",
            color = "grey15") +
  geom_text(data = gap,
            aes(x = pmax(esp, swe), y = grup, label = txt),
            inherit.aes = FALSE,
            hjust = -1.6, size = 3.4, color = "#C0392B",
            fontface = "italic") +
  scale_fill_manual(values = c("Spain" = col_esp, "Sweden" = col_swe),
                    labels = c("Spain" = "Espanya", "Sweden" = "Suècia"),
                    name = NULL) +
  scale_x_continuous(labels = scales::comma_format(big.mark = "."),
                     expand = expansion(mult = c(0, 0.30))) +
  labs(
    title = "ON EMET CADA PAÍS? EMISSIONS PER SECTOR I PER CÀPITA",
    subtitle = sprintf(
      "<b style='color:%s'>Espanya</b> vs <b style='color:%s'>Suècia</b> · kg CO₂eq per persona i any · %d",
      col_esp, col_swe, ANY),
    x = "kg CO₂ equivalent per persona",
    y = NULL,
    caption = "ESP Nx = Espanya emet N vegades el que Suècia. Land-use change exclòs (volàtil any a any).<br>Font: Our World in Data | Climate Watch (GHG by sector). Població INE/Banc Mundial."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey92", linetype = "33"),
    panel.grid.minor   = element_blank(),
    plot.title       = element_text(face = "bold", size = 17, color = "#1A1A1A",
                                    margin = margin(b = 4)),
    plot.subtitle    = element_markdown(color = "grey25", size = 11.5,
                                        margin = margin(b = 16)),
    plot.caption     = element_markdown(color = "grey45", size = 8.5,
                                        hjust = 0, lineheight = 1.3,
                                        margin = margin(t = 12)),
    axis.text.y      = element_text(face = "bold", size = 11),
    axis.title.x     = element_text(color = "grey35", size = 10,
                                    margin = margin(t = 6)),
    legend.position  = "top",
    legend.justification = "left",
    legend.text      = element_text(size = 11, face = "bold"),
    plot.margin      = margin(18, 22, 14, 18)
  )

if (!dir.exists("grafics")) dir.create("grafics")
ggsave("grafics/sectors_esp_swe.png", plot = p,
       width = 12, height = 7, dpi = 160, bg = "white")

cat("Per capita emissions per sector (kg/pers/any):\n")
print(dades |> pivot_wider(names_from = Entity, values_from = kg_pc))
cat("\nGrafic guardat a grafics/sectors_esp_swe.png\n")