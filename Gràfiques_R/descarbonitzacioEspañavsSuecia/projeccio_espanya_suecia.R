library(tidyverse)

# ============================================================
# España vs Suecia: proyección simple de CO2
# ============================================================

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

serie <- df |>
  filter(Code %in% c("ESP","SWE"), Year >= 1970, !is.na(co2_prod_pc)) |>
  select(pais = Entity, Year, co2 = co2_prod_pc)

# Datos actuales
ult <- max(serie$Year)

esp_now <- serie |> filter(pais == "Spain", Year == ult) |> pull(co2)
swe_now <- serie |> filter(pais == "Sweden", Year == ult) |> pull(co2)

# Modelos simples
m_esp <- lm(co2 ~ Year, data = serie |> filter(pais == "Spain", Year >= 2010))
m_swe <- lm(co2 ~ Year, data = serie |> filter(pais == "Sweden", Year <= 1990))

sl_esp <- coef(m_esp)[2]
sl_swe <- coef(m_swe)[2]

# Proyección
years <- ult:2050

proj <- function(y0, slope, label){
  tibble(
    Year = years,
    co2  = pmax(y0 + slope * (years - ult), 0),
    esc  = label
  )
}

p1 <- proj(esp_now, sl_esp, "España actual")
p2 <- proj(swe_now, sl_swe, "Si España siguiera ritmo Suecia")

data_plot <- bind_rows(p1, p2)

# ============================================================
# Plot simple
# ============================================================
ggplot(data_plot, aes(Year, co2, color = esc)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = ult, linetype = "dashed") +
  labs(
    title = "Proyección CO2: España vs escenario Suecia",
    x = "Año",
    y = "CO2 per cápita",
    color = ""
  ) +
  theme_minimal()

ggsave("grafics/proyeccion_simple.png", width = 10, height = 6)
