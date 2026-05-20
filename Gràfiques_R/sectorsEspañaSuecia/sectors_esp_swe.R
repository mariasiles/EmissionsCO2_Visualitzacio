library(tidyverse)

# ============================================================
# Emisiones por sector: España vs Suecia (2021)
# ============================================================

sec <- read_csv("ghg-emissions-by-sector.csv")
pob <- read_csv("population.csv")

ANY <- 2021

# población
pop <- pob |>
  filter(Year == ANY, Code %in% c("ESP","SWE")) |>
  select(Code, population)

# datos sectores
d <- sec |>
  filter(Year == ANY, Code %in% c("ESP","SWE")) |>
  pivot_longer(-c(Entity, Code, Year),
               names_to = "sector",
               values_to = "co2") |>
  left_join(pop, by = "Code") |>
  mutate(kg_pc = co2 / population * 1000)

# agrupar sectores
d$sector <- recode(d$sector,
  "Transport" = "Transporte",
  "Electricity and heat" = "Electricidad",
  "Industry" = "Industria",
  "Agriculture" = "Agricultura",
  "Buildings" = "Edificios"
)

d <- d |> group_by(Entity, sector) |>
  summarise(kg_pc = sum(kg_pc), .groups = "drop")

# ============================================================
# Plot simple
# ============================================================
ggplot(d, aes(kg_pc, sector, fill = Entity)) +
  geom_col(position = "dodge") +
  labs(
    title = "Emisiones por sector: España vs Suecia",
    x = "kg CO2 per cápita",
    y = ""
  ) +
  scale_fill_manual(values = c("Spain" = "#D4A017",
                                "Sweden" = "#27AE60")) +
  theme_minimal()

ggsave("grafics/sectors_simple.png", width = 10, height = 6)
