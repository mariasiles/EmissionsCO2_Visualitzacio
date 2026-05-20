library(tidyverse)
library(countrycode)

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

# ============================================================
# CO2 2000 vs 2019
# ============================================================
d <- df |>
  filter(Year %in% c(2000, 2019),
         !is.na(Code), !is.na(co2_prod_pc)) |>
  select(Entity, Code, Year, co2_prod_pc) |>
  pivot_wider(names_from = Year, values_from = co2_prod_pc) |>
  drop_na() |>
  mutate(
    change = `2019` - `2000`,
    group = ifelse(change >= 0, "Augmenta", "Redueix")
  )

# Top cambios más extremos
plot_d <- d |>
  slice_max(abs(change), n = 36) |>
  mutate(Entity = reorder(Entity, change))

# ============================================================
# Plot
# ============================================================
ggplot(plot_d, aes(change, Entity, fill = group)) +
  geom_col() +
  geom_vline(xintercept = 0) +
  labs(
    title = "Canvi de CO2 per càpita (2000–2019)",
    x = "Canvi",
    y = ""
  ) +
  scale_fill_manual(values = c("Redueix" = "#2ECC71",
                                "Augmenta" = "#E74C3C")) +
  theme_minimal()

ggsave("grafics/diverging_bar.png", width = 10, height = 8)
