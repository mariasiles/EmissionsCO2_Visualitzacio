library(tidyverse)

# ============================================================
# Mix eléctrico simple España vs Suecia
# ============================================================

mix <- tribble(
  ~pais, ~font, ~pct,
  "Espanya","Eòlica",10.5,
  "Espanya","Solar",14,
  "Espanya","Hidràulica",7,
  "Espanya","Nuclear",20.3,
  "Espanya","Gas",15.2,

  "Suècia","Hidràulica",42,
  "Suècia","Eòlica",22,
  "Suècia","Biomassa",7,
  "Suècia","Nuclear",22
)

paleta <- c(
  "Eòlica"="skyblue",
  "Solar"="gold",
  "Hidràulica"="blue",
  "Nuclear"="purple",
  "Gas"="orange",
  "Biomassa"="green"
)

# ============================================================
# Plot simple con pie charts
# ============================================================
ggplot(mix, aes(x = "", y = pct, fill = font)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  facet_wrap(~pais) +
  scale_fill_manual(values = paleta) +
  labs(title = "Mix elèctric: Espanya vs Suècia (2023)") +
  theme_void()

ggsave("grafics/mix_simple.png", width = 10, height = 6)
