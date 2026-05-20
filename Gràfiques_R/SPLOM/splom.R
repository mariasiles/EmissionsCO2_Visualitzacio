library(tidyverse)
library(GGally)

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

dat <- df |>
  filter(Year==2019, !is.na(Code)) |>
  select(co2_prod_pc, gdp_pc, hdi, life_exp, gini) |> drop_na() |>
  mutate(co2_q = cut(co2_prod_pc, quantile(co2_prod_pc, c(0,.25,.5,.75,1), na.rm=TRUE),
                     labels=c("Baix","Mig","Alt","Molt alt"), include.lowest=TRUE),
         log_co2 = log10(co2_prod_pc+.01),
         log_gdp = log10(gdp_pc))

ggpairs(dat, columns=c("log_co2","log_gdp","hdi","life_exp","gini"),
        aes(color=co2_q, alpha=0.5),
        columnLabels=c("CO2 pc","PIB pc","HDI","Esp.Vida","Gini")) +
  scale_color_manual(values=c(Baix="#2ECC71",Mig="#F1C40F",Alt="#E67E22","Molt alt"="#C0392B")) +
  scale_fill_manual(values=c(Baix="#2ECC71",Mig="#F1C40F",Alt="#E67E22","Molt alt"="#C0392B")) +
  labs(title="SPLOM variables socioeconomiques i CO2 (2019)") +
  theme_minimal()

ggsave("grafics/splom.png", width=12, height=10, dpi=150, bg="white")
