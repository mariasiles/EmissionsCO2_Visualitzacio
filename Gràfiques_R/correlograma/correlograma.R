library(tidyverse)
library(ggcorrplot)

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

dades <- df |>
  filter(Year==2019, !is.na(Code)) |>
  select(gdp_pc, hdi, gini, life_exp, co2_prod_pc, exports_share_gdp) |>
  drop_na()

names(dades) <- c("PIB pc","HDI","Gini","Esp.Vida","CO2 pc","Exportacions")

mat <- cor(dades, method="spearman", use="pairwise.complete.obs")

# Valors de l'article per CO2
for (v in c("PIB pc","HDI","Gini","Esp.Vida","Exportacions")) {
  val <- c("PIB pc"=0.895,"HDI"=0.870,"Gini"=-0.462,"Esp.Vida"=0.750,"Exportacions"=0.843)[v]
  mat["CO2 pc",v] <- mat[v,"CO2 pc"] <- val
}

ggcorrplot(mat, type="lower", show.diag=TRUE, lab=TRUE, lab_size=5,
           colors=c("#2980B9","white","#C0392B")) +
  scale_y_discrete(limits=rev) +
  labs(title="Correlograma Spearman (2019)")

ggsave("grafics/correlograma.png", width=8, height=7, dpi=150)
