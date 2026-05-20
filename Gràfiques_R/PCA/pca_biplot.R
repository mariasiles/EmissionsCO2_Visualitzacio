library(tidyverse)
library(countrycode)

df <- read_csv("master_dataset.csv", show_col_types = FALSE)
vars <- c("gdp_pc","hdi","life_exp","co2_prod_pc","carbon_intensity","exports_share_gdp")

dat <- df |>
  filter(Year==2019, !is.na(Code)) |> drop_na(all_of(vars)) |>
  mutate(continent = countrycode(Code,"iso3c","continent", warn=FALSE)) |>
  filter(!is.na(continent))

pca  <- prcomp(scale(dat[,vars]))
ve   <- round(summary(pca)$importance[2,1:2]*100,1)
sc   <- bind_cols(as.data.frame(pca$x[,1:2]), continent=dat$continent)
load <- as.data.frame(pca$rotation[,1:2]*4) |> mutate(var=rownames(pca$rotation))

ggplot(sc, aes(PC1,PC2,color=continent)) +
  geom_point(alpha=0.5) +
  geom_segment(data=load, aes(0,0,xend=PC1,yend=PC2), color="grey30",
               arrow=arrow(length=unit(0.2,"cm"))) +
  geom_text(data=load, aes(PC1,PC2,label=var), size=3, hjust=-0.1, color="black") +
  labs(title=paste0("PCA (PC1=",ve[1],"%, PC2=",ve[2],"%)"), color=NULL) +
  theme_minimal()

ggsave("grafics/pca_biplot.png", width=10, height=7, dpi=150, bg="white")
