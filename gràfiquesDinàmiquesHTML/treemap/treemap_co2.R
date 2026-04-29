# ============================================================
# Treemap estil "Our World in Data" - Who emits the most CO2?
# Color per CONTINENT (categoric), mida = emissions
# Etiquetes grosses tipus OWID
# ============================================================
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(countrycode)

df <- read_csv("master_dataset.csv", show_col_types = FALSE)

pal_cont <- c(
  "Asia"            = "#E63946",
  "America Nord"    = "#2A9D8F",
  "Europa"          = "#F4A261",
  "America Llatina" = "#457B9D",
  "Africa"          = "#9D4EDD",
  "Oceania"         = "#6A994E"
)

dat <- df |>
  filter(Year == 2019, !is.na(Code), !is.na(co2_prod), co2_prod > 0,
         !Code %in% c("OWID_WRL")) |>
  mutate(
    co2_mt    = co2_prod / 1e6,
    continent = countrycode(Code, "iso3c", "continent"),
    region    = countrycode(Code, "iso3c", "region")
  ) |>
  filter(!is.na(continent)) |>
  mutate(continent_cat = case_when(
    continent == "Americas" & region == "North America" ~ "America Nord",
    continent == "Americas" ~ "America Llatina",
    continent == "Europe"   ~ "Europa",
    continent == "Asia"     ~ "Asia",
    continent == "Africa"   ~ "Africa",
    continent == "Oceania"  ~ "Oceania"
  ))

total_co2 <- sum(dat$co2_mt)
cat("Total mundial:", round(total_co2/1000, 2), "Gt | Paisos:", nrow(dat), "\n\n")

cont_df <- dat |>
  group_by(continent_cat) |>
  summarise(co2_mt = sum(co2_mt), .groups = "drop") |>
  mutate(
    pct = co2_mt / total_co2 * 100,
    ids = continent_cat,
    parents = "",
    color = pal_cont[continent_cat],
    label_html = sprintf(
      "<span style='font-size:72px'><b>%s</b></span>  <span style='font-size:44px'><b>%.2f Gt · %.1f%%</b></span>",
      toupper(continent_cat), co2_mt/1000, pct)
  )

pais_df <- dat |>
  mutate(
    pct = co2_mt / total_co2 * 100,
    ids = paste0(continent_cat, "_", Entity),
    parents = continent_cat,
    color = pal_cont[continent_cat],
    label_html = ifelse(
      pct >= 1.5,
      sprintf("<b>%s</b><br>%.0f Mt CO2<br>%.1f%%", Entity, co2_mt, pct),
      ifelse(pct >= 0.4,
        sprintf("<b>%s</b><br>%.0f Mt · %.1f%%", Entity, co2_mt, pct),
        ifelse(pct >= 0.15,
          sprintf("<b>%s</b> %.1f%%", Entity, pct),
          ""
        )
      )
    )
  ) |>
  select(ids, parents, co2_mt, color, label_html, Entity, pct)

nodes <- bind_rows(
  cont_df |> transmute(ids, parents, values = co2_mt, color, label_html,
                       hover = label_html),
  pais_df |> transmute(ids, parents, values = co2_mt, color, label_html,
                       hover = sprintf("<b>%s</b><br>%.1f Mt CO2<br>%.2f%% mundial",
                                       Entity, co2_mt, pct))
)

cat("Nodes:", nrow(nodes), "\n")

fig <- plot_ly(
  type = "treemap",
  ids = nodes$ids,
  labels = nodes$label_html,
  parents = nodes$parents,
  values = nodes$values,
  text = nodes$hover,
  branchvalues = "total",
  marker = list(
    colors = nodes$color,
    line = list(color = "white", width = 2)
  ),
  textinfo = "label",
  textposition = "top left",
  hovertemplate = "%{text}<extra></extra>",
  textfont = list(family = "Arial Black", color = "white", size = 14),
  pathbar = list(visible = TRUE, side = "top", thickness = 22),
  tiling = list(packing = "squarify", pad = 8)
) |>
  layout(
    title = list(
      text = paste0(
        "<b style='font-size:24px'>Qui emet mes CO2?</b><br>",
        "<span style='font-size:13px;color:#555'>Emissions globals de CO2: ",
        sprintf("<b>%.2f Gt</b> al 2019  |  Color = continent, mida = emissions totals",
                total_co2/1000),
        "</span>"
      ),
      x = 0.02, xanchor = "left", y = 0.97
    ),
    margin = list(t = 90, b = 10, l = 10, r = 10),
    paper_bgcolor = "white"
  )

dir.create("grafics", showWarnings = FALSE)
out_path <- "grafics/treemap_co2.html"
saveWidget(fig, file = out_path, selfcontained = FALSE,
           title = "Who emits the most CO2 - 2019",
           libdir = "treemap_co2_files")

cat("\nGuardat a", out_path, "\n\n--- TOP 10 ---\n")
dat |> arrange(desc(co2_mt)) |>
  mutate(pct = co2_mt/total_co2*100) |>
  select(Entity, continent_cat, co2_mt, pct) |> head(10) |> print()
