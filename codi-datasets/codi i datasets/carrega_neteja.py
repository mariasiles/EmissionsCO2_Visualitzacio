"""
REPTE 7 — Emissions de CO₂ Globals
Fase 1 V3.0 (Definitiva per a Tableau): Càrrega, Neteja i Consolidació Total
"""

import pandas as pd
import numpy as np
import os

BASE = os.path.dirname(os.path.abspath(__file__))

# =============================================================================
# 1. CÀRREGA DE TOTS ELS DATASETS
# =============================================================================
print("1. Carregant tots els datasets...")

df_co2_prod  = pd.read_csv(os.path.join(BASE, "datasets/annual-co2-emissions-per-country.csv"))
df_co2_cons  = pd.read_csv(os.path.join(BASE, "datasets/consumption-co2-emissions.csv"))
df_sectors   = pd.read_csv(os.path.join(BASE, "datasets/ghg-emissions-by-sector.csv"))
df_pop       = pd.read_csv(os.path.join(BASE, "datasets/population.csv"))
df_gdp       = pd.read_csv(os.path.join(BASE, "datasets/gdp-per-capita-worldbank.csv"))
df_hdi       = pd.read_csv(os.path.join(BASE, "datasets/human-development-index.csv"))
df_gini      = pd.read_csv(os.path.join(BASE, "datasets/economic-inequality-gini-index.csv"))
df_life      = pd.read_csv(os.path.join(BASE, "datasets/life-expectancy.csv"))
df_resp      = pd.read_csv(os.path.join(BASE, "datasets/chronic-respiratory-diseases-death-rate-who-mdb.csv"))
df_exports   = pd.read_csv(os.path.join(BASE, "datasets/value-of-exported-goods-and-services.csv"))
df_energy    = pd.read_csv(os.path.join(BASE, "datasets/primary-energy-cons.csv"))
df_renew     = pd.read_csv(os.path.join(BASE, "datasets/share-electricity-low-carbon.csv"))
df_forest    = pd.read_csv(os.path.join(BASE, "datasets/annual-change-forest-area.csv"))
df_dc        = pd.read_csv(os.path.join(BASE, "datasets/datacenters_per_pais.csv"))


# =============================================================================
# 2. NETEJA I NORMALITZACIÓ DE COLUMNES
# =============================================================================
print("2. Normalitzant columnes i preparant dades especials...")

df_co2_prod.columns  = ["Entity", "Code", "Year", "co2_prod"]
df_co2_cons.columns  = ["Entity", "Code", "Year", "co2_cons"]
df_pop.columns       = ["Entity", "Code", "Year", "population"]
df_hdi               = df_hdi.rename(columns={"Human Development Index": "hdi"})
df_gini              = df_gini.rename(columns={"Gini coefficient": "gini"})
df_life              = df_life.rename(columns={"Life expectancy": "life_exp"})
df_resp.columns      = ["Entity", "Code", "Year", "resp_death_rate"]
df_exports           = df_exports.rename(columns={df_exports.columns[3]: "exports_usd"})
df_energy            = df_energy.rename(columns={"Primary energy consumption": "energy_twh"})
df_renew             = df_renew.rename(columns={"Share of electricity from low-carbon sources": "pct_lowcarbon"})
df_forest            = df_forest.rename(columns={"Annual change in forest area": "forest_change_ha"})

# Extreure la Regió del fitxer de GDP
df_regions = df_gdp[["Code", "World region according to OWID"]].dropna().drop_duplicates(subset=["Code"])
df_regions = df_regions.rename(columns={"World region according to OWID": "Region"})

df_gdp = df_gdp.rename(columns={"GDP per capita": "gdp_pc"}).drop(columns=["World region according to OWID"], errors='ignore')

# Neteja de les dades dels Data Centers
def parse_num(val):
    if pd.isna(val): return np.nan
    try: return float(str(val).replace("~", "").replace("+", "").replace(",", "").strip())
    except: return np.nan

def parse_renew(x):
    if pd.isna(x): return np.nan
    try: return float(str(x).replace("%","").replace("~","").replace("+","").strip().split()[0])
    except: return np.nan

df_dc["total_data_centers"] = df_dc["total_data_centers"].apply(parse_num)
df_dc["power_capacity_MW_total"] = df_dc["power_capacity_MW_total"].apply(parse_num)
df_dc["average_renewable_energy_usage_percent"] = df_dc["average_renewable_energy_usage_percent"].apply(parse_renew)

df_dc = df_dc.rename(columns={
    "country": "Entity",
    "total_data_centers": "dc_total_count",
    "power_capacity_MW_total": "dc_power_mw",
    "average_renewable_energy_usage_percent": "dc_renew_pct"
})[["Entity", "dc_total_count", "dc_power_mw", "dc_renew_pct"]]

# Calcular MW Nets i Bruts
df_dc["dc_mw_net"] = df_dc["dc_power_mw"] * (df_dc["dc_renew_pct"] / 100)
df_dc["dc_mw_brut"] = df_dc["dc_power_mw"] - df_dc["dc_mw_net"]

# =============================================================================
# 3. CONSTRUCCIÓ DEL MASTER DATASET
# =============================================================================
print("3. Construint el master dataset (Merge massiu)...")

# Base: Totes les combinacions possibles d'Entity i Year que produeixin CO2
master = df_co2_prod.copy()

# Afegim la columna Entity_Type
def classify_entity(row):
    if row['Entity'] == 'World': return 'World'
    elif pd.notna(row['Code']) and len(str(row['Code'])) == 3: return 'Country'
    else: return 'Region/Group'

master['Entity_Type'] = master.apply(classify_entity, axis=1)

# Merge de tots els indicadors temporals
merges_temporals = [
    df_co2_cons, df_pop, df_gdp, df_hdi, df_gini, df_life, 
    df_resp, df_exports, df_energy, df_renew, df_forest, df_sectors
]

for df_merge in merges_temporals:
    # Evitem duplicar la columna 'Code' en els merges
    cols_to_use = [c for c in df_merge.columns if c != 'Code']
    master = master.merge(df_merge[cols_to_use], on=['Entity', 'Year'], how='left')

# Merge d'indicadors estàtics (Regions i Data Centers)
master = master.merge(df_regions, on='Code', how='left')
master = master.merge(df_dc, on='Entity', how='left')


# =============================================================================
# 4. CÀLCUL DE VARIABLES DERIVADES (Llestes per pintar)
# =============================================================================
print("4. Calculant mètriques derivades (per càpita, intensitats, etc.)...")

# De la Fase 1:
master["co2_prod_pc"] = master["co2_prod"] / master["population"]
master["co2_cons_pc"] = master["co2_cons"] / master["population"]
master["co2_trade_balance"] = master["co2_prod"] - master["co2_cons"]
master["carbon_intensity"] = master["co2_prod"] / (master["energy_twh"] * 1e6)

# De la Fase 3:
master["exports_pc"] = master["exports_usd"] / master["population"]
master["exports_share_gdp"] = master["exports_usd"] / (master["gdp_pc"] * master["population"])


# =============================================================================
# 5. ORDENACIÓ FINAL I EXPORTACIÓ
# =============================================================================
print("5. Guardant el fitxer final preparat per a Tableau...")

# Ordenem les columnes
first_cols = ['Entity', 'Code', 'Year', 'Entity_Type', 'Region']
other_cols = sorted([c for c in master.columns if c not in first_cols])
master = master[first_cols + other_cols]

out_path = os.path.join(BASE, "master_dataset_tableau.csv")
master.to_csv(out_path, index=False)

print(f"\n✅ ÈXIT TOTAL! Dataset 'master_dataset_tableau.csv' creat.")
print(f"   Files: {master.shape[0]:,} | Columnes: {master.shape[1]}")
print("   Aquest dataset inclou sectors, data centers, regions i mitjanes globals.")