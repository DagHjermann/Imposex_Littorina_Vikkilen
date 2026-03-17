# Project: Imposex & TBT — Vikkilen

Environmental monitoring study of intersex (sexual disruption) in marine snails and TBT contamination in Vikkilen fjord (near the Nymo shipyard). Data spans 2005–2024. Goal: produce figures and tables for a scientific manuscript.

## Species & Metrics

| Species | Norwegian name | Metric(s) |
|---------|---------------|-----------|
| *Littorina littorea* | Strandsnegl | ISI (Intersex Stage Index), PRL (Female Prostate Length), %S (% sterile females) |
| *Nassarius reticulatus* | Nettsnegl | VDSI (Vas Deferens Sequence Index) |
| *Nucella lapillus* | Purpursnegl | — |
| *Buccinum undatum* | Kongsnegl | PCI (Penis Classification Index) |
| *Mytilus edulis* | Blåskjell | TBT only |

TBT concentrations in µg/kg wet weight.

## Stations

| Station | Name | Distance from shipyard |
|---------|------|------------------------|
| 6 (Nymo) | Shipyard | 0 km (source) |
| 5 / 5B | Skjeviga | 0.1 km |
| 7 | Inner Vikkilen | 0.5 km |
| 4 | Hasseldalen | 2.5 km |
| 1 | Håøya | 5.5 km (local reference) |
| 71G | Langesundfjord | ~100 km (regional reference) |

Station coordinates: `08_Station_map.R` and `Data/03_Stasjoner_Vikkilen.xlsx`.

## Analysis Pipeline

```
Input_data/ (raw Excel files)
    ↓
02_Read_data.R              → Data/02_*.RData   (intersex data, all species)
05_Read_chemistry_data_excel.R → Data/05_TBT.RData/.xlsx  (TBT data)
21_Read_new_data_2016-2021.Rmd → Data/21_data_*.csv  (from Nivabasen DB)
    ↓
06_Tables_graphs.Rmd        → Figures 2–6, Output_excel/ tables  ← MAIN SCRIPT
07_Sediment_newfile_July2020.Rmd → Figure 7 (sediment TBT + map)
08_Station_map.R            → Interactive station map (HTML)
```

## Figures

| Figure | File pattern | Content |
|--------|-------------|---------|
| FIG2 | `06_Timeseries_Comb_ISI_TBT_bw_FIG2` | Littorina ISI + TBT time series (B&W) |
| FIG3 | `06_Timeseries_Comb_TBT_VDSI_bw_FIG3` | Nassarius VDSI + Littorina ISI time series (B&W) |
| FIG4 | `06_Timeseries_Litt_PRL_FIG4` | Female Prostate Length time series |
| FIG5 | `06_Correlations_combined_color_FIG5` | Correlation matrix (ISI, TBT, PRL) |
| FIG6 | `06_Timeseries_allsp_TBT_3_log_FIG6` | All species TBT with logistic regression |
| FIG7A | `07_tbt_vs_year_FIG7A` | Sediment TBT trends with logistic fit |
| FIG1C/7B | `07_sediment_areas_manual_ppt3_FIG1C` | Sediment sampling map + bathymetry |

Figures saved as PNG and SVG in `Figures/`.

## Key Scripts

- **`06_Tables_graphs.Rmd`** — main analysis: combines all data, calculates summary stats, runs logistic/linear regression trends, Kendall tau correlations, produces Figures 2–6 and Excel summary tables.
- **`06_Tables_graphs_cleaned.Rmd`** — latest cleaned/updated version.
- **`06_Tables_graphs_functions.R`** — statistical helper functions: `lm_trans()`, `pred_logistic()`, `pred_logistic_from_stationname()`, `polygon.lines()`, `round_pvalue()`, etc.

## Output Tables (`Output_excel/`)

- `06_Means_Littorina*.xlsx` — mean ISI, PRL, % sterile by station/year
- `06_Mean_tbt*.xlsx` — mean TBT by species/station/year
- `06_Imposex_nettsnegl*.xlsx` — Nassarius summary stats

## Tech Stack

- R / RMarkdown, tidyverse (dplyr, ggplot2, purrr), readxl, openxlsx
- leaflet (interactive maps), cowplot (multi-panel figures)
- niRvana package — access to Nivabasen (NIVA environmental database), O-number 15404
