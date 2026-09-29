# MacroDefenseNATOEU
### A comprehensive dataset on military, social and macroeconomic dynamics in Europe, 1960–2023

This repository hosts the research project on the joint dynamics of military expenditure,
public finances and macroeconomic indicators in European NATO countries. It contains:

* a harmonised **country-year database** on defence expenditure, arms trade, public R&D,
  social expenditure, manufacturing and macroeconomic performance, built from official
  international sources;
* the **replication packages** of the papers that use it.

**Project leader:** [Prof. Mario Pianta](https://www.sns.it/it/persona/mario-pianta) (Scuola Normale Superiore, Florence, Italy)

**Project members:** [Dr. Paolo Maranzano](https://www.paolomaranzano.net/home) (University of Milano-Bicocca, Milan, Italy), [Dr. Marco Stamegna](https://www.sns.it/it/persona/marco-stamegna) (Scuola Normale Superiore, Florence, Italy) and [Mrs. Sara Mombelli](https://www.linkedin.com/in/sara-mombelli-64053b277/) (Sapienza University of Rome, Rome, Italy)

---

## Repository structure

```
MacroDefenseNATOEU/
├── README.md                                   this file
├── DefExp_CoreEurope_1960_2023.xlsx / .RData   main database (10 countries, 193 variables)
├── DefExp_EastEurope_1960_2023.xlsx / .RData   Central and Eastern Europe (9 countries, 21 variables)
├── DefExp_Europe_1960_2023 - Metadata.xlsx     variable descriptions and units
├── StamegnaEtAl2024_PEPSPP/                    replication package, Stamegna et al. (2024)
└── StamegnaEtAl2026_SCED/                      replication package, Stamegna et al. (2026)
```

## The database

### Coverage

| File | Countries | Years | Variables |
|---|---|---|---|
| `DefExp_CoreEurope_1960_2023` | Czechia, France, Germany, Hungary, Italy, Netherlands, Poland, Slovak Republic, Spain, United Kingdom | 1960–2023 | 193 |
| `DefExp_EastEurope_1960_2023` | Bulgaria, Czechia, Estonia, Hungary, Latvia, Lithuania, Poland, Romania, Slovak Republic | 1960–2023 | 21 |

The coverage of each series depends on its source and country (typical first year, most series end in 2023):

| Source | Typical first year |
|---|---|
| NATO defence expenditure, SIPRI arms transfers | 1960 (NATO data for later members start with membership) |
| OECD STAN/AMECO manufacturing series | 1970s (1960 for some countries) |
| OECD ICIO / WIOD trade series | 1965 (latest year 2020–2022) |
| OECD MSTI (GBARD), OECD SOCX | 1980–1981 |
| Eurostat (R&D, investment), Eurostat COFOG, ARDECO, World Bank WDI | 1990s |

Missing values are left empty.

### Format

Each database is provided in two formats with the same content: an Excel workbook (`.xlsx`) and an R
data file (`.RData`). The tables are in **wide format by year**, with one row per country and variable:

| Column | Content |
|---|---|
| `Country` | Country name |
| `Source` | Original source of the series (see below) |
| `Variable` | Variable name |
| `1960` … `2023` | Annual values |

The metadata file (`DefExp_Europe_1960_2023 - Metadata.xlsx`) reports, for each variable, its source,
a description and the unit of measure. Variable names follow a common convention:

* `_MillConst2015US`: millions of constant 2015 US dollars;
* `_PercGDP` or `_ShareGDP`: percent of GDP;
* `Share_`: shares in percent;
* `Log_` and `DiffLog_`: logarithms and log-differences.

### Sources

| Source | Content |
|---|---|
| **NATO** – Defence expenditure of NATO countries | Military expenditure (total, % of GDP, by category: personnel, equipment, infrastructure, other), GDP and deflators, military personnel |
| **SIPRI** – Arms Transfers database | Arms exports, imports and balance (trend-indicator values) |
| **Eurostat** – R&D statistics, national accounts and COFOG | Total, business and government R&D; investment (including machinery, equipment and weapons); government expenditure by function (defence, social protection, foreign military aid) |
| **Eurostat – ARDECO** (Annual Regional Database of the European Commission) | Value added, employment and investment in industry and in the total economy (constant 2015 euros), population and labour force |
| **OECD – STAN** (Structural Analysis database), complemented with **AMECO** | Manufacturing value added, employment and gross fixed capital formation; value added of the manufacturing sectors of the Aerospace and Defence ecosystem |
| **OECD – ICIO** (Inter-Country Input-Output tables) and **WIOD** | Exports and imports of manufacturing and of the total economy (final and intermediate) |
| **OECD – MSTI** (Main Science and Technology Indicators) | Government budget allocations for R&D (GBARD), total, civilian and defence |
| **OECD – SOCX** (Social Expenditure database) | Public social expenditure |
| **World Bank – WDI** (World Development Indicators) | Government expenditure (total, final consumption, education, health), defence as a share of government expenditure |

Labels that combine two sources (for example `ICIO - STAN` or `SIPRI - NATO`) identify derived
indicators, such as manufacturing exports as a share of GDP. The label `Logarithm` identifies log
transformations of other series. Monetary variables are expressed in constant 2015 prices, in US
dollars (`_MillConst2015US`) or, for ARDECO, in euros (`_MillConst2015Eur`).

---

## Replication packages

| Folder | Paper | Content |
|---|---|---|
| [`StamegnaEtAl2024_PEPSPP`](StamegnaEtAl2024_PEPSPP) | Stamegna, Bonaiuti, Maranzano and Pianta (2024), *Peace Economics, Peace Science and Public Policy* | R code and data for the figures of the paper |
| [`StamegnaEtAl2026_SCED`](StamegnaEtAl2026_SCED) | Stamegna, Maranzano, Mombelli and Pianta (2026), *Military deindustrialisation and economic performance in Europe* | Panel dataset (5 countries, 1981–2024), R and Stata code reproducing all tables and figures; see its [README](StamegnaEtAl2026_SCED/README.md) |

---

## Scientific references and further readings

1. Greenpeace Italia, Spagna e Germania & Sbilanciamoci.info. [Arming Europe: military expenditure and their economic impact in Germany, Italy and Spain](https://www.greenpeace.org/italy/rapporto/19382/leuropa-si-arma/). Nov. 2023
2. Stamegna, M., Bonaiuti, C., Maranzano, P., & Pianta, M. (2024). [The economic impact of arms spending in Germany, Italy, and Spain](https://www.degruyter.com/document/doi/10.1515/peps-2024-0019/html). *Peace Economics, Peace Science and Public Policy*. DOI: 10.1515/peps-2024-0019
3. D'Aprile, F., Koehler, M., Maranzano, P., Pianta, M. & Strazzari, F. (2025). [Europe's military programmes: strategies, costs and trade-offs](https://ideas.repec.org/p/ssa/lemwps/2025-25.html). LEM Working Paper Series No. 2025/25. DOI: 10.57838/sssa/r1fr-jd35
4. Stamegna, M., Maranzano, P., Mombelli, S., & Pianta, M. (2026). The rise and impact of the military economy in Europe. LEM Working Paper Series No. 2026/16.
5. Stamegna, M., Maranzano, P., Mombelli, S., & Pianta, M. (2026). Military deindustrialisation and economic performance in Europe. Working paper.

## How to cite

If you use the database, please cite this repository and the relevant paper above:

> Pianta, M., Maranzano, P., Stamegna, M., & Mombelli, S. *MacroDefenseNATOEU: a comprehensive dataset on military, social and macroeconomic dynamics in Europe, 1960–2023*. GitHub repository, https://github.com/PaoloMaranzano/MacroDefenseNATOEU
