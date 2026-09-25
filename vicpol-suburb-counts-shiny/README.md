# Victorian criminal incidents Shiny app

This two-tab Shiny app uses separate Crime Statistics Agency source tables:

- **Suburb counts** uses Table 03 / `CSA sub raw.RDS` to chart recorded incident counts from 2018–2026.
- **LGA rates** uses Table 02 / `CSA lga raw.RDS` to rank LGAs by recorded incidents per 100,000 population.

## What the app does

### Suburb counts

- Defaults to a single **All Victoria** line, with all LGAs and suburbs summed.
- Adds the geography hierarchy **Area Type → LGA → Suburb/Town Name**.
- Selecting an Area Type while leaving LGA blank produces one summed line per selected area type.
- Selecting an LGA while leaving Suburb/Town Name blank produces one summed line per selected LGA.
- Selecting one or more suburbs draws a separate line for each LGA–suburb combination.
- Dots and hover text show incident counts with thousands separators and year-on-year change to one decimal place.
- The line segments joining 2019–2021 are dotted to identify the COVID-affected period.
- A summary table and CSV download reproduce the displayed annual values.

### LGA rates

- Displays a horizontal bar chart of all LGAs, ordered from highest to lowest rate.
- Defaults to the year ending June **2026**.
- Colours each LGA bar by Area Type: Metro or Regional.
- Filters by Area Type, year, Offence Division, Offence Subdivision and Offence Subgroup.
- Leaving Year blank includes 2018–2026. When more than one year is included, the ranking uses the **average annual LGA rate**; rates from different years are not added together.
- Leaving an offence field blank includes and sums all mutually exclusive child categories beneath that level.
- Missing LGA/year combinations after filtering are treated as zero before a multi-year average is calculated.
- A CSV download reproduces the displayed ranking.

The offence hierarchy is:

```text
Offence Division → Offence Subdivision → Offence Subgroup
```

## Required packages

```r
install.packages(c("shiny", "tidyverse", "readxl", "plotly"))
```

## Expected folder layout

Unzip this folder inside your existing project, alongside `R-code-cleaning` and `Primary datasets - VicPol Search`:

```text
your-project/
├── Primary datasets - VicPol Search/
│   └── Data_Tables_LGA_Criminal_Incidents_Year_Ending_June_2026.xlsx
├── R-code-cleaning/
│   └── Processed/
│       ├── CSA lga raw.RDS
│       └── CSA sub raw.RDS
└── vicpol-suburb-counts-shiny/
    ├── app.R
    ├── README.md
    └── R/
        └── data_helpers.R
```

The app first looks for both RDS files. Both files must contain the new `Area.type` column with the values `Metro` and `Regional`. If an RDS file is absent, the app attempts to read the source workbook, but the workbook table must also contain `Area.type`.

## Prepare both RDS files

Run this from the root of your existing project:

```r
library(tidyverse)
library(readxl)

source_workbook <- paste0(
  "./Primary datasets - VicPol Search/",
  "Data_Tables_LGA_Criminal_Incidents_Year_Ending_June_2026.xlsx"
)

csa.lga <- read_xlsx(
  source_workbook,
  sheet = "Table 02",
  .name_repair = "universal"
)

csa.sub <- read_xlsx(
  source_workbook,
  sheet = "Table 03",
  .name_repair = "universal"
)

# Add your reproducible Metro/Regional classification here before saving.
# Both objects must contain Area.type with values "Metro" or "Regional".
stopifnot(
  "Area.type" %in% names(csa.lga),
  "Area.type" %in% names(csa.sub),
  all(na.omit(unique(csa.lga$Area.type)) %in% c("Metro", "Regional")),
  all(na.omit(unique(csa.sub$Area.type)) %in% c("Metro", "Regional"))
)

dir.create("R-code-cleaning/Processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(csa.lga, "R-code-cleaning/Processed/CSA lga raw.RDS")
saveRDS(csa.sub, "R-code-cleaning/Processed/CSA sub raw.RDS")
```

## Run the app

From the root of your existing project:

```r
shiny::runApp("vicpol-suburb-counts-shiny")
```

## Aggregation rules

1. Both datasets are limited to 2018–2026.
2. On the suburb tab, blank Area Type, LGA and Suburb/Town Name means Victoria total.
3. With no LGA selected, selected Area Types are summed separately. With an LGA selected and no suburb selected, all suburbs are summed by LGA.
4. On both tabs, Division filters Subdivision, and Division plus Subdivision filters Subgroup.
5. Blank offence selections include all records at that point in the hierarchy.
6. Counts and rates are summed across the selected lowest-level, mutually exclusive offence-subgroup rows.
7. Missing geography/year combinations after filtering are displayed as zero.
8. On the suburb tab, year-on-year change is based on the displayed annual count and is unavailable when the prior year is zero.
9. On the LGA tab, a single year displays that year's summed LGA rate. Multiple years display the arithmetic mean of the annual summed rates.
