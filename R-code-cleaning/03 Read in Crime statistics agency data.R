library(tidyverse)
library(readxl)
library(writexl)

# Read in crime stats data
csa.lga <- read_xlsx(
  "./Primary datasets - VicPol Search/Data_Tables_LGA_Criminal_Incidents_Year_Ending_June_2026.xlsx",
  sheet = "Table 02",
  .name_repair = "universal"
)

# Read in crime stats data
csa.sub <- read_xlsx(
  "./Primary datasets - VicPol Search/Data_Tables_LGA_Criminal_Incidents_Year_Ending_June_2026.xlsx",
  sheet = "Table 03",
  .name_repair = "universal"
)

names(csa.lga)

csa_rate <- csa.lga  %>%
  # Keep relevant years
  filter(Year %in% c(2018, 2019, 2022, 2023, 2024, 2025, 2026)) %>%
  group_by(Local.Government.Area, Year) %>%
  summarise(
    Drug.crime.rate = sum(LGA.Rate.per.100.000.population[Offence.Division == "C Drug offences"], na.rm = TRUE),
    Total.crime.rate = sum(LGA.Rate.per.100.000.population, na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(csa_rate, "R-code-cleaning/Processed/CSA rates.RDS")