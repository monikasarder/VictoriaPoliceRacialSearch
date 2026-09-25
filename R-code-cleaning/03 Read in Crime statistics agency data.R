library(tidyverse)
library(readxl)
library(writexl)

setwd("C:/Users/monik/OneDrive/Documents/VictoriaPoliceRacialSearch/")

categories <- read_xlsx("./Primary datasets - VicPol Search/Council-category-data.xlsx")


getwd()
  
# Read in crime stats data
csa.lga <- read_xlsx(
  "./Primary datasets - VicPol Search/Data_Tables_LGA_Criminal_Incidents_Year_Ending_June_2026.xlsx",
  sheet = "Table 02",
  .name_repair = "universal"
)

csa.lga <- csa.lga %>%
  left_join(categories, by = "Local.Government.Area") %>%
  mutate(Area.type = case_when(
    Category %in% c("Metropolitan", "Interface") ~ "Metro",
    Category %in% c("Large shire", "Regional", "Small shire") ~ "Regional",
    TRUE ~ NA_character_
  ))


saveRDS(csa.lga, "R-code-cleaning/Processed/CSA lga raw.RDS")

# Read in crime stats data
csa.sub <- read_xlsx(
  "./Primary datasets - VicPol Search/Data_Tables_LGA_Criminal_Incidents_Year_Ending_June_2026.xlsx",
  sheet = "Table 03",
  .name_repair = "universal"
)


csa.sub <- csa.sub %>%
  left_join(categories, by = "Local.Government.Area") %>%
  mutate(Area.type = case_when(
    Category %in% c("Metropolitan", "Interface") ~ "Metro",
    Category %in% c("Large shire", "Regional", "Small shire") ~ "Regional",
    TRUE ~ NA_character_
  ))


saveRDS(csa.sub, "R-code-cleaning/Processed/CSA sub raw.RDS")

table(csa.sub$Area.type)

csa_rate <- csa.lga  %>%
  # Keep relevant years
  filter(Year %in% c(2018, 2019, 2022, 2023, 2024, 2025, 2026)) %>%
  group_by(Local.Government.Area, Year) %>%
  summarise(
    Prohibited.weapons.crime = sum(LGA.Rate.per.100.000.population[Offence.Subgroup == "D12 Prohibited and controlled weapons offences"], na.rm = TRUE),
    Drug.crime.rate = sum(LGA.Rate.per.100.000.population[Offence.Division == "C Drug offences"], na.rm = TRUE),
    Total.crime.rate = sum(LGA.Rate.per.100.000.population, na.rm = TRUE),
    .groups = "drop"
  )



# Read in crime stats data
csa.sub <- read_xlsx(
  "./Primary datasets - VicPol Search/Data_Tables_LGA_Criminal_Incidents_Year_Ending_June_2026.xlsx",
  sheet = "Table 03",
  .name_repair = "universal"
)

saveRDS(csa_rate, "R-code-cleaning/Processed/CSA rates.RDS")
