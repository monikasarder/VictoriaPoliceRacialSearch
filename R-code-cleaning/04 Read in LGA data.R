library(tidyverse)
library(readxl)
library(writexl)

# Read in crime stats data
abs <- read_xlsx(
  "./Primary datasets - VicPol Search/ABS_Census_2021_LGA.xlsx",
  sheet = 1,
  .name_repair = "universal"
)

abs <- abs %>%
  select(LGA.Population = Usual.Resident.Population,
         Local.Government.Area = Local.Government.Areas.2021.name)

saveRDS(abs, "R-code-cleaning/Processed/LGA pop.RDS")