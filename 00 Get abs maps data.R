library(sf)
library(remotes)
library(tidyverse)
library(readxl)

#remotes::install_github("runapp-aus/strayr")
library(strayr)


vic <-read_absmap("lga2022")%>%
  filter(state_name_2021 == "Victoria")

vic <- vic %>%
  mutate(Local.Government.Area = case_when(
    str_detect(lga_name_2022, "Bayside")~ "Bayside",
    str_detect(lga_name_2022, "Colac")~ "Colac-Otway",
    str_detect(lga_name_2022, "Moreland")~ "Merri-bek",
    str_detect(lga_name_2022, "Kingston")~ "Kingston",
    str_detect(lga_name_2022, "Latrobe")~ "Latrobe",
    TRUE ~ lga_name_2022
  ))

categories <- read_xlsx("Data/Council-category-data.xlsx")

vic <- vic %>%
  left_join(categories, by = "Local.Government.Area")%>%
  mutate(Area.type = ifelse(Category %in% c("Metropolitan", "Interface"), "Metro", "Regional"))

table(vic$Area.type)

saveRDS(vic, "Output.data/Victoria.lga.RDS")
