library(tidyverse)
library(readxl)
library(writexl)

#Purpose: add in VicPol unit LGAs and VicPol hierarchy
# Extract individual units
comp <-readRDS("Output.data/data.all.variables.RDS")

table(comp$Legislative.power)
unidat19 <- read_xlsx("Data/Victoria Police Search Data 2018 and 2019.xlsx",
                      sheet = "FINAL_DATA",
                      .name_repair = "universal")


dat <- unidat19

dat <- dat %>%
  mutate(Contact.ID = as.character(Contact.ID))

#remove all records relating to FPOs
exclude.fpo <- dat %>%
  filter(..001.FPO.INTENT.TO.CHARGE== TRUE| ..001.FPO.NO.INTENT.TO.CHARGE== TRUE)%>%
  pull(Contact.ID)

dat <- dat %>%
  filter(!Contact.ID %in% exclude.fpo)

#get table of person ID and search type
powers <-subset(names(dat),grepl("001", names(dat)))


search.dat1 <- dat %>%
  select(Contact.ID, all_of(powers))%>%
  mutate_if(is.logical, as.numeric)%>%
  mutate(Contact.ID = as.character(Contact.ID))%>%
  pivot_longer(!Contact.ID, names_to = "Legislative.power", values_to = "Power")%>%
  mutate(Legislative.power = case_when(
    str_detect(Legislative.power, "WEAPONS") ~ "CONTROL OF WEAPONS ACT",
    str_detect(Legislative.power, "DP") ~ "DP&CS S.82",
    str_detect(Legislative.power, "FPO") ~ "FPO",
    str_detect(Legislative.power, "GRAFFITI") ~ "GRAFFITI PREVENTION ACT",
    str_detect(Legislative.power, "FIREARMS") ~ "FIREARMS ACT",
    str_detect(Legislative.power, "SUB.18") ~ "VOLATILE SUB 18+ 60F",
    str_detect(Legislative.power, "SUB.U") ~ "VOLATILE SUB U/18 60E",
    TRUE ~ Legislative.power
  ))

search.dat <- search.dat1 %>%
  filter(Power == 1)%>%
  mutate(Search.type = case_when(
    str_detect(Legislative.power, "CONTROL") ~ "Weapons",
    str_detect(Legislative.power, "DP&CS") ~ "Drugs",
    str_detect(Legislative.power, "FIREARMS") ~ "Firearms",
    str_detect(Legislative.power, "VOLATILE") ~ "Volatile inhalation substance",
    str_detect(Legislative.power, "GRAFFITI") ~ "Graffiti",
    TRUE ~ "Unknown"
  ))%>%
  #Create unique person and Search type ID
  mutate(Psn.Search.ID = str_c(FieldContactID, Search.type, sep = " - "))


