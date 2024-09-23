library(tidyverse)
library(readxl)
library(writexl)

#Purpose: add in VicPol unit LGAs and VicPol hierarchy
# Extract individual units
comp <-readRDS("Output.data/data.all.variables.RDS")

unidat19 <- read_xlsx("Data/Victoria Police Search Data 2018 and 2019.xlsx",
                      sheet = "FINAL_DATA",
                      .name_repair = "universal")


dat <- unidat19

#remove columns where all values are NA
dat <- dat[,colSums(is.na(dat))<nrow(dat)]


#change formats of variables to keep
dat <- dat %>%
  mutate(Contact.ID = as.character(Contact.ID),
        Year = format(as.Date(Contact.Date), "%Y"))%>%
  #rename variables
  rename(Gender = Sex,
        Age = Age.of.Contact,
        Reporting.Station.Description = Reporting.Station,
        FieldContactID = Contact.ID,
        Rank.of.Member = Contacting.Member.Rank)


names(dat)
#remove all records relating to FPOs
exclude.fpo <- dat %>%
  filter(..001.FPO.INTENT.TO.CHARGE== TRUE| ..001.FPO.NO.INTENT.TO.CHARGE== TRUE)%>%
  pull(FieldContactID)

dat <- dat %>%
  filter(!FieldContactID %in% exclude.fpo)

#harmonise all logical fields to logic
dat <- dat %>%
  mutate_if(is.numeric, as.logical)

#get table of person ID and search type
powers <-subset(names(dat),grepl("001", names(dat)))

#set values for Legislative powers based on substring
search.dat1 <- dat %>%
  select(FieldContactID, all_of(powers))%>%
  mutate(FieldContactID = as.character(FieldContactID))%>%
  pivot_longer(!FieldContactID, names_to = "Legislative.power", values_to = "Power")%>%
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


#Create short name for search type
search.dat <- search.dat1 %>%
  filter(Power == TRUE)%>%
  mutate(Search.type = case_when(
    str_detect(Legislative.power, "CONTROL") ~ "Weapons",
    str_detect(Legislative.power, "DP") ~ "Drugs",
    str_detect(Legislative.power, "FIREARMS") ~ "Firearms",
    str_detect(Legislative.power, "VOLATILE") ~ "Volatile inhalation substance",
    str_detect(Legislative.power, "GRAFFITI") ~ "Graffiti",
    TRUE ~ "Unknown"
  ))%>%
  #Create unique person and Search type ID
  mutate(Psn.Search.ID = str_c(FieldContactID, Search.type, sep = " - "))%>%
  select(-Power)


#link search ID and search type back to data
dat.s <- dat %>%
  left_join(search.dat, by = "FieldContactID")%>%
  mutate(Search.power.missing = ifelse(is.na(Psn.Search.ID), "Search info missing", "Not missing"))


#Check if item searched for is found
dat.s <- dat.s %>%
  mutate(Search.items.found = case_when(
    Search.type == "Weapons" & (DANGEROUS.ARTICLES == "Yes"|
      CONTROLLED.WEAPONS == "Yes"|
      PROHIBITED.WEAPONS == "Yes") ~ 1,
    Search.type == "Firearms" & FIREARMS == "Yes" ~ 1,
    Search.type == "Drugs" & OTHER.ARTICLES == "Yes" ~ 1,
    Search.type == "Graffiti" & GRAFFITI.IMPLEMENTS == "Yes" ~ 1,
    Search.type == "Volatile inhalation substance" & VOLATILE.SUBSTANCES.TYPES == "Yes" ~ 1,
    TRUE ~ 0))

#Check if any contraband was found
dat.s <- dat.s %>%
  mutate(Any.items.found = case_when(
    DANGEROUS.ARTICLES == "Yes"|
    CONTROLLED.WEAPONS == "Yes"|
    PROHIBITED.WEAPONS == "Yes"|
    FIREARMS == "Yes"|
    OTHER.ARTICLES == "Yes"|
    GRAFFITI.IMPLEMENTS == "Yes"|
    VOLATILE.SUBSTANCES.TYPES == "Yes" ~ 1,
  TRUE ~ 0))

#Explore contact type
#get colums with contact type details
contact <-subset(names(dat.s),grepl("Contact.Type", names(dat.s)))

#If one of the contact types is vehicle check then V, otherwise P
dat.s$Contact.Type <- ifelse(rowSums(dat.s[,contact]=="VEHICLE CHECK", na.rm = T) >= 1, "V","P")

#Set racial fields
#Aggregate race and identify missing
dat.s <- dat.s %>%
  mutate(Racial.appearance = case_when(
    str_detect(Racial.Appearance, "CAUC") ~ "White",
    str_detect(Racial.Appearance, "ABORIGINAL") ~ "Aboriginal",
    str_detect(Racial.Appearance, "AFRICAN") ~ "African",
    Racial.Appearance == "ASIAN" ~ "Asian",
    str_detect(Racial.Appearance, "INDIAN") ~ "South Asian",
    str_detect(Racial.Appearance, "MIDDLE") ~ "Middle Eastern/Med",
    Racial.Appearance == "MAORI" ~ "Pacific Islander",
    str_detect(Racial.Appearance, "PACIFIC") ~ "Pacific Islander",
    Racial.Appearance == "ARAB" ~  "Middle Eastern/Med",
    Racial.Appearance == "BLACK" ~ "African",
    Racial.Appearance == "SOUTH/EUROPE" ~  "Middle Eastern/Med",
    Racial.Appearance == "NORTH/EUROPE" ~ "White",
    Racial.Appearance %in% c("SOUTH AMERICAN", "UNDETERMINED") ~ "Other",
    TRUE ~ "Other"))

dat.s <- dat.s %>%
  mutate(Racial.appearance.missing = ifelse(is.na(Racial.Appearance), "Missing", "Not missing"))%>%
  mutate(VicPol.racialised = case_when(
    Racial.appearance == "White" ~ "Not-racialised",
    Racial.appearance.missing == "Missing" ~ "Missing",
    TRUE ~ "Racialised"
  ))

dat.s <- #Assign unit types
dat.s <- dat.s %>%
  mutate(
    Unit.type = case_when(
      str_detect(str_to_upper(Reporting.Station.Description), 'UNI') ~ 'Uniform',
      str_detect(str_to_upper(Reporting.Station.Description), 'TRANSIT') & !str_detect(str_to_upper(Reporting.Station.Description), 'PSO') ~ 'Transit',
      str_detect(str_to_upper(Reporting.Station.Description), 'PSO') ~ 'PSO',
      str_detect(str_to_upper(Reporting.Station.Description), 'CIU') ~ 'CIU',
      str_detect(str_to_upper(Reporting.Station.Description), 'DRU') ~ 'DRU',
      str_detect(str_to_upper(Reporting.Station.Description), 'HIGHWAY PATROL') | str_detect(str_to_upper(Reporting.Station.Description), 'HWY PATROL') ~ 'Highway Patrol',
      TRUE ~ 'Crime' # default to 'Crime' for empty or unmatched cases
    )
  )

fin.dat.2 <- dat.s %>%
  select(Year, Contact.Date, Contact.Time, FieldContactID, Psn.Search.ID, Legislative.power, Search.type, 
         Search.items.found, Any.items.found, 
         Racial.appearance, Racial.Appearance.original = Racial.Appearance, VicPol.racialised, Racial.appearance.missing,
         Gender, Age, Reporting.Station.Description, Rank.of.Member)


saveRDS(fin.dat.2, "Output.data/data.18.19.wrangled.RDS")