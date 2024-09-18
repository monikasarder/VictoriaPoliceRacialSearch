library(tidyverse)
library(readxl)
library(writexl)

#Purpose: add in VicPol unit LGAs and VicPol hierarchy
# Extract individual units

dat23 <- read_xlsx("Data/Victoria Police Search Data 2023.xlsx",
                    .name_repair = "universal")

dat23 <- dat23 %>%
  rename(Racial.Appearance = Ethnic.Appearance,
         Quantity = Quantity.of.item.Found)


dat22 <- read_xlsx("Data/Victoria Police Search Data 2022.xlsx",
                      skip =18,
                    .name_repair = "universal")

#join datasets
dat <- rbind(dat23, dat22)

#Year
dat <- dat %>%
  mutate(Year = format(as.Date(Contact.Date), "%Y"))

#Assign unit types
dat <- dat %>%
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

#Set ID variables to char
dat <- dat %>%
  mutate_at(c("FieldContactID", "FieldReportID"), as.character)

#Set NAs variables to char
dat <- dat %>%
  mutate(across(where(is.character), ~na_if(., ".")))%>%
  rename(Field.Contact.Search.Type = Field.Contact.Search.Type...10)



counts <- dat %>%
  count(Field.Contact.Search.Type, Field.Contact.Code.Description)

#Aggregate race and identify missing
dat <- dat %>%
   mutate(Racial.appearance = case_when(
    str_detect(Racial.Appearance, "CAUC") ~ "White",
    str_detect(Racial.Appearance, "ABORIGINAL") ~ "Aboriginal",
    str_detect(Racial.Appearance, "AFRICAN") ~ "African",
    Racial.Appearance %in% c("ASIAN","INDIAN SUB-CONTINENTAL") ~ "Asian",
    str_detect(Racial.Appearance, "MIDDLE") ~ "Middle Eastern",
    str_detect(Racial.Appearance, "MAORI") ~ "Pacific Islander",
    Racial.Appearance %in% c("SOUTH AMERICAN", "UNDETERMINED") ~ "Other",
    TRUE ~ Racial.Appearance))

dat <- dat %>%
  mutate(Racial.missing = ifelse(is.na(Racial.appearance), "Missing", "Not missing"))

#Identify search type - removed 88 ContactIDs where no reason identified
dat1 <- dat %>%
  mutate(Search.type = case_when(
    str_detect(Field.Contact.Search.Type, "WITHOUT") & str_detect(Field.Contact.Code.Description, "CONTROL") ~ "Weapons",
    str_detect(Field.Contact.Search.Type, "WITHOUT") & str_detect(Field.Contact.Code.Description, "DP&CS") ~ "Drugs",
    str_detect(Field.Contact.Search.Type, "WITHOUT") & str_detect(Field.Contact.Code.Description, "FIREARMS") ~ "Firearms",
    str_detect(Field.Contact.Search.Type, "WITHOUT") & str_detect(Field.Contact.Code.Description, "VOLATILE") ~ "Volatile inhalation substance",
    str_detect(Field.Contact.Search.Type, "WITHOUT") & str_detect(Field.Contact.Code.Description, "FPO") ~ "Firearm Prohibition Order",
    str_detect(Field.Contact.Search.Type, "WITHOUT") & str_detect(Field.Contact.Code.Description, "GRAFFITI") ~ "Graffiti",
  TRUE ~ "Unknown"
  ))

#Exclude FPO searches
weapons.search.contacts <- dat1 %>%
  filter(Search.type == "Firearm Prohibition Order")%>%
  pull(unique(FieldContactID))


dat1 <- dat1 %>%
  filter(!FieldContactID %in% weapons.search.contacts)

#check if anything was found on the person in relation to the search and summarise
found<- dat1 %>%
      group_by(FieldContactID)%>%
      summarise(Found = sum(Quantity, na.rm = T))%>%
      mutate(Found = ifelse(Found>=1, 1, Found))


#Set found variable
dat1 <- dat1 %>%
  left_join(found, by = "FieldContactID")%>%
  mutate(Found = as.factor(Found))

#Create ID for 
dat1 <- dat1 %>%
  group_by(FieldContactID)%>%
  filter(row_number()==1)%>%
  ungroup()

saveRDS(dat1, "Output.data/wrangled.search.data.RDS")


