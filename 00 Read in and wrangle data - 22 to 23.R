library(tidyverse)
library(readxl)
library(writexl)

#Purpose: add in VicPol unit LGAs and VicPol hierarchy
# Extract individual units

unidat23 <- read_xlsx("Data/Victoria Police Search Data 2023.xlsx",
                    .name_repair = "universal")

unidat23 <- unidat23 %>%
  rename(Racial.Appearance = Ethnic.Appearance,
         Quantity = Quantity.of.item.Found)


unidat22 <- read_xlsx("Data/Victoria Police Search Data 2022.xlsx",
                      skip =18,
                    .name_repair = "universal")

#Join datasets
unidat <- rbind(unidat23, unidat22)

#add year and rename type
dat <- unidat %>%
  mutate(Year = format(as.Date(Contact.Date), "%Y"))%>%
  rename(Field.Contact.Search.Type = Field.Contact.Search.Type...10)

#get list of all persons with FPO
has.fpo <- dat %>%
  filter(str_detect(Field.Contact.Search.Type, "WITHOUT") & str_detect(Field.Contact.Code.Description, "FPO"))%>%
  pull(unique(FieldContactID))

#remove all FPO records
dat <- dat %>%
  filter(!FieldContactID %in% has.fpo)

combos <- dat %>%
  count(Field.Contact.Search.Type, Field.Contact.Code.Description)

counts <- dat %>%
  group_by(Field.Contact.Search.Type, Field.Contact.Code.Description)%>%
  summarise(Total = sum(Quantity, na.rm = T))


#MAKE SEARCH TYPE TABLE
#extract column of individual and search type combinations
#extract column of individual and search type combinations
search.dat <- dat %>%
     filter(Field.Contact.Search.Type == "SEARCH WITHOUT WARRANT TYPES")%>%
     mutate(Search.type = case_when(
         str_detect(Field.Contact.Code.Description, "CONTROL") ~ "Weapons",
         str_detect(Field.Contact.Code.Description, "DP&CS") ~ "Drugs",
         str_detect(Field.Contact.Code.Description, "FIREARMS") ~ "Firearms",
         str_detect(Field.Contact.Code.Description, "VOLATILE") ~ "Volatile inhalation substance",
         str_detect(Field.Contact.Code.Description, "GRAFFITI") ~ "Graffiti",
         TRUE ~ "Unknown"
       ))%>%
     select(Search.type, Legislative.power = Field.Contact.Code.Description, FieldContactID)%>%
     unique()%>%
     mutate(Psn.Search.ID = str_c(FieldContactID, Search.type, sep = " - "))

#MAKE ITEMS FOUND TABLE
#create item with Person search ID and whether an Item was found for that search
#extract columns with item counts
items.vec <- c("CONTROLLED WEAPONS","DANGEROUS ARTICLES","PROHIBITED WEAPONS","FIREARMS","GRAFFITI IMPLEMENTS", "OTHER ARTICLE", "VOLATILE SUBSTANCES TYPES")

item.dat <- dat %>%
  mutate(Field.Contact.Search.Type = trimws(Field.Contact.Search.Type))%>%
  filter(Field.Contact.Search.Type %in% items.vec)%>%
  select(Item = Field.Contact.Search.Type, Field.Contact.Code.Description, FieldContactID, Quantity)%>%
  mutate(Search.type = case_when(
    Item %in% c("CONTROLLED WEAPONS","DANGEROUS ARTICLES","PROHIBITED WEAPONS") ~ "Weapons",
    Item == "OTHER ARTICLE" ~ "Drugs",
    Item == "FIREARMS" ~ "Firearms",
    Item == "VOLATILE SUBSTANCES TYPES" ~ "Volatile inhalation substance",
    Item == "GRAFFITI IMPLEMENTS" ~ "Graffiti",
    TRUE ~ Field.Contact.Code.Description
  ))%>%
  mutate(Psn.Search.ID = str_c(FieldContactID, Search.type, sep = " - "))%>%
  group_by(Psn.Search.ID)%>%
  mutate(Search.items.found = ifelse(sum(Quantity, na.rm = T)>=1, 1, 0))%>%
  ungroup()%>%
  select(Psn.Search.ID, Search.items.found)%>%
  unique()

#Join found items to SEARCH TABLE
search.dat <- search.dat %>%
  left_join(item.dat, by = "Psn.Search.ID")%>%
  mutate(Search.items.found = ifelse(is.na(Search.items.found), 0, Search.items.found))



person.id <- dat %>%
  #exclude non contrand items with quantity listed
  filter(Field.Contact.Search.Type != "ITEMS USED - VOLATILE SUBS")%>%
  group_by(FieldContactID)%>%
  mutate(Any.items.found = ifelse(sum(Quantity, na.rm = T)>=1, 1, 0))%>%
  select(FieldReportID, Rank.of.Member, Reporting.Station.Description, Contact.Date, Contact.Time, 
         Contact.Type, FieldContactID, Racial.Appearance, Age, Gender, Year, Any.items.found)%>%
  unique()

search.dat <- person.id %>%
  left_join(search.dat, by = "FieldContactID")

#Assign unit types
dat <- search.dat %>%
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


#Set NAs variables to char
dat <- dat %>%
  mutate(across(where(is.character), ~na_if(., ".")))

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
  mutate(Racial.appearance.missing = ifelse(is.na(Racial.appearance), "Missing", "Not missing"))%>%
  mutate(Racialised.person = case_when(
    Racial.appearance == "White" ~ "Not-racialised",
    Racial.appearance.missing == "Missing" ~ "Missing",
    TRUE ~ "Racialised"
  ))

#reorder for sense
dat <- dat %>%
  select(Year, Contact.Date, Contact.Time,FieldReportID, FieldContactID, Psn.Search.ID, Legislative.power, Search.type, 
         Search.items.found, Any.items.found, 
         Racial.appearance, Racial.appearance.original = Racial.appearance, Racialised.person, Racial.appearance.missing,
         Gender, Age, Reporting.Station.Description, Rank.of.Member)
saveRDS(dat, "Output.data/data.all.variables.RDS")


check <- dat %>% filter(Search.items.found ==0 & Any.items.found == 1) %>%
  pull(FieldContactID)

