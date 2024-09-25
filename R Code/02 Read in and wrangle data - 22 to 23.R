library(tidyverse)
library(readxl)
library(writexl)

## Read in 2022 and 2023 datasets

unidat23 <- read_xlsx("./Primary datasets - VicPol Search/Victoria Police Search Data 2023.xlsx",
                    .name_repair = "universal")

unidat23 <- unidat23 %>%
  rename(Racial.Appearance = Ethnic.Appearance,
         Quantity = Quantity.of.item.Found)


unidat22 <- read_xlsx("./Primary datasets - VicPol Search/Victoria Police Search Data 2022.xlsx",
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

#EXTRACT AND PRESERVE SEARCH TYPE AND ITEM FOUND DATA
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
items.vec <- c("CONTROLLED WEAPONS","DANGEROUS ARTICLES","PROHIBITED WEAPONS","FIREARMS","GRAFFITI IMPLEMENTS", "OTHER ARTICLE", 
               "VOLATILE SUBSTANCES TYPES","ITEMS USED - VOLATILE SUB")

#Match items found to legislative power for search
item.dat <- dat %>%
  mutate(Field.Contact.Search.Type = trimws(Field.Contact.Search.Type))%>%
  filter(Field.Contact.Search.Type %in% items.vec)%>%
  select(Item = Field.Contact.Search.Type, Field.Contact.Code.Description, FieldContactID, Quantity)%>%
  mutate(Search.type = case_when(
    Item %in% c("CONTROLLED WEAPONS","DANGEROUS ARTICLES","PROHIBITED WEAPONS") ~ "Weapons",
    Item == "OTHER ARTICLE" ~ "Drugs",
    Item == "FIREARMS" ~ "Firearms",
    Item %in% c("VOLATILE SUBSTANCES TYPES", "ITEMS USED - VOLATILE SUB") ~ "Volatile inhalation substance",
    Item == "GRAFFITI IMPLEMENTS" ~ "Graffiti",
    TRUE ~ Field.Contact.Code.Description
  ))%>%
  mutate(Psn.Search.ID = str_c(FieldContactID, Search.type, sep = " - "))%>%
  #If there is at least one item matching type then search item is found
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
  group_by(FieldContactID)%>%
  #If at least one item is found for person then item found
  mutate(Any.items.found = ifelse(sum(Quantity, na.rm = T)>=1, 1, 0))%>%
  select(FieldReportID, Rank.of.Member, Reporting.Station.Description, Year, Contact.Date, Contact.Time, 
         Contact.Type, FieldContactID, Racial.Appearance, Indigenous.Status, Gender, Age, Complexion, Hair.Colour, Hair.Style, Any.items.found)%>%
  unique()

search.dat <- person.id %>%
  left_join(search.dat, by = "FieldContactID")

#Assign unit types
fdat <- search.dat %>%
  mutate(
    Unit.type = case_when(
      str_detect(str_to_upper(Reporting.Station.Description), 'UNI') ~ 'Uniform',
      str_detect(str_to_upper(Reporting.Station.Description), 'TRANSIT') & !str_detect(str_to_upper(Reporting.Station.Description), 'PSO') ~ 'Transit',
      str_detect(str_to_upper(Reporting.Station.Description), 'PSO') ~ 'PSO',
      str_detect(str_to_upper(Reporting.Station.Description), 'CIU') ~ 'CIU',
      str_detect(str_to_upper(Reporting.Station.Description), 'DRU') ~ 'DRU',
      str_detect(str_to_upper(Reporting.Station.Description), 'HIGHWAY PATROL') | str_detect(str_to_upper(Reporting.Station.Description), 'HWY PATROL') ~ 'Highway Patrol',
      str_detect(str_to_upper(Reporting.Station.Description), "PUBLIC ORDER RESPONSE") ~ "Public Order Response",
      TRUE ~ 'Other' # default to 'Crime' for empty or unmatched cases
    )
  )

#Set NAs variables to char
fdat <- fdat %>%
  mutate(across(where(is.character), ~na_if(., ".")))

#Aggregate race and identify missing
fdat <- fdat %>%
   mutate(Racial.appearance = case_when(
    str_detect(Racial.Appearance, "CAUC") ~ "White",
    str_detect(Racial.Appearance, "ABORIGINAL") ~ "Aboriginal",
    str_detect(Racial.Appearance, "AFRICAN") ~ "African",
    Racial.Appearance == "ASIAN" ~ "Asian",
    Racial.Appearance == "INDIAN SUB-CONTINENTAL" ~ "South Asian",
    str_detect(Racial.Appearance, "MIDDLE") ~ "Middle Eastern/Med",
    str_detect(Racial.Appearance, "MAORI") ~ "Pacific Islander",
    Racial.Appearance == "SOUTH AMERICAN" ~ "South American",
    Racial.Appearance == "UNDETERMINED" ~ "Other",
    TRUE ~ "Missing"))



fdat <- fdat %>%
  mutate(Racial.appearance.missing = ifelse(Racial.appearance == "Missing", "Missing", "Not missing"))%>%
  mutate(VicPol.racialised = case_when(
    Racial.appearance == "White" ~ "Not-racialised",
    Racial.appearance.missing == "Missing" ~ "Missing",
    TRUE ~ "Racialised"
  ))

#reorder for sense
fin.dat <- fdat %>%
  select(FieldReportID, FieldContactID, Psn.Search.ID, Year, Contact.Date, Contact.Time,  Contact.Type,  Legislative.power, Search.type, 
         Search.items.found, Any.items.found, 
         Racial.appearance, Racial.Appearance.original = Racial.Appearance, VicPol.racialised, Racial.appearance.missing, Indigenous.Status,
         Gender, Age, Complexion, Hair.Colour, Hair.Style, Reporting.Station.Description, Unit.type, Rank.of.Member)



saveRDS(fin.dat, "Output.data/data.22.23.wrangled.RDS")

dat18 <- readRDS("./Output.data/data.18.19.wrangled.RDS")



