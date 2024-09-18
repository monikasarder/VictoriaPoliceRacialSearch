library(tidyverse)
library(readxl)
library(writexl)

#Purpose: add in VicPol unit LGAs and VicPol hierarchy
# Extract individual units
library(foreign)

datp <- read.spss("SPSS.code/Data.sav", to.data.frame=TRUE)

datp <- datp %>%
  rename(Field.Contact.ID = Contact.ID,  
         Reporting.Station.Description = Reporting.Station,
         Rank.of.Member = Contacting.Member.Rank,
         Gender = Sex,
         Age = Age.of.Contact)%>%
  mutate(Quantity = NA)


names(datp)
         "Contact.Date"                      "Contact.Time"                     
  [4] "Reporting.Station"                 "Contacting.Member.Rank"            "Age.of.Contact"                   
  [7] "Racial.Appearance"                 "Sex"                               "Person.Reports"                   
 [10] "Complexion.of.contact"             "Hair.colour.of.contact"            "Hair.style.1.of.contact"          
 [13] "Hair.style.2.of.contact"           "Indigeneous.Status"                "SEARCH.W.O.WARRANT.TYPES"     )

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

names(dat)
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


#check if anything was found on the person and summarise
found<- dat %>%
  group_by(FieldContactID)%>%
  summarise(Found = sum(Quantity, na.rm = T))%>%
  mutate(Found = ifelse(Found>=1, 1, Found))


#Set found variable
dat <- dat %>%
  left_join(found, by = "FieldContactID")%>%
  mutate(Found = as.factor(Found))

#Set ID variables to char
dat <- dat %>%
  mutate_at(c("FieldContactID", "FieldReportID"), as.character)

#Set NAs variables to char
dat <- dat %>%
  mutate(across(where(is.character), ~na_if(., ".")))%>%
  rename(Field.Contact.Search.Type = Field.Contact.Search.Type...10)

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



#remove redundant
#dat <- dat %>%
#  select(-FieldContactID, -Contact.Time, -Field.Contact.Search.Type...9,
#         -Field.Contact.Code, -Hair.Colour, -Hair.Style, -Complexion)


dat <- dat %>%
  group_by(FieldContactID)%>%
  filter(row_number()==1)%>%
  ungroup()

saveRDS(dat, "Output.data/wrangled.search.data.RDS")



