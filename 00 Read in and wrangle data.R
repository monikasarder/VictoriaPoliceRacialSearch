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

#join datasets
unidat <- rbind(unidat23, unidat22)

#Year
unidat <- unidat %>%
  mutate(Year = format(as.Date(Contact.Date), "%Y"))


#Assign unit types
unidat <- unidat %>%
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
found<- unidat %>%
  group_by(FieldContactID)%>%
  summarise(Found = sum(Quantity, na.rm = T))%>%
  mutate(Found = ifelse(Found>=1, 1, Found))


#Set found variable
unidat <- unidat %>%
  left_join(found, by = "FieldContactID")%>%
  mutate(Found = as.factor(Found))

#Set ID variables to char
unidat <- unidat %>%
  mutate_at(c("FieldContactID", "FieldReportID"), as.character)

#Set NAs variables to char
unidat <- unidat %>%
  mutate(across(where(is.character), ~na_if(., ".")))%>%
  rename(Field.Contact.Search.Type = Field.Contact.Search.Type...10)

#Aggregate race and identify missing
unidat <- unidat %>%
   mutate(Racial.appearance = case_when(
    str_detect(Racial.Appearance, "CAUC") ~ "White",
    str_detect(Racial.Appearance, "ABORIGINAL") ~ "Aboriginal",
    str_detect(Racial.Appearance, "AFRICAN") ~ "African",
    Racial.Appearance %in% c("ASIAN","INDIAN SUB-CONTINENTAL") ~ "Asian",
    str_detect(Racial.Appearance, "MIDDLE") ~ "Middle Eastern",
    str_detect(Racial.Appearance, "MAORI") ~ "Pacific Islander",
    Racial.Appearance %in% c("SOUTH AMERICAN", "UNDETERMINED") ~ "Other",
    TRUE ~ Racial.Appearance))

unidat <- unidat %>%
  mutate(Racial.missing = ifelse(is.na(Racial.appearance), "Missing", "Not missing"))



#remove redundant
#unidat <- unidat %>%
#  select(-FieldContactID, -Contact.Time, -Field.Contact.Search.Type...9,
#         -Field.Contact.Code, -Hair.Colour, -Hair.Style, -Complexion)


unidat <- unidat %>%
  group_by(FieldContactID)%>%
  filter(row_number()==1)%>%
  ungroup()

saveRDS(unidat, "Output.data/wrangled.search.data.RDS")



