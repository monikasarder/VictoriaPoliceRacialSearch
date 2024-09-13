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

#Set found variable
unidat <- unidat %>%
  mutate(
    Found = case_when(
      Quantity == 0 ~ 0,  # If Quantity is 0, set Found to 0
      Quantity >= 1 ~ 1    # If Quantity is 1 or more, set Found to 1
    )
  )%>%
  mutate(Found = as.factor(Found))

#Set ID variables to char
unidat <- unidat %>%
  mutate_at(c("FieldContactID", "FieldReportID"), as.character)

#Set NAs variables to char
unidat <- unidat %>%
  mutate(across(where(is.character), ~na_if(., ".")))%>%
  rename(Field.Contact.Search.Type = Field.Contact.Search.Type...10)

#remove redundant
unidat <- unidat %>%
  select(-FieldContactID, -Contact.Time, -Field.Contact.Search.Type...9,
         -Field.Contact.Code, -Hair.Colour, -Hair.Style, -Complexion)

saveRDS(unidat, "Output.data/wrangled.search.data.RDS")

