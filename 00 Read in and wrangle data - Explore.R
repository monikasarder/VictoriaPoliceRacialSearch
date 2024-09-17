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

#extract column for types
act <- unidat %>%
  filter(Field.Contact.Search.Type...10 == "SEARCH WITHOUT WARRANT TYPES")%>%
  select(Field.Contact.Code.Description, FieldContactID)%>%
  unique()%>%
  rename(Legislation = Field.Contact.Code.Description)%>%
  group_by(FieldContactID)%>%
  filter(row_number()==1)%>%
  ungroup()

dat <- unidat %>%
  filter(Field.Contact.Search.Type...10 != "SEARCH WITHOUT WARRANT TYPES")

dat1 <- dat %>%
  left_join(act, by = "FieldContactID")


#extract column for actions
action <- unidat %>%
  filter(Field.Contact.Search.Type...10 == "ACTION TAKEN SRCH W/O WARRANT")%>%
  select(Field.Contact.Code.Description, FieldContactID)%>%
  unique()%>%
  rename(Action.taken = Field.Contact.Code.Description)%>%
  group_by(FieldContactID)%>%
  filter(row_number()==1)%>%
  ungroup()

dat2 <- dat1 %>%
  filter(Field.Contact.Search.Type...10 != "ACTION TAKEN SRCH W/O WARRANT")

dat2 <- dat2 %>%
  left_join(action, by = "FieldContactID")

counts <- dat2 %>%
  count(Field.Contact.Search.Type...10, Field.Contact.Code.Description)

#extract column for objects
objects <- dat2 %>%
  filter(Field.Contact.Search.Type...10 != "OBJECTS SEARCHED")%>%
  select(Field.Contact.Code.Description, FieldContactID, Field.Contact.Search.Type...10)%>%
  unique()%>%
  rename(Objects.found = Field.Contact.Code.Description, Object.search = Field.Contact.Search.Type...10)%>%
  group_by(FieldContactID)%>%
  filter(row_number()==1)%>%
  ungroup()

dat3 <- dat2 %>%
  filter(Field.Contact.Search.Type...10 == "OBJECTS SEARCHED")

dat3 <- dat3 %>%
  left_join(objects, by = "FieldContactID")

counts <- dat2 %>%
  count(Field.Contact.Search.Type...10, Field.Contact.Code.Description)




var <- names(data)
types <- unidat %>%
 filter(Field.Contact.Search.Type...10 == "SEARCH WITHOUT WARRANT TYPES")%>%
  select(Field.Contact.Search.Type...10, Field.Contact.Code.Description, FieldReportID, FieldContactID)%>%
  unique()
  
types <- unidat %>%
  filter(Field.Contact.Search.Type...10 == "SEARCH WITHOUT WARRANT TYPES")%>%
  select(FieldReportID, FieldContactID)%>%
  unique()

sum(unique(dat$FieldContactID))
sum(unique(dat$FieldReportID))

sum(unique(unidat$FieldContactID))
sum(unique(unidat$FieldReportID))

sum(unique(types$FieldContactID))
sum(unique(types$FieldReportID))

#Assign unit types
dat3 <- dat3 %>%
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
dat3 <- dat3 %>%
  mutate(
    Found = case_when(
      Quantity == 0 ~ 0,  # If Quantity is 0, set Found to 0
      Quantity >= 1 ~ 1    # If Quantity is 1 or more, set Found to 1
    )
  )%>%
  mutate(Found = as.factor(Found))

#Set ID variables to char
dat3 <- dat3 %>%
  mutate_at(c("FieldContactID", "FieldReportID"), as.character)

#Set NAs variables to char
dat3 <- dat3 %>%
  mutate(across(where(is.character), ~na_if(., ".")))%>%
  rename(Field.Contact.Search.Type = Field.Contact.Search.Type...10)

#Aggregate race and identify missing
dat3 <- dat3 %>%
   mutate(Racial.appearance = case_when(
    str_detect(Racial.Appearance, "CAUC") ~ "White",
    str_detect(Racial.Appearance, "ABORIGINAL") ~ "Aboriginal",
    str_detect(Racial.Appearance, "AFRICAN") ~ "African",
    Racial.Appearance %in% c("ASIAN","INDIAN SUB-CONTINENTAL") ~ "Asian",
    str_detect(Racial.Appearance, "MIDDLE") ~ "Middle Eastern",
    str_detect(Racial.Appearance, "MAORI") ~ "Pacific Islander",
    Racial.Appearance %in% c("SOUTH AMERICAN", "UNDETERMINED") ~ "Other",
    TRUE ~ Racial.Appearance))

dat3 <- dat3 %>%
  mutate(Racial.missing = ifelse(is.na(Racial.appearance), "Missing", "Not missing"))

table(dat3$Racial.missing)

#remove redundant
#dat3 <- dat3 %>%
#  select(-FieldContactID, -Contact.Time, -Field.Contact.Search.Type...9,
#         -Field.Contact.Code, -Hair.Colour, -Hair.Style, -Complexion)

length(unique(Field.Contact.ID))
search <- dat3 %>%
  filter(Field.Contact.Search.Type == "SEARCH WITHOUT WARRANT TYPES") 
saveRDS(dat3, "Output.data/wrangled.search.data.RDS")



