library(tidyverse)
library(readxl)
library(writexl)

# ────────────────────────────────────────────────────────────────────────────────
# Read in 2022,2023 and 2024 datasets
# ────────────────────────────────────────────────────────────────────────────────


unidat24 <- read_xlsx(
  "./Primary datasets - VicPol Search/Victoria Police Search Data 2024.xlsx",
  sheet = "Data FINAL",
  skip = 23,
  .name_repair = "universal"
) %>%
  rename(
    Racial.Appearance = Ethnic.Appearance,
    Quantity = Quantity.of.item.Found
  )


unidat23 <- read_xlsx(
  "./Primary datasets - VicPol Search/Victoria Police Search Data 2023.xlsx",
  .name_repair = "universal"
) %>%
  rename(
    Racial.Appearance = Ethnic.Appearance,
    Quantity = Quantity.of.item.Found
  )%>%
  mutate(Postcode = NA, LGA = NA)

unidat22 <- read_xlsx(
  "./Primary datasets - VicPol Search/Victoria Police Search Data 2022.xlsx",
  skip = 18,
  .name_repair = "universal"
)%>%
  mutate(Postcode = NA, LGA = NA)

# Combine datasets
unidat <- bind_rows(unidat24, unidat23, unidat22)

# ────────────────────────────────────────────────────────────────────────────────
# Clean and tag dataset
# ────────────────────────────────────────────────────────────────────────────────

dat <- unidat %>%
  mutate(
    Year = format(as.Date(Contact.Date), "%Y")
  ) %>%
  rename(Field.Contact.Search.Type = Field.Contact.Search.Type...10)

# Exclude all FPO records
has.fpo <- dat %>%
  filter(
    str_detect(Field.Contact.Search.Type, "WITHOUT") &
      str_detect(Field.Contact.Code.Description, "FPO")
  ) %>%
  pull(FieldContactID) %>%
  unique()

dat <- dat %>%
  filter(!FieldContactID %in% has.fpo)

# ────────────────────────────────────────────────────────────────────────────────
# Extract & label search types
# ────────────────────────────────────────────────────────────────────────────────

search.dat <- dat %>%
  filter(Field.Contact.Search.Type == "SEARCH WITHOUT WARRANT TYPES") %>%
  mutate(
    Search.type = case_when(
      str_detect(Field.Contact.Code.Description, "CONTROL")   ~ "Weapons",
      str_detect(Field.Contact.Code.Description, "DP&CS")     ~ "Drugs",
      str_detect(Field.Contact.Code.Description, "FIREARMS")  ~ "Firearms",
      str_detect(Field.Contact.Code.Description, "VOLATILE")  ~ "Volatile inhalation substance",
      str_detect(Field.Contact.Code.Description, "GRAFFITI")  ~ "Graffiti",
      TRUE ~ "Unknown"
    ),
    Psn.Search.ID = str_c(FieldContactID, Search.type, sep = " - ")
  ) %>%
  select(Search.type, Legislative.power = Field.Contact.Code.Description, FieldContactID, Psn.Search.ID) %>%
  unique()

# ────────────────────────────────────────────────────────────────────────────────
# Determine if items were found
# ────────────────────────────────────────────────────────────────────────────────

items.vec <- c(
  "CONTROLLED WEAPONS", "DANGEROUS ARTICLES", "PROHIBITED WEAPONS",
  "FIREARMS", "GRAFFITI IMPLEMENTS", "OTHER ARTICLE",
  "VOLATILE SUBSTANCES TYPES", "ITEMS USED - VOLATILE SUB"
)

item.dat <- dat %>%
  mutate(Field.Contact.Search.Type = trimws(Field.Contact.Search.Type)) %>%
  filter(Field.Contact.Search.Type %in% items.vec) %>%
  select(
    Item = Field.Contact.Search.Type,
    Field.Contact.Code.Description,
    FieldContactID,
    Quantity
  ) %>%
  mutate(
    Search.type = case_when(
      Item %in% c("CONTROLLED WEAPONS", "DANGEROUS ARTICLES", "PROHIBITED WEAPONS") ~ "Weapons",
      Item == "OTHER ARTICLE"                           ~ "Drugs",
      Item == "FIREARMS"                                ~ "Firearms",
      Item %in% c("VOLATILE SUBSTANCES TYPES", "ITEMS USED - VOLATILE SUB") ~ "Volatile inhalation substance",
      Item == "GRAFFITI IMPLEMENTS"                     ~ "Graffiti",
      TRUE ~ Field.Contact.Code.Description
    ),
    Psn.Search.ID = str_c(FieldContactID, Search.type, sep = " - ")
  ) %>%
  group_by(Psn.Search.ID) %>%
  summarise(Search.items.found = as.integer(sum(Quantity, na.rm = TRUE) >= 1), .groups = "drop") %>%
  unique()

# ────────────────────────────────────────────────────────────────────────────────
# Join found items to search data
# ────────────────────────────────────────────────────────────────────────────────

search.dat <- search.dat %>%
  left_join(item.dat, by = "Psn.Search.ID") %>%
  mutate(Search.items.found = replace_na(Search.items.found, 0))

# ────────────────────────────────────────────────────────────────────────────────
# Create person-level data
# ────────────────────────────────────────────────────────────────────────────────

person.id <- dat %>%
  group_by(FieldContactID) %>%
  mutate(Any.items.found = as.integer(sum(Quantity, na.rm = TRUE) >= 1)) %>%
  select(
    FieldReportID, Rank.of.Member, Reporting.Station.Description, Year, Contact.Date, Contact.Time,
    Contact.Type, FieldContactID, Racial.Appearance, Indigenous.Status, Gender, Age,
    Complexion, Hair.Colour, Hair.Style, Any.items.found, Postcode, LGA
  ) %>%
  unique()

search.dat <- person.id %>%
  left_join(search.dat, by = "FieldContactID")

# ────────────────────────────────────────────────────────────────────────────────
# Assign Unit Types
# ────────────────────────────────────────────────────────────────────────────────

fdat <- search.dat %>%
  mutate(
    Unit.type = case_when(
      str_detect(str_to_upper(Reporting.Station.Description), "UNI")       ~ "Uniform",
      str_detect(str_to_upper(Reporting.Station.Description), "TRANSIT") &
        !str_detect(str_to_upper(Reporting.Station.Description), "PSO")     ~ "Transit",
      str_detect(str_to_upper(Reporting.Station.Description), "PSO")       ~ "Protective Services Officer",
      str_detect(str_to_upper(Reporting.Station.Description), "CIU")       ~ "Criminal Investigation Unit",
      str_detect(str_to_upper(Reporting.Station.Description), "DRU")       ~ "Divisional Response Unit",
      str_detect(str_to_upper(Reporting.Station.Description), "HIGHWAY")   ~ "Highway Patrol",
      str_detect(str_to_upper(Reporting.Station.Description), "HWY")       ~ "Highway Patrol",
      str_detect(str_to_upper(Reporting.Station.Description), "PUBLIC ORDER RESPONSE")   ~ "Public Order Response",
      str_detect(str_to_upper(Reporting.Station.Description), "FAMILY VIOLENCE") ~ "Family Violence Investigation",
      str_detect(str_to_upper(Reporting.Station.Description), "SOCIT") ~ "Sexual Offences and Child Abuse Investigation Team",
      str_detect(str_to_upper(Reporting.Station.Description), "DIU") ~ "Divisional Intelligence Unit",
      str_detect(str_to_upper(Reporting.Station.Description), "VIPER") ~ "VIPER Taskforce",
      str_detect(str_to_upper(Reporting.Station.Description), "TASKFORCE") ~ "Other taskforces",
      str_detect(str_to_upper(Reporting.Station.Description), "COMMAND") ~ "Command",
      TRUE ~ "Other"
    )
  )

table(fdat$Unit.type)

# ────────────────────────────────────────────────────────────────────────────────
# Racial appearance harmonisation
# ────────────────────────────────────────────────────────────────────────────────

fdat <- fdat %>%
  mutate(across(where(is.character), ~ na_if(., "."))) %>%
  mutate(
    Racial.appearance = case_when(
      str_detect(Racial.Appearance, "CAUC")                 ~ "White",
      str_detect(Racial.Appearance, "ABORIGINAL")           ~ "Aboriginal",
      str_detect(Racial.Appearance, "AFRICAN")              ~ "African",
      Racial.Appearance == "ASIAN"                          ~ "Asian",
      Racial.Appearance == "INDIAN SUB-CONTINENTAL"         ~ "South Asian",
      str_detect(Racial.Appearance, "MIDDLE")               ~ "Mediterarranean/Mid",
      str_detect(Racial.Appearance, "MAORI")                ~ "Pacific Islander",
      Racial.Appearance == "SOUTH AMERICAN"                 ~ "South American",
      Racial.Appearance == "UNDETERMINED"                   ~ "Other",
      TRUE                                                  ~ "Missing"
    ),
    Racial.appearance.missing = ifelse(Racial.appearance == "Missing", "Missing", "Not missing"),
    VicPol.racialised = case_when(
      Racial.appearance == "White"      ~ "Not-racialised",
      Racial.appearance == "Missing"    ~ "Missing",
      TRUE                              ~ "Racialised"
    )
  )



# ────────────────────────────────────────────────────────────────────────────────
# Final dataset selection and export
# ────────────────────────────────────────────────────────────────────────────────

fin.dat <- fdat %>%
  select(
    FieldReportID, FieldContactID, Psn.Search.ID, Year, Contact.Date, Contact.Time, Contact.Type,
    Legislative.power, Search.type, Search.items.found, Any.items.found,
    Racial.appearance, Racial.Appearance.original = Racial.Appearance,
    VicPol.racialised, Racial.appearance.missing, Indigenous.Status,
    Gender, Age, Complexion, Hair.Colour, Hair.Style,
    Reporting.Station.Description, Unit.type, Rank.of.Member, Postcode, LGA
  )

saveRDS(fin.dat,  "R-code-cleaning/Processed/data.22.24.wrangled.RDS")
