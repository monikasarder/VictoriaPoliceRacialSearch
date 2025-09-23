library(tidyverse)
library(readxl)
library(writexl)

# Read in 2018 and 2019 data
unidat19 <- read_xlsx(
  "./Primary datasets - VicPol Search/Victoria Police Search Data 2018 and 2019.xlsx",
  sheet = "FINAL_DATA",
  .name_repair = "universal"
)

dat <- unidat19

# Remove columns where all values are NA
dat <- dat[, colSums(is.na(dat)) < nrow(dat)]

# Change formats and rename columns
dat <- dat %>%
  mutate(
    Contact.ID = as.character(Contact.ID),
    Year       = format(as.Date(Contact.Date), "%Y")
  ) %>%
  rename(
    Gender                        = Sex,
    Age                           = Age.of.Contact,
    Reporting.Station.Description = Reporting.Station,
    FieldContactID                = Contact.ID,
    Rank.of.Member                = Contacting.Member.Rank
  )

# Remove all records relating to FPOs
exclude.fpo <- dat %>%
  filter(..001.FPO.INTENT.TO.CHARGE == TRUE | ..001.FPO.NO.INTENT.TO.CHARGE == TRUE) %>%
  pull(FieldContactID)

dat <- dat %>%
  filter(!FieldContactID %in% exclude.fpo)

# Harmonise all numeric logical fields to logical type
dat <- dat %>%
  mutate_if(is.numeric, as.logical)

# Extract search type columns
powers <- subset(names(dat), grepl("001", names(dat)))

# Preserve search type data and finds
search.dat1 <- dat %>%
  select(FieldContactID, all_of(powers)) %>%
  mutate(FieldContactID = as.character(FieldContactID)) %>%
  pivot_longer(
    !FieldContactID,
    names_to = "Legislative.power",
    values_to = "Power"
  ) %>%
  mutate(
    Legislative.power = case_when(
      str_detect(Legislative.power, "WEAPONS")    ~ "CONTROL OF WEAPONS ACT",
      str_detect(Legislative.power, "DP")         ~ "DP&CS S.82",
      str_detect(Legislative.power, "FPO")        ~ "FPO",
      str_detect(Legislative.power, "GRAFFITI")   ~ "GRAFFITI PREVENTION ACT",
      str_detect(Legislative.power, "FIREARMS")   ~ "FIREARMS ACT",
      str_detect(Legislative.power, "SUB.18")     ~ "VOLATILE SUB 18+ 60F",
      str_detect(Legislative.power, "SUB.U")      ~ "VOLATILE SUB U/18 60E",
      TRUE ~ Legislative.power
    )
  )

# Create short name for search type
search.dat <- search.dat1 %>%
  filter(Power == TRUE) %>%
  mutate(
    Search.type = case_when(
      str_detect(Legislative.power, "CONTROL")   ~ "Weapons",
      str_detect(Legislative.power, "DP")        ~ "Drugs",
      str_detect(Legislative.power, "FIREARMS")  ~ "Firearms",
      str_detect(Legislative.power, "VOLATILE")  ~ "Volatile inhalation substance",
      str_detect(Legislative.power, "GRAFFITI")  ~ "Graffiti",
      TRUE ~ "Unknown"
    ),
    Psn.Search.ID = str_c(FieldContactID, Search.type, sep = " - ")
  ) %>%
  select(-Power)

# Link search ID and type back to dataset
dat.s <- dat %>%
  left_join(search.dat, by = "FieldContactID") %>%
  mutate(
    Search.power.missing = ifelse(is.na(Psn.Search.ID), "Search info missing", "Not missing")
  )

# Identify if item searched for is found
dat.s <- dat.s %>%
  mutate(
    Search.items.found = case_when(
      Search.type == "Weapons" & (
        DANGEROUS.ARTICLES == "Yes" |
          CONTROLLED.WEAPONS == "Yes" |
          PROHIBITED.WEAPONS == "Yes"
      ) ~ 1,
      Search.type == "Firearms" & FIREARMS == "Yes" ~ 1,
      Search.type == "Drugs" & OTHER.ARTICLES == "Yes" ~ 1,
      Search.type == "Graffiti" & GRAFFITI.IMPLEMENTS == "Yes" ~ 1,
      Search.type == "Volatile inhalation substance" & (
        VOLATILE.SUBSTANCES.TYPES == "Yes" |
          ITEMS.USED.VOLATILE.SUBS == "Yes"
      ) ~ 1,
      TRUE ~ 0
    )
  )

# Identify if any contraband was found
dat.s <- dat.s %>%
  mutate(
    Any.items.found = case_when(
      DANGEROUS.ARTICLES == "Yes" |
        CONTROLLED.WEAPONS == "Yes" |
        PROHIBITED.WEAPONS == "Yes" |
        FIREARMS == "Yes" |
        OTHER.ARTICLES == "Yes" |
        GRAFFITI.IMPLEMENTS == "Yes" |
        VOLATILE.SUBSTANCES.TYPES == "Yes" |
        ITEMS.USED.VOLATILE.SUBS == "Yes" ~ 1,
      TRUE ~ 0
    )
  )

# Determine contact type: if any row contains VEHICLE CHECK, assign "V", otherwise "P"
contact_cols <- subset(names(dat.s), grepl("Contact.Type", names(dat.s)))

dat.s$Contact.Type <- ifelse(
  rowSums(dat.s[, contact_cols] == "VEHICLE CHECK", na.rm = TRUE) >= 1,
  "V",
  "P"
)

# Set racial appearance fields
dat.s <- dat.s %>%
  mutate(
    Racial.appearance = case_when(
      str_detect(Racial.Appearance, "CAUC")                      ~ "White",
      str_detect(Racial.Appearance, "ABORIGINAL")               ~ "Aboriginal",
      str_detect(Racial.Appearance, "AFRICAN")                  ~ "African",
      Racial.Appearance == "ASIAN"                              ~ "Asian",
      str_detect(Racial.Appearance, "INDIAN")                   ~ "South Asian",
      Racial.Appearance == "MEDITERRANEAN/MIDDLE-EASTERN"       ~ "Mediterarranean/Mid",
      Racial.Appearance == "MIDDLE EASTERN"                     ~ "Middle Eastern",
      Racial.Appearance == "AFRICA/MIDEAST (DONT USE)"          ~ "Other",
      Racial.Appearance == "MAORI"                              ~ "Pacific Islander",
      str_detect(Racial.Appearance, "PACIFIC")                  ~ "Pacific Islander",
      Racial.Appearance == "ARAB"                               ~ "Middle Eastern",
      Racial.Appearance == "BLACK"                              ~ "African",
      Racial.Appearance == "SOUTH/EUROPE"                       ~ "White",
      Racial.Appearance == "NORTH/EUROPE"                       ~ "White",
      Racial.Appearance %in% c("LATIN AMERICAN", "SOUTH AMERICAN") ~ "South American",
      Racial.Appearance %in% c("UNDETERMINED", "UNKNOWN", "UNCLASSIFIED/OTHER RACE") ~ "Other",
      TRUE ~ "Missing"
    ),
    Racial.appearance.missing = ifelse(Racial.Appearance == "Missing", "Missing", "Not missing"),
    VicPol.racialised = case_when(
      Racial.appearance == "White" ~ "Not-racialised",
      Racial.appearance.missing == "Missing" ~ "Missing",
      TRUE ~ "Racialised"
    )
  )

# View racial group counts
dat.s %>% count(Racial.appearance)

# Assign unit types
dat.s <- dat.s %>%
  mutate(
    Unit.type = case_when(
      str_detect(str_to_upper(Reporting.Station.Description), "UNI") ~ "Uniform",
      str_detect(str_to_upper(Reporting.Station.Description), "TRANSIT") &
        !str_detect(str_to_upper(Reporting.Station.Description), "PSO") ~ "Transit",
      str_detect(str_to_upper(Reporting.Station.Description), "PSO") ~ "PSO",
      str_detect(str_to_upper(Reporting.Station.Description), "CIU") ~ "CIU",
      str_detect(str_to_upper(Reporting.Station.Description), "DRU") ~ "DRU",
      str_detect(str_to_upper(Reporting.Station.Description), "HIGHWAY PATROL") |
        str_detect(str_to_upper(Reporting.Station.Description), "HWY PATROL") ~ "Highway Patrol",
      str_detect(Reporting.Station.Description, "OPERATIONS RESPONSE") |
        str_detect(Reporting.Station.Description, "PUBLIC ORDER RESPONSE") ~ "Public Order Response",
      TRUE ~ "Other"
    )
  )

# Final data selection
fin.dat.2 <- dat.s %>%
  mutate(FieldReportID = NA) %>%
  select(
    FieldReportID, FieldContactID, Psn.Search.ID, Year, Contact.Date, Contact.Time, Contact.Type,
    Legislative.power, Search.type, Search.items.found, Any.items.found,
    Racial.appearance, Racial.Appearance.original = Racial.Appearance,
    VicPol.racialised, Racial.appearance.missing,
    Indigenous.Status = Indigeneous.Status, Gender, Age,
    Complexion = Complexion.of.contact,
    Hair.Colour = Hair.colour.of.contact,
    Hair.Style = Hair.style.1.of.contact,
    Reporting.Station.Description, Unit.type, Rank.of.Member
  )


# Save to RDS
saveRDS(fin.dat.2, "R-code-cleaning/Processed/data.18.19.wrangled.RDS")

