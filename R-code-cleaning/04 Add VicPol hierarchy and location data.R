library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)

# ------------------------------------------------------------
# Purpose: Add in VicPol unit LGAs and VicPol hierarchy
# ------------------------------------------------------------

# Load 2018-19 and 2022-23 data
dat1 <- readRDS( "R-code-cleaning/Processed/data.18.19.wrangled.RDS")
dat2 <- readRDS( "R-code-cleaning/Processed/data.22.23.wrangled.RDS")

# Combine datasets
dat <- rbind(dat1, dat2) %>%
  mutate(Racial.appearance = ifelse(Racial.appearance == "Missing", NA, Racial.appearance))

# Strip station prefixes and postfixes
dat <- dat %>%
  mutate(Unit = toupper(str_remove(Reporting.Station.Description, "UNI-"))) %>%
  mutate(Unit = toupper(str_remove(Unit, " UNIFORM"))) %>%
  mutate(Unit = toupper(str_remove(Unit, "CIU-"))) %>%
  mutate(Unit = toupper(str_remove(Unit, " CIU"))) %>%
  mutate(Unit = toupper(str_remove(Unit, "DRU-"))) %>%
  mutate(Unit = toupper(str_remove(Unit, " DRU"))) %>%
  mutate(Unit = toupper(str_remove(Unit, "HIGHWAY PATROL-"))) %>%
  mutate(Unit = toupper(str_remove(Unit, " HIGHWAY PATROL"))) %>%
  mutate(Unit = toupper(str_remove(Unit, "SOCIT-"))) %>%
  mutate(Unit = toupper(str_remove(Unit, " SOCIT"))) %>%
  mutate(Unit = ifelse(Unit.type == "Uniform", Unit, ""))

# ------------------------------------------------------------
# Read PSA and LGA data
# ------------------------------------------------------------
psadat <- read_xlsx("./Primary datasets - VicPol Search/geographicclassification.xlsx",
                    skip = 12, .name_repair = "universal") %>%
  fill(names(.)) %>%
  filter(!is.na(Police.Service.Area), Police.Service.Area != "Police Service Area") %>%
  unique()

# ------------------------------------------------------------
# Read VicPol hierarchy data
# ------------------------------------------------------------
hierdat1 <- read_xlsx("./Primary datasets - VicPol Search/Victoria-Police-employee-numbers-June-2024.xlsx",
                      skip = 8, .name_repair = "universal") %>%
  select(1:2, 4, 6, 8, 9, 10)

names(hierdat1) <- c("Region", "Div", "PSA1", "Police", "PSO", "PCO", "VPS")

hierdat1[c("Division", "PSA")] <- str_split_fixed(hierdat1$Div, "  ", 2)

hierdat1 <- hierdat1 %>%
  mutate(across(c(PSA, Division), trimws)) %>%
  filter(!str_detect(Division, "Total") | is.na(Division)) %>%
  filter(!str_detect(Region, "TOTAL")) %>%
  filter(!Police %in% c("Police", "FTE")) %>%
  replace(., . == "", NA) %>%
  mutate(PSA = ifelse(is.na(PSA), PSA1, PSA)) %>%
  select(Region, Division, PSA, Police, PSO, PCO, VPS)

hierdat <- hierdat1 %>%
  mutate(PSA = gsub("-.*", "", PSA),
         PSA = str_remove(PSA, "PSA "),
         Police.Service.Area = str_remove(PSA, "Greater "),
         Police.Service.Area = str_replace_all(Police.Service.Area, c(
           "Moreland" = "Merribek",
           "Merri" = "Merri-bek",
           "Dandenong" = "Greater Dandenong",
           "La Trobe" = "Latrobe",
           "Melbourne East " = "Melbourne",
           "Melbourne West " = "Melbourne"))) %>%
  select(-PSA) %>%
  filter(!is.na(Police.Service.Area)) %>%
  select(Region, Division, Police.Service.Area) %>%
  unique()

# ------------------------------------------------------------
# Read and clean station-to-LGA data
# ------------------------------------------------------------
sta.lga <- read_excel("./Primary datasets - VicPol Search/Police.station.location.xlsx")

sta.lga1 <- sta.lga %>%
  mutate(LGA = gsub(" Shire Council.*$| City Council.*$| Rural.*$| Borough Council.*$", "", Municipality),
         LGA = ifelse(LGA == "Colac Otway", "Colac-Otway", LGA)) %>%
  filter(!str_detect(LGA, "Unincorporated")) %>%
  rename(Local.Government.Area = LGA)

# ------------------------------------------------------------
# Merge PSA and Region/Division hierarchy
# ------------------------------------------------------------
sta.lga2 <- sta.lga1 %>%
  left_join(psadat, by = "Local.Government.Area") %>%
  select(-Municipality)

sta.hier <- sta.lga2 %>%
  left_join(hierdat, by = "Police.Service.Area") %>%
  mutate(Unit = str_remove(Station, " POLICE STATION")) %>%
  select(Region, Division, Police.Service.Area, Local.Government.Area, Locality, Postcode, Unit) %>%
  ungroup()

# ------------------------------------------------------------
# Merge with main dataset
# ------------------------------------------------------------
dat <- dat %>%
  mutate(Unit = str_replace_all(Unit, c("ST KILDA" = "ST. KILDA", "ALTONA NORTH" = "ALTONA")))

dat.hier <- dat %>%
  left_join(sta.hier, by = "Unit")

categories <- read_xlsx("./Primary datasets - VicPol Search/Council-category-data.xlsx")

dat.hier <- dat.hier %>%
  left_join(categories, by = "Local.Government.Area") %>%
  mutate(Area.type = case_when(
    Category %in% c("Metropolitan", "Interface") ~ "Metro",
    Category %in% c("Large shire", "Regional", "Small shire") ~ "Regional",
    TRUE ~ NA_character_
  ),
  Found = ifelse(Any.items.found == 1, "Yes", "No")) %>%
  filter(!is.na(Legislative.power))

# ------------------------------------------------------------
# Pivot search powers — one row per search
# ------------------------------------------------------------
dat.sr <- dat.hier %>%
  mutate(Search.items.found = as.character(Search.items.found),
         Search.items.found = case_when(
           Search.items.found == 1 ~ "Search item found",
           Search.items.found == 0 ~ "Nothing found",
           TRUE ~ "Not search basis")) %>%
  select(-Search.type, -Psn.Search.ID) %>%
  pivot_wider(names_from = Legislative.power, values_from = Search.items.found) %>%
  unique()

# ------------------------------------------------------------
# Rename search powers
# ------------------------------------------------------------
dat.sr1 <- dat.sr %>%
  rename(
    `Search.type - Drugs`             = `DP&CS S.82`,
    `Search.type - Weapons`           = `CONTROL OF WEAPONS ACT`,
    `Search.type - Firearms`          = `FIREARMS ACT`,
    `Search.type - Graffiti`          = `GRAFFITI PREVENTION ACT`,
    `Search.type - Volatile.sub.U18`  = `VOLATILE SUB U/18 60E`,
    `Search.type - Volatile.sub.adult`= `VOLATILE SUB 18+ 60F`
  )

# ------------------------------------------------------------
# Derive "Search item found" flag
# ------------------------------------------------------------
dat.sr2 <- dat.sr1 %>%
  mutate(Search.item.found = case_when(
    `Search.type - Drugs` == "Search item found" |
      `Search.type - Weapons` == "Search item found" |
      `Search.type - Firearms` == "Search item found" |
      `Search.type - Graffiti` == "Search item found" |
      `Search.type - Volatile.sub.U18` == "Search item found" |
      `Search.type - Volatile.sub.adult` == "Search item found" ~ "Search item found",
    TRUE ~ "Nothing found"
  ))

# ------------------------------------------------------------
# Clean racial appearance values
# ------------------------------------------------------------
dat.sr2 <- dat.sr2 %>%
  mutate(Racial.appearance.transformed = case_when(
    Racial.appearance == "Other" ~ "Other",
    is.na(Racial.appearance) ~ "Missing",
    Racial.appearance == "Mediterarranean/Mid" ~ "Mediterranean/Middle Eastern – Unusable",
    TRUE ~ Racial.appearance
  ))

# ------------------------------------------------------------
# Select and reorder columns
# ------------------------------------------------------------
dat.sr2 <- dat.sr2 %>%
  select(FieldReportID, FieldContactID, Year, Contact.Date, Contact.Time, Contact.Type,
         Racial.Appearance.original, Racial.appearance.transformed,
         Found, Search.item.found,
         `Search.type - Drugs`, `Search.type - Weapons`, `Search.type - Firearms`,
         `Search.type - Graffiti`, `Search.type - Volatile.sub.U18`, `Search.type - Volatile.sub.adult`,
         Indigenous.Status, Gender, Age, Complexion, Hair.Colour, Hair.Style,
         Reporting.Station.Description, Station.uniform = Unit,
         Unit.type, Rank.of.Member, Region, Division, Police.Service.Area,
         Area.type, Local.Government.Area, Locality, Postcode)

# ------------------------------------------------------------
# Update classifications where something was found but not searched for
# ------------------------------------------------------------
search.types <- c("Search.type - Drugs", "Search.type - Weapons", "Search.type - Firearms",
                  "Search.type - Graffiti", "Search.type - Volatile.sub.U18", "Search.type - Volatile.sub.adult")

dat.sr3 <- dat.sr2 %>%
  mutate(across(all_of(search.types),
                ~ ifelse(Found == "Yes" & .x == "Nothing found", "Non-search item found", .x))) %>%
  select(-Search.item.found)


# ------------------------------------------------------------
# Add more intuitive division labels
# ------------------------------------------------------------
division_lga_map <- c(
  "NWMR Division 1" = "Melbourne, Yarra",
  "NWMR Division 2" = "Hobsons Bay, Maribyrnong, Wyndham",
  "NWMR Division 3" = "Brimbank, Melton",
  "NWMR Division 4" = "Hume, Moonee Valley, Merri-bek",
  "NWMR Division 5" = "Banyule, Darebin, Nillumbik, Whittlesea",
  "Eastern Region Division 1" = "Boroondara, Manningham, Monash, Whitehorse",
  "Eastern Region Division 2" = "Knox, Maroondah, Yarra Ranges",
  "SMR Division 1" = "Port Phillip, Stonnington",
  "SMR Division 2" = "Glen Eira, Kingston", 
  "SMR Division 3" = "Cardinia, Casey, Dandenong",
  "SMR Division 4" = "Frankston, Mornington"
)

# Append LGAs in brackets to each Division value
dat.sr3 <- dat.sr3 %>%
  mutate(
    Division = ifelse(
      Division %in% names(division_lga_map),
      paste0(Division, " (", division_lga_map[Division], ")"),
      as.character(Division)
    )
  )

#Add age group

dat.sr3 <- dat.sr3 %>%
  mutate(`Age group` = case_when(
    is.na(Age) ~ "Missing",
    Age >= 2  & Age <= 11 ~ "Under 12",
    Age >= 12  & Age <= 17 ~ "12 to 17",
    Age >= 18 & Age <= 25 ~ "18 to 25",
    Age >= 26 & Age <= 35 ~ "26 to 35",
    Age >= 36 & Age <= 45 ~ "36 to 45",
    Age >= 46 ~ "46 and over",
    TRUE ~ "Missing"  # catch any unexpected cases
  ))

#

#Merge in CSA stats
csa <- readRDS( "R-code-cleaning/Processed/CSA rates.RDS")

csa <- csa %>%
  mutate(yearl = str_c(Local.Government.Area, as.character(Year), sep = " - "))%>%
  select(-Year, -Local.Government.Area)

dat.sr3 <-dat.sr3 %>%
  mutate(yearl = str_c(Local.Government.Area, as.character(Year), sep = " - "))

dat.sr3 <- dat.sr3 %>% 
  left_join(csa, by = "yearl")%>%
  select(-yearl)

# ------------------------------------------------------------
# Save outputs
# ------------------------------------------------------------
saveRDS(dat.sr3, "VicPol Search Data Clean/VicPol Search Data Clean.RDS")

wb <- loadWorkbook("VicPol Search Data Clean/VicPol Search Data Clean.xlsx")
writeData(wb, sheet = "Data", x = dat.sr3)
saveWorkbook(wb, "VicPol Search Data Clean/VicPol Search Data Clean.xlsx", overwrite = TRUE)




