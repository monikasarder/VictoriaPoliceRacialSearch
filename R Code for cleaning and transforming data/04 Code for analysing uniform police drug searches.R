library(tidyverse)
library(forcats)
library(writexl)
library(purrr)

# Read in cleaned data
dat <- readRDS("./Output.data/Clean.search.data.RDS")

# Filter: uniform AND person searches AND drug searches AND Melbourne
loc.racial <- dat %>%
  filter(
    !is.na(Station.uniform),
    Contact.Type == "P",
    !is.na(`Search.type - Drugs`),
    Area.type == "Metro"
  ) %>%
  group_by(Division, Station.uniform) %>%
  mutate(
    `Searches: All drug`     = n(),
    `Finds: All drug`        = sum(Found == "Yes", na.rm = TRUE),
    `Hit rate (%): All drug` = round(`Finds: All drug` / `Searches: All drug` * 100, digits = 0)
  ) %>%
  group_by(
    Division, Station.uniform, 
    `Searches: All drug`, `Finds: All drug`, `Hit rate (%): All drug`, 
    Racial.appearance.transformed
  ) %>%
  summarise(
    Finds    = sum(Found == "Yes", na.rm = TRUE),
    Searches = n(),
    .groups  = "drop"
  ) %>%
  mutate(
    `Hit rate (%)` = round(Finds / Searches * 100, digits = 1),
    `Hit rate (%)` = ifelse(Searches <= 20, NA, `Hit rate (%)`),
    `Hit rate (%)` = ifelse(
      Racial.appearance.transformed == "Mediterranean and Middle Eastern - DO NOT USE",
      NA,
      `Hit rate (%)`
    )
  )
 
# Get racial groups in alphabetical order
racial_order <- c("White", "African", "Aboriginal", "Asian", "South Asian", "Middle Eastern", 
                  "Pacific Islander", "Other", "Missing", "Mediterranean/Middle Eastern – Unusable")

# Construct ordered list of pivoted column names
col_order <- map(racial_order, function(race) {
  c(
    paste0("Searches: ", race),
    paste0("Finds: ", race),
    paste0("Hit rate (%): ", race)
  )
}) %>% unlist()

# Identify non-pivoted columns (i.e. not in values_from or names_from)
non_pivot_cols <- setdiff(names(loc.racial), c("Racial.appearance.transformed", "Searches", "Finds", "Hit rate (%)"))

# Pivot and reorder columns: first original columns, then pivoted columns
loc.racial.wide <- loc.racial %>%
  pivot_wider(
    names_from = Racial.appearance.transformed,
    values_from = c(Searches, Finds, `Hit rate (%)`),
    names_glue = "{.value}: {Racial.appearance.transformed}"
  ) %>%
  select(all_of(non_pivot_cols), all_of(col_order))%>%
  mutate(`Relative likelihood: African` = round(((`Hit rate (%): White`-`Hit rate (%): African`)/`Hit rate (%): African`)*100, 
                                                digits = 1 ))


######################
#LOCAL GOVERNMENT AREA
######################

# Filter: uniform AND person searches AND drug searches AND Melbourne
lga.racial <- dat %>%
  filter(
    !is.na(Station.uniform),
    Contact.Type == "P",
    !is.na(`Search.type - Drugs`),
    Area.type == "Metro"
  ) %>%
  group_by(Local.Government.Area) %>%
  mutate(
    `Searches: All drug`     = n(),
    `Finds: All drug`        = sum(Found == "Yes", na.rm = TRUE),
    `Hit rate (%): All drug` = round(`Finds: All drug` / `Searches: All drug` * 100, digits = 0)
  ) %>%
  group_by(
    Local.Government.Area,
    `Searches: All drug`, `Finds: All drug`, `Hit rate (%): All drug`, 
    Racial.appearance.transformed
  ) %>%
  summarise(
    Finds    = sum(Found == "Yes", na.rm = TRUE),
    Searches = n(),
    .groups  = "drop"
  ) %>%
  mutate(
    `Hit rate (%)` = round(Finds / Searches * 100, digits = 1),
    `Hit rate (%)` = ifelse(Searches <= 20, NA, `Hit rate (%)`),
    `Hit rate (%)` = ifelse(
      Racial.appearance.transformed == "Mediterranean and Middle Eastern - DO NOT USE",
      NA,
      `Hit rate (%)`
    )
  )


non_pivot_cols <- setdiff(names(lga.racial), c("Racial.appearance.transformed", "Searches", "Finds", "Hit rate (%)"))

# Pivot and reorder columns: first original columns, then pivoted columns
lga.racial.wide <- lga.racial %>%
  pivot_wider(
    names_from = Racial.appearance.transformed,
    values_from = c(Searches, Finds, `Hit rate (%)`),
    names_glue = "{.value}: {Racial.appearance.transformed}"
  ) %>%
  select(all_of(non_pivot_cols), all_of(col_order)) %>%
  mutate(`Relative likelihood: African` = round(((`Hit rate (%): White`-`Hit rate (%): African`)/`Hit rate (%): African`)*100, 
         digits = 1 ))
         