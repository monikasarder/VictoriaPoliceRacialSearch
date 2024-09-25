library(tidyverse)
library(readxl)
library(writexl)

#Purpose: add in VicPol unit LGAs and VicPol hierarchy
# Extract individual units


dat1 <- readRDS("./Output.data/data.18.19.wrangled.RDS")

dat2 <- readRDS("./Output.data/data.22.23.wrangled.RDS")

#combine 2018-19 data with 2022-23 data

dat <- rbind(dat1,dat2)



# Assuming 'data' is your dataframe and 'ReportingStationDescription' is a column in that dataframe

table(dat$Reporting.Station.Description)
#Strip UNI prefix and create new unit name
dat <- dat %>%
  mutate(Unit= toupper(str_remove(Reporting.Station.Description, "UNI-")))%>%
  mutate(Unit= toupper(str_remove(Unit, " UNIFORM")))%>%
  mutate(Unit= toupper(str_remove(Unit, "CIU-")))%>%
  mutate(Unit= toupper(str_remove(Unit, " CIU")))%>%
  mutate(Unit= toupper(str_remove(Unit, "DRU-")))%>%
  mutate(Unit= toupper(str_remove(Unit, " DRU")))%>%
  mutate(Unit= toupper(str_remove(Unit, "HIGHWAY PATROL-")))%>%
  mutate(Unit= toupper(str_remove(Unit, " HIGHWAY PATROL")))%>%
  mutate(Unit= toupper(str_remove(Unit, "SOCIT-")))%>%
  mutate(Unit= toupper(str_remove(Unit, " SOCIT")))

# Read in PSA and LGA data
psadat <- read_xlsx("Secondary datasets/geographicclassification.xlsx",
                    skip = 12,
                    .name_repair = "universal")

psadat <- psadat %>%
  fill(names(psadat))%>%
  filter(!is.na(Police.Service.Area), Police.Service.Area != "Police Service Area")

psadat <- psadat %>%
  unique()


# Read in VicPol hierarchy data
hierdat1 <- read_xlsx("Secondary datasets/Victoria-Police-employee-numbers-June-2024.xlsx",
                    skip = 8,
                    .name_repair = "universal")


hierdat1 <-hierdat1[,c(1:2,4,6,8,9,10)]

names(hierdat1) <- c("Region","Div", "PSA1" ,"Police", "PSO", "PCO", "VPS")

hierdat1[c("Division", "PSA")] <- str_split_fixed(hierdat1$Div, "  ", 2)

hierdat1 <- hierdat1 %>%
  mutate(across(c(PSA, Division), trimws))

hierdat1 <- hierdat1 %>%
  filter(!str_detect(Division, "Total")| is.na(Division))%>%
  filter(!str_detect(Region, "TOTAL")) %>%
  filter(!Police %in% c("Police", "FTE"))

hierdat1 <- replace(hierdat1, hierdat1 == "", NA)

hierdat1 <- hierdat1 %>%
  mutate(PSA = ifelse(is.na(PSA), PSA1, PSA))

hierdat1 <- hierdat1 %>%
  select(Region, Division, PSA,  Police, PSO, PCO, VPS)

hierdat <- hierdat1 %>%
  mutate(PSA = gsub( "-.*", "", PSA))%>%
  mutate(PSA = str_remove(PSA, "PSA "))

hierdat1 <- hierdat %>%
  mutate(Police.Service.Area = str_remove(PSA, "Greater "))%>%
  mutate(Police.Service.Area = str_replace_all(Police.Service.Area,
                                               c("Moreland" = "Merribek",
                                                 "Merri" = "Merri-bek",
                                                 "Dandenong" = "Greater Dandenong",
                                                 "La Trobe" = "Latrobe",
                                                 "Melbourne East " = "Melbourne",
                                                 "Melbourne West " = "Melbourne")))%>%
  select(-PSA)%>%
  filter(!(is.na(Police.Service.Area)))%>%
  select(Region, Division, Police.Service.Area)%>%
  unique()# %>%
  #mutate_at(c('PSO','PCO', 'Police','VPS'), as.numeric)

  
#Read in station to LGA data
sta.lga <- read_excel("Secondary datasets/Police.station.location.xlsx")

sta.lga1 <- sta.lga %>%
  mutate(LGA = gsub(" Shire Council.*$", "", Municipality))%>%
  mutate(LGA = gsub(" City Council.*$", "", LGA))%>%
  mutate(LGA = gsub(" Rural.*$", "", LGA))%>%
  mutate(LGA = gsub(" Borough Council.*$", "", LGA))%>%
  filter(!str_detect(LGA, "Unincorporated"))%>%
  mutate(LGA = ifelse(LGA == "Colac Otway", "Colac-Otway", LGA))%>%
  rename(Local.Government.Area = LGA)


#Link in LGA and PSA
sta.lga2 <- sta.lga1 %>%
  left_join(psadat, by = "Local.Government.Area")%>%
  select(-Municipality)

#Link LGA with Region and Division
sta.hier <- sta.lga2 %>%
  left_join(hierdat1, by = "Police.Service.Area")
  

sta.hier <- sta.hier %>%
  mutate(Unit = str_remove(Station, " POLICE STATION"))%>%
  select(Region, Division, Police.Service.Area, Local.Government.Area, Locality, Postcode, Unit)%>%
  ungroup()


#Link hierarchy and location with search data

#rename two stations to enable join with hierarch
dat <- dat %>%
  mutate(Unit = str_replace_all(Unit, c("ST KILDA" = "ST. KILDA", "ALTONA NORTH" = "ALTONA")))

dat.hier <- dat %>%
  left_join(sta.hier, by = "Unit")

categories <- read_xlsx("Secondary datasets/Council-category-data.xlsx")

dat.hier <- dat.hier %>%
  left_join(categories, by = "Local.Government.Area")%>%
  mutate(Area.type = case_when(
    Category %in% c("Metropolitan", "Interface") ~ "Metro", 
    Category %in% c("Large shire", "Regional", "Small shire") ~ "Regional",
    TRUE ~ NA))


dat.hier <- dat.hier %>%
  mutate(Found = ifelse(Any.items.found == 1, "Yes", "No"))

dat.hier <- dat.hier %>%
  filter(!is.na(Legislative.power))

#saveRDS(dat.hier, "Output.data/Searches.with.hierarchy.RDS")

#create single row is single search data - pivot based on search power
dat.sr <- dat.hier %>%
  mutate(Search.items.found = as.character(Search.items.found))%>%
  mutate(Search.items.found = case_when(
    Search.items.found == 1 ~ "Search item found", 
    Search.items.found == 0 ~ "Nothing found",
    TRUE ~ "Not search basis")) %>%
  select(-Search.type, -Psn.Search.ID)%>% 
  pivot_wider(names_from = Legislative.power, values_from = Search.items.found)%>%
  unique()


#choose better names
dat.sr1 <- dat.sr %>%
  rename(`Search.type - Drugs` = `DP&CS S.82`,  `Search.type - Weapons`  = `CONTROL OF WEAPONS ACT`,
         `Search.type - Firearms` = `FIREARMS ACT`, `Search.type - Graffiti` = `GRAFFITI PREVENTION ACT`,
         `Search.type - Volatile.sub.U18` = `VOLATILE SUB U/18 60E`, `Search.type - Volatile.sub.adult` = `VOLATILE SUB 18+ 60F`)

table(dat.sr1$`Search.type - Drugs`)
#check if item for which a search occurred was found
dat.sr2 <- dat.sr1 %>%
     mutate(Search.item.found = case_when(
         `Search.type - Drugs` == "Search item found"|
           `Search.type - Weapons` == "Search item found"|
           `Search.type - Firearms` == "Search item found"|
             `Search.type - Graffiti`== "Search item found"|          
           `Search.type - Volatile.sub.U18`== "Search item found"|
           `Search.type - Volatile.sub.adult`== "Search item found" ~ "Search item found",
         TRUE ~"Nothing found")
       )


table(dat.sr2$Found, dat.sr2$Search.item.found)

#dat.sr2 <- dat.sr2 %>%
#  mutate(Search.item.found = 
#           ifelse(Search.item.found == "Nothing found" & Found == "Yes", "Non-search item found", Search.item.found))

#select final inclusions
dat.sr2 <- dat.sr2 %>%
  select(FieldReportID,FieldContactID, Year, Contact.Date, Contact.Time,  Contact.Type, 
         Racial.appearance, Racial.Appearance.original, 
         Found, Search.item.found,
         `Search.type - Drugs` ,  `Search.type - Weapons` ,
         `Search.type - Firearms` , `Search.type - Graffiti` ,
         `Search.type - Volatile.sub.U18` , `Search.type - Volatile.sub.adult`,
         Indigenous.Status,
         Gender, Age,   Complexion, Hair.Colour, Hair.Style, 
         Reporting.Station.Description, Unit.type, Rank.of.Member, Region, Division, Police.Service.Area, Area.type, Local.Government.Area, Locality,
         Postcode)


search.types <- c("Search.type - Drugs", "Search.type - Weapons","Search.type - Firearms",          
                  "Search.type - Graffiti" , "Search.type - Volatile.sub.U18","Search.type - Volatile.sub.adult")


dat.sr3 <- dat.sr2 %>%
  mutate(across(all_of(search.types), 
                ~ ifelse(Found == "Yes" & .x == "Nothing found",
                         "Non-search item found", .x)
                )
         )

dat.sr3 <- dat.sr3 %>%
  select(-Search.item.found)

write_xlsx(dat.sr2, "Output.data/VicPol Search data for analysis.xlsx")

values <- names(dat.sr2)

values <-as.data.frame(values)
#save data for analysis
saveRDS(dat.sr2, "Output.data/Clean.search.data.RDS")

write_xlsx(values, "Output.data/dictionary.xlsx")
