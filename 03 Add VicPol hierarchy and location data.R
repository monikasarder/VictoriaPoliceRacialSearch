library(tidyverse)
library(readxl)
library(writexl)

#Purpose: add in VicPol unit LGAs and VicPol hierarchy
# Extract individual units


dat1 <- readRDS("Output.data/data.18.19.wrangled.RDS")

dat2 <- readRDS("Output.data/data.22.23.wrangled.RDS")

#combine 2018-19 data with 2022-23 data

dat <- rbind(dat1,dat2)



# Assuming 'data' is your dataframe and 'ReportingStationDescription' is a column in that dataframe

dat <- dat %>%
  mutate(Reporting.Station.Description = str_to_upper(Reporting.Station.Description))

dat <- dat %>%
  mutate(Unit.type = case_when(
      str_detect(Reporting.Station.Description, "UNI-") ~ "Uniform",
      str_detect(Reporting.Station.Description, "UNIFORM") ~ "Uniform",
      str_detect(Reporting.Station.Description, "TRANSIT") & !str_detect(Reporting.Station.Description, "PSO") ~ "Transit",
      str_detect(Reporting.Station.Description, "PSO") ~ "PSO",
      str_detect(Reporting.Station.Description, "CIU") ~ "CIU",
      str_detect(Reporting.Station.Description, "DRU") ~ "DRU",
      str_detect(Reporting.Station.Description, "HIGHWAY PATROL") | str_detect(Reporting.Station.Description, "HWY PATROL") ~ "Highway Patrol",
      str_detect(Reporting.Station.Description, "OPERATIONS RESPONSE") ~ "Public Order Response",
      str_detect(Reporting.Station.Description, "PUBLIC ORDER RESPONSE") ~ "Public Order Response",
      TRUE ~ "Other"  # Default value for all other cases
    )
  )


#Strip UNI prefix and create new unit name
dat <- dat %>%
  mutate(Unit= toupper(str_remove(Reporting.Station.Description, "UNI-")))%>%
  mutate(Unit= toupper(str_remove(Unit, " UNIFORM")))


# Read in PSA and LGA data
psadat <- read_xlsx("Data/geographicclassification.xlsx",
                    skip = 12,
                    .name_repair = "universal")

psadat <- psadat %>%
  fill(names(psadat))%>%
  filter(!is.na(Police.Service.Area), Police.Service.Area != "Police Service Area")

psadat <- psadat %>%
  unique()


# Read in VicPol hierarchy data
hierdat1 <- read_xlsx("Data/Victoria-Police-employee-numbers-June-2024.xlsx",
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
sta.lga <- read_excel("Data/Police.station.location.xlsx")

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
  mutate(Area.type = ifelse(str_detect(Region, "Metro"), "Metro", "Region"))%>%
  ungroup()

#Link hierarchy and location with search data

#rename two stations to enable join with hierarch
dat <- dat %>%
  mutate(Unit = str_replace_all(Unit, c("ST KILDA" = "ST. KILDA", "ALTONA NORTH" = "ALTONA")))

dat.hier <- dat %>%
  left_join(sta.hier, by = "Unit")

categories <- read_xlsx("Data/Council-category-data.xlsx")

dat.hier <- dat.hier %>%
  left_join(categories, by = "Local.Government.Area")%>%
  mutate(Area.type = ifelse(Category %in% c("Metropolitan", "Interface"), "Metro", "Regional"))

dat.hier <- dat.hier %>%
  mutate(Found = ifelse(Any.items.found == 1, "Yes", "No"))

dat.hier <- dat.hier %>%
  filter(!is.na(Legislative.power))

saveRDS(dat.hier, "Output.data/Searches.with.hierarchy.RDS")

