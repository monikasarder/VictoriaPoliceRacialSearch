library(tidyverse)
library(readxl)
library(writexl)

#Purpose: add in VicPol unit LGAs and VicPol hierarchy
# Extract individual units

unidat <- readRDS("Output.data/wrangled.search.data.RDS")

unidat <- unidat %>%
#  filter(str_detect(Reporting.Station.Description, "UNI"))%>%
  mutate(Sta.key = toupper(str_remove(Reporting.Station.Description, "UNI-")))

# Read in PSA and LGA data
psadat <- read_xlsx("Data/geographicclassification.xlsx",
                    skip = 12,
                    .name_repair = "universal")

psadat <- psadat %>%
  fill(Police.Region, Police.Service.Area) %>%
  filter(!is.na(Local.Government.Area), Local.Government.Area!="Local Government Area")

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

hierdat <- hierdat %>%
  mutate(Police.Service.Area = str_remove(PSA, "Greater "))%>%
  mutate(Police.Service.Area = str_replace_all(Police.Service.Area,
                                               c("Moreland" = "Merribek",
                                                 "Merri" = "Merri-bek",
                                                 "Dandenong" = "Greater Dandenong",
                                                 "La Trobe" = "Latrobe",
                                                 "Melbourne East " = "Melbourne",
                                                 "Melbourne West " = "Melbourne")))%>%
  select(-PSA)%>%
  filter(!(is.na(Police.Service.Area))) %>%
  mutate_at(c('PSO','PCO', 'Police','VPS'), as.numeric)

hierdat2<- hierdat %>%
  group_by(Police.Service.Area) %>% 
  mutate(across(where(is.numeric), sum))%>%
  ungroup()%>%
  unique()

  
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
  left_join(hierdat2, by = "Police.Service.Area")
  

sta.hier <- sta.hier %>%
  mutate(Sta.key = str_remove(Station, " POLICE STATION"))%>%
  select(Region, Division, Police.Service.Area, Local.Government.Area, Locality, Postcode, Sta.key)%>%
  mutate(Metro = ifelse(str_detect(Region, "Metro"), "Yes", "No"))%>%
  ungroup()

sta.hier.for.tamar <- sta.hier %>%
  mutate(Station = str_c("UNI-", Sta.key))%>%
  select(-Sta.key)%>%
  write_xlsx("Output.data/Station details.xlsx")

#Link hierarchy and location with search data
sta.hier <- unidat %>%
  left_join(sta.hier, by = "Sta.key")

saveRDS(sta.hier, "Output.data/Uniform.searches.with.hierarchy.RDS")


