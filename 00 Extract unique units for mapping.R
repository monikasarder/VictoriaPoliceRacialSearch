library(tidyverse)
library(readxl)
library(writexl)

#Purpose: add in VicPol unit hierarchy by linking 
#Police station and postcode to postcode to LGA to obtain LGA THEN
#LGA to Police station area
# Extract individual units

unidat <- read_xlsx("Data/Victoria Police Search Data 2023.xlsx",
                    .name_repair = "universal")

head(unidat)

unidat <- unidat %>%
  select(Reporting.Station.Description)%>%
  unique()

# Read in PSA and LGA

psadat <- read_xlsx("Data/geographicclassification.xlsx",
                    skip = 12,
                    .name_repair = "universal")

psadat <- psadat %>%
  fill(Police.Region, Police.Service.Area) %>%
  filter(!is.na(Local.Government.Area), Local.Government.Area!="Local Government Area")

# Read in VicPol hierarchy data

hierdat <- read_xlsx("Data/Victoria-Police-employee-numbers-June-2024.xlsx",
                    skip = 8,
                    .name_repair = "universal")

hierdat <-hierdat[,c(1:2,6,8,9,10)]

names(hierdat) <- c("Region","Div", "Police", "PSO", "PCO", "VPS")

hierdat[c("Division", "PSA")] <- str_split_fixed(hierdat$Div, "  ", 2)

hierdat <- hierdat %>%
  mutate(across(c(PSA, Division), trimws))

hierdat <- hierdat %>%
  select(Region, Division, PSA, Police, PSO, PCO, VPS)
         

hierdat <- hierdat %>%
  filter(!str_detect(Division, "Total")| is.na(Division))%>%
  filter(!str_detect(Region, "TOTAL")) %>%
  filter(!Police %in% c("Police", "FTE"))

hierdat <- replace(hierdat, hierdat == "", NA)

