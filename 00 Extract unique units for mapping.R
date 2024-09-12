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

#write_xlsx(unidat, "Data/Units.xlsx")

# Read in Postcode and station

stationdat <- read_xlsx("Data/Police-station-contact-details-to-serve-court-documents_0.xlsx",
                        skip = 1,
                        .name_repair = "universal")

stationdat <- stationdat %>%
  select(Postcode, Police.station.contact.details)%>%
  unique()

# Read in Postcode and LGA
lgadat <- read_xlsx("Data/Postcode Import - Locality Finder - 7 June 2023.xlsx",
                        skip = 2,
                        .name_repair = "universal")

lgadat <- lgadat %>%
  select(Postcode = Post...Code, Municipality = Municipality...Name, LGA, Region = Region...Name)%>%
  unique()

# Read in PSA and LGA
psadat <- read_xlsx("Data/geographicclassification.xlsx",
                    skip = 12,
                    .name_repair = "universal")

psadat <- psadat %>%
  fill(Police.Region, Police.Service.Area) %>%
  filter(!is.na(Local.Government.Area), Local.Government.Area!="Local Government Area")
