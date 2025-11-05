#remotes::install_github("runapp-aus/strayr")
library(sf)
library(strayr)
read_absmap("sa32021")



load("Melbourne.wrangled.RDS")

getwd()

map <-
  sa32021 %>%
  filter(gcc_name_2016 == "Greater Melbourne") %>%   # let's just look Melbourne
  ggplot() +
  geom_sf(aes(geometry = geometry))  # use the geometry variable

map