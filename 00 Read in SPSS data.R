library(foreign)
library(writexl)
data <- read.spss("SPSS.code/Data.sav", to.data.frame=TRUE)


counts <- data %>%
  count(RacialCat, Racial.Appearance, Racialiased)

write_xlsx(counts, "Output.data/Racial.mapping.18.19.xlsx")
