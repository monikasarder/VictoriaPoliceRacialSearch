library(foreign)

data <- read.spss("SPSS.code/Data.sav", to.data.frame=TRUE)

table(data$Racial.Appearance)

