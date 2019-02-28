library(readr)

countries <- read_csv("C:/Users/joris/OneDrive/Documents/Master/Semestre_2/SEP/projet/countries.csv", locale = locale(decimal_mark = ","))

View(countries)

typeof(countries)


cols.dont.want <- c("Region", "Other (%)", "Climate", "Arable (%)", "Crops (%)", "Phones (per 1000)", "Area (sq. mi.)") # if you want to remove multiple columns

data <- countries[, ! names(countries) %in% cols.dont.want, drop = F]
data
