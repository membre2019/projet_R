library(readr)

countries <- read_csv("C:/Users/joris/OneDrive/Documents/Master/Semestre_2/SEP/projet/countries.csv", 
         col_types = cols(Agriculture = col_number(), 
         `Arable (%)` = col_number(), `Coastline (coast/area ratio)` = col_number(), 
         `Crops (%)` = col_number(), Industry = col_number(), 
         `Net migration` = col_number(), `Phones (per 1000)` = col_number(), 
         Service = col_number()))

View(countries)
