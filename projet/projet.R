library(readr)

# Chargement des données
countries <- read_csv("C:/Users/joris/OneDrive/Documents/Master/Semestre_2/SEP/projet/countries.csv", locale = locale(decimal_mark = ","))
View(countries)

# Suppression des colonnes les moins informatives
cols.dont.want <- c("Population", "Region", "Other (%)", "Climate", "Arable (%)", "Crops (%)", "Phones (per 1000)", "Area (sq. mi.)") 
data <- countries[, ! names(countries) %in% cols.dont.want, drop = F]
data

# Corrélation entre la migration et les autres variables
cor(subset(data, select=-c(1)), data["Net migration"], use="complete.obs")

# Suppression des lignes contenant des valeurs NA
data = data[complete.cases(data), ]
data
