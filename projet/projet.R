library(readr)
library(dplyr)
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)
library(grid)
library(corrplot)

# Chargement des donn?es
countries <- read_csv("D:/Cours/Dossier_stat/countries.csv", locale = locale(decimal_mark = ","))
View(countries)
# Suppression des colonnes les moins informatives
cols.dont.want <- c("Population", "Other (%)", "Climate", "Arable (%)", "Crops (%)", "Phones (per 1000)", "Area (sq. mi.)") 
data <- countries[, ! names(countries) %in% cols.dont.want, drop = F]
View(data)



# Remplacement des Na par la moyenne des colonnes
data[] <- lapply(data, function(x) { 
  x[is.na(x)] <- round(mean(x, na.rm = TRUE), 3)
  x
})
View(data)
# Dictionnaire de donn?es
colnames(data)
# Pays par r?gion
data %>%
  select(Country, Region) %>%
  group_by(Region) %>%
  summarise(pays = n())


# Solde migratoire moyen par r?gion 
data %>%
  select(Region, `Net migration`) %>%
  group_by(Region) %>%
  dplyr::summarize(Moyenne = mean(`Net migration`)) %>%
  collect() %>% 
  ggplot() +
  geom_col(aes(x=Region, y=Moyenne)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))


# Boîte à moustache

boxplot(data$`Net migration`~ data$Region,
        main = 'Distribution du solde migratoire par Région', #Un titre au graphique
        xlab = 'Regions',
        ylab = 'Solde migratoire', #Un label pour l'axe
        outcol = 2, outpch = 4, cex=0.7)  #On change la forme et la couleur des outliers


net_migration_per_region <- data %>%
  select(Country, Region, `Net migration`) %>%
  group_by(Region) %>%
  dplyr::summarize(Moyenne_migratoire=mean(`Net migration`)) # on stocke dans l'objet net_migration_per_region
                                                             #les moyennes par groupe

#points(1:11,net_migration_per_region , pch = 4, col = 4)#On ajoute les moyennes par groupe
                                

abline(h = mean(data$`Net migration`, na.rm = TRUE),
       lty = 2, col = 4, lwd = 1)          #On ajoute la moyenne de l'ensemb


#legend("topleft", horiz = TRUE,
       legend = c("Outliers", "Moyennes des groupes", "Moyenne de l'ensembe"),
       lty = c(NA, NA, 2), col = c(2, 4, 4), pch = c(3,4, NA))  #On ajoute une boîte de légende


# Test de Bartlett pour la migration par rÃ©gion
bartlett.test(data$`Net migration` ~ data$Region)

# Test ANOVA
oneway.test(data$`Net migration` ~ data$Region, var.equal = FALSE)


# Corr?lation entre la migration et les autres vairables
correlation <- cor(data[,c(-1,-2,-5)], data[,5])
correlation



# Moyenne des variables (ordonnÃ©es par solde migratoire)
variables_mean <- data %>%
  select(c(-1,-6,-8,-9,-10,-12)) %>%
  group_by(Region) %>%
  summarise_all(funs(mean)) %>%
  arrange(desc(`Net migration`))
View(variables_mean)

