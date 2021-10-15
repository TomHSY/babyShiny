# Importation des données
data <- read.table("inst/shinyapp/data/bb.txt",
                   dec = ",",
                   sep="\t",
                   header = TRUE,
                   stringsAsFactors = T,
                   na.strings = c('NA',''))


data_corr = data[,-c(1,3:9,11,15,22,25:27)]
data_corr = na.omit(data_corr)
data_corr = data_corr[data_corr$ModeAccouc!='Autre',]
data_corr$ModeAccouc = droplevels(data_corr$ModeAccouc)

# Création d'une liste avec les variables quantitatives pour l'input utilisateur
ylist <- colnames(data)[which(sapply(data,is.numeric))]

values <- c("Nbsem",
            "Agedelamere",
            "TailMere",
            "PoidsMere",
            "Agedupere",
            "TailPere",
            "PoidsPere",
            "NbGrossess",
            "NbEnfants",
            "NbIVG",
            "NbFC")

names <- c("Nombre de semaines de grossesse",
           "Age de la mere",
           "Taille de la mere (en cm)",
           "Poids de la mere (en kg)",
           "Age du pere",
           "Taille du pere (en cm)",
           "Poids du pere (en kg)",
           "Nombre de grossesses précédentes",
           "Nombre d'enfants de la mere",
           "Nombre d'IVG",
           "Nombre de fausses couches")
