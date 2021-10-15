library(shiny)
library(graphics)
# Analyse factorielle
library(FactoMineR)
library(devtools)
library(factoextra)
# Graphiques
library(ggplot2)
library (cowplot)

#définition de l'interface utilisateur (ui)
ui = ui_func()

#définition du serveur (server)
server = server_func()

#lancement de l'application
shinyApp(ui = ui, server = server)
