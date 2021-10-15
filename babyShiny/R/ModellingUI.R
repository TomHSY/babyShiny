

#' ModellingUI
#'
#' Creates and exports the ui for the 'modelling' part of the app
#'
#' @return ui
#' @export
#'
#' @import shiny
#' @import shinyjs
#'
ModellingUI <- function(){

  ui <- fluidPage(

    useShinyjs(),

    titlePanel("Modélisation et sélection de variables"),

    sidebarLayout(

      sidebarPanel(

        h3(id='title1', '1. Prétraitement des données'),
        actionButton('button1', "Go!"),

        hidden(h3(id='title2', '2. Test du modèle global')),
        hidden(actionButton('button2', "Go!")),

        hidden(h3(id='title3', '3. Sélection de variables')),
        hidden(radioButtons('vsmeth',"Choisir la méthode de sélection de variables :",
                            choices = list('Arbre de classification',
                                           'Forêt aléatoire'))),
        hidden(actionButton('button3', "Go!")),
        hidden(actionButton('reset', "Réinitialiser")),
      ),

      mainPanel(

        htmlOutput("part1"),

        htmlOutput("part2"),

        htmlOutput("part3"),
      )

    )

  )

  return(ui)
}
