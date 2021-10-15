
#' ui_func
#'
#' Creates and exports the app's main shiny user interface
#'
#' @return ui, the main shiny user interface
#' @export
#'
#' @import shiny
#' @import shinythemes
#'
ui_func <- function(){

  ui <- fluidPage(

      theme = shinytheme(theme = "flatly"),


      navbarPage("Application shiny",

                 tabPanel('Introduction', IntroUI()),
                 tabPanel('Visualisation des données', DataVisUI()),
                 tabPanel('Sélection de variables', ModellingUI()),
                 tabPanel('Prédiction', PredictionUI())
      )
    )

  return(ui)
}
