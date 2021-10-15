
#' PredictionUI
#'
#' Creates and exports the ui for the 'prediction' part of the app
#'
#' @return ui
#' @export
#'
#' @import shiny
#'
PredictionUI = function(){

  ui = fluidPage(

    sidebarPanel(

      checkboxGroupInput(
        inputId = "variables",
        label = "Choisissez les variables de votre modèle",
        choiceNames = names,
        choiceValues = values
        ),


      actionButton(inputId = "ajust",
                   label = "Ajuster le modèle"),

      br(),
      br(),

      uiOutput("varsInput")

      ),

    mainPanel(

      htmlOutput("model"),

      br(),
      br(),

      htmlOutput("predict")
    )
  )

  return(ui)
}
