
#' IntroUI
#'
#' Creates and exports the ui for the 'introduction' part of the app
#'
#' @return ui
#' @export
#'
#' @import shiny
#'
IntroUI = function(){

  ui = fluidPage(
    includeHTML("www/Intro_app.html")
    )

  return(ui)
}

