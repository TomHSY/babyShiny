

#' launchApp
#'
#' Launches the shiny app
#'
#' @param launch.browser boolean value indicating whether the app should be
#' launched in the user's default browser (T), or in a R window (F)
#'
#' @export
#'
#' @import shiny
#'
#' @examples
#' launchApp() #in the browser
#' launchApp(launch.browser=F) #in a R window
#'
launchApp <- function(launch.browser = T){

  #choix du dossier de l'application
  appDir <- system.file("shinyapp", package = "babyShiny")

  #si l'emplacement n'est pas trouvé, affichage d'un message d'erreur
  if (appDir == "") {

    stop("Could not find the app. Try re-installing the package.",
         call. = FALSE)

  }

  #si l'utilisateur ne veut pas utiliser le navigateur, on choisit la fenêtre R
  if (!launch.browser){
    launch.browser <- .rs.invokeShinyWindowViewer
  }

  #lancement de l'application
  shiny::runApp(appDir, launch.browser = launch.browser)
}
