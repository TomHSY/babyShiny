
#' server_func
#'
#' Creates and exports the app's main server
#'
#' @return server, the main server
#' @export
#'
#' @import shiny
#'
server_func <- function(){

  server <- function(input, output){

    output <- DataVisServer(input,output)

    output <- ModellingServer(input, output)

    output <- PredictionServer(input, output)

  }

  return(server)
}
