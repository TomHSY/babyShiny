

#' ModellingServer
#'
#' Creates and exports the server for the 'modelling' part of the app
#'
#' @param input
#' @param output
#'
#' @return output
#' @export
#'
#' @import shiny
#' @import shinyjs
#' @import nnet
#' @import RcmdrMisc
#' @import ggplot2
#' @import dplyr
#' @import rpart
#' @import randomForest
#' @import rpart.plot
#' @import markdown
#'
ModellingServer <- function(input, output){

  hide('title2')
  hide('button2')
  hide('title3')
  hide('vsmeth')
  hide('button3')
  hide('reset')

  observeEvent(input$button1, {

    output$part1 <- renderUI({
      includeHTML(
        rmarkdown::render(input = "www/part1.Rmd")
      )
    })

    show('part1')
    hide('button1')

    show('title2')
    show('button2')
  })

  observeEvent(input$button2, {


    output$part2 <- renderUI({
      includeHTML(
        rmarkdown::render(input = "www/part2.Rmd")
      )
    })

    show('part2')
    hide('part1')
    hide('button2')

    show('title3')
    show('vsmeth')
    show('button3')
  })

  observeEvent(input$button3, {


    output$part3 <- renderUI({
      includeHTML(
        rmarkdown::render(input = "www/part3.Rmd",
                          params = list(vsmeth = isolate(input$vsmeth)))
      )
    })

    show('part3')
    hide('part2')
    show('reset')
  })

  observeEvent(input$reset, {

    hide('title2')
    hide('title3')
    hide('vsmeth')
    hide('reset')
    hide('part3')
    hide('button3')

    show('button1')

    reset('button1')
    reset('button2')
    reset('button3')

  })

  return(output)
}
