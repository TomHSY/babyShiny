

#' DataVisUI
#'
#' Creates and exports the ui for the 'data visualisation' part of the app
#'
#' @return ui
#' @export
#'
#' @import shiny
#'
DataVisUI = function(){



  ui = fluidPage(

    tabPanel( "Exploration de données",
              tabsetPanel(

                ## onglet données brutes ##

                tabPanel("Données Brutes",
                         dataTableOutput("data_table")),

                ## onglet résumé ##

                tabPanel("Résumé",
                         wellPanel(
                           textOutput("exp_data")),
                         verbatimTextOutput("summary")),

                ## onglet boxplot ##

                tabPanel("Boxplot",
                         sidebarLayout(
                           sidebarPanel(

                             # Sélection de x et de y pour le boxplot
                             radioButtons("choixx",
                                          label = "Choissisez la variable à expliquer (x)",
                                          choices = list("ModeAccouc","ModeTravai")),
                             selectInput("choixy",
                                         label = "Choissisez la variable explicative (y)",
                                         choices = ylist),

                             width =2),

                           mainPanel(
                             plotOutput("boxplot"),
                             width =10),
                         )
                ),

                ## onglet acp ##
                tabPanel("ACP",
                         plotOutput("acp_grille",
                                    height = "800px"),
                         wellPanel(
                           textOutput("int_acp"))),

                ## onglet acm ##
                tabPanel("ACM",
                         plotOutput("acm_grille",
                                    height = "800px"),
                         wellPanel(
                           textOutput("int_acm")))

              )
    )
  )

  return(ui)
}
