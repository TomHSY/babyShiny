
#' DataVisServer
#'
#' Creates and exports the server for the 'data visualisation' part of the app
#'
#' @param input
#' @param output
#'
#' @return output
#' @export
#'
#' @import shiny
#' @import graphics
#' @import FactoMineR
#' @import devtools
#' @import factoextra
#' @import ggplot2
#' @import cowplot
#'
DataVisServer = function(input, output){

  output$data_table <- renderDataTable(data)

  ## onglet Summary ##

  # Explication des données

  output$exp_data <- renderText("Les deux variables que l'on souhaite expliquer sont les variables ModeTravai et ModeAccouc.
                            Nous cherchons à prédire ces variables en fonction des différents paramètres quantitatifs concernant la mère et le père.
                            Vous pouvez visualiser ici différents boxplots en sélectionnant les données à inclure.")

  output$summary <- renderPrint(summary(data))

  ## onglet boxplot ##

  data$ModeAccouc <- droplevels(data$ModeAccouc, "") # On enlève le facteur "" qui est vide
  data$ModeTravai <- droplevels(data$ModeTravai, "") # On enlève le facteur "" qui est vide

  output$boxplot <- renderPlot(
    boxplot((data[,input$choixy]~data[,input$choixx]),
            xlab=input$choixx,
            ylab=input$choixy,
            col = "cyan4",
            border = "black",
            main = paste0(input$choixx," en fonction de ", input$choixy))
  )

  ## onglet ACP ##
  
  #Résultat de l'ACP
  res.PCA <- FactoMineR::PCA(data,
                             quali.sup = c(3,7,8,9,22,23,24,25),
                             quanti.sup = c(1,4,5,6,11,15),
                             graph = FALSE)
  # Graphiques de l'ACP
  acp_var <- FactoMineR::plot.PCA(res.PCA,
                                  choix = 'var',
                                  title ="Graphe des variables de l'ACP")

  acp_ind <- FactoMineR::plot.PCA(res.PCA,
                                  invisible = c('ind','ind.sup'),
                                  title = "Graphe des individus de l'ACP",
                                  label = c('quali'))

  # Visualiser les variables avec cos2> = 0.6
  acp_cos <- factoextra::fviz_pca_var(res.PCA,
                                      select.var = list(cos2 = 0.6),
                                      title = "Variables avec un cos2 supérieur ou égal à 0.6")


  # Top 5 variables actives avec le cos2 le plus élevé
  acp_cos_top5 <- factoextra::fviz_pca_var (res.PCA,
                                            select.var = list(cos2 = 5),
                                            title = "Top 5 variable active avec le cos2 le plus élevé")

  # Top 5 des individus/variables les plus contibutifs
  acp_cont_top5 <- factoextra::fviz_pca_biplot (res.PCA,
                                                select.ind = list (contrib = 5),
                                                select.var = list (contrib = 5),
                                                ggtheme = theme_minimal(),
                                                title = "Top 5 des individus et des variables les plus contributifs")

  # Graphiques de l'ACP agencés ensemble dans une grile
  output$acp_grille <- renderPlot(plot_grid(acp_var,
                                            acp_ind,
                                            acp_cos,
                                            acp_cos_top5,
                                            acp_cont_top5))

  output$int_acp <- renderText("Une Analyse en Composante Principale est réalisée sur les variables quantitatives.
                               Les données obtenues après la naissance du bébé sont exclues de l'analyse car elle ne participe pas à la prédiction.
                               L'analyse montre que les variables qui contribue le plus à la création des axes sont l'âge des parents et le nombre de grossesses pour le premier axe et le poids/IMC de la mère pour le second.
                               L'analyse semble également mettre en avant un lien entre le mode de travail : Césarienne et l'âge de la mère et le nombre de grossesse.
                               Les variables liées au père semblent peu corrélées au mode de travail et d'accouchement.")

  ## onglet ACM ##
  #Résultat de l'ACM

  res.MCA <- FactoMineR::MCA(data,
                             quanti.sup = c(1,2,4,5,6,10,11,12,13,14,15,16,17,18,19,20,21,26,27),
                             graph = FALSE)

  # Graphiques de l'ACM
  acm_var <- FactoMineR::plot.MCA(res.MCA,
                                  choix = 'var',
                                  title = "Graphe des variables",
                                  col.var = c(1,2,3,4,5,6,7,8))
  acm_ind <- FactoMineR::plot.MCA(res.MCA,
                                  invisible = 'ind',
                                  col.var = c(1,1,1,2,2,2,3,3,3,3,3,3,3,4,4,4,4,5,5,5,6,6,6,6,6,7,7,7,7,7,7,8,8,8),
                                  title = "Graphe de l'ACM",
                                  label = c('var'))
  acm_sup <- FactoMineR::plot.MCA(res.MCA,
                                  choix = 'quanti.sup',
                                  title = "Variables quantitatives supplémentaires")

  # Graphiques de l'ACM agencés ensemble dans une grille
  output$acm_grille <- renderPlot(plot_grid(acm_var,
                                            acm_ind,
                                            acm_sup,
                                            rel_widths = c(1,2))
  )

  output$int_acm <- renderText("Nous avons effectuée ici une analyse des correspondances multiples sur les variables qualitatives.
                               Les variables mode d'accouchement, mode de travail et opérant semblent corrélées.
                               Cela est cohérent avec l'ACP.")


  return(output)
}
