
#' PredictionServer
#'
#' Creates and exports the server for the 'prediction' part of the app
#'
#' @param input
#' @param output
#'
#' @return output
#' @export
#'
#' @import shiny
#' @import pls
#' @import class
#' @import caret
#' @import nnet
#'
PredictionServer = function(input, output){

  #attendre la confirmation de l'utilisateur avant d'ajuster le modèle
  model <- reactiveValues(go = FALSE)

  #le bouton go compte le nombre de fois ou il est cliqué. On le transforme
  #en une valeur reactive booleenne :
  #des qu'il est cliqué, sa valeur augmente d'un, model$go prendra la valeur
  #TRUE
  #et tout ce qui depend de model$go est refait
  observeEvent(input$ajust, {

    # 0 sera compris comme FALSE
    # 1+ sera compris comme TRUE
    model$go <- input$ajust
  })

  #adapter le jeu de données en fonction de la sélection utilisateur
  select.data <- reactive({

    if(model$go==FALSE) return() #tant que l'utilisateur n'a pas cliqué sur le
    #bouton "ajust", on attend

    isolate({
      data_corr <- data_corr[,c(input$variables, "ModeAccouc")]
      #on restreint le jeu de données aux variables sélectionnées par
      #l'utilisateur + la variable réponse

      data_corr
    })
  })

  #choisir le meilleur modèle en fonction de la sélection utilisateur
  select.model <- reactive({

    dat <- select.data()

    #cvperformance évalue la performance des deux modèles en validation croisée
    #à 10 segments
    acc.multinom <- cvperformance(data = dat, model = "multinom")
    knn <- cvperformance(data = dat, model = "knn")
    #attention : cvperformance ne renvoie pas le même nombre de variables en
    #fonction des options sélectionnées

    if(acc.multinom > knn$acc){ #si le modèle de régression multinomiale
                                #est meilleur que le knn

      return(list("result" = "multinomial",
                  "acc.multinom" = acc.multinom,
                  "acc.knn" = knn$acc,
                  "k.knn" = knn$k))

    } else { #si le modèle knn est meilleur que la régression multinomiale

      return(list("result" = "knn",
                  "acc.multinom" = acc.multinom,
                  "acc.knn" = knn$acc,
                  "k.knn" = knn$k)
      )
    }

  })

  #Présenter à l'utilisateur une comparaison entre deux modèles : multinom et
  #knn
  output$model <- renderUI({
    if(model$go==FALSE) return() #tant que l'utilisateur n'a pas cliqué
                                 #sur le bouton "ajust", on attend

    isolate({
      eval <- select.model() #la reactiveValue select.model() compare
                             #les deux modèles

      return(HTML(paste("<h1>Comparaison des performances de deux modèles</h1>",
                        "<p>La performance des deux modèles a été évaluée par",
                        "validation croisée en 10 segments.</p>",
                        "<p>Performance du modèle multinomial sur les variables",
                        "sélectionnées (accuracy):",
                        round(eval$acc.multinom,3),"</p>",
                        "<p>Performance du modèle knn sur les variables",
                        "sélectionnées (accuracy):",
                        round(eval$acc.knn,3),"<br>",
                        "Nombre de voisins retenus :", eval$k.knn,
                        "</p>",
                        "<p>On choisit donc le modèle", eval$result, ".</p>")))
    })

  })

  #adapter le type de variables que l'utilisateur peut renseigner en fonction
  #de ses choix
  output$varsInput <- renderUI({
    if(model$go==FALSE) return() #tant que l'utilisateur n'a pas cliqué sur
                                 #le bouton "ajust", on attend

    isolate({

      var <- input$variables
      p <- length(var)

      if(p==0) #si aucune variable sélectionnée, on arrete
      {
        return()
      }

      output <- tagList()

      for(i in 1:p)
      {
        #on crée une boîte de texte pour chaque variable
        output[[i]] <- textInput(inputId = var[i],
                                 label = names[which(values == var[i])])
      }

      #on ajoute un 'action button' pour retarder la prédiction
      output[[p+1]] <- actionButton("predict", "Prédire")

      return(output)
    })

  })

  #un deuxième action button permet de retarder la prédiction pour attendre
  #la fin de la saisie utilisateur
  predict <- reactiveValues(go = FALSE)

  observeEvent(input$predict, {
    # 0 sera compris comme FALSE
    # 1+ sera compris comme TRUE
    predict$go <- input$predict
  })

  #Prédire le mode d'accouchement le plus probable à partir des variables
  #entrées par l'utilisateur
  output$predict <- renderUI({
    if(predict$go==FALSE) return() #tant que le bouton "prédire" n'est pas
                                   #cliqué on attend

    isolate({
      var <- input$variables #on récupère les variables sélectionnées par
                             #l'utilisateur
      p <- length(var)

      newdata <- data.frame(matrix(ncol = p, nrow = 1))
      #creation d'un dataframe vide pour stocker les valeurs
      colnames(newdata) <- input$variables

      for(j in 1:p){ #on va récupérer les variables une à une
        case <- get.newdata(var[j], reactiveValuesToList(input))
        #get.newdata va chercher l'input entré par l'utilisateur et vérifie sa
        #validité : si c'est bien un type numérique et si les valeurs sont
        #cohérentes avec le jeu de données existant

        if(is.na(case) || is.null(case)) #si la valeur entrée par l'utilisateur
                                         #est fausse
        {
          return(HTML(paste("<strong>Erreur : Veuillez entrer une valeur",
          "numérique et cohérente avec les données observées.</strong>")))
        } else { #sinon on l'ajoute au jeu de données à prédire
          newdata[1,j] <- case
        }
      }

      ajusted <- select.model()
      #on ajuste le modèle sur les variables sélectionnées

      dat <- select.data()

      if(ajusted$result=="multinomial"){
        #si le meilleur modèle est le modèle multinomial

        mod <- multinom(ModeAccouc~., data = dat) #on ajuste le modèle

        pred <- predict(mod, newdata = newdata, type = "class")
        #on récupère la prédiction

      } else { #si le meilleur modèle est le knn

        pred <- knn(train = dat[,-ncol(dat)],
                    test = newdata,
                    cl = dat[,ncol(dat)],
                    k = ajusted$k.knn)
      }

      #on transforme la prédiction "en français"
      if(pred=="VBS"){
        pred <- "voie basse"
        }

      return(HTML(paste("<h1>Prédiction du mode d'accouchement</h1>",
                        "<p>Selon notre modèle, le mode d'accouchement sera",
                        "le suivant :<b>", pred,"</b></p>")))
    })
  })

  return(output)
}
