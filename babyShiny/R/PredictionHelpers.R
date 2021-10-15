

#' cvperformance
#'
#' Cette fonction évalue la performance de prédiction de deux modèles (multinom
#' et knn) en validation croisée par 10 segments
#'
#' @param data un dataframe contenant la variable réponse ModeAccouc et les
#' variables explicatives numériques sélctionnées par l'utilisateur
#' @param model le type de modèle choisi dans c("multinom", "knn")
#'
#' @return acc : accuracy du modèle calculée par validation croisée en 10
#' segments
#' k : hyperparamètre du modèle knn (si model="knn")
#'
#' @import caret
#' @import nnet
#'
cvperformance <- function(data, model){

  if(!model %in% c("knn", "multinom")){
    stop("model doit etre un des suivants : knn ou multinom")
  }

  if(model=="multinom"){
    segs <- cvsegments(N = nrow(data), k = 10) #10 segments pour la CV

    predict <- rep(NA, nrow(data)) #initialisation d'un vecteur de valeurs
                                   #prédites

    for(k in 1:10){
      train <- data[-segs[[k]],]
      test <- data[segs[[k]],]

      mod.k <- multinom(ModeAccouc~., data = train, family = "multinom", trace = FALSE)
      predict[segs[[k]]] <- predict(mod.k, newdata = test, type = "class")
    }

    return("acc" = mean(as.numeric(data$ModeAccouc)==predict))
    #la transformation en as.numeric permet d'harmoniser avec predict, qui a
    # prédit les niveaux du facteur
  }

  if(model=="knn"){

    fitControl <- trainControl(method = "LGOCV",
                               number=10)

    mod_knn <- caret::train(form=ModeAccouc ~ .,
                            data = data,
                            method = "knn",
                            trControl = fitControl,
                            tuneGrid=data.frame(k=1:20),
                            metric="Accuracy") #choix d'un paramètre k qui
                                               #maximise l'accuracy

    return(list("acc" = max(mod_knn$results$Accuracy),
                "k" = which.max(mod_knn$results$Accuracy)))
  }
}

#' get.newdata
#'
#' @param var variable à tester, comprise dans le vecteur values :
#' c("Nbsem","Agedelamere","TailMere","PoidsMere","Agedupere","TailPere",
#' "PoidsPere","NbGrossess","NbEnfants","NbIVG","NbFC")
#'
#' @param ipt la reactiveValues input est transformée en liste par
#' reactiveValuesToList()
#'
#' @return la fonction renvoie la valeur entrée par l'utilisateur, en vérifiant
#' au préalable que les données entrées sont correctes (format numérique et
#' comprises dans les variations normales de la variable avec une marge de 20%)
#'
get.newdata <- function(var, ipt){
  res <- switch(var,
                "Nbsem" = try(ifelse(as.numeric(ipt$Nbsem) >
                                       max(data_corr$Nbsem)*1.2 ||
                                       as.numeric(ipt$Nbsem) <
                                       min(data_corr$Nbsem)*0.8,
                                     return(),
                                     as.numeric(ipt$Nbsem)), silent = TRUE),
                #on verifie que les valeurs entrées sont valides, avec une marge
                #de 20%

                "Agedelamere" = try(ifelse(as.numeric(ipt$Agedelamere) >
                                             max(data_corr$Agedelamere)*1.2 ||
                                             as.numeric(ipt$Agedelamere) <
                                             min(data_corr$Agedelamere)*0.8,
                                           return(),
                                           as.numeric(ipt$Agedelamere)),
                                    silent = TRUE),

                "TailMere" = try(ifelse(as.numeric(ipt$TailMere) >
                                          max(data_corr$TailMere)*1.2 ||
                                          as.numeric(ipt$TailMere) <
                                          min(data_corr$TailMere)*0.8,
                                        return(),
                                        as.numeric(ipt$TailMere)),
                                 silent = TRUE),

                "PoidsMere" = try(ifelse(as.numeric(ipt$PoidsMere) >
                                           max(data_corr$PoidsMere)*1.2 ||
                                           as.numeric(ipt$PoidsMere) <
                                           min(data_corr$PoidsMere)*0.8,
                                         return(),
                                         as.numeric(ipt$PoidsMere)),
                                  silent = TRUE),

                "Agedupere" = try(ifelse(as.numeric(ipt$Agedupere) >
                                           max(data_corr$Agedupere)*1.2 ||
                                           as.numeric(ipt$Agedupere) <
                                           min(data_corr$Agedupere)*0.8,
                                         return(),
                                         as.numeric(ipt$Agedupere)),
                                  silent = TRUE),

                "TailPere" = try(ifelse(as.numeric(ipt$TailPere) >
                                          max(data_corr$TailPere)*1.2 ||
                                          as.numeric(ipt$TailPere) <
                                          min(data_corr$TailPere)*0.8,
                                        return(),
                                        as.numeric(ipt$TailPere)),
                                 silent = TRUE),

                "PoidsPere" = try(ifelse(as.numeric(ipt$PoidsPere) >
                                           max(data_corr$PoidsPere)*1.2 ||
                                           as.numeric(ipt$PoidsPere) <
                                           min(data_corr$PoidsPere)*0.8,
                                         return(),
                                         as.numeric(ipt$PoidsPere)),
                                  silent = TRUE),

                "NbGrossess" = try(ifelse(as.numeric(ipt$NbGrossess) >
                                            max(data_corr$NbGrossess)*1.2 ||
                                            as.numeric(ipt$NbGrossess) < 0,
                                          return(),
                                          as.numeric(ipt$NbGrossess)),
                                   silent = TRUE),

                "NbEnfants" = try(ifelse(as.numeric(ipt$NbEnfants) >
                                           max(data_corr$NbEnfants)*1.2 ||
                                           as.numeric(ipt$NbEnfants) < 0,
                                         return(),
                                         as.numeric(ipt$NbEnfants)),
                                  silent = TRUE),

                "NbIVG" = try(ifelse(as.numeric(ipt$NbIVG) >
                                       max(data_corr$NbIVG)*1.2 ||
                                       as.numeric(ipt$NbIVG) < 0,
                                     return(),
                                     as.numeric(ipt$NbIVG)), silent = TRUE),

                "NbFC" = try(ifelse(as.numeric(ipt$NbFC) >
                                      max(data_corr$NbFC)*1.2 ||
                                      as.numeric(ipt$NbFC) < 0,
                                    return(),
                                    as.numeric(ipt$NbFC)), silent = TRUE)
  )
  return(res)
}

