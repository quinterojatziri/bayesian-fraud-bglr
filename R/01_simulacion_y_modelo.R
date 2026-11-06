# =====================================================================
# Evaluación de un modelo bayesiano (BGLR, BRR) para clasificación de
# fraude en datos simulados.
#
# Diseño: 10 proporciones x 2 números de variables x 3 thresholds
#         x 5 pliegues = 300 filas de resultados.
# Salida: data/correccion.csv
# Tiempo aproximado: 30-60 minutos (300 ajustes MCMC).
#
# Nota: la respuesta y se genera con las mismas nv variables que usa el
#       modelo, por lo que NV = 5 y NV = 10 usan respuestas distintas.
#
# =====================================================================

rm(list = ls())
# Limpia el entorno para que objetos previos no afecten los resultados.

library(BGLR)    # modelo bayesiano (MCMC)
library(caret)   # confusionMatrix: cálculo de métricas
library(dplyr)   # manipulación de datos
library(rsample) # vfold_cv: validación cruzada

# ---------------------------------------------------------------------
# Simulación de la respuesta binaria con proporción exacta de fraude
# ---------------------------------------------------------------------
# Se generan betas aleatorios, se calcula la probabilidad con la función
# logística y se asigna fraude (1) a la proporción indicada de
# observaciones con las probabilidades más altas (cuantil).
# Así cada escenario tiene exactamente la proporción deseada.
simulate_logistic_data_proporcion <- function(X, seed = NULL,
                                              proporcion = 0.5,
                                              beta_mean = 0,
                                              beta_sd = 1,
                                              intercept = 0) {
  if (!is.null(seed)) set.seed(seed)
  n    <- nrow(X)
  p    <- ncol(X)
  beta <- rnorm(p, mean = beta_mean, sd = beta_sd)
  eta  <- intercept + X %*% beta
  prob <- 1 / (1 + exp(-eta))
  threshold_real <- quantile(prob, 1 - proporcion)
  y <- ifelse(prob > threshold_real, 1, 0)
  return(list(y = y, p = prob, beta = beta, X = X))
}

# ---------------------------------------------------------------------
# Métricas de desempeño
# ---------------------------------------------------------------------
# positive = "1": la clase positiva es el fraude. Sin esto, caret
# tomaría el primer nivel del factor ("0") como clase positiva.
Metrics <- function(Y, Yhat) {
  u     <- union(Yhat, Y)                         # mismas categorías en ambos
  xtab1 <- table(factor(Yhat, u), factor(Y, u))   # matriz de confusión
  Conf_Matrix1 <- confusionMatrix(xtab1, positive = "1")
  pccc        <- Conf_Matrix1$overall[1]   # Accuracy
  kappa       <- Conf_Matrix1$overall[2]   # Kappa
  sensitivity <- Conf_Matrix1$byClass[1]   # Sensitivity
  Specificity <- Conf_Matrix1$byClass[2]   # Specificity
  F1          <- Conf_Matrix1$byClass[7]   # F1 (NA si no se predice fraude)
  return(c(pccc, kappa, sensitivity, Specificity, F1))
}

# ---------------------------------------------------------------------
# Matriz de predictores (500 observaciones, 10 variables)
# ---------------------------------------------------------------------
set.seed(123)  # reproducibilidad
X_full <- matrix(rnorm(500 * 10), nrow = 500, ncol = 10)
colnames(X_full) <- paste0("X", 1:10)

# ---------------------------------------------------------------------
# Factores del diseño experimental
# ---------------------------------------------------------------------
proporciones     <- c(0.05, 0.15, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85, 0.95)
num_variables    <- c(5, 10)
thresholds_fijos <- c(0.50, 0.80)  # el tercero (igual a la proporción) se agrega abajo

resultados <- data.frame()

# ---------------------------------------------------------------------
# Ciclo principal
# ---------------------------------------------------------------------
for (prop in proporciones) {

  cat("Corriendo proporcion:", prop, "\n")

  for (nv in num_variables) {

    cat("  Variables:", nv, "\n")

    X <- X_full[, 1:nv]  # primeras nv variables

    data1 <- simulate_logistic_data_proporcion(X, seed = 42, proporcion = prop)
    y         <- data1$y
    y_factor  <- as.factor(y)  # BGLR con respuesta ordinal requiere factor
    Data_Sim1 <- data.frame(y = y, X)

    ETA <- list(list(X = X, model = "BRR"))  # Regresión de Cresta Bayesiana

    # Validación cruzada de 5 pliegues; strata = "y" conserva la
    # proporción de fraudes en cada pliegue.
    set.seed(123)
    folds_rs <- vfold_cv(Data_Sim1, v = 5, strata = "y")
    folds <- lapply(1:5, function(i) {
      list(testing = as.integer(folds_rs$splits[[i]], "assessment"))
    })

    # Tres thresholds: 0.50, 0.80 e igual a la proporción
    thresholds_combo <- unique(c(thresholds_fijos, prop))

    for (th in thresholds_combo) {

      cat("    Threshold:", th, "\n")

      for (k in 1:5) {

        test_idx <- folds[[k]]$testing

        # Los datos de prueba se enmascaran con NA: el modelo no los ve
        # al entrenar y BGLR los trata como valores a predecir.
        y_factorNA <- y_factor
        y_factorNA[test_idx] <- NA

        fit <- BGLR(
          y             = y_factorNA,
          ETA           = ETA,
          response_type = "ordinal",  # verosimilitud probit
          nIter         = 2000,       # iteraciones MCMC
          burnIn        = 200,        # calentamiento descartado
          thin          = 10,         # se guarda 1 de cada 10
          verbose       = FALSE
        )

        y_hat  <- fit$probs[test_idx, 2]   # probabilidad de fraude
        y_obs  <- y_factor[test_idx]
        y_pred <- ifelse(y_hat > th, 1, 0) # clasificación con el threshold

        Pred_Performance <- Metrics(y_obs, y_pred)

        resultados <- rbind(resultados, data.frame(
          Prop        = prop,
          NV          = nv,
          Th          = th,
          Accuracy    = Pred_Performance[1],
          Kappa       = Pred_Performance[2],
          Sensitivity = Pred_Performance[3],
          Specificity = Pred_Performance[4],
          F1          = Pred_Performance[5]
        ))
      }
    }
  }
}

# ---------------------------------------------------------------------
# Guardar resultados
# ---------------------------------------------------------------------
dir.create("data", showWarnings = FALSE)
write.csv(resultados, file = "data/correccion.csv", row.names = FALSE)
cat("Listo. Total de filas:", nrow(resultados), "\n")
