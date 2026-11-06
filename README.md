# Evaluación de un modelo bayesiano (BGLR) para clasificación de fraude en datos simulados

Código y resultados de la tesis **"Aplicación de inferencia Bayesiana en sistemas de detección de fraude financiero basados en datos transaccionales"**, Facultad de Telemática, Universidad de Colima.

## Qué hace

Simula una variable respuesta binaria (fraude / no fraude) con proporciones controladas de fraude y ajusta un modelo bayesiano con la librería **BGLR** (Regresión de Cresta Bayesiana, `model = "BRR"`, respuesta `ordinal`, que usa una verosimilitud probit). Evalúa el desempeño con validación cruzada de 5 pliegues y cinco métricas: Accuracy, Kappa, Sensitivity, Specificity y F1 Score.

## Diseño experimental

| Factor | Niveles |
|---|---|
| Proporción de fraude | 0.05, 0.15, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85, 0.95 |
| Variables predictoras (NV) | 5 y 10 |
| Threshold de clasificación | 0.50, 0.80 e igual a la proporción |
| Pliegues de validación cruzada | 5 |

10 × 2 × 3 × 5 = **300 filas** de resultados (60 escenarios).

Los datos son simulados: 500 observaciones, predictores `X ~ N(0, 1)`, coeficientes beta aleatorios `N(0, 1)`. La respuesta se genera con una función logística y se asigna como fraude el porcentaje de observaciones con las probabilidades más altas que indica la proporción, de modo que cada escenario tiene exactamente la proporción deseada.

## Cómo ejecutarlo

1. Instalar R y los paquetes:
   ```r
   install.packages(c("BGLR", "caret", "dplyr", "rsample"))
   ```
2. Ejecutar el script de simulación y ajuste:
   ```r
   source("R/01_simulacion_y_modelo.R")
   ```
   Realiza 300 ajustes MCMC, por lo que tarda alrededor de 30 a 60 minutos.
3. Se genera `correccion.csv` con los resultados.
4. Las gráficas se generan a partir de ese archivo con el script de `R/`.

## Columnas del archivo de resultados

| Columna | Descripción |
|---|---|
| Prop | Proporción de fraude en los datos |
| NV | Número de variables predictoras |
| Th | Threshold de clasificación |
| Accuracy, Kappa, Sensitivity, Specificity, F1 | Métricas por pliegue |

`F1` es `NA` cuando el modelo no predice ningún fraude en el pliegue, porque la Precisión queda como 0/0.

## Notas importantes para interpretar los resultados

- **Datos simulados.** No se usaron datos transaccionales reales.
- **NV = 5 y NV = 10 usan respuestas distintas.** La respuesta se genera con las mismas variables que usa el modelo, así que cambiar NV cambia también la variable de fraude. No es una comparación entre menos y más información sobre el mismo fraude.
- **Threshold igual a la proporción.** La Sensitivity sale artificialmente alta por la forma en que se generan los datos (el mismo porcentaje define quién es fraude y se reutiliza como umbral). No debe interpretarse como capacidad real de detección.
- **El modelo se reajusta para cada threshold.** El ajuste no depende del threshold, pero se repite por cada uno, por lo que parte de la diferencia entre thresholds incluye variación del MCMC.
- **Kappa.** Es más robusto que la Accuracy ante el desbalance, pero depende de la prevalencia (paradoja de Kappa).
- **Clase positiva.** En `confusionMatrix` se especifica `positive = "1"`, que corresponde a fraude.
- No se compara con otros modelos ni se evalúa la relevancia de las variables.

## Reproducibilidad

Semillas fijas: `set.seed(123)` para la matriz de predictores y los pliegues, y `seed = 42` en la simulación de la respuesta. Versiones de R y de las librerías: ver `sessionInfo()`.

```r
sessionInfo()
```

## Licencia

Código bajo licencia MIT. Resultados y figuras bajo CC BY 4.0.
