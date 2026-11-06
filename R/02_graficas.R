# Genera las 26 gráficas de barras a partir de data/correccion.csv.
# Ejecutar desde la carpeta raíz del repositorio. Las gráficas aparecen en el panel Plots.

graphics.off()

library(ggplot2)
library(dplyr)
library(tidyr)

# ---------------------------------------------------------------
# 1. CARGAR Y PREPARAR DATOS
# ---------------------------------------------------------------

df <- read.csv("data/correccion.csv")

df$Accuracy    <- as.numeric(df$Accuracy)
df$Kappa       <- as.numeric(df$Kappa)
df$Sensitivity <- as.numeric(df$Sensitivity)
df$Specificity <- as.numeric(df$Specificity)
df$F1          <- as.numeric(df$F1)

df <- df %>%
  mutate(
    Th_tipo = case_when(
      abs(Th - Prop) < 1e-9 ~ "Igual a Prop",
      abs(Th - 0.50) < 1e-9 ~ "Fijo 0.50",
      abs(Th - 0.80) < 1e-9 ~ "Fijo 0.80"
    ),
    Prop_f = factor(Prop),
    NV_f   = factor(NV, labels = c("NV = 5", "NV = 10")),
    Th_tipo = factor(Th_tipo, levels = c("Igual a Prop", "Fijo 0.50", "Fijo 0.80"))
  )

metricas    <- c("Accuracy", "Kappa", "Sensitivity", "Specificity", "F1")
etiquetas_m <- c("Accuracy", "Kappa", "Sensitivity", "Specificity", "F1 Score")

col_prop   <- "#4472C4"
col_nv     <- c("#4472C4", "#ED7D31")
col_nv_bar <- c("#4472C4", "#ED7D31")

tema_barras <- function(titulo, x_lab, y_lab = "Media") {
  list(
    theme_minimal(base_size = 12),
    theme(
      plot.title         = element_text(face = "bold", size = 13, hjust = 0.5),
      axis.title         = element_text(size = 11),
      axis.text          = element_text(size = 10),
      legend.title       = element_text(size = 10, face = "bold"),
      legend.text        = element_text(size = 9),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank()
    ),
    labs(title = titulo, x = x_lab, y = y_lab)
  )
}

props_sel <- c(0.05, 0.45, 0.95)

# ---------------------------------------------------------------
# GRUPO 1 — métrica ~ Proporción
# ---------------------------------------------------------------
cat("Generando Grupo 1...\n")
resumen_prop <- df %>%
  group_by(Prop_f) %>%
  summarise(across(all_of(metricas), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

for (i in seq_along(metricas)) {
  m <- metricas[i]; etq <- etiquetas_m[i]
  p <- ggplot(resumen_prop, aes(x = Prop_f, y = .data[[m]])) +
    geom_col(fill = col_prop, color = "white", width = 0.7) +
    geom_text(aes(label = round(.data[[m]], 2)), vjust = -0.4, size = 3.2, color = "gray20") +
    scale_y_continuous(limits = c(0, 1.15), breaks = seq(0, 1, 0.2)) +
    tema_barras(paste0(etq, " por Proporción"), "Proporción (Prop)", paste0("Media de ", etq))
  print(p)
}

# ---------------------------------------------------------------
# GRUPO 2 — métrica ~ NV
# ---------------------------------------------------------------
cat("Generando Grupo 2...\n")
resumen_nv <- df %>%
  group_by(NV_f) %>%
  summarise(across(all_of(metricas), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

for (i in seq_along(metricas)) {
  m <- metricas[i]; etq <- etiquetas_m[i]
  p <- ggplot(resumen_nv, aes(x = NV_f, y = .data[[m]], fill = NV_f)) +
    geom_col(color = "white", width = 0.5) +
    geom_text(aes(label = round(.data[[m]], 2)), vjust = -0.4, size = 4, color = "gray20") +
    scale_fill_manual(values = col_nv, guide = "none") +
    scale_y_continuous(limits = c(0, 1.15), breaks = seq(0, 1, 0.2)) +
    tema_barras(paste0(etq, " por Número de Variables"), "Número de Variables (NV)", paste0("Media de ", etq))
  print(p)
}

# ---------------------------------------------------------------
# GRUPO 3 — 12 barras por valor de Th
# ---------------------------------------------------------------
cat("Generando Grupo 3...\n")
col_grupo3 <- c("Igual a Prop" = "#4472C4", "Fijo 0.50" = "#ED7D31", "Fijo 0.80" = "#70AD47")

resumen_th <- df %>%
  group_by(Th) %>%
  summarise(across(all_of(metricas), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  arrange(Th) %>%
  mutate(
    Th_label = as.factor(round(Th, 2)),
    Th_color = case_when(
      abs(Th - 0.50) < 1e-9 ~ "Fijo 0.50",
      abs(Th - 0.80) < 1e-9 ~ "Fijo 0.80",
      TRUE                   ~ "Igual a Prop"
    )
  )

for (i in seq_along(metricas)) {
  m <- metricas[i]; etq <- etiquetas_m[i]
  p <- ggplot(resumen_th, aes(x = Th_label, y = .data[[m]], fill = Th_color)) +
    geom_col(color = "white", width = 0.75) +
    geom_text(aes(label = round(.data[[m]], 2)), vjust = -0.4, size = 3, color = "gray20") +
    scale_fill_manual(name = "Tipo de Threshold", values = col_grupo3) +
    scale_y_continuous(limits = c(0, 1.15), breaks = seq(0, 1, 0.2)) +
    tema_barras(paste0(etq, " por Valor de Threshold"), "Threshold (Th)", paste0("Media de ", etq))
  print(p)
}

# ---------------------------------------------------------------
# GRUPO 4 — props {0.05, 0.45, 0.95} × NV
# ---------------------------------------------------------------
cat("Generando Grupo 4...\n")
resumen_g4 <- df %>%
  filter(Prop %in% props_sel) %>%
  mutate(Prop_etq = factor(paste0("Prop = ", Prop), levels = paste0("Prop = ", props_sel))) %>%
  group_by(Prop_etq, NV_f) %>%
  summarise(across(all_of(metricas), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

for (i in seq_along(metricas)) {
  m <- metricas[i]; etq <- etiquetas_m[i]
  p <- ggplot(resumen_g4, aes(x = Prop_etq, y = .data[[m]], fill = NV_f)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65, color = "white") +
    geom_text(aes(label = round(.data[[m]], 2)),
              position = position_dodge(width = 0.75), vjust = -0.4, size = 3.2, color = "gray20") +
    scale_fill_manual(name = "Núm. Variables", values = col_nv_bar) +
    scale_y_continuous(limits = c(0, 1.15), breaks = seq(0, 1, 0.2)) +
    tema_barras(paste0(etq, " por Proporción y NV"), "Proporción seleccionada", paste0("Media de ", etq))
  print(p)
}

# ---------------------------------------------------------------
# GRUPO 5 — props {0.05, 0.45, 0.95} × Th {0.05, 0.50, 0.80}
# ---------------------------------------------------------------
cat("Generando Grupo 5...\n")
th_sel  <- c(0.05, 0.50, 0.80)
col_g5  <- c("#4472C4", "#ED7D31", "#70AD47")
names(col_g5) <- paste0("Th = ", th_sel)

resumen_g5 <- df %>%
  filter(Prop %in% props_sel, Th %in% th_sel) %>%
  mutate(
    Prop_etq = factor(paste0("Prop = ", Prop), levels = paste0("Prop = ", props_sel)),
    Th_etq   = factor(paste0("Th = ", Th),     levels = paste0("Th = ", th_sel))
  ) %>%
  group_by(Prop_etq, Th_etq) %>%
  summarise(across(all_of(metricas), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

for (i in seq_along(metricas)) {
  m <- metricas[i]; etq <- etiquetas_m[i]
  p <- ggplot(resumen_g5, aes(x = Prop_etq, y = .data[[m]], fill = Th_etq)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "white", na.rm = TRUE) +
    geom_text(aes(label = round(.data[[m]], 2)),
              position = position_dodge(width = 0.8), vjust = -0.4, size = 3, color = "gray20", na.rm = TRUE) +
    scale_fill_manual(name = "Threshold", values = col_g5) +
    scale_y_continuous(limits = c(0, 1.15), breaks = seq(0, 1, 0.2)) +
    labs(caption = "Th=0.05 solo existe para Prop=0.05 en el diseño experimental") +
    tema_barras(paste0(etq, " por Proporción y Threshold específico"),
                "Proporción seleccionada (0.45 ≈ 0.5)", paste0("Media de ", etq))
  print(p)
}

# ---------------------------------------------------------------
# GRÁFICA EXTRA — Sensitivity por Proporción × Th fijo (0.50 y 0.80)
# ---------------------------------------------------------------
cat("Generando gráfica extra: Sensitivity por Th fijo...\n")

resumen_sens <- df %>%
  filter(Th %in% c(0.50, 0.80)) %>%
  group_by(Prop_f, Th) %>%
  summarise(Sensitivity = mean(Sensitivity, na.rm = TRUE), .groups = "drop") %>%
  mutate(Th_label = factor(paste0("Th = ", Th)))

p <- ggplot(resumen_sens, aes(x = Prop_f, y = Sensitivity, fill = Th_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, color = "white") +
  geom_text(aes(label = round(Sensitivity, 2)),
            position = position_dodge(width = 0.75), vjust = -0.4, size = 3.2, color = "gray20") +
  scale_fill_manual(name = "Threshold", values = c("Th = 0.5" = "#ED7D31", "Th = 0.8" = "#70AD47")) +
  scale_y_continuous(limits = c(0, 1.15), breaks = seq(0, 1, 0.2)) +
  tema_barras("Sensitivity por Proporción y Threshold fijo",
              "Proporción (Prop)", "Media de Sensitivity")
print(p)

cat("\nListo. 26 gráficas generadas.\n")
