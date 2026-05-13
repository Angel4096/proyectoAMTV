
# Librerías ---------------------------------------------------------------

cargar_paquete <- function(paquete) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    stop(
      sprintf(
        "Falta el paquete '%s'. Instalalo con install.packages('%s') antes de ejecutar este script.",
        paquete,
        paquete
      ),
      call. = FALSE
    )
  }
  suppressPackageStartupMessages(
    library(paquete, character.only = TRUE)
  )
}

cargar_paquete("dplyr")
cargar_paquete("FactoMineR")
cargar_paquete("factoextra")

source("R/helpers_pca_ols.R")

if (!exists("df", inherits = TRUE)) {
  stop(
    "No existe el objeto `df` en el entorno. Primero cargá y prepará el panel KoTaP completo.",
    call. = FALSE
  )
}


# Entendimiento -----------------------------------------------------------

if (exists("df_raw", inherits = TRUE) && "year" %in% names(df_raw)) {
  pivote <-
    df_raw %>%
    group_by(year) %>%
    summarise(conteo = n(), .groups = "drop")

  sum(pivote$conteo)
} else if ("year" %in% names(df)) {
  pivote <-
    df %>%
    group_by(year) %>%
    summarise(conteo = n(), .groups = "drop")

  sum(pivote$conteo)
} else {
  message("Entendimiento: no existe `df_raw` ni columna `year`; se omite pivote por año.")
}


# Target y split temporal -------------------------------------------------

vars_pca_candidatas <- c(
  "SIZE", "LEV", "CUR", "GRW", "CFO", "PPE", "AGE", "INVREC", "MB", "TQ",
  "GETR", "CETR", "GETR3", "CETR3", "GETR5", "CETR5",
  "TSTA", "TSDA", "A_GETR", "A_CETR", "A_GETR3", "A_CETR3", "A_GETR5", "A_CETR5",
  "forn", "own", "KOSPI", "big4", "LOSS"
)

vars_excluidas_pca <- c(
  "stock", "name", "firm", "company", "ticker", "id", "year",
  "target_ROA", "ROA", "ROE"
)

vars_removidas_colinealidad <- c("GETR3", "GETR5", "A_GETR3", "A_GETR5")

# OJO metodológico: target_ROA es ROA(t + 1) por firma. Por eso se crea sobre
# el panel completo ANTES de remover outliers; si filtramos primero, el lead
# puede saltar años y deja de significar “próximo año”.
df_con_target <- crear_target_roa(df)

df_modelo <- filtrar_outliers_iqr_predictores(
  df_con_target,
  vars = vars_pca_candidatas
)

particiones <- dividir_temporalmente(df_modelo)

cat("\nSplit temporal para OLS-PC:\n")
cat(sprintf("  Train (year <= 2022, con target): %s filas\n", format(nrow(particiones$train), big.mark = ",")))
cat(sprintf("  Test  (year == 2023, con target): %s filas\n", format(nrow(particiones$test), big.mark = ",")))
cat(sprintf("  Futuro(year == 2024, sin exigir target): %s filas\n", format(nrow(particiones$future), big.mark = ",")))


# PCA snapshot interpretativo ---------------------------------------------

# Esta sección usa el objeto `df` creado por el flujo principal.
# El PCA NO usa target_ROA, ROA ni ROE: buscamos perfiles financieros explicativos,
# no variables que revelen directa o indirectamente la rentabilidad objetivo.
# Este PCA snapshot contesta “cómo se posicionan las firmas en un año”. Es
# interpretativo y NO se reutiliza para el OLS-PC predictivo/validatorio.
if (!exists("ANIO_PCA", inherits = TRUE)) {
  ANIO_PCA <- 2023
}

df_base_pca <- df_con_target

# Snapshot temporal: si existe el año pedido, analizamos un solo corte transversal.
# Esto evita mezclar diferencias entre empresas con efectos macro de distintos años.
if ("year" %in% names(df_base_pca) && ANIO_PCA %in% df_base_pca$year) {
  df_base_pca <- df_base_pca %>%
    filter(year == ANIO_PCA)
  message(sprintf("PCA: usando snapshot del año %s.", ANIO_PCA))
} else if ("year" %in% names(df_base_pca)) {
  message(sprintf(
    "PCA: el año %s no está disponible; se usan todas las filas.",
    ANIO_PCA
  ))
} else {
  message("PCA: no existe columna `year`; se usan todas las filas.")
}

df_pca <- preparar_matriz_pca(
  df_base_pca,
  vars_candidatas = vars_pca_candidatas,
  vars_excluidas = vars_excluidas_pca,
  vars_removidas_colinealidad = vars_removidas_colinealidad
)

cat("\nPCA – variables usadas:\n")
cat(sprintf("  n observaciones completas: %s\n", format(nrow(df_pca), big.mark = ",")))
cat(sprintf("  n variables: %d\n", ncol(df_pca)))
cat(sprintf("  %s\n", paste(names(df_pca), collapse = ", ")))

vars_colineales_presentes <- intersect(vars_removidas_colinealidad, names(df_base_pca))
if (length(vars_colineales_presentes) > 0) {
  cat(sprintf(
    "  Eliminadas por colinealidad manual: %s\n",
    paste(vars_colineales_presentes, collapse = ", ")
  ))
}

pca_snapshot <- FactoMineR::PCA(
  df_pca,
  scale.unit = TRUE,
  graph = FALSE
)

eigen_pca <- as.data.frame(pca_snapshot$eig)
cat("\nPCA – eigenvalues y varianza explicada:\n")
print(round(eigen_pca, 3))

# Gráficos en memoria: se muestran/guardan manualmente solo si el alumno lo decide.
p_pca_scree <- factoextra::fviz_eig(
  pca_snapshot,
  addlabels = TRUE,
  barfill = "#1E3A5F",
  barcolor = "#1E3A5F"
)

p_pca_var <- factoextra::fviz_pca_var(
  pca_snapshot,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
)

p_pca_ind <- factoextra::fviz_pca_ind(
  pca_snapshot,
  geom = "point",
  pointsize = 1.8,
  alpha.ind = 0.65,
  repel = TRUE
)

p_pca_biplot <- factoextra::fviz_pca_biplot(
  pca_snapshot,
  axes = c(1, 2),
  repel = TRUE,
  col.var = "#D97706",
  col.ind = "#1E3A5F",
  alpha.ind = 0.55
)

p_pca_scree
p_pca_var
p_pca_ind
p_pca_biplot


# OLS con componentes principales -----------------------------------------

# El OLS-PC es OTRO flujo: PCA se aprende solo en train (year <= 2022) y el
# test 2023 se proyecta con el centro/escala/rotación del train. Esto evita
# fuga temporal. No usamos `pca_snapshot` acá porque ese objeto describe un
# corte transversal para interpretación, no una base de validación temporal.
prep_train_ols <- preparar_matriz_pca(
  particiones$train,
  vars_candidatas = vars_pca_candidatas,
  vars_excluidas = vars_excluidas_pca,
  vars_removidas_colinealidad = vars_removidas_colinealidad,
  return_source_rows = TRUE
)
x_train_ols <- prep_train_ols$x

df_train_ols <- particiones$train[prep_train_ols$source_rows, , drop = FALSE]
pca_train <- ajustar_pca_train(
  cbind(year = df_train_ols$year, x_train_ols),
  vars = names(x_train_ols)
)

scores_train <- proyectar_pca(pca_train, x_train_ols)
modelo_ols_pc <- ajustar_ols_pc(scores_train, df_train_ols$target_ROA)
diagnosticos_ols_pc <- diagnosticar_ols_pc(modelo_ols_pc)

cat("\nOLS-PC sobre train histórico:\n")
cat(sprintf("  Componentes retenidos por Kaiser: %d\n", pca_train$k_kaiser))
print(summary(modelo_ols_pc))

cat("\nDiagnósticos OLS-PC — residuos atípicos:\n")
print(diagnosticos_ols_pc$residuos_atipicos)
cat("\nDiagnósticos OLS-PC — alto leverage:\n")
print(diagnosticos_ols_pc$leverage)
cat("\nDiagnósticos OLS-PC — influencia Cook's D:\n")
print(diagnosticos_ols_pc$cook)

if (nrow(particiones$test) > 0) {
  x_test_candidato <- particiones$test[, pca_train$vars, drop = FALSE]
  test_completo <- stats::complete.cases(x_test_candidato) &
    apply(x_test_candidato, 1, function(row) all(is.finite(row)))
  df_test_ols <- particiones$test[test_completo, , drop = FALSE]
  scores_test <- proyectar_pca(pca_train, df_test_ols)
  pred_test_ols_pc <- stats::predict(modelo_ols_pc, newdata = scores_test)
  comparacion_test_ols_pc <- data.frame(
    obs = seq_len(nrow(df_test_ols)),
    year = df_test_ols$year,
    target_ROA_real = df_test_ols$target_ROA,
    target_ROA_predicho = pred_test_ols_pc,
    residuo = df_test_ols$target_ROA - pred_test_ols_pc
  )

  cat("\nValidación temporal OLS-PC — test 2023:\n")
  print(utils::head(comparacion_test_ols_pc, 10))
}
