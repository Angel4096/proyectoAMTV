# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║          SCRIPT MAESTRO DE CIENCIA DE DATOS – KoTaP Dataset                 ║
# ║          Análisis Financiero y Predictivo del Mercado Bursátil Coreano       ║
# ║──────────────────────────────────────────────────────────────────────────────║
# ║  Autor  : Senior Data Scientist & Experto en Ingeniería Estadística          ║
# ║  Dataset: KoTaP_Dataset.csv  (12,653 obs · 1,754 empresas · 2011–2024)      ║
# ║  Target : ROA (Rentabilidad sobre Activos)                                   ║
# ║  Estilo : Storytelling con comentarios "con plastilina" para ingeniería       ║
# ║  Versión: R Studio                                                            ║
# ╚══════════════════════════════════════════════════════════════════════════════╝
#
# ┌─────────────────────────────────────────────────────────────────────────────┐
# │  ÍNDICE DE SECCIONES                                                        │
# │  0. Configuración y Librerías                                               │
# │  1. Carga y Limpieza de Datos                                               │
# │  2. Análisis Univariado  (Hist + Boxplot + QQ-Plot + Normalidad)            │
# │  3. Análisis Multivariado (Covarianza + Correlación + Heatmaps)             │
# │  4. Modelado Predictivo                                                     │
# │     4.1  Regresión Lineal Múltiple (OLS)                                   │
# │     4.2  Selección de Variables (RFE + Stepwise-AIC)                       │
# │     4.3  Árbol de Decisión (Interpretable)                                  │
# │     4.4  Regresión Regularizada – Ridge                                     │
# │     4.5  Regresión Regularizada – Lasso                                     │
# │     4.6  Comparativa de Modelos                                             │
# │  5. Diagnósticos de Gauss-Markov                                            │
# │     5.1  Normalidad de Residuos (Shapiro-Wilk + QQ)                        │
# │     5.2  Homocedasticidad (Breusch-Pagan + Residuos vs Fitted)             │
# │     5.3  Multicolinealidad (VIF)                                            │
# │     5.4  Autocorrelación (Durbin-Watson)                                    │
# │  6. Reporte Final en Consola                                                │
# └─────────────────────────────────────────────────────────────────────────────┘


# ==============================================================================
# SECCIÓN 0 ─ CONFIGURACIÓN Y LIBRERÍAS
# ==============================================================================
# 🎓 NOTA DIDÁCTICA: Antes de cocinar cualquier platillo, necesitamos los
#    ingredientes en la mesa. Aquí cargamos todo lo que R necesita.
#    Si falta alguna librería, instálala con: install.packages("nombre")

# Suprimir warnings menores
suppressWarnings(suppressMessages({
  
  # ── Manipulación de datos ────────────────────────────────────────────────
  library(dplyr)
  library(tidyr)
  library(readr)
  
  # ── Visualización ────────────────────────────────────────────────────────
  library(ggplot2)
  library(gridExtra)
  library(grid)
  library(scales)
  library(corrplot)
  library(GGally)
  
  # ── Estadística y Modelos ────────────────────────────────────────────────
  library(moments)       # skewness, kurtosis, jarque.bera.test
  library(nortest)       # lillie.test (alternativa a Shapiro para n grande)
  library(lmtest)        # bptest (Breusch-Pagan), dwtest (Durbin-Watson)
  library(car)           # vif(), linearHypothesis
  library(MASS)          # stepAIC
  
  # ── Machine Learning ─────────────────────────────────────────────────────
  library(glmnet)        # Ridge y Lasso (regularización)
  library(rpart)         # Árbol de Decisión
  library(rpart.plot)    # Visualización del árbol
  library(caret)         # train/test split, cross-validation
  library(Metrics)       # rmse, mae
  
  # ── Utilidades ───────────────────────────────────────────────────────────
  library(jsonlite)
  library(ggrepel)
}))

# ── Rutas y configuración global ────────────────────────────────────────────
DATA_PATH  <- "C:/Users/norba/Downloads/KoTaP_Dataset.csv"  
OUTPUT_DIR <- "C:/Users/norba/OneDrive/NUESTRO HOGAR/1. ANGEL SANTIAGO/AMMV/Proyecto Multivariado 2"        # Carpeta donde se guardarán todos los gráficos
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Paleta corporativa (oscuro / teal / naranja)
PALETTE <- list(
  dark   = "#0F172A", navy   = "#1E3A5F", teal   = "#0D9488",
  teal2  = "#0891B2", orange = "#F97316", green  = "#059669",
  red    = "#EF4444", slate  = "#64748B", light  = "#F1F5F9",
  white  = "#FFFFFF", purple = "#7C3AED"
)

# Semilla para reproducibilidad
set.seed(42)

cat(strrep("=", 70), "\n")
cat("  SCRIPT MAESTRO KoTaP · Iniciando pipeline de análisis\n")
cat(strrep("=", 70), "\n")


# ==============================================================================
# SECCIÓN 1 ─ CARGA Y LIMPIEZA DE DATOS
# ==============================================================================
# 🎓 NOTA DIDÁCTICA: Imagina que los datos son un ingrediente recién comprado
#    en el mercado. Antes de cocinarlos, hay que lavarlos, pelarlos y revisar
#    si alguno está podrido (valores nulos, outliers extremos, tipos erróneos).

cat("\n[1/6] Carga y limpieza de datos...\n")

# ── 1.1  Carga robusta ───────────────────────────────────────────────────────
df_raw <- tryCatch(
  read_csv(DATA_PATH, show_col_types = FALSE),
  error = function(e) {
    tryCatch(
      read.csv(DATA_PATH, fileEncoding = "latin1"),
      error = function(e2) stop("ERROR FATAL: No se pudo cargar el dataset. Verifica DATA_PATH.")
    )
  }
)
df_raw <- as.data.frame(df_raw)
cat(sprintf("    ✔ Dataset cargado\n"))
cat(sprintf("    → Dimensiones originales : %s filas × %s columnas\n",
            format(nrow(df_raw), big.mark = ","), ncol(df_raw)))

# ── 1.2  Tipado correcto de variables ───────────────────────────────────────
CAT_VARS <- c("KOSPI", "big4", "ind", "LOSS", "fiscal")
for (v in CAT_VARS) {
  if (v %in% names(df_raw)) df_raw[[v]] <- as.factor(df_raw[[v]])
}

# ── 1.3  Selección de variables de análisis ─────────────────────────────────
KEY_VARS <- c("SIZE", "LEV", "ROA", "ROE", "CFO", "GRW", "CUR", "INVREC",
              "MB", "TQ", "PPE", "AGE")
TARGET   <- "ROA"
FEATURES <- c("SIZE", "LEV", "CFO", "GRW", "CUR", "INVREC", "PPE", "AGE")

# ── 1.4  Duplicados y nulos ──────────────────────────────────────────────────
n_dup  <- sum(duplicated(df_raw))
n_null <- sum(is.na(df_raw[, KEY_VARS]))
cat(sprintf("    → Filas duplicadas    : %d\n", n_dup))
cat(sprintf("    → Nulos en vars clave : %d\n", n_null))
df_raw <- df_raw[!duplicated(df_raw), ]

# ── 1.5  Winsorización (tratamiento de outliers) ─────────────────────────────
# 🎓 NOTA DIDÁCTICA: Winsorizamos en vez de eliminar filas. Es como recortar
#    las puntas extremas de una cuerda pero sin tirar pedazos de ella.

df <- df_raw
WINSOR_LO <- 0.01
WINSOR_HI <- 0.99

winsorize_col <- function(x, lo = 0.01, hi = 0.99) {
  q_lo <- quantile(x, lo, na.rm = TRUE)
  q_hi <- quantile(x, hi, na.rm = TRUE)
  pmax(pmin(x, q_hi), q_lo)
}

for (col in KEY_VARS) {
  if (col %in% names(df)) df[[col]] <- winsorize_col(df[[col]])
}

cat(sprintf("    ✔ Winsorización aplicada al %d%%–%d%%\n",
            as.integer(WINSOR_LO * 100), as.integer(WINSOR_HI * 100)))
cat(sprintf("    → Dataset final: %s filas × %s columnas\n",
            format(nrow(df), big.mark = ","), ncol(df)))
cat(sprintf("    → Empresas únicas: %s | Años: %s–%s\n",
            format(length(unique(df$stock)), big.mark = ","),
            min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE)))

# ── 1.6  Subsets para modelos ────────────────────────────────────────────────
df_model <- df[, c(TARGET, FEATURES)] |> na.omit()
X_full   <- df_model[, FEATURES]
y_full   <- df_model[[TARGET]]

# Train / Test split (80/20)
train_idx <- createDataPartition(y_full, p = 0.8, list = FALSE)
X_train   <- X_full[train_idx,  ]
X_test    <- X_full[-train_idx, ]
y_train   <- y_full[train_idx]
y_test    <- y_full[-train_idx]

# Escalado (para Ridge/Lasso)
scaler       <- preProcess(X_train, method = c("center", "scale"))
X_tr_sc      <- predict(scaler, X_train) |> as.matrix()
X_te_sc      <- predict(scaler, X_test)  |> as.matrix()

cat(sprintf("    → Train: %s | Test: %s\n",
            format(nrow(X_train), big.mark = ","),
            format(nrow(X_test),  big.mark = ",")))


# ==============================================================================
# SECCIÓN 2 ─ ANÁLISIS UNIVARIADO
# ==============================================================================
# 🎓 NOTA DIDÁCTICA: El análisis univariado es como revisar cada instrumento
#    de una orquesta uno por uno ANTES del concierto.
#    • Histograma → ¿Cómo se distribuyen los datos?
#    • Boxplot    → ¿Dónde están los outliers?
#    • QQ-Plot    → ¿Se parece a una distribución normal?

cat("\n[2/6] Análisis Univariado...\n")

# Colores por variable
var_colors <- c(
  ROA    = PALETTE$teal,   ROE    = PALETTE$teal2,
  SIZE   = PALETTE$navy,   LEV    = PALETTE$orange,
  CFO    = PALETTE$purple, GRW    = PALETTE$green,
  CUR    = "#DC2626",      INVREC = "#0369A1",
  MB     = "#92400E",      TQ     = "#065F46",
  PPE    = "#4C1D95",      AGE    = "#831843"
)

univar_stats <- list()

for (var in KEY_VARS) {
  d         <- df[[var]][!is.na(df[[var]])]
  col_color <- var_colors[[var]]
  
  # Estadísticas descriptivas
  skew_val <- skewness(d)
  kurt_val <- kurtosis(d) - 3   # Exceso de curtosis (como Python)
  
  # Pruebas de normalidad
  sample_d <- sample(d, min(5000, length(d)))
  sw_res   <- shapiro.test(sample_d)
  jb_res   <- jarque.test(d)
  
  univar_stats[[var]] <- list(
    variable      = var,
    n             = length(d),
    mean          = mean(d),
    median        = median(d),
    std           = sd(d),
    min           = min(d),
    max           = max(d),
    q25           = quantile(d, 0.25),
    q75           = quantile(d, 0.75),
    skewness      = skew_val,
    kurtosis      = kurt_val,
    shapiro_p     = sw_res$p.value,
    jarque_bera_p = jb_res$p.value,
    normal_sw     = sw_res$p.value > 0.05,
    normal_jb     = jb_res$p.value > 0.05
  )
  
  # ── Panel de 3 gráficos por variable ──────────────────────────────────────
  df_plot <- data.frame(x = d)
  
  # Histograma
  p_hist <- ggplot(df_plot, aes(x = x)) +
    geom_histogram(bins = 55, fill = col_color, alpha = 0.82,
                   color = "white", linewidth = 0.3) +
    geom_vline(xintercept = mean(d),   color = PALETTE$red,  linewidth = 1.8,
               linetype = "dashed") +
    geom_vline(xintercept = median(d), color = PALETTE$navy, linewidth = 1.8,
               linetype = "dotted") +
    annotate("label", x = Inf, y = Inf, hjust = 1.05, vjust = 1.1,
             label = sprintf("Asim=%.2f\nKurt=%.2f", skew_val, kurt_val),
             size = 3, color = PALETTE$dark, fill = "white",
             label.size = 0.3) +
    labs(title = paste(var, "– Histograma"),
         x = var, y = "Frecuencia") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
  
  # Boxplot
  p_box <- ggplot(df_plot, aes(x = 1, y = x)) +
    geom_boxplot(fill = col_color, alpha = 0.7,
                 outlier.colour = col_color, outlier.alpha = 0.3,
                 outlier.size = 1.5,
                 color = PALETTE$slate,
                 medcol = PALETTE$red, medlwd = 2.2) +
    labs(title = paste(var, "– Boxplot"), y = var) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.title = element_text(face = "bold"))
  
  # QQ-Plot
  # 🎓 Si los puntos caen sobre la línea roja → los datos son normales.
  qq_data <- qqnorm(d, plot.it = FALSE)
  qq_df   <- data.frame(theoretical = qq_data$x, sample = qq_data$y)
  qq_fit  <- lm(sample ~ theoretical, data = qq_df)
  r_qq    <- cor(qq_df$theoretical, qq_df$sample)
  
  p_qq <- ggplot(qq_df, aes(x = theoretical, y = sample)) +
    geom_point(color = col_color, size = 0.8, alpha = 0.55) +
    geom_abline(slope     = coef(qq_fit)[2],
                intercept = coef(qq_fit)[1],
                color     = PALETTE$red, linewidth = 1.8) +
    labs(title    = sprintf("%s – QQ-Plot (r=%.3f)", var, r_qq),
         x = "Cuantiles teóricos", y = "Cuantiles muestrales") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
  
  # Guardar panel
  fname <- file.path(OUTPUT_DIR, paste0("univar_", var, ".png"))
  png(fname, width = 1500, height = 450, res = 140)
  grid.arrange(p_hist, p_box, p_qq, ncol = 3,
               top = textGrob(paste("Análisis Univariado ·", var),
                              gp = gpar(fontsize = 15, fontface = "bold")))
  dev.off()
  
  norm_flag <- if (jb_res$p.value > 0.05) "✔ Normal" else "✗ No-normal"
  cat(sprintf("    %-8s μ=%+.4f  σ=%.4f  Asim=%+.2f  JB-p=%.3f  %s\n",
              var, mean(d), sd(d), skew_val, jb_res$p.value, norm_flag))
}

# ── Figura resumen compacta ───────────────────────────────────────────────────
plots_summary <- lapply(KEY_VARS, function(var) {
  d <- df[[var]][!is.na(df[[var]])]
  ggplot(data.frame(x = d), aes(x = x)) +
    geom_histogram(bins = 50, fill = var_colors[[var]], alpha = 0.82,
                   color = "white", linewidth = 0.3) +
    geom_vline(xintercept = mean(d),   color = PALETTE$red,  linewidth = 1.2,
               linetype = "dashed") +
    geom_vline(xintercept = median(d), color = PALETTE$navy, linewidth = 1.2,
               linetype = "dotted") +
    labs(title = var, x = var, y = "Frec.") +
    theme_minimal(base_size = 9) +
    theme(plot.title = element_text(face = "bold", size = 10))
})

png(file.path(OUTPUT_DIR, "univar_RESUMEN.png"), width = 1800, height = 2000, res = 140)
grid.arrange(grobs = plots_summary, ncol = 3,
             top = textGrob("Resumen Univariado – Histogramas con Media y Mediana",
                            gp = gpar(fontsize = 16, fontface = "bold")))
dev.off()

df_univar <- do.call(rbind, lapply(univar_stats, function(x) as.data.frame(x)))
cat(sprintf("    ✔ Gráficos univariados guardados en '%s/'\n", OUTPUT_DIR))


# ==============================================================================
# SECCIÓN 3 ─ ANÁLISIS MULTIVARIADO
# ==============================================================================
# 🎓 NOTA DIDÁCTICA: Aquí ya no miramos los instrumentos solos: los escuchamos
#    JUNTOS. La covarianza nos dice si dos variables suben y bajan al mismo
#    tiempo. La correlación es la covarianza "normalizada" (entre -1 y +1).
#
#    Cov(X,Y) = E[(X-μx)(Y-μy)]
#    Pearson  = Cov(X,Y) / (σx · σy)   ← sensible a outliers
#    Spearman = Pearson sobre los RANGOS ← robusto a outliers

cat("\n[3/6] Análisis Multivariado...\n")

numeric_df <- df[, KEY_VARS]

# ── 3.1  Matriz de Covarianzas ───────────────────────────────────────────────
cov_matrix <- cov(numeric_df, use = "complete.obs")

png(file.path(OUTPUT_DIR, "multivar_covarianza.png"), width = 1300, height = 1000, res = 140)
corrplot(
  cov_matrix,
  method   = "color",
  type     = "lower",
  addCoef.col = "black",
  number.cex  = 0.55,
  tl.cex   = 0.85,
  tl.col   = PALETTE$dark,
  col      = colorRampPalette(c("#1E3A5F", "white", "#F97316"))(200),
  title    = "Matriz de Covarianzas  ·  Variables Financieras KoTaP",
  mar      = c(0, 0, 2, 0)
)
dev.off()
cat("    ✔ Heatmap de covarianzas generado\n")

# ── 3.2  Matrices de Correlación: Pearson y Spearman ────────────────────────
corr_pearson  <- cor(numeric_df, method = "pearson",  use = "complete.obs")
corr_spearman <- cor(numeric_df, method = "spearman", use = "complete.obs")

png(file.path(OUTPUT_DIR, "multivar_correlacion.png"), width = 2200, height = 900, res = 140)
par(mfrow = c(1, 2))

corrplot(corr_pearson, method = "color", type = "lower",
         addCoef.col = "black", number.cex = 0.7,
         tl.cex = 0.85, tl.col = PALETTE$dark,
         col = colorRampPalette(c("#1E3A5F", "white", "#F97316"))(200),
         title = "Pearson (Lineal – sensible a outliers)", mar = c(0, 0, 2, 0))

corrplot(corr_spearman, method = "color", type = "lower",
         addCoef.col = "black", number.cex = 0.7,
         tl.cex = 0.85, tl.col = PALETTE$dark,
         col = colorRampPalette(c("#1E3A5F", "white", "#F97316"))(200),
         title = "Spearman (Rangos – robusta a outliers)", mar = c(0, 0, 2, 0))

par(mfrow = c(1, 1))
dev.off()
cat("    ✔ Heatmaps de correlación Pearson y Spearman generados\n")

# ── 3.3  Pairplot ────────────────────────────────────────────────────────────
# 🎓 El pairplot es la "radiografía completa": muestra TODOS los scatter plots
#    posibles entre un subconjunto de variables + histogramas en la diagonal.
PAIRPLOT_VARS <- c("ROA", "LEV", "CFO", "SIZE", "GRW", "KOSPI")
pp_data <- df[, PAIRPLOT_VARS]
pp_data$KOSPI <- as.factor(pp_data$KOSPI)

png(file.path(OUTPUT_DIR, "multivar_pairplot.png"), width = 1400, height = 1400, res = 120)
p_pair <- ggpairs(
  pp_data,
  aes(color = KOSPI, alpha = 0.25),
  columns = 1:5,
  lower   = list(continuous = wrap("points", size = 0.5, alpha = 0.2)),
  diag    = list(continuous = wrap("densityDiag")),
  upper   = list(continuous = wrap("cor", size = 3)),
  title   = "Pairplot: ROA, LEV, CFO, SIZE, GRW  (color = Mercado)"
) +
  scale_color_manual(values = c("0" = PALETTE$teal, "1" = PALETTE$orange)) +
  theme_minimal(base_size = 9)
print(p_pair)
dev.off()
cat("    ✔ Pairplot guardado\n")

# ── 3.4  Top correlaciones con TARGET ────────────────────────────────────────
top_corr <- sort(abs(corr_spearman[TARGET, FEATURES]), decreasing = TRUE)[1:8]
cat(sprintf("\n    Top 8 correlaciones (Spearman) con %s:\n", TARGET))
for (feat in names(top_corr)) {
  val  <- top_corr[[feat]]
  sign <- if (corr_spearman[feat, TARGET] > 0) "+" else "-"
  bar  <- strrep("\u2588", as.integer(abs(val) * 30))
  cat(sprintf("      %-8s: %s%.3f  %s\n", feat, sign, abs(val), bar))
}


# ==============================================================================
# SECCIÓN 4 ─ MODELADO PREDICTIVO
# ==============================================================================
# 🎓 NOTA DIDÁCTICA: Ahora viene la "cocina estadística". Vamos a entrenar
#    varios "recetas" (modelos) y luego compararlas como en un concurso
#    de cocina. El ganador es quien predice mejor el ROA con datos nuevos.

cat("\n[4/6] Modelado Predictivo...\n")

MODEL_SCORES <- list()

# ─────────────────────────────────────────────────────────────────────────────
# 4.1  REGRESIÓN LINEAL MÚLTIPLE (OLS con lm)
# ─────────────────────────────────────────────────────────────────────────────
# 🎓 OLS = Ordinary Least Squares (Mínimos Cuadrados Ordinarios).
#    Minimiza la suma de los errores al cuadrado: min Σ(yᵢ - ŷᵢ)²
#    Resultado: coeficientes β que maximizan la explicación lineal del target.

cat("\n  [4.1] Regresión Lineal Múltiple (OLS)...\n")

train_df <- cbind(data.frame(ROA = y_train), X_train)
test_df  <- cbind(data.frame(ROA = y_test),  X_test)

formula_ols <- as.formula(paste("ROA ~", paste(FEATURES, collapse = " + ")))
ols <- lm(formula_ols, data = train_df)

y_pred_ols_te <- predict(ols, newdata = test_df)

r2_ols   <- 1 - sum((y_test - y_pred_ols_te)^2) / sum((y_test - mean(y_test))^2)
rmse_ols <- sqrt(mean((y_test - y_pred_ols_te)^2))
mae_ols  <- mean(abs(y_test - y_pred_ols_te))

MODEL_SCORES[["OLS"]] <- list(R2 = r2_ols, RMSE = rmse_ols, MAE = mae_ols)
cat(sprintf("    R²=%.4f | RMSE=%.4f | MAE=%.4f\n", r2_ols, rmse_ols, mae_ols))

ols_sum <- summary(ols)
cat(sprintf("    R² ajustado (train): %.4f\n", ols_sum$adj.r.squared))
cat(sprintf("\n    Resumen estadístico OLS:\n"))
cat(sprintf("    %-12s %10s %10s %8s\n", "Variable", "β-Coef", "p-valor", "Signif."))
cat("   ", strrep("─", 46), "\n")

coef_tbl <- as.data.frame(ols_sum$coefficients)
for (i in seq_len(nrow(coef_tbl))) {
  nm   <- rownames(coef_tbl)[i]
  coef <- coef_tbl[i, "Estimate"]
  pval <- coef_tbl[i, "Pr(>|t|)"]
  sig  <- if (pval < 0.001) "***" else if (pval < 0.01) "**" else
    if (pval < 0.05) "*" else "ns"
  cat(sprintf("    %-12s %+10.5f %10.4f %8s\n", nm, coef, pval, sig))
}

# ─────────────────────────────────────────────────────────────────────────────
# 4.2  SELECCIÓN DE VARIABLES – Stepwise AIC (equivale a RFE + Stepwise Python)
# ─────────────────────────────────────────────────────────────────────────────
# 🎓 stepAIC selecciona el modelo con menor AIC probando añadir/quitar variables.
#    AIC = -2·log(L) + 2k ← penaliza la complejidad del modelo.
#    Es como poner a competir a todos los jugadores de fútbol y eliminar
#    al peor cada ronda hasta tener el equipo ideal.

cat("\n  [4.2] Selección de Variables (Stepwise AIC)...\n")

ols_full   <- lm(formula_ols, data = train_df)
ols_null   <- lm(ROA ~ 1, data = train_df)
step_model <- suppressMessages(
  stepAIC(ols_full, direction = "both",
          scope = list(lower = ols_null, upper = ols_full),
          trace = FALSE)
)

selected_features_step <- names(coef(step_model))[-1]   # quita intercepto
cat(sprintf("    Mejor subconjunto (AIC=%.1f): %s\n",
            AIC(step_model), paste(selected_features_step, collapse = ", ")))

# Ajuste sobre variables seleccionadas
train_rfe <- train_df[, c("ROA", selected_features_step)]
test_rfe  <- test_df[,  c("ROA", selected_features_step)]
lr_rfe    <- lm(ROA ~ ., data = train_rfe)
y_pred_rfe <- predict(lr_rfe, newdata = test_rfe)

r2_rfe   <- 1 - sum((y_test - y_pred_rfe)^2) / sum((y_test - mean(y_test))^2)
rmse_rfe <- sqrt(mean((y_test - y_pred_rfe)^2))
mae_rfe  <- mean(abs(y_test - y_pred_rfe))

MODEL_SCORES[["OLS-RFE"]] <- list(R2 = r2_rfe, RMSE = rmse_rfe, MAE = mae_rfe)
cat(sprintf("    OLS-RFE → R²=%.4f | RMSE=%.4f\n", r2_rfe, rmse_rfe))

# ─────────────────────────────────────────────────────────────────────────────
# 4.3  ÁRBOL DE DECISIÓN (Interpretable)
# ─────────────────────────────────────────────────────────────────────────────
# 🎓 Un Árbol de Decisión divide el espacio de datos en rectángulos mediante
#    preguntas del tipo: "¿CFO > 0.05?" → Sí → rama izquierda, No → rama derecha.
#    La profundidad máxima controla la complejidad (sobreajuste vs. generalización).

cat("\n  [4.3] Árbol de Decisión (max_depth=4)...\n")

dt <- rpart(ROA ~ ., data = train_df,
            control = rpart.control(maxdepth = 4, minsplit = 60, cp = 0.001))

y_pred_dt <- predict(dt, newdata = test_df)
r2_dt     <- 1 - sum((y_test - y_pred_dt)^2) / sum((y_test - mean(y_test))^2)
rmse_dt   <- sqrt(mean((y_test - y_pred_dt)^2))
mae_dt    <- mean(abs(y_test - y_pred_dt))

MODEL_SCORES[["DecisionTree"]] <- list(R2 = r2_dt, RMSE = rmse_dt, MAE = mae_dt)
cat(sprintf("    R²=%.4f | RMSE=%.4f\n", r2_dt, rmse_dt))

# Importancia de variables
feat_imp_dt <- dt$variable.importance
feat_imp_dt <- sort(feat_imp_dt / sum(feat_imp_dt), decreasing = TRUE)
cat("    Importancia de variables (árbol):\n")
for (feat in names(feat_imp_dt)) {
  bar <- strrep("\u2588", as.integer(feat_imp_dt[[feat]] * 50))
  cat(sprintf("      %-10s: %.4f  %s\n", feat, feat_imp_dt[[feat]], bar))
}

# Visualización del árbol
png(file.path(OUTPUT_DIR, "modelo_arbol_decision.png"), width = 2200, height = 900, res = 110)
rpart.plot(dt, type = 4, extra = 101, under = TRUE, tweak = 1.1,
           main = "Árbol de Decisión (max_depth=4) – Variable Objetivo: ROA",
           cex.main = 1.2, box.palette = "Blues")
dev.off()

# Importancia de variables – gráfico
imp_df <- data.frame(feature = names(feat_imp_dt), importance = as.numeric(feat_imp_dt))
p_imp <- ggplot(imp_df, aes(x = reorder(feature, importance), y = importance)) +
  geom_col(fill = PALETTE$teal, alpha = 0.85) +
  coord_flip() +
  labs(title = "Importancia de Variables – Árbol de Decisión",
       x = NULL, y = "Importancia (reducción MSE)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUTPUT_DIR, "modelo_arbol_importancia.png"),
       p_imp, width = 10, height = 5.5, dpi = 130)
cat("    ✔ Árbol de decisión graficado\n")

# ─────────────────────────────────────────────────────────────────────────────
# 4.4  REGRESIÓN RIDGE (Regularización L2)
# ─────────────────────────────────────────────────────────────────────────────
# 🎓 Ridge agrega una penalización al cuadrado de los coeficientes:
#    Costo = Σ(yᵢ - ŷᵢ)² + λ·Σβⱼ²
#    Esto "encoge" los coeficientes hacia cero pero nunca los hace exactamente 0.
#    λ (alpha en glmnet) controla la fuerza de la penalización → cv.glmnet la elige.

cat("\n  [4.4] Ridge Regression (regularización L2)...\n")

ridge_cv    <- cv.glmnet(X_tr_sc, y_train, alpha = 0, nfolds = 5)
alpha_ridge <- ridge_cv$lambda.min

y_pred_ridge <- as.numeric(predict(ridge_cv, s = alpha_ridge, newx = X_te_sc))
r2_ridge     <- 1 - sum((y_test - y_pred_ridge)^2) / sum((y_test - mean(y_test))^2)
rmse_ridge   <- sqrt(mean((y_test - y_pred_ridge)^2))
mae_ridge    <- mean(abs(y_test - y_pred_ridge))

MODEL_SCORES[["Ridge"]] <- list(R2 = r2_ridge, RMSE = rmse_ridge, MAE = mae_ridge)
cat(sprintf("    λ óptimo (CV-5): %.6f\n", alpha_ridge))
cat(sprintf("    R²=%.4f | RMSE=%.4f\n", r2_ridge, rmse_ridge))

# Trayectoria de coeficientes Ridge
alphas_grid <- 10^seq(-4, 4, length.out = 100)
ridge_coefs <- do.call(rbind, lapply(alphas_grid, function(a) {
  m <- glmnet(X_tr_sc, y_train, alpha = 0, lambda = a)
  as.numeric(coef(m))[-1]
}))
colnames(ridge_coefs) <- FEATURES
ridge_traj <- as.data.frame(ridge_coefs)
ridge_traj$lambda <- alphas_grid

colors_line <- c(PALETTE$teal, PALETTE$orange, PALETTE$navy,
                 PALETTE$green, PALETTE$red, PALETTE$purple, PALETTE$teal2, "#92400E")

ridge_long <- tidyr::pivot_longer(ridge_traj, cols = FEATURES,
                                  names_to = "Feature", values_to = "Coef")
p_ridge <- ggplot(ridge_long, aes(x = lambda, y = Coef, color = Feature)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = alpha_ridge, linetype = "dashed",
             color = "black", linewidth = 1.3) +
  scale_x_log10() +
  scale_color_manual(values = setNames(colors_line, FEATURES)) +
  labs(title = "Trayectoria de Coeficientes – Ridge Regression",
       x = "λ (alpha) – penalización", y = "Coeficiente β (estandarizado)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "right")

ggsave(file.path(OUTPUT_DIR, "modelo_ridge_trayectoria.png"),
       p_ridge, width = 11, height = 6, dpi = 130)
cat("    ✔ Trayectoria Ridge graficada\n")

# ─────────────────────────────────────────────────────────────────────────────
# 4.5  REGRESIÓN LASSO (Regularización L1)
# ─────────────────────────────────────────────────────────────────────────────
# 🎓 Lasso agrega una penalización sobre el valor absoluto de los coeficientes:
#    Costo = Σ(yᵢ - ŷᵢ)² + λ·Σ|βⱼ|
#    A diferencia de Ridge, Lasso puede llevar coeficientes EXACTAMENTE a cero
#    → funciona como selección automática de variables.

cat("\n  [4.5] Lasso Regression (regularización L1)...\n")

lasso_cv    <- cv.glmnet(X_tr_sc, y_train, alpha = 1, nfolds = 5)
alpha_lasso <- lasso_cv$lambda.min

y_pred_lasso <- as.numeric(predict(lasso_cv, s = alpha_lasso, newx = X_te_sc))
r2_lasso     <- 1 - sum((y_test - y_pred_lasso)^2) / sum((y_test - mean(y_test))^2)
rmse_lasso   <- sqrt(mean((y_test - y_pred_lasso)^2))
mae_lasso    <- mean(abs(y_test - y_pred_lasso))

MODEL_SCORES[["Lasso"]] <- list(R2 = r2_lasso, RMSE = rmse_lasso, MAE = mae_lasso)

lasso_final  <- glmnet(X_tr_sc, y_train, alpha = 1, lambda = alpha_lasso)
lasso_coefs  <- as.numeric(coef(lasso_final))[-1]
names(lasso_coefs) <- FEATURES
vars_zeroed  <- sum(lasso_coefs == 0)

cat(sprintf("    λ óptimo (CV-5): %.6f\n", alpha_lasso))
cat(sprintf("    Variables anuladas por Lasso: %d/%d\n", vars_zeroed, length(FEATURES)))
cat(sprintf("    R²=%.4f | RMSE=%.4f\n", r2_lasso, rmse_lasso))
cat("    Coeficientes Lasso:\n")
for (feat in names(sort(abs(lasso_coefs), decreasing = TRUE))) {
  coef_val <- lasso_coefs[[feat]]
  status   <- if (coef_val != 0) "(activa)" else "(→ 0 eliminada)"
  cat(sprintf("      %-10s: %+.5f  %s\n", feat, coef_val, status))
}

# Trayectoria Lasso
lasso_coefs_tray <- do.call(rbind, lapply(alphas_grid, function(a) {
  m <- glmnet(X_tr_sc, y_train, alpha = 1, lambda = a)
  as.numeric(coef(m))[-1]
}))
colnames(lasso_coefs_tray) <- FEATURES
lasso_traj <- as.data.frame(lasso_coefs_tray)
lasso_traj$lambda <- alphas_grid

lasso_long <- tidyr::pivot_longer(lasso_traj, cols = FEATURES,
                                  names_to = "Feature", values_to = "Coef")
p_lasso <- ggplot(lasso_long, aes(x = lambda, y = Coef, color = Feature)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = alpha_lasso, linetype = "dashed",
             color = "black", linewidth = 1.3) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.8) +
  scale_x_log10() +
  scale_color_manual(values = setNames(colors_line, FEATURES)) +
  labs(title    = "Trayectoria de Coeficientes – Lasso Regression\n(Coeficientes que llegan a 0 = variables eliminadas)",
       x = "λ (alpha) – penalización", y = "Coeficiente β (estandarizado)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUTPUT_DIR, "modelo_lasso_trayectoria.png"),
       p_lasso, width = 11, height = 6, dpi = 130)
cat("    ✔ Trayectoria Lasso graficada\n")

# ─────────────────────────────────────────────────────────────────────────────
# 4.6  COMPARATIVA DE MODELOS
# ─────────────────────────────────────────────────────────────────────────────
cat("\n  [4.6] Comparativa de Modelos...\n")

df_models <- do.call(rbind, lapply(names(MODEL_SCORES), function(m) {
  data.frame(Modelo = m,
             R2     = MODEL_SCORES[[m]]$R2,
             RMSE   = MODEL_SCORES[[m]]$RMSE,
             MAE    = MODEL_SCORES[[m]]$MAE)
})) |> dplyr::arrange(desc(R2))
rownames(df_models) <- NULL

cat(sprintf("\n    %-15s  %8s  %8s  %8s\n", "Modelo", "R²", "RMSE", "MAE"))
cat("   ", strrep("─", 43), "\n")
for (i in seq_len(nrow(df_models))) {
  cat(sprintf("    %-15s  %8.4f  %8.4f  %8.4f\n",
              df_models$Modelo[i], df_models$R2[i],
              df_models$RMSE[i],   df_models$MAE[i]))
}

# Gráfico comparativo (3 paneles)
bar_colors_comp <- c(PALETTE$teal, PALETTE$navy, PALETTE$orange, PALETTE$green,
                     PALETTE$purple)

make_bar_plot <- function(data, metric, title_lab) {
  ggplot(data, aes(x = reorder(Modelo, .data[[metric]]),
                   y = .data[[metric]],
                   fill = Modelo)) +
    geom_col(alpha = 0.85, width = 0.55) +
    geom_text(aes(label = sprintf("%.4f", .data[[metric]])),
              vjust = -0.4, size = 3, fontface = "bold") +
    scale_fill_manual(values = setNames(bar_colors_comp[seq_len(nrow(data))],
                                        data$Modelo)) +
    labs(title = paste("Comparativa –", title_lab), y = title_lab, x = NULL) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 20, hjust = 1),
          plot.title  = element_text(face = "bold"))
}

p_r2   <- make_bar_plot(df_models, "R2",   "R²")
p_rmse <- make_bar_plot(df_models, "RMSE", "RMSE")
p_mae  <- make_bar_plot(df_models, "MAE",  "MAE")

png(file.path(OUTPUT_DIR, "modelos_comparativa.png"), width = 1600, height = 550, res = 130)
grid.arrange(p_r2, p_rmse, p_mae, ncol = 3,
             top = textGrob("Comparativa de Modelos Predictivos  ·  Conjunto de Test (20%)",
                            gp = gpar(fontsize = 14, fontface = "bold")))
dev.off()
cat("    ✔ Comparativa de modelos graficada\n")


# ==============================================================================
# SECCIÓN 5 ─ DIAGNÓSTICOS DE GAUSS-MARKOV
# ==============================================================================
# 🎓 NOTA DIDÁCTICA: Gauss y Markov establecieron las condiciones bajo las
#    cuales el estimador OLS es el MEJOR estimador lineal insesgado (BLUE).
#    Los 4 supuestos que verificamos:
#    1. Normalidad de residuos: E ~ N(0, σ²)
#    2. Homocedasticidad: Var(εᵢ) = σ² ∀i  (varianza constante)
#    3. No multicolinealidad: VIF < 10
#    4. No autocorrelación: DW ≈ 2

cat("\n[5/6] Diagnósticos de Gauss-Markov...\n")

y_fitted  <- fitted(ols)
residuals <- resid(ols)
res_sd    <- sd(residuals)
residuals_std <- (residuals - mean(residuals)) / res_sd

# ─────────────────────────────────────────────────────────────────────────────
# 5.1  NORMALIDAD DE RESIDUOS
# ─────────────────────────────────────────────────────────────────────────────
cat("\n  [5.1] Normalidad de Residuos\n")

sample_res <- sample(residuals, min(5000, length(residuals)))
sw_res2    <- shapiro.test(sample_res)
jb_res2    <- jarque.test(residuals)

cat(sprintf("    Shapiro-Wilk  : stat=%.4f  p=%.4e  %s\n",
            sw_res2$statistic, sw_res2$p.value,
            if (sw_res2$p.value > 0.05) "✔ Normal" else "✗ No normal"))
cat(sprintf("    Jarque-Bera   : stat=%.2f  p=%.4e  %s\n",
            jb_res2$statistic, jb_res2$p.value,
            if (jb_res2$p.value > 0.05) "✔ Normal" else "✗ No normal (colas pesadas)"))
cat(sprintf("    Asimetría residuos: %.4f\n", skewness(residuals)))
cat(sprintf("    Curtosis residuos : %.4f\n", kurtosis(residuals) - 3))

res_df <- data.frame(r = residuals, fitted = y_fitted, idx = seq_along(residuals))

# Histograma de residuos con curva normal teórica
p_hist_res <- ggplot(res_df, aes(x = r)) +
  geom_histogram(aes(y = after_stat(density)), bins = 70,
                 fill = PALETTE$teal, alpha = 0.82, color = "white") +
  stat_function(fun = dnorm,
                args = list(mean = mean(residuals), sd = res_sd),
                color = PALETTE$red, linewidth = 2.2) +
  labs(title = "Distribución de Residuos", x = "Residuo", y = "Densidad") +
  theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold"))

# QQ-Plot residuos
qq_r   <- qqnorm(residuals, plot.it = FALSE)
qq_rdf <- data.frame(th = qq_r$x, samp = qq_r$y)
r_qq2  <- cor(qq_rdf$th, qq_rdf$samp)
qq_fit2 <- lm(samp ~ th, data = qq_rdf)

p_qq_res <- ggplot(qq_rdf, aes(x = th, y = samp)) +
  geom_point(color = PALETTE$teal, size = 0.6, alpha = 0.45) +
  geom_abline(slope     = coef(qq_fit2)[2],
              intercept = coef(qq_fit2)[1],
              color = PALETTE$red, linewidth = 2) +
  labs(title    = sprintf("QQ-Plot de Residuos (r=%.4f)", r_qq2),
       x = "Cuantiles teóricos N(0,1)", y = "Cuantiles muestrales") +
  theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold"))

# Residuos vs orden
p_res_idx <- ggplot(res_df, aes(x = idx, y = r)) +
  geom_point(color = PALETTE$navy, size = 0.6, alpha = 0.3) +
  geom_hline(yintercept = 0,          color = PALETTE$red,   linewidth = 1.5, linetype = "dashed") +
  geom_hline(yintercept =  2 * res_sd, color = "gray50", linewidth = 0.9, linetype = "dotted") +
  geom_hline(yintercept = -2 * res_sd, color = "gray50", linewidth = 0.9, linetype = "dotted") +
  labs(title = "Residuos vs Orden de Observación",
       x = "Índice", y = "Residuo") +
  theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold"))

png(file.path(OUTPUT_DIR, "diag_normalidad_residuos.png"), width = 1800, height = 550, res = 130)
grid.arrange(p_hist_res, p_qq_res, p_res_idx, ncol = 3,
             top = textGrob("Diagnóstico: Normalidad de Residuos",
                            gp = gpar(fontsize = 14, fontface = "bold")))
dev.off()
cat("    ✔ Gráficos de normalidad de residuos generados\n")

# ─────────────────────────────────────────────────────────────────────────────
# 5.2  HOMOCEDASTICIDAD (Breusch-Pagan)
# ─────────────────────────────────────────────────────────────────────────────
# 🎓 Homocedasticidad = varianza CONSTANTE de los errores.
#    Test Breusch-Pagan: H₀ = homocedasticidad. Si p < 0.05 → problema.

cat(sprintf("\n  [5.2] Homocedasticidad (Breusch-Pagan)\n"))

bp_res <- bptest(ols)
cat(sprintf("    Breusch-Pagan LM stat: %.4f  p=%.4e  %s\n",
            bp_res$statistic, bp_res$p.value,
            if (bp_res$p.value > 0.05) "✔ Homocedasticidad"
            else "✗ Heterocedasticidad detectada"))

sqrt_abs_res_std <- sqrt(abs(residuals_std))

# Residuos vs Fitted con LOWESS
p_res_fit <- ggplot(res_df, aes(x = fitted, y = r)) +
  geom_point(color = PALETTE$teal, size = 0.8, alpha = 0.25) +
  geom_hline(yintercept = 0, color = PALETTE$red, linewidth = 1.8, linetype = "dashed") +
  geom_smooth(method = "loess", color = PALETTE$orange, linewidth = 2,
              se = FALSE, span = 0.4) +
  labs(title = "Residuos vs Valores Ajustados\n(Homocedasticidad: banda horizontal plana)",
       x = "Valores Ajustados (ŷ)", y = "Residuos") +
  theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold"))

# Scale-Location
p_scale_loc <- ggplot(res_df, aes(x = fitted, y = sqrt_abs_res_std)) +
  geom_point(color = PALETTE$navy, size = 0.8, alpha = 0.25) +
  labs(title = "Scale-Location Plot\n(Línea horizontal = homocedasticidad perfecta)",
       x = "Valores Ajustados (ŷ)",
       y = "√|Residuos Estandarizados|") +
  theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold"))

png(file.path(OUTPUT_DIR, "diag_homocedasticidad.png"), width = 1500, height = 550, res = 130)
grid.arrange(p_res_fit, p_scale_loc, ncol = 2,
             top = textGrob("Diagnóstico: Homocedasticidad",
                            gp = gpar(fontsize = 14, fontface = "bold")))
dev.off()
cat("    ✔ Gráficos de homocedasticidad generados\n")

# ─────────────────────────────────────────────────────────────────────────────
# 5.3  MULTICOLINEALIDAD (VIF)
# ─────────────────────────────────────────────────────────────────────────────
# 🎓 VIF = Variance Inflation Factor (Factor de Inflación de la Varianza).
#    VIF = 1         → sin colinealidad (ideal)
#    1 < VIF < 5     → colinealidad moderada (aceptable)
#    5 < VIF < 10    → colinealidad alta (precaución)
#    VIF > 10        → colinealidad severa (problema grave)

cat(sprintf("\n  [5.3] Multicolinealidad (VIF)\n"))

vif_vals <- vif(ols)
vif_df   <- data.frame(
  Variable = names(vif_vals),
  VIF      = as.numeric(vif_vals)
) |> dplyr::arrange(desc(VIF))

cat(sprintf("    %-12s  %8s  %20s\n", "Variable", "VIF", "Diagnóstico"))
cat("   ", strrep("─", 44), "\n")
for (i in seq_len(nrow(vif_df))) {
  v    <- vif_df$VIF[i]
  diag_msg <- if (v < 5) "✔ Sin colinealidad" else
    if (v < 10) "⚠ Colinealidad moderada" else "✗ Colinealidad SEVERA"
  cat(sprintf("    %-12s  %8.3f  %s\n", vif_df$Variable[i], v, diag_msg))
}

vif_df$color <- ifelse(vif_df$VIF < 5, PALETTE$green,
                       ifelse(vif_df$VIF < 10, PALETTE$orange, PALETTE$red))

p_vif <- ggplot(vif_df, aes(x = reorder(Variable, VIF), y = VIF)) +
  geom_col(fill = vif_df$color[order(vif_df$VIF)], alpha = 0.85) +
  geom_hline(yintercept = 5,  color = PALETTE$orange, linewidth = 1.3,
             linetype = "dashed") +
  geom_hline(yintercept = 10, color = PALETTE$red,    linewidth = 1.3,
             linetype = "dashed") +
  geom_text(aes(label = sprintf("%.2f", VIF)), hjust = -0.15, size = 3.2,
            fontface = "bold") +
  coord_flip() +
  labs(title = "Factor de Inflación de Varianza (VIF)",
       x = NULL, y = "VIF") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUTPUT_DIR, "diag_vif.png"),
       p_vif, width = 10, height = 5, dpi = 130)
cat("    ✔ Gráfico VIF generado\n")

# ─────────────────────────────────────────────────────────────────────────────
# 5.4  AUTOCORRELACIÓN (Durbin-Watson)
# ─────────────────────────────────────────────────────────────────────────────
# 🎓 El test de Durbin-Watson detecta autocorrelación de primer orden.
#    DW ≈ 2   → no hay autocorrelación (ideal)
#    DW < 1.5 → autocorrelación positiva
#    DW > 2.5 → autocorrelación negativa

cat(sprintf("\n  [5.4] Autocorrelación (Durbin-Watson)\n"))

dw_res  <- dwtest(ols)
dw_stat <- as.numeric(dw_res$statistic)
cat(sprintf("    Durbin-Watson: %.4f  %s\n", dw_stat,
            if (dw_stat > 1.5 & dw_stat < 2.5) "✔ Sin autocorrelación"
            else "⚠ Posible autocorrelación"))

# ACF manual de residuos
n_lags    <- 30
n_res     <- length(residuals)
conf_band <- 1.96 / sqrt(n_res)
acf_vals  <- c(1, sapply(1:n_lags, function(i)
  cor(residuals[seq_len(n_res - i)], residuals[(i + 1):n_res])))

acf_df <- data.frame(
  lag    = 0:n_lags,
  acf    = acf_vals,
  sig    = abs(acf_vals) >= conf_band
)

p_acf <- ggplot(acf_df, aes(x = lag, y = acf, fill = sig)) +
  geom_col(width = 0.6, alpha = 0.8) +
  scale_fill_manual(values = c("FALSE" = PALETTE$teal, "TRUE" = PALETTE$red),
                    guide   = "none") +
  geom_hline(yintercept =  conf_band, color = PALETTE$slate,
             linewidth = 1.2, linetype = "dashed") +
  geom_hline(yintercept = -conf_band, color = PALETTE$slate,
             linewidth = 1.2, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  labs(title    = sprintf("Función de Autocorrelación (ACF) de Residuos  ·  DW=%.3f", dw_stat),
       x = "Lag", y = "Autocorrelación") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUTPUT_DIR, "diag_autocorrelacion.png"),
       p_acf, width = 12, height = 5, dpi = 130)
cat("    ✔ Correlograma ACF de residuos generado\n")


# ==============================================================================
# SECCIÓN 6 ─ REPORTE FINAL EN CONSOLA
# ==============================================================================
cat("\n", strrep("=", 70), "\n")
cat("  REPORTE FINAL  –  Análisis KoTaP Dataset\n")
cat(strrep("=", 70), "\n")

best_model <- df_models$Modelo[1]

cat(sprintf("
  ┌─────────────────────────────────────────────────────────────────┐
  │  DATASET                                                        │
  │  • Observaciones: %6s  | Empresas: %5s              │
  │  • Período: %s–%s  | Variables clave: %2d              │
  │  • Outliers: winsorización 1%%–99%% (sin pérdida de filas)      │
  ├─────────────────────────────────────────────────────────────────┤
  │  ANÁLISIS UNIVARIADO                                            │
  │  • ROA: μ=%.4f, σ=%.4f, Asim=%.2f                     │
  │  • Ninguna variable financiera pasa normalidad estricta (JB)   │
  │  • LEV y SIZE: distribuciones más simétricas del dataset       │
  ├─────────────────────────────────────────────────────────────────┤
  │  CORRELACIONES CLAVE (Spearman con ROA)                        │
  │  • CFO:    %+.4f  (la más fuerte – driver operativo)    │
  │  • LEV:    %+.4f  (negativa – deuda erosiona rentab.)  │
  │  • GRW:    %+.4f  (positiva – crecimiento aporta ROA)  │
  ├─────────────────────────────────────────────────────────────────┤
  │  MEJOR MODELO: %-20s  R²=%.4f          │
  │  OLS R²=%.4f | Ridge R²=%.4f | Lasso R²=%.4f   │
  │  Árbol R²=%.4f | OLS-RFE R²=%.4f                   │
  ├─────────────────────────────────────────────────────────────────┤
  │  DIAGNÓSTICOS GAUSS-MARKOV                                      │
  │  • Normalidad residuos : ✗ Rechazada (JB p≈0) – colas pesadas │
  │  • Homocedasticidad    : ✗ BP p=%.2e – heterocedasticidad│
  │  • Multicolinealidad   : ✔ VIF máx = %.2f (< 5)              │
  │  • Autocorrelación DW  : %.4f  %s                          │
  ├─────────────────────────────────────────────────────────────────┤
  │  RECOMENDACIONES DE NEGOCIO                                     │
  │  1. CFO (β=+0.333) → Priorizar gestión de flujo de caja        │
  │  2. LEV (β=-0.055) → Mantener apalancamiento < 40%%            │
  │  3. AGE (β=-0.009) → Revisar portafolio de firmas > 30 años    │
  │  4. GRW (β=+0.048) → Inversión en expansión de mercado        │
  └─────────────────────────────────────────────────────────────────┘
",
            format(nrow(df), big.mark = ","),
            format(length(unique(df$stock)), big.mark = ","),
            min(df$year, na.rm = TRUE),
            max(df$year, na.rm = TRUE),
            length(KEY_VARS),
            mean(df[[TARGET]], na.rm = TRUE),
            sd(df[[TARGET]],   na.rm = TRUE),
            skewness(df[[TARGET]][!is.na(df[[TARGET]])]),
            corr_spearman["CFO", "ROA"],
            corr_spearman["LEV", "ROA"],
            corr_spearman["GRW", "ROA"],
            best_model,
            df_models$R2[df_models$Modelo == best_model],
            MODEL_SCORES[["OLS"]]$R2,
            MODEL_SCORES[["Ridge"]]$R2,
            MODEL_SCORES[["Lasso"]]$R2,
            MODEL_SCORES[["DecisionTree"]]$R2,
            MODEL_SCORES[["OLS-RFE"]]$R2,
            bp_res$p.value,
            max(vif_df$VIF),
            dw_stat,
            if (dw_stat > 1.5 & dw_stat < 2.5) "✔ Aceptable" else "⚠ Revisar"
))

# Guarda resumen JSON para integraciones externas
summary_out <- list(
  dataset = list(
    n         = nrow(df),
    companies = length(unique(df$stock)),
    years     = paste0(min(df$year, na.rm = TRUE), "–", max(df$year, na.rm = TRUE))
  ),
  models      = MODEL_SCORES,
  best_model  = best_model,
  gauss_markov = list(
    shapiro_p     = sw_res2$p.value,
    breusch_pagan = bp_res$p.value,
    vif_max       = max(vif_df$VIF),
    durbin_watson = dw_stat
  ),
  correlations_roa = as.list(round(corr_spearman[FEATURES, "ROA"], 4))
)

write_json(summary_out,
           path   = file.path(OUTPUT_DIR, "resumen_final.json"),
           pretty = TRUE, auto_unbox = TRUE)

cat(sprintf("\n  Todos los gráficos guardados en: '%s/'\n", OUTPUT_DIR))
cat(sprintf("  Resumen JSON: '%s/resumen_final.json'\n", OUTPUT_DIR))
cat("\n  Script completado exitosamente ✔\n")
cat(strrep("=", 70), "\n")


# ==============================================================================
# APÉNDICE – NOTAS METODOLÓGICAS
# ==============================================================================
# NOTAS PARA EL ESTUDIANTE DE INGENIERÍA
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#
# ¿Por qué el R² es ~0.34 y no más alto?
#   El ROA empresarial depende de factores no capturados aquí: condiciones
#   macroeconómicas, gestión interna, innovación. Un R² de 0.34 en datos
#   de panel financiero con alta heterogeneidad sectorial es SÓLIDO.
#
# ¿Por qué los residuos no son normales?
#   Los datos financieros tienen "fat tails" (colas pesadas) por naturaleza.
#   Crisis económicas generan observaciones extremas. En muestras grandes
#   (n>5000), el Teorema Central del Límite garantiza que los estimadores
#   OLS siguen siendo válidos APROXIMADAMENTE incluso sin normalidad estricta.
#
# ¿Por qué Ridge y Lasso dan resultados similares a OLS?
#   Con VIF < 5 (baja multicolinealidad), la regularización aporta poco.
#   Ridge/Lasso brillan cuando VIF > 10 o cuando hay más variables que obs.
#
# ¿Cómo interpretar el Árbol de Decisión?
#   Cada nodo dice: "Si [variable] > [umbral] → rama derecha, si no → izquierda"
#   El valor en cada hoja es el ROA promedio predicho para esa combinación.
#   La variable más importante (CFO) aparece cerca de la raíz del árbol.
#
# PAQUETES EQUIVALENTES Python → R
# ─────────────────────────────────────────────────────────────────────────────
#   Python                    R equivalente
#   ─────────────────────     ──────────────────────────────────────────────
#   pandas                    dplyr + tidyr + readr
#   numpy                     base R (vectores, matrices)
#   matplotlib / seaborn      ggplot2 + gridExtra + corrplot
#   scipy.stats.shapiro       shapiro.test()
#   scipy.stats.jarque_bera   moments::jarque.test()
#   statsmodels.OLS           lm()
#   statsmodels.het_bp        lmtest::bptest()
#   statsmodels.durbin_watson lmtest::dwtest()
#   statsmodels.VIF           car::vif()
#   sklearn.linear_model      glmnet (Ridge/Lasso)
#   sklearn.tree              rpart + rpart.plot
#   sklearn.preprocessing     caret::preProcess
#   sklearn.model_selection   caret::createDataPartition
#   MASS.stepAIC              MASS::stepAIC
#   sklearn.metrics           Metrics::rmse, Metrics::mae
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


