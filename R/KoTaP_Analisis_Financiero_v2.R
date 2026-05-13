# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║         SCRIPT KoTaP v6.0 – Correcciones aplicadas                         ║
# ║           1. Outliers: eliminación por IQR (no winsorización)              ║
# ║           2. Histogramas: boundary=0, leyenda clara, sin raya negra        ║
# ║           3. Correlación: solo Spearman (sin Pearson, sin covarianza)      ║
# ║           4. Regresión: un solo gráfico principal, sin top-correlaciones   ║
# ║           5. KMeans: sin Silhouette, biplot un solo color                  ║
# ║           6. Boxplots finales con muescas (IC 95% mediana)                 ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ==============================================================================
# SECCIÓN 0 ─ PAQUETES
# ==============================================================================
paquetes <- c(
  "e1071", "tidyverse", "patchwork",
  "cluster", "factoextra", "scales", "ggrepel",
  "jsonlite", "data.table", "tseries",
  "treemap", "ggfittext", "ggtext"
)
instalar_faltantes <- function(pkgs) {
  f <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  if (length(f) > 0) install.packages(f, dependencies = TRUE)
}
instalar_faltantes(paquetes)

suppressPackageStartupMessages({
  library(e1071);      library(tidyverse); library(patchwork)
  library(cluster);    library(factoextra)
  library(scales);     library(ggrepel)
  library(jsonlite);   library(data.table); library(tseries)
  library(treemap);    library(ggfittext);  library(ggtext)
})

DATA_PATH  <- r"(C:\Users\norba\Downloads\KoTaP_Dataset.csv)"
OUTPUT_DIR <- "output_kotap_v6"
dir.create(OUTPUT_DIR, showWarnings = FALSE)

# ── Paleta ────────────────────────────────────────────────────────────────────
PAL <- list(
  dark   = "#0F172A", navy   = "#1E3A5F", teal   = "#0D9488",
  teal2  = "#0891B2", orange = "#F97316", green  = "#059669",
  red    = "#EF4444", slate  = "#475569", white  = "#FFFFFF",
  purple = "#7C3AED", pink   = "#DB2777", amber  = "#D97706",
  rose   = "#E11D48"
)

HEAT_POS <- "#C0392B"
HEAT_NEU <- "#FFFFFF"
HEAT_NEG <- "#1D4E89"

auto_txt <- function(hex) {
  r <- strtoi(substr(hex, 2, 3), 16) / 255
  g <- strtoi(substr(hex, 4, 5), 16) / 255
  b <- strtoi(substr(hex, 6, 7), 16) / 255
  ifelse(0.299*r + 0.587*g + 0.114*b > 0.50, "#0F172A", "#FFFFFF")
}

# ── Tema global ───────────────────────────────────────────────────────────────
tema_k <- theme_minimal(base_size = 12) +
  theme(
    plot.background   = element_rect(fill = "white",   color = NA),
    panel.background  = element_rect(fill = "#F8FAFC", color = NA),
    panel.grid.major  = element_line(color = "#E2E8F0", linewidth = 0.4),
    panel.grid.minor  = element_line(color = "#E2E8F0", linewidth = 0.2),
    plot.title        = element_text(face = "bold", color = PAL$dark, size = 13),
    plot.subtitle     = element_text(color = PAL$slate, size = 9.5),
    axis.text         = element_text(color = PAL$slate),
    axis.title        = element_text(color = PAL$dark),
    legend.background = element_rect(fill = "white", color = "#CBD5E1"),
    strip.background  = element_rect(fill = "#EFF6FF"),
    strip.text        = element_text(face = "bold", color = PAL$dark)
  )
theme_set(tema_k)

REGISTRO <- list()
reg <- function(p, nm) { print(p); REGISTRO[[nm]] <<- p; invisible(p) }

set.seed(42)
cat(strrep("=", 70), "\n  KoTaP v6.0\n", strrep("=", 70), "\n")


# ==============================================================================
# SECCIÓN 1 ─ CARGA Y LIMPIEZA
# ==============================================================================
# CAMBIO 1: Se reemplaza winsorización por eliminación de outliers extremos.
# Método: IQR con factor k = 3.  Un valor se considera outlier extremo si
#   x < Q1 − 3·IQR   o   x > Q3 + 3·IQR
# k = 3 (Tukey "far out") es más conservador que k = 1.5: solo elimina los
# valores verdaderamente extremos, preservando la mayor parte de los datos.
# La winsorización reemplazaba los extremos por los percentiles 1% y 99%,
# lo que introducía una masa artificial de valores idénticos en los extremos
# del rango y sesgaba los coeficientes de la regresión.
# ─────────────────────────────────────────────────────────────────────────────
cat("\n[1/7] Carga y limpieza...\n")



df_raw <- NULL
for (enc in c("unknown", "CP949", "UTF-8-BOM", "latin1")) {
  ok <- tryCatch({
    tmp <- if (enc == "unknown")
      data.table::fread(DATA_PATH, encoding = enc, data.table = FALSE)
    else
      read.csv(DATA_PATH, stringsAsFactors = FALSE, fileEncoding = enc)
    if (nrow(tmp) > 0) { df_raw <- tmp; TRUE } else FALSE
  }, error = function(e) FALSE,
  warning = function(w) !is.null(df_raw) && nrow(df_raw) > 0)
  if (isTRUE(ok) && nrow(df_raw) > 0) {
    cat(sprintf("  OK encoding='%s'\n", enc)); break
  }
}
if (is.null(df_raw) || nrow(df_raw) == 0)
  stop("No se cargó el CSV. Guarda el archivo como UTF-8 en Excel.")

for (v in c("KOSPI", "big4", "ind", "LOSS", "fiscal"))
  if (v %in% names(df_raw)) df_raw[[v]] <- as.factor(df_raw[[v]])

KEY_VARS <- c("SIZE","LEV","ROA","ROE","CFO","GRW","CUR","INVREC",
              "MB","TQ","PPE","AGE")
TARGET   <- "ROA"
FEATURES <- c("SIZE","LEV","CFO","GRW","CUR","INVREC","PPE","AGE")

for (col in KEY_VARS)
  if (col %in% names(df_raw))
    df_raw[[col]] <- suppressWarnings(as.numeric(df_raw[[col]]))

df_raw <- df_raw[!duplicated(df_raw), ]

# ── Función de eliminación de outliers extremos (IQR ×3) ─────────────────────
eliminar_outliers_iqr <- function(data, vars, k = 3) {
  # Construye una máscara lógica: TRUE = observación válida en TODAS las vars
  mask <- rep(TRUE, nrow(data))
  for (col in vars) {
    if (!col %in% names(data) || !is.numeric(data[[col]])) next
    q   <- quantile(data[[col]], probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    if (iqr == 0) next   # si IQR = 0 no aplicar (ej: variable constante)
    lim_lo <- q[1] - k * iqr
    lim_hi <- q[2] + k * iqr
    dentro <- !is.na(data[[col]]) &
      data[[col]] >= lim_lo &
      data[[col]] <= lim_hi
    mask <- mask & dentro
  }
  data[mask, ]
}

n_antes <- nrow(df_raw)
df <- eliminar_outliers_iqr(df_raw, KEY_VARS, k = 3)
n_despues <- nrow(df)
cat(sprintf("  Outliers eliminados (IQR×3): %d filas removidas  (%d → %d)\n",
            n_antes - n_despues, n_antes, n_despues))

yr_min <- as.integer(min(df$year, na.rm = TRUE))
yr_max <- as.integer(max(df$year, na.rm = TRUE))
n_emp  <- length(unique(df$stock))
cat(sprintf("  Dataset final: %d filas | %d empresas | %d–%d\n",
            nrow(df), n_emp, yr_min, yr_max))

df_model <- df[, c(TARGET, FEATURES)] |> na.omit()
if (nrow(df_model) < 20) stop("df_model muy pequeño, revisa columnas.")

set.seed(42)
idx      <- sample(nrow(df_model), floor(0.8 * nrow(df_model)))
train_df <- df_model[ idx, ]
test_df  <- df_model[-idx, ]
y_train  <- train_df[[TARGET]]
y_test   <- test_df[[TARGET]]

cat(sprintf("  Train: %d | Test: %d\n", nrow(train_df), nrow(test_df)))
readline("\n  [Enter] → Sección 2: Estadística Descriptiva\n")


# ==============================================================================
# SECCIÓN 2 ─ ESTADÍSTICA DESCRIPTIVA
# ==============================================================================
# ── Justificación de Asimetría y Curtosis ─────────────────────────────────────
# La media y la desviación estándar suponen distribuciones simétricas con colas
# normales, condición que casi nunca se cumple en datos financieros.
#
# ASIMETRÍA: mide el sesgo de la distribución.
#   > 0 → cola derecha (pocas empresas muy rentables elevan la media)
#   < 0 → cola izquierda (pocas empresas con pérdidas extremas)
#   |Asim| > 1 → la media ya no es representativa del dato típico.
#
# CURTOSIS: mide el peso de las colas vs la normal.
#   > 0 (leptocúrtica) → más valores extremos de lo esperado → mayor riesgo.
#   < 0 (platicúrtica) → distribución más plana, colas ligeras.
#   Sin este coeficiente, dos variables con la misma σ pueden tener perfiles
#   de riesgo completamente distintos.
# ─────────────────────────────────────────────────────────────────────────────
cat("\n[2/7] Estadística Descriptiva...\n")

VAR_COL <- c(ROA="#0D9488", ROE="#0891B2", SIZE="#1E3A5F", LEV="#F97316",
             CFO="#7C3AED", GRW="#059669", CUR="#DC2626", INVREC="#0369A1",
             MB="#92400E",  TQ="#065F46",  PPE="#4C1D95", AGE="#831843")

panel_desc <- function(var, data, color) {
  d   <- data[[var]]; d <- d[is.finite(d)]
  if (length(d) < 5) return(NULL)
  
  pos  <- sum(d >= 0); neg <- sum(d < 0)
  pct  <- round(pos / length(d) * 100, 1)
  mu   <- mean(d); md <- median(d); sg <- sd(d)
  asim <- round(e1071::skewness(d),  3)
  kurt <- round(e1071::kurtosis(d),  3)
  cv   <- round(sg / abs(mu) * 100,  2)
  
  # ── CAMBIO 2a: boundary = 0 fuerza un borde de bin exactamente en cero ────
  # Esto garantiza que NINGÚN bin cruce el cero: cada barra es totalmente verde
  # (valores ≥ 0) o totalmente roja (valores < 0).
  # Antes, con bins normales, un bin que iba de -0.002 a +0.003 recibía el
  # color de la mayoría de sus observaciones, creando barras "mitad y mitad".
  # ─────────────────────────────────────────────────────────────────────────
  df_p <- data.frame(
    x     = d,
    color = ifelse(d >= 0, "Ganancia (ROA \u2265 0)", "P\u00e9rdida (ROA < 0)")
  )
  
  # ── CAMBIO 2b: se elimina la raya negra en x = 0 (geom_vline negro) ───────
  # La barra de boundary=0 ya separa visualmente los dos grupos; la línea
  # negra vertical sobraba y dificultaba la lectura.
  # ─────────────────────────────────────────────────────────────────────────
  
  # ── CAMBIO 2c: leyenda explícita con scale_fill_manual y name descriptivo ──
  ph <- ggplot(df_p, aes(x = x, fill = color)) +
    geom_histogram(
      bins      = 55,
      boundary  = 0,        # ← bin boundary exactamente en cero (sin barras mixtas)
      alpha     = 0.82,
      color     = "white",
      linewidth = 0.15
    ) +
    scale_fill_manual(
      values = c(
        "Ganancia (ROA \u2265 0)" = PAL$green,
        "P\u00e9rdida (ROA < 0)"  = PAL$red
      ),
      name   = "Signo del valor"     # ← leyenda con nombre descriptivo
    ) +
    geom_vline(xintercept = mu, color = PAL$amber, linewidth = 1.3,
               linetype = "dashed") +
    geom_vline(xintercept = md, color = PAL$navy,  linewidth = 1.3,
               linetype = "dotted") +
    annotate("label",
             x     = Inf, y = Inf,
             label = sprintf(
               "Media   = %.4f\nMediana = %.4f\nDesv.   = %.4f\nCV      = %.2f%%\nAsim    = %.3f\nKurt    = %.3f\nn       = %s",
               mu, md, sg, cv, asim, kurt, format(length(d), big.mark = ",")),
             hjust = 1.05, vjust = 1.05, size = 2.9,
             fill = "#FFFDE7", color = PAL$dark, family = "mono") +
    annotate("text",
             x     = if (is.finite(max(d)) && max(d) != 0) max(d) * 0.55 else 1,
             y     = Inf,
             label = sprintf("Verde: %.1f%%\nRojo: %.1f%%", pct, 100 - pct),
             vjust = 1.6, size = 3.5, color = PAL$dark, fontface = "bold") +
    labs(title = paste(var, "– Distribución"),
         x = var, y = "Frecuencia") +
    theme(legend.position = "bottom",
          legend.key.size = unit(0.4, "cm"))
  
  # ── Boxplot con muescas ───────────────────────────────────────────────────
  yhi <- if (is.finite(max(d))) max(d) * 1.1 else 1
  ylo <- if (is.finite(min(d))) min(d) * 1.1 else -1
  notch_semi <- 1.58 * IQR(d) / sqrt(length(d))
  notch_lo   <- md - notch_semi
  notch_hi   <- md + notch_semi
  
  pb <- ggplot(df_p, aes(x = "", y = x)) +
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=0,   ymax=yhi, fill=PAL$green, alpha=.07) +
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=ylo, ymax=0,   fill=PAL$red,   alpha=.07) +
    geom_boxplot(
      fill       = color, alpha = 0.60, color = PAL$rose,
      linewidth  = 1.1,
      notch      = TRUE, notchwidth = 0.25,
      outlier.alpha = .2, outlier.size = 1.5, outlier.color = color
    ) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 1.0,
               linetype = "dashed") +
    annotate("segment",
             x = 0.62, xend = 0.82,
             y    = notch_hi + (yhi - notch_hi) * 0.15,
             yend = notch_hi,
             arrow = arrow(length = unit(0.22, "cm"), type = "closed"),
             color = PAL$rose, linewidth = 1.4) +
    annotate("label",
             x     = 0.62,
             y     = notch_hi + (yhi - notch_hi) * 0.18,
             label = sprintf("\u2190 MUESCA\nIC 95%% mediana\n[%.4f, %.4f]",
                             notch_lo, notch_hi),
             size = 2.6, color = PAL$rose, fill = "white",
             hjust = 0.5, fontface = "bold") +
    annotate("label", x = 1.47, y = quantile(d, .75),
             label = sprintf("Verde\n%s obs\n(%.1f%%)",
                             format(pos, big.mark = ","), pct),
             size = 2.8, color = PAL$green, fontface = "bold",
             hjust = 0, fill = "white") +
    annotate("label", x = 1.47, y = quantile(d, .25),
             label = sprintf("Rojo\n%s obs\n(%.1f%%)",
                             format(neg, big.mark = ","), 100 - pct),
             size = 2.8, color = PAL$red, fontface = "bold",
             hjust = 0, fill = "white") +
    labs(title = paste(var, "– Boxplot [notch = IC 95% mediana]"),
         y = var, x = NULL) +
    coord_cartesian(xlim = c(0.5, 1.9))
  
  # ── QQ-Plot ───────────────────────────────────────────────────────────────
  qqr <- qqnorm(d, plot.it = FALSE)
  dfq <- data.frame(t = qqr$x, m = qqr$y)
  rq  <- round(cor(dfq$t, dfq$m), 4)
  pq  <- ggplot(dfq, aes(x = t, y = m)) +
    geom_point(color = color, alpha = .45, size = 1.3) +
    geom_abline(slope = sd(d), intercept = mean(d),
                color = PAL$red, linewidth = 1.8) +
    annotate("label", x = -Inf, y = Inf,
             label = sprintf("r = %.4f", rq),
             hjust = -.1, vjust = 1.3, size = 3.5,
             fill = "#FFF3E0", color = PAL$dark) +
    labs(title = paste(var, "– QQ-Plot"),
         x = "Cuantiles teóricos N(0,1)", y = "Cuantiles muestrales")
  
  list(h = ph, b = pb, q = pq,
       s = list(var=var, n=length(d), pos=pos, neg=neg, pct=pct,
                mu=mu, md=md, sg=sg, cv=cv, asim=asim, kurt=kurt))
}

uni_stats <- list()
for (var in KEY_VARS) {
  if (!var %in% names(df)) next
  res <- panel_desc(var, df, VAR_COL[[var]])
  if (is.null(res)) next
  
  p <- (res$h | res$b | res$q) +
    plot_annotation(
      title    = sprintf("Estadística Descriptiva – %s", var),
      subtitle = sprintf(
        "n=%s | Media=%.4f | Desv=%.4f | CV=%.2f%% | Asim=%.3f | Kurt=%.3f | Verde=%.1f%%",
        format(res$s$n, big.mark=","), res$s$mu, res$s$sg,
        res$s$cv, res$s$asim, res$s$kurt, res$s$pct),
      theme = theme(
        plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 9)
      )
    )
  reg(p, paste0("desc_", var, ".png"))
  uni_stats[[var]] <- res$s
  cat(sprintf("  %-6s mu=%+.4f sg=%.4f CV=%.1f%% asim=%+.2f kurt=%.2f\n",
              var, res$s$mu, res$s$sg, res$s$cv, res$s$asim, res$s$kurt))
}

# Grid resumen ─────────────────────────────────────────────────────────────────
pg <- lapply(KEY_VARS, function(var) {
  if (!var %in% names(df)) return(NULL)
  d <- df[[var]]; d <- d[is.finite(d)]
  if (length(d) < 5) return(NULL)
  df_p <- data.frame(
    x  = d,
    cl = ifelse(d >= 0, "Ganancia (\u2265 0)", "P\u00e9rdida (< 0)")
  )
  pct <- round(mean(d >= 0) * 100, 1)
  ggplot(df_p, aes(x = x, fill = cl)) +
    geom_histogram(bins = 50, boundary = 0,         # ← boundary=0 también en el grid
                   alpha = 0.78, color = "white", linewidth = 0.1) +
    scale_fill_manual(
      values = c("Ganancia (\u2265 0)" = PAL$green,
                 "P\u00e9rdida (< 0)"  = PAL$red),
      name = NULL
    ) +
    geom_vline(xintercept = mean(d), color = PAL$amber,
               linewidth = 1, linetype = "dashed") +
    annotate("text", x = Inf, y = Inf,
             label = sprintf("Asim=%.2f\nKurt=%.2f",
                             e1071::skewness(d), e1071::kurtosis(d)),
             hjust = 1.1, vjust = 1.3, size = 2.8,
             color = PAL$dark, fontface = "bold") +
    labs(title = var, x = NULL, y = "Frec.") +
    theme(legend.position = "none")
}) |> Filter(Negate(is.null), x = _)

if (length(pg) > 0) {
  p_grid <- wrap_plots(pg, ncol = 3) +
    plot_annotation(
      title    = "Resumen Descriptivo – Ganancias (verde) vs Pérdidas (rojo)",
      subtitle = "Verde = valor \u2265 0 (ganancia)  |  Rojo = valor < 0 (pérdida)  |  Asimetría y Curtosis en cada panel",
      theme    = theme(
        plot.title    = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(size = 9.5, color = PAL$slate)
      )
    )
  reg(p_grid, "desc_RESUMEN.png")
}
cat("  Descriptiva completada\n")
readline("\n  [Enter] → Sección 3: Correlación Spearman\n")


# ==============================================================================
# SECCIÓN 3 ─ CORRELACIÓN (solo Spearman)
# ==============================================================================
# CAMBIO 3: Se eliminan la matriz de covarianza y la correlación de Pearson.
# Solo se presenta Spearman porque:
#   • Los datos financieros tienen outliers frecuentes → Pearson es sensible.
#   • Las relaciones no siempre son lineales → Spearman captura tendencias.
#   • Spearman trabaja con rangos → robusto sin importar la escala.
# ─────────────────────────────────────────────────────────────────────────────
cat("\n[3/7] Correlación Spearman...\n")

vars_ok  <- KEY_VARS[KEY_VARS %in% names(df)]
num_df   <- df[, vars_ok] |> mutate(across(everything(), as.numeric))
corr_s   <- cor(num_df, method = "spearman", use = "pairwise.complete.obs")

# Función heatmap (triángulo inferior, sin diagonal)
hmap <- function(mat, titulo, subtitulo = "", fmt = "%.2f", lim = NULL) {
  ml <- as.data.frame(as.table(mat)) |> rename(X = Var1, Y = Var2, V = Freq)
  ml <- ml |>
    mutate(xi = as.integer(X), yi = as.integer(Y)) |>
    filter(yi > xi) |>
    select(-xi, -yi)
  
  rng <- if (!is.null(lim)) lim else range(ml$V, na.rm = TRUE)
  pfn <- scales::col_numeric(
    palette = c(HEAT_NEG, HEAT_NEU, HEAT_POS),
    domain  = rng, na.color = "grey90"
  )
  ml$fondo <- pfn(ml$V)
  ml$txt   <- sapply(ml$fondo, auto_txt)
  ml$lbl   <- sprintf(fmt, ml$V)
  
  ggplot(ml, aes(x = X, y = Y, fill = V)) +
    geom_tile(color = "white", linewidth = 0.55) +
    geom_text(aes(label = lbl, color = I(txt)), size = 2.9, fontface = "bold") +
    scale_fill_gradient2(
      low = HEAT_NEG, mid = HEAT_NEU, high = HEAT_POS,
      midpoint = 0, limits = rng, name = NULL,
      guide = guide_colorbar(barwidth = .9, barheight = 9,
                             label.theme = element_text(size = 8, color = PAL$dark))
    ) +
    scale_color_identity() +
    labs(title = titulo, subtitle = subtitulo, x = NULL, y = NULL) +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 9,
                                     color = PAL$dark, face = "bold"),
      axis.text.y     = element_text(size = 9, color = PAL$dark, face = "bold"),
      panel.grid      = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right"
    )
}

p_spearman <- hmap(
  corr_s,
  titulo    = "¿Cómo se asocian las variables por tendencia? (Spearman)",
  subtitulo = paste(
    "Rojo oscuro = asociación positiva fuerte | Azul oscuro = asociación negativa fuerte | Blanco = sin tendencia",
    "Robusto a outliers — no supone linealidad — trabaja con rangos",
    sep = "\n"
  ),
  lim = c(-1, 1)
)
reg(p_spearman, "corr_spearman.png")

# Top correlaciones con ROA (tabla en consola, sin gráfico)
if (TARGET %in% colnames(corr_s)) {
  tc <- sort(abs(corr_s[TARGET, vars_ok[vars_ok != TARGET]]),
             decreasing = TRUE)[1:min(8, length(vars_ok) - 1)]
  cat("\n  Top correlaciones Spearman con ROA:\n")
  for (feat in names(tc)) {
    val  <- corr_s[feat, TARGET]
    bar  <- paste(rep("\u2588", floor(abs(val) * 30)), collapse = "")
    sign <- ifelse(val > 0, "+", "-")
    cat(sprintf("    %-8s: %s%.3f  %s\n", feat, sign, abs(val), bar))
  }
}

cat("  Correlación completada\n")
readline("\n  [Enter] → Sección 4: Regresión Múltiple\n")


# ==============================================================================
# SECCIÓN 4 ─ REGRESIÓN LINEAL MÚLTIPLE
# ==============================================================================
# CAMBIO 4: Se mantiene solo el panel principal (predichos vs reales + betas).
# Se elimina el panel de residuos (segundo gráfico) por ser redundante para
# el propósito del análisis actual.
# Se elimina el gráfico "top correlaciones con ROA" de esta sección.
# ─────────────────────────────────────────────────────────────────────────────
cat("\n[4/7] Regresión Lineal Múltiple...\n")

f_ols <- as.formula(paste(TARGET, "~", paste(FEATURES, collapse = "+")))
mod   <- lm(f_ols, data = train_df)
yp    <- predict(mod, newdata = test_df)
res   <- residuals(mod)
fv    <- fitted(mod)
r2    <- 1 - sum((y_test - yp)^2) / sum((y_test - mean(y_test))^2)
rmse  <- sqrt(mean((y_test - yp)^2))
mae   <- mean(abs(y_test - yp))
r2a   <- summary(mod)$adj.r.squared



cat(sprintf("  R²=%.4f | R²-adj=%.4f | RMSE=%.4f | MAE=%.4f\n",
            r2, r2a, rmse, mae))
ctbl <- summary(mod)$coefficients
cat(sprintf("\n  %-12s  %+10s  %8s  %10s  %8s\n",
            "Variable","Beta","t-stat","p-valor","Signif."))
cat(sprintf("  %s\n", strrep("-", 55)))
for (i in seq_len(nrow(ctbl))) {
  nm <- rownames(ctbl)[i]; b <- ctbl[i,1]; tv <- ctbl[i,3]; pv <- ctbl[i,4]
  sig <- ifelse(pv<.001,"***", ifelse(pv<.01,"**", ifelse(pv<.05,"*","ns")))
  cat(sprintf("  %-12s  %+10.5f  %8.3f  %10.4f  %8s\n", nm, b, tv, pv, sig))
}
fs <- summary(mod)$fstatistic
cat(sprintf("\n  F=%.2f (p=%.4e) | AIC=%.1f | BIC=%.1f\n",
            fs[1], pf(fs[1], fs[2], fs[3], lower.tail=FALSE),
            AIC(mod), BIC(mod)))

# ── Panel superior: Predichos vs Reales ───────────────────────────────────────
dfpv  <- data.frame(obs = y_test, pred = yp, ep = (yp - y_test) > 0)
p_pred <- ggplot(dfpv, aes(x = obs, y = pred, color = ep)) +
  geom_point(alpha = .40, size = 1.8) +
  geom_abline(slope = 1, intercept = 0,
              color = PAL$red, linewidth = 2, linetype = "dashed") +
  geom_ribbon(data = dfpv,
              aes(x = obs, ymin = obs - rmse, ymax = obs + rmse),
              fill = PAL$teal, alpha = .07, inherit.aes = FALSE) +
  scale_color_manual(
    values = c("TRUE" = PAL$orange, "FALSE" = PAL$teal),
    labels = c("TRUE" = "Sobreestimado (pred > real)",
               "FALSE" = "Subestimado (pred < real)"),
    name   = NULL
  ) +
  annotate("label",
           x     = min(y_test, na.rm = TRUE),
           y     = max(yp,     na.rm = TRUE),
           label = sprintf("R\u00b2     = %.4f\nR\u00b2-adj = %.4f\nRMSE   = %.4f\nMAE    = %.4f",
                           r2, r2a, rmse, mae),
           hjust = 0, vjust = 1, size = 3.6,
           fill = "#EFF6FF", color = PAL$dark, family = "mono") +
  labs(
    title    = "Regresión Lineal Múltiple – ROA predicho vs ROA observado",
    subtitle = sprintf("R\u00b2=%.4f | R\u00b2-adj=%.4f | RMSE=%.4f | MAE=%.4f  |  Banda = ±RMSE",
                       r2, r2a, rmse, mae),
    x = "ROA Observado", y = "ROA Predicho"
  ) +
  theme(legend.position = "bottom")

# ── Panel inferior izquierdo: Betas con IC 95% ────────────────────────────────
cdf <- data.frame(
  Variable = rownames(ctbl)[-1],
  B        = ctbl[-1, 1],
  SE       = ctbl[-1, 2],
  p        = ctbl[-1, 4]
) |>
  mutate(
    sig   = ifelse(p < .05, "Significativo (p<0.05)", "No significativo"),
    lo    = B - 1.96 * SE,
    hi    = B + 1.96 * SE,
    label = sprintf("%.4f%s", B, ifelse(p<.001,"***", ifelse(p<.01,"**", ifelse(p<.05,"*",""))))
  ) |>
  arrange(desc(abs(B))) |>
  mutate(Variable = factor(Variable, levels = Variable))

p_betas <- ggplot(cdf, aes(x = B, y = Variable, color = sig)) +
  geom_vline(xintercept = 0, color = "gray35", linewidth = 1.3, linetype = "dashed") +
  geom_segment(aes(x = lo, xend = hi, yend = Variable),
               linewidth = 2.5, alpha = .30) +
  geom_point(size = 5) +
  geom_text(aes(label = label), hjust = -.15, size = 3.3,
            color = PAL$dark, fontface = "bold") +
  scale_color_manual(
    values = c("Significativo (p<0.05)" = PAL$teal,
               "No significativo"        = PAL$slate),
    name = NULL
  ) +
  labs(
    title    = "Coeficientes \u03b2 (ordenados por |valor|)",
    subtitle = "Barras = IC 95% | Derecha de 0 = efecto positivo en ROA\n*** p<0.001  ** p<0.01  * p<0.05",
    x = "Valor del coeficiente \u03b2", y = NULL
  ) +
  theme(legend.position = "bottom")

# ── Panel inferior derecho: Betas estandarizados ──────────────────────────────
sds_x     <- sapply(FEATURES, function(v) sd(train_df[[v]], na.rm = TRUE))
sd_y      <- sd(y_train, na.rm = TRUE)
betas_std <- ctbl[-1, 1] * sds_x / sd_y

df_std <- data.frame(
  Variable = names(betas_std),
  Beta_std = as.numeric(betas_std)
) |>
  arrange(desc(abs(Beta_std))) |>
  mutate(Variable = factor(Variable, levels = Variable))

p_contrib <- ggplot(df_std, aes(
  x    = abs(Beta_std),
  y    = reorder(Variable, abs(Beta_std)),
  fill = Beta_std > 0
)) +
  geom_col(alpha = .85, width = .72) +
  geom_text(aes(label = sprintf("%s\u03b2*=%.3f",
                                ifelse(Beta_std > 0, "+", ""), Beta_std)),
            hjust = -.07, size = 3.2, color = PAL$dark, fontface = "bold") +
  scale_fill_manual(
    values = c("TRUE" = PAL$teal, "FALSE" = PAL$rose),
    labels = c("TRUE" = "Efecto positivo en ROA",
               "FALSE" = "Efecto negativo en ROA"),
    name = NULL
  ) +
  labs(
    title    = "Contribuci\u00f3n relativa (\u03b2 estandarizados)",
    subtitle = "|\u03b2*| permite comparar variables con distintas escalas",
    x = "|\u03b2 estandarizado|", y = NULL
  ) +
  xlim(0, max(abs(df_std$Beta_std)) * 1.35) +
  theme(legend.position = "bottom")

# ── Panel único de regresión ──────────────────────────────────────────────────
p_regresion <- (
  p_pred /
    (p_betas | p_contrib)
) +
  plot_layout(heights = c(1.4, 1)) +
  plot_annotation(
    title    = "Regresión Lineal Múltiple – Panel Completo",
    subtitle = sprintf("R\u00b2=%.4f | R\u00b2-adj=%.4f | F=%.2f (p<0.001) | train=%d | test=%d",
                       r2, r2a, fs[1], nrow(train_df), nrow(test_df)),
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 15, color = PAL$dark),
      plot.subtitle = element_text(size = 9.5, color = PAL$slate)
    )
  )
reg(p_regresion, "ols_regresion.png")

cat("  Regresión completada\n")
readline("\n  [Enter] → Sección 5: PCA\n")


# ==============================================================================
# SECCIÓN 5 ─ ANÁLISIS DE COMPONENTES PRINCIPALES (PCA)
# ==============================================================================
cat("\n[5/7] PCA...\n")

feats_ok <- FEATURES[FEATURES %in% names(df)]
Xpr      <- df[, feats_ok] |> na.omit()
vok      <- sapply(Xpr, function(x) var(x, na.rm = TRUE) > 1e-10)
Xpr      <- Xpr[, vok, drop = FALSE]
fp       <- colnames(Xpr)
Xsc      <- scale(Xpr)
pca      <- prcomp(Xsc, center = FALSE, scale. = FALSE)
ve       <- (pca$sdev^2) / sum(pca$sdev^2)
vc       <- cumsum(ve)
n80      <- min(which(vc >= .80))
n90      <- min(which(vc >= .90))
eig      <- pca$sdev^2

cat("\n  Eigenvalores:\n")
for (i in seq_along(ve))
  cat(sprintf("  PC%-3d  eigen=%.4f  var=%.2f%%  acum=%.2f%%  %s\n",
              i, eig[i], ve[i]*100, vc[i]*100,
              ifelse(eig[i] >= 1, "Retener", "Descartar")))
cat(sprintf("  Comps para 80%%: %d | para 90%%: %d\n", n80, n90))

np  <- length(ve)
dfs <- data.frame(
  PC  = factor(paste0("PC", seq_len(np)), levels = paste0("PC", seq_len(np))),
  eg  = eig, vp = ve * 100, cp = vc * 100
)

p_sc <- ggplot(dfs, aes(x = PC, y = eg)) +
  geom_col(aes(fill = eg >= 1), alpha = .85, width = .7) +
  geom_line(aes(group = 1), color = PAL$orange, linewidth = 1.8) +
  geom_point(color = PAL$orange, size = 3.5) +
  geom_hline(yintercept = 1, color = PAL$red, linewidth = 1.5, linetype = "dashed") +
  annotate("text", x = 1.5, y = 1.08,
           label = "Criterio Kaiser (retener si eigenvalor \u2265 1)",
           color = PAL$red, size = 3, hjust = 0) +
  scale_fill_manual(values = c("TRUE" = PAL$teal, "FALSE" = PAL$slate),
                    labels  = c("TRUE" = "Retener", "FALSE" = "Descartar"),
                    name    = NULL) +
  labs(title    = "Scree Plot – Eigenvalores",
       subtitle = "Teal = eigenvalor \u2265 1 (Criterio Kaiser)",
       x = "Componente", y = "Eigenvalor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

p_cp <- ggplot(dfs, aes(x = PC, y = cp, group = 1)) +
  geom_area(fill = PAL$teal, alpha = .2) +
  geom_line(color = PAL$teal, linewidth = 2) +
  geom_point(size = 3, color = PAL$teal) +
  geom_hline(yintercept = 80, color = PAL$orange, linewidth = 1.4, linetype = "dashed") +
  geom_hline(yintercept = 90, color = PAL$red,    linewidth = 1.4, linetype = "dashed") +
  annotate("label", x = as.integer(n80) + .5, y = 80,
           label = sprintf("80%%: %d PCs", n80),
           color = PAL$orange, fill = "white", size = 3.2) +
  annotate("label", x = as.integer(n90) + .5, y = 90,
           label = sprintf("90%%: %d PCs", n90),
           color = PAL$red, fill = "white", size = 3.2) +
  labs(title = "Varianza Acumulada",
       x = "N\u00b0 Componentes", y = "% Varianza acum.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

reg((p_sc | p_cp) + plot_annotation(
  title = "PCA – Selección de Componentes",
  theme = theme(plot.title = element_text(face = "bold"))
), "pca_scree.png")

# Loadings heatmap ──────────────────────────────────────────────────────────────
ns  <- min(4, ncol(pca$rotation))
lm2 <- pca$rotation[, seq_len(ns), drop = FALSE]
colnames(lm2) <- paste0("PC", seq_len(ns))
ll  <- as.data.frame(lm2) |>
  rownames_to_column("Variable") |>
  pivot_longer(-Variable, names_to = "PC", values_to = "Loading")
pfn      <- scales::col_numeric(palette = c(HEAT_NEG, HEAT_NEU, HEAT_POS), domain = c(-1,1))
ll$fondo <- pfn(ll$Loading)
ll$txt   <- sapply(ll$fondo, auto_txt)

p_ld <- ggplot(ll, aes(x = PC, y = Variable, fill = Loading)) +
  geom_tile(color = "white", linewidth = .7) +
  geom_text(aes(label = sprintf("%.3f", Loading), color = I(txt)),
            size = 3.2, fontface = "bold") +
  scale_fill_gradient2(low = HEAT_NEG, mid = HEAT_NEU, high = HEAT_POS,
                       midpoint = 0, limits = c(-1,1), name = "Loading",
                       guide = guide_colorbar(barwidth=.8, barheight=7)) +
  scale_color_identity() +
  labs(title    = "PCA – Matriz de Loadings",
       subtitle = "|loading|>0.3 \u2192 variable relevante en ese componente",
       x = "Componente Principal", y = NULL) +
  theme(axis.text.x  = element_text(face = "bold", size = 10),
        axis.text.y  = element_text(size = 10),
        panel.grid   = element_blank(),
        plot.background = element_rect(fill = "white", color = NA))
reg(p_ld, "pca_loadings.png")

# ── Biplot: UN SOLO COLOR (CAMBIO 5) ──────────────────────────────────────────
# CAMBIO 5b: Se eliminó el gradiente de color por ROA en los puntos.
# La segmentación por colores del ROA no permitía extraer conclusiones sólidas
# porque la proyección en solo dos componentes no separa bien los niveles de ROA.
# Con un solo color uniforme (teal) el biplot muestra claramente:
#   • La estructura de las empresas en el espacio de PCA (nube de puntos)
#   • La dirección e importancia de cada variable (flechas)
# sin el ruido visual de un gradiente que no era interpretable.
# ─────────────────────────────────────────────────────────────────────────────
sc   <- as.data.frame(pca$x[, 1:2]); colnames(sc) <- c("PC1","PC2")
sf   <- 3.5
adf  <- data.frame(
  Variable = fp, x0 = 0, y0 = 0,
  x1 = pca$rotation[fp, 1] * sf,
  y1 = pca$rotation[fp, 2] * sf
)

p_bp <- ggplot(sc, aes(x = PC1, y = PC2)) +
  # UN SOLO COLOR para todos los puntos
  geom_point(color = PAL$teal, alpha = .35, size = 0.9) +
  geom_segment(
    data = adf, aes(x = x0, y = y0, xend = x1, yend = y1),
    inherit.aes = FALSE,
    arrow  = arrow(length = unit(.28,"cm"), type = "closed"),
    color  = PAL$dark, linewidth = 1.3
  ) +
  geom_label_repel(
    data = adf, aes(x = x1 * 1.18, y = y1 * 1.18, label = Variable),
    inherit.aes = FALSE,
    size = 3.5, fontface = "bold",
    fill = "white", color = PAL$dark,
    box.padding = .25, max.overlaps = 20
  ) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = .8) +
  geom_vline(xintercept = 0, color = "gray50", linewidth = .8) +
  labs(
    title    = "PCA – Biplot (PC1 vs PC2)",
    subtitle = paste(
      sprintf("PC1=%.1f%% var | PC2=%.1f%% var | Acumulado=%.1f%%",
              ve[1]*100, ve[2]*100, (ve[1]+ve[2])*100),
      "Puntos = empresas | Flechas = variables originales",
      "Flechas paralelas \u2192 correlaci\u00f3n positiva | Opuestas \u2192 negativa",
      sep = "\n"
    ),
    x = sprintf("PC1 (%.1f%% varianza)", ve[1]*100),
    y = sprintf("PC2 (%.1f%% varianza)", ve[2]*100)
  )
reg(p_bp, "pca_biplot.png")
cat("  PCA completado\n")
readline("\n  [Enter] → Sección 6: K-Means\n")


# ==============================================================================
# SECCIÓN 6 ─ K-MEANS (sin Silhouette)
# ==============================================================================
# CAMBIO 5a: Se elimina completamente el Silhouette Score.
# K óptimo se determina únicamente por el Método del Codo (inercia).
# ─────────────────────────────────────────────────────────────────────────────
cat("\n[6/7] K-Means...\n")

nkm <- min(4, ncol(pca$x))
Xkm <- pca$x[, seq_len(nkm)]

Kr  <- 2:9
ine <- numeric(length(Kr))

for (i in seq_along(Kr)) {
  kk     <- kmeans(Xkm, centers = Kr[i], nstart = 25, iter.max = 300)
  ine[i] <- kk$tot.withinss
}

# Seleccionar K en el "codo": mayor caída de inercia de un K al siguiente
dife   <- diff(ine)                           # cambio de inercia al añadir 1 cluster
ok     <- Kr[which.min(dife) + 1]             # K donde la caída frena más

cat(sprintf("  K óptimo (codo): K=%d\n", ok))

# Gráfico del codo (solo inercia)
dfe <- data.frame(k = Kr, ine = ine)

p_elbow <- ggplot(dfe, aes(x = k, y = ine)) +
  geom_line(color = PAL$teal, linewidth = 2.2) +
  geom_point(size = 3.5, color = PAL$teal) +
  geom_vline(xintercept = ok, color = PAL$red, linewidth = 2, linetype = "dashed") +
  annotate("label",
           x = ok + 0.45, y = max(ine) * 0.90,
           label = sprintf("K=%d\n(codo)", ok),
           color = PAL$red, fill = "white", size = 3.2, fontface = "bold") +
  scale_x_continuous(breaks = Kr) +
  labs(
    title    = "Método del Codo – Selección de K",
    subtitle = "K óptimo: punto donde la inercia frena su caída más bruscamente",
    x = "K (número de clusters)", y = "Inercia (WCSS)"
  )
reg(p_elbow, "kmeans_codo.png")

# K-Means final
km  <- kmeans(Xkm, centers = ok, nstart = 30, iter.max = 500)
lkm <- km$cluster
cc  <- c(PAL$teal, PAL$orange, PAL$navy, PAL$purple,
         PAL$green, PAL$red, PAL$teal2, PAL$pink)[seq_len(ok)]

# Visualización 2D PC1-PC2
dk2d <- data.frame(PC1 = Xkm[,1], PC2 = Xkm[,2], cl = factor(lkm))
c2d  <- as.data.frame(km$centers[, 1:2])
colnames(c2d) <- c("PC1","PC2")
c2d$cl <- factor(seq_len(ok))
c2d$lb <- paste0("C", seq_len(ok))

p_k2 <- ggplot(dk2d, aes(x = PC1, y = PC2, color = cl)) +
  geom_point(alpha = .35, size = 1.3) +
  geom_point(data = c2d, aes(x = PC1, y = PC2),
             inherit.aes = FALSE, shape = 8, size = 9, stroke = 2.5, color = "black") +
  geom_label(data = c2d, aes(x = PC1, y = PC2 + .35, label = lb),
             inherit.aes = FALSE, fontface = "bold", size = 4, fill = "white") +
  scale_color_manual(values = cc, name = "Cluster") +
  geom_hline(yintercept = 0, color = "gray55", linewidth = .7) +
  geom_vline(xintercept = 0, color = "gray55", linewidth = .7) +
  labs(
    title    = sprintf("K-Means (K=%d) – Espacio PC1–PC2", ok),
    subtitle = sprintf("Distancia Euclidiana | Datos estandarizados + PCA"),
    x = sprintf("PC1 (%.1f%% var)", ve[1]*100),
    y = sprintf("PC2 (%.1f%% var)", ve[2]*100)
  ) +
  theme(legend.position = "bottom")
reg(p_k2, "kmeans_2d.png")

# ── Perfil financiero con muescas y color ganancia/pérdida (CAMBIO 6) ─────────
# CAMBIO 6: notch=TRUE en boxplots del perfil + color por signo de mediana.
# Verde = mediana ≥ 0 (ese cluster gana en esa variable).
# Rojo  = mediana < 0 (ese cluster pierde en esa variable).
# ─────────────────────────────────────────────────────────────────────────────
pvx  <- c("ROA","CFO","LEV","SIZE","GRW"); pvx <- pvx[pvx %in% names(df)]
dfpr <- df[rownames(Xpr), pvx]; dfpr$cluster <- factor(lkm)

cat("\n  Perfil por Cluster:\n")
cat(sprintf("  %-8s  %10s  %10s  %10s  %8s\n", "Cluster","ROA","CFO","LEV","n"))
for (cl in levels(dfpr$cluster)) {
  s <- dfpr[dfpr$cluster == cl, ]
  cat(sprintf("  C%-7s  %+10.4f  %+10.4f  %10.4f  %8d\n",
              cl, mean(s$ROA,na.rm=TRUE), mean(s$CFO,na.rm=TRUE),
              mean(s$LEV,na.rm=TRUE), nrow(s)))
}

dfpl <- dfpr |>
  pivot_longer(cols = all_of(pvx), names_to = "Variable", values_to = "Valor")

# Calcular signo de la mediana por cluster × variable
mediana_sign <- dfpl |>
  group_by(cluster, Variable) |>
  summarise(mediana = median(Valor, na.rm = TRUE), .groups = "drop") |>
  mutate(signo = ifelse(mediana >= 0,
                        "Ganancia  (mediana \u2265 0)",
                        "P\u00e9rdida  (mediana < 0)"))

dfpl2 <- dfpl |>
  left_join(mediana_sign |> select(cluster, Variable, signo),
            by = c("cluster","Variable"))

p_pf <- ggplot(dfpl2, aes(x = cluster, y = Valor, fill = signo)) +
  geom_boxplot(
    notch         = TRUE,     # ← IC 95% de la mediana
    notchwidth    = 0.25,     # ← cintura muy visible
    color         = "gray20",
    linewidth     = 0.75,
    alpha         = 0.80,
    outlier.alpha = 0.15,
    outlier.size  = 0.7
  ) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1.0, linetype = "dashed") +
  scale_fill_manual(
    values = c("Ganancia  (mediana \u2265 0)" = PAL$green,
               "P\u00e9rdida  (mediana < 0)"  = PAL$red),
    name   = NULL
  ) +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(
    title    = sprintf("Perfil Financiero por Cluster (K=%d)", ok),
    subtitle = paste(
      "VERDE = mediana positiva (cluster con ganancia en esa variable)",
      "ROJO  = mediana negativa (cluster con p\u00e9rdida en esa variable)",
      "Cu\u00f1as = IC 95% de la mediana | Si no se solapan \u2192 medianas distintas al 95%",
      sep = "\n"
    ),
    x = "Cluster", y = "Valor"
  ) +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"),
        legend.text     = element_text(size = 10, face = "bold"))
reg(p_pf, "kmeans_perfiles.png")
cat("  K-Means completado\n")
readline("\n  [Enter] → Sección 7: Treemaps ROA\n")


# ==============================================================================
# SECCIÓN 7 ─ TREEMAPS ANUALES ROA (2018–2024)
# ==============================================================================
cat("\n[7/7] Treemaps anuales ROA (2018–2024)...\n")

COL_GANANCIA   <- "#059669"
COL_PERDIDA    <- "#EF4444"
COL_EQUILIBRIO <- "#94A3B8"

df_tree_base <- df |>
  filter(year %in% 2018:2024,
         !is.na(ROA), !is.na(SIZE), is.finite(ROA), is.finite(SIZE)) |>
  group_by(stock, year) |>
  summarise(ROA     = mean(ROA,  na.rm = TRUE),
            activos = mean(exp(SIZE), na.rm = TRUE),
            .groups = "drop") |>
  mutate(
    signo = case_when(
      ROA >  0.001 ~ "Ganancia  (ROA > 0)",
      ROA < -0.001 ~ "Perdida  (ROA < 0)",
      TRUE         ~ "Equilibrio  (ROA \u2248 0)"
    ),
    stock = as.character(stock)
  )

hacer_treemap_anio <- function(anio) {
  df_anio <- df_tree_base |> filter(year == anio)
  if (nrow(df_anio) < 3) { message(sprintf("  Año %d: omitido.", anio)); return(NULL) }
  
  n_gan   <- sum(df_anio$signo == "Ganancia  (ROA > 0)")
  n_tot   <- nrow(df_anio)
  pct_gan <- round(n_gan / n_tot * 100, 1)
  roa_med <- round(median(df_anio$ROA, na.rm = TRUE) * 100, 3)
  
  tm_obj <- suppressMessages(
    treemap(df_anio, index="stock", vSize="activos",
            type="categorical", vColor="signo",
            algorithm="pivotSize", sortID="ROA",
            mirror.y=TRUE, mirror.x=TRUE,
            border.lwds=0.5, aspRatio=5/3, draw=FALSE)
  )
  
  dg <- tm_obj[["tm"]] |>
    as_tibble() |>
    arrange(desc(vSize)) |>
    mutate(rank = row_number(), xmax = x0+w, ymax = y0+h,
           stock = as.character(stock)) |>
    left_join(df_anio |> select(stock, ROA, signo), by = "stock") |>
    mutate(label_vis = sprintf("%s\n%.2f%%", stock, ROA * 100))
  
  nota <- tibble(
    label = c("**Cómo leer:**",
              "Cada rectángulo = una empresa",
              "**Tamaño** = activos totales",
              paste0("<span style='color:", COL_GANANCIA, "'>**VERDE**</span>",
                     " = ROA > 0  |  ",
                     "<span style='color:", COL_PERDIDA,  "'>**ROJO**</span>",
                     " = ROA < 0")),
    x = c(0.5,0.5,0.5,0.5), y = c(-0.065,-0.105,-0.145,-0.190)
  )
  
  p <- ggplot(dg) +
    geom_rect(aes(xmin=x0, ymin=y0, xmax=xmax, ymax=ymax, fill=signo),
              linewidth=0.15, colour="#1E1D23", alpha=0.90) +
    geom_fit_text(data = dg |> filter(rank <= 80),
                  aes(xmin=x0, xmax=xmax, ymin=y0, ymax=ymax, label=label_vis),
                  colour="white", size=3, reflow=TRUE, min.size=2) +
    geom_richtext(data=nota, aes(x=x, y=y, label=label),
                  size=3.2, color="#E8EADC", fill=NA, label.color=NA, hjust=0.5) +
    scale_fill_manual(
      values   = c("Ganancia  (ROA > 0)"     = COL_GANANCIA,
                   "Perdida  (ROA < 0)"       = COL_PERDIDA,
                   "Equilibrio  (ROA \u2248 0)" = COL_EQUILIBRIO),
      na.value = COL_EQUILIBRIO
    ) +
    labs(
      title   = sprintf("Rentabilidad (ROA) – Mercado Coreano %d", anio),
      subtitle = sprintf("Ganancia: %d/%d (%.1f%%)  |  Mediana ROA: %.3f%%  |  Tamaño = activos",
                         n_gan, n_tot, pct_gan, roa_med),
      caption  = sprintf("KoTaP Dataset \u2022 %d \u2022 n=%d empresas", anio, n_tot)
    ) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill="#1E1D23", colour="#1E1D23"),
      plot.margin     = margin(28,12,50,12),
      plot.title      = element_text(size=18, hjust=0.5, face="bold",
                                     colour="#E8EADC", margin=margin(b=6)),
      plot.subtitle   = element_text(size=9.5, hjust=0.5, colour="#94A3B8"),
      plot.caption    = element_text(size=7.5, hjust=0.5, colour="#64748B")
    )
  
  cat(sprintf("  %d → %d empresas | Gan=%.1f%% | Med.ROA=%.3f%%\n",
              anio, n_tot, pct_gan, roa_med))
  return(p)
}

for (anio in 2018:2024) {
  p_tm <- hacer_treemap_anio(anio)
  if (!is.null(p_tm)) reg(p_tm, sprintf("treemap_roa_%d.png", anio))
}

# Panel comparativo
plots_panel <- lapply(2018:2024, function(anio) {
  df_a <- df_tree_base |> filter(year == anio)
  if (nrow(df_a) < 3) return(NULL)
  n_tot   <- nrow(df_a)
  pct_gan <- round(sum(df_a$signo == "Ganancia  (ROA > 0)") / n_tot * 100, 1)
  tm_obj  <- suppressMessages(
    treemap(df_a, index="stock", vSize="activos", type="categorical", vColor="signo",
            algorithm="pivotSize", sortID="ROA", mirror.y=TRUE, mirror.x=TRUE,
            border.lwds=0.3, aspRatio=5/3, draw=FALSE)
  )
  dg <- tm_obj[["tm"]] |> as_tibble() |>
    mutate(xmax=x0+w, ymax=y0+h, stock=as.character(stock)) |>
    left_join(df_a |> select(stock, signo), by="stock")
  
  ggplot(dg) +
    geom_rect(aes(xmin=x0,ymin=y0,xmax=xmax,ymax=ymax,fill=signo),
              linewidth=0.08, colour="#1E1D23", alpha=0.90) +
    scale_fill_manual(
      values   = c("Ganancia  (ROA > 0)"     = COL_GANANCIA,
                   "Perdida  (ROA < 0)"       = COL_PERDIDA,
                   "Equilibrio  (ROA \u2248 0)" = COL_EQUILIBRIO),
      na.value = COL_EQUILIBRIO
    ) +
    labs(title    = as.character(anio),
         subtitle = sprintf("Verde=%.1f%%  n=%d", pct_gan, n_tot)) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill="#1E1D23", colour="#2D3748"),
      plot.title      = element_text(size=13, hjust=0.5, face="bold",
                                     colour="#E8EADC", margin=margin(b=2)),
      plot.subtitle   = element_text(size=8, hjust=0.5, colour="#94A3B8"),
      plot.margin     = margin(8,6,8,6)
    )
}) |> Filter(Negate(is.null), x=_)

if (length(plots_panel) > 0) {
  p_panel_tm <- wrap_plots(plots_panel, ncol=4) +
    plot_annotation(
      title    = "Evolución de la Rentabilidad (ROA) – Mercado Coreano 2018–2024",
      subtitle = "VERDE = ROA > 0 (ganancia)  |  ROJO = ROA < 0 (pérdida)  |  Tamaño = activos totales",
      caption  = "KoTaP Dataset \u2022 2018–2024",
      theme    = theme(
        plot.background = element_rect(fill="#1E1D23", colour="#1E1D23"),
        plot.title      = element_text(size=16, hjust=0.5, face="bold",
                                       colour="#E8EADC", margin=margin(b=6)),
        plot.subtitle   = element_text(size=9.5, hjust=0.5,
                                       colour="#94A3B8", margin=margin(b=4)),
        plot.caption    = element_text(size=7.5, hjust=0.5, colour="#64748B"),
        plot.margin     = margin(18,10,14,10)
      )
    )
  reg(p_panel_tm, "treemap_roa_panel_2018_2024.png")
}
cat("  Treemaps completados\n")
readline("\n  [Enter] → Sección 8: Guardar gráficos\n")


# ==============================================================================
# SECCIÓN 8 ─ GUARDADO FINAL
# ==============================================================================
cat("\n[8/8] Guardando gráficos...\n")

script_dir <- tryCatch({
  if (requireNamespace("rstudioapi", quietly=TRUE) && rstudioapi::isAvailable()) {
    ruta <- rstudioapi::getSourceEditorContext()$path
    if (nchar(ruta) > 0) dirname(ruta) else stop("no guardado")
  } else stop("sin rstudioapi")
}, error = function(e) {
  args <- commandArgs(trailingOnly=FALSE)
  fa   <- grep("--file=", args, value=TRUE)
  if (length(fa) > 0) dirname(normalizePath(sub("--file=","",fa)))
  else { message("  Usando: ", getwd()); getwd() }
})

cat(sprintf("  Destino: %s\n", script_dir))

ANCHO_STD <- 2800; ALTO_STD <- 1600; RES_STD <- 200
ANCHO_TM  <- 3200; ALTO_TM  <- 2000; RES_TM  <- 180
ANCHO_PNL <- 4800; ALTO_PNL <- 2600; RES_PNL <- 180

n_ok <- 0; n_err <- 0

for (nombre in names(REGISTRO)) {
  ruta        <- file.path(script_dir, nombre)
  p           <- REGISTRO[[nombre]]
  es_panel_tm <- grepl("panel_2018_2024", nombre)
  es_tm       <- grepl("^treemap_roa_\\d{4}", nombre)
  
  if (es_panel_tm) { ancho <- ANCHO_PNL; alto <- ALTO_PNL; res <- RES_PNL
  } else if (es_tm) { ancho <- ANCHO_TM; alto <- ALTO_TM;  res <- RES_TM
  } else             { ancho <- ANCHO_STD; alto <- ALTO_STD; res <- RES_STD }
  
  tryCatch({
    if (inherits(p, "gg") || inherits(p, "patchwork"))
      ggsave(ruta, plot=p, width=ancho/res, height=alto/res,
             dpi=res, bg="white", limitsize=FALSE)
    else if (inherits(p, "recordedplot")) {
      png(ruta, width=ancho, height=alto, res=res, bg="white")
      replayPlot(p); dev.off()
    }
    cat(sprintf("  [OK]  %s\n", nombre))
    n_ok <- n_ok + 1
  }, error = function(e) {
    cat(sprintf("  [ERR] %s — %s\n", nombre, e$message))
    n_err <<- n_err + 1
  })
}

cat(sprintf("\n  %s\n", strrep("=", 60)))
cat(sprintf("  Guardados: %d / %d  |  Errores: %d\n",
            n_ok, length(REGISTRO), n_err))
cat(sprintf("  Destino  : %s\n", script_dir))
cat(sprintf("  %s\n", strrep("=", 60)))
cat("  PIPELINE v6.0 COMPLETADO\n")

















# Seccion 2 


# Quitar raya negra 
# Poner una leyenda donde indique se significa cada color 
# Revisar la winsorizacion, contemplar la opcion de quitar esos datos
# Revisar analisis univariado al ROA y al ROE, estan raras esas barras con una barra que sea de dos colores al tiempo, es decir que la mitad de la barra sea roja y la otra verde


# Seccion Matrices covarianzas spearman y eso

# Quitar matriz de correlaciones 
# Quitar a Pearson



# En la de regresión queda redundante el segundo grafico
# Esa winsorizacion esta afectando el modelo, preferiblemente eliminar esos outliers 
# Esa grafica de variables con mas roa SOBRA 


# Olvidarse del slhoutte score 

# En el biplot dejar de un color los puntos, no es posible dar una conclusión medianamente solida

# Anotar las muesquitas IC 95% de los boxplots del final 



# Orden: para expo 
# el EDA, PCA, Kluster, Boxplot,  Modelos


# Dejar un modelo, profundizar en el analisis multivariado 
