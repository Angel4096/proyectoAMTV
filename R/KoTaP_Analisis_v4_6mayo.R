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

# Función heatmap (Triángulo inferior anclado, sin espacios flotantes)
hmap <- function(mat, titulo, subtitulo = "", fmt = "%.2f", lim = NULL) {
  # 1. Convertir a formato largo
  ml <- as.data.frame(as.table(mat)) |> rename(X = Var1, Y = Var2, V = Freq)
  
  # 2. Filtrar solo el triángulo inferior (sin la diagonal redundante de 1.00)
  ml <- ml |>
    mutate(xi = as.integer(X), yi = as.integer(Y)) |>
    filter(yi > xi) |>
    select(-xi, -yi)
  
  # 3. Limpiar los ejes (el truco para que no "flote")
  # Al quitar el triángulo superior y la diagonal, la última variable de X 
  # y la primera variable de Y quedan totalmente vacías. Hay que borrarlas del gráfico.
  niveles <- colnames(mat)
  ml$X <- factor(ml$X, levels = niveles[-length(niveles)]) # Quitar la última de X
  ml$Y <- factor(ml$Y, levels = rev(niveles[-1]))          # Quitar la primera de Y e invertir
  
  rng <- if (!is.null(lim)) lim else range(ml$V, na.rm = TRUE)
  pfn <- scales::col_numeric(
    palette = c(HEAT_NEG, HEAT_NEU, HEAT_POS),
    domain  = rng, na.color = "grey90"
  )
  ml$fondo <- pfn(ml$V)
  ml$txt   <- sapply(ml$fondo, auto_txt)
  ml$lbl   <- sprintf(fmt, ml$V)
  
  ggplot(ml, aes(x = X, y = Y, fill = V)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = lbl, color = I(txt)), size = 3.2, fontface = "bold") +
    scale_fill_gradient2(
      low = HEAT_NEG, mid = HEAT_NEU, high = HEAT_POS,
      midpoint = 0, limits = rng, name = NULL,
      guide = guide_colorbar(barwidth = 1.2, barheight = 12,
                             label.theme = element_text(size = 9, color = PAL$dark))
    ) +
    coord_fixed() +                  # <--- Fuerza a que sean cuadrados perfectos
    labs(title = titulo, subtitle = subtitulo, x = NULL, y = NULL) +
    theme_minimal() +                # <--- Fondo limpio sin líneas grises
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 10, color = PAL$dark, face = "bold"),
      axis.text.y     = element_text(size = 10, color = PAL$dark, face = "bold"),
      panel.grid      = element_blank(), # <--- Quita la cuadrícula de fondo
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right"
    )
}

p_spearman <- hmap(
  corr_s,
  titulo    = "¿Cómo se asocian las variables por tendencia? (Spearman)",
  subtitulo = paste(
    "Rojo oscuro = asociación positiva fuerte | Azul oscuro = asociación negativa fuerte | Blanco = sin tendencia",
    "Robusto a outliers \u2014 no supone linealidad \u2014 trabaja con rangos",
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
# SECCIÓN 4 ─ REGRESIÓN LINEAL MÚLTIPLE + VISUALIZACIÓN + RESIDUOS
# ==============================================================================
# Modelo:
#   ROA = β0 + β1·SIZE + β2·LEV + β3·CFO + β4·GRW + β5·CUR
#             + β6·INVREC + β7·PPE + β8·AGE + ε
#
#   βi = cambio en ROA por +1 unidad de la variable i,
#        manteniendo TODAS las demás constantes (ceteris paribus).
#
# Valores reales obtenidos del modelo (referencia con los datos KoTaP):
#   CFO    ≈ +0.30+   (principal driver, β* ≈ 0.458)
#   GRW    ≈ +0.0503  ***
#   LEV    ≈ -0.0505  ***
#   INVREC ≈ +0.0400  ***
#   AGE    ≈ -0.0080  ***
#   PPE    ≈ -0.0222  ***
#   CUR    ≈ -0.0006  *    (significativo pero de magnitud pequeña)
#   SIZE   ≈  0.0000  ns
# ==============================================================================
cat("\n[4/7] Regresión Lineal Múltiple...\n")

f_ols <- as.formula(paste(TARGET, "~", paste(FEATURES, collapse = "+")))
mod   <- lm(f_ols, data = train_df)

yp   <- predict(mod, newdata = test_df)
res  <- residuals(mod)
fv   <- fitted(mod)
r2   <- 1 - sum((y_test - yp)^2) / sum((y_test - mean(y_test))^2)
rmse <- sqrt(mean((y_test - yp)^2))
mae  <- mean(abs(y_test - yp))
r2a  <- summary(mod)$adj.r.squared

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


# ==============================================================================
# GRÁFICO 1 ─ PANEL DE REGRESIÓN
# ==============================================================================
# Diseño (dos filas):
#   ┌──────────────────────────────────────────────────────────────┐
#   │  ARRIBA (altura 1.4x):  Predichos vs Reales + métricas       │
#   ├──────────────────────────────────────────────────────────────┤
#   │  ABAJO  (altura 1x):    Coeficientes β con IC 95%           │
#   └──────────────────────────────────────────────────────────────┘
# Se eliminó el panel de β estandarizados (redundante con el de β brutos
# cuando ambos están en el mismo panel y la escala ya comunica magnitud).
# ==============================================================================

# ── Panel superior: Predichos vs Reales ───────────────────────────────────────
dfpv  <- data.frame(obs = y_test, pred = yp, ep = (yp - y_test) > 0)

p_pred <- ggplot(dfpv, aes(x = obs, y = pred, color = ep)) +
  geom_point(alpha = .40, size = 1.8) +
  geom_abline(slope = 1, intercept = 0,
              color = PAL$red, linewidth = 2, linetype = "dashed") +
  # Banda ±RMSE alrededor de la línea perfecta
  geom_ribbon(data = dfpv,
              aes(x = obs, ymin = obs - rmse, ymax = obs + rmse),
              fill = PAL$teal, alpha = .07, inherit.aes = FALSE) +
  scale_color_manual(
    values = c("TRUE"  = PAL$orange,
               "FALSE" = PAL$teal),
    labels = c("TRUE"  = "Sobreestimado (pred > real)",
               "FALSE" = "Subestimado (pred < real)"),
    name   = NULL
  ) +
  # Métricas anotadas en esquina superior izquierda
  annotate("label",
           x     = min(y_test, na.rm = TRUE),
           y     = max(yp,     na.rm = TRUE),
           label = sprintf(
             "R\u00b2       = %.4f\nR\u00b2-adj  = %.4f\nRMSE    = %.4f\nMAE     = %.4f",
             r2, r2a, rmse, mae),
           hjust = 0, vjust = 1, size = 3.6,
           fill  = "#EFF6FF", color = PAL$dark, family = "mono") +
  labs(
    title    = "Regresión Lineal Múltiple \u2013 ROA predicho vs ROA observado",
    subtitle = sprintf(
      "R\u00b2 = %.4f  |  R\u00b2-adj = %.4f  |  RMSE = %.4f  |  MAE = %.4f  |  Banda = \u00b1RMSE",
      r2, r2a, rmse, mae),
    x = "ROA Observado",
    y = "ROA Predicho"
  ) +
  theme(legend.position = "bottom")


# ── Panel inferior: Coeficientes β con IC 95% ─────────────────────────────────
# Orden: de mayor a menor |β|  →  variables más influyentes arriba.
# La barra = IC al 95%: si cruza el cero, el efecto no es
#   estadísticamente distinguible de cero (variable no significativa).
cdf <- data.frame(
  Variable = rownames(ctbl)[-1],
  B        = ctbl[-1, 1],
  SE       = ctbl[-1, 2],
  p        = ctbl[-1, 4]
) |>
  mutate(
    sig   = ifelse(p < .05, "Significativo (p<0.05)", "No significativo"),
    lo    = B - 1.96 * SE,         # límite inferior IC 95%
    hi    = B + 1.96 * SE,         # límite superior IC 95%
    # Etiqueta: valor del coeficiente + estrellas de significancia
    label = sprintf("%.4f%s", B,
                    ifelse(p < .001, "***",
                           ifelse(p < .01, "**",
                                  ifelse(p < .05, "*", ""))))
  ) |>
  arrange(desc(abs(B))) |>
  mutate(Variable = factor(Variable, levels = Variable))

p_betas <- ggplot(cdf, aes(x = B, y = Variable, color = sig)) +
  # Línea en 0: derecha = efecto positivo en ROA, izquierda = negativo
  geom_vline(xintercept = 0, color = "gray35",
             linewidth = 1.3, linetype = "dashed") +
  # Barra semitransparente del IC 95%
  geom_segment(aes(x = lo, xend = hi, yend = Variable),
               linewidth = 2.5, alpha = .30) +
  # Punto del coeficiente estimado
  geom_point(size = 5) +
  # Etiqueta con valor numérico y estrellas
  geom_text(aes(label = label),
            hjust = -.15, size = 3.3,
            color = PAL$dark, fontface = "bold") +
  scale_color_manual(
    values = c("Significativo (p<0.05)" = PAL$teal,
               "No significativo"        = PAL$slate),
    name   = NULL
  ) +
  labs(
    title    = "Coeficientes \u03b2 (ordenados por |valor|)",
    subtitle = paste(
      "Barras = IC 95%  |  Derecha de 0 = efecto positivo en ROA",
      "*** p<0.001   ** p<0.01   * p<0.05",
      sep = "\n"
    ),
    x = "Valor del coeficiente \u03b2",
    y = NULL
  ) +
  theme(legend.position = "bottom")


# ── Ensamblaje del panel de regresión (solo 2 filas) ──────────────────────────
p_regresion <- (
  p_pred /       # fila superior
    p_betas        # fila inferior
) +
  plot_layout(heights = c(1.4, 1)) +
  plot_annotation(
    title    = "Regresi\u00f3n Lineal M\u00faltiple \u2013 Panel Completo",
    subtitle = sprintf(
      "R\u00b2 = %.4f  |  R\u00b2-adj = %.4f  |  F = %.2f (p < 0.001)  |  n-train = %d  |  n-test = %d",
      r2, r2a, fs[1], nrow(train_df), nrow(test_df)),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 15, color = PAL$dark),
      plot.subtitle = element_text(size = 9.5,  color = PAL$slate)
    )
  )

reg(p_regresion, "ols_regresion.png")


# ==============================================================================
# GRÁFICO 2 ─ DIAGNÓSTICO DE RESIDUOS Y VERIFICACIÓN DE SUPUESTOS
# ==============================================================================
# Los residuos εi = ROA_real - ROA_predicho condensan todo lo que el modelo
# NO explicó. Tres gráficos verifican supuestos distintos:
#
# A) Histograma + curva normal
#    → Verifica NORMALIDAD de los errores.
#    → Las barras deben seguir la campana roja N(0,σ²).
#    → Colas pesadas = leptocúrtica (frecuente en finanzas).
#    → Test Jarque-Bera: H0 = normalidad. Con n>5000 el rechazo es
#      esperado pero no invalida el modelo (TCL garantiza validez).
#
# B) QQ-Plot
#    → Verifica NORMALIDAD con más detalle en las colas.
#    → Puntos sobre la línea roja = normalidad.
#    → Desviación en las esquinas = colas más pesadas de lo esperado.
#    → Coeficiente r: cercano a 1.00 = muy normal; r<0.97 = notable.
#
# C) Residuos vs Valores Ajustados + curva LOWESS
#    → Verifica LINEALIDAD y HOMOCEDASTICIDAD (los más importantes).
#    → Curva naranja (LOWESS) debe ser PLANA en 0.
#      Si se curva → no-linealidad no capturada.
#      Si la nube se ensancha → heterocedasticidad.
#    → Bandas ±2σ: ~95% de los residuos deben caer dentro.
# ==============================================================================

rf  <- res[is.finite(res)]
jbt <- jarque.bera.test(rf)
jbp <- jbt$p.value

# ── A: Histograma de residuos ─────────────────────────────────────────────────
qr  <- qqnorm(rf, plot.it = FALSE)
dfq <- data.frame(t = qr$x, m = qr$y)
rqq <- round(cor(dfq$t, dfq$m), 4)
xn  <- seq(min(rf), max(rf), length.out = 300)
dfn <- data.frame(x = xn, y = dnorm(xn, mean(rf), sd(rf)))

ph_r <- ggplot(data.frame(r = rf), aes(x = r)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 70, fill = PAL$teal, alpha = .75, color = "white") +
  geom_line(data = dfn, aes(x = x, y = y),
            color = PAL$red, linewidth = 2.2) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.4) +
  annotate("label",
           x     = Inf, y = Inf,
           label = sprintf(
             "Jarque-Bera\np = %.3e\n%s\n\nAsimetria = %.3f\nCurtosis  = %.3f",
             jbp,
             ifelse(jbp > .05, "OK: Normal aprox.", "Colas pesadas\n(valid. con n>5000)"),
             e1071::skewness(rf),
             e1071::kurtosis(rf)),
           hjust = 1.05, vjust = 1.1, size = 2.9,
           fill = "#FFF8E7", color = PAL$dark, family = "mono") +
  labs(
    title    = "A) Distribución de Residuos",
    subtitle = "Barras deben seguir la campana roja  |  Colas pesadas = leptocúrtica (frecuente en finanzas)",
    x = "Residuo  (ROA real \u2212 ROA predicho)",
    y = "Densidad"
  )


# ── B: QQ-Plot ────────────────────────────────────────────────────────────────
pq_r <- ggplot(dfq, aes(x = t, y = m)) +
  geom_point(color = PAL$teal, alpha = .4, size = 1) +
  geom_abline(slope     = sd(rf),
              intercept = mean(rf),
              color     = PAL$red, linewidth = 2) +
  annotate("label",
           x     = -Inf, y = Inf,
           label = sprintf("r = %.4f\n%s", rqq,
                           ifelse(rqq > .99, "Muy normal",
                                  ifelse(rqq > .97, "Aprox. normal",
                                         "Colas pesadas"))),
           hjust = -.05, vjust = 1.2, size = 3.2,
           fill = "#FFF3E0", color = PAL$dark) +
  labs(
    title    = "B) QQ-Plot de Residuos",
    subtitle = "Puntos sobre la línea = normalidad  |  Extremos alejados = colas pesadas",
    x = "Cuantiles teóricos N(0,1)",
    y = "Cuantiles muestrales de residuos"
  )


# ── C: Residuos vs Valores Ajustados ─────────────────────────────────────────
dfh <- data.frame(ft = fv, r = res) |>
  filter(is.finite(ft) & is.finite(r))
smh <- as.data.frame(lowess(dfh$ft, dfh$r, f = .3))
sd2 <- sd(rf)

prf <- ggplot(dfh, aes(x = ft, y = r)) +
  # Bandas ±2σ: 95% de los residuos deberían caer dentro
  geom_hline(yintercept =  2 * sd2,
             color = PAL$slate, linewidth = .9, linetype = "dotted") +
  geom_hline(yintercept = -2 * sd2,
             color = PAL$slate, linewidth = .9, linetype = "dotted") +
  geom_point(color = PAL$navy, alpha = .20, size = .9) +
  geom_hline(yintercept = 0,
             color = PAL$red, linewidth = 1.5, linetype = "dashed") +
  # Curva LOWESS: debe ser plana en 0
  geom_line(data = smh, aes(x = x, y = y),
            color = PAL$orange, linewidth = 2.2) +
  annotate("text",
           x     = max(dfh$ft, na.rm = TRUE),
           y     =  2 * sd2,
           label = "+2\u03c3", hjust = 1, vjust = -.3,
           size = 3, color = PAL$slate, fontface = "bold") +
  annotate("text",
           x     = max(dfh$ft, na.rm = TRUE),
           y     = -2 * sd2,
           label = "-2\u03c3", hjust = 1, vjust = 1.2,
           size = 3, color = PAL$slate, fontface = "bold") +
  labs(
    title    = "C) Residuos vs Valores Ajustados",
    subtitle = paste(
      "LOWESS naranja: PLANA en 0 = linealidad + homocedasticidad",
      "Curva \u2192 no-linealidad  |  Embudo \u2192 heterocedasticidad",
      "Bandas punteadas = \u00b12\u03c3  (\u223c95% de residuos deben caer dentro)",
      sep = "\n"
    ),
    x = "Valores ajustados (ROA predicho \u2013 entrenamiento)",
    y = "Residuos"
  )


# ── Panel de residuos ensamblado (tres gráficos en una fila) ──────────────────
p_residuos <- (ph_r | pq_r | prf) +
  plot_annotation(
    title    = "Diagn\u00f3stico de Residuos \u2013 Verificaci\u00f3n de Supuestos OLS",
    subtitle = sprintf(
      "JB p = %.3e  (%s)  |  QQ r = %.4f  |  n residuos = %d",
      jbp,
      ifelse(jbp > .05, "Normal aprox.",
             "No-normal: colas pesadas (aceptable con n grande)"),
      rqq,
      length(rf)
    ),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 14, color = PAL$dark),
      plot.subtitle = element_text(size = 9, color = PAL$slate)
    )
  )

reg(p_residuos, "ols_residuos.png")
cat("  Regresión completada\n")
readline("\n  [Enter] \u2192 Sección 5: PCA\n")
# ==============================================================================
# SECCIÓN 5 ─ ANÁLISIS DE COMPONENTES PRINCIPALES (PCA)
# ==============================================================================
# OBJETIVO: Reducir la dimensionalidad de las 8 variables financieras (altamente
# correlacionadas) en un conjunto menor de componentes independientes, para 
# facilitar la visualización y mejorar el rendimiento del clustering (K-Means).
# ==============================================================================
cat("\n[5/7] PCA...\n")

feats_ok <- FEATURES[FEATURES %in% names(df)]
Xpr      <- df[, feats_ok] |> na.omit()

# Filtrar variables con varianza casi cero (prcomp fallaría con ellas)
vok  <- sapply(Xpr, function(x) var(x, na.rm = TRUE) > 1e-10)
Xpr  <- Xpr[, vok, drop = FALSE]
fp   <- colnames(Xpr)

# Estandarizar antes de PCA
Xsc  <- scale(Xpr)
pca  <- prcomp(Xsc, center = FALSE, scale. = FALSE)

ve  <- (pca$sdev^2) / sum(pca$sdev^2)   # varianza explicada por cada PC
vc  <- cumsum(ve)                       # varianza acumulada
eig <- pca$sdev^2                       # eigenvalores
np  <- length(ve)

# Imprimir tabla de eigenvalores en consola
cat("\n  Eigenvalores (Criterio Kaiser: retener si >= 1):\n")
cat(sprintf("  %-5s  %10s  %10s  %10s  %s\n",
            "PC","Eigenvalor","% Var","% Acum.","Decision"))
cat(sprintf("  %s\n", strrep("-", 58)))
for (i in seq_along(ve)) {
  cat(sprintf("  PC%-3d  %10.4f  %10.2f  %10.2f  %s\n",
              i, eig[i], ve[i]*100, vc[i]*100,
              ifelse(eig[i] >= 1, "RETENER", "descartar")))
}

# ==============================================================================
# GRÁFICO 1 ─ SCREE PLOT + VARIANZA ACUMULADA
# ==============================================================================
dfs <- data.frame(
  PC = factor(paste0("PC", seq_len(np)), levels = paste0("PC", seq_len(np))),
  eg = eig, vp = ve * 100, cp = vc * 100
)

# Scree plot: barras + línea Kaiser en 1
p_sc <- ggplot(dfs, aes(x = PC, y = eg)) +
  geom_col(aes(fill = eg >= 1), alpha = .85, width = .7) +
  geom_line(aes(group = 1), color = PAL$orange, linewidth = 1.8) +
  geom_point(color = PAL$orange, size = 3.5) +
  geom_hline(yintercept = 1, color = PAL$red, linewidth = 1.5, linetype = "dashed") +
  annotate("text", x = 1.5, y = 1.10,
           label = "Criterio Kaiser: retener si \u2265 1",
           color = PAL$red, size = 3.5, hjust = 0) +
  scale_fill_manual(
    values = c("TRUE"  = PAL$teal,  "FALSE" = PAL$slate),
    labels = c("TRUE"  = "Retener", "FALSE" = "Descartar"),
    name   = NULL
  ) +
  labs(
    title    = "Scree Plot",
    subtitle = "Teal = eigenvalor \u2265 1 (aporta informaci\u00f3n \u00fatil)",
    x = "Componente Principal",
    y = "Eigenvalor"
  ) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Varianza acumulada: Limpiado de líneas de umbral
p_cp <- ggplot(dfs, aes(x = PC, y = cp, group = 1)) +
  geom_area(fill = PAL$teal, alpha = .18) +
  geom_line(color = PAL$teal, linewidth = 2.2) +
  geom_point(size = 3.5, color = PAL$teal) +
  scale_y_continuous(limits = c(0, 101), breaks = seq(0, 100, 20)) +
  labs(
    title    = "Varianza Acumulada",
    subtitle = "Crecimiento del % de informaci\u00f3n total capturada",
    x = "N\u00b0 de Componentes",
    y = "% Varianza total capturada"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Unir y guardar ambos gráficos
reg(
  (p_sc | p_cp) +
    plot_annotation(
      title    = "PCA \u2013 Selecci\u00f3n del n\u00famero de Componentes",
      subtitle = sprintf("Dataset: %d empresas \u00d7 %d variables  |  Retenidos por Kaiser: %d PCs",
                         nrow(Xpr), length(fp), sum(eig >= 1)),
      theme = theme(
        plot.title    = element_text(face = "bold", size = 14, color = PAL$dark),
        plot.subtitle = element_text(size = 9, color = PAL$slate)
      )
    ),
  "pca_scree.png"
)

# ==============================================================================
# GRÁFICO 2 ─ BIPLOT: EMPRESAS + VARIABLES EN EL MISMO ESPACIO
# ==============================================================================
sc  <- as.data.frame(pca$x[, 1:2])
colnames(sc) <- c("PC1", "PC2")

# Escalar las flechas
sf  <- max(abs(sc)) * 0.65
adf <- data.frame(
  Variable = fp,
  x0 = 0, y0 = 0,
  x1 = pca$rotation[fp, 1] * sf,
  y1 = pca$rotation[fp, 2] * sf
)

p_bp <- ggplot(sc, aes(x = PC1, y = PC2)) +
  # Fondo
  annotate("rect", xmin = -Inf, xmax = 0, ymin = 0,    ymax = Inf, fill = "#F0FDF4", alpha = .25) +
  annotate("rect", xmin = 0,    xmax = Inf, ymin = -Inf, ymax = 0, fill = "#FFF7ED", alpha = .25) +
  # Puntos
  geom_point(color = PAL$teal, alpha = .30, size = 0.85) +
  # Ejes
  geom_hline(yintercept = 0, color = "gray55", linewidth = 0.8, linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray55", linewidth = 0.8, linetype = "dashed") +
  # Flechas
  geom_segment(
    data        = adf,
    aes(x = x0, y = y0, xend = x1, yend = y1),
    arrow       = arrow(length = unit(.30, "cm"), type = "closed"),
    color       = PAL$dark, linewidth = 1.4
  ) +
  # Etiquetas
  geom_label_repel(
    data          = adf,
    aes(x = x1 * 1.22, y = y1 * 1.22, label = Variable),
    size          = 3.8, fontface = "bold",
    fill          = "white", color = PAL$dark,
    box.padding   = 0.30,
    segment.color = PAL$slate
  ) +
  labs(
    title    = "PCA \u2013 Biplot: Empresas y Variables",
    subtitle = sprintf("PC1 y PC2 capturan el %.1f%% de la informaci\u00f3n total", (ve[1]+ve[2])*100),
    x = sprintf("PC1 (%.1f%% var)", ve[1]*100),
    y = sprintf("PC2 (%.1f%% var)", ve[2]*100)
  ) +
  theme(legend.position = "none")

reg(p_bp, "pca_biplot.png")
cat("  PCA completado\n")
# ==============================================================================
# SECCIÓN 6 ─ K-MEANS 
# ==============================================================================
# OBJETIVO: Agrupar las empresas utilizando los componentes principales y 
# encontrar el número óptimo de clusters (K) mediante el Método del Codo.
# ==============================================================================
cat("\n[6/7] K-Means...\n")

nkm <- min(4, ncol(pca$x))
Xkm <- pca$x[, seq_len(nkm)]

Kr  <- 2:9
ine <- numeric(length(Kr))

# Calcular la inercia (varianza intra-cluster) para diferentes valores de K
for (i in seq_along(Kr)) {
  kk     <- kmeans(Xkm, centers = Kr[i], nstart = 25, iter.max = 300)
  ine[i] <- kk$tot.withinss
}

# Seleccionar K donde la caída de inercia se frena bruscamente (el "codo")
dife <- diff(ine)                                 
ok   <- Kr[which.min(dife) + 1]                   

cat(sprintf("  K óptimo (codo): K=%d\n", ok))

# ── GRÁFICO 1: MÉTODO DEL CODO ────────────────────────────────────────────────
dfe <- data.frame(k = Kr, ine = ine)

p_elbow <- ggplot(dfe, aes(x = k, y = ine)) +
  geom_line(color = PAL$teal, linewidth = 2) +
  geom_point(size = 3.5, color = PAL$teal) +
  geom_vline(xintercept = ok, color = PAL$red, linewidth = 1.5, linetype = "dashed") +
  annotate("label", x = ok + 0.4, y = max(ine) * 0.90,
           label = sprintf("K=%d", ok),
           color = PAL$red, fill = "white", size = 4, fontface = "bold") +
  scale_x_continuous(breaks = Kr) +
  labs(title = "Método del Codo \u2013 Selección de K", x = "N\u00b0 de Clusters (K)", y = "Inercia")

reg(p_elbow, "kmeans_codo.png")

# ── GRÁFICO 2: VISUALIZACIÓN 2D DE LOS CLUSTERS ───────────────────────────────
# Ejecutar el modelo final con el K óptimo
km  <- kmeans(Xkm, centers = ok, nstart = 30, iter.max = 500)
lkm <- km$cluster
cc  <- c(PAL$teal, PAL$orange, PAL$navy, PAL$purple,
         PAL$green, PAL$red, PAL$teal2, PAL$pink)[seq_len(ok)]

# Preparar datos espaciales y centroides para graficar
dk2d <- data.frame(PC1 = Xkm[,1], PC2 = Xkm[,2], cl = factor(lkm))
c2d  <- as.data.frame(km$centers[, 1:2])
colnames(c2d) <- c("PC1","PC2")
c2d$cl <- factor(seq_len(ok))
c2d$lb <- paste0("C", seq_len(ok))

p_k2 <- ggplot(dk2d, aes(x = PC1, y = PC2, color = cl)) +
  geom_point(alpha = .35, size = 1.3) +
  geom_point(data = c2d, aes(x = PC1, y = PC2),
             inherit.aes = FALSE, shape = 8, size = 8, stroke = 2, color = "black") +
  geom_label(data = c2d, aes(x = PC1, y = PC2 + .4, label = lb),
             inherit.aes = FALSE, fontface = "bold", size = 4, fill = "white") +
  scale_color_manual(values = cc) +
  geom_hline(yintercept = 0, color = "gray55", linewidth = .7) +
  geom_vline(xintercept = 0, color = "gray55", linewidth = .7) +
  labs(title = sprintf("Distribución de Clusters en 2D (K=%d)", ok), x = "PC1", y = "PC2") +
  theme(legend.position = "none") # Leyenda omitida para una exposición más limpia

reg(p_k2, "kmeans_2d.png")

# ── GRÁFICO 3: PERFIL FINANCIERO POR CLUSTER ──────────────────────────────────
pvx  <- c("ROA","CFO","LEV","SIZE","GRW"); pvx <- pvx[pvx %in% names(df)]
dfpr <- df[rownames(Xpr), pvx]; dfpr$cluster <- factor(lkm)

# Imprimir resumen numérico en consola
cat("\n  Perfil por Cluster:\n")
cat(sprintf("  %-8s  %10s  %10s  %10s  %8s\n", "Cluster","ROA","CFO","LEV","n"))
for (cl in levels(dfpr$cluster)) {
  s <- dfpr[dfpr$cluster == cl, ]
  cat(sprintf("  C%-7s  %+10.4f  %+10.4f  %10.4f  %8d\n",
              cl, mean(s$ROA,na.rm=TRUE), mean(s$CFO,na.rm=TRUE), mean(s$LEV,na.rm=TRUE), nrow(s)))
}

# Preparar datos largos para facet_wrap
dfpl <- dfpr |> pivot_longer(cols = all_of(pvx), names_to = "Variable", values_to = "Valor")

# Crear el Boxplot coloreado por cluster
p_pf <- ggplot(dfpl, aes(x = cluster, y = Valor, fill = cluster)) +
  geom_boxplot(
    notch = TRUE, 
    notchwidth = 0.25, 
    alpha = 0.85, 
    outlier.alpha = 0.1,
    color = "gray20"
  ) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8, linetype = "dashed") +
  scale_fill_manual(values = cc) +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(
    title    = "Perfil Financiero por Cluster",
    subtitle = "Si las cu\u00f1as (muescas) no se solapan, hay diferencia estad\u00edstica al 95%.",
    x = "Cluster", y = "Valor Original"
  ) +
  theme(legend.position = "none") # Se oculta la leyenda porque el eje X ya dice qué cluster es

reg(p_pf, "kmeans_perfiles.png")
cat("  K-Means completado\n")
readline("\n  [Enter] \u2192 Sección 7: Treemaps ROA\n")


# ==============================================================================
# SECCIÓN 7 ─ LOS 100 TITANES VS. LA INFLACIÓN (2020–2024) - FINAL 100 CAJAS
# ==============================================================================
cat("\n[7/8] Treemaps Top 100 (Ajustados por Inflación 2020-2024)...\n")

# 1. Inflación de Corea del Sur (solo 2020-2024)
inflacion_kr <- data.frame(
  year = 2020:2024,
  inflacion = c(0.0054, 0.0250, 0.0509, 0.0360, 0.0232)
)

# 2. Base de datos (Clave Única + Traductor de Titanes)
df_tree_base <- df |>
  filter(year %in% 2020:2024,
         !is.na(ROA), !is.na(SIZE), is.finite(ROA), is.finite(SIZE), !is.na(name)) |>
  group_by(stock, name, year) |>
  summarise(ROA     = mean(ROA,  na.rm = TRUE),
            activos = mean(exp(SIZE), na.rm = TRUE),
            .groups = "drop") |>
  mutate(
    # Creamos una columna de ID en texto para garantizar que NO se fusionen
    stock_char = as.character(stock),
    # Forzamos nombres occidentales para las gigantes
    name_clean = case_when(
      stock == 5930   ~ "SAMSUNG ELECTRONICS",
      stock == 660    ~ "SK HYNIX",
      stock == 5380   ~ "HYUNDAI MOTORS",
      stock == 270    ~ "KIA MOTORS",
      stock == 35420  ~ "NAVER",
      stock == 35720  ~ "KAKAO",
      stock == 51910  ~ "LG CHEMICAL",
      stock == 6400   ~ "SAMSUNG SDI",
      TRUE ~ iconv(as.character(name), to = "UTF-8", sub = "")
    ),
    # Si el nombre quedó vacío por el idioma, mostramos su ID para no dejarlo en blanco
    name_clean = ifelse(name_clean == "" | is.na(name_clean), paste0("ID: ", stock_char), name_clean)
  ) |>
  left_join(inflacion_kr, by = "year") |>
  mutate(ROA_Real = ROA - inflacion)

# 3. Función generadora de Treemaps Individuales
hacer_treemap_inflacion <- function(anio) {
  df_anio <- df_tree_base |> filter(year == anio)
  if (nrow(df_anio) < 3) return(NULL)
  
  inflacion_actual <- inflacion_kr$inflacion[inflacion_kr$year == anio]
  df_top100 <- df_anio |> slice_max(order_by = activos, n = 100)
  n_ganadores <- sum(df_top100$ROA_Real > 0, na.rm = TRUE)
  
  # ¡CLAVE! Usamos 'stock_char' como index para obligar a dibujar exactamente 100 cuadros
  tm_obj <- suppressMessages(
    treemap(df_top100, index="stock_char", vSize="activos",
            type="index", algorithm="pivotSize", sortID="ROA_Real",
            mirror.y=TRUE, mirror.x=TRUE, draw=FALSE)
  )
  
  dg <- tm_obj[["tm"]] |>
    as_tibble() |>
    mutate(
      xmax = x0+w, ymax = y0+h, stock_char = as.character(stock_char),
      x_centro = x0 + (w / 2),
      y_centro = y0 + (h / 2)
    ) |>
    # Recuperamos el nombre limpio para la etiqueta visual
    left_join(df_top100 |> select(stock_char, name_clean, ROA_Real, activos), by = "stock_char") |>
    mutate(
      label_vis = sprintf("%s\n%+.1f%%", name_clean, ROA_Real * 100)
    )
  
  # Límite artificial al 5% para saturar los colores (Mantiene los colores vivos)
  lim_escala <- 0.05 
  
  p <- ggplot(dg) +
    geom_rect(aes(xmin=x0, ymin=y0, xmax=xmax, ymax=ymax, fill=ROA_Real),
              linewidth=0.5, colour="#0F172A") +
    geom_text(aes(x = x_centro, y = y_centro, label = label_vis),
              colour="white", size=3.2, fontface="bold", lineheight=0.9, check_overlap = TRUE) +
    
    scale_fill_gradient2(
      low = "#EF4444",       # Rojo (Pierde)
      mid = "#334155",       # Gris oscuro (Empate)
      high = "#10B981",      # Verde (Vence)
      midpoint = 0,
      limits = c(-lim_escala, lim_escala),
      oob = scales::squish,  
      labels = scales::percent_format(accuracy = 1),
      name = "Desempeño Real",
      guide = guide_colorbar(
        title.position = "left",     
        title.vjust = 0.8,           
        barwidth = unit(10, "cm"),   
        barheight = unit(0.6, "cm"),
        frame.colour = "#475569",
        ticks.colour = "white"
      )
    ) +
    labs(
      title    = sprintf("Los 100 Titanes frente a la Inflación (%d)", anio),
      subtitle = sprintf("Inflación oficial: %.2f%% | %d de 100 gigantes crearon riqueza real este año", inflacion_actual * 100, n_ganadores)
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.text     = element_text(color = "#E8EADC", size = 11),
      legend.title    = element_text(color = "#E8EADC", size = 13, face = "bold"),
      plot.background = element_rect(fill="#0F172A", colour="#0F172A"),
      plot.margin     = margin(20,15,20,15),
      plot.title      = element_text(size=18, hjust=0.5, face="bold", colour="#F8FAFC", margin=margin(b=8)),
      plot.subtitle   = element_text(size=12, hjust=0.5, colour="#94A3B8", margin=margin(b=15))
    )
  
  cat(sprintf("  %d → Inflación: %.2f%% | %d/100 superaron la inflación\n", anio, inflacion_actual * 100, n_ganadores))
  return(p)
}

# 4. Generar y guardar 
for (anio in 2020:2024) {
  p_tm <- hacer_treemap_inflacion(anio)
  if (!is.null(p_tm)) reg(p_tm, sprintf("treemap_real_escala_%d.png", anio))
}

cat("  Treemaps completados.\n")

# ==============================================================================
# SECCIÓN 8 ─ GUARDADO FINAL (Resolución Maximizada)
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

# Tamaños base
ANCHO_STD <- 2800; ALTO_STD <- 1600; RES_STD <- 200
# TAMAÑO MASIVO PARA LOS TREEMAPS (Garantiza que quepan los textos)
ANCHO_TM  <- 3800; ALTO_TM  <- 2400; RES_TM  <- 200 

n_ok <- 0; n_err <- 0

for (nombre in names(REGISTRO)) {
  ruta <- file.path(script_dir, nombre)
  p    <- REGISTRO[[nombre]]
  
  es_tm <- grepl("treemap", nombre)
  
  if (es_tm) { ancho <- ANCHO_TM; alto <- ALTO_TM;  res <- RES_TM
  } else     { ancho <- ANCHO_STD; alto <- ALTO_STD; res <- RES_STD }
  
  tryCatch({
    if (inherits(p, "gg") || inherits(p, "patchwork")) {
      ggsave(ruta, plot=p, width=ancho/res, height=alto/res,
             dpi=res, bg="white", limitsize=FALSE)
    } else if (inherits(p, "recordedplot")) {
      png(ruta, width=ancho, height=alto, res=res, bg="white")
      replayPlot(p)
      dev.off()
    }
    cat(sprintf("  [OK]  %s\n", nombre))
    n_ok <- n_ok + 1
  }, error = function(e) {
    cat(sprintf("  [ERR] %s — %s\n", nombre, e$message))
    n_err <<- n_err + 1
  })
}

cat(sprintf("\n  %s\n", strrep("=", 60)))
cat(sprintf("  Guardados: %d / %d  |  Errores: %d\n", n_ok, length(REGISTRO), n_err))
cat(sprintf("  Destino  : %s\n", script_dir))
cat(sprintf("  %s\n", strrep("=", 60)))
cat("  PIPELINE v6.0 COMPLETADO\n")




# Orden: para expo 
# el EDA, PCA, Kluster, Boxplot,  Modelos


# Dejar un modelo, profundizar en el analisis multivariado 

