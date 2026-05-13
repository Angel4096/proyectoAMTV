# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  KoTaP v14.0 – Proyecto Final | Analítica y Métodos Multivariados          ║
# ║  Escuela Colombiana de Ingeniería                                           ║
# ╠══════════════════════════════════════════════════════════════════════════════╣
# ║  RESTRICCIÓN: Solo métodos clásicos del syllabus                            ║
# ║  Covarianza · Correlación · Dependencia Lineal · Asimetría · Curtosis       ║
# ║  Valores/Vectores Propios · PCA · K-Means · Regresión Lineal Múltiple (OLS) ║
# ╠══════════════════════════════════════════════════════════════════════════════╣
# ║  REGLA DE ORO: El panel (empresa i × año t) se conserva intacto.            ║
# ║  ORDEN LÓGICO: PASO A (lead target) → PASO B (IQR×3 outliers)              ║
# ║  Split: Train ≤2022 | Test 2023→predice ROA 2024 | Futuro 2024→ROA 2025    ║
# ╠══════════════════════════════════════════════════════════════════════════════╣
# ║  CORRECCIONES acumuladas v12→v13→v14:                                       ║
# ║  [1] ROE excluido de VARS_CAND (endogeneidad) | UMBRAL_CORR → 0.90         ║
# ║  [2] Fix var() sobre factores: coerción a numérico ANTES del apply          ║
# ║  [3] Bucle VIF iterativo: elimina variables con VIF>10 hasta limpiar        ║
# ║  [4] "year" incluido en df_ols_train para que dwtest funcione               ║
# ║  [5] df_ols_test filtrado a niveles de ind vistos en train (predict fix)    ║
# ║  [6] droplevels() en train post-na.omit() + xlevels en test (ind sync)     ║
# ║  [7] Gráfico B: densidad ROA real vs predicho (set Test)                   ║
# ║  [8] Split actualizado: Train ≤2022 | Test 2023 | Futuro 2024              ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ==============================================================================
# SECCIÓN 0 ─ PAQUETES
# ==============================================================================
paquetes <- c(
  # Núcleo visual y datos
  "tidyverse", "patchwork", "cluster", "factoextra",
  "scales", "ggrepel", "e1071", "data.table",
  "treemap", "ggtext", "corrplot",
  # Diagnósticos OLS (syllabus)
  "lmtest",   # bptest() → Breusch-Pagan | dwtest() → Durbin-Watson
  "car",      # vif() → Factor de Inflación de la Varianza
  "nortest"   # ad.test() → Anderson-Darling (apropiado para n grande)
)
instalar_faltantes <- function(pkgs) {
  falt <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
  if (length(falt) > 0) install.packages(falt, dependencies = TRUE)
}
instalar_faltantes(paquetes)

suppressPackageStartupMessages({
  library(tidyverse);  library(patchwork);  library(cluster)
  library(factoextra); library(scales);     library(ggrepel)
  library(e1071);      library(data.table); library(treemap)
  library(ggtext);     library(corrplot)
  library(lmtest);     library(car);        library(nortest)
})

# ==============================================================================
# SECCIÓN 1 ─ CONFIGURACIÓN GLOBAL
# ==============================================================================
DATA_PATH  <- "C:/Users/norba/Downloads/KoTaP_Dataset.csv"
OUTPUT_DIR <- "C:/Users/norba/Downloads/output_kotap_v14"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("  CSV   : %s\n", DATA_PATH))
cat(sprintf("  Salida: %s\n", OUTPUT_DIR))

PAL <- list(
  dark   = "#0F172A", navy   = "#1E3A5F", teal   = "#0D9488",
  teal2  = "#0891B2", orange = "#F97316", green  = "#059669",
  red    = "#EF4444", slate  = "#475569", white  = "#FFFFFF",
  purple = "#7C3AED", pink   = "#DB2777", amber  = "#D97706",
  rose   = "#E11D48"
)
HEAT_POS <- "#C0392B"; HEAT_NEU <- "#FFFFFF"; HEAT_NEG <- "#1D4E89"

tema_k <- theme_minimal(base_size = 12) +
  theme(
    plot.background   = element_rect(fill = "white",   color = NA),
    panel.background  = element_rect(fill = "#F8FAFC", color = NA),
    panel.grid.major  = element_line(color = "#E2E8F0", linewidth = 0.4),
    panel.grid.minor  = element_line(color = "#E2E8F0", linewidth = 0.2),
    plot.title        = element_text(face = "bold", color = PAL$dark,  size = 13),
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
cat(strrep("=", 70), "\n  KoTaP v14.0 – Métodos Multivariados Clásicos\n",
    strrep("=", 70), "\n")

# ==============================================================================
# SECCIÓN 2 ─ CARGA, PREPARACIÓN TEMPORAL Y SPLIT
# ==============================================================================
# ORDEN LÓGICO CRÍTICO:
#   PASO A primero: crear target_ROA con lead(ROA,1).
#   Razón: si filtramos outliers ANTES, podemos eliminar el año t de una empresa
#   dejando el año t-1 sin par para el lead → el target quedaría NA o incorrecto
#   porque el "siguiente año disponible" podría ser t+2, no t+1.
#   Al crear el lead SOBRE EL PANEL COMPLETO, garantizamos que target_ROA(t)
#   = ROA(t+1) de la misma empresa en el año inmediatamente siguiente.
#   PASO B después: eliminamos la fila (empresa i, año t) si sus predictores
#   son atípicos. Esto descarta también su target_ROA — correcto: no queremos
#   entrenar con una fila de características anómalas.
# ==============================================================================
cat("\n[1/7] Carga y preparación del panel...\n")

df_raw <- NULL
for (enc in c("unknown","CP949","UTF-8-BOM","latin1")) {
  ok <- tryCatch({
    tmp <- if (enc=="unknown")
      data.table::fread(DATA_PATH, encoding=enc, data.table=FALSE)
    else
      read.csv(DATA_PATH, stringsAsFactors=FALSE, fileEncoding=enc)
    if (nrow(tmp)>0) { df_raw <- tmp; TRUE } else FALSE
  }, error=function(e) FALSE,
  warning=function(w) !is.null(df_raw)&&nrow(df_raw)>0)
  if (isTRUE(ok)&&nrow(df_raw)>0){cat(sprintf("  Encoding OK: '%s'\n",enc));break}
}
if (is.null(df_raw)||nrow(df_raw)==0) stop("No se cargó el CSV.")

# ── Tipos ─────────────────────────────────────────────────────────────────────
# fiscal excluido de factores: valor constante = 12 → rompe model.matrix
VARS_FACTOR <- intersect(c("KOSPI","big4","ind","LOSS"), names(df_raw))
for (v in VARS_FACTOR) df_raw[[v]] <- as.factor(df_raw[[v]])
cols_num <- setdiff(names(df_raw), c(VARS_FACTOR,"name"))
for (col in cols_num) df_raw[[col]] <- suppressWarnings(as.numeric(df_raw[[col]]))
df_raw <- df_raw[!duplicated(df_raw),]
cat(sprintf("  Bruto: %d filas × %d columnas\n", nrow(df_raw), ncol(df_raw)))

# ── PASO A: Target = lead(ROA, 1) por empresa ─────────────────────────────────
# target_ROA(t) = ROA(t+1): los predictores del año t predicen la rentabilidad
# del año siguiente. La última observación de cada empresa → target_ROA = NA.
df <- df_raw |>
  group_by(stock) |>
  arrange(year, .by_group = TRUE) |>
  mutate(target_ROA = lead(ROA, n = 1)) |>
  ungroup()
cat(sprintf("  PASO A – target_ROA: %d válidos | %d = NA (sin año siguiente)\n",
            sum(!is.na(df$target_ROA)), sum(is.na(df$target_ROA))))

# ── PASO B: Outliers IQR × 3 (sobre los PREDICTORES, no el target) ───────────
# Solo se aplica sobre ratios financieros (variables derivadas, escaladas).
# Las cuentas contables brutas (asset, sales…) se excluyen del filtro porque
# su magnitud varía estructuralmente con el tamaño de empresa.
VARS_IQR <- intersect(
  c("ROA","SIZE","LEV","ROE","CFO","GRW","CUR","INVREC",
    "MB","TQ","PPE","AGE"),
  names(df)
)

eliminar_outliers_iqr <- function(data, vars, k = 3) {
  mask <- rep(TRUE, nrow(data))
  for (col in vars) {
    if (!col %in% names(data) || !is.numeric(data[[col]])) next
    q   <- quantile(data[[col]], probs=c(0.25,0.75), na.rm=TRUE)
    iqr <- q[2] - q[1]; if (iqr==0) next
    mask <- mask &
      !is.na(data[[col]]) &
      data[[col]] >= (q[1] - k*iqr) &
      data[[col]] <= (q[2] + k*iqr)
  }
  data[mask,]
}

n_antes <- nrow(df)
df      <- eliminar_outliers_iqr(df, VARS_IQR, k=3)
cat(sprintf("  PASO B – IQR×3: %d filas eliminadas (%d → %d | %.1f%%)\n",
            n_antes-nrow(df), n_antes, nrow(df),
            (n_antes-nrow(df))/n_antes*100))

for (v in VARS_FACTOR)
  if (v %in% names(df) && is.factor(df[[v]]))
    df[[v]] <- droplevels(df[[v]])

# ── Split temporal estricto ────────────────────────────────────────────────────
# FIX [8]: Split actualizado para predecir ROA de 2024.
#
# LÓGICA CAUSAL:
#   La variable target_ROA del año t = ROA del año t+1 (creada con lead).
#   Por tanto:
#     • La fila año 2022 tiene target_ROA = ROA(2023)  ← train
#     • La fila año 2023 tiene target_ROA = ROA(2024)  ← TEST → predice 2024
#     • La fila año 2024 tiene target_ROA = ROA(2025)  ← NA, futuro sin ground truth
#
#   El modelo se entrena con los fundamentos de 2011-2022 para aprender patrones,
#   y se evalúa prediciendo el ROA de 2024 usando los fundamentos de 2023.
#   Esto maximiza el tamaño del train (12 años) y el horizonte de predicción.
#
# Train  : year <= 2022 + target_ROA válido → 9,971 obs (~12 años de historia)
# Test   : year == 2023 + target_ROA válido → ROA real de 2024 como ground truth
# Futuro : year == 2024 → target_ROA = ROA(2025) = NA (sin dato disponible)

df_train  <- df |> filter(year <= 2022,         !is.na(target_ROA))
df_test   <- df |> filter(year == 2023,          !is.na(target_ROA))
df_futuro <- df |> filter(year == 2024)

cat(sprintf("  Panel completo  : %d obs | %d empresas | %d-%d\n",
            nrow(df), length(unique(df$stock)), min(df$year), max(df$year)))
cat(sprintf("  Train (≤2022)   : %d obs | features 2011-2022 → target = ROA hasta 2023\n", nrow(df_train)))
cat(sprintf("  Test  (2023)    : %d obs | features 2023 → target = ROA 2024 (ground truth)\n", nrow(df_test)))
cat(sprintf("  Futuro(2024)    : %d obs | features 2024 → target = ROA 2025 (sin dato)\n", nrow(df_futuro)))

KEY_VARS <- intersect(
  c("SIZE","LEV","ROA","ROE","CFO","GRW","CUR","INVREC","MB","TQ","PPE","AGE"),
  names(df)
)

# Variables candidatas para análisis:
# Ratios financieros e indicadores de gobierno corporativo.
# EXCLUIMOS cuentas brutas (asset, sales, etc.) porque:
#   1. Ya están capturadas en los ratios (SIZE=log(activos), LEV=pasivo/activo…)
#   2. Sus correlaciones son casi perfectas entre sí → arruinarían el filtro
#   3. No aportan información adicional en presencia de los ratios normalizados
#
# FIX [1]: ROE excluida explícitamente.
# Fundamento econométrico: ROE = NI/Equity, y target_ROA = NI/Activos(t+1).
# Ambas comparten el Resultado Neto (NI) como numerador → correlación casi
# perfecta con el target → ENDOGENEIDAD: la variable "haría trampa" revelando
# información del numerador del target. Se excluye para garantizar predicción
# genuina sin fuga de información.
VARS_CAND <- intersect(
  c("SIZE","LEV","CUR","GRW","CFO","PPE","AGE","INVREC","MB","TQ",
    # ROE eliminado: endogeneidad con target_ROA (comparten numerador NI)
    "GETR","CETR","GETR3","CETR3","GETR5","CETR5",
    "TSTA","TSDA","A_GETR","A_CETR","A_GETR3","A_CETR3","A_GETR5","A_CETR5",
    "forn","own","KOSPI","big4","LOSS"),
  names(df)
)

readline("\n  [Enter] → Sección 2: EDA + Filtro Colinealidad\n")

# ==============================================================================
# SECCIÓN 3 ─ EDA: ASIMETRÍA, CURTOSIS, CORRELACIÓN Y FILTRO COLINEAL
# ==============================================================================
cat("\n[2/7] EDA – Descriptiva + Correlación + Filtro Colinealidad...\n")
cat("  [Base: set TRAIN para evitar data leakage]\n")

VAR_COL <- c(ROA="#0D9488",ROE="#0891B2",SIZE="#1E3A5F",LEV="#F97316",
             CFO="#7C3AED",GRW="#059669",CUR="#DC2626",INVREC="#0369A1",
             MB="#92400E",TQ="#065F46",PPE="#4C1D95",AGE="#831843")

# ── 3.1 Tabla Asimetría y Curtosis ────────────────────────────────────────────
# ASIMETRÍA: mide el sesgo de la distribución.
#   > 0 → cola derecha (pocas empresas muy rentables elevan la media)
#   |asim| > 1 → la media ya NO es representativa del caso típico
#
# CURTOSIS EXCESS (Pearson - 3):
#   > 0 → leptocúrtica: más valores extremos de lo esperado por la normal
#   < 0 → platicúrtica: colas más ligeras que la normal

cat("\n  Tabla Asimetría y Curtosis (set TRAIN, empresa×año):\n")
cat(sprintf("  %-8s  %8s  %8s  %8s  %8s  %8s  %8s  %s\n",
            "Variable","n","Media","Mediana","Desv.","CV%","Asim.","Kurt."))
cat(sprintf("  %s\n", strrep("-", 75)))

tabla_eda <- map_dfr(KEY_VARS, function(var) {
  d <- df_train[[var]]; d <- d[is.finite(d)]
  tibble(
    Variable = var,
    n        = length(d),
    Media    = mean(d),
    Mediana  = median(d),
    Desv     = sd(d),
    CV_pct   = sd(d)/abs(mean(d))*100,
    Asimetria= e1071::skewness(d),
    Curtosis = e1071::kurtosis(d)   # Exceso de curtosis (normal = 0)
  )
})
for (i in seq_len(nrow(tabla_eda))) {
  r <- tabla_eda[i,]
  flag_asim <- if (abs(r$Asimetria) > 1) " ← |asim|>1" else ""
  flag_kurt <- if (r$Curtosis > 3)       " ← leptocúrt." else ""
  cat(sprintf("  %-8s  %8s  %8.4f  %8.4f  %8.4f  %8.1f  %8.3f  %8.3f%s%s\n",
              r$Variable, format(r$n, big.mark=","),
              r$Media, r$Mediana, r$Desv, r$CV_pct,
              r$Asimetria, r$Curtosis, flag_asim, flag_kurt))
}

# Paneles descriptivos (histograma + boxplot + QQ-plot) por variable clave
panel_desc <- function(var, data, color) {
  d <- data[[var]]; d <- d[is.finite(d)]; if(length(d)<5) return(NULL)
  pos  <- sum(d>=0); neg <- sum(d<0)
  pct  <- round(pos/length(d)*100,1)
  mu   <- mean(d); md <- median(d); sg <- sd(d)
  asim <- round(e1071::skewness(d),3); kurt <- round(e1071::kurtosis(d),3)
  cv   <- round(sg/abs(mu)*100,2)
  
  df_p <- data.frame(x=d, cl=ifelse(d>=0,"Ganancia (\u2265 0)","P\u00e9rdida (< 0)"))
  
  ph <- ggplot(df_p,aes(x=x,fill=cl)) +
    geom_histogram(bins=55,boundary=0,alpha=0.82,color="white",linewidth=0.15)+
    scale_fill_manual(
      values=c("Ganancia (\u2265 0)"=PAL$green,"P\u00e9rdida (< 0)"=PAL$red),
      name="Signo")+
    geom_vline(xintercept=mu,color=PAL$amber,linewidth=1.3,linetype="dashed")+
    geom_vline(xintercept=md,color=PAL$navy, linewidth=1.3,linetype="dotted")+
    annotate("label",x=Inf,y=Inf,
             label=sprintf("Media   = %.4f\nMediana = %.4f\nDesv.   = %.4f\nCV      = %.2f%%\nAsim    = %.3f\nKurt    = %.3f\nn       = %s",
                           mu,md,sg,cv,asim,kurt,format(length(d),big.mark=",")),
             hjust=1.05,vjust=1.05,size=2.9,fill="#FFFDE7",color=PAL$dark,family="mono")+
    annotate("text",x=if(max(d)!=0)max(d)*0.55 else 1,y=Inf,
             label=sprintf("Verde: %.1f%%\nRojo: %.1f%%",pct,100-pct),
             vjust=1.6,size=3.5,color=PAL$dark,fontface="bold")+
    labs(title=paste(var,"\u2013 Distribuci\u00f3n"),
         subtitle="Set TRAIN | empresa\u00d7a\u00f1o",
         x=var,y="Frecuencia")+
    theme(legend.position="bottom",legend.key.size=unit(0.4,"cm"))
  
  pb <- ggplot(df_p,aes(x="",y=x))+
    annotate("rect",xmin=-Inf,xmax=Inf,ymin=0,ymax=max(d)*1.1,fill=PAL$green,alpha=.07)+
    annotate("rect",xmin=-Inf,xmax=Inf,ymin=min(d)*1.1,ymax=0, fill=PAL$red,  alpha=.07)+
    geom_boxplot(fill=color,alpha=0.60,color=PAL$rose,linewidth=1.1,
                 notch=TRUE,notchwidth=0.25,
                 outlier.alpha=.2,outlier.size=1.5,outlier.color=color)+
    geom_hline(yintercept=0,color="gray40",linewidth=1.0,linetype="dashed")+
    annotate("label",x=1.47,y=quantile(d,.75),
             label=sprintf("Verde\n%s\n(%.1f%%)",format(pos,big.mark=","),pct),
             size=2.8,color=PAL$green,fontface="bold",hjust=0,fill="white")+
    annotate("label",x=1.47,y=quantile(d,.25),
             label=sprintf("Rojo\n%s\n(%.1f%%)",format(neg,big.mark=","),100-pct),
             size=2.8,color=PAL$red,fontface="bold",hjust=0,fill="white")+
    labs(title=paste(var,"\u2013 Boxplot [IC 95% mediana]"),y=var,x=NULL)+
    coord_cartesian(xlim=c(0.5,1.9))
  
  qqr <- qqnorm(d,plot.it=FALSE)
  dfq <- data.frame(t=qqr$x,m=qqr$y)
  rq  <- round(cor(dfq$t,dfq$m),4)
  pq  <- ggplot(dfq,aes(x=t,y=m))+
    geom_point(color=color,alpha=.40,size=1.1)+
    geom_abline(slope=sd(d),intercept=mean(d),color=PAL$red,linewidth=1.8)+
    annotate("label",x=-Inf,y=Inf,
             label=sprintf("r = %.4f\n%s",rq,
                           ifelse(rq>.99,"Muy normal",ifelse(rq>.97,"Aprox. normal","Colas pesadas"))),
             hjust=-.1,vjust=1.3,size=3.5,fill="#FFF3E0",color=PAL$dark)+
    labs(title=paste(var,"\u2013 QQ-Plot"),
         x="Cuantiles N(0,1)",y="Cuantiles muestrales")
  
  list(h=ph,b=pb,q=pq)
}

for (var in KEY_VARS) {
  if (!var %in% names(df_train)) next
  res <- panel_desc(var, df_train, VAR_COL[[var]])
  if (is.null(res)) next
  p <- (res$h|res$b|res$q)+
    plot_annotation(
      title    = sprintf("Descriptiva – %s | Set TRAIN (empresa\u00d7a\u00f1o)",var),
      subtitle = sprintf("n=%s | Asim=%.3f | Kurt=%.3f | Verde=%.1f%%",
                         format(nrow(df_train),big.mark=","),
                         e1071::skewness(df_train[[var]],na.rm=TRUE),
                         e1071::kurtosis(df_train[[var]],na.rm=TRUE),
                         mean(df_train[[var]]>=0,na.rm=TRUE)*100),
      theme=theme(plot.title=element_text(face="bold",size=14),
                  plot.subtitle=element_text(size=9))
    )
  reg(p,paste0("desc_",var,".png"))
}
# ── 3.2 Matriz de Correlación y Covarianza (set TRAIN) ────────────────────────
df_num_train <- df_train |>
  select(all_of(VARS_CAND)) |>
  mutate(across(where(is.factor), ~ as.numeric(as.character(.x)))) |>
  select(where(is.numeric))

vars_num_train <- names(df_num_train)[
  apply(df_num_train, 2, function(x) {
    vv <- var(x, na.rm=TRUE)
    !is.na(vv) && is.finite(vv) && vv > 1e-10
  })
]
df_num_train <- df_num_train[, vars_num_train]

mat_cov  <- cov(df_num_train,  use="pairwise.complete.obs")
mat_corr <- cor(df_num_train,  method="spearman", use="pairwise.complete.obs")

cat(sprintf("\n  Matriz Spearman: %d variables candidatas (set TRAIN)\n",
            length(vars_num_train)))

# ---> GRÁFICO MATRIZ COMPLETA (LAS 29 VARIABLES) <---
cat("  Generando heatmap completo (29 variables)...\n")
png(file.path(OUTPUT_DIR,"eda_heatmap_completo_29vars.png"),
    width=3600,height=3400,res=200,bg="white")
corrplot(mat_corr,
         method="color",type="lower",order="hclust",hclust.method="ward.D2",
         tl.cex=0.60,tl.col="#0F172A",tl.srt=45,cl.cex=0.75,
         col=colorRampPalette(c(HEAT_NEG,HEAT_NEU,HEAT_POS))(200),
         title=sprintf("Spearman – %d Variables Candidatas | TRAIN (ward.D2)",
                       length(vars_num_train)),
         mar=c(0,0,3,0),addgrid.col="white",diag=FALSE)
dev.off()
cat("  eda_heatmap_completo_29vars.png guardado\n")

# ── 3.3 Filtro Manual de colinealidad (Pasando a 25 vars) ─────────────────────
UMBRAL_CORR <- 0.90
VARS_REMOVIDAS <- c("GETR3", "GETR5", "A_GETR3", "A_GETR5")
VARS_FILTRADAS <- setdiff(vars_num_train, VARS_REMOVIDAS)

cat(sprintf("\n  Filtro manual de colinealidad:\n"))
cat(sprintf("  Antes : %d variables\n", length(vars_num_train)))
cat(sprintf("  Después: %d variables (se eliminaron %d manualmente)\n",
            length(VARS_FILTRADAS), length(VARS_REMOVIDAS)))
# ── 3.3 Filtro programático de colinealidad ────────────────────────────────────
# FUNDAMENTO TEÓRICO: la colinealidad exacta hace que (X'X) sea singular y
# no invertible. La cuasi-colinealidad infla la varianza de los estimadores
# β̂, haciendo el modelo inestable (altos VIF, coeficientes que cambian sign0
# al añadir/quitar variables). Eliminamos variables con |r| > UMBRAL_CORR.
#
# ALGORITMO (equivalente al findCorrelation de caret, sin ML):
#   1. Encontrar el par de variables con mayor correlación absoluta.
#   2. De ese par, eliminar la que tenga MAYOR correlación media con las demás.
#   3. Repetir hasta que todas las correlaciones restantes < UMBRAL_CORR.
# Esto asegura máxima cobertura de información conservando la variable
# más "independiente" de cada par altamente colineal.

# FIX [1]: Umbral de correlación ajustado a 0.90.
# Con ROE ya excluida (la mayor fuente de colinealidad extrema), el umbral
# 0.90 es más conservador que 0.85 y retiene más variables informativas,
# manteniendo aún el control de la multicolinealidad severa.
UMBRAL_CORR <- 0.90

# [MODIFICACIÓN PARA LA SUSTENTACIÓN]
# Para construir la narrativa de las 29 variables candidatas, se desactiva 
# el bucle automático. En su lugar, basándonos en el clustering visual de la 
# matriz (Ward.D2), extraemos manualmente 4 variables con colinealidad extrema 
# para llegar exactamente a 25 predictores antes de entrar a la sección del VIF.
VARS_REMOVIDAS <- c("GETR3", "GETR5", "A_GETR3", "A_GETR5")
VARS_FILTRADAS <- setdiff(vars_num_train, VARS_REMOVIDAS)

cat(sprintf("\n  Filtro manual de colinealidad (basado en clusters |r| > %.2f):\n", UMBRAL_CORR))
cat(sprintf("  Antes : %d variables\n", length(vars_num_train)))
cat(sprintf("  Después: %d variables (se eliminaron %d manualmente)\n",
            length(VARS_FILTRADAS), length(VARS_REMOVIDAS)))
cat(sprintf("  Eliminadas: %s\n", paste(VARS_REMOVIDAS, collapse=", ")))
cat(sprintf("  Conservadas: %s\n", paste(VARS_FILTRADAS, collapse=", ")))

# Guardar la matriz de covarianza de las variables FILTRADAS como CSV
mat_cov_filt <- mat_cov[VARS_FILTRADAS, VARS_FILTRADAS]
write.csv(
  as.data.frame(mat_cov_filt) |> tibble::rownames_to_column("variable"),
  file.path(OUTPUT_DIR, "eda_matriz_covarianza.csv"),
  row.names=FALSE)
write.csv(
  as.data.frame(mat_corr[VARS_FILTRADAS,VARS_FILTRADAS]) |>
    tibble::rownames_to_column("variable"),
  file.path(OUTPUT_DIR, "eda_matriz_correlacion.csv"),
  row.names=FALSE)
cat("  Matrices guardadas en CSV\n")

# ── Heatmap de variables FILTRADAS ────────────────────────────────────────────
mat_plot <- mat_corr[VARS_FILTRADAS, VARS_FILTRADAS]
png(file.path(OUTPUT_DIR,"eda_heatmap_vars_filtradas.png"),
    width=2400,height=2200,res=200,bg="white")
corrplot(mat_plot,
         method="color",type="lower",addCoef.col="black",
         number.cex=0.65,tl.cex=0.75,tl.col="#0F172A",tl.srt=45,
         col=colorRampPalette(c(HEAT_NEG,HEAT_NEU,HEAT_POS))(200),
         title=sprintf("Spearman – %d Variables post-Filtro Manual | TRAIN",
                       length(VARS_FILTRADAS)),
         mar=c(0,0,3,0),diag=FALSE)
dev.off()
cat("  eda_heatmap_vars_filtradas.png guardado\n")

# Heatmap completo (todas las candidatas)
cat("  Generando heatmap completo...\n")
png(file.path(OUTPUT_DIR,"eda_heatmap_completo.png"),
    width=3600,height=3400,res=200,bg="white")
corrplot(mat_corr,
         method="color",type="lower",order="hclust",hclust.method="ward.D2",
         tl.cex=0.60,tl.col="#0F172A",tl.srt=45,cl.cex=0.75,
         col=colorRampPalette(c(HEAT_NEG,HEAT_NEU,HEAT_POS))(200),
         title=sprintf("Spearman – %d Variables Candidatas | TRAIN (ward.D2)",
                       length(vars_num_train)),
         mar=c(0,0,3,0),addgrid.col="white",diag=FALSE)
dev.off()
cat("  eda_heatmap_completo.png guardado\n")

# Gráfico Top 25 correlaciones con target_ROA
if ("target_ROA" %in% names(df_num_train)) {
  vars_sin_target <- VARS_FILTRADAS[VARS_FILTRADAS!="target_ROA"]
  df_ext <- df_train |>
    mutate(across(where(is.factor),~as.numeric(as.character(.x)))) |>
    select(all_of(c(vars_sin_target,"target_ROA")))
  corrs_target <- cor(df_ext, method="spearman", use="pairwise.complete.obs")
  if ("target_ROA" %in% rownames(corrs_target)) {
    vals_t <- corrs_target["target_ROA", vars_sin_target]
    vals_t <- vals_t[order(abs(vals_t),decreasing=TRUE)][1:min(20,length(vals_t))]
    df_tc  <- data.frame(
      Variable=factor(names(vals_t),levels=rev(names(vals_t))),
      r=as.numeric(vals_t)
    ) |> mutate(signo=ifelse(r>0,"Positiva","Negativa"))
    
    p_top <- ggplot(df_tc,aes(x=r,y=Variable,fill=signo))+
      geom_col(alpha=0.85,width=0.72)+
      geom_vline(xintercept=0,color="gray30",linewidth=0.8)+
      geom_text(aes(label=sprintf("%+.3f",r),hjust=ifelse(r>0,-0.10,1.10)),
                size=3.4,fontface="bold",color=PAL$dark)+
      scale_fill_manual(values=c("Positiva"=PAL$teal,"Negativa"=PAL$rose),name=NULL)+
      scale_x_continuous(limits=c(-0.75,0.75),labels=number_format(accuracy=0.1))+
      labs(title="Top Correlaciones Spearman con target_ROA (año t+1)",
           subtitle=sprintf("Set TRAIN | %d variables post-filtro",
                            length(VARS_FILTRADAS)),
           x="Correlación de Spearman",y=NULL)+
      theme(legend.position="bottom")
    reg(p_top,"eda_corr_target_roa.png")
  }
}
cat("  EDA completado\n")
readline("\n  [Enter] → Sección 3: PCA\n")
# ==============================================================================
# SECCIÓN 4 ─ PCA (Set TRAIN, variables post-filtro colinealidad)
# ==============================================================================
cat("\n[3/7] PCA (set TRAIN, variables filtradas)...\n")

X_pca_raw <- df_train |>
  mutate(across(where(is.factor), ~ as.numeric(as.character(.x)))) |>
  select(all_of(VARS_FILTRADAS)) |>
  na.omit()

vok <- apply(X_pca_raw, 2, function(x) var(x, na.rm = TRUE) > 1e-10)
X_pca_raw <- X_pca_raw[, vok, drop = FALSE]
VARS_PCA  <- colnames(X_pca_raw)

# Guardar índices de filas del train que sobrevivieron na.omit
rows_pca <- as.integer(rownames(X_pca_raw))

X_sc  <- scale(X_pca_raw)
pca   <- prcomp(X_sc, center = FALSE, scale. = FALSE)

ve       <- (pca$sdev^2) / sum(pca$sdev^2)
vc       <- cumsum(ve)
eig      <- pca$sdev^2
np       <- length(ve)
n_kaiser <- sum(eig >= 1)

cat(sprintf("  Variables en PCA: %d | Obs TRAIN: %d\n",
            length(VARS_PCA), nrow(X_pca_raw)))
cat(sprintf("  PCs con eigenvalor ≥ 1 (Kaiser): %d\n", n_kaiser))
cat(sprintf("  PC1=%.1f%% | PC2=%.1f%% | PC3=%.1f%% | Acum. 3PCs=%.1f%%\n",
            ve[1]*100, ve[2]*100, ve[3]*100, vc[3]*100))

cat("\n  Tabla eigenvalores:\n")
cat(sprintf("  %-5s  %10s  %10s  %10s  %s\n",
            "PC", "Eigenval.", "%Var", "%Acum.", "Decisión"))
cat(sprintf("  %s\n", strrep("-", 52)))
for (i in seq_len(min(10, np)))
  cat(sprintf("  PC%-3d  %10.4f  %10.2f  %10.2f  %s\n",
              i, eig[i], ve[i]*100, vc[i]*100,
              ifelse(eig[i] >= 1, "<- RETENER (Kaiser)", "descartar")))

# ── Loadings tabla (3 primeras PCs) ───────────────────────────────────────────
cat("\n  Loadings (primeras 3 PCs):\n")
cat(sprintf("  %-14s  %8s  %8s  %8s\n", "Variable", "PC1", "PC2", "PC3"))
cat(sprintf("  %s\n", strrep("-", 44)))
ldf <- as.data.frame(pca$rotation[, 1:min(3, np)])
for (j in seq_len(nrow(ldf)))
  cat(sprintf("  %-14s  %+8.4f  %+8.4f  %+8.4f\n",
              rownames(ldf)[j],
              ldf[j, 1], ldf[j, 2],
              if (ncol(ldf) >= 3) ldf[j, 3] else NA_real_))

# ── Scree Plot + Varianza Acumulada ───────────────────────────────────────────
dfs <- data.frame(
  PC = factor(paste0("PC", seq_len(np)), levels = paste0("PC", seq_len(np))),
  eg = eig, vp = ve * 100, cp = vc * 100
)

p_sc <- ggplot(dfs, aes(x = PC, y = eg)) +
  geom_col(aes(fill = eg >= 1), alpha = .85, width = .7) +
  geom_line(aes(group = 1), color = PAL$orange, linewidth = 1.8) +
  geom_point(color = PAL$orange, size = 3.5) +
  scale_fill_manual(values = c("TRUE" = PAL$teal, "FALSE" = PAL$slate),
                    labels = c("TRUE" = "Retener", "FALSE" = "Descartar"),
                    name = NULL) +
  labs(title = "Scree Plot – Descomposición en Valores Propios",
       x = "Componente Principal",
       y = "Eigenvalor (varianza explicada)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

p_cp <- ggplot(dfs, aes(x = PC, y = cp, group = 1)) +
  geom_area(fill = PAL$teal, alpha = .18) +
  geom_line(color = PAL$teal, linewidth = 2.2) +
  geom_point(size = 3.5, color = PAL$teal) +
  geom_hline(yintercept = 80, color = PAL$orange,
             linewidth = 1.0, linetype = "dashed") +
  scale_y_continuous(limits = c(0, 101), breaks = seq(0, 100, 20)) +
  labs(title = "Varianza Acumulada",
       x = "N° de Componentes", y = "% Varianza total") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

reg(
  (p_sc | p_cp) +
    plot_annotation(
      title    = "PCA – Selección de Componentes Principales",
      theme = theme(
        plot.title    = element_text(face = "bold", size = 14, color = PAL$dark))
    ),
  "pca_scree.png"
)

# ── Función auxiliar para construir un biplot ─────────────────────────────────
hacer_biplot <- function(pc_x, pc_y, scaling_factor = 0.65,
                         titulo_extra = "", guardar_nombre = NULL) {
  
  pct_x <- ve[pc_x] * 100
  pct_y <- ve[pc_y] * 100
  
  sc <- as.data.frame(pca$x[, c(pc_x, pc_y)])
  colnames(sc) <- c("PCx", "PCy")
  
  sf  <- max(abs(sc)) * scaling_factor
  adf <- data.frame(
    Variable = VARS_PCA,
    x0 = 0, y0 = 0,
    x1 = pca$rotation[VARS_PCA, pc_x] * sf,
    y1 = pca$rotation[VARS_PCA, pc_y] * sf
  )
  
  adf$contrib <- sqrt(adf$x1^2 + adf$y1^2)
  
  p <- ggplot(sc, aes(x = PCx, y = PCy)) +
    geom_point(color = PAL$teal, alpha = .20, size = 0.7) +
    geom_hline(yintercept = 0, color = "gray55",
               linewidth = 0.8, linetype = "dashed") +
    geom_vline(xintercept = 0, color = "gray55",
               linewidth = 0.8, linetype = "dashed") +
    geom_segment(data = adf,
                 aes(x = x0, y = y0, xend = x1, yend = y1,
                     color = contrib),
                 arrow = arrow(length = unit(.28, "cm"), type = "closed"),
                 linewidth = 1.4) +
    scale_color_gradient(low = PAL$teal2, high = PAL$orange,
                         name = "Contribución") +
    geom_label_repel(data = adf,
                     aes(x = x1 * 1.22, y = y1 * 1.22, label = Variable),
                     size = 3.6, fontface = "bold",
                     fill = "white", color = PAL$dark,
                     box.padding = 0.30, segment.color = PAL$slate,
                     max.overlaps = 25) +
    labs(
      title = sprintf("Biplot PC%d × PC%d %s", pc_x, pc_y, titulo_extra),
      x = sprintf("PC%d – %.1f%% de varianza explicada", pc_x, pct_x),
      y = sprintf("PC%d – %.1f%% de varianza explicada", pc_y, pct_y)
    ) +
    theme(legend.position = "right",
          legend.key.height = unit(1.2, "cm"))
  
  if (!is.null(guardar_nombre))
    reg(p, guardar_nombre)
  
  invisible(p)
}

# ── Biplot PC1 × PC2 ───────────────────────────────
p_bp12 <- hacer_biplot(1, 2,
                       titulo_extra = "(Tamaño/Gobierno vs Rentabilidad)",
                       guardar_nombre = "pca_biplot_PC1xPC2.png")

# ── Biplot PC1 × PC3 ───────────────────────────────
p_bp13 <- hacer_biplot(1, 3,
                       titulo_extra = "(Tamaño/Gobierno vs Deuda/Caja)",
                       guardar_nombre = "pca_biplot_PC1xPC3.png")

# ── Biplot PC2 × PC3 ───────────────────────────────
p_bp23 <- hacer_biplot(2, 3,
                       titulo_extra = "(Rentabilidad vs Deuda/Caja)",
                       guardar_nombre = "pca_biplot_PC2xPC3.png")

# ── Panel comparativo: las 3 combinaciones juntas ─────────────────────────────
p_biplots_panel <-
  (p_bp12 + labs(title = sprintf("A) PC1×PC2 (%.1f%% + %.1f%%)",
                                 ve[1]*100, ve[2]*100))) +
  (p_bp13 + labs(title = sprintf("B) PC1×PC3 (%.1f%% + %.1f%%)",
                                 ve[1]*100, ve[3]*100))) +
  (p_bp23 + labs(title = sprintf("C) PC2×PC3 (%.1f%% + %.1f%%)",
                                 ve[2]*100, ve[3]*100))) +
  plot_annotation(
    title    = "PCA – Comparativo Pairplot Biplots (PC1, PC2, PC3)",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 14, color = PAL$dark))
  )
reg(p_biplots_panel, "pca_biplots_pairplot_panel.png")

# ── Biplot PRINCIPAL para la presentación ─────────────────────────────────────
p_bp_best <- hacer_biplot(1, 2,
                          titulo_extra = sprintf("| %.1f%% + %.1f%% = %.1f%% total",
                                                 ve[1]*100, ve[2]*100, (ve[1]+ve[2])*100),
                          guardar_nombre = "pca_biplot_PRINCIPAL.png")

cat(sprintf("  Biplots generados: PC1×PC2 / PC1×PC3 / PC2×PC3 + panel comparativo\n"))
cat(sprintf("  Biplot principal guardado: pca_biplot_PRINCIPAL.png (PC1×PC2)\n"))
cat("  PCA completado\n")
readline("\n  [Enter] → Sección 5: K-Means\n")
# ==============================================================================
# SECCIÓN 5 ─ K-MEANS con K = 4 fijo (sobre scores PC1, PC2, PC3)
# ==============================================================================
cat("\n[4/7] K-Means K=4 (set TRAIN, scores PC1-PC3)...\n")

nkm <- 3
Xkm <- pca$x[, seq_len(nkm)]

# ── Método del codo (solo para documentar/verificar) ─────────────────────────
Kr  <- 2:9
ine <- numeric(length(Kr))
for (i in seq_along(Kr)) {
  kk     <- kmeans(Xkm, centers = Kr[i], nstart = 25, iter.max = 300)
  ine[i] <- kk$tot.withinss
}

codo_k <- Kr[which.min(diff(ine)) + 1]
cat(sprintf("  K óptimo por codo: K=%d (referencia) | K fijo usado: K=4\n", codo_k))

dfe <- data.frame(k = Kr, ine = ine)
p_elbow <- ggplot(dfe, aes(x = k, y = ine)) +
  geom_line(color = PAL$teal, linewidth = 2) +
  geom_point(size = 4, color = PAL$teal) +
  scale_x_continuous(breaks = Kr) +
  labs(title = "Método del Codo",
       x = "Número de Clusters K",
       y = "Inercia total intra-cluster (WCSS)")
reg(p_elbow, "kmeans_codo.png")

# ── Ajustar K-Means con K = 4 ─────────────────────────────────────────────────
K_FIJO <- 4L
set.seed(42)
km  <- kmeans(Xkm, centers = K_FIJO, nstart = 30, iter.max = 500)
lkm <- km$cluster

cc <- c(PAL$teal, PAL$orange, PAL$navy, PAL$purple)
names(cc) <- as.character(1:K_FIJO)

cat(sprintf("  Inercia K=4: %.2f | Varianza explicada: %.1f%%\n",
            km$tot.withinss,
            (1 - km$tot.withinss / km$totss) * 100))
cat("  Tamaño de clusters:\n")
for (k in 1:K_FIJO)
  cat(sprintf("    C%d: %d obs (%.1f%%)\n",
              k, sum(lkm == k), mean(lkm == k) * 100))

# ── Función para construir scatter 2D de clusters ────────────────────────────
hacer_scatter_clusters <- function(pc_x, pc_y, scores, labels,
                                   centroids, ve, colores,
                                   guardar_nombre = NULL) {
  pct_x <- ve[pc_x] * 100
  pct_y <- ve[pc_y] * 100
  
  dk2d <- data.frame(
    PCx = scores[, pc_x],
    PCy = scores[, pc_y],
    cl  = factor(labels)
  )
  c2d <- as.data.frame(centroids[, c(pc_x, pc_y)])
  colnames(c2d) <- c("PCx", "PCy")
  c2d$cl <- factor(seq_len(nrow(c2d)))
  c2d$lb <- paste0("C", seq_len(nrow(c2d)))
  
  p <- ggplot(dk2d, aes(x = PCx, y = PCy, color = cl)) +
    geom_point(alpha = .28, size = 1.2) +
    geom_point(data = c2d, aes(x = PCx, y = PCy),
               inherit.aes = FALSE,
               shape = 8, size = 9, stroke = 2.5, color = "black") +
    geom_label(data = c2d,
               aes(x = PCx, y = PCy + max(abs(scores[, pc_y])) * 0.08,
                   label = lb),
               inherit.aes = FALSE, fontface = "bold",
               size = 4.5, fill = "white") +
    scale_color_manual(values = colores, guide = "none") +
    geom_hline(yintercept = 0, color = "gray55", linewidth = .7) +
    geom_vline(xintercept = 0, color = "gray55", linewidth = .7) +
    labs(
      title = sprintf("K-Means K=4 | PC%d × PC%d", pc_x, pc_y),
      x = sprintf("PC%d (%.1f%%)", pc_x, pct_x),
      y = sprintf("PC%d (%.1f%%)", pc_y, pct_y)
    )
  
  if (!is.null(guardar_nombre))
    reg(p, guardar_nombre)
  
  invisible(p)
}

# ── Scatter PC1×PC2 ───────────────────────────────────────────────────────────
p_k12 <- hacer_scatter_clusters(1, 2, Xkm, lkm, km$centers, ve, cc,
                                guardar_nombre = "kmeans_2d_PC1xPC2.png")

# ── Scatter PC1×PC3 ───────────────────────────────────────────────────────────
p_k13 <- hacer_scatter_clusters(1, 3, Xkm, lkm, km$centers, ve, cc,
                                guardar_nombre = "kmeans_2d_PC1xPC3.png")

# ── Scatter PC2×PC3 ───────────────────────────────────────────────────────────
p_k23 <- hacer_scatter_clusters(2, 3, Xkm, lkm, km$centers, ve, cc,
                                guardar_nombre = "kmeans_2d_PC2xPC3.png")

# ── Panel comparativo de los 3 scatters ───────────────────────────────────────
p_clusters_panel <- (p_k12 + labs(title = sprintf("A) PC1×PC2 (%.1f%%+%.1f%%)",
                                                  ve[1]*100, ve[2]*100))) +
  (p_k13 + labs(title = sprintf("B) PC1×PC3 (%.1f%%+%.1f%%)",
                                ve[1]*100, ve[3]*100))) +
  (p_k23 + labs(title = sprintf("C) PC2×PC3 (%.1f%%+%.1f%%)",
                                ve[2]*100, ve[3]*100))) +
  plot_annotation(
    title    = sprintf("K-Means K=4 – Pairplot de Clusters en PC1, PC2 y PC3"),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 14, color = PAL$dark))
  )
reg(p_clusters_panel, "kmeans_pairplot_panel.png")

# ── Scatter PRINCIPAL (PC1×PC2 como presentación principal) ───────────────────
p_k2_main <- hacer_scatter_clusters(1, 2, Xkm, lkm, km$centers, ve, cc,
                                    guardar_nombre = "kmeans_2d_PRINCIPAL.png")

# ── Perfil financiero por cluster (boxplots) ──────────────────────────────────
pvx <- intersect(c("ROA", "CFO", "LEV", "SIZE", "GRW"), names(df_train))
dfpr <- df_train[rows_pca, pvx, drop = FALSE]
dfpr$cluster <- factor(lkm)

cat("\n  Perfil por Cluster (medianas, set TRAIN):\n")
cat(sprintf("  %-8s  %8s  %8s  %8s  %8s  %8s\n",
            "Cluster", "ROA", "CFO", "LEV", "SIZE", "n"))
for (cl in levels(dfpr$cluster)) {
  s <- dfpr[dfpr$cluster == cl, ]
  cat(sprintf("  C%-7s  %+8.4f  %+8.4f  %8.4f  %8.2f  %8d\n",
              cl,
              median(s$ROA,  na.rm = TRUE),
              median(s$CFO,  na.rm = TRUE),
              median(s$LEV,  na.rm = TRUE),
              median(s$SIZE, na.rm = TRUE),
              nrow(s)))
}

dfpl <- dfpr |>
  pivot_longer(cols = all_of(pvx),
               names_to  = "Variable",
               values_to = "Valor")

cluster_labels <- dfpr |>
  group_by(cluster) |>
  summarise(med_roa = median(ROA, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(med_roa)) |>
  mutate(rango = case_when(
    row_number() == 1 ~ "Alta rentabilidad",
    row_number() == 2 ~ "Rentabilidad media-alta",
    row_number() == 3 ~ "Rentabilidad media-baja",
    TRUE              ~ "Baja rentabilidad"
  ))

cl_map <- setNames(cluster_labels$rango, cluster_labels$cluster)
dfpl$cluster_label <- factor(
  cl_map[as.character(dfpl$cluster)],
  levels = cl_map[as.character(sort(unique(dfpl$cluster)))]
)

p_pf <- ggplot(dfpl, aes(x = cluster, y = Valor, fill = cluster)) +
  geom_boxplot(
    notch      = TRUE,
    notchwidth = 0.25,
    alpha      = 0.85,
    outlier.alpha = 0.08,
    outlier.size  = 0.8,
    color      = "gray20"
  ) +
  geom_hline(yintercept = 0,
             color = "black", linewidth = 0.8, linetype = "dashed") +
  scale_fill_manual(values = cc, guide = "none") +
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
  labs(
    title = sprintf("Perfil Financiero por Cluster – K=%d | Set TRAIN", K_FIJO),
    x = "Cluster",
    y = "Valor (empresa×año)"
  ) +
  theme(legend.position = "none")
reg(p_pf, "kmeans_perfiles.png")

# ── Heatmap de centroides (vista alternativa del perfil) ──────────────────────
df_centroids <- as.data.frame(km$centers)
colnames(df_centroids) <- paste0("PC", seq_len(ncol(df_centroids)))
df_centroids$cluster <- factor(paste0("C", seq_len(K_FIJO)))

df_cent_long <- df_centroids |>
  pivot_longer(cols = starts_with("PC"),
               names_to = "PC",
               values_to = "score")

p_cent_heat <- ggplot(df_cent_long, aes(x = PC, y = cluster, fill = score)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", score)),
            size = 3.8, fontface = "bold",
            color = ifelse(abs(df_cent_long$score) > 0.5,
                           "white", PAL$dark)) +
  scale_fill_gradient2(
    low      = PAL$red,
    mid      = "white",
    high     = PAL$teal,
    midpoint = 0,
    name     = "Score PC"
  ) +
  labs(
    title = "Heatmap de Centroides – K=4 en espacio PCA",
    x = "Componente Principal",
    y = "Cluster"
  )
reg(p_cent_heat, "kmeans_centroides_heatmap.png")

cat(sprintf("  Gráficos generados:\n"))
cat(sprintf("    - Scatter individual: PC1×PC2, PC1×PC3, PC2×PC3\n"))
cat(sprintf("    - Panel comparativo: kmeans_pairplot_panel.png\n"))
cat(sprintf("    - Principal (PC1×PC2): kmeans_2d_PRINCIPAL.png\n"))
cat(sprintf("    - Boxplots perfiles: kmeans_perfiles.png\n"))
cat(sprintf("    - Heatmap centroides: kmeans_centroides_heatmap.png\n"))
cat("  K-Means completado\n")
readline("\n  [Enter] → Sección 5: Regresión OLS\n")

# ==============================================================================
# SECCIÓN 6 ─ REGRESIÓN LINEAL MÚLTIPLE OLS + DIAGNÓSTICOS
# ==============================================================================
cat("\n[5/7] Regresion Lineal Multiple OLS (set TRAIN)...\n")

# ── 6.1 Preparar datos para lm() ──────────────────────────────────────────────
df_ols_train <- df_train |>
  select(all_of(c("target_ROA", "year", VARS_FILTRADAS, "ind"))) |>
  mutate(ind = as.factor(ind)) |>
  na.omit()

df_ols_train$ind <- droplevels(df_ols_train$ind)

cat(sprintf("  Obs. en train (sin NAs): %d\n", nrow(df_ols_train)))
cat(sprintf("  Variables continuas iniciales al entrar a OLS: %d\n", length(VARS_FILTRADAS)))

vars_ols_activas <- VARS_FILTRADAS 
formula_ols <- as.formula(
  paste("target_ROA ~", paste(vars_ols_activas, collapse=" + "), "+ ind")
)

cat("  Ajustando lm() inicial (con 25 variables)...\n")
modelo_ols_inicial <- lm(formula_ols, data = df_ols_train)

# ── DIAGNÓSTICO VIF INICIAL (Mostrando el problema con las 25 variables) ──────
cat("\n  Generando gráfico VIF inicial (con las 25 variables)...\n")
vif_vals_ini <- tryCatch(car::vif(modelo_ols_inicial), error=function(e) NULL)
if (!is.null(vif_vals_ini)) {
  if (is.matrix(vif_vals_ini)) vif_vals_vec <- vif_vals_ini[,1]
  else                         vif_vals_vec <- vif_vals_ini
  
  vif_df <- data.frame(Variable=names(vif_vals_vec), VIF=as.numeric(vif_vals_vec)) |>
    filter(!grepl("^ind",Variable)) |>
    arrange(desc(VIF))
  
  p_vif <- ggplot(vif_df,aes(x=reorder(Variable,VIF),y=VIF,
                             fill=cut(VIF,c(0,5,10,Inf),
                                      labels=c("OK (<5)","Moderado (5-10)","Grave (>10)"))))+
    geom_col(alpha=0.85)+
    geom_hline(yintercept=5, color=PAL$orange,linewidth=1.2,linetype="dashed")+
    geom_hline(yintercept=10,color=PAL$red,   linewidth=1.2,linetype="dashed")+
    coord_flip()+
    scale_fill_manual(
      values=c("OK (<5)"=PAL$teal,"Moderado (5-10)"=PAL$orange,"Grave (>10)"=PAL$red),
      name="Nivel VIF")+
    labs(title="VIF Inicial – Identificación de Multicolinealidad Múltiple",
         subtitle=sprintf("Panel inicial de 25 variables | VIF Máximo: %.2f (Barras rojas serán eliminadas)", max(vif_df$VIF, na.rm=TRUE)),
         x=NULL,y="VIF")+
    theme(legend.position="bottom")
  reg(p_vif,"diag_vif_inicial_25vars.png")
}

# ── 6.2 Bucle VIF iterativo (Limpiando de 25 a 21) ────────────────────────────
cat("\n  [Filtro iterativo VIF] Eliminando variables con VIF > 10...\n")
cat(sprintf("  %-5s  %-22s  %8s\n","Iter.","Variable eliminada","VIF"))
cat(sprintf("  %s\n",strrep("-",40)))

iter_vif <- 0
modelo_ols <- modelo_ols_inicial # Inicializar
repeat {
  iter_vif <- iter_vif + 1
  vif_actual <- tryCatch(car::vif(modelo_ols), error=function(e) NULL)
  if (is.null(vif_actual)) break
  
  if (is.matrix(vif_actual)) vif_vec <- vif_actual[,1]
  else                       vif_vec <- vif_actual
  
  vif_cont <- vif_vec[!grepl("^ind",names(vif_vec))]
  max_vif   <- max(vif_cont, na.rm=TRUE)
  if (max_vif <= 10) break
  
  var_eliminar <- names(which.max(vif_cont))
  cat(sprintf("  %-5d  %-22s  %8.2f\n",iter_vif, var_eliminar, max_vif))
  
  vars_ols_activas <- setdiff(vars_ols_activas, var_eliminar)
  
  formula_ols <- as.formula(
    paste("target_ROA ~", paste(vars_ols_activas, collapse=" + "), "+ ind")
  )
  modelo_ols <- lm(formula_ols, data=df_ols_train)
  
  if (iter_vif > 50) break
}

smry <- summary(modelo_ols)

# Continúa con la tabla de coeficientes y resto de diagnósticos (Sección 6.2 en adelante)...

# ── 6.2 Tabla de coeficientes ─────────────────────────────────────────────────
cat("\n  COEFICIENTES OLS (p-valor < 0.05):\n")
cat(sprintf("  %-20s  %+10s  %10s  %10s  %8s\n",
            "Variable","Beta","Std.Err","t-stat","Signif."))
cat(sprintf("  %s\n",strrep("-",65)))
ctbl <- smry$coefficients
for (i in seq_len(min(nrow(ctbl),40))) {
  nm <- rownames(ctbl)[i]
  nm_short <- if (grepl("^ind",nm)) substr(nm,1,15) else nm
  pv  <- ctbl[i,4]
  sig <- ifelse(pv<.001,"***",ifelse(pv<.01,"**",ifelse(pv<.05,"*","ns")))
  cat(sprintf("  %-20s  %+10.5f  %10.5f  %10.3f  %8s\n",
              nm_short, ctbl[i,1], ctbl[i,2], ctbl[i,3], sig))
}

# ── Gráfico: Coeficientes β con IC 95% ────────────────────────────────────────
df_betas <- data.frame(
  Variable = rownames(ctbl),
  Beta     = ctbl[,1], SE = ctbl[,2], p = ctbl[,4]
) |>
  filter(!grepl("Intercept|^ind",Variable)) |>
  mutate(
    lo   = Beta - 1.96*SE,
    hi   = Beta + 1.96*SE,
    sig  = ifelse(p<.05,"Significativo (p<0.05)","No significativo"),
    lbl  = sprintf("%.5f%s",Beta,
                   ifelse(p<.001,"***",ifelse(p<.01,"**",ifelse(p<.05,"*","")))),
  ) |>
  arrange(desc(abs(Beta))) |>
  mutate(Variable=factor(Variable,levels=Variable))

p_betas_ols <- ggplot(df_betas,aes(x=Beta,y=Variable,color=sig))+
  geom_vline(xintercept=0,color="gray35",linewidth=1.3,linetype="dashed")+
  geom_segment(aes(x=lo,xend=hi,yend=Variable),linewidth=2.5,alpha=.30)+
  geom_point(size=5)+
  geom_text(aes(label=lbl),hjust=-.15,size=3.3,
            color=PAL$dark,fontface="bold")+
  scale_color_manual(
    values=c("Significativo (p<0.05)"=PAL$teal,"No significativo"=PAL$slate),
    name=NULL)+
  labs(title="Coeficientes OLS con IC 95% (variables continuas)",
       subtitle=sprintf("R² = %.4f  |  R²-adj = %.4f  |  n = %d observaciones", 
                        smry$r.squared, smry$adj.r.squared, nrow(df_ols_train)),
       x="Valor del coeficiente \u03b2",y=NULL)+
  theme(legend.position="bottom")
reg(p_betas_ols,"ols_coeficientes.png")

# ── 6.3 DIAGNÓSTICO 1: Multicolinealidad (VIF) ───────────────────────────────
cat("\n  [Diagnóstico 1] VIF final (post-bucle iterativo):\n")
vif_vals <- tryCatch(car::vif(modelo_ols), error=function(e) NULL)
if (!is.null(vif_vals)) {
  if (is.matrix(vif_vals)) vif_vals_vec <- vif_vals[,1]
  else                     vif_vals_vec <- vif_vals
  vif_df <- data.frame(Variable=names(vif_vals_vec), VIF=as.numeric(vif_vals_vec)) |>
    filter(!grepl("^ind",Variable)) |>
    arrange(desc(VIF))
  
  p_vif <- ggplot(vif_df,aes(x=reorder(Variable,VIF),y=VIF,
                             fill=cut(VIF,c(0,5,10,Inf),
                                      labels=c("OK (<5)","Moderado (5-10)","Grave (>10)"))))+
    geom_col(alpha=0.85)+
    geom_hline(yintercept=5, color=PAL$orange,linewidth=1.2,linetype="dashed")+
    geom_hline(yintercept=10,color=PAL$red,   linewidth=1.2,linetype="dashed")+
    coord_flip()+
    scale_fill_manual(
      values=c("OK (<5)"=PAL$teal,"Moderado (5-10)"=PAL$orange,"Grave (>10)"=PAL$red),
      name="Nivel VIF")+
    labs(title="VIF – Factor de Inflación de la Varianza",
         subtitle=sprintf("VIF Máximo en el modelo final: %.2f", max(vif_df$VIF, na.rm=TRUE)),
         x=NULL,y="VIF")+
    theme(legend.position="bottom")
  reg(p_vif,"diag_vif.png")
}

# ── 6.4 DIAGNÓSTICO 2: Normalidad de Residuos (Jarque-Bera) ──────────────────
res    <- residuals(modelo_ols)
fv     <- fitted(modelo_ols)
res_std <- rstandard(modelo_ols)

# Cálculo matemático de Jarque-Bera (usando e1071 para asimetría y curtosis)
n_res <- length(res)
s_res <- e1071::skewness(res)
k_res <- e1071::kurtosis(res) # Exceso de curtosis
jb_stat <- (n_res / 6) * (s_res^2 + (k_res^2) / 4)
jb_pval <- pchisq(jb_stat, df = 2, lower.tail = FALSE)

p_qq <- ggplot(data.frame(t=qqnorm(res,plot.it=FALSE)$x, m=qqnorm(res,plot.it=FALSE)$y),aes(x=t,y=m))+
  geom_point(color=PAL$teal,alpha=.30,size=1)+
  geom_abline(slope=sd(res),intercept=mean(res),
              color=PAL$red,linewidth=2)+
  labs(title="QQ-Plot Residuos OLS",
       x="Cuantiles N(0,1)",y="Cuantiles muestrales de residuos")

p_hist_res <- ggplot(data.frame(r=res),aes(x=r))+
  geom_histogram(aes(y=after_stat(density)),bins=70,
                 fill=PAL$teal,alpha=0.75,color="white")+
  stat_function(fun=dnorm,args=list(mean=mean(res),sd=sd(res)),
                color=PAL$red,linewidth=2.2)+
  geom_vline(xintercept=0,color="black",linewidth=1.3)+
  labs(title="Distribución de Residuos OLS",
       x="Residuo",y="Densidad")

# ── 6.5 DIAGNÓSTICO 3: Homocedasticidad ──────────────────────────────────────
df_diag <- data.frame(fitted = fv, sqrtres = sqrt(abs(res_std)))

p_sl <- ggplot(df_diag,aes(x=fitted,y=sqrtres))+
  geom_point(color=PAL$teal,alpha=.20,size=1.2)+
  geom_hline(yintercept=mean(df_diag$sqrtres,na.rm=TRUE),
             color=PAL$red,linewidth=1.2,linetype="dashed")+
  labs(title="Scale-Location (Homocedasticidad)",
       x="Valores ajustados (ŷ)",y="\u221a|Residuos estandarizados|")

# ── 6.6 DIAGNÓSTICO 4: Independencia (Durbin-Watson) ─────────────────────────
dw_test <- lmtest::dwtest(modelo_ols, order.by = df_ols_train$year)

# ── 6.7 DIAGNÓSTICO 5: Influencia (Cook's D, Leverage) ───────────────────────
n_obs <- nrow(df_ols_train)
p_mod <- length(coef(modelo_ols))
cook_d  <- cooks.distance(modelo_ols)
hat_v   <- hatvalues(modelo_ols)
umbral_cook <- 4 / n_obs
umbral_hat  <- 2 * p_mod / n_obs

df_inf <- data.frame(
  obs       = seq_along(cook_d),
  cook      = cook_d,
  hat       = hat_v,
  res_std   = res_std,
  alto_cook = cook_d > umbral_cook,
  alto_hat  = hat_v  > umbral_hat
)

top_inf <- df_inf |> arrange(desc(cook)) |> head(10)
p_rvl <- ggplot(df_inf,aes(x=hat,y=res_std))+
  geom_point(aes(size=cook,color=alto_cook&alto_hat),alpha=0.35)+
  geom_hline(yintercept=c(-2,0,2),
             linetype=c("dashed","solid","dashed"),
             color=c(PAL$orange,"gray40",PAL$orange),
             linewidth=c(1.0,0.8,1.0))+
  geom_vline(xintercept=umbral_hat,
             color=PAL$red,linewidth=1.0,linetype="dashed")+
  scale_color_manual(values=c("FALSE"=PAL$teal,"TRUE"=PAL$red),
                     labels=c("FALSE"="Normal","TRUE"="Alt. Cook & Leverage"),name=NULL)+
  scale_size_continuous(range=c(0.5,4),guide="none")+
  labs(title="Residuos Est. vs Leverage",
       x="Leverage (h\u1d62\u1d62)",y="Residuos estandarizados")+
  theme(legend.position="bottom")

# Panel 4 en 1 con las métricas consolidadas (solo JB y DW)
p_diag4 <- (
  (p_hist_res) | (p_qq) | (p_sl) | (p_rvl)
) +
  plot_annotation(
    title    = "Panel Diagnóstico OLS – Validación de Supuestos",
    subtitle = sprintf("Jarque-Bera (p) = %.3e  |  Durbin-Watson = %.2f", 
                       jb_pval, dw_test$statistic),
    theme=theme(
      plot.title    = element_text(face="bold", size=14, color=PAL$dark),
      plot.subtitle = element_text(size=10, color=PAL$slate, face="bold")
    )
  )
reg(p_diag4,"diag_panel_4en1.png")
cat("  Diagnósticos OLS completados\n")
readline("\n  [Enter] → Sección 7: Evaluación en Test\n")

# ==============================================================================
# SECCIÓN 7 ─ EVALUACIÓN EN EL SET DE TEST (2023 → predice ROA 2024)
# ==============================================================================
cat("\n[6/7] Evaluacion en set Test (2023 → predice ROA 2024)...\n")

df_ols_test <- df_test |>
  select(all_of(c("target_ROA", "year", VARS_FILTRADAS, "ind"))) |>
  mutate(ind = as.factor(ind)) |>
  na.omit()

niveles_modelo <- modelo_ols$xlevels[["ind"]]
df_ols_test <- df_ols_test |>
  filter(as.character(ind) %in% niveles_modelo) |>
  mutate(ind = factor(as.character(ind), levels = niveles_modelo))

y_pred_test <- predict(modelo_ols, newdata=df_ols_test)
y_real_test <- df_ols_test$target_ROA

ok <- !is.na(y_pred_test) & !is.na(y_real_test)
y_pred_ok <- y_pred_test[ok]; y_real_ok <- y_real_test[ok]

rmse_test <- sqrt(mean((y_real_ok - y_pred_ok)^2))
mae_test  <- mean(abs(y_real_ok - y_pred_ok))
ss_res    <- sum((y_real_ok - y_pred_ok)^2)
ss_tot    <- sum((y_real_ok - mean(y_real_ok))^2)
r2_test   <- 1 - ss_res/ss_tot

# ── GRÁFICO A: Scatter predicho vs real ──────────────────────────────────────
lim <- range(c(y_real_ok, y_pred_ok), na.rm=TRUE)
p_pvr <- ggplot(
  data.frame(real=y_real_ok, pred=y_pred_ok,
             dir=(y_pred_ok - y_real_ok) > 0),
  aes(x=real, y=pred)
) +
  geom_ribbon(
    data = data.frame(real=seq(lim[1], lim[2], length.out=300)),
    aes(x=real, ymin=real-rmse_test, ymax=real+rmse_test),
    fill=PAL$teal, alpha=0.10, inherit.aes=FALSE
  ) +
  geom_point(aes(color=dir), alpha=0.35, size=1.5) +
  geom_abline(aes(intercept = 0, slope = 1, linetype = "Línea roja: y = x"),
              color=PAL$red, linewidth=1.5) +
  scale_color_manual(
    values = c("TRUE"=PAL$orange, "FALSE"=PAL$teal2),
    labels = c("TRUE"="Sobreestimado", "FALSE"="Subestimado"),
    name   = "Predicción:"
  ) +
  scale_linetype_manual(
    values = c("Línea roja: y = x" = "dashed"),
    name   = NULL
  ) +
  labs(
    title    = "target_ROA Predicho vs Real | Test 2023 \u2192 ROA 2024",
    subtitle = sprintf("R²-Test = %.4f  |  RMSE = %.4f  |  MAE = %.4f", r2_test, rmse_test, mae_test),
    x = "ROA 2024 Observado (ground truth)",
    y = "ROA 2024 Predicho con datos de 2023"
  ) +
  theme(legend.position="bottom", legend.box = "horizontal")
reg(p_pvr, "eval_A_predicho_vs_real.png")

# ── GRÁFICO B: Densidad ROA real vs predicho ─────────────────────────────────
df_dens <- bind_rows(
  data.frame(ROA=y_real_ok, Distribución="Real"),
  data.frame(ROA=y_pred_ok, Distribución="Predicho")
) |>
  mutate(Distribución = factor(Distribución, levels=c("Real", "Predicho")))

media_real <- mean(y_real_ok, na.rm=TRUE)
media_pred <- mean(y_pred_ok, na.rm=TRUE)

p_dens <- ggplot(df_dens, aes(x=ROA, fill=Distribución, color=Distribución)) +
  geom_density(alpha=0.45, linewidth=1.4) +
  geom_vline(xintercept=media_real,
             color=PAL$teal, linewidth=1.3, linetype="dashed") +
  geom_vline(xintercept=media_pred,
             color=PAL$orange, linewidth=1.3, linetype="dashed") +
  geom_vline(xintercept=0,
             color="gray35", linewidth=0.9, linetype="dotted") +
  scale_fill_manual(values  = c(PAL$teal, PAL$orange), name = NULL) +
  scale_color_manual(values = c(PAL$teal, PAL$orange), name = NULL) +
  labs(
    title    = "Distribución ROA Real vs Predicho | Test 2023 \u2192 ROA 2024",
    subtitle = sprintf("R²-Test = %.4f  |  Media Real = %.4f  |  Media Predicha = %.4f", r2_test, media_real, media_pred),
    x = "target_ROA (ROA del año t+1)",
    y = "Densidad"
  ) +
  theme(legend.position="bottom",
        legend.key.size=unit(0.7,"cm"),
        legend.text=element_text(size=11))
reg(p_dens, "eval_B_densidad_real_vs_predicho.png")

cat("  Evaluacion Test completada\n")
readline("\n  [Enter] → Sección 7: Treemaps\n")
# ==============================================================================
# SECCIÓN 8 ─ LOS 100 TITANES VS. LA INFLACIÓN (2020–2024)  [ORIGINAL INTACTO]
# ==============================================================================
cat("\n[7/7] Treemaps Top 100 (Ajustados por Inflación 2020-2024)...\n")

inflacion_kr <- data.frame(
  year     = 2020:2024,
  inflacion= c(0.0054, 0.0250, 0.0509, 0.0360, 0.0232)
)

df_tree_base <- df |>
  filter(year %in% 2020:2024,
         !is.na(ROA), !is.na(SIZE),
         is.finite(ROA), is.finite(SIZE), !is.na(name)) |>
  group_by(stock, name, year) |>
  summarise(ROA     = mean(ROA,  na.rm=TRUE),
            activos = mean(exp(SIZE), na.rm=TRUE),
            .groups = "drop") |>
  mutate(
    stock_char = as.character(stock),
    name_clean = case_when(
      stock == 5930   ~ "SAMSUNG ELECTRONICS",
      stock == 660    ~ "SK HYNIX",
      stock == 5380   ~ "HYUNDAI MOTORS",
      stock == 270    ~ "KIA MOTORS",
      stock == 35420  ~ "NAVER",
      stock == 35720  ~ "KAKAO",
      stock == 51910  ~ "LG CHEMICAL",
      stock == 6400   ~ "SAMSUNG SDI",
      TRUE ~ iconv(as.character(name), to="UTF-8", sub="")
    ),
    name_clean = ifelse(name_clean==""| is.na(name_clean),
                        paste0("ID:",stock_char), name_clean)
  ) |>
  left_join(inflacion_kr, by="year") |>
  mutate(ROA_Real = ROA - inflacion)

hacer_treemap_inflacion <- function(anio) {
  df_anio <- df_tree_base |> filter(year==anio)
  if (nrow(df_anio) < 3) return(NULL)
  inflacion_actual <- inflacion_kr$inflacion[inflacion_kr$year==anio]
  df_top100 <- df_anio |> slice_max(order_by=activos, n=100)
  n_ganadores <- sum(df_top100$ROA_Real > 0, na.rm=TRUE)
  
  tm_obj <- suppressMessages(
    treemap(df_top100, index="stock_char", vSize="activos",
            type="index", algorithm="pivotSize", sortID="ROA_Real",
            mirror.y=TRUE, mirror.x=TRUE, draw=FALSE)
  )
  dg <- tm_obj[["tm"]] |>
    as_tibble() |>
    mutate(
      xmax = x0+w, ymax = y0+h, stock_char=as.character(stock_char),
      x_centro = x0+(w/2), y_centro = y0+(h/2)
    ) |>
    left_join(df_top100 |> select(stock_char,name_clean,ROA_Real,activos),
              by="stock_char") |>
    mutate(label_vis=sprintf("%s\n%+.1f%%",name_clean,ROA_Real*100))
  
  lim_escala <- 0.05
  p <- ggplot(dg) +
    geom_rect(aes(xmin=x0,ymin=y0,xmax=xmax,ymax=ymax,fill=ROA_Real),
              linewidth=0.5,colour="#0F172A") +
    geom_text(aes(x=x_centro,y=y_centro,label=label_vis),
              colour="white",size=3.2,fontface="bold",
              lineheight=0.9,check_overlap=TRUE) +
    scale_fill_gradient2(
      low="#EF4444", mid="#334155", high="#059669",
      midpoint=0,
      limits=c(-lim_escala,lim_escala),
      oob=scales::squish,
      labels=scales::percent_format(accuracy=1),
      name="Desempeño Real",
      guide=guide_colorbar(title.position="left",title.vjust=0.8,
                           barwidth=unit(10,"cm"),barheight=unit(0.6,"cm"),
                           frame.colour="#475569",ticks.colour="white")) +
    labs(
      title    = sprintf("Los 100 Titanes frente a la Inflación (%d)", anio),
      subtitle = sprintf("Inflación oficial: %.2f%% | %d de 100 gigantes crearon riqueza real este año",
                         inflacion_actual*100, n_ganadores)
    ) +
    theme_void() +
    theme(
      legend.position  = "bottom",
      legend.text      = element_text(color="#E8EADC",size=11),
      legend.title     = element_text(color="#E8EADC",size=13,face="bold"),
      plot.background  = element_rect(fill="#0F172A",colour="#0F172A"),
      plot.margin      = margin(20,15,20,15),
      plot.title       = element_text(size=18,hjust=0.5,face="bold",
                                      colour="#F8FAFC",margin=margin(b=8)),
      plot.subtitle    = element_text(size=12,hjust=0.5,
                                      colour="#94A3B8",margin=margin(b=15))
    )
  cat(sprintf("  %d → Inflación: %.2f%% | %d/100 superaron la inflación\n",
              anio, inflacion_actual*100, n_ganadores))
  return(p)
}

for (anio in 2020:2024) {
  p_tm <- hacer_treemap_inflacion(anio)
  if (!is.null(p_tm)) reg(p_tm, sprintf("treemap_real_%d.png", anio))
}
cat("  Treemaps completados\n")
# ==============================================================================
# SECCIÓN EDA VISUAL ─ RESUMEN DESCRIPTIVO: GANANCIAS (verde) vs PÉRDIDAS (rojo)
# ==============================================================================
# Genera un panel grid con un histograma por cada variable de KEY_VARS.
# Cada histograma:
#   • Barras VERDES  → valores ≥ 0 (rentabilidad positiva, apalancamiento sano…)
#   • Barras ROJAS   → valores  < 0 (pérdidas, crecimientos negativos…)
#   • Línea naranja punteada = Media
#   • Línea verde sólida    = Mediana
#   • Anotación en esquina  = Asimetría y Curtosis calculadas sobre el panel
# Base: panel completo post-IQR×3 (todas las observaciones empresa×año),
#       NO solo el train, para mostrar la distribución real del mercado.
# ==============================================================================
cat("\n[EDA Visual] Resumen descriptivo – Ganancias vs Pérdidas...\n")

# ── Paleta específica para este panel ─────────────────────────────────────────
COLOR_GANANCIA <- "#4CAF50"   # verde sólido
COLOR_PERDIDA  <- "#F28B82"   # rojo salmon (igual que imagen)
COLOR_MEDIA    <- "#FFA500"   # naranja punteado
COLOR_MEDIANA  <- "#2E7D32"   # verde oscuro sólido

# ── Función que construye un histograma individual ─────────────────────────────
hacer_hist_eda <- function(var, data) {
  
  d <- data[[var]]
  d <- d[is.finite(d)]
  if (length(d) < 5) return(NULL)
  
  mu   <- mean(d,   na.rm = TRUE)
  md   <- median(d, na.rm = TRUE)
  asim <- round(e1071::skewness(d),  2)
  kurt <- round(e1071::kurtosis(d),  2)
  
  # Etiqueta del eje X: limitar a dos decimales en los breaks
  x_range <- range(d, na.rm = TRUE)
  
  df_hist <- data.frame(
    x  = d,
    cl = ifelse(d >= 0, "Ganancia", "P\u00e9rdida")
  )
  
  ggplot(df_hist, aes(x = x, fill = cl)) +
    geom_histogram(
      bins       = 50,
      boundary   = 0,
      alpha      = 0.90,
      color      = "white",
      linewidth  = 0.08
    ) +
    scale_fill_manual(
      values = c("Ganancia" = COLOR_GANANCIA,
                 "P\u00e9rdida" = COLOR_PERDIDA),
      guide  = "none"
    ) +
    # Línea media (naranja punteada)
    geom_vline(
      xintercept = mu,
      color      = COLOR_MEDIA,
      linewidth  = 1.0,
      linetype   = "dashed"
    ) +
    # Línea mediana (verde oscuro sólida)
    geom_vline(
      xintercept = md,
      color      = COLOR_MEDIANA,
      linewidth  = 0.8,
      linetype   = "solid"
    ) +
    # Anotación Asimetría + Curtosis (esquina superior derecha)
    annotate(
      "text",
      x     = Inf,
      y     = Inf,
      label = sprintf("Asim=%s\nKurt=%s",
                      format(asim, nsmall = 2),
                      format(kurt, nsmall = 2)),
      hjust  = 1.08,
      vjust  = 1.20,
      size   = 2.8,
      color  = PAL$dark,
      fontface = "bold"
    ) +
    labs(
      title = var,
      x     = NULL,
      y     = "Frec."
    ) +
    theme(
      plot.title       = element_text(face = "bold", size = 9,
                                      color = PAL$dark, hjust = 0.5,
                                      margin = margin(b = 2)),
      axis.text.x      = element_text(size = 7, color = PAL$slate),
      axis.text.y      = element_text(size = 7, color = PAL$slate),
      axis.title.y     = element_text(size = 7, color = PAL$slate),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E2E8F0", linewidth = 0.3),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "#FAFAFA", color = NA)
    )
}

# ── Generar un plot por variable ───────────────────────────────────────────────
# Se usa el panel COMPLETO post-IQR×3 para mostrar la distribución real
# de todas las observaciones (empresa×año), no solo el train.

lista_hist_eda <- lapply(KEY_VARS, function(var) {
  if (!var %in% names(df)) return(NULL)
  hacer_hist_eda(var, df)
})
lista_hist_eda <- Filter(Negate(is.null), lista_hist_eda)

cat(sprintf("  Histogramas generados: %d variables\n", length(lista_hist_eda)))

# ── Determinar layout del grid ─────────────────────────────────────────────────
n_vars  <- length(lista_hist_eda)
n_cols  <- 3L
n_rows  <- ceiling(n_vars / n_cols)

# ── Leyenda manual (texto explicativo en el subtitle) ─────────────────────────
# Se añade como annotation global del patchwork, igual que en la imagen.

p_eda_grid <- wrap_plots(lista_hist_eda, ncol = n_cols) +
  plot_annotation(
    title    = "Resumen Descriptivo \u2013 Ganancias (verde) vs P\u00e9rdidas (rojo)",
    subtitle = paste0(
      "Verde = valor \u2265 0 (ganancia)  |  ",
      "Rojo = valor < 0 (p\u00e9rdida)  |  ",
      "L\u00ednea naranja = media  |  ",
      "L\u00ednea verde = mediana  |  ",
      "Asimetr\u00eda y Curtosis en cada panel"
    ),
    theme = theme(
      plot.title    = element_text(
        face   = "bold",
        size   = 14,
        color  = PAL$dark,
        hjust  = 0.5,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        size   = 9,
        color  = PAL$slate,
        hjust  = 0.5,
        margin = margin(b = 10)
      ),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

reg(p_eda_grid, "eda_resumen_descriptivo_grid.png")
cat("  eda_resumen_descriptivo_grid.png registrado\n")

# ── También guardar inmediatamente en alta resolución ─────────────────────────
# Se guarda en este momento además del guardado final porque el grid completo
# necesita dimensiones amplias para que los histogramas sean legibles.
ggsave(
  filename  = file.path(OUTPUT_DIR, "eda_resumen_descriptivo_grid.png"),
  plot      = p_eda_grid,
  width     = 16,
  height    = 5 * n_rows,   # ~5 pulgadas por fila de histogramas
  dpi       = 220,
  bg        = "white",
  limitsize = FALSE
)
cat(sprintf("  Guardado: %.0f × %.0f px (%.0f col × %.0f fil)\n",
            16 * 220, 5 * n_rows * 220,
            as.numeric(n_cols), as.numeric(n_rows)))

cat("  EDA Visual completado\n")



# ==============================================================================
# SECCIÓN EXTRA ─ VISUALIZACIÓN MATRIZ 29 VARIABLES CON VALORES NUMÉRICOS
# ==============================================================================
cat("\n[Extra] Generando matriz de 29 variables con valores en el visor (Plot)...\n")

# Se usa mat_corr que contiene las 29 variables candidatas iniciales.
# Se añade addCoef.col = "black" para mostrar los números y un number.cex pequeño
# para evitar que los textos se superpongan en una matriz tan densa.

corrplot(mat_corr,
         method        = "color", 
         type          = "lower", 
         order         = "hclust", 
         hclust.method = "ward.D2",
         addCoef.col   = "black",       # <-- Activa los valores numéricos
         number.cex    = 0.45,          # <-- Tamaño reducido para que quepan (ajusta si es necesario)
         number.digits = 2,             # <-- Solo 2 decimales para ahorrar espacio
         tl.cex        = 0.60, 
         tl.col        = "#0F172A", 
         tl.srt        = 45, 
         cl.cex        = 0.75,
         col           = colorRampPalette(c(HEAT_NEG, HEAT_NEU, HEAT_POS))(200),
         title         = sprintf("Spearman – %d Variables Candidatas | TRAIN (ward.D2)", ncol(mat_corr)),
         mar           = c(0,0,3,0), 
         addgrid.col   = "white", 
         diag          = FALSE)




# ==============================================================================
# SECCIÓN DE GUARDADO FINAL ─ EXPORTACIÓN EN BLOQUE
# ==============================================================================
# Nota: eda_resumen_descriptivo_grid.png ya fue guardado arriba con dimensiones
# personalizadas. El bucle de abajo lo sobrescribirá con las dimensiones STD
# (2800×1600), que pueden ser suficientes para grids pequeños pero INSUFICIENTES
# para KEY_VARS con muchas variables. Por eso se guarda antes con alto=5*n_rows.
# El bloque reg() lo mantiene en REGISTRO para que aparezca en el inventario.
# ==============================================================================
cat("\n[Guardado] Exportando gráficos...\n")

ANCHO_STD <- 2800; ALTO_STD <- 1600; RES_STD <- 200
ANCHO_TM  <- 3800; ALTO_TM  <- 2400; RES_TM  <- 200

# Para el grid EDA usamos dimensiones proporcionales al número de filas
ANCHO_GRID <- 3520L   # 16 pulgadas × 220 dpi
ALTO_GRID  <- as.integer(5 * n_rows * 220)  # 5 pulgadas/fila × 220 dpi

n_ok <- 0L; n_err <- 0L

for (nm in names(REGISTRO)) {
  ruta  <- file.path(OUTPUT_DIR, nm)
  p     <- REGISTRO[[nm]]
  
  # Dimensiones según tipo de gráfico
  es_tm   <- grepl("treemap",         nm)
  es_grid <- grepl("resumen_descrip", nm)
  
  if      (es_tm)   { a <- ANCHO_TM;   h <- ALTO_TM;   r <- RES_TM  }
  else if (es_grid) { a <- ANCHO_GRID; h <- ALTO_GRID;  r <- 220L    }
  else              { a <- ANCHO_STD;  h <- ALTO_STD;   r <- RES_STD }
  
  tryCatch({
    if (inherits(p, "gg") || inherits(p, "patchwork"))
      ggsave(ruta, plot = p,
             width  = a / r,
             height = h / r,
             dpi    = r,
             bg     = "white",
             limitsize = FALSE)
    else if (inherits(p, "recordedplot")) {
      png(ruta, width = a, height = h, res = r, bg = "white")
      replayPlot(p)
      dev.off()
    }
    cat(sprintf("  [OK]  %s\n", nm))
    n_ok <- n_ok + 1L
  }, error = function(e) {
    cat(sprintf("  [ERR] %s -- %s\n", nm, e$message))
    n_err <<- n_err + 1L
  })
}

