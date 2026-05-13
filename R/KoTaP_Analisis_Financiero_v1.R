# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║         SCRIPT KoTaP v5.0 – Análisis Financiero Mercado Coreano            ║
# ║         Cambios v5.0:                                                       ║
# ║           • Justificación explícita de Asimetría y Curtosis                ║
# ║           • Triángulo superior eliminado de matrices (no NA, no renderiza) ║
# ║           • Muescas (notch) muy visibles: pinch dramático + flecha roja    ║
# ║           • Árbol de Decisión eliminado del pipeline                       ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ==============================================================================
# SECCIÓN 0 ─ PAQUETES
# ==============================================================================
paquetes <- c(
  "e1071", "tidyverse", "patchwork",
  "cluster", "factoextra", "scales", "ggrepel",
  "jsonlite", "data.table", "tseries"
)
instalar_faltantes <- function(pkgs) {
  f <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
  if (length(f) > 0) install.packages(f, dependencies = TRUE)
}
instalar_faltantes(paquetes)

suppressPackageStartupMessages({
  library(e1071); library(tidyverse); library(patchwork)
  library(cluster); library(factoextra)
  library(scales);  library(ggrepel)
  library(jsonlite); library(data.table); library(tseries)
})

DATA_PATH  <- r"(C:\Users\norba\Downloads\KoTaP_Dataset.csv)"
OUTPUT_DIR <- "output_kotap_v4"
dir.create(OUTPUT_DIR, showWarnings = FALSE)

# ── Paleta corporativa ─────────────────────────────────────────────────────────
PAL <- list(
  dark="#0F172A", navy="#1E3A5F", teal="#0D9488", teal2="#0891B2",
  orange="#F97316", green="#059669", red="#EF4444", slate="#475569",
  white="#FFFFFF", purple="#7C3AED", pink="#DB2777",
  amber="#D97706",  rose="#E11D48"
)

# ── Paleta heatmap: rojo–blanco–azul (alta legibilidad en R)  ─────────────────
# Rojo    = correlación positiva fuerte
# Blanco  = sin correlación (~0)
# Azul    = correlación negativa fuerte
HEAT_POS <- "#C0392B"   # rojo oscuro  (positiva)
HEAT_NEU <- "#FFFFFF"   # blanco puro  (cero)
HEAT_NEG <- "#1D4E89"   # azul oscuro  (negativa)

# Función de texto automático (negro sobre claro, blanco sobre oscuro)
auto_txt <- function(hex) {
  r <- strtoi(substr(hex,2,3),16)/255
  g <- strtoi(substr(hex,4,5),16)/255
  b <- strtoi(substr(hex,6,7),16)/255
  ifelse(0.299*r + 0.587*g + 0.114*b > 0.50, "#0F172A", "#FFFFFF")
}

# ── Tema global ────────────────────────────────────────────────────────────────
tema_k <- theme_minimal(base_size=12) +
  theme(
    plot.background  = element_rect(fill="white",   color=NA),
    panel.background = element_rect(fill="#F8FAFC", color=NA),
    panel.grid.major = element_line(color="#E2E8F0", linewidth=0.4),
    panel.grid.minor = element_line(color="#E2E8F0", linewidth=0.2),
    plot.title       = element_text(face="bold", color=PAL$dark, size=13),
    plot.subtitle    = element_text(color=PAL$slate, size=9.5),
    axis.text        = element_text(color=PAL$slate),
    axis.title       = element_text(color=PAL$dark),
    legend.background = element_rect(fill="white", color="#CBD5E1"),
    strip.background  = element_rect(fill="#EFF6FF"),
    strip.text        = element_text(face="bold", color=PAL$dark)
  )
theme_set(tema_k)

REGISTRO <- list()
reg <- function(p, nm) { print(p); REGISTRO[[nm]] <<- p; invisible(p) }

set.seed(42)
cat(strrep("=",70),"\n  KoTaP v4.0\n",strrep("=",70),"\n")


# ==============================================================================
# SECCIÓN 1 ─ CARGA Y LIMPIEZA
# ==============================================================================
cat("\n[1/6] Carga y limpieza...\n")
df_raw <- NULL
for (enc in c("unknown","CP949","UTF-8-BOM","latin1")) {
  ok <- tryCatch({
    tmp <- if (enc=="unknown")
      data.table::fread(DATA_PATH, encoding=enc, data.table=FALSE)
    else
      read.csv(DATA_PATH, stringsAsFactors=FALSE, fileEncoding=enc)
    if (nrow(tmp)>0){ df_raw <- tmp; TRUE } else FALSE
  }, error=function(e) FALSE, warning=function(w) !is.null(df_raw)&&nrow(df_raw)>0)
  if (isTRUE(ok) && nrow(df_raw)>0){ cat(sprintf("  OK encoding='%s'\n",enc)); break }
}
if (is.null(df_raw)||nrow(df_raw)==0)
  stop("No se cargo el CSV. Guarda el archivo como UTF-8 en Excel.")

for(v in c("KOSPI","big4","ind","LOSS","fiscal"))
  if(v %in% names(df_raw)) df_raw[[v]] <- as.factor(df_raw[[v]])

KEY_VARS <- c("SIZE","LEV","ROA","ROE","CFO","GRW","CUR","INVREC","MB","TQ","PPE","AGE")
TARGET   <- "ROA"
FEATURES <- c("SIZE","LEV","CFO","GRW","CUR","INVREC","PPE","AGE")

for(col in KEY_VARS)
  if(col %in% names(df_raw))
    df_raw[[col]] <- suppressWarnings(as.numeric(df_raw[[col]]))
df_raw <- df_raw[!duplicated(df_raw),]

winsor <- function(x, lo=.01, hi=.99){
  q <- quantile(x,probs=c(lo,hi),na.rm=TRUE)
  pmax(pmin(x,q[2]),q[1])
}
df <- df_raw
for(col in KEY_VARS)
  if(col %in% names(df) && is.numeric(df[[col]]))
    df[[col]] <- winsor(df[[col]])

yr_min <- as.integer(min(df$year,na.rm=TRUE))
yr_max <- as.integer(max(df$year,na.rm=TRUE))
n_emp  <- length(unique(df$stock))
cat(sprintf("  Dataset: %d filas | %d empresas | %d-%d\n",nrow(df),n_emp,yr_min,yr_max))

df_model <- df[,c(TARGET,FEATURES)] |> na.omit()
if(nrow(df_model)<20) stop("df_model muy pequeño, revisa columnas.")

set.seed(42)
idx      <- sample(nrow(df_model), floor(.8*nrow(df_model)))
train_df <- df_model[idx,];   test_df <- df_model[-idx,]
y_train  <- train_df[[TARGET]]; y_test  <- test_df[[TARGET]]

cat(sprintf("  Train: %d | Test: %d\n",nrow(train_df),nrow(test_df)))
readline("\n  [Enter] → Sección 2: Estadística Descriptiva\n")


# ==============================================================================
# SECCIÓN 2 ─ ESTADÍSTICA DESCRIPTIVA
# ==============================================================================
# ── ¿Por qué incluimos Asimetría (Skewness) y Curtosis (Kurtosis)? ───────────
#
#   Las medidas clásicas de variabilidad (media, desviación estándar) asumen
#   que la distribución es aproximadamente SIMÉTRICA y con colas NORMALES.
#   En datos financieros esto casi nunca se cumple. Por eso añadimos:
#
#   ASIMETRÍA (Skewness):
#     Mide si la distribución "jala" más hacia la derecha o la izquierda.
#     • Asim > 0 → cola derecha larga: pocas empresas con valores muy altos
#                  (ej: ROA alto solo en minorías muy rentables)
#     • Asim < 0 → cola izquierda larga: pocas empresas con pérdidas extremas
#     • |Asim| > 1 → asimetría severa; la media es un mal representante
#     Sin este coeficiente, no sabemos si μ y σ están siendo distorsionados
#     por valores extremos en un solo lado de la distribución.
#
#   CURTOSIS (Kurtosis):
#     Mide el "peso" de las colas respecto a la distribución normal.
#     • Kurt > 0 → colas más pesadas (leptocúrtica): hay más observaciones
#                  extremas de lo esperado → mayor riesgo de outliers
#     • Kurt < 0 → colas más livianas (platicúrtica): distribución más plana
#     • Kurt > 3 es señal de alerta para normalidad
#     Sin este coeficiente, dos variables con la misma σ pueden tener perfiles
#     de riesgo completamente distintos si una tiene colas pesadas y la otra no.
#
#   CONCLUSIÓN: Asimetría y Curtosis son indispensables para entender la forma
#   real de la distribución. En el histograma se ven en la anotación del panel
#   derecho; en el QQ-Plot, desviaciones de la línea confirman lo que dicen.
# ─────────────────────────────────────────────────────────────────────────────
cat("\n[2/5] Estadística Descriptiva...\n")

VAR_COL <- c(ROA="#0D9488",ROE="#0891B2",SIZE="#1E3A5F",LEV="#F97316",
             CFO="#7C3AED",GRW="#059669",CUR="#DC2626",INVREC="#0369A1",
             MB="#92400E",TQ="#065F46",PPE="#4C1D95",AGE="#831843")

panel_desc <- function(var, data, color){
  d   <- data[[var]]; d <- d[is.finite(d)]
  if(length(d)<5) return(NULL)
  pos <- sum(d>=0); neg <- sum(d<0)
  pct <- round(pos/length(d)*100,1)
  mu  <- mean(d); md <- median(d); sg <- sd(d)
  asim<- round(e1071::skewness(d),3)
  kurt<- round(e1071::kurtosis(d),3)
  cv  <- round(sg/abs(mu)*100,2)
  df_p<- data.frame(x=d, s=ifelse(d>=0,">=0 (ganancia)","<0 (perdida)"))
  
  # Histograma
  ph <- ggplot(df_p, aes(x=x, fill=s)) +
    geom_histogram(bins=60, alpha=.80, color="white", linewidth=.15) +
    scale_fill_manual(values=c(">=0 (ganancia)"=PAL$green,"<0 (perdida)"=PAL$red),name=NULL)+
    geom_vline(xintercept=0,  color="black",   linewidth=1.5)+
    geom_vline(xintercept=mu, color=PAL$amber, linewidth=1.3, linetype="dashed")+
    geom_vline(xintercept=md, color=PAL$navy,  linewidth=1.3, linetype="dotted")+
    annotate("label", x=Inf, y=Inf,
             label=sprintf("Media   = %.4f\nMediana = %.4f\nDesv.   = %.4f\nCV      = %.2f%%\nAsim    = %.3f\nKurt    = %.3f\nn       = %s",
                           mu,md,sg,cv,asim,kurt,format(length(d),big.mark=",")),
             hjust=1.05, vjust=1.05, size=2.9, fill="#FFFDE7",
             color=PAL$dark, label.size=.3, family="mono")+
    annotate("text",
             x=if(is.finite(max(d))&&max(d)!=0) max(d)*.55 else 1,
             y=Inf,
             label=sprintf("Gan: %.1f%%\nPerd: %.1f%%",pct,100-pct),
             vjust=1.6, size=3.5, color=PAL$dark, fontface="bold")+
    labs(title=paste(var,"– Distribución"), x=var, y="Frecuencia")+
    theme(legend.position="bottom", legend.key.size=unit(.4,"cm"))
  
  # ── Boxplot con MUESCAS VISIBLES (notch=TRUE) ─────────────────────────────
  # ════════════════════════════════════════════════════════════════════════════
  # CONFIRMACIÓN: las muescas (cuñas) SÍ están implementadas y son visibles.
  #
  # ¿Qué es una muesca (notch)?
  #   Es el estrechamiento en forma de "cintura" que aparece alrededor de la
  #   línea de la mediana. Representa el intervalo de confianza al 95% de la
  #   mediana. Cuanto más estrecha la cintura, más precisa es la estimación.
  #
  # Parámetros para máxima visibilidad:
  #   notch      = TRUE   → activa la cuña
  #   notchwidth = 0.25   → la caja se estrecha al 25% de su ancho en la
  #                         mediana → "cintura" muy dramática y clara
  #   color      = rose   → borde contrastante que resalta la forma de cuña
  #
  # La flecha roja anotada apunta DIRECTAMENTE a la cuña con la etiqueta
  # "← MUESCA" para que no haya ambigüedad sobre dónde está.
  # ════════════════════════════════════════════════════════════════════════════
  yhi <- if(is.finite(max(d))) max(d)*1.1 else 1
  ylo <- if(is.finite(min(d))) min(d)*1.1 else -1
  
  # Calcular límites de la muesca para anotación (fórmula estándar de Tukey)
  notch_semi <- 1.58 * IQR(d) / sqrt(length(d))
  notch_lo   <- md - notch_semi
  notch_hi   <- md + notch_semi
  
  pb <- ggplot(df_p, aes(x="", y=x)) +
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=0,   ymax=yhi, fill=PAL$green, alpha=.07)+
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=ylo, ymax=0,   fill=PAL$red,   alpha=.07)+
    geom_boxplot(
      fill       = color,
      alpha      = 0.60,
      color      = PAL$rose,      # borde rosa-rojo: resalta la silueta de la cuña
      linewidth  = 1.1,           # borde más grueso para ver mejor la cintura
      notch      = TRUE,          # ACTIVA LA CUÑA/MUESCA
      notchwidth = 0.25,          # 0.25 = cintura muy estrecha → cuña dramática y visible
      outlier.alpha = .2, outlier.size=1.5, outlier.color=color
    )+
    # Línea horizontal en 0 (break-even)
    geom_hline(yintercept=0, color="black", linewidth=1.6, linetype="dashed")+
    # ── Flecha roja que apunta DIRECTAMENTE a la muesca ─────────────────────
    # Esta flecha elimina toda ambigüedad: la cuña está justo donde apunta
    annotate("segment",
             x    = 0.62, xend = 0.82,
             y    = notch_hi + (yhi - notch_hi)*0.15,
             yend = notch_hi,
             arrow= arrow(length=unit(0.22,"cm"), type="closed"),
             color= PAL$rose, linewidth=1.4)+
    annotate("label",
             x    = 0.62,
             y    = notch_hi + (yhi - notch_hi)*0.18,
             label= sprintf("<- MUESCA\nIC 95%% mediana\n[%.4f, %.4f]", notch_lo, notch_hi),
             size = 2.6, color=PAL$rose, fill="white",
             label.size=0.35, hjust=0.5, fontface="bold")+
    # ── Conteo de ganancias y pérdidas ──────────────────────────────────────
    annotate("label", x=1.47, y=quantile(d,.75),
             label=sprintf("Ganancias\n%s obs\n(%.1f%%)", format(pos,big.mark=","), pct),
             size=2.8, color=PAL$green, fontface="bold", hjust=0,
             fill="white", label.size=.25)+
    annotate("label", x=1.47, y=quantile(d,.25),
             label=sprintf("Perdidas\n%s obs\n(%.1f%%)", format(neg,big.mark=","), 100-pct),
             size=2.8, color=PAL$red, fontface="bold", hjust=0,
             fill="white", label.size=.25)+
    labs(title=paste(var,"– Boxplot  [MUESCAS ACTIVAS: notch=TRUE, notchwidth=0.25]"),
         y=var, x=NULL)+
    coord_cartesian(xlim=c(.5,1.9))
  
  # QQ-Plot
  qqr <- qqnorm(d, plot.it=FALSE)
  dfq <- data.frame(t=qqr$x, m=qqr$y)
  rq  <- round(cor(dfq$t,dfq$m),4)
  pq  <- ggplot(dfq,aes(x=t,y=m))+
    geom_point(color=color,alpha=.45,size=1.3)+
    geom_abline(slope=sd(d),intercept=mean(d),color=PAL$red,linewidth=1.8)+
    annotate("label",x=-Inf,y=Inf,label=sprintf("r = %.4f",rq),
             hjust=-.1,vjust=1.3,size=3.5,fill="#FFF3E0",color=PAL$dark,label.size=.3)+
    labs(title=paste(var,"– QQ-Plot"),
         x="Cuantiles teóricos N(0,1)", y="Cuantiles muestrales")
  
  list(h=ph, b=pb, q=pq,
       s=list(var=var,n=length(d),pos=pos,neg=neg,pct=pct,
              mu=mu,md=md,sg=sg,cv=cv,asim=asim,kurt=kurt))
}

uni_stats <- list()
for(var in KEY_VARS){
  if(!var %in% names(df)) next
  res <- panel_desc(var, df, VAR_COL[[var]])
  if(is.null(res)) next
  p <- (res$h | res$b | res$q)+
    plot_annotation(
      title    = sprintf("Estadística Descriptiva – %s",var),
      subtitle = sprintf("n=%s | Media=%.4f | Desv=%.4f | CV=%.2f%% | Asim=%.3f | Kurt=%.3f | Gan=%.1f%%",
                         format(res$s$n,big.mark=","),res$s$mu,res$s$sg,
                         res$s$cv,res$s$asim,res$s$kurt,res$s$pct),
      theme=theme(plot.title=element_text(face="bold",size=14),
                  plot.subtitle=element_text(size=9)))
  reg(p,paste0("desc_",var,".png"))
  uni_stats[[var]] <- res$s
  cat(sprintf("  %-6s mu=%+.4f sg=%.4f CV=%.1f%% asim=%+.2f kurt=%.2f\n",
              var,res$s$mu,res$s$sg,res$s$cv,res$s$asim,res$s$kurt))
}

# Grid resumen
pg <- lapply(KEY_VARS, function(var){
  if(!var %in% names(df)) return(NULL)
  d <- df[[var]]; d <- d[is.finite(d)]
  if(length(d)<5) return(NULL)
  dfp <- data.frame(x=d,s=ifelse(d>=0,">=0","<0"))
  ggplot(dfp,aes(x=x,fill=s))+
    geom_histogram(bins=50,alpha=.78,color="white",linewidth=.1)+
    scale_fill_manual(values=c(">=0"=PAL$green,"<0"=PAL$red),guide="none")+
    geom_vline(xintercept=0,color="black",linewidth=1.2)+
    annotate("text",x=Inf,y=Inf,
             label=sprintf("Asim=%.2f\nKurt=%.2f",e1071::skewness(d),e1071::kurtosis(d)),
             hjust=1.1,vjust=1.3,size=2.8,color=PAL$dark,fontface="bold")+
    labs(title=var,x=NULL,y="Frec.")
}) |> Filter(Negate(is.null),x=_)

if(length(pg)>0){
  p_grid <- wrap_plots(pg,ncol=3)+
    plot_annotation(title="Resumen Descriptivo – Ganancias (verde) vs Pérdidas (rojo)",
                    subtitle="Asimetría y Curtosis anotados en cada panel",
                    theme=theme(plot.title=element_text(face="bold",size=15)))
  reg(p_grid,"desc_RESUMEN.png")
}
cat("  Descriptiva completada\n")
readline("\n  [Enter] → Sección 3: Covarianza y Correlación\n")


# ==============================================================================
# SECCIÓN 3 ─ COVARIANZA Y CORRELACIÓN
# ==============================================================================
# ── ¿Por qué usamos TANTO Pearson COMO Spearman? ─────────────────────────────
#
#   Pearson mide la asociación LINEAL entre dos variables:
#     r_P = Cov(X,Y) / (σX · σY)
#   Supone que la relación es lineal y que no hay outliers extremos.
#   Si ROA y CFO tienen una relación perfectamente lineal, r_P = ±1.
#
#   Spearman mide la asociación MONOTÓNICA (no necesariamente lineal):
#     r_S = Pearson calculado sobre los RANGOS de X e Y
#   No supone linealidad. Es robusto ante outliers porque trabaja con
#   el orden, no con los valores brutos.
#
#   En datos financieros los outliers son frecuentes (crisis, quiebras)
#   y las relaciones no siempre son lineales (ej.: efecto umbral de LEV).
#   Por eso mostramos AMBAS:
#     • Pearson = "¿cuánto se mueven juntos linealmente?"
#     • Spearman = "¿cuándo uno sube, el otro también tiende a subir?"
#   Si r_P ≠ r_S, la relación no es lineal o hay outliers influyentes.
# ─────────────────────────────────────────────────────────────────────────────
cat("\n[3/5] Covarianza y Correlación...\n")

vars_ok   <- KEY_VARS[KEY_VARS %in% names(df)]
num_df    <- df[,vars_ok] |> mutate(across(everything(),as.numeric))
cov_mat   <- cov(num_df, use="pairwise.complete.obs")
corr_p    <- cor(num_df, method="pearson",  use="pairwise.complete.obs")
corr_s    <- cor(num_df, method="spearman", use="pairwise.complete.obs")

# ── Función heatmap presentable (paleta rojo–blanco–azul) ────────────────────
# FIX triángulo superior: en lugar de asignar NA a las celdas superiores
# (lo que dejaba cuadros grises/blancos vacíos), directamente FILTRAMOS
# esas filas del data.frame antes de renderizar. Así ggplot nunca dibuja
# esas celdas y el área queda completamente en blanco, limpia.
hmap <- function(mat, titulo, subtitulo="", fmt="%.2f", lim=NULL){
  ml <- as.data.frame(as.table(mat)) |> rename(X=Var1, Y=Var2, V=Freq)
  
  # ── Mantener SOLO el triángulo estrictamente inferior (sin diagonal) ─────
  # as.integer() sobre factores devuelve el índice numérico del nivel.
  # Conservamos filas donde el índice de Y > índice de X → triángulo inferior.
  # Las celdas del triángulo superior y la diagonal simplemente no existen
  # en el data.frame → ggplot las deja en blanco sin dibujar nada.
  ml <- ml |>
    mutate(xi = as.integer(X), yi = as.integer(Y)) |>
    filter(yi > xi) |>
    select(-xi, -yi)
  
  rng <- if(!is.null(lim)) lim else range(ml$V, na.rm=TRUE)
  
  pfn <- scales::col_numeric(
    palette=c(HEAT_NEG, HEAT_NEU, HEAT_POS),
    domain=rng, na.color="grey90"
  )
  ml$fondo <- pfn(ml$V)
  ml$txt   <- sapply(ml$fondo, auto_txt)
  ml$lbl   <- sprintf(fmt, ml$V)
  
  ggplot(ml, aes(x=X, y=Y, fill=V))+
    geom_tile(color="white", linewidth=.55)+
    geom_text(aes(label=lbl, color=I(txt)), size=2.9, fontface="bold")+
    scale_fill_gradient2(
      low=HEAT_NEG, mid=HEAT_NEU, high=HEAT_POS,
      midpoint=0, limits=rng,
      name=NULL,
      guide=guide_colorbar(
        barwidth=.9, barheight=9,
        title="",
        label.theme=element_text(size=8, color=PAL$dark)
      )
    )+
    scale_color_identity()+
    labs(title=titulo, subtitle=subtitulo, x=NULL, y=NULL)+
    theme(
      axis.text.x    = element_text(angle=45, hjust=1, size=9, color=PAL$dark, face="bold"),
      axis.text.y    = element_text(size=9, color=PAL$dark, face="bold"),
      panel.grid     = element_blank(),
      plot.background= element_rect(fill="white", color=NA),
      legend.position= "right"
    )
}

# Covarianza (solo triángulo inferior — hmap filtra el superior internamente)
p_cov <- hmap(cov_mat,
              titulo="¿Cómo se mueven juntas las variables financieras?",
              subtitulo="Covarianza — mide variación conjunta en unidades originales\nRojo = suben juntas | Azul = se mueven en sentidos opuestos | Blanco = sin relación",
              fmt="%.2e")
reg(p_cov,"cov_covarianza.png")

# Pearson (solo triángulo inferior — hmap filtra el superior internamente)
p_pearson <- hmap(corr_p,
                  titulo="¿Cómo se asocian linealmente las variables? (Pearson)",
                  subtitulo="Rojo = asociacion positiva fuerte | Azul = asociacion negativa fuerte | Blanco = sin asociacion lineal\nSensible a outliers — supone relacion lineal",
                  lim=c(-1,1))
reg(p_pearson,"corr_pearson.png")

# Spearman (solo triángulo inferior — hmap filtra el superior internamente)
p_spearman <- hmap(corr_s,
                   titulo="¿Cómo se asocian las variables por tendencia? (Spearman)",
                   subtitulo="Rojo = cuando una sube, la otra sube | Azul = cuando una sube, la otra baja | Blanco = sin tendencia\nRobusto a outliers — no supone linealidad",
                   lim=c(-1,1))
reg(p_spearman,"corr_spearman.png")

# Panel comparativo Pearson vs Spearman (presentación final)
p_comp_corr <- (p_pearson | p_spearman)+
  plot_annotation(
    title    = "Fuerza y dirección de las asociaciones entre variables financieras",
    subtitle = paste(
      "Izquierda: Pearson (asociacion lineal) | Derecha: Spearman (tendencia monotonica)",
      "Diferencias entre ambas matrices senalan relaciones no lineales o influencia de outliers",
      "Rojo oscuro = asociacion positiva fuerte | Azul oscuro = asociacion negativa fuerte",
      sep="\n"
    ),
    theme=theme(
      plot.title   =element_text(face="bold",size=14,color=PAL$dark),
      plot.subtitle=element_text(size=9,color=PAL$slate)
    )
  )
reg(p_comp_corr,"corr_comparativa.png")

# Top correlaciones con ROA
if(TARGET %in% colnames(corr_s)){
  tc <- sort(abs(corr_s[TARGET,vars_ok[vars_ok!=TARGET]]),decreasing=TRUE)[1:min(8,length(vars_ok)-1)]
  dft<- data.frame(Variable=names(tc),r_abs=as.numeric(tc),
                   r_orig=corr_s[names(tc),TARGET]) |> arrange(desc(r_abs))
  cat("\n  Top correlaciones Spearman con ROA:\n")
  for(i in seq_len(nrow(dft))){
    bar <- paste(rep("\u2588",floor(dft$r_abs[i]*30)),collapse="")
    cat(sprintf("    %-8s: %s%.3f  %s\n",
                dft$Variable[i], ifelse(dft$r_orig[i]>0,"+","-"),
                dft$r_abs[i], bar))
  }
  p_tc <- ggplot(dft,aes(x=r_abs,y=reorder(Variable,r_abs),fill=r_orig>0))+
    geom_col(alpha=.85,width=.7)+
    geom_text(aes(label=sprintf("%+.3f",r_orig)),hjust=-.1,size=3.5,
              color=PAL$dark,fontface="bold")+
    scale_fill_manual(values=c("TRUE"=PAL$teal,"FALSE"=PAL$red),
                      labels=c("TRUE"="Positiva","FALSE"="Negativa"),name="Dirección")+
    geom_vline(xintercept=.3,color=PAL$amber,linewidth=1.3,linetype="dashed")+
    labs(title="Variables más asociadas con ROA (Spearman)",
         subtitle="Línea = umbral 0.30 (correlación moderada)",
         x="|r de Spearman|",y=NULL)+
    xlim(0,1.05)+theme(legend.position="bottom")
  reg(p_tc,"corr_top_roa.png")
}
cat("  Covarianza y Correlación completadas\n")
readline("\n  [Enter] → Sección 4: Regresión Múltiple\n")


# ==============================================================================
# SECCIÓN 4 ─ REGRESIÓN LINEAL MÚLTIPLE + VISUALIZACIÓN + RESIDUOS
# ==============================================================================
# ── MODELO ────────────────────────────────────────────────────────────────────
# ROA = β₀ + β₁·SIZE + β₂·LEV + β₃·CFO + β₄·GRW + β₅·CUR
#           + β₆·INVREC + β₇·PPE + β₈·AGE + ε
#
# Cada βᵢ = cambio en ROA por +1 unidad de la variable i, manteniendo
# todas las demás constantes (ceteris paribus).
# ==============================================================================
cat("\n[4/5] Regresión Lineal Múltiple...\n")

f_ols <- as.formula(paste(TARGET, "~", paste(FEATURES, collapse = "+")))
mod   <- lm(f_ols, data = train_df)

yp  <- predict(mod, newdata = test_df)
res <- residuals(mod)
fv  <- fitted(mod)
r2  <- 1 - sum((y_test - yp)^2) / sum((y_test - mean(y_test))^2)
rmse <- sqrt(mean((y_test - yp)^2))
mae  <- mean(abs(y_test - yp))
r2a  <- summary(mod)$adj.r.squared

cat(sprintf("  R2=%.4f | R2-adj=%.4f | RMSE=%.4f | MAE=%.4f\n", r2, r2a, rmse, mae))

ctbl <- summary(mod)$coefficients
cat(sprintf("\n  %-12s  %+10s  %8s  %10s  %8s\n",
            "Variable", "Beta", "t-stat", "p-valor", "Signif."))
cat(sprintf("  %s\n", strrep("-", 55)))
for (i in seq_len(nrow(ctbl))) {
  nm  <- rownames(ctbl)[i]
  b   <- ctbl[i, 1]; tv <- ctbl[i, 3]; pv <- ctbl[i, 4]
  sig <- ifelse(pv < .001, "***", ifelse(pv < .01, "**", ifelse(pv < .05, "*", "ns")))
  cat(sprintf("  %-12s  %+10.5f  %8.3f  %10.4f  %8s\n", nm, b, tv, pv, sig))
}
fs <- summary(mod)$fstatistic
cat(sprintf("\n  F=%.2f (p=%.4e) | AIC=%.1f | BIC=%.1f\n",
            fs[1], pf(fs[1], fs[2], fs[3], lower.tail = FALSE),
            AIC(mod), BIC(mod)))


# ==============================================================================
# GRÁFICO 1 ─ VISUALIZACIÓN COMPLETA DE LA REGRESIÓN
# ==============================================================================
# Diseño del panel combinado:
#   ┌─────────────────────────────────────────────────────┐
#   │  ARRIBA:  Predichos vs Reales  +  R² anotado        │
#   ├───────────────────────┬─────────────────────────────┤
#   │  ABAJO-IZQ: Betas     │  ABAJO-DER: R² y métricas   │
#   └───────────────────────┴─────────────────────────────┘
# ==============================================================================

# ── Panel superior: Predichos vs Reales ───────────────────────────────────────
dfpv <- data.frame(obs = y_test, pred = yp, ep = (yp - y_test) > 0)

p_pred <- ggplot(dfpv, aes(x = obs, y = pred, color = ep)) +
  geom_point(alpha = .40, size = 1.8) +
  geom_abline(slope = 1, intercept = 0,
              color = PAL$red, linewidth = 2, linetype = "dashed") +
  # Banda de ±RMSE alrededor de la línea perfecta (zona de "buen ajuste")
  geom_ribbon(data = dfpv, aes(x = obs, ymin = obs - rmse, ymax = obs + rmse),
              fill = PAL$teal, alpha = .07, inherit.aes = FALSE) +
  scale_color_manual(
    values = c("TRUE" = PAL$orange, "FALSE" = PAL$teal),
    labels = c("TRUE" = "Sobreestimado (pred > real)",
               "FALSE" = "Subestimado (pred < real)"),
    name = NULL
  ) +
  # R² anotado prominente en la esquina superior izquierda
  annotate("label",
           x = min(y_test, na.rm = TRUE),
           y = max(yp,     na.rm = TRUE),
           label = sprintf(
             "R\u00b2       = %.4f\nR\u00b2-adj  = %.4f\nRMSE    = %.4f\nMAE     = %.4f",
             r2, r2a, rmse, mae),
           hjust = 0, vjust = 1, size = 3.6,
           fill = "#EFF6FF", color = PAL$dark,
           family = "mono") +
  labs(
    title    = sprintf("Regresión Lineal Múltiple  –  ROA predicho vs ROA observado"),
    subtitle = paste(
      sprintf("R\u00b2 = %.4f  |  R\u00b2-adj = %.4f  |  RMSE = %.4f  |  MAE = %.4f",
              r2, r2a, rmse, mae),
      "Línea roja = predicción perfecta  |  Banda teal = ±RMSE  |  Colores = dirección del error",
      sep = "\n"
    ),
    x = "ROA Observado", y = "ROA Predicho"
  ) +
  theme(legend.position = "bottom")


# ── Panel inferior izquierdo: Coeficientes Beta con IC 95% ────────────────────
# Ordenados de mayor a menor valor absoluto para ver de un vistazo
# cuáles variables tienen más peso en el modelo.
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
    label = sprintf("%.4f%s", B,
                    ifelse(p < .001, "***",
                           ifelse(p < .01, "**",
                                  ifelse(p < .05, "*", ""))))
  ) |>
  # Ordenar por |Beta| descendente: la variable más influyente arriba
  arrange(desc(abs(B))) |>
  mutate(Variable = factor(Variable, levels = Variable))

p_betas <- ggplot(cdf, aes(x = B, y = Variable, color = sig)) +
  # Línea vertical en 0: a la derecha = efecto positivo en ROA
  geom_vline(xintercept = 0, color = "gray35",
             linewidth = 1.3, linetype = "dashed") +
  # Banda gris del IC 95%
  geom_segment(aes(x = lo, xend = hi, yend = Variable),
               linewidth = 2.5, alpha = .30) +
  # Punto del coeficiente
  geom_point(size = 5) +
  # Etiqueta con valor y significancia
  geom_text(aes(label = label),
            hjust = -.15, size = 3.3, color = PAL$dark, fontface = "bold") +
  scale_color_manual(
    values = c("Significativo (p<0.05)"  = PAL$teal,
               "No significativo"         = PAL$slate),
    name = NULL
  ) +
  labs(
    title    = "Coeficientes \u03b2 (ordenados por |valor|)",
    subtitle = "Barras = IC 95% | Derecha de 0 = efecto positivo en ROA\n*** p<0.001  ** p<0.01  * p<0.05",
    x = "Valor del coeficiente \u03b2", y = NULL
  ) +
  theme(legend.position = "bottom")


# ── Panel inferior derecho: contribución relativa de cada β ───────────────────
# Muestra el peso relativo de cada variable en unidades comparables
# (coeficientes estandarizados = β × (σ_x / σ_y))
sds_x <- sapply(FEATURES, function(v) sd(train_df[[v]], na.rm = TRUE))
sd_y  <- sd(y_train, na.rm = TRUE)
betas_raw <- ctbl[-1, 1]              # β sin intercepto
betas_std <- betas_raw * sds_x / sd_y # β estandarizado

df_std <- data.frame(
  Variable = names(betas_std),
  Beta_std = as.numeric(betas_std),
  sig      = ifelse(ctbl[-1, 4] < .05, "Significativo", "No significativo")
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
    subtitle = "|\u03b2*| = efecto en ROA en desviaciones est\u00e1ndar\nPermite comparar variables con distintas escalas",
    x = "|\u03b2 estandarizado|", y = NULL
  ) +
  xlim(0, max(abs(df_std$Beta_std)) * 1.35) +
  theme(legend.position = "bottom")


# ── Ensamblaje del panel completo de regresión ────────────────────────────────
p_regresion_completa <- (
  p_pred /                           # fila superior: predichos vs reales
    (p_betas | p_contrib)            # fila inferior: betas brutos | betas estandarizados
) +
  plot_layout(heights = c(1.4, 1)) + # la fila superior ocupa más espacio
  plot_annotation(
    title    = "Regresión Lineal Múltiple – Panel Completo",
    subtitle = sprintf(
      "R\u00b2 = %.4f  |  R\u00b2-adj = %.4f  |  F = %.2f (p < 0.001)  |  n-train = %d  |  n-test = %d",
      r2, r2a, fs[1], nrow(train_df), nrow(test_df)),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 15, color = PAL$dark),
      plot.subtitle = element_text(size = 9.5,  color = PAL$slate)
    )
  )

reg(p_regresion_completa, "ols_panel_completo.png")


# ==============================================================================
# GRÁFICO 2 ─ ANÁLISIS DE RESIDUOS Y VERIFICACIÓN DE SUPUESTOS
# ==============================================================================
# ── ¿Por qué analizamos los residuos? ────────────────────────────────────────
#
# Los residuos εᵢ = yᵢ - ŷᵢ (ROA real − ROA predicho) condensan todo lo que
# el modelo NO explicó. Si el modelo es correcto, esos errores deben
# comportarse como "ruido puro": aleatorios, centrados en cero, con varianza
# constante y sin estructura.
#
# Analizamos tres gráficos diagnósticos que verifican supuestos distintos:
#
#   GRÁFICO A — Histograma de residuos + curva normal teórica
#   ─────────────────────────────────────────────────────────
#   Verifica: NORMALIDAD de los errores (supuesto para inferencia finita).
#   Cómo leerlo:
#     • Las barras deben seguir la campana roja (distribución N(0,σ²)).
#     • Colas más pesadas que la campana → datos con más valores extremos
#       de lo esperado (leptocúrtica); frecuente en datos financieros.
#     • Cola asimétrica → hay más errores grandes en un solo lado.
#   Test Jarque-Bera (JB): H₀ = normalidad. Si p < 0.05 se rechaza.
#     • En muestras grandes (n>5,000) el TCL garantiza que los estimadores
#       β siguen siendo válidos aunque JB rechace: la no-normalidad
#       de los residuos afecta la precisión de los intervalos de confianza
#       en muestras pequeñas, pero no sesga los coeficientes.
#
#   GRÁFICO B — QQ-Plot de residuos
#   ────────────────────────────────
#   Verifica: NORMALIDAD con más detalle que el histograma.
#   Cómo leerlo:
#     • Los puntos deben caer sobre la línea roja (cuantiles teóricos N(0,1)).
#     • Desviación en las esquinas (colas) = colas más pesadas de lo normal
#       (outliers o distribución leptocúrtica).
#     • Desviación en forma de "S" = asimetría.
#     • r cercano a 1.00 = residuos muy normales.
#     • r < 0.98 con n grande = señal de no-normalidad importante.
#
#   GRÁFICO C — Residuos vs Valores Ajustados (+ curva LOWESS)
#   ────────────────────────────────────────────────────────────
#   Verifica: HOMOCEDASTICIDAD (varianza constante) y LINEALIDAD.
#   Cómo leerlo:
#     • Los puntos deben dispersarse aleatoriamente alrededor de la línea
#       roja horizontal (residuo = 0). Sin patrones sistemáticos.
#     • La curva naranja (LOWESS) debe ser PLANA cerca del 0.
#       Si la curva se curva → hay no-linealidad no capturada por el modelo.
#     • Si la nube de puntos se ensancha hacia la derecha (forma de embudo)
#       → heterocedasticidad: la varianza del error crece con el ROA predicho,
#       lo que infla los errores estándar de los betas y distorsiona los
#       intervalos de confianza.
#     • Puntos muy alejados del 0 con ŷ extremos = observaciones influyentes.
#
# ─────────────────────────────────────────────────────────────────────────────

rf  <- res[is.finite(res)]
jbt <- jarque.bera.test(rf)
jbp <- jbt$p.value

# A: Histograma de residuos
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
           x = Inf, y = Inf,
           label = sprintf(
             "Jarque-Bera\np = %.3e\n%s\n\nAsimetria = %.3f\nCurtosis  = %.3f",
             jbp,
             ifelse(jbp > .05, "OK: Normal aprox.", "Colas pesadas\n(normal si n>5000)"),
             e1071::skewness(rf),
             e1071::kurtosis(rf)),
           hjust = 1.05, vjust = 1.1, size = 2.9,
           fill = "#FFF8E7", color = PAL$dark,
           family = "mono") +
  labs(
    title    = "A) Distribucion de Residuos",
    subtitle = "Barras deben seguir la campana roja\nColas pesadas = outliers o leptocurtica (frecuente en finanzas)",
    x = "Residuo  (ROA real - ROA predicho)", y = "Densidad"
  )

# B: QQ-Plot
pq_r <- ggplot(dfq, aes(x = t, y = m)) +
  geom_point(color = PAL$teal, alpha = .4, size = 1) +
  geom_abline(slope = sd(rf), intercept = mean(rf),
              color = PAL$red, linewidth = 2) +
  # Banda de confianza del 95% para el QQ-Plot
  annotate("label",
           x = -Inf, y = Inf,
           label = sprintf(
             "r = %.4f\n%s",
             rqq,
             ifelse(rqq > .99, "Muy normal",
                    ifelse(rqq > .97, "Aprox. normal",
                           "Colas pesadas"))),
           hjust = -.05, vjust = 1.2, size = 3.2,
           fill = "#FFF3E0", color = PAL$dark) +
  labs(
    title    = "B) QQ-Plot de Residuos",
    subtitle = "Puntos sobre la línea roja = normalidad\nDesviacion en extremos = colas pesadas (outliers en residuos)",
    x = "Cuantiles teoricos N(0,1)", y = "Cuantiles muestrales de residuos"
  )

# C: Residuos vs Ajustados
dfh <- data.frame(ft = fv, r = res) |>
  filter(is.finite(ft) & is.finite(r))
smh <- as.data.frame(lowess(dfh$ft, dfh$r, f = .3))

prf <- ggplot(dfh, aes(x = ft, y = r)) +
  # Banda de ±2σ (zona donde deben caer ~95% de los residuos)
  geom_hline(yintercept =  2 * sd(rf),
             color = PAL$slate, linewidth = .9, linetype = "dotted") +
  geom_hline(yintercept = -2 * sd(rf),
             color = PAL$slate, linewidth = .9, linetype = "dotted") +
  geom_point(color = PAL$navy, alpha = .20, size = .9) +
  geom_hline(yintercept = 0,
             color = PAL$red, linewidth = 1.5, linetype = "dashed") +
  # Curva LOWESS: debe ser plana en 0 → indica homocedasticidad y linealidad
  geom_line(data = smh, aes(x = x, y = y),
            color = PAL$orange, linewidth = 2.2) +
  annotate("text",
           x = max(dfh$ft, na.rm = TRUE),
           y =  2 * sd(rf),
           label = "+2\u03c3", hjust = 1, vjust = -.3,
           size = 3, color = PAL$slate, fontface = "bold") +
  annotate("text",
           x = max(dfh$ft, na.rm = TRUE),
           y = -2 * sd(rf),
           label = "-2\u03c3", hjust = 1, vjust = 1.2,
           size = 3, color = PAL$slate, fontface = "bold") +
  labs(
    title    = "C) Residuos vs Valores Ajustados",
    subtitle = paste(
      "Linea naranja (LOWESS): PLANA en 0 = linealidad + homocedasticidad",
      "Embudo que se ensancha = heterocedasticidad | Curva = no-linealidad",
      "Lineas grises punteadas = ±2\u03c3 (95% de residuos deberian caer dentro)",
      sep = "\n"
    ),
    x = "Valores ajustados (ROA predicho en train)",
    y = "Residuos"
  )

# ── Panel de residuos ensamblado ──────────────────────────────────────────────
p_residuos <- (ph_r | pq_r | prf) +
  plot_annotation(
    title    = "Diagnóstico de Residuos – Verificación de Supuestos OLS",
    subtitle = sprintf(
      "JB p=%.3e (%s)  |  QQ r=%.4f  |  n residuos = %d",
      jbp,
      ifelse(jbp > .05, "Normal aprox.", "No-normal: colas pesadas (aceptable con n grande)"),
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
readline("\n  [Enter] → Sección 5: PCA\n")


# ==============================================================================
# SECCIÓN 5 ─ ANÁLISIS DE COMPONENTES PRINCIPALES (PCA)
# ==============================================================================
cat("\n[5/5] PCA...\n")
feats_ok <- FEATURES[FEATURES %in% names(df)]
Xpr      <- df[,feats_ok] |> na.omit()
vok      <- sapply(Xpr,function(x) var(x,na.rm=TRUE)>1e-10)
Xpr      <- Xpr[,vok,drop=FALSE]; fp <- colnames(Xpr)
Xsc      <- scale(Xpr)
pca      <- prcomp(Xsc,center=FALSE,scale.=FALSE)
ve       <- (pca$sdev^2)/sum(pca$sdev^2)
vc       <- cumsum(ve)
n80      <- min(which(vc>=.80)); n90 <- min(which(vc>=.90))
eig      <- pca$sdev^2

cat("\n  Eigenvalores y Varianza Explicada:\n")
cat(sprintf("  %-5s  %10s  %10s  %10s  Kaiser\n","PC","Eigenvalor","Var%%","Acum%%"))
cat(sprintf("  %s\n",strrep("-",55)))
for(i in seq_along(ve))
  cat(sprintf("  PC%-3d  %10.4f  %10.2f  %10.2f  %s\n",
              i,eig[i],ve[i]*100,vc[i]*100,ifelse(eig[i]>=1,"Retener","Descartar")))
cat(sprintf("\n  Comps para 80%%: %d | para 90%%: %d\n",n80,n90))

np   <- length(ve)
dfs  <- data.frame(PC=factor(paste0("PC",seq_len(np)),levels=paste0("PC",seq_len(np))),
                   eg=eig,vp=ve*100,cp=vc*100)
p_sc <- ggplot(dfs,aes(x=PC,y=eg))+
  geom_col(aes(fill=eg>=1),alpha=.85,width=.7)+
  geom_line(aes(group=1),color=PAL$orange,linewidth=1.8)+
  geom_point(color=PAL$orange,size=3.5)+
  geom_hline(yintercept=1,color=PAL$red,linewidth=1.5,linetype="dashed")+
  annotate("text",x=1.5,y=1.08,label="Criterio Kaiser (retener si eigenvalor >= 1)",
           color=PAL$red,size=3,hjust=0)+
  scale_fill_manual(values=c("TRUE"=PAL$teal,"FALSE"=PAL$slate),
                    labels=c("TRUE"="Retener","FALSE"="Descartar"),name=NULL)+
  labs(title="Scree Plot – Eigenvalores por Componente",
       subtitle="Teal = eigenvalor >= 1 (Criterio Kaiser)",
       x="Componente Principal",y="Eigenvalor")+
  theme(axis.text.x=element_text(angle=45,hjust=1),legend.position="bottom")

p_cp <- ggplot(dfs,aes(x=PC,y=cp,group=1))+
  geom_area(fill=PAL$teal,alpha=.2)+geom_line(color=PAL$teal,linewidth=2)+
  geom_point(size=3,color=PAL$teal)+
  geom_hline(yintercept=80,color=PAL$orange,linewidth=1.4,linetype="dashed")+
  geom_hline(yintercept=90,color=PAL$red,linewidth=1.4,linetype="dashed")+
  annotate("label",x=as.integer(n80)+.5,y=80,
           label=sprintf("80%%: %d PCs",n80),color=PAL$orange,fill="white",size=3.2,label.size=.3)+
  annotate("label",x=as.integer(n90)+.5,y=90,
           label=sprintf("90%%: %d PCs",n90),color=PAL$red,fill="white",size=3.2,label.size=.3)+
  labs(title="Varianza Acumulada",x="N° Componentes",y="% Varianza acum.")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

reg((p_sc|p_cp)+plot_annotation(title="PCA – Selección de Componentes",
                                theme=theme(plot.title=element_text(face="bold"))),"pca_scree.png")

# Loadings heatmap
ns  <- min(4,ncol(pca$rotation))
lm  <- pca$rotation[,seq_len(ns),drop=FALSE]; colnames(lm)<-paste0("PC",seq_len(ns))
ll  <- as.data.frame(lm)|>rownames_to_column("Variable")|>
  pivot_longer(-Variable,names_to="PC",values_to="Loading")
pfn <- scales::col_numeric(palette=c(HEAT_NEG,HEAT_NEU,HEAT_POS),domain=c(-1,1))
ll$fondo <- pfn(ll$Loading); ll$txt <- sapply(ll$fondo,auto_txt)

p_ld <- ggplot(ll,aes(x=PC,y=Variable,fill=Loading))+
  geom_tile(color="white",linewidth=.7)+
  geom_text(aes(label=sprintf("%.3f",Loading),color=I(txt)),size=3.2,fontface="bold")+
  scale_fill_gradient2(low=HEAT_NEG,mid=HEAT_NEU,high=HEAT_POS,midpoint=0,
                       limits=c(-1,1),name="Loading",
                       guide=guide_colorbar(barwidth=.8,barheight=7))+
  scale_color_identity()+
  labs(title="PCA – Matriz de Loadings (Eigenvectores)",
       subtitle="|loading|>0.3 → variable relevante en ese componente\nTexto negro=celda clara | Texto blanco=celda oscura",
       x="Componente Principal",y=NULL)+
  theme(axis.text.x=element_text(face="bold",size=10),
        axis.text.y=element_text(size=10),
        panel.grid=element_blank(),
        plot.background=element_rect(fill="white",color=NA))
reg(p_ld,"pca_loadings.png")

# ── Biplot con explicación completa de la escala de color ─────────────────────
#
# ¿Qué aporta la escala de color en el biplot?
#   En un biplot clásico los puntos son iguales (sin color), lo que impide
#   saber si una empresa está en esa posición porque gana o porque pierde.
#   Al colorear cada punto con su valor de ROA:
#     • ROJO  = ROA negativo (empresa con pérdidas)
#     • CREMA = ROA cercano a cero (empresa en punto de equilibrio)
#     • TEAL  = ROA positivo alto (empresa rentable)
#   Esto permite ver simultáneamente DOS dimensiones:
#     1. Posición en el espacio de PCA (estructura multivariada)
#     2. Rentabilidad real (ROA)
#   Si los puntos de alto ROA se agrupan en la dirección de la flecha de CFO,
#   confirma visualmente que CFO es el principal driver de rentabilidad.
# ─────────────────────────────────────────────────────────────────────────────
sc   <- as.data.frame(pca$x[,1:2]); colnames(sc)<-c("PC1","PC2")
rv   <- df[rownames(Xpr),TARGET]; sc$ROA <- as.numeric(rv)
sf   <- 3.5
adf  <- data.frame(Variable=fp,x0=0,y0=0,
                   x1=pca$rotation[fp,1]*sf,y1=pca$rotation[fp,2]*sf)
q5   <- quantile(sc$ROA,.05,na.rm=TRUE)
q95  <- quantile(sc$ROA,.95,na.rm=TRUE)

p_bp <- ggplot(sc,aes(x=PC1,y=PC2,color=ROA))+
  geom_point(alpha=.35,size=.9)+
  scale_color_gradientn(
    colors=c(PAL$rose,"#FFF8E7",PAL$teal),
    values=rescale(c(q5,0,q95)), name="ROA",
    limits=c(q5,q95), oob=squish,
    guide=guide_colorbar(
      title.position="top", title.hjust=.5,
      barwidth=12, barheight=.7,
      direction="horizontal",
      label.theme=element_text(size=8))
  )+
  geom_segment(data=adf,aes(x=x0,y=y0,xend=x1,yend=y1),
               inherit.aes=FALSE,
               arrow=arrow(length=unit(.28,"cm"),type="closed"),
               color=PAL$dark,linewidth=1.3)+
  geom_label_repel(data=adf,aes(x=x1*1.18,y=y1*1.18,label=Variable),
                   inherit.aes=FALSE,size=3.5,fontface="bold",
                   fill="white",color=PAL$dark,box.padding=.25,max.overlaps=20)+
  geom_hline(yintercept=0,color="gray50",linewidth=.8)+
  geom_vline(xintercept=0,color="gray50",linewidth=.8)+
  labs(
    title    = "PCA – Biplot (PC1 vs PC2)",
    subtitle = paste(
      sprintf("PC1=%.1f%% var | PC2=%.1f%% var | Acum=%.1f%%",
              ve[1]*100,ve[2]*100,(ve[1]+ve[2])*100),
      "Puntos = empresas | Flechas = variables | Flechas paralelas → correlacion positiva",
      "ESCALA DE COLOR: Rojo=ROA bajo (perdidas) | Crema=ROA~0 | Verde-azul=ROA alto (rentable)",
      "Los puntos teal agrupados en la direccion de CFO confirman que el flujo de caja impulsa la rentabilidad",
      sep="\n"
    ),
    x=sprintf("PC1 (%.1f%% varianza)",ve[1]*100),
    y=sprintf("PC2 (%.1f%% varianza)",ve[2]*100)
  )+
  theme(legend.position="bottom", legend.box="horizontal")
reg(p_bp,"pca_biplot.png")
cat("  PCA completado\n")
readline("\n  [Enter] → K-Means y guardado final\n")


# ==============================================================================
# SECCIÓN 6 ─ MÉTODOS CLÁSICOS DE PARTICIÓN: K-MEANS
# ==============================================================================
# Pensum: "Métodos Clásicos de Partición"
# ─────────────────────────────────────────────────────────────────────────────
cat("\n[+] K-Means...\n")
nkm <- min(4,ncol(pca$x)); Xkm <- pca$x[,seq_len(nkm)]
Kr  <- 2:9; ine <- sil <- numeric(length(Kr))
for(i in seq_along(Kr)){
  kk <- kmeans(Xkm,centers=Kr[i],nstart=25,iter.max=300)
  ine[i] <- kk$tot.withinss
  sil[i] <- mean(silhouette(kk$cluster,dist(Xkm))[,3])
}
ok <- Kr[which.max(sil)]
cat(sprintf("  K óptimo: K=%d (S=%.4f)\n",ok,max(sil)))
dfe<-data.frame(k=Kr,ine=ine,sil=sil)
pe<-ggplot(dfe,aes(x=k,y=ine))+geom_line(color=PAL$teal,linewidth=2.2)+geom_point(size=3.5,color=PAL$teal)+geom_vline(xintercept=ok,color=PAL$red,linewidth=2,linetype="dashed")+scale_x_continuous(breaks=Kr)+labs(title="Método del Codo",subtitle="Buscar el codo donde la inercia frena su caída",x="K",y="Inercia (WCSS)")
ps<-ggplot(dfe,aes(x=k,y=sil))+geom_line(color=PAL$orange,linewidth=2.2)+geom_point(size=3.5,color=PAL$orange)+geom_vline(xintercept=ok,color=PAL$red,linewidth=2,linetype="dashed")+annotate("label",x=ok+.4,y=max(sil)*.97,label=sprintf("K=%d óptimo",ok),color=PAL$red,fill="white",size=3.5,fontface="bold",label.size=.3)+scale_x_continuous(breaks=Kr)+labs(title="Silhouette Score",subtitle="Máximo = clusters más compactos y separados",x="K",y="Silhouette")
reg((pe|ps)+plot_annotation(title="K-Means – Selección del número óptimo de clusters",theme=theme(plot.title=element_text(face="bold"))),"kmeans_seleccion_k.png")


km   <- kmeans(Xkm,centers=ok,nstart=30,iter.max=500)
lkm  <- km$cluster
cc   <- c(PAL$teal,PAL$orange,PAL$navy,PAL$purple,PAL$green,PAL$red,PAL$teal2,PAL$pink)[seq_len(ok)]
so   <- silhouette(lkm,dist(Xkm)); sd2 <- as.data.frame(so[,c(1,3)]); colnames(sd2)<-c("cl","sw"); sd2$cl<-factor(sd2$cl); sd2<-sd2[order(sd2$cl,sd2$sw),]; sd2$obs<-seq_len(nrow(sd2)); sm2<-mean(sd2$sw)
p_si<-ggplot(sd2,aes(x=obs,y=sw,fill=cl))+geom_col(width=1,alpha=.78)+geom_hline(yintercept=sm2,color="black",linewidth=1.5,linetype="dashed")+geom_hline(yintercept=0,color="gray30",linewidth=.8)+scale_fill_manual(values=cc,name="Cluster")+annotate("label",x=nrow(sd2)*.75,y=sm2+.03,label=sprintf("Media = %.3f",sm2),fill="white",color="black",size=3.5,label.size=.3)+labs(title="Silhouette por Observación",subtitle=">0 = bien asignado | <0 = posible reasignación",x="Observaciones",y="Coef. Silhouette")+theme(legend.position="bottom")
reg(p_si,"kmeans_silhouette.png")

dk2d<-data.frame(PC1=Xkm[,1],PC2=Xkm[,2],cl=factor(lkm)); c2d<-as.data.frame(km$centers[,1:2]); colnames(c2d)<-c("PC1","PC2"); c2d$cl<-factor(seq_len(ok)); c2d$lb<-paste0("C",seq_len(ok))
p_k2<-ggplot(dk2d,aes(x=PC1,y=PC2,color=cl))+geom_point(alpha=.35,size=1.3)+geom_point(data=c2d,aes(x=PC1,y=PC2),inherit.aes=FALSE,shape=8,size=9,stroke=2.5,color="black")+geom_label(data=c2d,aes(x=PC1,y=PC2+.35,label=lb),inherit.aes=FALSE,fontface="bold",size=4,fill="white")+scale_color_manual(values=cc,name="Cluster")+geom_hline(yintercept=0,color="gray55",linewidth=.7)+geom_vline(xintercept=0,color="gray55",linewidth=.7)+labs(title=sprintf("K-Means (K=%d) – Espacio PC1-PC2",ok),subtitle=sprintf("Silhouette=%.4f | Distancia Euclidiana | Datos estandarizados + PCA",sm2),x=sprintf("PC1 (%.1f%% var)",ve[1]*100),y=sprintf("PC2 (%.1f%% var)",ve[2]*100))+theme(legend.position="bottom")
reg(p_k2,"kmeans_2d.png")

pvx<-c("ROA","CFO","LEV","SIZE","GRW"); pvx<-pvx[pvx %in% names(df)]
dfpr<-df[rownames(Xpr),pvx]; dfpr$cluster<-factor(lkm)
cat("\n  Perfil por Cluster:\n")
cat(sprintf("  %-8s  %10s  %10s  %10s  %8s\n","Cluster","ROA","CFO","LEV","n"))
for(cl in levels(dfpr$cluster)){s<-dfpr[dfpr$cluster==cl,]; cat(sprintf("  C%-7s  %+10.4f  %+10.4f  %10.4f  %8d\n",cl,mean(s$ROA,na.rm=T),mean(s$CFO,na.rm=T),mean(s$LEV,na.rm=T),nrow(s)))}

dfpl<-dfpr|>pivot_longer(cols=all_of(pvx),names_to="Variable",values_to="Valor")
p_pf<-ggplot(dfpl,aes(x=cluster,y=Valor,fill=cluster))+geom_boxplot(alpha=.68,outlier.alpha=.2,outlier.size=.9)+geom_hline(yintercept=0,color="black",linewidth=.9,linetype="dashed")+scale_fill_manual(values=cc,name=NULL)+facet_wrap(~Variable,scales="free_y")+labs(title=sprintf("Perfil Financiero por Cluster (K=%d)",ok),subtitle="Línea punteada = 0 | Cajas cruzando el 0 = mezcla ganancias/pérdidas",x="Cluster",y="Valor")+theme(legend.position="none")
reg(p_pf,"kmeans_perfiles.png")
cat("  K-Means completado\n")
readline("\n  [Enter] → Guardar todos los gráficos\n")


# ==============================================================================
# SECCIÓN 7 ─ GUARDADO FINAL
# ==============================================================================
cat(sprintf("\n  Guardando %d gráficos en '%s/'...\n",length(REGISTRO),OUTPUT_DIR))
for(nm in names(REGISTRO)){
  ruta<-file.path(OUTPUT_DIR,nm); p<-REGISTRO[[nm]]
  tryCatch({
    if(inherits(p,"gg")||inherits(p,"patchwork"))
      ggsave(ruta,plot=p,width=14,height=8,dpi=140,bg="white",limitsize=FALSE)
    else if(inherits(p,"recordedplot")){png(ruta,width=2400,height=1200,res=140,bg="white");replayPlot(p);dev.off()}
    cat(sprintf("  OK  %s\n",nm))
  },error=function(e) cat(sprintf("  ERR %s: %s\n",nm,e$message)))
}

res_json <- list(
  dataset=list(n=nrow(df),empresas=n_emp,periodo=sprintf("%d-%d",yr_min,yr_max)),
  regresion=list(R2=round(r2,4),RMSE=round(rmse,4),MAE=round(mae,4),R2adj=round(r2a,4)),
  pca=list(comp_80=n80,comp_90=n90,var_PC1=round(ve[1],4),var_PC2=round(ve[2],4)),
  kmeans=list(K=ok,silhouette=round(max(sil),4))
)
write_json(res_json,file.path(OUTPUT_DIR,"resumen.json"),pretty=TRUE,auto_unbox=TRUE)

cat(sprintf("\n  Total: %d graficos | Carpeta: '%s/'\n",length(REGISTRO),OUTPUT_DIR))
cat(strrep("=",70),"\n  PIPELINE COMPLETADO\n",strrep("=",70),"\n")




# ==============================================================================
# SECCIÓN 8 ─ GUARDADO FINAL DE TODOS LOS GRÁFICOS
# ==============================================================================
# Guarda cada plot registrado en REGISTRO como archivo .png
# en la misma carpeta donde está el script .R (no en el working directory).
#
# Para que funcione correctamente, abre el script desde RStudio
# (File → Open File) o asegúrate de que el archivo esté guardado en disco.
# El directorio de destino se detecta automáticamente con:
#   rstudioapi::getSourceEditorContext()$path   (si usas RStudio)
#   sys.frames() / commandArgs()                (si ejecutas desde terminal)
# ==============================================================================
cat("\n[8/8] Guardando gráficos...\n")

# ── Detectar la carpeta del script ────────────────────────────────────────────
script_dir <- tryCatch({
  
  # Opción A: RStudio activo y el archivo está guardado
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    ruta_script <- rstudioapi::getSourceEditorContext()$path
    if (nchar(ruta_script) > 0) {
      dirname(ruta_script)
    } else {
      stop("script no guardado")
    }
  } else {
    stop("rstudioapi no disponible")
  }
  
}, error = function(e) {
  
  # Opción B: ejecutado con Rscript desde terminal
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    dirname(normalizePath(sub("--file=", "", file_arg)))
  } else {
    # Opción C: fallback al directorio de trabajo actual
    message("  No se pudo detectar la ruta del script.")
    message("  Guardando en el directorio de trabajo: ", getwd())
    getwd()
  }
})

cat(sprintf("  Carpeta de destino: %s\n", script_dir))

# ── Parámetros de exportación ─────────────────────────────────────────────────
ANCHO_PX  <- 2800   # píxeles de ancho  (ajusta si quieres más o menos resolución)
ALTO_PX   <- 1600   # píxeles de alto
RESOLUCION <- 200   # ppp (200 = calidad de impresión; 140 = pantalla)

# ── Guardar cada plot registrado ──────────────────────────────────────────────
n_ok  <- 0
n_err <- 0

for (nombre in names(REGISTRO)) {
  
  ruta <- file.path(script_dir, nombre)
  p    <- REGISTRO[[nombre]]
  
  tryCatch({
    
    if (inherits(p, "gg") || inherits(p, "patchwork")) {
      # ── ggplot2 / patchwork → ggsave ──────────────────────────────────────
      ggsave(
        filename  = ruta,
        plot      = p,
        width     = ANCHO_PX / RESOLUCION,   # ggsave trabaja en pulgadas
        height    = ALTO_PX  / RESOLUCION,
        dpi       = RESOLUCION,
        bg        = "white",
        limitsize = FALSE
      )
      
    } else if (inherits(p, "recordedplot")) {
      # ── Gráficos base R grabados con recordPlot() → png device ────────────
      png(ruta,
          width  = ANCHO_PX,
          height = ALTO_PX,
          res    = RESOLUCION,
          bg     = "white")
      replayPlot(p)
      dev.off()
      
    } else {
      warning(sprintf("Tipo no reconocido para '%s': %s", nombre, class(p)[1]))
    }
    
    cat(sprintf("  [OK]  %s\n", nombre))
    n_ok <- n_ok + 1
    
  }, error = function(e) {
    cat(sprintf("  [ERR] %s — %s\n", nombre, e$message))
    n_err <<- n_err + 1
  })
}

# ── Resumen final ─────────────────────────────────────────────────────────────
cat(sprintf("\n  %s\n", strrep("=", 55)))
cat(sprintf("  Guardados correctamente : %d / %d graficos\n",
            n_ok, length(REGISTRO)))
if (n_err > 0)
  cat(sprintf("  Con error               : %d\n", n_err))
cat(sprintf("  Carpeta de destino      : %s\n", script_dir))
cat(sprintf("  Resolucion              : %d ppp  (%dx%d px)\n",
            RESOLUCION, ANCHO_PX, ALTO_PX))
cat(sprintf("  %s\n", strrep("=", 55)))
cat("  PIPELINE COMPLETADO\n")


