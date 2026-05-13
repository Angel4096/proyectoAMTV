# ==============================================================================
# KoTaP v14.0 · Interfaz Shiny — COMPLETA
# Escuela Colombiana de Ingeniería – Métodos Multivariados Clásicos
# ==============================================================================

# ── 0. Paquetes ────────────────────────────────────────────────────────────────
pkgs <- c(
  "shiny", "bslib", "shinycssloaders",
  "tidyverse", "patchwork", "cluster",
  "scales", "ggrepel", "e1071", "data.table",
  "treemap", "corrplot",
  "lmtest", "car", "nortest"
)
nuevos <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(nuevos)) install.packages(nuevos, dependencies = TRUE)
suppressPackageStartupMessages(lapply(pkgs, library, character.only = TRUE))

options(shiny.maxRequestSize = 200 * 1024^2)
set.seed(42)

# ── 1. Paleta y tema global ───────────────────────────────────────────────────
PAL <- list(
  dark   = "#0F172A", navy   = "#1E3A5F", teal   = "#0D9488",
  teal2  = "#0891B2", orange = "#F97316", green  = "#059669",
  red    = "#EF4444", slate  = "#475569", white  = "#FFFFFF",
  purple = "#7C3AED", pink   = "#DB2777", amber  = "#D97706",
  rose   = "#E11D48"
)
HEAT_POS <- "#C0392B"; HEAT_NEU <- "#FFFFFF"; HEAT_NEG <- "#1D4E89"

VAR_COL <- c(
  ROA = "#0D9488", ROE = "#0891B2", SIZE = "#1E3A5F", LEV = "#F97316",
  CFO = "#7C3AED", GRW = "#059669", CUR  = "#DC2626", INVREC = "#0369A1",
  MB  = "#92400E", TQ  = "#065F46", PPE  = "#4C1D95", AGE    = "#831843"
)

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

# ── 2. Funciones auxiliares ───────────────────────────────────────────────────
eliminar_outliers_iqr <- function(data, vars, k = 3) {
  mask <- rep(TRUE, nrow(data))
  for (col in vars) {
    if (!col %in% names(data) || !is.numeric(data[[col]])) next
    q   <- quantile(data[[col]], probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]; if (iqr == 0) next
    mask <- mask &
      !is.na(data[[col]]) &
      data[[col]] >= (q[1] - k * iqr) &
      data[[col]] <= (q[2] + k * iqr)
  }
  data[mask, ]
}

panel_desc_plots <- function(var, data, color) {
  d <- data[[var]]; d <- d[is.finite(d)]
  if (length(d) < 5) return(NULL)
  pos  <- sum(d >= 0); neg <- sum(d < 0)
  pct  <- round(pos / length(d) * 100, 1)
  mu   <- mean(d); md <- median(d); sg <- sd(d)
  asim <- round(e1071::skewness(d), 3)
  kurt <- round(e1071::kurtosis(d), 3)
  cv   <- round(sg / abs(mu) * 100, 2)
  
  df_p <- data.frame(x = d, cl = ifelse(d >= 0, "Ganancia (≥ 0)", "Pérdida (< 0)"))
  
  ph <- ggplot(df_p, aes(x = x, fill = cl)) +
    geom_histogram(bins = 55, boundary = 0, alpha = 0.82, color = "white", linewidth = 0.15) +
    scale_fill_manual(values = c("Ganancia (≥ 0)" = PAL$green, "Pérdida (< 0)" = PAL$red), name = "Signo") +
    geom_vline(xintercept = mu, color = PAL$amber, linewidth = 1.3, linetype = "dashed") +
    geom_vline(xintercept = md, color = PAL$navy,  linewidth = 1.3, linetype = "dotted") +
    annotate("label", x = Inf, y = Inf,
             label = sprintf("Media   = %.4f\nMediana = %.4f\nDesv.   = %.4f\nCV      = %.2f%%\nAsim    = %.3f\nKurt    = %.3f\nn       = %s",
                             mu, md, sg, cv, asim, kurt, format(length(d), big.mark = ",")),
             hjust = 1.05, vjust = 1.05, size = 2.9, fill = "#FFFDE7",
             color = PAL$dark, family = "mono") +
    labs(title = paste(var, "– Distribución"), subtitle = "Set TRAIN | empresa×año", x = var, y = "Frecuencia") +
    tema_k + theme(legend.position = "bottom", legend.key.size = unit(0.4, "cm"))
  
  pb <- ggplot(df_p, aes(x = "", y = x)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = max(d) * 1.1, fill = PAL$green, alpha = .07) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = min(d) * 1.1, ymax = 0, fill = PAL$red, alpha = .07) +
    geom_boxplot(fill = color, alpha = 0.60, color = PAL$rose, linewidth = 1.1,
                 notch = TRUE, notchwidth = 0.25,
                 outlier.alpha = .2, outlier.size = 1.5, outlier.color = color) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 1.0, linetype = "dashed") +
    annotate("label", x = 1.47, y = quantile(d, .75),
             label = sprintf("Verde\n%s\n(%.1f%%)", format(pos, big.mark = ","), pct),
             size = 2.8, color = PAL$green, fontface = "bold", hjust = 0, fill = "white") +
    annotate("label", x = 1.47, y = quantile(d, .25),
             label = sprintf("Rojo\n%s\n(%.1f%%)", format(neg, big.mark = ","), 100 - pct),
             size = 2.8, color = PAL$red, fontface = "bold", hjust = 0, fill = "white") +
    labs(title = paste(var, "– Boxplot [IC 95% mediana]"), y = var, x = NULL) +
    coord_cartesian(xlim = c(0.5, 1.9)) +
    tema_k
  
  qqr <- qqnorm(d, plot.it = FALSE)
  dfq <- data.frame(t = qqr$x, m = qqr$y)
  rq  <- round(cor(dfq$t, dfq$m), 4)
  pq  <- ggplot(dfq, aes(x = t, y = m)) +
    geom_point(color = color, alpha = .40, size = 1.1) +
    geom_abline(slope = sd(d), intercept = mean(d), color = PAL$red, linewidth = 1.8) +
    annotate("label", x = -Inf, y = Inf,
             label = sprintf("r = %.4f\n%s", rq,
                             ifelse(rq > .99, "Muy normal", ifelse(rq > .97, "Aprox. normal", "Colas pesadas"))),
             hjust = -.1, vjust = 1.3, size = 3.5, fill = "#FFF3E0", color = PAL$dark) +
    labs(title = paste(var, "– QQ-Plot"), x = "Cuantiles N(0,1)", y = "Cuantiles muestrales") +
    tema_k
  
  list(h = ph, b = pb, q = pq)
}

# ── 3. UI ─────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = tags$span(tags$b("KoTaP"), " v14.0", style = "color:#0D9488; font-size:1.1em;"),
  theme = bslib::bs_theme(bootswatch = "flatly", primary = "#0D9488"),
  collapsible = TRUE,
  
  # ╔══════════════════════════════════════════════╗
  # ║ TAB 0 – Configuración                       ║
  # ╚══════════════════════════════════════════════╝
  tabPanel("⚙ Configuración",
           sidebarLayout(
             sidebarPanel(width = 4,
                          tags$h4("📂 Cargar Dataset"),
                          fileInput("csv_file", "Archivo CSV (KoTaP_Dataset.csv)", accept = c("text/csv", ".csv")),
                          tags$hr(),
                          tags$h4("🔧 Parámetros"),
                          numericInput("umbral_corr", "Umbral Correlación Spearman", value = 0.90, min = 0.50, max = 1.00, step = 0.01),
                          numericInput("iqr_k",  "Multiplicador IQR outliers (k)", value = 3, min = 1, max = 5, step = 0.5),
                          numericInput("train_year", "Año máximo Train", value = 2022, min = 2010, max = 2023, step = 1),
                          numericInput("test_year",  "Año Set Test",     value = 2023, min = 2011, max = 2024, step = 1),
                          tags$hr(),
                          actionButton("run_all", "▶ Ejecutar Análisis",
                                       class = "btn-success", style = "width:100%; font-weight:bold; font-size:1.1em;"),
                          tags$br(), tags$br(),
                          tags$small(tags$b("Orden lógico:"), " PASO A (lead target) → PASO B (IQR outliers) → Split",
                                     tags$br(),
                                     "Train ≤ ", tags$code("train_year"),
                                     " | Test = ", tags$code("test_year"),
                                     " → predice ROA ", tags$code("test_year + 1"))
             ),
             mainPanel(width = 8,
                       div(style = "background:linear-gradient(135deg,#0D9488,#1E3A5F); color:white; padding:20px 24px; border-radius:8px; margin-bottom:20px;",
                           tags$h3(style = "margin:0 0 6px 0; color:white;", "📊 KoTaP v14.0 – Métodos Multivariados Clásicos"),
                           tags$p(style = "margin:0; opacity:0.88; font-size:0.95em;",
                                  "Escuela Colombiana de Ingeniería | Carga el CSV y haz clic en ", tags$b("▶ Ejecutar Análisis"), " para comenzar.")),
                       tags$h3("Resumen del Dataset"),
                       withSpinner(verbatimTextOutput("resumen_datos"), type = 4, color = "#0D9488"),
                       tags$hr(),
                       tags$h4("Primeras filas (variables clave)"),
                       tableOutput("head_datos")
             )
           )
  ),
  
  # ╔══════════════════════════════════════════════╗
  # ║ TAB 1 – EDA                                 ║
  # ╚══════════════════════════════════════════════╝
  tabPanel("📊 EDA",
           sidebarLayout(
             sidebarPanel(width = 3,
                          tags$h5("Variable para Panel Descriptivo:"),
                          selectInput("eda_var", NULL, choices = NULL),
                          tags$hr(),
                          tags$small(tags$b("Umbral correlación:"), " definido en Configuración.",
                                     tags$br(), "El filtro elimina variables con |rₛ| ≥ umbral."),
                          tags$hr(),
                          uiOutput("eda_estado_ui")
             ),
             mainPanel(width = 9,
                       uiOutput("sin_datos_eda"),
                       tabsetPanel(
                         tabPanel("Tabla Asimetría & Curtosis",
                                  br(),
                                  withSpinner(verbatimTextOutput("tabla_eda_txt"), type = 4, color = "#0D9488"),
                                  tableOutput("tabla_eda_tbl")),
                         tabPanel("Panel Descriptivo",
                                  br(),
                                  tags$h5("Distribución (histograma):"),
                                  withSpinner(plotOutput("panel_desc_hist", height = "340px"), type = 4),
                                  tags$h5("Boxplot:"),
                                  plotOutput("panel_desc_box", height = "340px"),
                                  tags$h5("QQ-Plot:"),
                                  plotOutput("panel_desc_qq", height = "340px")),
                         tabPanel("Correlación & Filtro",
                                  br(),
                                  withSpinner(plotOutput("heatmap_corr", height = "620px"), type = 4),
                                  verbatimTextOutput("filtro_corr_txt")),
                         tabPanel("Top Corr. con target_ROA",
                                  br(),
                                  withSpinner(plotOutput("corr_target_plot", height = "520px"), type = 4)),
                         tabPanel("Grid Histogramas EDA",
                                  br(),
                                  withSpinner(plotOutput("eda_grid", height = "700px"), type = 4))
                       )
             )
           )
  ),
  
  # ╔══════════════════════════════════════════════╗
  # ║ TAB 2 – PCA                                 ║
  # ╚══════════════════════════════════════════════╝
  tabPanel("🔬 PCA",
           fluidPage(
             uiOutput("sin_datos_pca"),
             verbatimTextOutput("pca_txt"),
             tabsetPanel(
               tabPanel("Varianza Explicada",
                        br(), withSpinner(plotOutput("pca_var", height = "480px"), type = 4)),
               tabPanel("Mapa de Calor Loadings",
                        br(), withSpinner(plotOutput("pca_heatmap", height = "520px"), type = 4)),
               tabPanel("Biplot PC1×PC2",
                        br(), withSpinner(plotOutput("pca_biplot_12", height = "600px"), type = 4)),
               tabPanel("Biplot PC1×PC3",
                        br(), withSpinner(plotOutput("pca_biplot_13", height = "600px"), type = 4)),
               tabPanel("Biplot PC2×PC3",
                        br(), withSpinner(plotOutput("pca_biplot_23", height = "600px"), type = 4)),
               tabPanel("Panel Biplots",
                        br(), withSpinner(plotOutput("pca_biplot_panel", height = "620px"), type = 4))
             )
           )
  ),
  
  # ╔══════════════════════════════════════════════╗
  # ║ TAB 3 – K-Means                             ║
  # ╚══════════════════════════════════════════════╝
  tabPanel("🔵 K-Means",
           sidebarLayout(
             sidebarPanel(width = 3,
                          numericInput("k_max", "K máximo a evaluar", value = 9, min = 3, max = 15),
                          tags$hr(),
                          tags$p(tags$b("K fijo: 4"), " (recomendado por el análisis)"),
                          checkboxInput("k_override", "Especificar K manualmente", value = FALSE),
                          numericInput("k_manual", "K manual", value = 4, min = 2, max = 10),
                          tags$hr(),
                          withSpinner(verbatimTextOutput("kmeans_txt"), type = 4, color = "#0D9488")
             ),
             mainPanel(width = 9,
                       uiOutput("sin_datos_kmeans"),
                       tabsetPanel(
                         tabPanel("Método del Codo",
                                  br(), withSpinner(plotOutput("kmeans_codo", height = "430px"), type = 4)),
                         tabPanel("Clusters PC1×PC2",
                                  br(), withSpinner(plotOutput("kmeans_2d_12", height = "520px"), type = 4)),
                         tabPanel("Clusters PC1×PC3",
                                  br(), withSpinner(plotOutput("kmeans_2d_13", height = "520px"), type = 4)),
                         tabPanel("Clusters PC2×PC3",
                                  br(), withSpinner(plotOutput("kmeans_2d_23", height = "520px"), type = 4)),
                         tabPanel("Panel Clusters",
                                  br(), withSpinner(plotOutput("kmeans_panel", height = "540px"), type = 4)),
                         tabPanel("Perfiles por Cluster",
                                  br(), withSpinner(plotOutput("kmeans_perfiles", height = "500px"), type = 4)),
                         tabPanel("Heatmap Centroides",
                                  br(), withSpinner(plotOutput("kmeans_centroides", height = "440px"), type = 4))
                       )
             )
           )
  ),
  
  # ╔══════════════════════════════════════════════╗
  # ║ TAB 4 – OLS                                 ║
  # ╚══════════════════════════════════════════════╝
  tabPanel("📈 OLS",
           fluidPage(
             uiOutput("sin_datos_ols"),
             verbatimTextOutput("ols_summary_txt"),
             tabsetPanel(
               tabPanel("Coeficientes β",
                        br(), withSpinner(plotOutput("ols_coefs", height = "500px"), type = 4)),
               tabPanel("VIF Inicial (25 vars)",
                        br(),
                        verbatimTextOutput("vif_ini_txt"),
                        withSpinner(plotOutput("diag_vif_ini", height = "440px"), type = 4)),
               tabPanel("VIF Final (post-bucle)",
                        br(),
                        verbatimTextOutput("vif_txt"),
                        withSpinner(plotOutput("diag_vif", height = "420px"), type = 4)),
               tabPanel("Normalidad Residuos (JB)",
                        br(),
                        verbatimTextOutput("jb_txt"),
                        fluidRow(
                          column(6, withSpinner(plotOutput("diag_hist_res", height = "380px"), type = 4)),
                          column(6, plotOutput("diag_qq", height = "380px"))
                        )),
               tabPanel("Homocedasticidad (BP)",
                        br(),
                        verbatimTextOutput("bp_txt"),
                        withSpinner(plotOutput("diag_sl", height = "420px"), type = 4)),
               tabPanel("Autocorrelación / Influencia",
                        br(),
                        verbatimTextOutput("dw_txt"),
                        fluidRow(
                          column(6, withSpinner(plotOutput("diag_cook", height = "380px"), type = 4)),
                          column(6, plotOutput("diag_rvl", height = "380px"))
                        )),
               tabPanel("Panel 4-en-1",
                        br(), withSpinner(plotOutput("diag_4en1", height = "680px"), type = 4))
             )
           )
  ),
  
  # ╔══════════════════════════════════════════════╗
  # ║ TAB 5 – Evaluación Test                     ║
  # ╚══════════════════════════════════════════════╝
  tabPanel("🎯 Test",
           fluidPage(
             br(),
             uiOutput("sin_datos_test"),
             verbatimTextOutput("test_metricas_txt"),
             tabsetPanel(
               tabPanel("Gráfico A: Predicho vs Real",
                        br(), withSpinner(plotOutput("eval_A", height = "520px"), type = 4)),
               tabPanel("Gráfico B: Densidad Real vs Predicho",
                        br(), withSpinner(plotOutput("eval_B", height = "480px"), type = 4))
             )
           )
  ),
  
  # ╔══════════════════════════════════════════════╗
  # ║ TAB 6 – Treemaps                            ║
  # ╚══════════════════════════════════════════════╝
  tabPanel("🗺 Treemaps",
           sidebarLayout(
             sidebarPanel(width = 3,
                          tags$h5("Año:"),
                          selectInput("treemap_year", NULL, choices = 2020:2024, selected = 2024),
                          tags$hr(),
                          tags$p(tags$b("Interpretación:")),
                          tags$ul(
                            tags$li(tags$span("Verde: ROA real > 0 (creó riqueza)", style = "color:#059669")),
                            tags$li(tags$span("Rojo: ROA real < 0 (destruyó riqueza)", style = "color:#EF4444")),
                            tags$li("Tamaño = activos (SIZE)")
                          ),
                          tags$small("ROA Real = ROA − Inflación oficial KR")
             ),
             mainPanel(width = 9,
                       withSpinner(plotOutput("treemap_plot", height = "660px"), type = 4)
             )
           )
  )
)

# ── 4. Server ─────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ══════════════════════════════════════════════════════════════════════════
  # REACTIVO 1 – Carga y preparación del dataset
  # ══════════════════════════════════════════════════════════════════════════
  datos_r <- eventReactive(input$run_all, {
    req(input$csv_file)
    path <- input$csv_file$datapath
    
    df_raw <- NULL
    for (enc in c("unknown", "UTF-8", "UTF-8-BOM", "latin1")) {
      ok <- tryCatch({
        tmp <- if (enc == "unknown")
          data.table::fread(path, encoding = enc, data.table = FALSE)
        else
          read.csv(path, stringsAsFactors = FALSE, fileEncoding = enc)
        if (nrow(tmp) > 0) { df_raw <- tmp; TRUE } else FALSE
      }, error = function(e) FALSE, warning = function(w) !is.null(df_raw) && nrow(df_raw) > 0)
      if (isTRUE(ok) && nrow(df_raw) > 0) break
    }
    validate(need(!is.null(df_raw) && nrow(df_raw) > 0, "No se pudo cargar el CSV. Verifica el formato."))
    
    VARS_FACTOR <- intersect(c("KOSPI", "big4", "ind", "LOSS"), names(df_raw))
    for (v in VARS_FACTOR) df_raw[[v]] <- as.factor(df_raw[[v]])
    cols_num <- setdiff(names(df_raw), c(VARS_FACTOR, "name"))
    for (col in cols_num) df_raw[[col]] <- suppressWarnings(as.numeric(df_raw[[col]]))
    df_raw <- df_raw[!duplicated(df_raw), ]
    
    # PASO A – target_ROA = lead(ROA, 1) por empresa
    df <- df_raw |>
      group_by(stock) |>
      arrange(year, .by_group = TRUE) |>
      mutate(target_ROA = lead(ROA, n = 1)) |>
      ungroup()
    
    # PASO B – Outliers IQR × k
    VARS_IQR <- intersect(c("ROA","SIZE","LEV","ROE","CFO","GRW","CUR","INVREC","MB","TQ","PPE","AGE"), names(df))
    df <- eliminar_outliers_iqr(df, VARS_IQR, k = input$iqr_k)
    
    for (v in VARS_FACTOR)
      if (v %in% names(df) && is.factor(df[[v]])) df[[v]] <- droplevels(df[[v]])
    
    df_train  <- df |> filter(year <= input$train_year, !is.na(target_ROA))
    df_test   <- df |> filter(year == input$test_year,  !is.na(target_ROA))
    df_futuro <- df |> filter(year == input$test_year + 1)
    
    KEY_VARS <- intersect(c("SIZE","LEV","ROA","ROE","CFO","GRW","CUR","INVREC","MB","TQ","PPE","AGE"), names(df))
    
    VARS_CAND <- intersect(
      c("SIZE","LEV","CUR","GRW","CFO","PPE","AGE","INVREC","MB","TQ",
        "GETR","CETR","GETR3","CETR3","GETR5","CETR5",
        "TSTA","TSDA","A_GETR","A_CETR","A_GETR3","A_CETR3","A_GETR5","A_CETR5",
        "forn","own","KOSPI","big4","LOSS"),
      names(df)
    )
    
    list(df = df, df_train = df_train, df_test = df_test, df_futuro = df_futuro,
         KEY_VARS = KEY_VARS, VARS_CAND = VARS_CAND, VARS_FACTOR = VARS_FACTOR)
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # REACTIVO 2 – EDA + Filtro de Colinealidad
  # ══════════════════════════════════════════════════════════════════════════
  eda_r <- reactive({
    d         <- datos_r()
    df_train  <- d$df_train
    KEY_VARS  <- d$KEY_VARS
    VARS_CAND <- d$VARS_CAND
    UMBRAL    <- input$umbral_corr
    
    tabla_eda <- map_dfr(KEY_VARS, function(var) {
      x <- df_train[[var]]; x <- x[is.finite(x)]
      tibble(Variable = var, n = length(x), Media = mean(x), Mediana = median(x),
             Desv = sd(x), CV_pct = sd(x) / abs(mean(x)) * 100,
             Asimetria = e1071::skewness(x), Curtosis = e1071::kurtosis(x))
    })
    
    vars_num_cand <- VARS_CAND[sapply(VARS_CAND, function(v)
      v %in% names(df_train) && is.numeric(df_train[[v]]))]
    
    mat_data <- df_train[, vars_num_cand, drop = FALSE]
    mat_data <- as.data.frame(lapply(mat_data, function(x) suppressWarnings(as.numeric(x))))
    vok <- apply(mat_data, 2, function(x) { vv <- var(x, na.rm = TRUE); !is.na(vv) && is.finite(vv) && vv > 1e-10 })
    vars_num_cand <- names(which(vok))
    mat_data <- mat_data[, vars_num_cand, drop = FALSE]
    mat_corr <- cor(mat_data, use = "pairwise.complete.obs", method = "spearman")
    
    # Filtro colinealidad algorítmico
    variancias <- sapply(vars_num_cand, function(v) var(df_train[[v]], na.rm = TRUE))
    vars_ord   <- vars_num_cand[order(variancias, decreasing = TRUE)]
    eliminar   <- c()
    for (i in seq_along(vars_ord)) {
      v <- vars_ord[i]; if (v %in% eliminar) next
      for (j in seq_along(vars_ord)) {
        u <- vars_ord[j]
        if (u == v || u %in% eliminar || i >= j) next
        if (!is.na(mat_corr[v, u]) && abs(mat_corr[v, u]) >= UMBRAL)
          eliminar <- c(eliminar, u)
      }
    }
    VARS_FILTRADAS <- setdiff(vars_ord, eliminar)
    
    # Correlaciones con target_ROA
    corr_target <- NULL
    if ("target_ROA" %in% names(df_train)) {
      vars_sin_t <- VARS_FILTRADAS[VARS_FILTRADAS != "target_ROA"]
      df_ext <- df_train |>
        mutate(across(where(is.factor), ~ as.numeric(as.character(.x)))) |>
        select(all_of(c(vars_sin_t, "target_ROA")))
      ct <- cor(df_ext, method = "spearman", use = "pairwise.complete.obs")
      if ("target_ROA" %in% rownames(ct)) {
        vals <- ct["target_ROA", vars_sin_t]
        vals <- vals[order(abs(vals), decreasing = TRUE)][1:min(20, length(vals))]
        corr_target <- data.frame(Variable = factor(names(vals), levels = rev(names(vals))),
                                  r = as.numeric(vals)) |>
          mutate(signo = ifelse(r > 0, "Positiva", "Negativa"))
      }
    }
    
    list(tabla_eda = tabla_eda, mat_corr = mat_corr,
         VARS_FILTRADAS = VARS_FILTRADAS, eliminar = eliminar,
         vars_num_cand = vars_num_cand, corr_target = corr_target)
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # REACTIVO 3 – PCA
  # ══════════════════════════════════════════════════════════════════════════
  pca_r <- reactive({
    d  <- datos_r(); ed <- eda_r()
    df_train       <- d$df_train
    VARS_FILTRADAS <- ed$VARS_FILTRADAS
    
    vars_pca <- intersect(VARS_FILTRADAS, names(df_train))
    vars_pca <- vars_pca[sapply(vars_pca, function(v) is.numeric(df_train[[v]]))]
    
    X_raw     <- df_train[, vars_pca, drop = FALSE]
    X_raw     <- X_raw[complete.cases(X_raw), ]
    row_idx   <- as.integer(rownames(X_raw))
    X_raw_num <- as.data.frame(lapply(X_raw, as.numeric))
    
    pca    <- prcomp(X_raw_num, center = TRUE, scale. = TRUE)
    ve     <- (pca$sdev^2) / sum(pca$sdev^2)
    ve_cum <- cumsum(ve)
    
    n_kaiser <- sum(pca$sdev^2 >= 1)
    n_80     <- which(ve_cum >= 0.80)[1]
    n_pcs    <- max(c(n_kaiser, if (!is.na(n_80)) n_80 else 2, 2), na.rm = TRUE)
    
    list(pca = pca, ve = ve, ve_cum = ve_cum, n_pcs = n_pcs,
         n_kaiser = n_kaiser, n_80 = n_80,
         X_raw = X_raw_num, vars_pca = vars_pca, row_idx = row_idx)
  })
  
  # Función biplot reutilizable
  hacer_biplot <- function(pr, pc_x, pc_y) {
    pca <- pr$pca; ve <- pr$ve
    sc  <- as.data.frame(pca$x[, c(pc_x, pc_y)]); colnames(sc) <- c("PCx","PCy")
    sf  <- max(abs(sc)) * 0.45
    rot <- pca$rotation[pr$vars_pca, c(pc_x, pc_y), drop = FALSE]
    adf <- data.frame(Variable = pr$vars_pca, x0 = 0, y0 = 0,
                      x1 = rot[, 1] * sf, y1 = rot[, 2] * sf)
    adf$contrib <- sqrt(adf$x1^2 + adf$y1^2)
    
    ggplot(sc, aes(x = PCx, y = PCy)) +
      geom_point(color = PAL$teal, alpha = .20, size = 0.7) +
      geom_hline(yintercept = 0, color = "gray55", linewidth = 0.8, linetype = "dashed") +
      geom_vline(xintercept = 0, color = "gray55", linewidth = 0.8, linetype = "dashed") +
      geom_segment(data = adf, aes(x = x0, y = y0, xend = x1, yend = y1, color = contrib),
                   arrow = arrow(length = unit(.28, "cm"), type = "closed"), linewidth = 1.3) +
      scale_color_gradient(low = PAL$teal2, high = PAL$orange, name = "Contribución") +
      geom_label_repel(data = adf, aes(x = x1 * 1.22, y = y1 * 1.22, label = Variable),
                       size = 3.6, fontface = "bold", fill = "white", color = PAL$dark,
                       box.padding = 0.30, segment.color = PAL$slate, max.overlaps = 25) +
      labs(title    = sprintf("Biplot PC%d × PC%d | %.1f%% + %.1f%% = %.1f%%",
                              pc_x, pc_y, ve[pc_x]*100, ve[pc_y]*100, (ve[pc_x]+ve[pc_y])*100),
           x = sprintf("PC%d – %.1f%% varianza", pc_x, ve[pc_x]*100),
           y = sprintf("PC%d – %.1f%% varianza", pc_y, ve[pc_y]*100)) +
      tema_k + theme(legend.position = "right")
  }
  
  # ══════════════════════════════════════════════════════════════════════════
  # REACTIVO 4 – K-Means
  # ══════════════════════════════════════════════════════════════════════════
  kmeans_r <- reactive({
    pr <- pca_r(); d <- datos_r()
    
    nkm <- min(max(pr$n_kaiser, 3), ncol(pr$pca$x))
    nkm <- min(nkm, 4)
    Xkm <- pr$pca$x[, seq_len(nkm), drop = FALSE]
    
    Kr  <- 2:input$k_max
    ine <- numeric(length(Kr))
    for (i in seq_along(Kr))
      ine[i] <- kmeans(Xkm, centers = Kr[i], nstart = 25, iter.max = 300)$tot.withinss
    
    codo_k <- Kr[which.min(diff(ine)) + 1]
    
    ok_k <- if (input$k_override) {
      input$k_manual
    } else {
      4L   # K=4 fijo según análisis
    }
    ok_k <- max(2L, min(as.integer(ok_k), input$k_max))
    
    km  <- kmeans(Xkm, centers = ok_k, nstart = 30, iter.max = 500)
    lkm <- km$cluster
    cc  <- c(PAL$teal, PAL$orange, PAL$navy, PAL$purple,
             PAL$green, PAL$red, PAL$teal2, PAL$pink)[seq_len(ok_k)]
    names(cc) <- as.character(seq_len(ok_k))
    
    pvx  <- intersect(c("ROA","CFO","LEV","SIZE","GRW"), names(d$df_train))
    dfpr <- d$df_train[pr$row_idx, pvx, drop = FALSE]
    dfpr$cluster <- factor(lkm)
    
    list(km = km, lkm = lkm, ok_k = ok_k, cc = cc, Kr = Kr, ine = ine,
         codo_k = codo_k, Xkm = Xkm, dfpr = dfpr, pvx = pvx, ve = pr$ve, nkm = nkm)
  })
  
  # Función scatter clusters reutilizable
  hacer_scatter_cl <- function(km_res, pc_x, pc_y) {
    ve   <- km_res$ve
    dk2d <- data.frame(PCx = km_res$Xkm[, pc_x], PCy = km_res$Xkm[, pc_y], cl = factor(km_res$lkm))
    c2d  <- as.data.frame(km_res$km$centers[, c(pc_x, pc_y)])
    colnames(c2d) <- c("PCx","PCy"); c2d$cl <- factor(seq_len(km_res$ok_k)); c2d$lb <- paste0("C", seq_len(km_res$ok_k))
    ofs <- max(abs(km_res$Xkm[, pc_y])) * 0.08
    
    ggplot(dk2d, aes(x = PCx, y = PCy, color = cl)) +
      geom_point(alpha = .28, size = 1.2) +
      geom_point(data = c2d, aes(x = PCx, y = PCy), inherit.aes = FALSE,
                 shape = 8, size = 9, stroke = 2.5, color = "black") +
      geom_label(data = c2d, aes(x = PCx, y = PCy + ofs, label = lb), inherit.aes = FALSE,
                 fontface = "bold", size = 4.5, fill = "white") +
      scale_color_manual(values = km_res$cc, guide = "none") +
      geom_hline(yintercept = 0, color = "gray55", linewidth = .7) +
      geom_vline(xintercept = 0, color = "gray55", linewidth = .7) +
      labs(title    = sprintf("K-Means K=%d | PC%d × PC%d", km_res$ok_k, pc_x, pc_y),
           subtitle = sprintf("%d obs | ⋆ = centroide", nrow(dk2d)),
           x = sprintf("PC%d (%.1f%%)", pc_x, ve[pc_x]*100),
           y = sprintf("PC%d (%.1f%%)", pc_y, ve[pc_y]*100)) +
      tema_k + theme(legend.position = "none")
  }
  
  # ══════════════════════════════════════════════════════════════════════════
  # REACTIVO 5 – OLS
  # ══════════════════════════════════════════════════════════════════════════
  ols_r <- reactive({
    d  <- datos_r(); ed <- eda_r()
    VARS_FILTRADAS <- ed$VARS_FILTRADAS
    
    df_ols_train <- d$df_train |>
      select(all_of(c("target_ROA", "year", VARS_FILTRADAS, "ind"))) |>
      mutate(ind = as.factor(ind)) |>
      na.omit()
    df_ols_train$ind <- droplevels(df_ols_train$ind)
    
    # VIF inicial (antes del bucle)
    vars_ini <- VARS_FILTRADAS
    f_ini    <- as.formula(paste("target_ROA ~", paste(vars_ini, collapse = " + "), "+ ind"))
    mod_ini  <- lm(f_ini, data = df_ols_train)
    
    vif_ini_df <- tryCatch({
      vv <- car::vif(mod_ini)
      vv <- if (is.matrix(vv)) vv[,1] else vv
      data.frame(Variable = names(vv), VIF = as.numeric(vv)) |>
        filter(!grepl("^ind", Variable)) |> arrange(desc(VIF))
    }, error = function(e) NULL)
    
    # Bucle VIF iterativo
    vars_ols_activas <- VARS_FILTRADAS
    make_formula <- function(vars) as.formula(paste("target_ROA ~", paste(vars, collapse = " + "), "+ ind"))
    modelo_ols <- lm(make_formula(vars_ols_activas), data = df_ols_train)
    
    iter_vif <- 0L; eliminadas_vif <- c()
    repeat {
      iter_vif <- iter_vif + 1L
      vif_actual <- tryCatch(car::vif(modelo_ols), error = function(e) NULL)
      if (is.null(vif_actual)) break
      vif_vec  <- if (is.matrix(vif_actual)) vif_actual[, 1] else vif_actual
      vif_cont <- vif_vec[!grepl("^ind", names(vif_vec))]
      if (max(vif_cont, na.rm = TRUE) <= 10) break
      var_el           <- names(which.max(vif_cont))
      eliminadas_vif   <- c(eliminadas_vif, var_el)
      vars_ols_activas <- setdiff(vars_ols_activas, var_el)
      modelo_ols       <- lm(make_formula(vars_ols_activas), data = df_ols_train)
      if (iter_vif > 50L) break
    }
    
    smry     <- summary(modelo_ols)
    res      <- residuals(modelo_ols)
    fv       <- fitted(modelo_ols)
    res_std  <- rstandard(modelo_ols)
    
    bp_test  <- lmtest::bptest(modelo_ols)
    dw_test  <- lmtest::dwtest(modelo_ols, order.by = df_ols_train$year)
    
    # Jarque-Bera (calculado manualmente con e1071)
    n_res   <- length(res[is.finite(res)])
    s_res   <- e1071::skewness(res[is.finite(res)])
    k_res   <- e1071::kurtosis(res[is.finite(res)])
    jb_stat <- (n_res / 6) * (s_res^2 + (k_res^2) / 4)
    jb_pval <- pchisq(jb_stat, df = 2, lower.tail = FALSE)
    
    vif_vals <- tryCatch(car::vif(modelo_ols), error = function(e) NULL)
    vif_df <- NULL
    if (!is.null(vif_vals)) {
      vif_v  <- if (is.matrix(vif_vals)) vif_vals[, 1] else vif_vals
      vif_df <- data.frame(Variable = names(vif_v), VIF = as.numeric(vif_v)) |>
        filter(!grepl("^ind", Variable)) |> arrange(desc(VIF))
    }
    
    n_obs       <- nrow(df_ols_train); p_mod <- length(coef(modelo_ols))
    cook_d      <- cooks.distance(modelo_ols)
    hat_v       <- hatvalues(modelo_ols)
    umbral_cook <- 4 / n_obs; umbral_hat <- 2 * p_mod / n_obs
    df_inf <- data.frame(obs = seq_along(cook_d), cook = cook_d, hat = hat_v, res_std = res_std,
                         alto_cook = cook_d > umbral_cook, alto_hat = hat_v > umbral_hat)
    
    ctbl     <- smry$coefficients
    df_betas <- data.frame(Variable = rownames(ctbl), Beta = ctbl[,1], SE = ctbl[,2], p = ctbl[,4]) |>
      filter(!grepl("Intercept|^ind", Variable)) |>
      mutate(lo = Beta - 1.96*SE, hi = Beta + 1.96*SE,
             sig  = ifelse(p < .05, "Significativo (p<0.05)", "No significativo"),
             lbl  = sprintf("%.5f%s", Beta,
                            ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))) |>
      arrange(desc(abs(Beta))) |>
      mutate(Variable = factor(Variable, levels = Variable))
    
    list(modelo_ols = modelo_ols, smry = smry, df_ols_train = df_ols_train,
         vars_ols_activas = vars_ols_activas, eliminadas_vif = eliminadas_vif,
         res = res, fv = fv, res_std = res_std,
         bp_test = bp_test, dw_test = dw_test,
         jb_stat = jb_stat, jb_pval = jb_pval, s_res = s_res, k_res = k_res,
         vif_df = vif_df, vif_ini_df = vif_ini_df, df_inf = df_inf, df_betas = df_betas,
         umbral_cook = umbral_cook, umbral_hat = umbral_hat, n_obs = n_obs,
         VARS_FILTRADAS = VARS_FILTRADAS)
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # REACTIVO 6 – Evaluación Test
  # ══════════════════════════════════════════════════════════════════════════
  test_r <- reactive({
    d <- datos_r(); ol <- ols_r(); ed <- eda_r()
    
    df_ols_test <- d$df_test |>
      select(all_of(c("target_ROA", "year", ed$VARS_FILTRADAS, "ind"))) |>
      mutate(ind = as.factor(ind)) |>
      na.omit()
    
    niveles_modelo <- ol$modelo_ols$xlevels[["ind"]]
    df_ols_test <- df_ols_test |>
      filter(as.character(ind) %in% niveles_modelo) |>
      mutate(ind = factor(as.character(ind), levels = niveles_modelo))
    
    y_pred_test <- predict(ol$modelo_ols, newdata = df_ols_test)
    y_real_test <- df_ols_test$target_ROA
    ok          <- !is.na(y_pred_test) & !is.na(y_real_test)
    
    y_pred_ok <- y_pred_test[ok]; y_real_ok <- y_real_test[ok]
    rmse_test <- sqrt(mean((y_real_ok - y_pred_ok)^2))
    mae_test  <- mean(abs(y_real_ok - y_pred_ok))
    ss_res    <- sum((y_real_ok - y_pred_ok)^2)
    ss_tot    <- sum((y_real_ok - mean(y_real_ok))^2)
    r2_test   <- 1 - ss_res / ss_tot
    
    list(y_pred_ok = y_pred_ok, y_real_ok = y_real_ok,
         rmse_test = rmse_test, mae_test = mae_test, r2_test = r2_test,
         r2_train = ol$smry$r.squared, n_ok = sum(ok))
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # REACTIVO 7 – Treemaps
  # ══════════════════════════════════════════════════════════════════════════
  inflacion_kr <- data.frame(year = 2020:2024,
                             inflacion = c(0.0054, 0.0250, 0.0509, 0.0360, 0.0232))
  
  treemap_base_r <- reactive({
    d  <- datos_r(); df <- d$df
    df |>
      filter(year %in% 2020:2024, !is.na(ROA), !is.na(SIZE), is.finite(ROA), is.finite(SIZE), !is.na(name)) |>
      group_by(stock, name, year) |>
      summarise(ROA = mean(ROA, na.rm = TRUE), activos = mean(exp(SIZE), na.rm = TRUE), .groups = "drop") |>
      mutate(
        stock_char = as.character(stock),
        name_clean = case_when(
          stock == 5930  ~ "SAMSUNG ELECTRONICS", stock == 660   ~ "SK HYNIX",
          stock == 5380  ~ "HYUNDAI MOTORS",       stock == 270   ~ "KIA MOTORS",
          stock == 35420 ~ "NAVER",                stock == 35720 ~ "KAKAO",
          stock == 51910 ~ "LG CHEMICAL",          stock == 6400  ~ "SAMSUNG SDI",
          TRUE ~ iconv(as.character(name), to = "UTF-8", sub = "")
        ),
        name_clean = ifelse(name_clean == "" | is.na(name_clean), paste0("ID:", stock_char), name_clean)
      ) |>
      left_join(inflacion_kr, by = "year") |>
      mutate(ROA_Real = ROA - inflacion)
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # HELPER – Sin datos / Estado
  # ══════════════════════════════════════════════════════════════════════════
  datos_cargados <- reactive({ tryCatch({ datos_r(); TRUE }, error = function(e) FALSE) })
  
  alerta_sin_datos <- function() {
    div(style = "background:#FFF3CD; border-left:5px solid #FFC107; padding:20px; border-radius:6px; margin:20px 0; font-size:1.05em;",
        tags$b("⚠️  Sin datos cargados"), tags$br(), tags$br(),
        "Para ver los resultados sigue estos pasos:",
        tags$ol(
          tags$li("Ve a la pestaña ", tags$b("⚙ Configuración")),
          tags$li("Sube tu archivo ", tags$code("KoTaP_Dataset.csv")),
          tags$li("Ajusta los parámetros si es necesario"),
          tags$li("Haz clic en ", tags$b("▶ Ejecutar Análisis"))))
  }
  
  output$sin_datos_eda    <- renderUI({ if (!datos_cargados()) alerta_sin_datos() })
  output$sin_datos_pca    <- renderUI({ if (!datos_cargados()) alerta_sin_datos() })
  output$sin_datos_kmeans <- renderUI({ if (!datos_cargados()) alerta_sin_datos() })
  output$sin_datos_ols    <- renderUI({ if (!datos_cargados()) alerta_sin_datos() })
  output$sin_datos_test   <- renderUI({ if (!datos_cargados()) alerta_sin_datos() })
  
  output$eda_estado_ui <- renderUI({
    if (datos_cargados()) {
      d <- datos_r()
      div(style = "background:#D4EDDA; border-left:4px solid #28A745; padding:10px; border-radius:4px; font-size:0.9em;",
          tags$b("✅ Datos cargados"), tags$br(),
          sprintf("%d obs | %d–%d", nrow(d$df), min(d$df$year), max(d$df$year)))
    }
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # OUTPUTS – Configuración
  # ══════════════════════════════════════════════════════════════════════════
  output$resumen_datos <- renderPrint({
    req(datos_r()); d <- datos_r(); df <- d$df
    cat(sprintf("Panel completo  : %d obs | %d empresas | %d–%d\n",
                nrow(df), length(unique(df$stock)), min(df$year), max(df$year)))
    cat(sprintf("Train (≤%d)  : %d obs\n", input$train_year, nrow(d$df_train)))
    cat(sprintf("Test  (%d)    : %d obs\n",  input$test_year,  nrow(d$df_test)))
    cat(sprintf("Futuro(%d)    : %d obs\n",  input$test_year + 1, nrow(d$df_futuro)))
    cat(sprintf("\nKEY_VARS  (%d): %s\n", length(d$KEY_VARS), paste(d$KEY_VARS, collapse = ", ")))
    cat(sprintf("VARS_CAND (%d): %s\n",  length(d$VARS_CAND), paste(d$VARS_CAND, collapse = ", ")))
  })
  
  output$head_datos <- renderTable({
    req(datos_r())
    cols_vis <- intersect(c("stock","name","year","ROA","SIZE","LEV","CFO","GRW"), names(datos_r()$df))
    head(datos_r()$df[, cols_vis], 10)
  })
  
  observe({
    req(datos_r())
    updateSelectInput(session, "eda_var", choices = datos_r()$KEY_VARS, selected = datos_r()$KEY_VARS[1])
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # OUTPUTS – EDA
  # ══════════════════════════════════════════════════════════════════════════
  output$tabla_eda_txt <- renderPrint({
    req(eda_r()); ed <- eda_r()
    cat(sprintf("  %-8s  %8s  %8s  %8s  %8s  %8s  %8s  %8s\n",
                "Variable","n","Media","Mediana","Desv.","CV%","Asim.","Kurt."))
    cat("  ", strrep("-", 72), "\n", sep = "")
    for (i in seq_len(nrow(ed$tabla_eda))) {
      r <- ed$tabla_eda[i,]
      cat(sprintf("  %-8s  %8s  %8.4f  %8.4f  %8.4f  %8.1f  %8.3f  %8.3f\n",
                  r$Variable, format(r$n, big.mark=","), r$Media, r$Mediana, r$Desv, r$CV_pct, r$Asimetria, r$Curtosis))
    }
    cat(sprintf("\nEliminadas por |rₛ| ≥ %.2f: %s\n", input$umbral_corr,
                if (length(ed$eliminar) > 0) paste(ed$eliminar, collapse = ", ") else "ninguna"))
    cat(sprintf("VARS_FILTRADAS (%d): %s\n", length(ed$VARS_FILTRADAS), paste(ed$VARS_FILTRADAS, collapse = ", ")))
  })
  
  output$tabla_eda_tbl <- renderTable({
    req(eda_r())
    eda_r()$tabla_eda |> mutate(across(where(is.numeric), ~ round(.x, 4)))
  })
  
  output$panel_desc_hist <- renderPlot({
    req(datos_r(), input$eda_var); var <- input$eda_var; df <- datos_r()$df_train
    if (!var %in% names(df)) return(NULL)
    col <- if (!is.na(VAR_COL[var])) VAR_COL[var] else PAL$teal
    res <- panel_desc_plots(var, df, col); if (!is.null(res)) print(res$h)
  })
  
  output$panel_desc_box <- renderPlot({
    req(datos_r(), input$eda_var); var <- input$eda_var; df <- datos_r()$df_train
    if (!var %in% names(df)) return(NULL)
    col <- if (!is.na(VAR_COL[var])) VAR_COL[var] else PAL$teal
    res <- panel_desc_plots(var, df, col); if (!is.null(res)) print(res$b)
  })
  
  output$panel_desc_qq <- renderPlot({
    req(datos_r(), input$eda_var); var <- input$eda_var; df <- datos_r()$df_train
    if (!var %in% names(df)) return(NULL)
    col <- if (!is.na(VAR_COL[var])) VAR_COL[var] else PAL$teal
    res <- panel_desc_plots(var, df, col); if (!is.null(res)) print(res$q)
  })
  
  output$heatmap_corr <- renderPlot({
    req(eda_r()); mat <- eda_r()$mat_corr
    corrplot::corrplot(mat, method = "color", type = "upper",
                       col = colorRampPalette(c(HEAT_NEG, HEAT_NEU, HEAT_POS))(200),
                       tl.cex = 0.75, tl.col = PAL$dark,
                       addCoef.col = "black", number.cex = 0.50, cl.cex = 0.75,
                       title = sprintf("Correlación Spearman | Umbral = %.2f", input$umbral_corr),
                       mar = c(0, 0, 2, 0))
  })
  
  output$filtro_corr_txt <- renderPrint({
    req(eda_r()); ed <- eda_r()
    cat(sprintf("Eliminadas por |r| ≥ %.2f: %d variable(s)\n  → %s\n",
                input$umbral_corr, length(ed$eliminar),
                if (length(ed$eliminar) > 0) paste(ed$eliminar, collapse = ", ") else "ninguna"))
    cat(sprintf("VARS_FILTRADAS (%d): %s\n", length(ed$VARS_FILTRADAS), paste(ed$VARS_FILTRADAS, collapse = ", ")))
  })
  
  output$corr_target_plot <- renderPlot({
    req(eda_r()); ct <- eda_r()$corr_target
    if (is.null(ct)) { plot.new(); text(0.5, 0.5, "target_ROA no disponible", cex = 1.5); return() }
    ggplot(ct, aes(x = r, y = Variable, fill = signo)) +
      geom_col(alpha = 0.85, width = 0.72) +
      geom_vline(xintercept = 0, color = "gray30", linewidth = 0.8) +
      geom_text(aes(label = sprintf("%+.3f", r), hjust = ifelse(r > 0, -0.10, 1.10)),
                size = 3.4, fontface = "bold", color = PAL$dark) +
      scale_fill_manual(values = c("Positiva" = PAL$teal, "Negativa" = PAL$rose), name = NULL) +
      scale_x_continuous(limits = c(-0.75, 0.75), labels = scales::number_format(accuracy = 0.1)) +
      labs(title    = "Top Correlaciones Spearman con target_ROA (año t+1)",
           subtitle = sprintf("Set TRAIN | %d variables post-filtro", length(eda_r()$VARS_FILTRADAS)),
           x = "Correlación de Spearman", y = NULL) +
      tema_k + theme(legend.position = "bottom")
  })
  
  output$eda_grid <- renderPlot({
    req(datos_r()); d <- datos_r(); KEY_VARS <- d$KEY_VARS
    lista <- lapply(KEY_VARS, function(v) {
      if (!v %in% names(d$df)) return(NULL)
      x <- d$df[[v]]; x <- x[is.finite(x)]; if (length(x) < 5) return(NULL)
      mu <- mean(x); md <- median(x)
      asim <- round(e1071::skewness(x), 2); kurt <- round(e1071::kurtosis(x), 2)
      df_h <- data.frame(x = x, cl = ifelse(x >= 0, "Ganancia", "Pérdida"))
      ggplot(df_h, aes(x = x, fill = cl)) +
        geom_histogram(bins = 50, boundary = 0, alpha = 0.90, color = "white", linewidth = 0.08) +
        scale_fill_manual(values = c("Ganancia" = "#4CAF50", "Pérdida" = "#F28B82"), guide = "none") +
        geom_vline(xintercept = mu, color = "#FFA500", linewidth = 1.0, linetype = "dashed") +
        geom_vline(xintercept = md, color = "#2E7D32", linewidth = 0.8, linetype = "solid") +
        annotate("text", x = Inf, y = Inf,
                 label = sprintf("Asim=%s\nKurt=%s", format(asim, nsmall=2), format(kurt, nsmall=2)),
                 hjust = 1.08, vjust = 1.20, size = 2.8, color = PAL$dark, fontface = "bold") +
        labs(title = v, x = NULL, y = "Frec.") + tema_k +
        theme(plot.title = element_text(face="bold",size=9,color=PAL$dark,hjust=0.5),
              axis.text.x = element_text(size=7), axis.text.y = element_text(size=7),
              panel.grid.minor = element_blank())
    })
    lista <- Filter(Negate(is.null), lista)
    wrap_plots(lista, ncol = 3) +
      plot_annotation(
        title    = "Resumen Descriptivo – Ganancias (verde) vs Pérdidas (rojo)",
        subtitle = "Verde ≥ 0 | Rojo < 0 | Línea naranja = media | Línea verde = mediana | Asimetría y Curtosis en cada panel",
        theme = theme(plot.title = element_text(face="bold",size=14,color=PAL$dark,hjust=0.5),
                      plot.subtitle = element_text(size=9,color=PAL$slate,hjust=0.5),
                      plot.background = element_rect(fill="white",color=NA)))
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # OUTPUTS – PCA
  # ══════════════════════════════════════════════════════════════════════════
  output$pca_txt <- renderPrint({
    req(pca_r()); pr <- pca_r()
    cat(sprintf("Componentes Kaiser (λ≥1): %d\n", pr$n_kaiser))
    cat(sprintf("PCs para ≥80%%:            %d\n", if (!is.na(pr$n_80)) pr$n_80 else pr$n_pcs))
    cat(sprintf("PCs seleccionados:         %d\n", pr$n_pcs))
    cat(sprintf("Variables en PCA (%d):     %s\n", length(pr$vars_pca), paste(pr$vars_pca, collapse = ", ")))
    cat("\nVarianza explicada por PC:\n")
    for (i in seq_len(min(pr$n_pcs + 2, length(pr$ve))))
      cat(sprintf("  PC%d: %5.2f%% (acum: %5.2f%%)\n", i, pr$ve[i]*100, pr$ve_cum[i]*100))
  })
  
  output$pca_var <- renderPlot({
    req(pca_r()); pr <- pca_r(); n_show <- min(length(pr$ve), 10)
    df_ve <- data.frame(PC = factor(paste0("PC",1:n_show), levels=paste0("PC",1:n_show)),
                        ind = pr$ve[1:n_show]*100, acum = pr$ve_cum[1:n_show]*100)
    ggplot(df_ve, aes(x = PC)) +
      geom_col(aes(y = ind), fill = PAL$teal, alpha = 0.8, width = 0.6) +
      geom_line(aes(y = acum, group=1), color = PAL$orange, linewidth = 2) +
      geom_point(aes(y = acum), color = PAL$orange, size = 4) +
      geom_hline(yintercept = 80, color = PAL$red, linewidth = 1.2, linetype = "dashed") +
      geom_text(aes(y = ind + 1.5, label = sprintf("%.1f%%", ind)), size = 3.5, color = PAL$dark) +
      annotate("text", x = n_show * 0.8, y = 83, label = "Umbral 80%", color = PAL$red, size = 3.5) +
      labs(title    = "PCA – Varianza Explicada por Componente",
           subtitle = sprintf("Kaiser: %d PCs con λ≥1 | %d PCs acumulan ≥80%% varianza", pr$n_kaiser, pr$n_pcs),
           x = NULL, y = "% Varianza") + tema_k
  })
  
  output$pca_heatmap <- renderPlot({
    req(pca_r()); pr <- pca_r(); n_c <- min(pr$n_pcs, ncol(pr$pca$rotation))
    rot <- pr$pca$rotation[, seq_len(n_c), drop = FALSE]
    df_load <- as.data.frame(rot) |>
      tibble::rownames_to_column("Variable") |>
      pivot_longer(-Variable, names_to = "PC", values_to = "Loading")
    ggplot(df_load, aes(x = PC, y = Variable, fill = Loading)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.2f", Loading)), size = 3.2, color = "white", fontface = "bold") +
      scale_fill_gradient2(low = HEAT_NEG, mid = "white", high = HEAT_POS, midpoint = 0, name = "Loading") +
      labs(title    = "PCA – Mapa de Calor de Loadings (Vectores Propios)",
           subtitle = sprintf("%d PCs × %d variables | Rojo=carga positiva | Azul=carga negativa", n_c, nrow(rot)),
           x = "Componente Principal", y = NULL) +
      tema_k + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
  })
  
  output$pca_biplot_12 <- renderPlot({ req(pca_r()); hacer_biplot(pca_r(), 1, 2) })
  output$pca_biplot_13 <- renderPlot({ req(pca_r()); hacer_biplot(pca_r(), 1, 3) })
  output$pca_biplot_23 <- renderPlot({ req(pca_r()); hacer_biplot(pca_r(), 2, 3) })
  
  output$pca_biplot_panel <- renderPlot({
    req(pca_r()); pr <- pca_r()
    p12 <- hacer_biplot(pr, 1, 2) + labs(title = sprintf("A) PC1×PC2 (%.1f%%+%.1f%%)", pr$ve[1]*100, pr$ve[2]*100))
    p13 <- hacer_biplot(pr, 1, 3) + labs(title = sprintf("B) PC1×PC3 (%.1f%%+%.1f%%)", pr$ve[1]*100, pr$ve[3]*100))
    p23 <- hacer_biplot(pr, 2, 3) + labs(title = sprintf("C) PC2×PC3 (%.1f%%+%.1f%%)", pr$ve[2]*100, pr$ve[3]*100))
    (p12 | p13 | p23) +
      plot_annotation(title = "PCA – Comparativo Pairplot Biplots (PC1, PC2, PC3)",
                      theme = theme(plot.title = element_text(face="bold",size=14,color=PAL$dark)))
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # OUTPUTS – K-Means
  # ══════════════════════════════════════════════════════════════════════════
  output$kmeans_txt <- renderPrint({
    req(kmeans_r()); km_res <- kmeans_r()
    cat(sprintf("K fijo usado: K=%d  |  K codo (ref): K=%d\n", km_res$ok_k, km_res$codo_k))
    cat(sprintf("WCSS total:   %.2f\n\n", km_res$km$tot.withinss))
    cat(sprintf("%-9s  %8s  %8s  %8s  %6s\n","Cluster","ROA","CFO","LEV","n"))
    cat(strrep("-",45),"\n")
    for (cl in levels(km_res$dfpr$cluster)) {
      s <- km_res$dfpr[km_res$dfpr$cluster == cl, ]
      cat(sprintf("C%-8s  %+8.4f  %+8.4f  %8.4f  %6d\n", cl,
                  mean(s$ROA,na.rm=TRUE),
                  if("CFO"%in%names(s)) mean(s$CFO,na.rm=TRUE) else NA_real_,
                  if("LEV"%in%names(s)) mean(s$LEV,na.rm=TRUE) else NA_real_,
                  nrow(s)))
    }
  })
  
  output$kmeans_codo <- renderPlot({
    req(kmeans_r()); km_res <- kmeans_r()
    dfe <- data.frame(k = km_res$Kr, ine = km_res$ine)
    ggplot(dfe, aes(x=k, y=ine)) +
      geom_line(color=PAL$teal, linewidth=2) + geom_point(size=4, color=PAL$teal) +
      geom_vline(xintercept=km_res$ok_k, color=PAL$red, linewidth=1.5, linetype="dashed") +
      geom_vline(xintercept=km_res$codo_k, color=PAL$orange, linewidth=1.2, linetype="dotted") +
      annotate("label", x=km_res$ok_k+0.3, y=max(km_res$ine)*0.90,
               label=sprintf("K=%d\n(usado)", km_res$ok_k), color=PAL$red, fill="white", size=4, fontface="bold") +
      annotate("label", x=km_res$codo_k-0.3, y=max(km_res$ine)*0.75,
               label=sprintf("K=%d\n(codo)", km_res$codo_k), color=PAL$orange, fill="white", size=3.5) +
      scale_x_continuous(breaks=km_res$Kr) +
      labs(title="K-Means – Método del Codo (Inercia Intra-Cluster)",
           subtitle=sprintf("Set TRAIN | %d obs | %d PCs como features", nrow(km_res$Xkm), km_res$nkm),
           x="Número de Clusters K", y="Inercia total intra-cluster (WCSS)") + tema_k
  })
  
  output$kmeans_2d_12 <- renderPlot({ req(kmeans_r()); hacer_scatter_cl(kmeans_r(), 1, 2) })
  output$kmeans_2d_13 <- renderPlot({ req(kmeans_r()); hacer_scatter_cl(kmeans_r(), 1, 3) })
  output$kmeans_2d_23 <- renderPlot({ req(kmeans_r()); hacer_scatter_cl(kmeans_r(), 2, 3) })
  
  output$kmeans_panel <- renderPlot({
    req(kmeans_r()); km_res <- kmeans_r()
    p12 <- hacer_scatter_cl(km_res,1,2)+labs(title=sprintf("A) PC1×PC2 (%.1f%%+%.1f%%)",km_res$ve[1]*100,km_res$ve[2]*100))
    p13 <- hacer_scatter_cl(km_res,1,3)+labs(title=sprintf("B) PC1×PC3 (%.1f%%+%.1f%%)",km_res$ve[1]*100,km_res$ve[3]*100))
    p23 <- hacer_scatter_cl(km_res,2,3)+labs(title=sprintf("C) PC2×PC3 (%.1f%%+%.1f%%)",km_res$ve[2]*100,km_res$ve[3]*100))
    (p12|p13|p23)+plot_annotation(title=sprintf("K-Means K=%d – Pairplot de Clusters en PC1, PC2 y PC3",km_res$ok_k),
                                  theme=theme(plot.title=element_text(face="bold",size=14,color=PAL$dark)))
  })
  
  output$kmeans_perfiles <- renderPlot({
    req(kmeans_r()); km_res <- kmeans_r()
    dfpl <- km_res$dfpr |> pivot_longer(cols=all_of(km_res$pvx), names_to="Variable", values_to="Valor")
    ggplot(dfpl, aes(x=cluster, y=Valor, fill=cluster)) +
      geom_boxplot(notch=TRUE, notchwidth=0.25, alpha=0.85, outlier.alpha=0.1, color="gray20") +
      geom_hline(yintercept=0, color="black", linewidth=0.8, linetype="dashed") +
      scale_fill_manual(values=km_res$cc) +
      facet_wrap(~Variable, scales="free_y") +
      labs(title=sprintf("Perfil Financiero por Cluster – K=%d | Set TRAIN", km_res$ok_k),
           subtitle="Muescas sin solapamiento = diferencia estadística al 95%",
           x="Cluster", y="Valor (empresa×año)") +
      tema_k + theme(legend.position="none")
  })
  
  output$kmeans_centroides <- renderPlot({
    req(kmeans_r()); km_res <- kmeans_r()
    df_c <- as.data.frame(km_res$km$centers)
    colnames(df_c) <- paste0("PC",seq_len(ncol(df_c)))
    df_c$cluster <- factor(paste0("C",seq_len(km_res$ok_k)))
    df_cl <- df_c |> pivot_longer(cols=starts_with("PC"), names_to="PC", values_to="score")
    ggplot(df_cl, aes(x=PC, y=cluster, fill=score)) +
      geom_tile(color="white", linewidth=0.8) +
      geom_text(aes(label=sprintf("%.2f",score)), size=3.8, fontface="bold",
                color=ifelse(abs(df_cl$score)>0.5,"white",PAL$dark)) +
      scale_fill_gradient2(low=PAL$red, mid="white", high=PAL$teal, midpoint=0, name="Score PC") +
      labs(title="Heatmap de Centroides – K-Means en espacio PCA",
           subtitle=sprintf("K=%d | Rojo=score bajo | Verde=score alto",km_res$ok_k),
           x="Componente Principal", y="Cluster") +
      tema_k
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # OUTPUTS – OLS
  # ══════════════════════════════════════════════════════════════════════════
  output$ols_summary_txt <- renderPrint({
    req(ols_r()); ol <- ols_r(); smry <- ol$smry; fs <- smry$fstatistic
    cat(sprintf("Obs. TRAIN (sin NAs):          %d\n", ol$n_obs))
    cat(sprintf("Variables iniciales (filtro):  %d\n", length(ol$VARS_FILTRADAS)))
    cat(sprintf("Variables continuas finales:   %d\n", length(ol$vars_ols_activas)))
    cat(sprintf("Eliminadas por VIF>10:         %s\n",
                if (length(ol$eliminadas_vif)>0) paste(ol$eliminadas_vif,collapse=", ") else "ninguna"))
    cat(sprintf("\nR²      = %.4f\nR²-adj  = %.4f\nF       = %.2f  (p = %.4e)\n",
                smry$r.squared, smry$adj.r.squared,
                fs[1], pf(fs[1],fs[2],fs[3],lower.tail=FALSE)))
  })
  
  output$ols_coefs <- renderPlot({
    req(ols_r()); ol <- ols_r()
    ggplot(ol$df_betas, aes(x=Beta, y=Variable, color=sig)) +
      geom_vline(xintercept=0, color="gray35", linewidth=1.3, linetype="dashed") +
      geom_segment(aes(x=lo, xend=hi, yend=Variable), linewidth=2.5, alpha=.30) +
      geom_point(size=5) +
      geom_text(aes(label=lbl), hjust=-.15, size=3.3, color=PAL$dark, fontface="bold") +
      scale_color_manual(values=c("Significativo (p<0.05)"=PAL$teal,"No significativo"=PAL$slate), name=NULL) +
      labs(title="Coeficientes OLS con IC 95% (variables continuas)",
           subtitle=sprintf("R²=%.4f | R²-adj=%.4f | n=%d | %d vars (post-VIF)",
                            ol$smry$r.squared, ol$smry$adj.r.squared, ol$n_obs, length(ol$vars_ols_activas)),
           x="Valor del coeficiente β", y=NULL) +
      tema_k + theme(legend.position="bottom")
  })
  
  output$vif_ini_txt <- renderPrint({
    req(ols_r()); ol <- ols_r()
    if (is.null(ol$vif_ini_df)) { cat("VIF inicial no disponible.\n"); return() }
    cat(sprintf("VIF INICIAL – %d variables ANTES del bucle iterativo:\n", nrow(ol$vif_ini_df)))
    cat(sprintf("%-20s  %8s  %s\n","Variable","VIF","Diagnóstico")); cat(strrep("-",45),"\n")
    for (i in seq_len(nrow(ol$vif_ini_df))) {
      flag <- if (ol$vif_ini_df$VIF[i]>10) "!!! GRAVE" else if (ol$vif_ini_df$VIF[i]>5) "! Moderado" else "OK"
      cat(sprintf("%-20s  %8.3f  %s\n", ol$vif_ini_df$Variable[i], ol$vif_ini_df$VIF[i], flag))
    }
  })
  
  output$diag_vif_ini <- renderPlot({
    req(ols_r()); ol <- ols_r(); req(!is.null(ol$vif_ini_df))
    ggplot(ol$vif_ini_df, aes(x=reorder(Variable,VIF), y=VIF,
                              fill=cut(VIF,c(0,5,10,Inf),labels=c("OK (<5)","Moderado (5-10)","Grave (>10)")))) +
      geom_col(alpha=0.85) +
      geom_hline(yintercept=5, color=PAL$orange,linewidth=1.2,linetype="dashed") +
      geom_hline(yintercept=10,color=PAL$red,   linewidth=1.2,linetype="dashed") +
      coord_flip() +
      scale_fill_manual(values=c("OK (<5)"=PAL$teal,"Moderado (5-10)"=PAL$orange,"Grave (>10)"=PAL$red), name="Nivel VIF") +
      labs(title="VIF Inicial – Identificación de Multicolinealidad",
           subtitle=sprintf("Panel de %d variables | VIF Máx: %.2f", nrow(ol$vif_ini_df), max(ol$vif_ini_df$VIF,na.rm=TRUE)),
           x=NULL, y="VIF") +
      tema_k + theme(legend.position="bottom")
  })
  
  output$vif_txt <- renderPrint({
    req(ols_r()); ol <- ols_r()
    if (is.null(ol$vif_df)) { cat("VIF no disponible.\n"); return() }
    cat(sprintf("VIF FINAL – %d variables DESPUÉS del bucle iterativo:\n", nrow(ol$vif_df)))
    cat(sprintf("%-20s  %8s  %s\n","Variable","VIF","Diagnóstico")); cat(strrep("-",45),"\n")
    for (i in seq_len(nrow(ol$vif_df))) {
      flag <- if (ol$vif_df$VIF[i]>10) "!!! GRAVE" else if (ol$vif_df$VIF[i]>5) "! Moderado" else "OK"
      cat(sprintf("%-20s  %8.3f  %s\n", ol$vif_df$Variable[i], ol$vif_df$VIF[i], flag))
    }
  })
  
  output$diag_vif <- renderPlot({
    req(ols_r()); ol <- ols_r(); req(!is.null(ol$vif_df))
    ggplot(ol$vif_df, aes(x=reorder(Variable,VIF), y=VIF,
                          fill=cut(VIF,c(0,5,10,Inf),labels=c("OK (<5)","Moderado (5-10)","Grave (>10)")))) +
      geom_col(alpha=0.85) +
      geom_hline(yintercept=5, color=PAL$orange,linewidth=1.2,linetype="dashed") +
      geom_hline(yintercept=10,color=PAL$red,   linewidth=1.2,linetype="dashed") +
      coord_flip() +
      scale_fill_manual(values=c("OK (<5)"=PAL$teal,"Moderado (5-10)"=PAL$orange,"Grave (>10)"=PAL$red), name="Nivel VIF") +
      labs(title="VIF Final – Factor de Inflación de la Varianza",
           subtitle=sprintf("Umbral corr.=%.2f | VIF máx=%.2f (modelo limpio)", input$umbral_corr, max(ol$vif_df$VIF,na.rm=TRUE)),
           x=NULL, y="VIF") +
      tema_k + theme(legend.position="bottom")
  })
  
  output$jb_txt <- renderPrint({
    req(ols_r()); ol <- ols_r()
    cat(sprintf("Jarque-Bera: stat=%.4f | df=2 | p=%.4e\n", ol$jb_stat, ol$jb_pval))
    cat(sprintf("  Asimetría residuos = %.4f | Curtosis exceso = %.4f\n", ol$s_res, ol$k_res))
    cat(sprintf("Conclusión: %s\n",
                if (ol$jb_pval > 0.05) "No se rechaza H₀ (normalidad) ✓"
                else "Se rechaza H₀ → no normalidad ✗"))
  })
  
  output$diag_hist_res <- renderPlot({
    req(ols_r()); ol <- ols_r(); res <- ol$res[is.finite(ol$res)]
    ggplot(data.frame(res=res), aes(x=res)) +
      geom_histogram(aes(y=after_stat(density)), bins=70, fill=PAL$teal, alpha=0.75, color="white") +
      stat_function(fun=dnorm, args=list(mean=mean(res),sd=sd(res)), color=PAL$red, linewidth=2.2) +
      geom_vline(xintercept=0, color="black", linewidth=1.3) +
      annotate("label", x=Inf, y=Inf,
               label=sprintf("JB stat=%.4f\np=%.3e\n%s", ol$jb_stat, ol$jb_pval,
                             if(ol$jb_pval>0.05) "Normal ✓" else "No normal ✗"),
               hjust=1.05, vjust=1.05, size=3.5, fill="#FFF3E0", color=PAL$dark) +
      labs(title="Distribución de Residuos OLS (Jarque-Bera)",
           subtitle="Curva roja = N(μ,σ) teórica | Coincidencia → normalidad",
           x="Residuo", y="Densidad") + tema_k
  })
  
  output$diag_qq <- renderPlot({
    req(ols_r()); ol <- ols_r(); d <- ol$res[is.finite(ol$res)]
    qqr <- qqnorm(d, plot.it=FALSE); dfq <- data.frame(t=qqr$x, m=qqr$y)
    ggplot(dfq, aes(x=t, y=m)) +
      geom_point(color=PAL$teal, alpha=.25, size=1.2) +
      geom_abline(slope=sd(d), intercept=mean(d), color=PAL$red, linewidth=1.8) +
      labs(title="QQ-Plot – Normalidad Residuos",
           x="Cuantiles N(0,1)", y="Cuantiles muestrales") + tema_k
  })
  
  output$diag_sl <- renderPlot({
    req(ols_r()); ol <- ols_r()
    valid <- is.finite(ol$res_std) & is.finite(ol$fv)
    df_sl <- data.frame(f=ol$fv[valid], s=sqrt(abs(ol$res_std[valid])))
    sm_sl <- as.data.frame(lowess(df_sl$f, df_sl$s, f=0.3))
    ggplot(df_sl, aes(x=f, y=s)) +
      geom_point(color=PAL$teal, alpha=.20, size=1.2) +
      geom_line(data=sm_sl, aes(x=x,y=y), color=PAL$orange, linewidth=2.2) +
      annotate("label", x=Inf, y=Inf,
               label=sprintf("BP: χ²=%.3f\np=%.3e\n%s",
                             ol$bp_test$statistic, ol$bp_test$p.value,
                             if(ol$bp_test$p.value>0.05) "Homocedasticidad ✓" else "Heterocedasticidad ✗"),
               hjust=1.05, vjust=1.05, size=3.5, fill="#FFF0F0", color=PAL$dark, family="mono") +
      labs(title="Scale-Location – Homocedasticidad (Breusch-Pagan)",
           subtitle="Curva naranja PLANA → varianza constante | Pendiente = heterocedasticidad",
           x="Valores ajustados (ŷ)", y="√|Residuos estand.|") + tema_k
  })
  
  output$bp_txt <- renderPrint({
    req(ols_r()); ol <- ols_r()
    cat(sprintf("Breusch-Pagan: χ²=%.4f | df=%d | p=%.4e\nConclusión: %s\n",
                ol$bp_test$statistic, ol$bp_test$parameter, ol$bp_test$p.value,
                if(ol$bp_test$p.value>0.05) "No se rechaza H₀ (homocedasticidad) ✓"
                else "Se rechaza H₀ → heterocedasticidad ✗ (frecuente en datos financieros)"))
  })
  
  output$dw_txt <- renderPrint({
    req(ols_r()); ol <- ols_r()
    cat(sprintf("Durbin-Watson: DW=%.4f | p=%.4e\nConclusión: %s\n",
                ol$dw_test$statistic, ol$dw_test$p.value,
                if(ol$dw_test$statistic>1.5&&ol$dw_test$statistic<2.5) "≈ 2 → sin autocorrelación significativa ✓"
                else if(ol$dw_test$statistic<1.5) "< 1.5 → posible autocorrelación positiva ⚠"
                else "> 2.5 → posible autocorrelación negativa ⚠"))
  })
  
  output$diag_cook <- renderPlot({
    req(ols_r()); ol <- ols_r(); df_inf <- ol$df_inf
    ggplot(df_inf, aes(x=obs, y=cook)) +
      geom_segment(aes(x=obs,xend=obs,y=0,yend=cook,color=alto_cook), linewidth=0.5, alpha=0.6) +
      geom_point(aes(color=alto_cook), size=0.8, alpha=0.7) +
      geom_hline(yintercept=ol$umbral_cook, color=PAL$red, linewidth=1.3, linetype="dashed") +
      scale_color_manual(values=c("FALSE"=PAL$teal,"TRUE"=PAL$red),
                         labels=c("FALSE"="Normal","TRUE"="Influyente"), name=NULL) +
      labs(title="Distancia de Cook – Observaciones Influyentes",
           subtitle=sprintf("Umbral=4/n=%.5f | %d obs sobre umbral (%.1f%%)",
                            ol$umbral_cook, sum(df_inf$alto_cook), sum(df_inf$alto_cook)/ol$n_obs*100),
           x="Índice de observación", y="Distancia de Cook (Dᵢ)") +
      tema_k + theme(legend.position="bottom")
  })
  
  output$diag_rvl <- renderPlot({
    req(ols_r()); ol <- ols_r(); df_inf <- ol$df_inf
    top_inf <- df_inf |> arrange(desc(cook)) |> head(10)
    ggplot(df_inf, aes(x=hat, y=res_std)) +
      geom_point(aes(size=cook, color=alto_cook&alto_hat), alpha=0.35) +
      geom_hline(yintercept=c(-2,0,2), linetype=c("dashed","solid","dashed"),
                 color=c(PAL$orange,"gray40",PAL$orange), linewidth=c(1.0,0.8,1.0)) +
      geom_vline(xintercept=ol$umbral_hat, color=PAL$red, linewidth=1.0, linetype="dashed") +
      geom_label_repel(data=top_inf, aes(x=hat,y=res_std,label=sprintf("obs %d",obs)),
                       size=3, color=PAL$dark, fill="white", box.padding=0.3, max.overlaps=10) +
      scale_color_manual(values=c("FALSE"=PAL$teal,"TRUE"=PAL$red),
                         labels=c("FALSE"="Normal","TRUE"="Alto Cook & Leverage"), name=NULL) +
      scale_size_continuous(range=c(0.5,4), guide="none") +
      labs(title="Residuos Estand. vs Leverage (Hat Values)", x="Leverage (hᵢᵢ)", y="Residuos estandarizados") +
      tema_k + theme(legend.position="bottom")
  })
  
  output$diag_4en1 <- renderPlot({
    req(ols_r()); ol <- ols_r(); d <- ol$res[is.finite(ol$res)]
    p_h <- ggplot(data.frame(res=d), aes(x=res)) +
      geom_histogram(aes(y=after_stat(density)), bins=60, fill=PAL$teal, alpha=0.75, color="white") +
      stat_function(fun=dnorm, args=list(mean=mean(d),sd=sd(d)), color=PAL$red, linewidth=2) +
      labs(title="A) Histograma residuos", x="Residuo", y="Densidad") + tema_k
    
    dfq <- data.frame(t=qqnorm(d,plot.it=FALSE)$x, m=qqnorm(d,plot.it=FALSE)$y)
    p_q <- ggplot(dfq, aes(x=t,y=m)) +
      geom_point(color=PAL$teal, alpha=.25, size=1.2) +
      geom_abline(slope=sd(d),intercept=mean(d),color=PAL$red,linewidth=1.8) +
      labs(title="B) QQ-Plot", x="Cuantiles N(0,1)", y="Cuantiles muestrales") + tema_k
    
    valid <- is.finite(ol$res_std) & is.finite(ol$fv)
    df_sl <- data.frame(f=ol$fv[valid], s=sqrt(abs(ol$res_std[valid])))
    sm_sl <- as.data.frame(lowess(df_sl$f, df_sl$s, f=0.3))
    p_sl2 <- ggplot(df_sl,aes(x=f,y=s)) +
      geom_point(color=PAL$teal,alpha=.20,size=1.2) +
      geom_line(data=sm_sl,aes(x=x,y=y),color=PAL$orange,linewidth=2.2) +
      labs(title="C) Scale-Location", x="Ajustados (ŷ)", y="√|Res. est.|") + tema_k
    
    df_inf  <- ol$df_inf; top_inf <- df_inf |> arrange(desc(cook)) |> head(10)
    p_rvl2 <- ggplot(df_inf,aes(x=hat,y=res_std)) +
      geom_point(aes(size=cook,color=alto_cook&alto_hat),alpha=0.35) +
      geom_hline(yintercept=c(-2,0,2),linetype=c("dashed","solid","dashed"),
                 color=c(PAL$orange,"gray40",PAL$orange)) +
      geom_vline(xintercept=ol$umbral_hat,color=PAL$red,linewidth=1.0,linetype="dashed") +
      scale_color_manual(values=c("FALSE"=PAL$teal,"TRUE"=PAL$red),guide="none") +
      scale_size_continuous(range=c(0.5,4),guide="none") +
      labs(title="D) Residuos vs Leverage", x="Leverage", y="Res. estand.") + tema_k
    
    (p_h|p_q|p_sl2|p_rvl2) +
      plot_annotation(
        title    = "Panel Diagnóstico OLS – Validación Supuestos Gauss-Markov",
        subtitle = sprintf("R²=%.4f | R²adj=%.4f | JB p=%.3e | BP p=%.3e | DW=%.3f",
                           ol$smry$r.squared, ol$smry$adj.r.squared,
                           ol$jb_pval, ol$bp_test$p.value, ol$dw_test$statistic))
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # OUTPUTS – Evaluación Test
  # ══════════════════════════════════════════════════════════════════════════
  output$test_metricas_txt <- renderPrint({
    req(test_r()); tr <- test_r()
    cat(strrep("=",56),"\n  MÉTRICAS FUERA DE MUESTRA – TEST\n",strrep("-",56),"\n")
    cat(sprintf("  RMSE = %.6f\n  MAE  = %.6f\n  R²   out-of-sample = %.6f\n", tr$rmse_test, tr$mae_test, tr$r2_test))
    cat(strrep("-",56),"\n")
    cat(sprintf("  R²   in-sample (train) = %.6f\n", tr$r2_train))
    cat(sprintf("  Brecha R² (overfit?) = %.6f\n", tr$r2_train - tr$r2_test))
    cat(sprintf("  n predicciones válidas  = %d\n", tr$n_ok))
    cat(strrep("=",56),"\n")
  })
  
  output$eval_A <- renderPlot({
    req(test_r()); tr <- test_r()
    lim <- range(c(tr$y_real_ok, tr$y_pred_ok), na.rm=TRUE)
    df_eval <- data.frame(real=tr$y_real_ok, pred=tr$y_pred_ok, dir=(tr$y_pred_ok-tr$y_real_ok)>0)
    ggplot(df_eval, aes(x=real, y=pred, color=dir)) +
      geom_ribbon(data=data.frame(real=seq(lim[1],lim[2],length.out=300)),
                  aes(x=real,ymin=real-tr$rmse_test,ymax=real+tr$rmse_test),
                  fill=PAL$teal,alpha=0.10,inherit.aes=FALSE) +
      geom_point(alpha=0.35, size=1.5) +
      geom_abline(slope=1,intercept=0,color=PAL$red,linewidth=1.8,linetype="dashed") +
      scale_color_manual(values=c("TRUE"=PAL$orange,"FALSE"=PAL$teal2),
                         labels=c("TRUE"="Sobreestimado","FALSE"="Subestimado"),name=NULL) +
      annotate("label", x=lim[1], y=lim[2],
               label=sprintf("R²   = %.4f\nRMSE = %.4f\nMAE  = %.4f\nn    = %d",
                             tr$r2_test,tr$rmse_test,tr$mae_test,tr$n_ok),
               hjust=0,vjust=1,size=4.0,fill="#EFF6FF",color=PAL$dark,family="mono") +
      labs(title    = sprintf("OLS – target_ROA Predicho vs Real | Test %d → ROA %d", input$test_year, input$test_year+1),
           subtitle = sprintf("R²-test=%.4f | RMSE=%.4f | n=%d predicciones válidas", tr$r2_test,tr$rmse_test,tr$n_ok),
           x="ROA Observado (ground truth)", y="ROA Predicho") +
      tema_k + theme(legend.position="bottom")
  })
  
  output$eval_B <- renderPlot({
    req(test_r()); tr <- test_r()
    df_dens <- bind_rows(
      data.frame(ROA=tr$y_real_ok,  Distribucion=sprintf("Real (n=%d)",    length(tr$y_real_ok))),
      data.frame(ROA=tr$y_pred_ok,  Distribucion=sprintf("Predicho (n=%d)", length(tr$y_pred_ok)))
    ) |> mutate(Distribucion=factor(Distribucion, levels=unique(Distribucion)))
    
    media_real <- mean(tr$y_real_ok, na.rm=TRUE); media_pred <- mean(tr$y_pred_ok, na.rm=TRUE)
    
    ggplot(df_dens, aes(x=ROA, fill=Distribucion, color=Distribucion)) +
      geom_density(alpha=0.45, linewidth=1.4) +
      geom_vline(xintercept=media_real, color=PAL$teal,   linewidth=1.3, linetype="dashed") +
      geom_vline(xintercept=media_pred, color=PAL$orange, linewidth=1.3, linetype="dashed") +
      geom_vline(xintercept=0, color="gray35", linewidth=0.9, linetype="dotted") +
      scale_fill_manual(values=c(PAL$teal, PAL$orange),  name=NULL) +
      scale_color_manual(values=c(PAL$teal, PAL$orange), name=NULL) +
      annotate("label", x=media_real, y=Inf,
               label=sprintf("μ Real\n%.4f", media_real),
               vjust=1.3, size=3.5, fill=PAL$teal, color="white", fontface="bold") +
      annotate("label", x=media_pred, y=Inf,
               label=sprintf("μ Pred.\n%.4f", media_pred),
               vjust=1.3, size=3.5, fill=PAL$orange, color="white", fontface="bold") +
      labs(title    = sprintf("Distribución ROA Real vs Predicho | Test %d → ROA %d", input$test_year, input$test_year+1),
           subtitle = sprintf("R²-Test=%.4f | Media Real=%.4f | Media Predicha=%.4f", tr$r2_test, media_real, media_pred),
           x="target_ROA (ROA del año t+1)", y="Densidad") +
      tema_k + theme(legend.position="bottom", legend.key.size=unit(0.7,"cm"), legend.text=element_text(size=11))
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # OUTPUTS – Treemaps
  # ══════════════════════════════════════════════════════════════════════════
  output$treemap_plot <- renderPlot({
    req(treemap_base_r())
    anio <- as.integer(input$treemap_year)
    df_base <- treemap_base_r()
    df_anio <- df_base |> filter(year == anio)
    validate(need(nrow(df_anio) >= 3, paste0("Sin datos suficientes para el año ", anio)))
    
    inflacion_actual <- inflacion_kr$inflacion[inflacion_kr$year == anio]
    df_top100 <- df_anio |> slice_max(order_by = activos, n = 100)
    n_gan <- sum(df_top100$ROA_Real > 0, na.rm = TRUE)
    
    tm_obj <- suppressMessages(
      treemap(df_top100, index = "stock_char", vSize = "activos",
              type = "index", algorithm = "pivotSize", sortID = "ROA_Real",
              mirror.y = TRUE, mirror.x = TRUE, draw = FALSE))
    
    dg <- tm_obj[["tm"]] |> as_tibble() |>
      mutate(xmax=x0+w, ymax=y0+h, stock_char=as.character(stock_char),
             x_centro=x0+(w/2), y_centro=y0+(h/2)) |>
      left_join(df_top100 |> select(stock_char, name_clean, ROA_Real, activos), by="stock_char") |>
      mutate(label_vis = sprintf("%s\n%+.1f%%", name_clean, ROA_Real*100))
    
    lim_escala <- 0.05
    ggplot(dg) +
      geom_rect(aes(xmin=x0,ymin=y0,xmax=xmax,ymax=ymax,fill=ROA_Real), linewidth=0.5, colour="#0F172A") +
      geom_text(aes(x=x_centro,y=y_centro,label=label_vis), colour="white", size=3.2,
                fontface="bold", lineheight=0.9, check_overlap=TRUE) +
      scale_fill_gradient2(low="#EF4444",mid="#334155",high="#059669", midpoint=0,
                           limits=c(-lim_escala,lim_escala), oob=scales::squish,
                           labels=scales::percent_format(accuracy=1), name="Desempeño Real",
                           guide=guide_colorbar(title.position="left",title.vjust=0.8,
                                                barwidth=unit(10,"cm"),barheight=unit(0.6,"cm"),
                                                frame.colour="#475569",ticks.colour="white")) +
      labs(title    = sprintf("Los 100 Titanes frente a la Inflación (%d)", anio),
           subtitle = sprintf("Inflación oficial: %.2f%% | %d de 100 gigantes crearon riqueza real este año",
                              inflacion_actual*100, n_gan)) +
      theme_void() +
      theme(legend.position="bottom", legend.text=element_text(color="#E8EADC",size=11),
            legend.title=element_text(color="#E8EADC",size=13,face="bold"),
            plot.background=element_rect(fill="#0F172A",colour="#0F172A"),
            plot.margin=margin(20,15,20,15),
            plot.title=element_text(size=18,hjust=0.5,face="bold",colour="#F8FAFC",margin=margin(b=8)),
            plot.subtitle=element_text(size=12,hjust=0.5,colour="#94A3B8",margin=margin(b=15)))
  })
  
}

shinyApp(ui = ui, server = server)
