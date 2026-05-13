# ==============================================================================
# ANÁLISIS FINANCIERO Y PREDICTIVO - DATASET KoTaP (KOSPI / KOSDAQ)
# ==============================================================================

# 1. Instalación y carga de librerías necesarias
# ------------------------------------------------------------------------------
# Si no tienes pacman instalado, descomenta la siguiente línea:
# install.packages("pacman")
pacman::p_load(tidyverse, ggcorrplot, patchwork, broom)

# 2. Carga y Preparación de Datos
# ------------------------------------------------------------------------------
# Leemos el dataset real que has subido
df <- read_csv("C:/Users/norba/Downloads/KoTaP_Dataset.csv")

# Adecuamos algunas variables para los gráficos
df <- df %>%
  mutate(
    # Homologamos el nombre del año
    Year = year, 
    # Mapeamos la dummy KOSPI a texto para la leyenda (1 = KOSPI, 0 = KOSDAQ)
    Market = ifelse(KOSPI == 1, "KOSPI", "KOSDAQ"),
    # Etiquetamos las empresas con pérdida para el boxplot
    LOSS_label = ifelse(LOSS == 1, "Pérdida (LOSS=1)", "Ganancia (LOSS=0)")
  )

# ==============================================================================
# 3. ANÁLISIS UNIVARIADO - Comportamiento de Variables Financieras
# ==============================================================================

# Función auxiliar para crear el combo (Histograma + Boxplot)
plot_univariado <- function(data, variable, titulo, color_fill) {
  
  # Histograma
  p1 <- ggplot(data, aes_string(x = variable)) +
    geom_histogram(bins = 50, fill = color_fill, color = "white", alpha = 0.8) +
    geom_vline(aes(xintercept = mean(get(variable), na.rm = TRUE)), 
               color = "red", linetype = "dashed", linewidth = 1) +
    geom_vline(aes(xintercept = median(get(variable), na.rm = TRUE)), 
               color = "blue", linetype = "dotted", linewidth = 1) +
    theme_minimal() +
    labs(title = paste(titulo, "- Histograma"), y = "Frecuencia", x = variable)
  
  # Boxplot
  p2 <- ggplot(data, aes_string(y = variable)) +
    geom_boxplot(fill = color_fill, alpha = 0.7) +
    theme_minimal() +
    labs(title = paste(titulo, "- Boxplot"), y = variable) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  return(p1 + p2)
}

# Generamos los gráficos univariados clave
p_roa <- plot_univariado(df, "ROA", "ROA", "#339999")
p_roe <- plot_univariado(df, "ROE", "ROE", "#33AADD")
p_size <- plot_univariado(df, "SIZE", "SIZE", "#556688")
p_lev <- plot_univariado(df, "LEV", "LEV", "#FF8833")

# Mostrar univariados combinados usando patchwork (Puedes cambiar cuáles mostrar)
(p_roa / p_size) 


# ==============================================================================
# 4. ANÁLISIS BIVARIADO - Matriz de Correlaciones (Spearman)
# ==============================================================================

vars_corr <- df %>% select(SIZE, LEV, ROA, ROE, CFO, GRW, CUR, INVREC, PPE, AGE)
matriz_corr <- cor(vars_corr, method = "spearman", use = "complete.obs")

# Gráfico de calor de la matriz de correlación
ggcorrplot(matriz_corr, 
           method = "square", 
           type = "lower", 
           lab = TRUE, 
           colors = c("#3A7A8C", "white", "#C15C3D"),
           title = "Matriz de Correlación de Spearman - Variables Financieras",
           outline.color = "white")


# ==============================================================================
# 5. ANÁLISIS TEMPORAL Y POR SEGMENTOS
# ==============================================================================

# Evolución del ROA por Año (Media vs Mediana)
df_temporal <- df %>%
  group_by(Year) %>%
  summarise(Media = mean(ROA, na.rm = TRUE),
            Mediana = median(ROA, na.rm = TRUE)) %>%
  pivot_longer(cols = c(Media, Mediana), names_to = "Metrica", values_to = "Valor")

p_temp <- ggplot(df_temporal, aes(x = Year, y = Valor, color = Metrica, group = Metrica)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Media" = "#008880", "Mediana" = "#FF8000")) +
  theme_minimal() +
  labs(title = "Evolución del ROA por Año", x = "Año", y = "ROA")

# ROA: Empresas con Ganancia vs Pérdida (Usando la etiqueta generada)
p_loss <- ggplot(df, aes(x = LOSS_label, y = ROA, fill = LOSS_label)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("Ganancia (LOSS=0)" = "#339999", "Pérdida (LOSS=1)" = "#FF8833")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "ROA: Empresas con Ganancia vs Pérdida", x = "", y = "ROA")

# Combina ambos gráficos en uno
p_temp + p_loss


# ==============================================================================
# 6. ANÁLISIS MULTIVARIADO (Scatter Plots)
# ==============================================================================

# LEV vs ROA por Mercado (KOSPI vs KOSDAQ)
p_lev_roa <- ggplot(df, aes(x = LEV, y = ROA, color = Market)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  scale_color_manual(values = c("KOSPI" = "#FF9966", "KOSDAQ" = "#66B2A5")) +
  theme_minimal() +
  labs(title = "LEV vs ROA por Mercado", x = "Apalancamiento (LEV)", y = "ROA")

# SIZE vs ROA coloreado por Apalancamiento
p_size_roa <- ggplot(df, aes(x = SIZE, y = ROA, color = LEV)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  scale_color_gradientn(colors = c("#88BBAA", "#EEDD88", "#DD7777")) +
  theme_minimal() +
  labs(title = "SIZE vs ROA coloreado por Apalancamiento", x = "Tamaño (SIZE = ln Activos)", y = "ROA")

# Combina ambos gráficos multivariados
p_lev_roa + p_size_roa


# ==============================================================================
# 7. REGRESIÓN OLS - Modelo Predictivo del ROA
# ==============================================================================

# Estimación del modelo multivariado real con el dataset de KoTaP
modelo_ols <- lm(ROA ~ CFO + LEV + GRW + INVREC + PPE + AGE + SIZE + CUR, data = df)

# Resumen del modelo en consola (para que revises R^2, F-stat y p-values)
print(summary(modelo_ols))

# Extracción de los coeficientes limpios para graficar con 'broom'
resultados_ols <- tidy(modelo_ols, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Significancia = case_when(
      p.value < 0.05 & estimate > 0 ~ "Positivo significativo (p<0.05)",
      p.value < 0.05 & estimate < 0 ~ "Negativo significativo (p<0.05)",
      TRUE ~ "No significativo"
    ),
    term = reorder(term, estimate) # Ordenar por impacto visual
  )

# Gráfico de Coeficientes OLS (Forest Plot replicando la diapositiva final)
ggplot(resultados_ols, aes(x = estimate, y = term, fill = Significancia)) +
  geom_col(width = 0.6) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_fill_manual(values = c(
    "Positivo significativo (p<0.05)" = "#339999", 
    "Negativo significativo (p<0.05)" = "#FF8833", 
    "No significativo" = "#AABBCC"
  )) +
  theme_minimal() +
  labs(title = "Coeficientes OLS - Variable Objetivo: ROA",
       x = "Coeficiente β – Impacto sobre ROA", 
       y = "Variable")
