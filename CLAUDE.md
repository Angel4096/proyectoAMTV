# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**KoTaP** — Análisis Financiero Multivariado de empresas listadas en KOSPI y KOSDAQ (mercado coreano).
Proyecto académico para el curso "Analítica y Métodos Multivariados" de la Escuela Colombiana de Ingeniería.

- Dataset: 12,653 observaciones · 1,754 empresas · 2011–2024
- Variables clave: ROA, ROE, SIZE, LEV, CFO, GRW, CUR, INVREC, MB, TQ, PPE, AGE
- Métodos aplicados: Estadística descriptiva, Correlación de Spearman, PCA, K-Means, Regresión OLS

## Structure

```
R/
  KoTaP_Analisis_v14_main.R   ← script principal (versión definitiva)
  KoTaP_Analisis_Financiero_v1/v2/v3/v4.R  ← versiones anteriores (referencia)
  Dataset_Nuevo.R             ← preparación del dataset
  Dataset_Nuevo_Diapos.R      ← versión reducida para diapositivas
  shiny/app.R                 ← dashboard interactivo Shiny

python/
  KOSPI_KOSDAQ_prediccion.py  ← Random Forest sobre retornos KOSPI/KOSDAQ
  analisis_nuevo.py           ← análisis exploratorio adicional

output/
  figures/    ← PNGs generados por los scripts R
  resultados/ ← resumen_final.json, coeficientes_gamma.txt

docs/         ← documentos de soporte y explicaciones
presentaciones/ ← PPTX, HTML de presentación gerencial
scripts/      ← Setup_proyectoAMTV.ps1
```

## Running the Code

### R (script principal)
```r
# En RStudio o R console — ajusta DATA_PATH antes de correr
source("R/KoTaP_Analisis_v14_main.R")
```
El script instala automáticamente los paquetes faltantes (`instalar_faltantes()`).
Requiere cambiar `DATA_PATH` y `OUTPUT_DIR` en la Sección 1 para apuntar al CSV local (`KoTaP_Dataset.csv`).

### Shiny App
```r
shiny::runApp("R/shiny/app.R")
```
Acepta hasta 200 MB de archivo CSV via upload. Usa los mismos métodos multivariados del script principal.

### Python
```bash
pip install pandas numpy matplotlib seaborn scikit-learn
python python/KOSPI_KOSDAQ_prediccion.py
```
Requiere `KoTaP_Dataset.csv` en `C:/Users/norba/Downloads/` (ajustar `filepath` en el script).

## Key Constraints (Syllabus)

Solo se usan métodos clásicos del syllabus:
- Covarianza, correlación (Spearman), asimetría, curtosis
- Valores/vectores propios, PCA, K-Means
- Regresión Lineal Múltiple (OLS) con diagnósticos: VIF, Breusch-Pagan, Durbin-Watson, Anderson-Darling

**No se permite** usar modelos de ML avanzados en el análisis principal R (el Random Forest está solo en el script Python como extensión).

## Data Pipeline (R/KoTaP_Analisis_v14_main.R)

1. Carga `KoTaP_Dataset.csv` → limpieza de duplicados
2. **PASO A**: `ROA_lead` = ROA del año siguiente (variable target desplazada)
3. **PASO B**: eliminación de outliers con IQR × 3
4. Split temporal: Train ≤ 2022 | Test = 2023 (predice ROA 2024) | Futuro = 2024 (predice ROA 2025)
5. Correlación Spearman → selección de variables candidatas (umbral 0.90 para multicolinealidad)
6. PCA (componentes principales) + K-Means (clustering)
7. OLS con VIF iterativo (elimina variables con VIF > 10)
8. Exporta figuras a `OUTPUT_DIR`

## Version History

| Archivo | Versión | Cambios principales |
|---|---|---|
| `KoTaP_Analisis_v14_main.R` | v14 | ROE excluido (endogeneidad), VIF iterativo, split 2022/2023/2024 |
| `KoTaP_Analisis_v4_6mayo.R` | v≈12 | Versión previa |
| `KoTaP_Analisis_v3_4mayo.R` | v≈10 | Versión previa |
