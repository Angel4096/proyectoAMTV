# CLAUDE.md

Guía para Claude/agentes que trabajen en este repositorio. La prioridad es proteger la metodología: **`target_ROA` es ROA futuro por firma y la lógica temporal no se negocia**.

---

## Startup Checklist for Claude/Agents

Antes de tocar código, afirmar resultados o ejecutar scripts, verificá esto:

- [ ] Confirmá que R está disponible.
  ```bash
  R --version
  ```
- [ ] No corras el análisis completo ni “builds” si el usuario no lo pidió explícitamente.
- [ ] Verificá que existen los paquetes mínimos de R.
  ```r
  paquetes <- c(
    "tidyverse", "dplyr", "ggplot2",
    "FactoMineR", "factoextra",
    "patchwork", "cluster", "scales", "ggrepel",
    "e1071", "data.table", "treemap", "ggtext", "corrplot",
    "lmtest", "car", "nortest", "testthat"
  )
  faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
  faltantes
  ```
- [ ] Si faltan paquetes, indicá cómo instalarlos; no digas que una ejecución pasó.
  ```r
  install.packages(faltantes)
  ```
- [ ] Verificá que la ruta del dataset exista o pedile al usuario configurar `DATA_PATH`.
  ```r
  file.exists("ruta/a/KoTaP_Dataset.csv")
  ```
- [ ] Antes de correr `R/AMTV_F.R`, verificá que exista `df`.
  ```r
  exists("df")
  ```
- [ ] Si `df` no existe, indicá que primero hay que cargar/preparar el panel con el script principal o la preparación equivalente.
- [ ] Antes de afirmar que los tests pasaron, verificá que `testthat` esté instalado y que la ejecución real haya terminado sin errores.
  ```r
  requireNamespace("testthat", quietly = TRUE)
  source("tests/testthat.R")
  ```

> Regla dura: **nunca afirmes “tests passed” si `testthat` falta, si no ejecutaste los tests o si hubo errores**.

---

## Project Overview

**KoTaP AMTV** es un proyecto académico de análisis financiero multivariante sobre empresas listadas en KOSPI y KOSDAQ.

| Punto | Detalle |
|---|---|
| Curso | Analítica y Métodos Multivariados. |
| Dataset | KoTaP, panel de firmas coreanas 2011–2024. |
| Unidad de observación | Firma-año: una empresa específica en un año específico. |
| Variable central | `target_ROA = ROA(t + 1)` por firma. |
| Métodos centrales | Descriptivos, Spearman, PCA, K-Means, OLS y diagnósticos clásicos. |

El objetivo no es predecir con modelos complejos, sino explicar e interpretar la relación entre características financieras actuales y rentabilidad futura.

---

## Core Methodological Rules

| Regla | Motivo |
|---|---|
| Crear `target_ROA` sobre el panel completo antes de filtrar outliers. | Evita que el ROA futuro salte de `t` a `t + 2`. |
| Separar temporalmente train/test/futuro. | Evita fuga de información temporal. |
| Usar PCA snapshot para interpretación de un año. | No mezcla estructura de firmas con efectos macro de años distintos. |
| Usar PCA temporal aprendido solo con train para OLS-PC. | El test no debe participar en la rotación PCA. |
| Mantener `ROA`, `ROE` y `target_ROA` fuera de predictores PCA principales. | Evita contaminación conceptual/endogeneidad. |
| Mantener métodos clásicos en el análisis principal. | Es una restricción del curso. |

Orden metodológico correcto:

1. Cargar y tipar el panel completo.
2. Crear `target_ROA` por firma.
3. Filtrar outliers de predictores con IQR × 3.
4. Hacer split temporal: train `<= 2022`, test `2023`, futuro `2024`.
5. Ejecutar PCA/K-Means/OLS según corresponda.

---

## Repository Structure

```text
R/
  Kotap analisis 9 de mayo.R       <- referencia metodológica principal actual
  AMTV_F.R                         <- flujo operativo: PCA snapshot + OLS-PC temporal
  helpers_pca_ols.R                <- helpers para target, outliers, split, PCA, OLS
  KoTaP_Analisis_v14_main.R        <- versión principal previa/alternativa
  KoTaP_Analisis_Financiero_v*.R   <- versiones anteriores de referencia
  Dataset_Nuevo.R                  <- preparación/exploración del dataset
  Dataset_Nuevo_Diapos.R           <- versión reducida para diapositivas
  shiny/app.R                      <- dashboard interactivo Shiny

output/                            <- figuras/resultados generados localmente
presentaciones/                    <- materiales de presentación, si existen
scripts/                           <- scripts auxiliares del proyecto
tests/                             <- harness testthat, si está disponible
```

---

## Running the R Analysis

### 1. Configurar rutas

En `R/Kotap analisis 9 de mayo.R`, ajustar:

```r
DATA_PATH  <- "ruta/a/KoTaP_Dataset.csv"
OUTPUT_DIR <- "ruta/a/output_kotap"
```

Verificá que `DATA_PATH` exista antes de ejecutar.

### 2. Preparar el panel

```r
source("R/Kotap analisis 9 de mayo.R")
```

Ese flujo debe dejar disponible el objeto `df` o una preparación equivalente del panel completo. Si el script contiene pausas interactivas (`readline()`), no lo ejecutes en automatización sin revisar eso primero.

### 3. Ejecutar flujo operativo

Solo cuando `df` exista:

```r
exists("df")

ANIO_PCA <- 2023
source("R/AMTV_F.R")
```

`R/AMTV_F.R` se detiene explícitamente si `df` no existe. No intentes “arreglarlo” saltándote la preparación: ese objeto representa el panel base.

---

## Expected Objects from `R/AMTV_F.R`

| Objeto | Significado |
|---|---|
| `df_con_target` | Panel con `target_ROA` creado. |
| `df_modelo` | Panel filtrado por outliers en predictores. |
| `particiones` | Lista con train/test/futuro. |
| `pca_snapshot` | PCA interpretativo del año `ANIO_PCA`. |
| `eigen_pca` | Eigenvalues y varianza explicada. |
| `p_pca_scree`, `p_pca_var`, `p_pca_ind`, `p_pca_biplot` | Gráficos PCA en memoria. |
| `pca_train` | PCA aprendido solo con train histórico. |
| `modelo_ols_pc` | Modelo OLS sobre componentes principales. |
| `diagnosticos_ols_pc` | Residuos, leverage y Cook's D. |
| `comparacion_test_ols_pc` | Validación temporal en test 2023, si hay filas completas. |

---

## Tests

Antes de ejecutar o reportar tests:

```r
requireNamespace("testthat", quietly = TRUE)
```

Si devuelve `FALSE`, la respuesta correcta es: “los tests no se pudieron ejecutar porque falta `testthat`”.

Si está instalado:

```r
source("tests/testthat.R")
```

No afirmes éxito si no viste una ejecución real sin errores.

---

## Shiny App

```r
shiny::runApp("R/shiny/app.R")
```

La app es útil para exploración interactiva, pero no reemplaza la lógica metodológica del script principal.

---

## Python Notes

Hay scripts Python exploratorios, por ejemplo:

```bash
pip install pandas numpy matplotlib seaborn scikit-learn
python python/KOSPI_KOSDAQ_prediccion.py
```

Tratá esos scripts como extensión/contexto. El análisis central del curso debe mantenerse en métodos clásicos y en el flujo R.

---

## Version/Reference Notes

| Archivo | Nota |
|---|---|
| `R/Kotap analisis 9 de mayo.R` | Referencia metodológica actual según las normas del repo. |
| `R/KoTaP_Analisis_v14_main.R` | Versión principal previa/alternativa con split temporal y VIF. |
| `R/KoTaP_Analisis_v4_6mayo.R` y anteriores | Referencias históricas. |

Si hay conflicto entre scripts viejos y la guía actual, seguí la lógica temporal documentada en `README.md`, `ORQUESTADOR.md` y `AGENTS.md`.
