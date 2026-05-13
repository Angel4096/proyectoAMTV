# KoTaP AMTV — análisis multivariante para entender la rentabilidad futura

Este proyecto estudia empresas listadas en Corea usando métodos clásicos de análisis multivariante. La pregunta central es: **¿qué características financieras de una empresa en un año ayudan a explicar su ROA del año siguiente?**

> Si estás empezando de cero: no pienses en “empresas” sueltas. Cada fila del dataset representa una **empresa en un año específico**.

---

## 1. Qué es este proyecto

| Punto | Explicación simple |
|---|---|
| Proyecto | Análisis académico sobre firmas de Corea, principalmente KOSPI y KOSDAQ. |
| Dataset | Panel de datos: las mismas empresas pueden aparecer repetidas en distintos años. |
| Unidad de observación | **Firma-año**: por ejemplo, Samsung en 2019 es una observación; Samsung en 2020 es otra. |
| Variable objetivo | `target_ROA`, que representa el **ROA del año siguiente** de la misma firma. |
| Métodos principales | Estadística descriptiva, correlación, PCA, K-Means y regresión lineal OLS. |

El proyecto NO busca usar modelos avanzados de machine learning en el análisis principal. La idea es explicar e interpretar con herramientas clásicas del curso.

---

## 2. Qué representa el dataset

KoTaP reúne información financiera y de gobierno corporativo de firmas coreanas entre 2011 y 2024. Como es un **panel**, una firma puede tener muchas filas: una por cada año disponible.

### Ejemplo mental

| Empresa | Año | Qué significa la fila |
|---|---:|---|
| A | 2021 | Situación financiera de la empresa A en 2021. |
| A | 2022 | Situación financiera de la misma empresa A en 2022. |
| B | 2021 | Situación financiera de otra empresa en 2021. |

Eso es CLAVE: si mezclás todos los años como si fueran observaciones independientes sin cuidar el tiempo, podés sacar conclusiones engañosas.

---

## 3. Qué se mide

El análisis mira varias dimensiones de una firma:

| Dimensión | Variables típicas | Qué intenta capturar |
|---|---|---|
| Rentabilidad | `ROA`, `ROE`, `target_ROA` | Qué tan rentable es la firma. |
| Evitación/carga tributaria | `GETR`, `CETR`, `TSTA`, `TSDA`, variables `A_*` | Relación entre impuestos, resultados y activos. |
| Estabilidad financiera | `LEV`, `CUR`, `CFO` | Deuda, liquidez y flujo operativo. |
| Crecimiento y valoración | `GRW`, `MB`, `TQ` | Crecimiento y valoración de mercado. |
| Tamaño y estructura | `SIZE`, `PPE`, `AGE`, `INVREC` | Escala, activos físicos, edad e inventarios/cuentas por cobrar. |
| Gobierno/mercado | `forn`, `own`, `KOSPI`, `big4`, `LOSS` | Propiedad, mercado, auditoría y pérdidas. |

---

## 4. Qué es `target_ROA` y por qué es ROA futuro

`target_ROA` es la variable que queremos explicar. Se construye así:

```r
target_ROA(t) = ROA(t + 1)
```

Siempre se calcula **por firma**. Es decir:

| Firma | Año actual | `ROA` actual | `target_ROA` |
|---|---:|---:|---:|
| A | 2021 | ROA de 2021 | ROA de A en 2022 |
| A | 2022 | ROA de 2022 | ROA de A en 2023 |
| A | 2023 | ROA de 2023 | ROA de A en 2024 |

La lógica es: usamos características de la empresa en el año `t` para estudiar la rentabilidad del año `t + 1`.

---

## 5. Por qué la lógica temporal importa

El orden correcto es obligatorio:

1. Cargar y tipar el panel completo.
2. Crear `target_ROA` por firma sobre el panel completo.
3. Filtrar outliers de predictores con IQR × 3.
4. Separar temporalmente:
   - Train: `year <= 2022` y `target_ROA` válido.
   - Test: `year == 2023` y `target_ROA` válido.
   - Futuro: `year == 2024`, sin ROA futuro observado todavía.

> **No filtres outliers antes de crear `target_ROA`.** Si eliminás una fila intermedia, el `lead()` puede saltar de 2021 a 2023 y dejaría de significar “ROA del año siguiente”.

---

## 6. PCA snapshot vs modelo temporal OLS-PC

En el proyecto aparecen dos usos distintos de PCA. No son lo mismo.

| Flujo | Para qué sirve | Cómo usa el tiempo |
|---|---|---|
| PCA snapshot | Interpretar cómo se posicionan las empresas en un año específico. | Usa un solo año, por ejemplo 2023. |
| OLS-PC temporal | Explicar/predicir `target_ROA` con componentes principales y regresión OLS. | Aprende PCA con train histórico y evalúa test 2023. |

**PCA snapshot** responde: “¿cómo se agrupan o diferencian las firmas en este momento?”.  
**OLS-PC temporal** responde: “¿qué relación histórica hay entre perfiles financieros y ROA futuro?”.

---

## 7. Paquetes requeridos de R

Instalá primero los paquetes. En una consola de R:

```r
install.packages(c(
  "tidyverse", "dplyr", "ggplot2",
  "FactoMineR", "factoextra",
  "patchwork", "cluster", "scales", "ggrepel",
  "e1071", "data.table", "treemap", "ggtext", "corrplot",
  "lmtest", "car", "nortest", "testthat"
))
```

| Paquete | Uso principal |
|---|---|
| `tidyverse`, `dplyr`, `ggplot2` | Limpieza, transformación y gráficos. |
| `FactoMineR`, `factoextra` | PCA y visualizaciones de componentes principales. |
| `cluster` | K-Means y apoyo para clustering. |
| `lmtest`, `car`, `nortest` | Diagnósticos de regresión OLS. |
| `testthat` | Tests automáticos del proyecto. |

---

## 8. Orden exacto para ejecutar si sos principiante

### Paso 1 — Conseguí el CSV

Descargá `KoTaP_Dataset.csv` desde la fuente del proyecto y guardalo en una ruta local conocida.

### Paso 2 — Configurá rutas en el script principal

Abrí `R/Kotap analisis 9 de mayo.R` y ajustá:

```r
DATA_PATH  <- "ruta/a/KoTaP_Dataset.csv"
OUTPUT_DIR <- "ruta/a/output_kotap"
```

### Paso 3 — Cargá/prepará el dataset

Ejecutá el script principal o la preparación que deje en memoria el objeto `df` con el panel completo:

```r
source("R/Kotap analisis 9 de mayo.R")
```

> Nota: ese script puede tener pausas interactivas (`readline()`). Si lo corrés en modo no interactivo, revisá esas pausas antes.

### Paso 4 — Ejecutá el flujo operativo PCA + OLS-PC

Cuando ya exista `df` en memoria:

```r
ANIO_PCA <- 2023
source("R/AMTV_F.R")
```

Si aparece el error “No existe el objeto `df`”, volvé al paso 3. No es un error raro: significa que todavía no cargaste la base preparada.

### Paso 5 — Ejecutá tests solo si `testthat` está instalado

```r
source("tests/testthat.R")
```

Si falta `testthat`, primero instalalo. No afirmes que los tests pasaron si el paquete no está disponible.

---

## 9. Qué salidas y objetos esperar

Después de ejecutar `R/AMTV_F.R`, vas a ver objetos en memoria como estos:

| Objeto | Qué contiene |
|---|---|
| `df_con_target` | Dataset con `target_ROA` creado por firma. |
| `df_modelo` | Dataset después de filtrar outliers en predictores. |
| `particiones` | Lista con `train`, `test` y `future`. |
| `pca_snapshot` | PCA interpretativo del año elegido en `ANIO_PCA`. |
| `eigen_pca` | Eigenvalues y varianza explicada del PCA snapshot. |
| `p_pca_scree`, `p_pca_var`, `p_pca_ind`, `p_pca_biplot` | Gráficos de PCA en memoria. |
| `pca_train` | PCA aprendido solo con datos históricos de train. |
| `modelo_ols_pc` | Regresión OLS usando componentes principales. |
| `diagnosticos_ols_pc` | Residuos atípicos, leverage y Cook's D. |
| `comparacion_test_ols_pc` | Comparación real vs predicho para test 2023, si hay filas completas. |

El script metodológico principal también puede escribir archivos en `OUTPUT_DIR`, por ejemplo figuras, matrices de correlación/covarianza y resúmenes.

---

## 10. Mapa de archivos

| Archivo | Rol |
|---|---|
| `R/Kotap analisis 9 de mayo.R` | Referencia metodológica principal del trabajo. |
| `R/AMTV_F.R` | Flujo operativo con PCA snapshot, PCA temporal y OLS-PC. |
| `R/helpers_pca_ols.R` | Funciones reutilizables para target, outliers, split temporal, PCA y diagnósticos. |
| `R/nota_metodologica.md` | Nota corta para explicar temporalidad, PCA y OLS-PC. |
| `ORQUESTADOR.md` | Mapa de ejecución y revisión del proyecto. |
| `CLAUDE.md` | Instrucciones para Claude/agentes al iniciar trabajo. |
| `output/` | Salidas generadas localmente, si existen. |

---

## 11. Regla de oro

Si tenés una sola cosa para recordar, que sea esta:

> `target_ROA` se crea primero sobre el panel completo. Después vienen outliers, split temporal, PCA y OLS. Ese orden protege el significado de “rentabilidad del próximo año”.
