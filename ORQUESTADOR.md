# Orquestador del proyecto KoTaP AMTV

Este documento es el **mapa de lectura, ejecución y revisión** del proyecto. Si una persona llega sin saber nada, debería usar este archivo para no perderse y para no romper la lógica temporal del análisis.

---

## 1. Qué leer primero, segundo y tercero

| Orden | Archivo | Para qué leerlo |
|---:|---|---|
| 1 | `README.md` | Entender qué es KoTaP, qué representa una firma-año y por qué `target_ROA` es ROA futuro. |
| 2 | `ORQUESTADOR.md` | Seguir el orden correcto de ejecución y revisión. Estás acá. |
| 3 | `R/nota_metodologica.md` | Preparar la explicación técnica de temporalidad, PCA snapshot y OLS-PC. |
| 4 | `R/Kotap analisis 9 de mayo.R` | Ver la referencia metodológica principal del trabajo. |
| 5 | `R/AMTV_F.R` | Ejecutar el flujo operativo cuando ya exista `df` en memoria. |

---

## 2. Idea central del análisis

| Concepto | Explicación para principiante |
|---|---|
| Observación | Una fila es una **firma-año**, no solamente una firma. |
| Objetivo | Explicar `target_ROA`: ROA del año siguiente de la misma firma. |
| Riesgo principal | Romper el orden temporal y terminar usando información mal alineada. |
| Regla clave | Crear `target_ROA` antes de filtrar outliers y antes de separar train/test/futuro. |

---

## 3. Scripts a ejecutar y en qué orden

### Orden recomendado

| Paso | Acción | Archivo/comando |
|---:|---|---|
| 1 | Instalar paquetes requeridos. | Ver `README.md` → “Paquetes requeridos de R”. |
| 2 | Configurar `DATA_PATH` y `OUTPUT_DIR`. | `R/Kotap analisis 9 de mayo.R` |
| 3 | Cargar y preparar el panel KoTaP completo. | `source("R/Kotap analisis 9 de mayo.R")` |
| 4 | Confirmar que existe `df` en memoria. | `exists("df")` |
| 5 | Elegir año del PCA interpretativo. | `ANIO_PCA <- 2023` |
| 6 | Ejecutar PCA snapshot + OLS-PC temporal. | `source("R/AMTV_F.R")` |
| 7 | Ejecutar tests si `testthat` está instalado. | `source("tests/testthat.R")` |

### Comando mínimo del flujo operativo

```r
# Primero debe existir df, creado por la preparación principal.
exists("df")

ANIO_PCA <- 2023
source("R/AMTV_F.R")
```

Si `exists("df")` devuelve `FALSE`, no corras `R/AMTV_F.R` todavía. Primero cargá/prepará la base.

---

## 4. Qué hace cada script importante

| Archivo | Rol | Cuándo tocarlo |
|---|---|---|
| `R/Kotap analisis 9 de mayo.R` | Referencia metodológica principal: carga datos, crea target, EDA, PCA, K-Means, OLS. | Solo si vas a cambiar la metodología central. |
| `R/AMTV_F.R` | Flujo operativo enfocado: PCA snapshot + PCA temporal + OLS-PC. | Cuando querés correr el análisis operativo sobre `df`. |
| `R/helpers_pca_ols.R` | Funciones auxiliares: target, outliers, split temporal, PCA, OLS y diagnósticos. | Solo si necesitás modificar lógica reutilizable. |
| `R/Dataset_Nuevo.R` | Preparación/exploración de dataset. | Con cuidado; puede contener variantes exploratorias. |
| `R/shiny/app.R` | Dashboard interactivo. | Solo si el objetivo es la app, no el análisis central. |

---

## 5. Qué NO tocar sin una razón clara

| No tocar | Por qué |
|---|---|
| Orden target → outliers → split temporal | Si se cambia, `target_ROA` puede dejar de ser ROA del próximo año. |
| Definición de `target_ROA` | Es la variable central del proyecto. |
| Variables excluidas de PCA: `ROA`, `ROE`, `target_ROA` | Pueden contaminar la interpretación o introducir endogeneidad. |
| Test 2023 usado como validación | No debe usarse para aprender la rotación PCA del modelo temporal. |
| Métodos avanzados de ML en el análisis principal | El proyecto debe mantenerse dentro de métodos clásicos del curso. |
| Rutas locales hardcodeadas sin documentarlas | Otra persona no va a poder ejecutar el proyecto. |

---

## 6. Flujo interno de `R/AMTV_F.R`

| Paso | Objeto principal | Decisión clave |
|---|---|---|
| Target | `df_con_target` | `target_ROA` se crea antes de outliers. |
| Outliers | `df_modelo` | IQR × 3 solo sobre predictores. |
| Split | `particiones` | Train/test/futuro respetan años. |
| PCA snapshot | `pca_snapshot` | Posicionamiento visual del año `ANIO_PCA`. |
| PCA train | `pca_train` | Se aprende solo con train histórico. |
| OLS-PC | `modelo_ols_pc` | Usa `PC1...PCk` como predictores. |
| Diagnósticos | `diagnosticos_ols_pc` | Revisa residuos, leverage y Cook's D. |
| Test temporal | `comparacion_test_ols_pc` | Compara ROA futuro real vs predicho para 2023, si hay datos completos. |

---

## 7. Verificación rápida antes de entregar

- [ ] El README explica que una observación es una **firma-año**.
- [ ] `target_ROA` está explicado como `ROA(t + 1)` por firma.
- [ ] El target se crea antes de filtrar outliers.
- [ ] El split temporal sigue: train `<= 2022`, test `2023`, futuro `2024`.
- [ ] El PCA snapshot se presenta como interpretación de un año, no como validación predictiva.
- [ ] El OLS-PC temporal aprende PCA solo con train.
- [ ] `ROA`, `ROE` y `target_ROA` no se usan como variables explicativas del PCA principal.
- [ ] Los métodos centrales siguen siendo clásicos: descriptivos, correlación, PCA, K-Means y OLS.
- [ ] Si se mencionan tests, `testthat` está instalado y la ejecución realmente ocurrió.
- [ ] No se afirma que “pasó todo” si faltó un paquete o no se ejecutó el test.

---

## 8. Cómo revisar resultados

| Qué querés revisar | Objeto/salida |
|---|---|
| Variables usadas en PCA | Mensaje “PCA – variables usadas” y objeto `df_pca`. |
| Varianza explicada | `eigen_pca` y gráfico `p_pca_scree`. |
| Mapa visual de firmas | `p_pca_ind` y `p_pca_biplot`. |
| Modelo OLS-PC | `summary(modelo_ols_pc)`. |
| Observaciones problemáticas | `diagnosticos_ols_pc$residuos_atipicos`, `$leverage`, `$cook`. |
| Validación 2023 | `comparacion_test_ols_pc`. |

---

## 9. Próximos pasos sugeridos

1. Confirmar que todas las rutas locales (`DATA_PATH`, `OUTPUT_DIR`) están documentadas o configuradas.
2. Instalar `testthat` y ejecutar `source("tests/testthat.R")`.
3. Revisar observaciones influyentes: pueden ser errores de datos o casos económicos reales.
4. Preparar una explicación corta de por qué `target_ROA` es futuro y por qué el tiempo importa.
5. Mantener cualquier extensión avanzada fuera del núcleo metodológico del curso.

---

## 10. Regla final del orquestador

Antes de cambiar algo, preguntate:

> ¿Este cambio respeta que usamos información del año `t` para explicar ROA del año `t + 1`?

Si la respuesta no es claramente “sí”, frená y revisá la metodología. Mejor ir lento y correcto que rápido y mal alineado.
