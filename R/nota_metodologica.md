# Nota metodológica: temporalidad, PCA y OLS-PC

## Decisión central

`target_ROA` mide rentabilidad futura: para cada firma, `target_ROA(t) = ROA(t + 1)`. El análisis usa variables financieras del año `t` para explicar ROA del año siguiente.

## Por qué el target va antes de outliers

El target se crea sobre el panel completo. Después se filtran outliers de predictores.

Si se filtra antes, una firma podría perder el año `t + 1`; entonces el `lead()` saltaría a `t + 2` y la variable dejaría de representar el próximo año. Ese error parece chico en código, pero rompe la causalidad temporal del análisis.

## Dos PCA distintos

| PCA | Objeto | Uso | Regla temporal |
|---|---|---|---|
| Snapshot | `pca_snapshot` | Interpretar cómo se posicionan las firmas en `ANIO_PCA` | Usa un corte transversal. |
| OLS-PC | `pca_train` | Generar componentes para regresión y validación | Se entrena solo con `year <= 2022`; test 2023 se proyecta sin reentrenar. |

No se debe usar `pca_snapshot` para el OLS-PC. El primero es descriptivo; el segundo pertenece al flujo temporal de modelado.

## Regresión OLS con componentes

El modelo estimado es:

```text
target_ROA ~ PC1 + PC2 + ... + PCk
```

`k` se define por la regla de Kaiser: componentes con eigenvalue `>= 1`. No entran variables financieras crudas al OLS-PC, para evitar mezclar interpretaciones y reducir colinealidad.

## Diagnósticos incluidos

| Diagnóstico | Umbral | Lectura |
|---|---|---|
| Residuo atípico | `|rstandard| > 2`; crítico si `> 3` | Observación mal explicada por el modelo. |
| Alto leverage | `h_ii > 2p/n` | Observación con posición extrema en los predictores. |
| Cook's D | `D > 4/n` | Observación potencialmente influyente en coeficientes. |

Estas tablas no eliminan filas automáticamente: señalan casos que requieren interpretación económica o revisión de datos.
