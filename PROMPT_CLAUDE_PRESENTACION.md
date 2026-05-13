# Prompt para Claude Design: presentación del proyecto KoTaP AMTV

Copiá y pegá este prompt en Claude Design para generar una presentación clara, visual y defendible sobre el proyecto.

---

## Prompt

Actuá como **diseñador académico senior de presentaciones** y ayudame a crear una presentación sobre un proyecto de análisis multivariado llamado **KoTaP AMTV**.

La audiencia no sabe nada del proyecto, por lo tanto la presentación debe ser **muy pedagógica**, visual y progresiva. No asumas conocimiento previo de panel de datos, PCA, regresión OLS ni diagnósticos de influencia.

### Objetivo de la presentación

Explicar cómo se usa un dataset financiero de empresas coreanas para entender el comportamiento de una variable objetivo construida:

```text
target_ROA(t) = ROA(t + 1)
```

Es decir: se usan características financieras de una empresa en el año actual `t` para estudiar su rentabilidad del año siguiente.

### Contexto del dataset

El dataset viene de Zenodo:

- Fuente: https://zenodo.org/records/17149808
- Título: KoTaP: A Panel Dataset for Corporate Tax Avoidance, Performance, and Governance in Korea (2011–2024)
- Tipo: panel firma-año de empresas coreanas listadas.
- Cada individuo/observación NO es solo una empresa, sino una **empresa en un año específico**.

Ejemplo:

| Empresa | Año | Individuo |
|---|---:|---|
| Samsung | 2021 | Samsung-2021 |
| Samsung | 2022 | Samsung-2022 |
| Hyundai | 2022 | Hyundai-2022 |

### Qué se mide en cada observación

Organizá las variables en estas dimensiones:

| Dimensión | Ejemplos | Qué representa |
|---|---|---|
| Rentabilidad | `ROA`, `CFO`, `LOSS` | desempeño financiero |
| Estabilidad | `LEV`, `CUR`, `SIZE`, `AGE`, `PPE` | deuda, liquidez, tamaño y estructura |
| Crecimiento/mercado | `GRW`, `MB`, `TQ` | crecimiento y valoración |
| Impuestos | `GETR`, `CETR`, `TSTA`, `TSDA` | carga/planeación tributaria |
| Gobierno corporativo | `forn`, `own`, `KOSPI`, `big4` | propiedad, mercado y auditoría |

### Decisión metodológica clave: temporalidad

Explicá con mucho cuidado:

1. `target_ROA` se crea primero sobre el panel completo.
2. Después se filtran outliers con IQR × 3.
3. Después se hace split temporal:
   - Train: años `<= 2022`
   - Test: año `2023`, para validar ROA 2024
   - Futuro: año `2024`, para proyectar ROA 2025 sin ground truth

Incluí una slide con esta idea:

```text
Variables 2022  → predicen ROA 2023
Variables 2023  → predicen ROA 2024
Variables 2024  → proyectan ROA 2025
```

También explicá por qué NO se debe filtrar antes de construir el target: si se elimina un año intermedio, el `lead()` puede saltar de `t` a `t+2`, rompiendo el significado de “ROA del próximo año”.

### PCA: dos usos distintos

Necesito que la presentación separe muy bien estos dos conceptos:

#### 1. PCA snapshot

Sirve para responder:

> ¿Cómo se posicionan las empresas en un año específico?

Usa un solo año, por ejemplo `2023`, para evitar mezclar efectos económicos de distintos periodos.

#### 2. PCA para modelo temporal OLS-PC

Sirve para reducir dimensión y evitar multicolinealidad antes de la regresión.

Debe aprenderse SOLO con train histórico y luego proyectarse sobre test.

No se debe aprender PCA usando test porque eso sería fuga de información.

### Modelo principal a explicar

Explicá el modelo como:

```text
target_ROA ~ PC1 + PC2 + ... + PCk
```

Donde `PC1...PCk` son componentes principales retenidos, no variables originales.

La idea central:

> En lugar de meter muchas variables financieras correlacionadas, usamos componentes principales que resumen patrones financieros y luego estimamos una regresión lineal múltiple sobre esos componentes.

### Diagnósticos del modelo

Incluí una sección didáctica sobre:

| Diagnóstico | Pregunta que responde |
|---|---|
| Residuos atípicos | ¿Qué observaciones tienen errores de predicción muy grandes? |
| Leverage alto | ¿Qué empresas-año tienen combinaciones raras de variables explicativas? |
| Cook's Distance | ¿Qué observaciones cambian mucho el modelo si se eliminan? |
| DFFITS | ¿Qué observaciones afectan fuertemente su propia predicción? |

Marcá que una observación influyente NO necesariamente es mala: puede ser un caso económico real que merece análisis.

### Archivos importantes del repo

Usá esta estructura para explicar cómo se organiza el proyecto:

| Archivo | Rol |
|---|---|
| `README.md` | guía para principiantes |
| `ORQUESTADOR.md` | mapa de lectura, ejecución y revisión |
| `R/Kotap analisis 9 de mayo.R` | referencia metodológica principal |
| `R/AMTV_F.R` | flujo operativo PCA snapshot + OLS-PC |
| `R/helpers_pca_ols.R` | funciones reutilizables y testeables |
| `R/nota_metodologica.md` | explicación breve de temporalidad, PCA y OLS-PC |
| `tests/testthat/` | pruebas sintéticas del pipeline |

### Estructura sugerida de slides

Generá una presentación de **12 a 15 slides** con esta estructura:

1. Título y pregunta de investigación.
2. Qué problema se quiere resolver.
3. Qué es KoTaP y de dónde vienen los datos.
4. Qué significa “firma-año”.
5. Qué mide el dataset.
6. Qué es `target_ROA`.
7. Por qué la temporalidad importa.
8. Flujo metodológico completo.
9. PCA snapshot: foto de un año.
10. PCA temporal para OLS-PC.
11. Regresión lineal múltiple con componentes principales.
12. Diagnósticos: atípicos, leverage e influencia.
13. Cómo leer los resultados.
14. Limitaciones y cuidados metodológicos.
15. Cierre: aporte del proyecto y próximos pasos.

### Estilo visual

- Diseño académico, limpio y moderno.
- Usá una paleta sobria: azul oscuro, teal, naranja suave y gris claro.
- Evitá saturar slides con texto.
- Usá diagramas de flujo, tablas cortas y ejemplos visuales.
- Cada slide debe tener un mensaje principal claro.
- Incluí notas del presentador para explicar cada slide.

### Tono

Escribí en español claro, con tono docente. La presentación debe servir para que alguien que no conoce el proyecto pueda entender:

1. qué se estudia,
2. qué es cada individuo,
3. qué se mide,
4. por qué el tiempo importa,
5. cómo PCA ayuda,
6. cómo se usa OLS con componentes principales,
7. y cómo se revisa si el modelo tiene observaciones problemáticas.

### Entregable esperado

Devolveme:

1. Un guion slide por slide.
2. Título de cada slide.
3. Texto breve para la slide.
4. Sugerencia visual de cada slide.
5. Notas del presentador.
6. Una slide final con “próximos pasos”.

No inventes resultados numéricos. Si necesitás mencionar resultados, dejá placeholders como:

```text
[Insertar varianza explicada de PC1]
[Insertar R² del modelo OLS-PC]
[Insertar número de observaciones influyentes]
```

La prioridad es que la presentación sea metodológicamente correcta y fácil de defender.
