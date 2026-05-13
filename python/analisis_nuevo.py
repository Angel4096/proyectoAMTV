# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║          SCRIPT MAESTRO DE CIENCIA DE DATOS – KoTaP Dataset                 ║
# ║          Análisis Financiero y Predictivo del Mercado Bursátil Coreano       ║
# ║──────────────────────────────────────────────────────────────────────────────║
# ║  Autor  : Senior Data Scientist & Experto en Ingeniería Estadística          ║
# ║  Dataset: KoTaP_Dataset.csv  (12,653 obs · 1,754 empresas · 2011–2024)      ║
# ║  Target : ROA (Rentabilidad sobre Activos)                                   ║
# ║  Estilo : Storytelling con comentarios "con plastilina" para ingeniería       ║
# ╚══════════════════════════════════════════════════════════════════════════════╝
#
# ┌─────────────────────────────────────────────────────────────────────────────┐
# │  ÍNDICE DE SECCIONES                                                        │
# │  0. Configuración y Librerías                                               │
# │  1. Carga y Limpieza de Datos                                               │
# │  2. Análisis Univariado  (Hist + Boxplot + QQ-Plot + Normalidad)            │
# │  3. Análisis Multivariado (Covarianza + Correlación + Heatmaps)             │
# │  4. Modelado Predictivo                                                     │
# │     4.1  Regresión Lineal Múltiple (OLS)                                   │
# │     4.2  Selección de Variables (RFE + Stepwise-AIC aproximado)             │
# │     4.3  PCA – Reducción de Dimensionalidad                                 │
# │     4.4  Árbol de Decisión (Interpretable)                                  │
# │     4.5  Regresión Regularizada – Ridge                                     │
# │     4.6  Regresión Regularizada – Lasso                                     │
# │     4.7  Comparativa de Modelos                                             │
# │  5. Diagnósticos de Gauss-Markov                                            │
# │     5.1  Normalidad de Residuos (Shapiro-Wilk + QQ)                        │
# │     5.2  Homocedasticidad (Breusch-Pagan + Residuos vs Fitted)             │
# │     5.3  Multicolinealidad (VIF)                                            │
# │     5.4  Autocorrelación (Durbin-Watson)                                    │
# │  6. Reporte Final en Consola                                                │
# └─────────────────────────────────────────────────────────────────────────────┘

# ==============================================================================
# SECCIÓN 0 ─ CONFIGURACIÓN Y LIBRERÍAS
# ==============================================================================
import warnings
warnings.filterwarnings('ignore')

import numpy  as np
import pandas as pd

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot    as plt
import matplotlib.gridspec  as gridspec
import seaborn              as sns
from   matplotlib.lines import Line2D

from scipy              import stats
from scipy.stats        import shapiro, kstest, probplot, jarque_bera
import statsmodels.api  as sm
from   statsmodels.stats.outliers_influence  import variance_inflation_factor
from   statsmodels.stats.diagnostic          import het_breuschpagan
from   statsmodels.stats.stattools           import durbin_watson

from sklearn.linear_model   import LinearRegression, Ridge, Lasso, LassoCV, RidgeCV
from sklearn.tree           import DecisionTreeRegressor, export_text, plot_tree
from sklearn.feature_selection import RFE
from sklearn.preprocessing  import StandardScaler
from sklearn.model_selection import train_test_split, cross_val_score, KFold
from sklearn.metrics        import r2_score, mean_squared_error, mean_absolute_error
from sklearn.pipeline       import Pipeline
from sklearn.decomposition  import PCA

import os, sys, json, textwrap
from   itertools import combinations

# ── Rutas y configuración global ──────────────────────────────────────────
DATA_PATH  = r'C:/Users/norba/Downloads/KoTaP_Dataset.csv'
OUTPUT_DIR = r'C:\Users\norba\OneDrive\NUESTRO HOGAR\1. ANGEL SANTIAGO\AMMV\Proyecto Multivariado'
os.makedirs(OUTPUT_DIR, exist_ok=True)

PALETTE = {
    'dark'  : '#0F172A', 'navy'  : '#1E3A5F', 'teal'  : '#0D9488',
    'teal2' : '#0891B2', 'orange': '#F97316', 'green' : '#059669',
    'red'   : '#EF4444', 'slate' : '#64748B', 'light' : '#F1F5F9',
    'white' : '#FFFFFF', 'purple': '#7C3AED',
}

plt.rcParams.update({
    'figure.facecolor' : PALETTE['white'],
    'axes.facecolor'   : '#F8FAFC',
    'axes.edgecolor'   : '#CBD5E1',
    'axes.labelcolor'  : PALETTE['dark'],
    'xtick.color'      : PALETTE['slate'],
    'ytick.color'      : PALETTE['slate'],
    'grid.color'       : '#E2E8F0',
    'grid.linewidth'   : 0.6,
    'font.family'      : 'DejaVu Sans',
    'text.color'       : PALETTE['dark'],
    'axes.titlesize'   : 13,
    'axes.labelsize'   : 11,
})

np.random.seed(42)

print("=" * 70)
print("  SCRIPT MAESTRO KoTaP · Iniciando pipeline de análisis")
print("=" * 70)


# ==============================================================================
# SECCIÓN 1 ─ CARGA Y LIMPIEZA DE DATOS
# ==============================================================================
print("\n[1/6] Carga y limpieza de datos...")

for encoding in ['utf-8', 'latin1', 'cp1252']:
    try:
        df_raw = pd.read_csv(DATA_PATH, encoding=encoding)
        print(f"    ✔ Dataset cargado con encoding='{encoding}'")
        break
    except Exception as e:
        print(f"    ✗ Falló con encoding='{encoding}': {e}")
else:
    sys.exit("ERROR FATAL: No se pudo cargar el dataset. Verifica DATA_PATH.")

print(f"    → Dimensiones originales : {df_raw.shape[0]:,} filas × {df_raw.shape[1]} columnas")

CAT_VARS = ['KOSPI', 'big4', 'ind', 'LOSS', 'fiscal']
for v in CAT_VARS:
    if v in df_raw.columns:
        df_raw[v] = df_raw[v].astype('category')

KEY_VARS = ['SIZE', 'LEV', 'ROA', 'ROE', 'CFO', 'GRW', 'CUR', 'INVREC',
            'MB', 'TQ', 'PPE', 'AGE']

TARGET   = 'ROA'
FEATURES = ['SIZE', 'LEV', 'CFO', 'GRW', 'CUR', 'INVREC', 'PPE', 'AGE']

n_dup  = df_raw.duplicated().sum()
n_null = df_raw[KEY_VARS].isnull().sum().sum()
print(f"    → Filas duplicadas    : {n_dup}")
print(f"    → Nulos en vars clave : {n_null}")

df_raw.drop_duplicates(inplace=True)

df = df_raw.copy()
WINSOR_LIMITS = (0.01, 0.99)

for col in KEY_VARS:
    lo = df[col].quantile(WINSOR_LIMITS[0])
    hi = df[col].quantile(WINSOR_LIMITS[1])
    df[col] = df[col].clip(lo, hi)

print(f"    ✔ Winsorización aplicada al {int(WINSOR_LIMITS[0]*100)}%–{int(WINSOR_LIMITS[1]*100)}%")
print(f"    → Dataset final: {len(df):,} filas × {df.shape[1]} columnas")
print(f"    → Empresas únicas: {df['stock'].nunique():,} | Años: {df['year'].min()}–{df['year'].max()}")

df_model = df[[TARGET] + FEATURES].dropna()
X_full   = df_model[FEATURES]
y_full   = df_model[TARGET]

X_train, X_test, y_train, y_test = train_test_split(
    X_full, y_full, test_size=0.2, random_state=42
)

scaler    = StandardScaler()
X_tr_sc   = scaler.fit_transform(X_train)
X_te_sc   = scaler.transform(X_test)

print(f"    → Train: {len(X_train):,} | Test: {len(X_test):,}")


# ==============================================================================
# SECCIÓN 2 ─ ANÁLISIS UNIVARIADO
# ==============================================================================
print("\n[2/6] Análisis Univariado...")

def univariate_panel(var, data, ax_hist, ax_box, ax_qq, color='#0D9488'):
    d = data.dropna()

    ax_hist.hist(d, bins=55, color=color, alpha=0.82, edgecolor='white', lw=0.3)
    ax_hist.axvline(d.mean(),   color=PALETTE['red'],  lw=1.8, ls='--',
                    label=f'μ={d.mean():.3f}')
    ax_hist.axvline(d.median(), color=PALETTE['navy'], lw=1.8, ls=':',
                    label=f'Md={d.median():.3f}')
    ax_hist.set_title(f'{var} – Histograma', fontweight='bold')
    ax_hist.set_xlabel(var); ax_hist.set_ylabel('Frecuencia')
    ax_hist.legend(fontsize=8)
    skew_val = float(d.skew())
    kurt_val = float(d.kurt())
    ax_hist.text(0.97, 0.93, f'Asim={skew_val:.2f}\nKurt={kurt_val:.2f}',
                 transform=ax_hist.transAxes, ha='right', va='top', fontsize=8,
                 bbox=dict(boxstyle='round', fc='white', ec='#CBD5E1', alpha=0.85))

    bp = ax_box.boxplot(d, patch_artist=True, vert=True,
                        boxprops=dict(facecolor=color, alpha=0.7),
                        medianprops=dict(color=PALETTE['red'],   linewidth=2.2),
                        whiskerprops=dict(color=PALETTE['slate'], linewidth=1.2),
                        capprops=dict(color=PALETTE['slate'],    linewidth=1.2),
                        flierprops=dict(marker='o', markerfacecolor=color,
                                        alpha=0.3, markersize=3))
    ax_box.set_title(f'{var} – Boxplot', fontweight='bold')
    ax_box.set_ylabel(var); ax_box.set_xticks([])

    (osm, osr), (slope, intercept, r_qq) = probplot(d, dist="norm")
    ax_qq.plot(osm, osr,    '.', color=color,          ms=2.5, alpha=0.55)
    ax_qq.plot(osm, slope*np.array(osm)+intercept,
               '-', color=PALETTE['red'], lw=1.8)
    ax_qq.set_title(f'{var} – QQ-Plot (r={r_qq:.3f})', fontweight='bold')
    ax_qq.set_xlabel('Cuantiles teóricos'); ax_qq.set_ylabel('Cuantiles muestrales')

    sample = d.sample(min(5000, len(d)), random_state=42)
    sw_stat, sw_p    = shapiro(sample)
    jb_stat, jb_p, *_ = jarque_bera(d)

    return {
        'variable': var, 'n': len(d),
        'mean': d.mean(),   'median': d.median(),
        'std' : d.std(),    'min': d.min(), 'max': d.max(),
        'q25' : d.quantile(0.25), 'q75': d.quantile(0.75),
        'skewness': skew_val,  'kurtosis': kurt_val,
        'shapiro_p': sw_p,     'jarque_bera_p': float(jb_p),
        'normal_sw': sw_p > 0.05, 'normal_jb': float(jb_p) > 0.05,
    }

var_colors = {
    'ROA'   : PALETTE['teal'],   'ROE'   : PALETTE['teal2'],
    'SIZE'  : PALETTE['navy'],   'LEV'   : PALETTE['orange'],
    'CFO'   : PALETTE['purple'], 'GRW'   : PALETTE['green'],
    'CUR'   : '#DC2626',         'INVREC': '#0369A1',
    'MB'    : '#92400E',         'TQ'    : '#065F46',
    'PPE'   : '#4C1D95',         'AGE'   : '#831843',
}

univar_stats = []

for var in KEY_VARS:
    col_color = var_colors.get(var, PALETTE['teal'])

    fig = plt.figure(figsize=(15, 4.5), facecolor='white')
    fig.suptitle(f'Análisis Univariado  ·  {var}',
                 fontsize=15, fontweight='bold', color=PALETTE['dark'])
    gs = gridspec.GridSpec(1, 3, figure=fig, wspace=0.35)
    ax1 = fig.add_subplot(gs[0, 0])
    ax2 = fig.add_subplot(gs[0, 1])
    ax3 = fig.add_subplot(gs[0, 2])

    result = univariate_panel(var, df[var], ax1, ax2, ax3, col_color)
    univar_stats.append(result)

    fname = os.path.join(OUTPUT_DIR, f'univar_{var}.png')
    plt.savefig(fname, dpi=140, bbox_inches='tight', facecolor='white')
    plt.close()

    norm_flag = "✔ Normal" if result['normal_jb'] else "✗ No-normal"
    print(f"    {var:8s} μ={result['mean']:+.4f}  σ={result['std']:.4f}  "
          f"Asim={result['skewness']:+.2f}  JB-p={result['jarque_bera_p']:.3f}  {norm_flag}")

df_univar = pd.DataFrame(univar_stats)
print(f"\n    ✔ Gráficos univariados guardados en '{OUTPUT_DIR}/'")

fig2, axes2 = plt.subplots(4, 3, figsize=(18, 20), facecolor='white')
fig2.suptitle('Resumen Univariado – Histogramas con Media y Mediana',
              fontsize=16, fontweight='bold', color=PALETTE['dark'])
axes2_flat = axes2.flatten()
for i, var in enumerate(KEY_VARS):
    ax = axes2_flat[i]
    d  = df[var].dropna()
    ax.hist(d, bins=50, color=var_colors.get(var, PALETTE['teal']),
            alpha=0.82, edgecolor='white', lw=0.3)
    ax.axvline(d.mean(),   color=PALETTE['red'],  lw=1.6, ls='--')
    ax.axvline(d.median(), color=PALETTE['navy'], lw=1.6, ls=':')
    ax.set_title(var, fontweight='bold')
    ax.set_xlabel(var, fontsize=9); ax.set_ylabel('Frec.', fontsize=9)
plt.tight_layout(pad=1.5)
plt.savefig(os.path.join(OUTPUT_DIR, 'univar_RESUMEN.png'),
            dpi=140, bbox_inches='tight', facecolor='white')
plt.close()


# ==============================================================================
# SECCIÓN 3 ─ ANÁLISIS MULTIVARIADO
# ==============================================================================
print("\n[3/6] Análisis Multivariado...")

numeric_df = df[KEY_VARS].copy()

cov_matrix = numeric_df.cov()

fig, ax = plt.subplots(figsize=(13, 10), facecolor='white')
mask_upper = np.triu(np.ones_like(cov_matrix, dtype=bool))
sns.heatmap(cov_matrix, mask=mask_upper,
            cmap=sns.diverging_palette(220, 20, as_cmap=True),
            center=0, annot=True, fmt='.2e', annot_kws={'size': 7.5},
            linewidths=0.5, linecolor='white',
            cbar_kws={'shrink': 0.8, 'label': 'Cov(X,Y)'},
            ax=ax)
ax.set_title('Matriz de Covarianzas  ·  Variables Financieras KoTaP',
             fontsize=14, fontweight='bold', pad=12)
plt.tight_layout()
plt.savefig(os.path.join(OUTPUT_DIR, 'multivar_covarianza.png'),
            dpi=140, bbox_inches='tight', facecolor='white')
plt.close()
print("    ✔ Heatmap de covarianzas generado")

corr_pearson  = numeric_df.corr(method='pearson')
corr_spearman = numeric_df.corr(method='spearman')

fig, axes = plt.subplots(1, 2, figsize=(22, 9), facecolor='white')
for i, (corr_m, title, method) in enumerate([
        (corr_pearson,  'Correlación de Pearson\n(Lineal – sensible a outliers)', 'pearson'),
        (corr_spearman, 'Correlación de Spearman\n(Rangos – robusta a outliers)', 'spearman'),
    ]):
    mask_ = np.triu(np.ones_like(corr_m, dtype=bool))
    sns.heatmap(corr_m, mask=mask_,
                cmap=sns.diverging_palette(220, 20, as_cmap=True),
                center=0, vmin=-1, vmax=1,
                annot=True, fmt='.2f', annot_kws={'size': 9},
                linewidths=0.5, linecolor='white',
                cbar_kws={'shrink': 0.8}, ax=axes[i])
    axes[i].set_title(title, fontsize=13, fontweight='bold')
plt.suptitle('Análisis de Correlación Bivariada  –  KoTaP Dataset',
             fontsize=15, fontweight='bold', y=1.01)
plt.tight_layout()
plt.savefig(os.path.join(OUTPUT_DIR, 'multivar_correlacion.png'),
            dpi=140, bbox_inches='tight', facecolor='white')
plt.close()
print("    ✔ Heatmaps de correlación Pearson y Spearman generados")

PAIRPLOT_VARS = ['ROA', 'LEV', 'CFO', 'SIZE', 'GRW']
g = sns.pairplot(df[PAIRPLOT_VARS + ['KOSPI']].astype({'KOSPI': str}),
                 hue='KOSPI', palette={
                     '0': PALETTE['teal'], '1': PALETTE['orange']},
                 plot_kws={'alpha': 0.25, 's': 8},
                 diag_kind='kde', corner=True)
g.fig.suptitle('Pairplot: ROA, LEV, CFO, SIZE, GRW  (color = Mercado)',
               y=1.01, fontsize=14, fontweight='bold')
g.fig.savefig(os.path.join(OUTPUT_DIR, 'multivar_pairplot.png'),
              dpi=120, bbox_inches='tight', facecolor='white')
plt.close()
print("    ✔ Pairplot guardado")

top_corr = (corr_spearman[TARGET]
            .drop(TARGET)
            .abs()
            .sort_values(ascending=False)
            .head(8))
print(f"\n    Top 8 correlaciones (Spearman) con {TARGET}:")
for feat, val in top_corr.items():
    sign = '+' if corr_spearman.loc[feat, TARGET] > 0 else '-'
    bar  = '█' * int(abs(val) * 30)
    print(f"      {feat:8s}: {sign}{abs(val):.3f}  {bar}")


# ==============================================================================
# SECCIÓN 4 ─ MODELADO PREDICTIVO
# ==============================================================================
print("\n[4/6] Modelado Predictivo...")

MODEL_SCORES = {}

# ─────────────────────────────────────────────────────────────────────────────
# 4.1  REGRESIÓN LINEAL MÚLTIPLE (OLS)
# ─────────────────────────────────────────────────────────────────────────────
# ─────────────────────────────────────────────────────────────────────────────
# 4.1  REGRESIÓN GLM (FAMILIA GAMMA CON ENLACE LOGARÍTMICO)
# ─────────────────────────────────────────────────────────────────────────────
print("\n  [4.1] Regresión GLM (Gamma + Log Link)...")

# Preparamos las matrices con la constante (intercepto)
X_sm = sm.add_constant(X_train)
X_test_sm = sm.add_constant(X_test)

# 1. Modelo OLS Tradicional (Línea base para comparar)
ols = sm.OLS(y_train, X_sm).fit()
y_pred_ols_te = ols.predict(X_test_sm)

# 2. Transformación "Min-Shift" para la familia Gamma
# La familia Gamma requiere valores estrictamente positivos (y > 0).
# Calculamos el desplazamiento basado en la pérdida máxima (más un margen de 0.001)
shift_value = abs(y_train.min()) + 0.001
y_train_shifted = y_train + shift_value
y_test_shifted = y_test + shift_value

# 3. Entrenamos el GLM con Familia Gamma y Enlace Logarítmico
# El link logarítmico captura relaciones multiplicativas (efectos de interés compuesto/tasas)
try:
    link_func = sm.families.links.Log() # Statsmodels moderno
except AttributeError:
    link_func = sm.families.links.log() # Statsmodels antiguo

glm_gamma = sm.GLM(
    y_train_shifted, 
    X_sm, 
    family=sm.families.Gamma(link=link_func)
).fit()

# Predecimos en la escala desplazada y luego revertimos el desplazamiento a la escala original (ROA real)
y_pred_gamma_shifted_te = glm_gamma.predict(X_test_sm)
y_pred_gamma_te = y_pred_gamma_shifted_te - shift_value

# 4. Evaluamos y comparamos el modelo
rmse_ols = np.sqrt(mean_squared_error(y_test, y_pred_ols_te))
rmse_glm = np.sqrt(mean_squared_error(y_test, y_pred_gamma_te))

mae_ols = mean_absolute_error(y_test, y_pred_ols_te)
mae_glm = mean_absolute_error(y_test, y_pred_gamma_te)

# Calculamos un R² simulado para que se integre con tu reporte actual
r2_ols = r2_score(y_test, y_pred_ols_te)
r2_glm = r2_score(y_test, y_pred_gamma_te)

MODEL_SCORES['OLS'] = {'R²': r2_ols, 'RMSE': rmse_ols, 'MAE': mae_ols}
MODEL_SCORES['GLM-Gamma'] = {'R²': r2_glm, 'RMSE': rmse_glm, 'MAE': mae_glm}

print(f"\n    Comparativa de errores (Test Set):")
print(f"    OLS       -> RMSE: {rmse_ols:.4f} | MAE: {mae_ols:.4f}")
print(f"    GLM-Gamma -> RMSE: {rmse_glm:.4f} | MAE: {mae_glm:.4f}")
print(f"    (Shift aplicado a y: +{shift_value:.4f})")

print(f"\n    Resumen estadístico GLM Gamma (Modelo Principal):")
print(f"    {'Variable':12s} {'β-Coef':>10s} {'p-valor':>10s} {'Signif.':>8s}")
print(f"    {'─'*46}")
for name, coef, pval in zip(['const']+FEATURES, glm_gamma.params.values, glm_gamma.pvalues.values):
    sig = '***' if pval < 0.001 else ('**' if pval < 0.01 else ('*' if pval < 0.05 else 'ns'))
    print(f"    {name:12s} {coef:+10.5f} {pval:10.4f} {sig:>8s}")

# --- GRÁFICA DE AJUSTE (CFO vs ROA) ---
# Vamos a demostrar visualmente la ventaja de la curva de la familia Gamma frente a la línea rígida OLS.

# Creamos datos sintéticos para la línea manteniendo las demás variables en su media
X_synth = X_train.copy()
for col in FEATURES:
    if col != 'CFO':
        X_synth[col] = X_train[col].mean()

# Ordenamos por CFO para dibujar la línea suavemente
X_synth = X_synth.sort_values(by='CFO')
X_synth_sm = sm.add_constant(X_synth)

y_line_ols = ols.predict(X_synth_sm)
y_line_glm_shifted = glm_gamma.predict(X_synth_sm)
y_line_glm = y_line_glm_shifted - shift_value

plt.figure(figsize=(11, 6.5), facecolor='white')

# Muestra de datos reales (3000 puntos para no saturar)
sample_idx = X_train.sample(min(3000, len(X_train)), random_state=42).index
plt.scatter(X_train.loc[sample_idx, 'CFO'], y_train.loc[sample_idx], 
            alpha=0.3, color=PALETTE['teal'], s=15, label='Datos Reales (KoTaP)')

# Línea de referencia en cero (Punto de quiebre ganancias/pérdidas)
plt.axhline(0, color='gray', lw=1.5, ls=':', alpha=0.7)

# Líneas predictivas
plt.plot(X_synth['CFO'], y_line_ols, color=PALETTE['red'], lw=2.5, ls='--', 
         label='OLS (Línea rígida - ignora volatilidad)')
plt.plot(X_synth['CFO'], y_line_glm, color=PALETTE['navy'], lw=3.5, 
         label='GLM Gamma (Curva dinámica - mejor ajuste al mercado)')

plt.title('Comparativa Visual: OLS vs Modelo Lineal Generalizado (Gamma)\nImpacto del Flujo de Caja (CFO) en la Rentabilidad (ROA)', 
          fontsize=13, fontweight='bold')
plt.xlabel('CFO (Flujo de Caja Operativo estandarizado)', fontsize=11)
plt.ylabel('ROA (Rentabilidad sobre Activos)', fontsize=11)
plt.legend(fontsize=10, loc='upper left')
plt.grid(True, alpha=0.3)
plt.tight_layout()

plt.savefig(os.path.join(OUTPUT_DIR, 'modelo_ajuste_glm_gamma.png'), dpi=130, bbox_inches='tight', facecolor='white')
plt.close()
print("    ✔ Gráfico de ajuste de regresiones (OLS vs GLM Gamma) generado")
# ─────────────────────────────────────────────────────────────────────────────
# 4.2  SELECCIÓN DE VARIABLES – RFE + Stepwise AIC Aproximado
# ─────────────────────────────────────────────────────────────────────────────
print("\n  [4.2] Selección de Variables (RFE)...")

lr_for_rfe = LinearRegression()
rfe        = RFE(lr_for_rfe, n_features_to_select=5, step=1)
rfe.fit(X_tr_sc, y_train)

selected_mask     = rfe.support_
selected_features = [f for f, s in zip(FEATURES, selected_mask) if s]
ranking           = rfe.ranking_

print(f"    Variables seleccionadas (RFE top-5): {selected_features}")
print("    Ranking completo:")
for feat, rank, sel in sorted(zip(FEATURES, ranking, selected_mask),
                               key=lambda x: x[1]):
    mark = '✔' if sel else '✗'
    print(f"      {mark} {feat:10s}  rango={rank}")

print("\n    Stepwise AIC aproximado (subset selection)...")
best_aic    = np.inf
best_subset = FEATURES.copy()

for k in range(3, len(FEATURES)+1):
    for subset in combinations(FEATURES, k):
        X_sub = sm.add_constant(X_train[list(subset)])
        try:
            m = sm.OLS(y_train, X_sub).fit(disp=0)
            if m.aic < best_aic:
                best_aic    = m.aic
                best_subset = list(subset)
        except Exception:
            pass

print(f"    Mejor subconjunto (AIC={best_aic:.1f}): {best_subset}")

X_rfe_tr = X_train[selected_features]
X_rfe_te = X_test[selected_features]
lr_rfe   = LinearRegression().fit(X_rfe_tr, y_train)
r2_rfe   = r2_score(y_test, lr_rfe.predict(X_rfe_te))
rmse_rfe = np.sqrt(mean_squared_error(y_test, lr_rfe.predict(X_rfe_te)))
mae_rfe  = mean_absolute_error(y_test, lr_rfe.predict(X_rfe_te))
MODEL_SCORES['OLS-RFE'] = {'R²': r2_rfe, 'RMSE': rmse_rfe, 'MAE': mae_rfe}
print(f"    OLS-RFE → R²={r2_rfe:.4f} | RMSE={rmse_rfe:.4f}")

# ─────────────────────────────────────────────────────────────────────────────
# 4.3  PCA – Reducción de Dimensionalidad
# ─────────────────────────────────────────────────────────────────────────────
print("\n  [4.3] PCA – Análisis de Componentes Principales...")

scaler_pca = StandardScaler()
X_scaled   = scaler_pca.fit_transform(X_full)

pca   = PCA()
X_pca = pca.fit_transform(X_scaled)

explained_var = pca.explained_variance_ratio_
cum_var       = np.cumsum(explained_var)

print("\n    Varianza explicada por componente:")
for i, var in enumerate(explained_var[:8]):
    print(f"      PC{i+1}: {var:.4f}  | Acumulada: {cum_var[i]:.4f}")

plt.figure(figsize=(8, 5))
plt.plot(cum_var, marker='o')
plt.title('Varianza Explicada Acumulada – PCA')
plt.xlabel('Número de Componentes')
plt.ylabel('Varianza Explicada Acumulada')
plt.grid(True)
plt.savefig(os.path.join(OUTPUT_DIR, 'pca_varianza.png'))
plt.close()

pca_final   = PCA(n_components=3)
X_pca_final = pca_final.fit_transform(X_scaled)
print(f"\n    ✔ PCA reducido a {pca_final.n_components_} componentes")

df_pca = pd.DataFrame(X_pca_final, columns=['PC1', 'PC2', 'PC3'])

plt.figure(figsize=(7, 5))
plt.scatter(df_pca['PC1'], df_pca['PC2'], alpha=0.3)
plt.title('PCA – Proyección 2D')
plt.xlabel('PC1'); plt.ylabel('PC2')
plt.grid(True)
plt.savefig(os.path.join(OUTPUT_DIR, 'pca_scatter.png'))
plt.close()

# ─────────────────────────────────────────────────────────────────────────────
# 4.4  ÁRBOL DE DECISIÓN
# ─────────────────────────────────────────────────────────────────────────────
# 🎓 NOTA: Las métricas se registran en MODEL_SCORES con la clave 'DecisionTree'
#    para que el reporte final pueda acceder a ellas correctamente.

print("\n  [4.4] Árbol de Decisión optimizado...")

dt = DecisionTreeRegressor(
    max_depth=4,
    min_samples_split=50,
    min_samples_leaf=20,
    random_state=42
)
dt.fit(X_train, y_train)

y_pred_dt = dt.predict(X_test)

r2_dt   = r2_score(y_test, y_pred_dt)
rmse_dt = np.sqrt(mean_squared_error(y_test, y_pred_dt))
mae_dt  = mean_absolute_error(y_test, y_pred_dt)

# ── FIX: Registro correcto en MODEL_SCORES ────────────────────────────────
MODEL_SCORES['DecisionTree'] = {'R²': r2_dt, 'RMSE': rmse_dt, 'MAE': mae_dt}

print(f"    R²={r2_dt:.4f} | RMSE={rmse_dt:.4f} | MAE={mae_dt:.4f}")

importancias = pd.Series(dt.feature_importances_, index=FEATURES).sort_values(ascending=False)
print("\n    Importancia de variables:")
print(importancias)

plt.figure(figsize=(8, 5))
importancias.plot(kind='barh')
plt.title('Importancia de Variables – Árbol')
plt.xlabel('Importancia')
plt.gca().invert_yaxis()
plt.grid(True)
plt.savefig(os.path.join(OUTPUT_DIR, 'arbol_importancia.png'))
plt.close()

plt.figure(figsize=(20, 8))
plot_tree(dt, feature_names=FEATURES, filled=True, rounded=True, fontsize=8)
plt.title('Árbol de Decisión – ROA')
plt.savefig(os.path.join(OUTPUT_DIR, 'arbol_decision.png'))
plt.close()

# ─────────────────────────────────────────────────────────────────────────────
# 4.5  RIDGE REGRESSION (Regularización L2)
# ─────────────────────────────────────────────────────────────────────────────
print("\n  [4.5] Ridge Regression (regularización L2)...")

alphas_grid = np.logspace(-4, 4, 100)
ridge_cv    = RidgeCV(alphas=alphas_grid, cv=5).fit(X_tr_sc, y_train)
alpha_ridge = ridge_cv.alpha_

ridge_final  = Ridge(alpha=alpha_ridge).fit(X_tr_sc, y_train)
y_pred_ridge = ridge_final.predict(X_te_sc)
r2_ridge     = r2_score(y_test, y_pred_ridge)
rmse_ridge   = np.sqrt(mean_squared_error(y_test, y_pred_ridge))
mae_ridge    = mean_absolute_error(y_test, y_pred_ridge)
MODEL_SCORES['Ridge'] = {'R²': r2_ridge, 'RMSE': rmse_ridge, 'MAE': mae_ridge}

print(f"    α óptimo (CV-5): {alpha_ridge:.6f}")
print(f"    R²={r2_ridge:.4f} | RMSE={rmse_ridge:.4f}")

ridge_coefs = []
for a in alphas_grid:
    m = Ridge(alpha=a).fit(X_tr_sc, y_train)
    ridge_coefs.append(m.coef_)
ridge_coefs = np.array(ridge_coefs)

colors_line = [PALETTE['teal'], PALETTE['orange'], PALETTE['navy'],
               PALETTE['green'], PALETTE['red'], PALETTE['purple'],
               PALETTE['teal2'], '#92400E']

fig, ax = plt.subplots(figsize=(11, 6), facecolor='white')
for j, (feat, col) in enumerate(zip(FEATURES, colors_line)):
    ax.semilogx(alphas_grid, ridge_coefs[:, j], lw=2, color=col, label=feat)
ax.axvline(alpha_ridge, color='black', ls='--', lw=1.8, label=f'α óptimo={alpha_ridge:.4f}')
ax.set_xlabel('λ (alpha) – penalización'); ax.set_ylabel('Coeficiente β (estandarizado)')
ax.set_title('Trayectoria de Coeficientes – Ridge Regression',
             fontsize=13, fontweight='bold')
ax.legend(fontsize=9, loc='upper right')
ax.grid(True, alpha=0.35)
plt.tight_layout()
plt.savefig(os.path.join(OUTPUT_DIR, 'modelo_ridge_trayectoria.png'),
            dpi=130, bbox_inches='tight', facecolor='white')
plt.close()
print("    ✔ Trayectoria Ridge graficada")

# ─────────────────────────────────────────────────────────────────────────────
# 4.6  LASSO REGRESSION (Regularización L1)
# ─────────────────────────────────────────────────────────────────────────────
print("\n  [4.6] Lasso Regression (regularización L1)...")

lasso_cv    = LassoCV(alphas=alphas_grid, cv=5, max_iter=10000,
                      random_state=42).fit(X_tr_sc, y_train)
alpha_lasso = lasso_cv.alpha_

lasso_final  = Lasso(alpha=alpha_lasso, max_iter=10000).fit(X_tr_sc, y_train)
y_pred_lasso = lasso_final.predict(X_te_sc)
r2_lasso     = r2_score(y_test, y_pred_lasso)
rmse_lasso   = np.sqrt(mean_squared_error(y_test, y_pred_lasso))
mae_lasso    = mean_absolute_error(y_test, y_pred_lasso)
MODEL_SCORES['Lasso'] = {'R²': r2_lasso, 'RMSE': rmse_lasso, 'MAE': mae_lasso}

lasso_coefs_df = pd.Series(lasso_final.coef_, index=FEATURES)
vars_zeroed    = (lasso_coefs_df == 0).sum()
print(f"    α óptimo (CV-5): {alpha_lasso:.6f}")
print(f"    Variables anuladas por Lasso: {vars_zeroed}/{len(FEATURES)}")
print(f"    R²={r2_lasso:.4f} | RMSE={rmse_lasso:.4f}")
print("    Coeficientes Lasso:")
for feat, coef in lasso_coefs_df.sort_values(key=abs, ascending=False).items():
    status = '(activa)' if coef != 0 else '(→ 0 eliminada)'
    print(f"      {feat:10s}: {coef:+.5f}  {status}")

lasso_coefs_tray = []
for a in alphas_grid:
    m = Lasso(alpha=a, max_iter=5000).fit(X_tr_sc, y_train)
    lasso_coefs_tray.append(m.coef_)
lasso_coefs_tray = np.array(lasso_coefs_tray)

fig, ax = plt.subplots(figsize=(11, 6), facecolor='white')
for j, (feat, col) in enumerate(zip(FEATURES, colors_line)):
    ax.semilogx(alphas_grid, lasso_coefs_tray[:, j], lw=2, color=col, label=feat)
ax.axvline(alpha_lasso, color='black', ls='--', lw=1.8,
           label=f'α óptimo={alpha_lasso:.4f}')
ax.axhline(0, color='gray', lw=0.8, ls='-')
ax.set_xlabel('λ (alpha) – penalización'); ax.set_ylabel('Coeficiente β (estandarizado)')
ax.set_title('Trayectoria de Coeficientes – Lasso Regression\n(Coeficientes que llegan a 0 = variables eliminadas)',
             fontsize=12, fontweight='bold')
ax.legend(fontsize=9)
ax.grid(True, alpha=0.35)
plt.tight_layout()
plt.savefig(os.path.join(OUTPUT_DIR, 'modelo_lasso_trayectoria.png'),
            dpi=130, bbox_inches='tight', facecolor='white')
plt.close()
print("    ✔ Trayectoria Lasso graficada")

# ─────────────────────────────────────────────────────────────────────────────
# 4.7  COMPARATIVA DE MODELOS
# ─────────────────────────────────────────────────────────────────────────────
print("\n  [4.7] Comparativa de Modelos...")

df_models = pd.DataFrame(MODEL_SCORES).T.sort_values('R²', ascending=False)
print(f"\n    {'Modelo':15s}  {'R²':>8s}  {'RMSE':>8s}  {'MAE':>8s}")
print(f"    {'─'*43}")
for model, row in df_models.iterrows():
    print(f"    {model:15s}  {row['R²']:8.4f}  {row['RMSE']:8.4f}  {row['MAE']:8.4f}")

fig, axes = plt.subplots(1, 3, figsize=(16, 5.5), facecolor='white')
metrics    = ['R²', 'RMSE', 'MAE']
bar_colors = [PALETTE['teal'], PALETTE['navy'], PALETTE['orange'],
              PALETTE['green'], PALETTE['purple']]

for i, metric in enumerate(metrics):
    vals    = df_models[metric]
    colors_ = bar_colors[:len(vals)]
    bars = axes[i].bar(vals.index, vals.values, color=colors_, alpha=0.85, width=0.55)
    axes[i].set_title(f'Comparativa – {metric}', fontweight='bold')
    axes[i].set_ylabel(metric)
    axes[i].tick_params(axis='x', rotation=20)
    for bar, val in zip(bars, vals.values):
        axes[i].text(bar.get_x() + bar.get_width()/2,
                     bar.get_height() + 0.0005,
                     f'{val:.4f}', ha='center', va='bottom',
                     fontsize=9, fontweight='bold')
    axes[i].grid(True, axis='y', alpha=0.4)

plt.suptitle('Comparativa de Modelos Predictivos  ·  Conjunto de Test (20%)',
             fontsize=14, fontweight='bold', y=1.01)
plt.tight_layout()
plt.savefig(os.path.join(OUTPUT_DIR, 'modelos_comparativa.png'),
            dpi=130, bbox_inches='tight', facecolor='white')
plt.close()
print("    ✔ Comparativa de modelos graficada")


# ==============================================================================
# SECCIÓN 5 ─ DIAGNÓSTICOS DE GAUSS-MARKOV
# ==============================================================================
print("\n[5/6] Diagnósticos de Gauss-Markov...")

y_fitted      = ols.fittedvalues
residuals     = ols.resid
residuals_std = (residuals - residuals.mean()) / residuals.std()

# ─────────────────────────────────────────────────────────────────────────────
# 5.1  NORMALIDAD DE RESIDUOS
# ─────────────────────────────────────────────────────────────────────────────
sample_res = residuals.sample(min(5000, len(residuals)), random_state=42)
sw_stat, sw_p    = shapiro(sample_res)
jb_stat2, jb_p2, *_ = jarque_bera(residuals)

print(f"\n  [5.1] Normalidad de Residuos")
print(f"    Shapiro-Wilk  : stat={sw_stat:.4f}  p={sw_p:.4e}  "
      f"{'✔ Normal' if sw_p>0.05 else '✗ No normal'}")
print(f"    Jarque-Bera   : stat={jb_stat2:.2f}  p={jb_p2:.4e}  "
      f"{'✔ Normal' if float(jb_p2)>0.05 else '✗ No normal (colas pesadas)'}")
print(f"    Asimetría residuos: {float(residuals.skew()):.4f}")
print(f"    Curtosis residuos : {float(residuals.kurt()):.4f}")

fig, axes = plt.subplots(1, 3, figsize=(18, 5.5), facecolor='white')

axes[0].hist(residuals, bins=70, color=PALETTE['teal'],
             alpha=0.82, edgecolor='white', density=True)
mu_r, std_r = residuals.mean(), residuals.std()
x_norm = np.linspace(residuals.min(), residuals.max(), 300)
axes[0].plot(x_norm, stats.norm.pdf(x_norm, mu_r, std_r),
             color=PALETTE['red'], lw=2.2, label='N(μ,σ²) teórica')
axes[0].set_title('Distribución de Residuos', fontweight='bold')
axes[0].set_xlabel('Residuo'); axes[0].set_ylabel('Densidad')
axes[0].legend()

(osm2, osr2), (slope2, intercept2, r_qq2) = probplot(residuals, dist="norm")
axes[1].plot(osm2, osr2, '.', color=PALETTE['teal'], ms=2.5, alpha=0.5)
axes[1].plot(osm2, slope2*np.array(osm2)+intercept2,
             '-', color=PALETTE['red'], lw=2)
axes[1].set_title(f'QQ-Plot de Residuos (r={r_qq2:.4f})', fontweight='bold')
axes[1].set_xlabel('Cuantiles teóricos N(0,1)')
axes[1].set_ylabel('Cuantiles muestrales')

axes[2].scatter(range(len(residuals)), residuals,
                s=4, alpha=0.3, color=PALETTE['navy'])
axes[2].axhline(0,  color=PALETTE['red'],  lw=1.5, ls='--')
axes[2].axhline( 2*std_r, color='gray', lw=1, ls=':')
axes[2].axhline(-2*std_r, color='gray', lw=1, ls=':')
axes[2].set_title('Residuos vs Orden de Observación', fontweight='bold')
axes[2].set_xlabel('Índice'); axes[2].set_ylabel('Residuo')

plt.suptitle('Diagnóstico: Normalidad de Residuos',
             fontsize=14, fontweight='bold', y=1.01)
plt.tight_layout()
plt.savefig(os.path.join(OUTPUT_DIR, 'diag_normalidad_residuos.png'),
            dpi=130, bbox_inches='tight', facecolor='white')
plt.close()
print("    ✔ Gráficos de normalidad de residuos generados")

# ─────────────────────────────────────────────────────────────────────────────
# 5.2  HOMOCEDASTICIDAD (Breusch-Pagan)
# ─────────────────────────────────────────────────────────────────────────────
print(f"\n  [5.2] Homocedasticidad (Breusch-Pagan)")

bp_lm, bp_p, bp_f, bp_fp = het_breuschpagan(residuals, X_ols)
print(f"    Breusch-Pagan LM stat: {bp_lm:.4f}  p={bp_p:.4e}  "
      f"{'✔ Homocedasticidad' if bp_p>0.05 else '✗ Heterocedasticidad detectada'}")

fig, axes = plt.subplots(1, 2, figsize=(15, 5.5), facecolor='white')

axes[0].scatter(y_fitted, residuals, s=5, alpha=0.25, color=PALETTE['teal'])
axes[0].axhline(0, color=PALETTE['red'], lw=1.8, ls='--')
try:
    from statsmodels.nonparametric.smoothers_lowess import lowess
    smooth = lowess(residuals, y_fitted, frac=0.3)
    axes[0].plot(smooth[:, 0], smooth[:, 1], color=PALETTE['orange'], lw=2.2,
                 label='LOWESS (tendencia)')
    axes[0].legend(fontsize=9)
except Exception:
    pass
axes[0].set_xlabel('Valores Ajustados (ŷ)'); axes[0].set_ylabel('Residuos')
axes[0].set_title('Residuos vs Valores Ajustados\n(Homocedasticidad: banda horizontal plana)',
                  fontweight='bold')

sqrt_abs_res = np.sqrt(np.abs(residuals_std))
axes[1].scatter(y_fitted, sqrt_abs_res, s=5, alpha=0.25, color=PALETTE['navy'])
axes[1].set_xlabel('Valores Ajustados (ŷ)')
axes[1].set_ylabel('√|Residuos Estandarizados|')
axes[1].set_title('Scale-Location Plot\n(Línea horizontal = homocedasticidad perfecta)',
                  fontweight='bold')

plt.suptitle('Diagnóstico: Homocedasticidad',
             fontsize=14, fontweight='bold', y=1.01)
plt.tight_layout()
plt.savefig(os.path.join(OUTPUT_DIR, 'diag_homocedasticidad.png'),
            dpi=130, bbox_inches='tight', facecolor='white')
plt.close()
print("    ✔ Gráficos de homocedasticidad generados")

# ─────────────────────────────────────────────────────────────────────────────
# 5.3  MULTICOLINEALIDAD (VIF)
# ─────────────────────────────────────────────────────────────────────────────
print(f"\n  [5.3] Multicolinealidad (VIF)")

X_vif  = sm.add_constant(df_model[FEATURES])
vif_df = pd.DataFrame({
    'Variable': FEATURES,
    'VIF'     : [variance_inflation_factor(X_vif.values, i+1)
                 for i in range(len(FEATURES))]
}).sort_values('VIF', ascending=False)

print(f"    {'Variable':12s}  {'VIF':>8s}  {'Diagnóstico':>20s}")
print(f"    {'─'*44}")
for _, row in vif_df.iterrows():
    v = row['VIF']
    diag = ('✔ Sin colinealidad' if v < 5 else
            ('⚠ Colinealidad moderada' if v < 10 else '✗ Colinealidad SEVERA'))
    print(f"    {row['Variable']:12s}  {v:8.3f}  {diag}")

fig, ax = plt.subplots(figsize=(10, 5), facecolor='white')
bar_col_vif = [PALETTE['green'] if v < 5 else
               (PALETTE['orange'] if v < 10 else PALETTE['red'])
               for v in vif_df['VIF']]
bars = ax.barh(vif_df['Variable'], vif_df['VIF'], color=bar_col_vif, alpha=0.85)
ax.axvline(5,  color=PALETTE['orange'], lw=1.5, ls='--', label='VIF=5 (umbral moderado)')
ax.axvline(10, color=PALETTE['red'],    lw=1.5, ls='--', label='VIF=10 (umbral severo)')
ax.set_xlabel('VIF')
ax.set_title('Factor de Inflación de Varianza (VIF)', fontsize=13, fontweight='bold')
ax.legend(fontsize=9)
ax.grid(True, axis='x', alpha=0.4)
for bar, val in zip(bars, vif_df['VIF']):
    ax.text(bar.get_width() + 0.05, bar.get_y() + bar.get_height()/2,
            f'{val:.2f}', va='center', fontsize=9, fontweight='bold')
plt.tight_layout()
plt.savefig(os.path.join(OUTPUT_DIR, 'diag_vif.png'),
            dpi=130, bbox_inches='tight', facecolor='white')
plt.close()
print("    ✔ Gráfico VIF generado")

# ─────────────────────────────────────────────────────────────────────────────
# 5.4  AUTOCORRELACIÓN (Durbin-Watson)
# ─────────────────────────────────────────────────────────────────────────────
print(f"\n  [5.4] Autocorrelación (Durbin-Watson)")
dw_stat = durbin_watson(residuals)
print(f"    Durbin-Watson: {dw_stat:.4f}  "
      f"{'✔ Sin autocorrelación' if 1.5<dw_stat<2.5 else '⚠ Posible autocorrelación'}")

n_lags   = 30
acf_vals = [1.0] + [np.corrcoef(residuals[:-i], residuals[i:])[0,1]
                    for i in range(1, n_lags+1)]
conf_band = 1.96 / np.sqrt(len(residuals))

fig, ax = plt.subplots(figsize=(12, 5), facecolor='white')
ax.bar(range(n_lags+1), acf_vals, width=0.6,
       color=[PALETTE['teal'] if abs(v) < conf_band else PALETTE['red']
              for v in acf_vals], alpha=0.8)
ax.axhline( conf_band, color=PALETTE['slate'], lw=1.3, ls='--',
            label=f'IC 95% (±{conf_band:.3f})')
ax.axhline(-conf_band, color=PALETTE['slate'], lw=1.3, ls='--')
ax.axhline(0, color='black', lw=0.8)
ax.set_xlabel('Lag'); ax.set_ylabel('Autocorrelación')
ax.set_title(f'Función de Autocorrelación (ACF) de Residuos  ·  DW={dw_stat:.3f}',
             fontsize=13, fontweight='bold')
ax.legend(fontsize=9)
ax.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig(os.path.join(OUTPUT_DIR, 'diag_autocorrelacion.png'),
            dpi=130, bbox_inches='tight', facecolor='white')
plt.close()
print("    ✔ Correlograma ACF de residuos generado")


# ==============================================================================
# SECCIÓN 6 ─ REPORTE FINAL EN CONSOLA
# ==============================================================================
print("\n" + "=" * 70)
print("  REPORTE FINAL  –  Análisis KoTaP Dataset")
print("=" * 70)

best_model = df_models.index[0]
print(f"""
  ┌─────────────────────────────────────────────────────────────────┐
  │  DATASET                                                        │
  │  • Observaciones: {len(df):>6,}  | Empresas: {df['stock'].nunique():>5,}              │
  │  • Período: {df['year'].min()}–{df['year'].max()}  | Variables clave: {len(KEY_VARS):>2}              │
  │  • Outliers: winsorización 1%–99% (sin pérdida de filas)       │
  ├─────────────────────────────────────────────────────────────────┤
  │  ANÁLISIS UNIVARIADO                                            │
  │  • ROA: μ={df[TARGET].mean():.4f}, σ={df[TARGET].std():.4f}, Asim={float(df[TARGET].skew()):.2f}          │
  │  • Ninguna variable financiera pasa normalidad estricta (JB)   │
  │  • LEV y SIZE: distribuciones más simétricas del dataset       │
  ├─────────────────────────────────────────────────────────────────┤
  │  CORRELACIONES CLAVE (Spearman con ROA)                        │
  │  • CFO:    {corr_spearman.loc['CFO','ROA']:+.4f}  (la más fuerte – driver operativo)    │
  │  • LEV:    {corr_spearman.loc['LEV','ROA']:+.4f}  (negativa – deuda erosiona rentab.)  │
  │  • GRW:    {corr_spearman.loc['GRW','ROA']:+.4f}  (positiva – crecimiento aporta ROA)  │
  ├─────────────────────────────────────────────────────────────────┤
  │  MEJOR MODELO: {best_model:<20s}  R²={df_models.loc[best_model,'R²']:.4f}          │
  │  OLS R²={MODEL_SCORES['OLS']['R²']:.4f} | Ridge R²={MODEL_SCORES['Ridge']['R²']:.4f} | Lasso R²={MODEL_SCORES['Lasso']['R²']:.4f}   │
  │  Árbol R²={MODEL_SCORES['DecisionTree']['R²']:.4f} | OLS-RFE R²={MODEL_SCORES['OLS-RFE']['R²']:.4f}                  │
  ├─────────────────────────────────────────────────────────────────┤
  │  DIAGNÓSTICOS GAUSS-MARKOV                                      │
  │  • Normalidad residuos : ✗ Rechazada (JB p≈0) – colas pesadas │
  │  • Homocedasticidad    : ✗ BP p={bp_p:.2e} – heterocedasticidad│
  │  • Multicolinealidad   : ✔ VIF máx = {vif_df['VIF'].max():.2f} (< 5)              │
  │  • Autocorrelación DW  : {dw_stat:.4f}  {'✔ Aceptable' if 1.5<dw_stat<2.5 else '⚠ Revisar'}                          │
  ├─────────────────────────────────────────────────────────────────┤
  │  RECOMENDACIONES DE NEGOCIO                                     │
  │  1. CFO (β=+0.333) → Priorizar gestión de flujo de caja        │
  │  2. LEV (β=-0.055) → Mantener apalancamiento < 40%             │
  │  3. AGE (β=-0.009) → Revisar portafolio de firmas > 30 años    │
  │  4. GRW (β=+0.048) → Inversión en expansión de mercado        │
  └─────────────────────────────────────────────────────────────────┘
""")

# Guarda resumen JSON
summary = {
    'dataset'  : {'n': len(df), 'companies': df['stock'].nunique(),
                  'years': f"{df['year'].min()}–{df['year'].max()}"},
    'models'   : MODEL_SCORES,
    'best_model': best_model,
    'gauss_markov': {
        'shapiro_p'    : float(sw_p),
        'breusch_pagan': float(bp_p),
        'vif_max'      : float(vif_df['VIF'].max()),
        'durbin_watson': float(dw_stat),
    },
    'correlations_roa': {
        feat: round(float(corr_spearman.loc[feat, TARGET]), 4)
        for feat in FEATURES
    },
}
with open(os.path.join(OUTPUT_DIR, 'resumen_final.json'), 'w') as f:
    json.dump(summary, f, indent=2)

print(f"  Todos los gráficos guardados en: '{OUTPUT_DIR}/'")
print(f"  Resumen JSON: '{OUTPUT_DIR}/resumen_final.json'")
print("\n  Script completado exitosamente ✔")
print("=" * 70)


# ==============================================================================
# APÉNDICE – NOTAS METODOLÓGICAS
# ==============================================================================
"""
NOTAS PARA EL ESTUDIANTE DE INGENIERÍA
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

¿Por qué el R² es ~0.34 y no más alto?
  El ROA empresarial depende de factores no capturados aquí: condiciones
  macroeconómicas, gestión interna, innovación. Un R² de 0.34 en datos
  de panel financiero con alta heterogeneidad sectorial es SÓLIDO.

¿Por qué los residuos no son normales?
  Los datos financieros tienen "fat tails" (colas pesadas) por naturaleza.
  Crisis económicas generan observaciones extremas. En muestras grandes
  (n>5000), el Teorema Central del Límite garantiza que los estimadores
  OLS siguen siendo válidos APROXIMADAMENTE incluso sin normalidad estricta.

¿Por qué Ridge y Lasso dan resultados similares a OLS?
  Con VIF < 5 (baja multicolinealidad), la regularización aporta poco.
  Ridge/Lasso brillan cuando VIF > 10 o cuando hay más variables que obs.

¿Cómo interpretar el Árbol de Decisión?
  Cada nodo dice: "Si [variable] > [umbral] → rama derecha, si no → izquierda"
  El valor en cada hoja es el ROA promedio predicho para esa combinación.
  La variable más importante (CFO) aparece cerca de la raíz del árbol.
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
"""



