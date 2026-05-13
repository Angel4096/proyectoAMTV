import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error, r2_score

# ==========================================
# 1. CARGA DE DATOS
# ==========================================
filepath = r"C:\Users\norba\Downloads\KoTaP_Dataset.csv"

# Manejo de múltiples encodings por si el CSV tiene caracteres especiales
try:
    df_raw = pd.read_csv(filepath, encoding='utf-8')
except UnicodeDecodeError:
    try:
        df_raw = pd.read_csv(filepath, encoding='cp949') # Encoding común para datos coreanos
    except:
        df_raw = pd.read_csv(filepath, encoding='latin1')

# ==========================================
# 2. LIMPIEZA Y PREPARACIÓN (Winsorización)
# ==========================================
KEY_VARS = ["SIZE", "LEV", "ROA", "ROE", "CFO", "GRW", "CUR", "INVREC", "MB", "TQ", "PPE", "AGE"]
TARGET = "ROA"
FEATURES = ["SIZE", "LEV", "CFO", "GRW", "CUR", "INVREC", "PPE", "AGE"]

df = df_raw.drop_duplicates().copy()

# Forzar conversión a numérico para evitar errores
for col in KEY_VARS:
    if col in df.columns:
        df[col] = pd.to_numeric(df[col], errors='coerce')

# Función de Winsorización (1% y 99%) para lidiar con outliers extremos
def winsorize_series(s, lower=0.01, upper=0.99):
    q_low = s.quantile(lower)
    q_up = s.quantile(upper)
    return s.clip(lower=q_low, upper=q_up)

for col in KEY_VARS:
    if col in df.columns:
        df[col] = winsorize_series(df[col])

# Seleccionar solo las columnas necesarias y eliminar nulos
model_cols = [TARGET] + FEATURES
if 'stock' in df.columns: model_cols.append('stock')
if 'year' in df.columns: model_cols.append('year')

df_model = df[model_cols].dropna()

# ==========================================
# 3. SPLIT DE DATOS Y ENTRENAMIENTO
# ==========================================
# Si existe la columna 'year', usamos el último año como test (simulación real)
if 'year' in df_model.columns:
    max_year = df_model['year'].max()
    train_df = df_model[df_model['year'] < max_year]
    test_df = df_model[df_model['year'] == max_year]
else:
    # Si no hay año, hacemos un split tradicional 80/20
    train_df, test_df = train_test_split(df_model, test_size=0.2, random_state=42)

X_train = train_df[FEATURES]
y_train = train_df[TARGET]
X_test = test_df[FEATURES]
y_test = test_df[TARGET]

# Entrenar el Random Forest Regressor
print("Entrenando modelo Random Forest...")
rf = RandomForestRegressor(n_estimators=100, max_depth=10, random_state=42, n_jobs=-1)
rf.fit(X_train, y_train)

# Predicciones y métricas
y_pred = rf.predict(X_test)
r2 = r2_score(y_test, y_pred)
rmse = np.sqrt(mean_squared_error(y_test, y_pred))

print(f"R² del modelo: {r2:.4f}")
print(f"RMSE del modelo: {rmse:.4f}")

# ==========================================
# 4. CONSTRUCCIÓN DE LA CARTERA
# ==========================================
if 'stock' in test_df.columns:
    stocks = test_df['stock']
else:
    stocks = test_df.index

# Crear dataframe con los resultados
portfolio = pd.DataFrame({'Stock': stocks, 'ROA_Real': y_test, 'ROA_Predicho': y_pred})
# Ordenar de mayor a menor ROA predicho para seleccionar las mejores
portfolio = portfolio.sort_values(by='ROA_Predicho', ascending=False)
top_20 = portfolio.head(20)

print("\nTop 10 Empresas Seleccionadas para la Cartera (por ROA predicho):")
print(top_20.head(10).to_string(index=False))

# ==========================================
# 5. VISUALIZACIÓN DE RESULTADOS
# ==========================================
sns.set_theme(style="whitegrid", rc={"axes.facecolor": "#F8FAFC"})
fig, axes = plt.subplots(1, 2, figsize=(15, 6))

# Panel 1: Importancia de las variables (Feature Importance)
importances = pd.DataFrame({'Variable': FEATURES, 'Importancia': rf.feature_importances_})
importances = importances.sort_values(by='Importancia', ascending=False)
sns.barplot(x='Importancia', y='Variable', data=importances, ax=axes[0], palette="viridis")
axes[0].set_title("Importancia de Variables (Random Forest)", fontweight='bold', color="#0F172A")
axes[0].set_xlabel("Importancia Relativa")
axes[0].set_ylabel("")

# Panel 2: ROA Real vs Predicho
sns.scatterplot(x='ROA_Real', y='ROA_Predicho', data=portfolio, ax=axes[1], alpha=0.3, color="#475569", label="Universo (Test)")
sns.scatterplot(x='ROA_Real', y='ROA_Predicho', data=top_20, ax=axes[1], color="#0D9488", s=100, edgecolor="black", label="Top 20 Cartera")
# Línea de predicción perfecta
axes[1].plot([y_test.min(), y_test.max()], [y_test.min(), y_test.max()], color="#EF4444", linestyle="--", linewidth=2)
axes[1].set_title(f"ROA Real vs Predicho (R² = {r2:.3f})", fontweight='bold', color="#0F172A")
axes[1].set_xlabel("ROA Observado")
axes[1].set_ylabel("ROA Predicho")
axes[1].legend()

plt.tight_layout()
plt.savefig("portfolio_model.png", dpi=300, bbox_inches='tight')
plt.show()