# ============================================================
#  Setup_proyectoAMTV.ps1
#  Crea la estructura de carpetas en C:\proyectoAMTV
#  y copia todos los archivos del análisis KoTaP
#  Ejecutar desde PowerShell como Administrador
# ============================================================

$origen  = "C:\Users\norba\OneDrive\NUESTRO HOGAR\1. ANGEL SANTIAGO\AMMV\Proyecto Multivariado 2"
$destino = "C:\proyectoAMTV"

Write-Host ""
Write-Host "=====================================================" -ForegroundColor Cyan
Write-Host "   Configurando proyecto proyectoAMTV en C:\"         -ForegroundColor Cyan
Write-Host "=====================================================" -ForegroundColor Cyan
Write-Host ""

# ----------------------------------------------------------
# 1. Crear estructura de carpetas
# ----------------------------------------------------------
$carpetas = @(
    "$destino\01_codigo\R",
    "$destino\01_codigo\Python",
    "$destino\02_visualizaciones\descriptivos",
    "$destino\02_visualizaciones\correlacion",
    "$destino\02_visualizaciones\pca",
    "$destino\02_visualizaciones\clustering",
    "$destino\02_visualizaciones\regresion",
    "$destino\02_visualizaciones\treemaps",
    "$destino\03_documentos",
    "$destino\04_presentaciones",
    "$destino\05_resultados",
    "$destino\06_shiny_app"
)

foreach ($carpeta in $carpetas) {
    New-Item -ItemType Directory -Force -Path $carpeta | Out-Null
    Write-Host "  [OK] Carpeta creada: $carpeta" -ForegroundColor Green
}

Write-Host ""
Write-Host "  Copiando archivos..." -ForegroundColor Yellow
Write-Host ""

# ----------------------------------------------------------
# 2. Scripts R
# ----------------------------------------------------------
$scriptsR = @(
    "Dataset Nuevo.R",
    "KoTaP_Analisis_Financiero.R",
    "KoTaP_Analisis_Financiero 2 de mayo.R",
    "Kotap analisis 4 de mayo.R",
    "Kotap analisis 6 de mayo.R",
    "Kotap analisis 9 de mayo.R",
    "dataset nuevo diapos.R"
)
foreach ($f in $scriptsR) {
    $src = Join-Path $origen $f
    if (Test-Path $src) {
        Copy-Item $src "$destino\01_codigo\R\" -Force
        Write-Host "  [R]  $f" -ForegroundColor White
    }
}

# ----------------------------------------------------------
# 3. Scripts Python
# ----------------------------------------------------------
$scriptsPy = @(
    "Codigo nuevo proyecto.py",
    "KOSPI KOSDAQ retornos prediccion.py"
)
foreach ($f in $scriptsPy) {
    $src = Join-Path $origen $f
    if (Test-Path $src) {
        Copy-Item $src "$destino\01_codigo\Python\" -Force
        Write-Host "  [Py] $f" -ForegroundColor White
    }
}

# ----------------------------------------------------------
# 4. Visualizaciones - Descriptivos
# ----------------------------------------------------------
$descPng = @(
    "desc_AGE.png","desc_CFO.png","desc_CUR.png","desc_GRW.png",
    "desc_INVREC.png","desc_LEV.png","desc_MB.png","desc_PPE.png",
    "desc_RESUMEN.png","desc_ROA.png","desc_ROE.png","desc_SIZE.png","desc_TQ.png"
)
foreach ($f in $descPng) {
    $src = Join-Path $origen $f
    if (Test-Path $src) {
        Copy-Item $src "$destino\02_visualizaciones\descriptivos\" -Force
        Write-Host "  [IMG] $f" -ForegroundColor White
    }
}

# Correlación
Copy-Item "$origen\corr_spearman.png" "$destino\02_visualizaciones\correlacion\" -Force -ErrorAction SilentlyContinue
Write-Host "  [IMG] corr_spearman.png" -ForegroundColor White

# PCA
$pcaPng = @("pca_biplot.png","pca_scree.png")
foreach ($f in $pcaPng) {
    $src = Join-Path $origen $f
    if (Test-Path $src) {
        Copy-Item $src "$destino\02_visualizaciones\pca\" -Force
        Write-Host "  [IMG] $f" -ForegroundColor White
    }
}

# Clustering
$kmPng = @("kmeans_2d.png","kmeans_codo.png","kmeans_perfiles.png")
foreach ($f in $kmPng) {
    $src = Join-Path $origen $f
    if (Test-Path $src) {
        Copy-Item $src "$destino\02_visualizaciones\clustering\" -Force
        Write-Host "  [IMG] $f" -ForegroundColor White
    }
}

# Regresión
$olsPng = @("ols_regresion.png","ols_residuos.png")
foreach ($f in $olsPng) {
    $src = Join-Path $origen $f
    if (Test-Path $src) {
        Copy-Item $src "$destino\02_visualizaciones\regresion\" -Force
        Write-Host "  [IMG] $f" -ForegroundColor White
    }
}

# Treemaps
$treePng = @(
    "treemap_real_escala_2020.png","treemap_real_escala_2021.png",
    "treemap_real_escala_2022.png","treemap_real_escala_2023.png",
    "treemap_real_escala_2024.png"
)
foreach ($f in $treePng) {
    $src = Join-Path $origen $f
    if (Test-Path $src) {
        Copy-Item $src "$destino\02_visualizaciones\treemaps\" -Force
        Write-Host "  [IMG] $f" -ForegroundColor White
    }
}

# ----------------------------------------------------------
# 5. Documentos
# ----------------------------------------------------------
$docs = @(
    "Explicacion variables y graficos.docx",
    "Explicacion variables, regresion y verificacion de supuestos.docx",
    "KoTaP_Analisis_Experto_Completo.docx",
    [char]0x1F3A4 + " Guion Profundizado.docx"   # emoji en nombre
)
# Copiar todos los .docx directamente
Get-ChildItem "$origen\*.docx" | ForEach-Object {
    Copy-Item $_.FullName "$destino\03_documentos\" -Force
    Write-Host "  [DOC] $($_.Name)" -ForegroundColor White
}

# ----------------------------------------------------------
# 6. Presentaciones
# ----------------------------------------------------------
Get-ChildItem "$origen\*.pptx" | ForEach-Object {
    Copy-Item $_.FullName "$destino\04_presentaciones\" -Force
    Write-Host "  [PPT] $($_.Name)" -ForegroundColor White
}

# ----------------------------------------------------------
# 7. Resultados (outputs del análisis)
# ----------------------------------------------------------
$resultados = @(
    "coeficientes_gamma.txt",
    "resumen_final.json",
    "presenta.html",
    "presenta.txt",
    "explicacion multivariado.html.txt"
)
foreach ($f in $resultados) {
    $src = Join-Path $origen $f
    if (Test-Path $src) {
        Copy-Item $src "$destino\05_resultados\" -Force
        Write-Host "  [OUT] $f" -ForegroundColor White
    }
}

# ----------------------------------------------------------
# 8. Shiny App
# ----------------------------------------------------------
$shinyDst = "$destino\06_shiny_app"
Copy-Item "$origen\ShinytrabajoAMMV\app.R" $shinyDst -Force -ErrorAction SilentlyContinue
Write-Host "  [APP] app.R (Shiny)" -ForegroundColor White

# ----------------------------------------------------------
# Resumen final
# ----------------------------------------------------------
Write-Host ""
Write-Host "=====================================================" -ForegroundColor Cyan
Write-Host "   Estructura creada en C:\proyectoAMTV"              -ForegroundColor Cyan
Write-Host "=====================================================" -ForegroundColor Cyan
Write-Host ""

Get-ChildItem $destino -Recurse -File | Measure-Object | ForEach-Object {
    Write-Host "  Total de archivos copiados: $($_.Count)" -ForegroundColor Green
}

Write-Host ""
Write-Host "  Estructura de carpetas:" -ForegroundColor Yellow
Get-ChildItem $destino -Directory -Recurse | ForEach-Object {
    $indent = "  " * ($_.FullName.Split("\").Count - $destino.Split("\").Count)
    Write-Host "$indent  /$($_.Name)" -ForegroundColor Gray
}

Write-Host ""
Write-Host "  Listo. Abre C:\proyectoAMTV en el Explorador." -ForegroundColor Green
Write-Host ""
