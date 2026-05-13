source("R/helpers_pca_ols.R")

synthetic_panel <- function(n_firms = 20, years = 2019:2023) {
  panel <- expand.grid(
    stock = sprintf("F%02d", seq_len(n_firms)),
    year = years,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  panel <- panel[order(panel$stock, panel$year), ]
  firm_id <- as.integer(sub("F", "", panel$stock))
  year_id <- panel$year - min(panel$year) + 1

  panel$ROA <- firm_id / 100 + year_id / 10
  panel$ROE <- panel$ROA * 1.5
  panel$SIZE <- log(firm_id + 10)
  panel$LEV <- firm_id / 50 + year_id / 100
  panel$CUR <- 1 + firm_id / 30 - year_id / 100
  panel$GRW <- sin(firm_id / 3) + year_id / 20
  panel$CFO <- cos(firm_id / 4) + year_id / 30
  panel$PPE <- firm_id / 40 + year_id / 50
  panel$x_outlier <- firm_id / 10
  panel$x_outlier[panel$stock == "F01" & panel$year == min(years)] <- 999
  panel
}

testthat::test_that("T1 target_ROA is created before IQR removal and temporal split stays causal", {
  df_full <- synthetic_panel()

  df_with_target <- crear_target_roa(df_full)
  next_roa <- df_with_target$ROA[df_with_target$stock == "F01" & df_with_target$year == 2020]
  actual_target <- df_with_target$target_ROA[df_with_target$stock == "F01" & df_with_target$year == 2019]

  testthat::expect_equal(nrow(df_with_target), nrow(df_full))
  testthat::expect_equal(actual_target, next_roa)
  testthat::expect_true(is.na(df_with_target$target_ROA[df_with_target$stock == "F01" & df_with_target$year == 2023]))

  df_after_filter <- filtrar_outliers_iqr_predictores(df_with_target, vars = "x_outlier")
  testthat::expect_gt(nrow(df_with_target), nrow(df_after_filter))

  split <- dividir_temporalmente(df_after_filter)
  testthat::expect_true(all(split$train$year <= 2022))
  testthat::expect_true(all(split$test$year == 2023))
  testthat::expect_equal(nrow(split$future), 0L)

  testthat::expect_error(
    filtrar_outliers_iqr_predictores(df_full, vars = "x_outlier"),
    "target_ROA must exist before IQR filtering"
  )
})

testthat::test_that("T2 PCA excludes target_ROA, ROA, and ROE, fits on train, and projects test from train rotation", {
  df_train <- crear_target_roa(synthetic_panel(years = 2018:2022))
  df_test <- synthetic_panel(n_firms = 5, years = 2023)
  df_test$target_ROA <- seq_len(nrow(df_test)) / 100
  vars_candidatas <- c("ROA", "ROE", "target_ROA", "SIZE", "LEV", "CUR", "GRW", "CFO", "PPE")

  x_train <- preparar_matriz_pca(df_train, vars_candidatas = vars_candidatas)
  testthat::expect_false(any(c("ROA", "ROE", "target_ROA") %in% colnames(x_train)))

  pca_train <- ajustar_pca_train(x_train)
  testthat::expect_false(any(c("ROA", "ROE", "target_ROA") %in% colnames(pca_train$x)))
  testthat::expect_equal(nrow(pca_train$rotation), ncol(x_train))

  scores_test <- proyectar_pca(pca_train, df_test)
  x_test <- as.matrix(df_test[, pca_train$vars, drop = FALSE])
  manual_scores <- scale(x_test, center = pca_train$center, scale = pca_train$scale) %*% pca_train$rotation[, seq_len(pca_train$k_kaiser), drop = FALSE]
  testthat::expect_equal(unname(as.matrix(scores_test)), unname(manual_scores), tolerance = 1e-10)

  testthat::expect_error(
    ajustar_pca_train(df_train, vars = c("SIZE", "ROE")),
    "vars_pca must not contain ROA, ROE, or target_ROA"
  )
  df_leaky_train <- x_train
  df_leaky_train$year <- 2023
  testthat::expect_error(
    ajustar_pca_train(df_leaky_train, vars = colnames(x_train)),
    "df_train contains rows with year > 2022"
  )

  df_alignment <- crear_target_roa(synthetic_panel(n_firms = 8, years = 2018:2022))
  rownames(df_alignment) <- paste0("fila_", seq_len(nrow(df_alignment)) * 10)
  df_alignment$CUR[7] <- NA_real_
  split_alignment <- dividir_temporalmente(df_alignment)
  prep_alignment <- preparar_matriz_pca(
    split_alignment$train,
    vars_candidatas = vars_candidatas,
    return_source_rows = TRUE
  )
  aligned_train <- split_alignment$train[prep_alignment$source_rows, , drop = FALSE]
  pca_alignment <- ajustar_pca_train(
    cbind(year = aligned_train$year, prep_alignment$x),
    vars = names(prep_alignment$x)
  )
  scores_alignment <- proyectar_pca(pca_alignment, prep_alignment$x)
  model_alignment <- ajustar_ols_pc(scores_alignment, aligned_train$target_ROA)

  testthat::expect_equal(rownames(prep_alignment$x), prep_alignment$source_rownames)
  testthat::expect_equal(aligned_train$target_ROA, split_alignment$train$target_ROA[prep_alignment$source_rows])
  testthat::expect_false(any(is.na(aligned_train$target_ROA)))
  testthat::expect_equal(stats::nobs(model_alignment), nrow(aligned_train))
})

testthat::test_that("T3 OLS model terms are PC scores only and zero-component input is rejected", {
  scores <- data.frame(
    PC1 = seq(-2, 2, length.out = 30),
    PC2 = rep(c(-1, 0, 1), length.out = 30),
    PC3 = sin(seq_len(30))
  )
  target <- 0.2 + 0.5 * scores$PC1 - 0.1 * scores$PC2 + 0.01 * seq_len(30)

  model <- ajustar_ols_pc(scores, target)
  testthat::expect_true(all(grepl("^(\\(Intercept\\))$|^PC\\d+$", names(stats::coef(model)))))
  testthat::expect_false(any(c("SIZE", "LEV", "ROA", "ROE", "target_ROA") %in% names(stats::coef(model))))

  testthat::expect_error(
    ajustar_ols_pc(data.frame(row.names = seq_along(target)), target),
    "No components retained under Kaiser rule"
  )
})

testthat::test_that("T4 diagnostics flag Cook's D outliers and return zero-row tables when nothing qualifies", {
  pc1 <- c(seq(-2, 2, length.out = 30), 12)
  pc2 <- c(rep(c(-1, 0, 1), length.out = 30), 10)
  y <- c(1 + 2 * pc1[-31] + 0.05 * sin(seq_len(30)), -30)
  model_outlier <- ajustar_ols_pc(data.frame(PC1 = pc1, PC2 = pc2), y)

  diagnostics <- diagnosticar_ols_pc(model_outlier)
  testthat::expect_s3_class(diagnostics$cook, "data.frame")
  testthat::expect_gte(nrow(diagnostics$cook), 1)
  testthat::expect_true(all(diagnostics$cook$cook_d > diagnostics$umbrales$cook))
  if (nrow(diagnostics$residuos_atipicos) > 0) {
    testthat::expect_true(all(abs(diagnostics$residuos_atipicos$residuo_estandar) > diagnostics$umbrales$residuo_advertencia))
  }
  if (nrow(diagnostics$leverage) > 0) {
    testthat::expect_true(all(diagnostics$leverage$leverage > diagnostics$umbrales$leverage))
  }

  pc_clean <- seq(-2, 2, length.out = 20)
  model_clean <- ajustar_ols_pc(data.frame(PC1 = pc_clean), 1 + 3 * pc_clean)
  clean_diagnostics <- diagnosticar_ols_pc(model_clean)
  testthat::expect_s3_class(clean_diagnostics$residuos_atipicos, "data.frame")
  testthat::expect_s3_class(clean_diagnostics$leverage, "data.frame")
  testthat::expect_s3_class(clean_diagnostics$cook, "data.frame")
  testthat::expect_equal(nrow(clean_diagnostics$residuos_atipicos), 0L)
  testthat::expect_equal(nrow(clean_diagnostics$leverage), 0L)
  testthat::expect_equal(nrow(clean_diagnostics$cook), 0L)
})
