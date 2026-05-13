# Helpers PCA → OLS para KoTaP ----------------------------------------------
# Regla metodológica central: target_ROA se crea sobre el panel completo ANTES
# de filtrar outliers. Después se filtran filas por predictores y recién ahí se
# separa temporalmente train/test/futuro.

.forbidden_pca_vars <- c("ROA", "ROE", "target_ROA")

.require_columns <- function(data, cols, context) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      sprintf("%s requiere columnas ausentes: %s", context, paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }
}

.positive_variance_vars <- function(data) {
  names(data)[vapply(
    data,
    function(x) {
      var_x <- stats::var(x, na.rm = TRUE)
      is.finite(var_x) && !is.na(var_x) && var_x > 0
    },
    logical(1)
  )]
}

.complete_finite_cases <- function(data) {
  stats::complete.cases(data) & apply(
    data,
    1,
    function(row) all(is.finite(row))
  )
}

crear_target_roa <- function(data, firm_col = "stock", year_col = "year", roa_col = "ROA") {
  .require_columns(data, c(firm_col, year_col, roa_col), "crear_target_roa")

  ordered <- data[order(data[[firm_col]], data[[year_col]]), , drop = FALSE]
  ordered$target_ROA <- NA_real_

  group_index <- split(seq_len(nrow(ordered)), ordered[[firm_col]])
  for (idx in group_index) {
    ordered$target_ROA[idx] <- c(ordered[[roa_col]][idx[-1]], NA_real_)
  }

  ordered$.orden_original_target_roa <- seq_len(nrow(ordered))
  restored <- ordered[order(match(rownames(ordered), rownames(data))), , drop = FALSE]
  restored$.orden_original_target_roa <- NULL
  attr(restored, "target_roa_created") <- TRUE
  restored
}

filtrar_outliers_iqr_predictores <- function(data, vars, k = 3, target_col = "target_ROA") {
  if (!target_col %in% names(data)) {
    warning("target_ROA debe crearse antes de filtrar outliers IQR.", call. = FALSE)
    stop("target_ROA must exist before IQR filtering", call. = FALSE)
  }

  vars_presentes <- intersect(vars, names(data))
  mask <- rep(TRUE, nrow(data))

  for (col in vars_presentes) {
    if (!is.numeric(data[[col]])) next
    q <- stats::quantile(data[[col]], probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
    iqr <- q[2] - q[1]
    if (!is.finite(iqr) || is.na(iqr) || iqr == 0) next

    lower <- q[1] - k * iqr
    upper <- q[2] + k * iqr
    mask <- mask & !is.na(data[[col]]) & is.finite(data[[col]]) & data[[col]] >= lower & data[[col]] <= upper
  }

  filtered <- data[mask, , drop = FALSE]
  attr(filtered, "target_roa_created") <- TRUE
  attr(filtered, "iqr_filtered") <- TRUE
  filtered
}

dividir_temporalmente <- function(data, train_max_year = 2022, test_year = 2023, future_year = 2024) {
  .require_columns(data, "year", "dividir_temporalmente")

  list(
    train = data[data$year <= train_max_year & !is.na(data$target_ROA), , drop = FALSE],
    test = data[data$year == test_year & !is.na(data$target_ROA), , drop = FALSE],
    future = data[data$year == future_year, , drop = FALSE]
  )
}

preparar_matriz_pca <- function(data,
                                vars_candidatas,
                                vars_excluidas = c("stock", "name", "firm", "company", "ticker", "id", "year", .forbidden_pca_vars),
                                vars_removidas_colinealidad = character(),
                                return_source_rows = FALSE) {
  vars_presentes <- intersect(vars_candidatas, names(data))
  vars_limpias <- setdiff(vars_presentes, c(vars_excluidas, vars_removidas_colinealidad))

  if (length(vars_limpias) == 0) {
    stop("No hay variables candidatas válidas para PCA.", call. = FALSE)
  }

  x <- data[, vars_limpias, drop = FALSE]
  for (col in names(x)) {
    if (is.logical(x[[col]])) {
      x[[col]] <- as.numeric(x[[col]])
    } else if (is.factor(x[[col]])) {
      numeric_factor <- suppressWarnings(as.numeric(as.character(x[[col]])))
      if (all(is.na(x[[col]]) | !is.na(numeric_factor))) {
        x[[col]] <- numeric_factor
      } else if (length(stats::na.omit(unique(x[[col]]))) == 2) {
        x[[col]] <- as.numeric(x[[col]]) - 1
      }
    }
  }

  x <- x[, vapply(x, is.numeric, logical(1)), drop = FALSE]
  source_rows <- seq_len(nrow(data))
  complete_finite <- .complete_finite_cases(x)
  x <- x[complete_finite, , drop = FALSE]
  source_rows <- source_rows[complete_finite]
  x <- x[, .positive_variance_vars(x), drop = FALSE]

  if (ncol(x) < 2) {
    stop("PCA requiere al menos dos variables numéricas con varianza positiva.", call. = FALSE)
  }
  if (nrow(x) < 2) {
    stop("PCA requiere al menos dos observaciones completas después del filtrado.", call. = FALSE)
  }

  if (isTRUE(return_source_rows)) {
    return(list(
      x = x,
      source_rows = source_rows,
      source_rownames = rownames(data)[source_rows]
    ))
  }

  x
}

ajustar_pca_train <- function(df_train, vars = NULL, train_max_year = 2022) {
  if ("year" %in% names(df_train) && any(df_train$year > train_max_year, na.rm = TRUE)) {
    stop("df_train contains rows with year > 2022", call. = FALSE)
  }

  vars_pca <- if (is.null(vars)) setdiff(names(df_train), "year") else vars
  if (any(.forbidden_pca_vars %in% vars_pca)) {
    stop("vars_pca must not contain ROA, ROE, or target_ROA", call. = FALSE)
  }

  .require_columns(df_train, vars_pca, "ajustar_pca_train")
  x <- df_train[, vars_pca, drop = FALSE]
  if (!all(vapply(x, is.numeric, logical(1)))) {
    stop("vars_pca debe contener solo variables numéricas.", call. = FALSE)
  }

  x <- x[.complete_finite_cases(x), , drop = FALSE]
  x <- x[, .positive_variance_vars(x), drop = FALSE]

  if (ncol(x) < 2) {
    stop("PCA requiere al menos dos variables numéricas con varianza positiva.", call. = FALSE)
  }
  if (nrow(x) < 2) {
    stop("PCA requiere al menos dos observaciones completas después del filtrado.", call. = FALSE)
  }

  pca <- stats::prcomp(x, center = TRUE, scale. = TRUE)
  eigenvalues <- pca$sdev^2
  k_kaiser <- sum(eigenvalues >= 1)
  retained <- if (k_kaiser > 0) seq_len(k_kaiser) else integer(0)

  list(
    pca = pca,
    eigenvalues = eigenvalues,
    k_kaiser = k_kaiser,
    vars = colnames(x),
    center = pca$center,
    scale = pca$scale,
    rotation = pca$rotation,
    x = pca$x[, retained, drop = FALSE]
  )
}

proyectar_pca <- function(pca_fit, newdata) {
  .require_columns(newdata, pca_fit$vars, "proyectar_pca")
  if (pca_fit$k_kaiser == 0) {
    return(as.data.frame(matrix(nrow = nrow(newdata), ncol = 0)))
  }

  x <- as.matrix(newdata[, pca_fit$vars, drop = FALSE])
  x_scaled <- scale(x, center = pca_fit$center, scale = pca_fit$scale)
  scores <- x_scaled %*% pca_fit$rotation[, seq_len(pca_fit$k_kaiser), drop = FALSE]
  scores <- as.data.frame(scores)
  names(scores) <- paste0("PC", seq_len(ncol(scores)))
  scores
}

ajustar_ols_pc <- function(scores_train, target) {
  scores <- as.data.frame(scores_train)
  pc_cols <- grep("^PC[0-9]+$", names(scores), value = TRUE)
  scores <- scores[, pc_cols, drop = FALSE]

  if (ncol(scores) == 0) {
    stop("No components retained under Kaiser rule", call. = FALSE)
  }

  model_data <- cbind(target_ROA = target, scores)
  model_data <- model_data[stats::complete.cases(model_data), , drop = FALSE]
  stats::lm(stats::as.formula(paste("target_ROA ~", paste(pc_cols, collapse = " + "))), data = model_data)
}

diagnosticar_ols_pc <- function(modelo, top_n = 10) {
  n <- stats::nobs(modelo)
  p <- length(stats::coef(modelo))
  std_resid <- stats::rstandard(modelo)
  fitted_values <- stats::fitted(modelo)
  actual <- stats::model.response(stats::model.frame(modelo))
  leverage <- stats::hatvalues(modelo)
  cook <- stats::cooks.distance(modelo)

  residual_threshold <- 2
  leverage_threshold <- 2 * p / n
  cook_threshold <- 4 / n

  idx_resid <- which(is.finite(std_resid) & abs(std_resid) > residual_threshold)
  idx_leverage <- which(is.finite(leverage) & leverage > leverage_threshold)
  idx_cook <- which(is.finite(cook) & cook > cook_threshold)

  residuos <- data.frame(
    obs = idx_resid,
    residuo_estandar = std_resid[idx_resid],
    severidad = ifelse(abs(std_resid[idx_resid]) > 3, "critico", "advertencia"),
    fitted = fitted_values[idx_resid],
    actual = actual[idx_resid],
    row.names = NULL
  )

  leverage_df <- data.frame(
    obs = idx_leverage,
    leverage = leverage[idx_leverage],
    row.names = NULL
  )

  cook_df <- data.frame(
    obs = idx_cook,
    cook_d = cook[idx_cook],
    leverage = leverage[idx_cook],
    residuo_estandar = std_resid[idx_cook],
    row.names = NULL
  )

  if (is.finite(top_n) && top_n > 0) {
    residuos <- utils::head(residuos[order(-abs(residuos$residuo_estandar)), , drop = FALSE], top_n)
    leverage_df <- utils::head(leverage_df[order(-leverage_df$leverage), , drop = FALSE], top_n)
    cook_df <- utils::head(cook_df[order(-cook_df$cook_d), , drop = FALSE], top_n)
  }

  list(
    residuos_atipicos = residuos,
    leverage = leverage_df,
    cook = cook_df,
    umbrales = list(
      residuo_advertencia = residual_threshold,
      residuo_critico = 3,
      leverage = leverage_threshold,
      cook = cook_threshold
    )
  )
}
