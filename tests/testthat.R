if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Falta el paquete 'testthat'. Instalalo con install.packages('testthat').", call. = FALSE)
}

testthat::test_dir("tests/testthat")
