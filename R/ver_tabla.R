#' Visualizar datos sin notación científica
#'
#' Abre el visor de datos de R (View) formateando previamente las columnas numéricas
#' para evitar la notación científica. Es útil para visualizar identificadores largos.
#'
#' @param x Un data.frame o matriz a visualizar.
#' @param digits Entero (opcional). Cantidad de decimales fijos a mostrar.
#' @param ... Argumentos adicionales pasados a \code{utils::View} (ej. \code{title}).
#' @return Abre el visor de datos.
#' @export
ver_tabla <- function(x, digits = NULL, ...) {
  x_fmt <- x
  num_cols <- sapply(x_fmt, is.numeric)
  
  # Solo aplicar formato si existen columnas numéricas
  if (any(num_cols)) {
    x_fmt[num_cols] <- lapply(x_fmt[num_cols], function(col) {
      if (!is.null(digits)) {
        formatC(col, format = "f", digits = digits, drop0trailing = FALSE)
      } else {
        format(col, scientific = FALSE, trim = TRUE)
      }
    })
  }
  
  .mostrar_tabla(x_fmt, ...)
}

# Wrapper delgado sobre utils::View: permite mockearlo en los tests sin
# tocar el namespace de utils (ver testthat::local_mocked_bindings).
.mostrar_tabla <- function(x, ...) {
  utils::View(x, ...)
}