#' Convertir valores nominales a t\u00e9rminos reales
#'
#' Ajusta una serie de valores nominales por inflaci\u00f3n utilizando una tabla de IPC,
#' llevando los valores a precios de un mes base espec\u00edfico.
#'
#' @param df Dataframe que contiene las series a convertir.
#' @param col_fecha String. Nombre de la columna de fecha (acepta objetos Date o strings "YYYY-MM").
#' @param col_valor String. Nombre de la columna con los valores nominales.
#' @param tabla_ipc Dataframe. Tabla generada por \code{generar_tabla_ipc()} que contiene
#'   las columnas \code{indice_tiempo} y \code{valor_ipc}.
#'
#' @return El dataframe original con una columna nueva agregada (sufijo \code{_tr}) con los valores ajustados.
#' @export
#' @importFrom dplyr left_join filter pull first %>%
#' @importFrom stringr str_detect
#' @importFrom lubridate as_date floor_date
#' @importFrom rlang .data
pasar_a_terminos_reales <- function(df, col_fecha, col_valor, tabla_ipc) {

  # Validaci\u00f3n b\u00e1sica de input
  if (!all(c("indice_tiempo", "valor_ipc") %in% names(tabla_ipc))) {
    stop("La tabla_ipc no tiene el formato esperado (requiere 'indice_tiempo' y 'valor_ipc').")
  }

  # 1. Estandarizaci\u00f3n de tipos y chequeo de NAs
  na_ini_fecha <- sum(is.na(df[[col_fecha]]))
  na_ini_valor <- sum(is.na(df[[col_valor]]))

  df[[col_fecha]] <- as.character(df[[col_fecha]])
  df[[col_valor]] <- suppressWarnings(as.numeric(df[[col_valor]]))

  na_fin_fecha <- sum(is.na(df[[col_fecha]]))
  na_fin_valor <- sum(is.na(df[[col_valor]]))

  if (na_fin_fecha > na_ini_fecha) {
    warning(paste(na_fin_fecha - na_ini_fecha, "fechas se convirtieron en NA. Revisar formato."))
  }
  if (na_fin_valor > na_ini_valor) {
    warning(paste(na_fin_valor - na_ini_valor, "valores se convirtieron en NA. Revisar si hay caracteres no num\u00e9ricos."))
  }

  # 2. Obtener Mes Base de la tabla IPC (donde coeficiente == 1)
  mes_base <- tabla_ipc %>%
    dplyr::filter(.data$valor_ipc == 1) %>%
    dplyr::pull(.data$indice_tiempo) %>%
    dplyr::first()

  if (length(mes_base) == 0) warning("No se detect\u00f3 un mes base con valor_ipc == 1 en la tabla provista.")

  # 3. Normalizaci\u00f3n de fechas para el join
  fechas_temp <- df[[col_fecha]]
  es_formato_ym <- stringr::str_detect(fechas_temp, "^\\d{4}-\\d{2}$")
  es_formato_ym[is.na(es_formato_ym)] <- FALSE

  fechas_temp[es_formato_ym] <- paste0(fechas_temp[es_formato_ym], "-01")

  df$aux_fecha_join <- lubridate::floor_date(lubridate::as_date(fechas_temp), "month")

  # 4. Join y C\u00e1lculo
  df <- dplyr::left_join(df, tabla_ipc, by = c("aux_fecha_join" = "indice_tiempo"))

  sufijo <- "_tr"
  var_treales <- paste0(col_valor, sufijo)

  df[[var_treales]] <- round(df[[col_valor]] / df$valor_ipc, 2)

  # 5. Reporte y Limpieza
  na_finales <- sum(is.na(df[[var_treales]]))
  registros_sin_ipc <- sum(is.na(df$valor_ipc) & !is.na(df$aux_fecha_join))

  df$aux_fecha_join <- NULL
  df$valor_ipc <- NULL

  message(sprintf("[OK] Columna generada: %s (Base: %s)", var_treales, format(mes_base, "%Y-%m")))
  if (registros_sin_ipc > 0) {
    message(sprintf("[WARN] Advertencia: %d registros tienen fecha v\u00e1lida pero no encontraron dato en tabla_ipc.", registros_sin_ipc))
  }
  
  return(df)
}