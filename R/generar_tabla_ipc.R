#' Generar tabla de factores de ajuste IPC
#'
#' Descarga la serie histórica del IPC (Nivel General Nacional) desde el INDEC,
#' completa los meses faltantes hasta la actualidad proyectando el último valor
#' y calcula los coeficientes de ajuste respecto a un mes base.
#'
#' @param mes_base Carácter con formato "YYYY-MM" o el string "ult_disponible".
#'   Indica el mes en el que el coeficiente será 1. Si es \code{NULL}, devuelve la serie cruda.
#'
#' @return Un data.frame con columnas \code{indice_tiempo} (Date) y \code{valor_ipc} (numérico).
#' @export
#' @importFrom dplyr filter mutate select %>%
#' @importFrom lubridate ymd %m+% floor_date as_date
#' @importFrom utils read.csv2 tail
generar_tabla_ipc <- function(mes_base = "ult_disponible") {

  url <- "https://www.indec.gob.ar/ftp/cuadros/economia/serie_ipc_divisiones.csv"

  tabla_ipc <- tryCatch({
    utils::read.csv2(url, stringsAsFactors = FALSE)
  }, error = function(e) {
    stop("Error al descargar o leer datos del INDEC.")
  })

  tabla_ipc <- tabla_ipc %>%
    dplyr::filter(Descripcion == "NIVEL GENERAL", Region == "Nacional") %>%
    dplyr::mutate(indice_tiempo = lubridate::ymd(paste0(Periodo, "01"))) %>%
    dplyr::select(indice_tiempo, ipc_nivel_general_nacional = Indice_IPC)

  if (is.null(mes_base)) {
    return(tabla_ipc)
  }

  # Definir fecha base
  if (mes_base == "ult_disponible") {
    mes_base_date <- max(tabla_ipc$indice_tiempo, na.rm = TRUE)
  } else {
    mes_base_date <- lubridate::as_date(paste0(mes_base, "-01"))
    if (is.na(mes_base_date)) stop("Formato de mes_base inválido. Use 'YYYY-MM'.")
  }

  # Proyectar meses faltantes
  ultimo_mes <- max(tabla_ipc$indice_tiempo, na.rm = TRUE)
  fecha_actual <- lubridate::floor_date(Sys.Date(), "month")

  if (ultimo_mes < fecha_actual) {
    meses_faltantes <- seq(from = ultimo_mes %m+% months(1), to = fecha_actual, by = "1 month")
    
    if (length(meses_faltantes) > 0) {
      fila_nueva <- data.frame(
        indice_tiempo = meses_faltantes,
        ipc_nivel_general_nacional = utils::tail(tabla_ipc$ipc_nivel_general_nacional, 1)
      )
      tabla_ipc <- rbind(tabla_ipc, fila_nueva)
    }
  }

  # Calcular coeficientes
  ipc_base_val <- tabla_ipc$ipc_nivel_general_nacional[tabla_ipc$indice_tiempo == mes_base_date]

  if (length(ipc_base_val) == 0) {
    stop("El mes base seleccionado no existe en la serie disponible.")
  }

  tabla_ipc %>%
    dplyr::mutate(valor_ipc = (ipc_nivel_general_nacional / 100) / (ipc_base_val / 100)) %>%
    dplyr::select(indice_tiempo, valor_ipc)
}