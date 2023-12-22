localidades_oc <- dplyr::select(obtener_capa("localidades"),c(3,5)) %>%
  dplyr::rename(localidad_normalizada = nombre)
#' obtener_capa
#'
#' geolocalizar direcciónes dentro de una base de datos
#' @param df nombre de la capa a descargar
#' @param localidades description
#' @return devuelve la capa deseada, en caso de no conocer el nombre utilizar la función de inventario_capas()
#' @examples
#' capa <- obtener_capa(nombre_de_capa = "localidades");
#' @export

normalizar_localidades <- function(df = NULL, localidades = localidades_oc){
st_join(df,localidades, join = st_within)
}

