localidades_oc <- dplyr::rename(dplyr::select(obtener_capa("localidades"),c(3,5)),localidad_normalizada = nombre)
#' obtener_capa
#'
#' Permite normalizar y corregir los nombres de las localidades en base a la superposición geométrica
#' @param df nombre de la capa a descargar
#' @param localidades description
#' @return devuelve la capa deseada, en caso de no conocer el nombre utilizar la función de inventario_capas
#' @examples
#' capa <- obtener_capa(nombre_de_capa = "localidades");
#' @export

normalizar_localidades <- function(df = NULL, localidades = localidades_oc){
sf::st_join(df,localidades, join = sf::st_within)
}


