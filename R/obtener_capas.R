#' obtener_capa
#'
#' Permite descargar una capa del geoportal municipal ingresando su nombre
#' @param nombre_de_capa nombre de la capa a descargar
#' @return devuelve la capa deseada, en caso de no conocer el nombre utilizar la función de inventario_capas()
#' @examples
#' capa <- obtener_capa(nombre_de_capa = "localidades");
#' @export
obtener_capa <- function(nombre_de_capa){

  # URL del servicio del GeoPortal
  wfs_3f <- "https://geoportal.tresdefebrero.gob.ar/geoserver/ows"

  # Construir la consulta usando el nombre de la capa requerida
  url <- httr::parse_url(wfs_3f)
  url$query <- list(service = "wfs",
                    version = "1.1.0",
                    request = "GetFeature",
                    typename = nombre_de_capa,
                    outputFormat="json"
  )
  request <- httr::build_url(url)

  # Descargar capa
  print(paste0("Descargando '", nombre_de_capa, "' del URL: '", request, "'"))
  capa <- sf::read_sf(request)
  capa <- sf::st_transform(capa, crs = 4326)

  return(capa)

}

#' inventario_capas
#'
#' Nos devuelve un listado de las capas disponibles en el geoportal municipal
#' @return devuelve las capas disponibles
#' @export

inventario_capas <- function(wfs = "https://geoportal.tresdefebrero.gob.ar/geoserver/ows", service_version = "1.1.0", pretty_output = TRUE) {
  client <- ows4R::WFSClient$new(wfs, serviceVersion = service_version)
  capas <- client$getFeatureTypes(pretty = pretty_output)
  return(capas)
}

