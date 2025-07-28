
#' obtener_inventario
#'
#' Lista las capas disponibles en el geoportal.
#'
#' @param usar_autenticacion Lógico. Si TRUE, intenta cargar el token guardado del usuario para mostrar capas privadas.
#' @param limpiar_prefijo Lógico. Si TRUE, remueve el prefijo "geonode:" de los nombres de capa.
#' @param .interno No tocar. Usado internamente para evitar recursión infinita.
#' @return Un vector con los nombres de las capas disponibles
#' @export
obtener_inventario <- function(usar_autenticacion = FALSE, limpiar_prefijo = TRUE, .interno = FALSE) {
  if (!usar_autenticacion && !.interno) {
    return(callr::r(
      function(limpiar_prefijo) {
        req <- httr::GET(
          url = "https://geoportal.tresdefebrero.gob.ar/geoserver/ows",
          query = list(service = "WFS", request = "GetCapabilities")
        )
        httr::stop_for_status(req)
        xml <- xml2::read_xml(httr::content(req, as = "text", encoding = "UTF-8"))
        capas <- xml2::xml_find_all(xml, ".//*[local-name()='FeatureType']/*[local-name()='Name']")
        nombres <- xml2::xml_text(capas)
        if (limpiar_prefijo) nombres <- sub("^geonode:", "", nombres)
        return(nombres)
      },
      args = list(limpiar_prefijo = limpiar_prefijo),
      show = FALSE
    ))
  }
  
  token_dir <- tools::R_user_dir("geoportal3f", which = "cache")
  cache_path <- file.path(token_dir, "token_geoportal3F.rds")
  token <- NULL
  
  if (usar_autenticacion && file.exists(cache_path)) {
    token <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (is.null(token)) warning("⚠️ El token guardado no pudo leerse correctamente.")
  } else if (usar_autenticacion) {
    warning("No se encontró token guardado. Solo se mostrarán capas públicas.")
  }
  
  headers <- if (!is.null(token)) httr::add_headers(Authorization = paste("Bearer", token$credentials$access_token)) else NULL
  
  req <- httr::GET(
    url = "https://geoportal.tresdefebrero.gob.ar/geoserver/ows",
    query = list(service = "WFS", request = "GetCapabilities"),
    headers
  )
  
  httr::stop_for_status(req)
  xml <- xml2::read_xml(httr::content(req, as = "text", encoding = "UTF-8"))
  capas <- xml2::xml_find_all(xml, ".//*[local-name()='FeatureType']/*[local-name()='Name']")
  nombres <- xml2::xml_text(capas)
  
  if (limpiar_prefijo) {
    nombres <- sub("^geonode:", "", nombres)
  }
  
  return(nombres)
}
