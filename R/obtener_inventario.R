#' obtener_inventario
#'
#' Lista las capas disponibles en el geoportal.
#'
#' @param usar_autenticacion L\u00f3gico. Si TRUE, intenta cargar el token guardado del usuario para mostrar capas privadas.
#' @param limpiar_prefijo L\u00f3gico. Si TRUE, remueve el prefijo "geonode:" de los nombres de capa.
#' @param ignorar_SSL L\u00f3gico. Si TRUE, desactiva la verificaci\u00f3n SSL (solo usar en entornos de desarrollo o servidores internos).
#' @param .interno No tocar. Usado internamente para evitar recursi\u00f3n infinita.
#' @return Un vector con los nombres de las capas disponibles
#' @export
obtener_inventario <- function(usar_autenticacion = FALSE, limpiar_prefijo = TRUE, ignorar_SSL = FALSE, .interno = FALSE) {
  # --- Rama sin autenticaci\u00f3n (usa callr) ---
  if (!usar_autenticacion && !.interno) {
    return(callr::r(
      function(limpiar_prefijo, ignorar_SSL) {
        
        cfg <- if (isTRUE(ignorar_SSL)) httr::config(ssl_verifypeer = FALSE) else NULL
        
        req <- httr::GET(
          url = "https://geoportal.tresdefebrero.gob.ar/geoserver/ows",
          query = list(service = "WFS", request = "GetCapabilities"),
          cfg
        )
        httr::stop_for_status(req)
        
        xml <- xml2::read_xml(httr::content(req, as = "text", encoding = "UTF-8"))
        capas <- xml2::xml_find_all(xml, ".//*[local-name()='FeatureType']/*[local-name()='Name']")
        nombres <- xml2::xml_text(capas)
        
        if (limpiar_prefijo) nombres <- sub("^geonode:", "", nombres)
        return(nombres)
      },
      args = list(limpiar_prefijo = limpiar_prefijo, ignorar_SSL = ignorar_SSL),
      show = FALSE
    ))
  }
  
  # --- Rama autenticada ---
  token_dir <- tools::R_user_dir("geoportal3f", which = "cache")
  cache_path <- file.path(token_dir, "token_geoportal3F.rds")
  token <- NULL
  
  if (usar_autenticacion && file.exists(cache_path)) {
    token <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (is.null(token)) warning("[!] El token guardado no pudo leerse correctamente.")
  } else if (usar_autenticacion) {
    warning("No se encontr\u00f3 token guardado. Solo se mostrar\u00e1n capas p\u00fablicas.")
  }
  
  headers <- if (!is.null(token)) httr::add_headers(Authorization = paste("Bearer", token$credentials$access_token)) else NULL
  cfg <- if (isTRUE(ignorar_SSL)) httr::config(ssl_verifypeer = FALSE) else NULL
  
  req <- httr::GET(
    url = "https://geoportal.tresdefebrero.gob.ar/geoserver/ows",
    query = list(service = "WFS", request = "GetCapabilities"),
    headers,
    cfg
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