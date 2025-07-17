#' autenticar_geoportal
#'
#' Inicia sesión en el GeoPortal de Tres de Febrero mediante OAuth2 y devuelve un token de acceso válido.
#'
#' @param client_id ID del cliente OAuth2 registrado en el geoportal.
#' @param client_secret Clave secreta del cliente OAuth2.
#' @param guardar Si TRUE, guarda el token en un directorio persistente del usuario y lo reutiliza en sesiones siguientes.
#' @return Un objeto de clase `Token2.0` con credenciales OAuth2.
#' @examples
#' token <- autenticar_geoportal("mi_client_id", "mi_client_secret", guardar = TRUE)
#' @export
autenticar_geoportal <- function(client_id, client_secret, guardar = TRUE) {
  endpoint <- httr::oauth_endpoint(
    authorize = "https://geoportal.tresdefebrero.gob.ar/o/authorize/",
    access    = "https://geoportal.tresdefebrero.gob.ar/o/token/"
  )
  
  app <- httr::oauth_app(
    appname      = "geoportal3f",
    key          = client_id,
    secret       = client_secret,
    redirect_uri = "http://localhost:1410/"
  )
  
  # Token en directorio persistente del usuario
  token_dir <- tools::R_user_dir("geoportal3f", which = "cache")
  if (!dir.exists(token_dir)) dir.create(token_dir, recursive = TRUE)
  cache_path <- file.path(token_dir, "token.rds")
  
  if (guardar && file.exists(cache_path)) {
    token <- readRDS(cache_path)
  } else {
    token <- httr::oauth2.0_token(
      endpoint = endpoint,
      app = app,
      scope = "openid write",
      use_basic_auth = TRUE,
      cache = FALSE
    )
    if (guardar) {
      saveRDS(token, cache_path)
      message("🔐 Token guardado en: ", cache_path)
    }
  }
  
  return(token)
}

#' inventario_capas
#'
#' Lista las capas disponibles en el geoportal.
#'
#' @param usar_autenticacion Lógico. Si TRUE, intenta cargar el token guardado del usuario para mostrar capas privadas.
#' @param limpiar_prefijo Lógico. Si TRUE, remueve el prefijo "geonode:" de los nombres de capa.
#' @param .interno No tocar. Usado internamente para evitar recursión infinita.
#' @return Un vector con los nombres de las capas disponibles
#' @export
inventario_capas <- function(usar_autenticacion = FALSE, limpiar_prefijo = TRUE, .interno = FALSE) {
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
  cache_path <- file.path(token_dir, "token.rds")
  token <- NULL
  if (usar_autenticacion && file.exists(cache_path)) {
    token <- readRDS(cache_path)
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

#' obtener_capa
#'
#' Descarga una capa WFS del geoportal. Puede usarse con o sin autenticación.
#'
#' @param nombre_de_capa Nombre completo de la capa (por ejemplo: "geonode:localidades").
#' @param usar_autenticacion Lógico. Si TRUE, intenta usar el token guardado para autenticarse.
#' @param .interno No tocar. Usado internamente para evitar recursión infinita.
#' @return Un objeto `sf` con los datos espaciales descargados.
#' @export
obtener_capa <- function(nombre_de_capa, usar_autenticacion = FALSE, .interno = FALSE) {
  if (!usar_autenticacion && !.interno) {
    return(callr::r(
      function(nombre) {
        suppressPackageStartupMessages({
          library(httr)
          library(sf)
          library(jsonlite)
        })
        
        url <- httr::modify_url(
          url = "https://geoportal.tresdefebrero.gob.ar/geoserver/ows",
          query = list(
            service = "WFS",
            version = "1.1.0",
            request = "GetFeature",
            typename = nombre,
            outputFormat = "application/json"
          )
        )
        res <- httr::GET(url)
        httr::stop_for_status(res)
        
        contenido <- httr::content(res, as = "text", encoding = "UTF-8")
        
        # Verifica si es JSON válido
        if (!jsonlite::validate(contenido)) {
          stop("❌ La capa solicitada no está disponible públicamente o la respuesta no es válida.")
        }
        
        tmp <- tempfile(fileext = ".geojson")
        writeLines(contenido, tmp)
        capa <- sf::read_sf(tmp)
        sf::st_transform(capa, crs = 4326)
      },
      args = list(nombre = nombre_de_capa),
      show = FALSE
    ))
  }
  
  token_dir <- tools::R_user_dir("geoportal3f", which = "cache")
  cache_path <- file.path(token_dir, "token.rds")
  if (!file.exists(cache_path)) {
    warning("No se encontró token guardado. Se intentará acceso público.")
    return(obtener_capa(nombre_de_capa, usar_autenticacion = FALSE, .interno = TRUE))
  }
  
  token <- readRDS(cache_path)
  url <- httr::modify_url(
    url = "https://geoportal.tresdefebrero.gob.ar/geoserver/ows",
    query = list(
      service = "WFS",
      version = "1.1.0",
      request = "GetFeature",
      typename = nombre_de_capa,
      outputFormat = "application/json"
    )
  )
  res <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", token$credentials$access_token)))
  httr::stop_for_status(res)
  
  tmp <- tempfile(fileext = ".geojson")
  writeLines(httr::content(res, as = "text", encoding = "UTF-8"), tmp)
  capa <- sf::read_sf(tmp)
  sf::st_transform(capa, crs = 4326)
}
