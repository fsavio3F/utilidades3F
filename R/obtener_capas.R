#' autenticar_geoportal
#'
#' Inicia sesión en el GeoPortal de Tres de Febrero mediante OAuth2 y devuelve un token de acceso válido.
#'
#' @param client_id ID del cliente OAuth2 registrado en el geoportal. No es necesario si el token ya fue guardado previamente.
#' @param client_secret Clave secreta del cliente OAuth2. No es necesario si el token ya fue guardado previamente.
#' @param guardar Si TRUE, guarda el token en un directorio persistente del usuario y lo reutiliza en sesiones siguientes.
#' @return Un objeto de clase `Token2.0` con credenciales OAuth2.
#' @examples
#' token <- autenticar_geoportal("mi_client_id", "mi_client_secret", guardar = TRUE)
#' token <- autenticar_geoportal()  # Reutiliza el token ya guardado
#' @export
autenticar_geoportal <- function(client_id = NULL, client_secret = NULL, guardar = TRUE) {
  # Ruta persistente al token
  token_dir <- tools::R_user_dir("geoportal3f", which = "cache")
  if (!dir.exists(token_dir)) dir.create(token_dir, recursive = TRUE)
  cache_path <- file.path(token_dir, "token_geoportal3F.rds")
  
  # Si ya existe un token guardado, usarlo
  if (guardar && file.exists(cache_path)) {
    token <- readRDS(cache_path)
    message("🔓 Token cargado desde: ", cache_path)
    return(token)
  }
  
  # Si no hay credenciales para autenticar, dar error
  if (is.null(client_id) || is.null(client_secret)) {
    stop("❌ No se encontró un token guardado y no se proporcionaron client_id y client_secret.")
  }
  
  # Proceso de autenticación OAuth2
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
  cache_path <- file.path(token_dir, "token_geoportal3F.rds")
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
#' @param nombre_de_capa Nombre completo o sin prefijo de la capa (por ejemplo: "localidades" o "geonode:localidades").
#' @param usar_autenticacion Lógico. Si TRUE, intenta usar el token guardado para autenticarse.
#' @param .interno No tocar. Usado internamente para evitar recursión infinita.
#' @return Un objeto `sf` con los datos espaciales descargados.
#' @export
obtener_capa <- function(nombre_de_capa, usar_autenticacion = FALSE, .interno = FALSE) {
  # Validar nombre de capa antes de intentar descargar
  inventario <- inventario_capas(usar_autenticacion = usar_autenticacion)
  posibles_nombres <- unique(c(
    nombre_de_capa,
    paste0("geonode:", nombre_de_capa),
    sub("^geonode:", "", nombre_de_capa)
  ))
  nombre_valido <- intersect(posibles_nombres, inventario)[1]
  
  if (is.na(nombre_valido)) {
    stop("❌ La capa solicitada no existe en el geoportal. Revisá el nombre o el parámetro 'usar_autenticacion'.")
  }
  
  if (!usar_autenticacion && !.interno) {
    return(callr::r(
      function(nombre) {
        suppressPackageStartupMessages({
          library(httr)
          library(sf)
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
        
        tmp <- tempfile(fileext = ".geojson")
        content_text <- httr::content(res, as = "text", encoding = "UTF-8")
        writeLines(content_text, tmp)
        
        tryCatch({
          capa <- sf::read_sf(tmp)
          sf::st_transform(capa, crs = 4326)
        }, error = function(e) {
          stop("❌ La capa solicitada no está disponible públicamente o la respuesta no es válida.")
        })
      },
      args = list(nombre = nombre_valido),
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
      typename = nombre_valido,
      outputFormat = "application/json"
    )
  )
  res <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", token$credentials$access_token)))
  httr::stop_for_status(res)
  
  tmp <- tempfile(fileext = ".geojson")
  writeLines(httr::content(res, as = "text", encoding = "UTF-8"), tmp)
  
  tryCatch({
    capa <- sf::read_sf(tmp)
    sf::st_transform(capa, crs = 4326)
  }, error = function(e) {
    stop("❌ La capa solicitada no pudo ser cargada. Verificá permisos y formato.")
  })
}
