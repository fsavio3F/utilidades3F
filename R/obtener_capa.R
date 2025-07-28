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
  inventario <- obtener_inventario(usar_autenticacion = usar_autenticacion)
  
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
        writeLines(httr::content(res, as = "text", encoding = "UTF-8"), tmp)
        
        capa <- tryCatch({
          sf::read_sf(tmp) |> sf::st_transform(crs = 4326)
        }, error = function(e) {
          stop("❌ La capa solicitada no está disponible públicamente o la respuesta no es válida.")
        })
        
        return(capa)
      },
      args = list(nombre = nombre_valido),
      show = FALSE
    ))
  }
  
  token_dir <- tools::R_user_dir("geoportal3f", which = "cache")
  cache_path <- file.path(token_dir, "token_geoportal3F.rds")
  
  if (!file.exists(cache_path)) {
    warning("No se encontró token guardado. Se intentará acceso público.")
    return(obtener_capa(nombre_de_capa, usar_autenticacion = FALSE, .interno = TRUE))
  }
  
  token <- tryCatch(readRDS(cache_path), error = function(e) NULL)
  
  if (is.null(token)) {
    warning("⚠️ No se pudo leer el token. Se intentará acceso público.")
    return(obtener_capa(nombre_de_capa, usar_autenticacion = FALSE, .interno = TRUE))
  }
  
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
  
  capa <- tryCatch({
    sf::read_sf(tmp) |> sf::st_transform(crs = 4326)
  }, error = function(e) {
    stop("❌ La capa solicitada no pudo ser cargada. Verificá permisos y formato.")
  })
  
  return(capa)
}
