#' obtener_capa
#'
#' Descarga una capa WFS del geoportal. Puede usarse con o sin autenticación.
#'
#' @param nombre_de_capa Nombre completo o sin prefijo de la capa (por ejemplo: "localidades" o "geonode:localidades").
#' @param usar_autenticacion Lógico. Si TRUE, intenta usar el token guardado para autenticarse.
#' @param ignorar_SSL Lógico. Si TRUE, desactiva la verificación SSL (solo usar en entornos de desarrollo o servidores internos).
#' @param .interno No tocar. Usado internamente para evitar recursión infinita.
#' @return Un objeto `sf` con los datos espaciales descargados.
#' @export
obtener_capa <- function(nombre_de_capa, usar_autenticacion = FALSE, ignorar_SSL = FALSE, .interno = FALSE) {
  inventario <- tryCatch(
    obtener_inventario(usar_autenticacion = usar_autenticacion, ignorar_SSL = ignorar_SSL),
    error = function(e) character(0)
  )
  
  posibles_nombres <- unique(c(
    nombre_de_capa,
    paste0("geonode:", nombre_de_capa),
    sub("^geonode:", "", nombre_de_capa)
  ))
  
  nombre_valido <- intersect(posibles_nombres, inventario)[1]
  
  if (is.na(nombre_valido) || is.null(nombre_valido)) {
    stop("[ERROR] La capa solicitada no existe. Revis\u00e1 el nombre o el par\u00e1metro 'usar_autenticacion'.")
  }
  
  # --- Rama sin autenticación (usa callr) ---
  if (!usar_autenticacion && !.interno) {
    return(callr::r(
      function(nombre, ignorar_SSL) {
        
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
        
        cfg <- if (isTRUE(ignorar_SSL)) httr::config(ssl_verifypeer = FALSE) else NULL
        res <- httr::GET(url, cfg)
        httr::stop_for_status(res)
        
        tmp <- tempfile(fileext = ".geojson")
        writeLines(httr::content(res, as = "text", encoding = "UTF-8"), tmp)
        
        capa <- tryCatch({
          sf::read_sf(tmp) |> sf::st_transform(crs = 4326)
        }, error = function(e) {
          stop("La capa no est\u00e1 disponible p\u00fablicamente o la respuesta no es v\u00e1lida.")
        })
        
        return(capa)
      },
      args = list(nombre = nombre_valido, ignorar_SSL = ignorar_SSL),
      show = FALSE
    ))
  }
  
  # --- Rama autenticada ---
  token_dir <- tools::R_user_dir("geoportal3f", which = "cache")
  cache_path <- file.path(token_dir, "token_geoportal3F.rds")
  
  if (!file.exists(cache_path)) {
    warning("No se encontr\u00f3 token. Se intentar\u00e1 acceso p\u00fablico.")
    return(obtener_capa(nombre_de_capa, usar_autenticacion = FALSE, ignorar_SSL = ignorar_SSL, .interno = TRUE))
  }
  
  token <- tryCatch(readRDS(cache_path), error = function(e) NULL)
  
  if (is.null(token)) {
    warning("[!] No se pudo leer el token. Se intentar\u00e1 acceso p\u00fablico.")
    return(obtener_capa(nombre_de_capa, usar_autenticacion = FALSE, ignorar_SSL = ignorar_SSL, .interno = TRUE))
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
  
  cfg <- if (isTRUE(ignorar_SSL)) httr::config(ssl_verifypeer = FALSE) else NULL
  
  res <- httr::GET(
    url,
    httr::add_headers(Authorization = paste("Bearer", token$credentials$access_token)),
    cfg
  )
  httr::stop_for_status(res)
  
  tmp <- tempfile(fileext = ".geojson")
  writeLines(httr::content(res, as = "text", encoding = "UTF-8"), tmp)
  
  capa <- tryCatch({
    sf::read_sf(tmp) |> sf::st_transform(crs = 4326)
  }, error = function(e) {
    stop("[ERROR] La capa no pudo ser cargada. Verific\u00e1 permisos y formato.")
  })
  
  return(capa)
}
