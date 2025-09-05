# ─────────────────────────────────────────────────────────────────────────────
# CONFIG
# ─────────────────────────────────────────────────────────────────────────────
DOMAIN_URL <- "http://localhost:8000"   # tu API FastAPI

# ─────────────────────────────────────────────────────────────────────────────
# CORE HTTP
# ─────────────────────────────────────────────────────────────────────────────
request_json <- function(url_prefix, params) {
  # Fuerza UTF-8 y devuelve lista R
  res <- httr::GET(url_prefix, query = params, httr::add_headers(Accept = "application/json"))
  httr::stop_for_status(res)
  httr::content(res, type = "application/json", encoding = "UTF-8")
}

# ─────────────────────────────────────────────────────────────────────────────
# ENDPOINTS: builders
# ─────────────────────────────────────────────────────────────────────────────

# GET /health
health_url <- function() {
  list(url_prefix = paste0(DOMAIN_URL, "/health"), params = list())
}

# GET /sugerencias?qstr=&limit=
sugerencias_url <- function(qstr = "", limit = 20L) {
  limit <- as.integer(limit)
  stopifnot(limit >= 1L, limit <= 50L)
  list(
    url_prefix = paste0(DOMAIN_URL, "/sugerencias"),
    params = list(qstr = qstr, limit = limit)
  )
}

# GET /sugerencias_es2?qstr=&limit=
sugerencias_es2_url <- function(qstr = "", limit = 10L) {
  limit <- as.integer(limit)
  stopifnot(limit >= 1L, limit <= 50L)
  list(
    url_prefix = paste0(DOMAIN_URL, "/sugerencias_es2"),
    params = list(qstr = qstr, limit = limit)
  )
}

# GET /geocode_direccion?calle=&altura=&numero_cal=&fallback=
geocode_direccion_url <- function(calle = NULL, altura, numero_cal = NULL, fallback = FALSE) {
  stopifnot(length(altura) == 1)
  params <- list(altura = as.integer(altura), fallback = isTRUE(fallback))
  if (!is.null(calle))      params$calle      <- as.character(calle)
  if (!is.null(numero_cal)) params$numero_cal <- as.character(numero_cal)
  list(
    url_prefix = paste0(DOMAIN_URL, "/geocode_direccion"),
    params = params
  )
}

# GET /geocode_interseccion?calle1=&calle2=
geocode_interseccion_url <- function(calle1, calle2) {
  stopifnot(!is.null(calle1), !is.null(calle2))
  list(
    url_prefix = paste0(DOMAIN_URL, "/geocode_interseccion"),
    params = list(calle1 = as.character(calle1), calle2 = as.character(calle2))
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# ENDPOINTS: callers
# ─────────────────────────────────────────────────────────────────────────────
health <- function() {
  u <- health_url()
  request_json(u$url_prefix, u$params)
}

sugerencias <- function(qstr = "", limit = 20L) {
  u <- sugerencias_url(qstr = qstr, limit = limit)
  request_json(u$url_prefix, u$params)
}

sugerencias_es2 <- function(qstr = "", limit = 10L) {
  u <- sugerencias_es2_url(qstr = qstr, limit = limit)
  request_json(u$url_prefix, u$params)
}

geocode_direccion <- function(calle = NULL, altura, numero_cal = NULL, fallback = FALSE) {
  u <- geocode_direccion_url(calle = calle, altura = altura, numero_cal = numero_cal, fallback = fallback)
  request_json(u$url_prefix, u$params)
}

geocode_interseccion <- function(calle1, calle2) {
  u <- geocode_interseccion_url(calle1, calle2)
  request_json(u$url_prefix, u$params)
}

# ─────────────────────────────────────────────────────────────────────────────
# HELPERS: parsing robusto de coordenadas
# ─────────────────────────────────────────────────────────────────────────────
.extract_latlon <- function(x) {
  # x puede venir como lista con lat/lon arriba o nested en $centroid
  lat <- NULL; lon <- NULL
  if (!is.null(x$lat) && !is.null(x$lon)) {
    lat <- suppressWarnings(as.numeric(x$lat))
    lon <- suppressWarnings(as.numeric(x$lon))
  } else if (!is.null(x$centroid)) {
    lat <- suppressWarnings(as.numeric(x$centroid$lat))
    lon <- suppressWarnings(as.numeric(x$centroid$lon))
  }
  list(lat = lat, lon = lon)
}

# ─────────────────────────────────────────────────────────────────────────────
# MAIN: geocodificar_df (similar a tu geocodificar_df, pero para tu API)
# ─────────────────────────────────────────────────────────────────────────────
#' geocodificar_df_api
#'
#' Geocodifica un data.frame usando tu API /geocode_direccion
#' @param df data.frame con datos de dirección
#' @param calle_col nombre de la columna con el nombre de calle (character)
#' @param altura_col nombre de la columna con la altura (integer/numeric)
#' @param numero_cal_col (opcional) columna con número de calle alternativo
#' @param fallback bool para el parámetro `fallback` del endpoint
#' @param names_prefix prefijo para las columnas devueltas por la API (evita choques)
#' @param return_sf si TRUE, devuelve un sf (requiere paquete sf)
#' @param crs EPSG de salida si return_sf=TRUE (default 4326)
#' @param show_progress mostrar barra de progreso (paquete progress)
#' @return data.frame (o sf) con columnas originales + columnas de la API
geocodificar_df_api <- function(df,
                                calle_col,
                                altura_col,
                                numero_cal_col = NULL,
                                fallback = FALSE,
                                names_prefix = "api",
                                return_sf = FALSE,
                                crs = 4326,
                                show_progress = TRUE) {
  stopifnot(calle_col %in% names(df), altura_col %in% names(df))
  if (!is.null(numero_cal_col)) stopifnot(numero_cal_col %in% names(df))
  
  n <- nrow(df)
  out_list <- vector("list", n)
  
  # Barra de progreso opcional
  if (isTRUE(show_progress) && requireNamespace("progress", quietly = TRUE)) {
    pb <- progress::progress_bar$new(
      total = n, format = "geocodificando [:bar] :current/:total (:percent) eta: :eta"
    )
    tick <- function() pb$tick()
  } else {
    tick <- function() NULL
  }
  
  for (i in seq_len(n)) {
    calle_i  <- df[[calle_col]][i]
    altura_i <- df[[altura_col]][i]
    numero_i <- if (!is.null(numero_cal_col)) df[[numero_cal_col]][i] else NULL
    
    # Salteá filas sin datos mínimos
    if (is.na(calle_i) || is.na(altura_i) || is.null(calle_i) || is.null(altura_i) || calle_i == "") {
      out_list[[i]] <- NULL
      tick(); next
    }
    
    # Llamada segura
    resp <- try(
      geocode_direccion(calle = calle_i, altura = as.integer(altura_i), numero_cal = numero_i, fallback = fallback),
      silent = TRUE
    )
    if (inherits(resp, "try-error")) {
      out_list[[i]] <- NULL
      tick(); next
    }
    
    # Parseo robusto
    coords <- .extract_latlon(resp)
    # guardo también todo el payload por si querés más campos
    out_list[[i]] <- c(resp, coords)
    tick()
  }
  
  # Convertir lista en data.frame *evitando* list-columns problemáticas
  # jsonlite::flatten ayuda si las respuestas tienen anidamientos simples
  flat <- lapply(out_list, function(x) {
    if (is.null(x)) return(data.frame(lat = NA_real_, lon = NA_real_))
    # Aplana: centroid.lat/centroid.lon -> columnas
    as.data.frame(jsonlite::flatten(x), stringsAsFactors = FALSE)
  })
  
  # Rellenar a mismo esquema (bind_rows maneja nombres dispares)
  api_df <- do.call(dplyr::bind_rows, flat)
  
  # Prefijo para no chocar con columnas originales
  if (!is.null(names_prefix) && nzchar(names_prefix)) {
    names(api_df) <- paste0(names_prefix, "_", names(api_df))
  }
  
  # Unir con df original
  df_out <- cbind(df, api_df)
  
  # Si pedís sf, lo construyo desde las columnas lat/lon con prefijo
  if (isTRUE(return_sf)) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("El paquete 'sf' es necesario para return_sf=TRUE.")
    }
    # Detecto nombres de lat/lon luego del prefijo
    lat_name <- paste0(names_prefix, "_lat")
    lon_name <- paste0(names_prefix, "_lon")
    # Fallback: si vinieron como centroid.lat/lon (aplanado)
    if (!lat_name %in% names(df_out)) lat_name <- paste0(names_prefix, "_centroid.lat")
    if (!lon_name %in% names(df_out)) lon_name <- paste0(names_prefix, "_centroid.lon")
    
    # coerción segura
    if (lat_name %in% names(df_out)) df_out[[lat_name]] <- suppressWarnings(as.numeric(df_out[[lat_name]]))
    if (lon_name %in% names(df_out)) df_out[[lon_name]] <- suppressWarnings(as.numeric(df_out[[lon_name]]))
    
    # Filtrar filas sin coords y convertir
    ok <- is.finite(df_out[[lat_name]]) & is.finite(df_out[[lon_name]])
    sf_obj <- sf::st_as_sf(df_out[ok, ], coords = c(lon_name, lat_name), crs = crs)
    
    return(sf_obj)
  }
  
  df_out
}
