#' autenticar_geoportal
#'
#' Inicia sesión en el GeoPortal de Tres de Febrero mediante OAuth2 y devuelve un token de acceso válido.
#'
#' @param client_id ID del cliente OAuth2 registrado en el geoportal. No es necesario si el token ya fue guardado previamente.
#' @param client_secret Clave secreta del cliente OAuth2. No es necesario si el token ya fue guardado previamente.
#' @param guardar Si TRUE, guarda el token en un directorio persistente del usuario y lo reutiliza en sesiones siguientes.
#' @return Un objeto de clase `Token2.0` con credenciales OAuth2.
#' @export
autenticar_geoportal <- function(client_id = NULL, client_secret = NULL, guardar = TRUE) {
  token_dir <- tools::R_user_dir("geoportal3f", which = "cache")
  if (!dir.exists(token_dir)) dir.create(token_dir, recursive = TRUE)
  cache_path <- file.path(token_dir, "token_geoportal3F.rds")
  
  if (guardar && file.exists(cache_path)) {
    token <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (!is.null(token)) {
      message("🔓 Token cargado desde: ", cache_path)
      return(token)
    } else {
      message("⚠️ Error al leer el token guardado. Se intentará reautenticar.")
    }
  }
  
  if (is.null(client_id) || is.null(client_secret)) {
    stop("❌ No se encontró un token válido y no se proporcionaron client_id y client_secret.")
  }
  
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
