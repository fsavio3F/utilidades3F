#' autenticar_geoportal
#'
#' Inicia sesión en el GeoPortal de Tres de Febrero mediante OAuth2 y devuelve un token de acceso válido.
#'
#' @param client_id ID del cliente OAuth2. Si no está en `.Renviron`, se puede pasar como argumento.
#' @param client_secret Clave secreta OAuth2. Si no está en `.Renviron`, se puede pasar como argumento.
#' @param guardar Si TRUE, guarda el token y también las credenciales en `.Renviron` si no están.
#' @param usar_cache Si TRUE, intenta reutilizar el token guardado si tiene menos de 12 horas.
#' @param ignorar_SSL Si TRUE, ignora los errores SSL (por ejemplo, certificados vencidos o auto-firmados).
#' @return Un objeto de clase `Token2.0` con credenciales OAuth2.
#' @export
autenticar_geoportal <- function(client_id = NULL,
                                 client_secret = NULL,
                                 guardar = TRUE,
                                 usar_cache = TRUE,
                                 ignorar_SSL = FALSE) {
  
  # 🧩 Leer credenciales desde entorno o argumentos
  env_client_id <- Sys.getenv("GEOPORTAL3F_CLIENT_ID", unset = NA)
  env_client_secret <- Sys.getenv("GEOPORTAL3F_CLIENT_SECRET", unset = NA)
  
  if (!is.na(env_client_id) && !is.na(env_client_secret) &&
      nzchar(env_client_id) && nzchar(env_client_secret)) {
    client_id <- env_client_id
    client_secret <- env_client_secret
  } else if (!is.null(client_id) && !is.null(client_secret) &&
             nzchar(client_id) && nzchar(client_secret)) {
    if (guardar) {
      renv_path <- path.expand("~/.Renviron")
      if (file.exists(renv_path)) {
        renv_lines <- readLines(renv_path)
        renv_lines <- renv_lines[!grepl("^GEOPORTAL3F_CLIENT_", renv_lines)]
      } else {
        renv_lines <- character()
      }
      nuevas_lineas <- c(
        paste0("GEOPORTAL3F_CLIENT_ID=", client_id),
        paste0("GEOPORTAL3F_CLIENT_SECRET=", client_secret)
      )
      writeLines(c(renv_lines, nuevas_lineas), renv_path)
      message("✅ Credenciales guardadas en .Renviron.")
      readRenviron(renv_path)
    }
  } else {
    stop("❌ No se encontraron credenciales en el entorno ni se pasaron como argumentos.")
  }
  
  # 🗃️ Preparar cache del token
  token_dir <- tools::R_user_dir("geoportal3f", which = "cache")
  if (!dir.exists(token_dir)) dir.create(token_dir, recursive = TRUE)
  cache_path <- file.path(token_dir, "token_geoportal3F.rds")
  
  # ⏱️ Reutilizar token si sigue vigente (<12 horas)
  if (usar_cache && file.exists(cache_path)) {
    mod_time <- file.info(cache_path)$mtime
    age_hours <- difftime(Sys.time(), mod_time, units = "hours")
    if (age_hours < 12) {
      token <- tryCatch(readRDS(cache_path), error = function(e) NULL)
      if (!is.null(token)) {
        message("🕒 Token cacheado reutilizado (edad: ", round(age_hours, 1), " horas).")
        return(token)
      }
    } else {
      message("⏳ Token expirado (>12h). Reautenticando...")
    }
  }
  
  # ⚙️ Configurar SSL si ignorar_SSL = TRUE
  if (ignorar_SSL) {
    httr::set_config(httr::config(ssl_verifypeer = FALSE))
    message("⚠️ Advertencia: SSL deshabilitado temporalmente (ignorar_SSL = TRUE).")
  }
  
  # 🔐 OAuth2
  endpoint <- httr::oauth_endpoint(
    authorize = "https://geoportal.tresdefebrero.gob.ar/o/authorize/",
    access    = "https://geoportal.tresdefebrero.gob.ar/o/token/"
  )
  
  app <- httr::oauth_app(
    appname = "geoportal3f",
    key = client_id,
    secret = client_secret,
    redirect_uri = "http://localhost:1410/"
  )
  
  token <- tryCatch({
    httr::oauth2.0_token(
      endpoint = endpoint,
      app = app,
      scope = "openid write",
      use_basic_auth = TRUE,
      cache = FALSE
    )
  }, error = function(e) {
    stop("❌ Error durante la autenticación OAuth2: ", e$message)
  })
  
  # 💾 Guardar token si corresponde
  if (guardar) {
    saveRDS(token, cache_path)
    message("🔐 Token guardado en: ", cache_path)
  }
  
  # ✅ Restaurar configuración SSL por seguridad
  if (ignorar_SSL) {
    httr::reset_config()
  }
  
  return(token)
}
