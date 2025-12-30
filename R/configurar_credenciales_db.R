#' Configura las credenciales para un servicio de base de datos en el keyring
#'
#' Esta funci\u00f3n solicita interactivamente al usuario que guarde el servidor,
#' el usuario y la contrase\u00f1a de la BD para un servicio espec\u00edfico.
#'
#' @param nombre_servicio El nombre del servicio (ej. "SSO_vista")
#'
#' @export 
configurar_credenciales_db <- function(nombre_servicio) {
  
  # Verificar que el paquete 'keyring' est\u00e1 instalado
  if (!requireNamespace("keyring", quietly = TRUE)) {
    stop("El paquete 'keyring' es necesario. Por favor, inst\u00e1lalo con install.packages('keyring')")
  }
  
  message(paste("--- Configurando el servicio:", nombre_servicio, "---"))
  
  # --- 1 Guardar el Usuario de la BD ---
  message("\n1. Guardando el USUARIO de la BD...")
  message("Por favor, introduce el valor para 'db_user' (ej. UsuarioVistaSSO)")
  keyring::key_set(
    service = nombre_servicio,
    username = "db_user" # Etiqueta para el usuario
  )
  message("[OK] Usuario guardado.")
  
  # --- 2. Guardar la Contrase\u00f1a ---
  message("\n2. Guardando la CONTRASE\u00d1A de la BD...")
  message("Por favor, introduce el valor para 'db_pwd'")
  keyring::key_set(
    service = nombre_servicio,
    username = "db_pwd" # Etiqueta para la contrase\u00f1a
  )
  message("[OK] Contrase\u00f1a guardada.")
  
  message(paste("\n\u00a1Configuraci\u00f3n completada para el servicio '", nombre_servicio, "'!", sep=""))
}