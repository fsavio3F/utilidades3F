#' Configura las credenciales para un servicio de base de datos en el keyring
#'
#' Esta función solicita interactivamente al usuario que guarde el servidor,
#' el usuario y la contraseña de la BD para un servicio específico.
#'
#' @param nombre_servicio El nombre del servicio (ej. "SSO_vista")
#'
#' @export 
configurar_credenciales_db <- function(nombre_servicio) {
  
  # Verificar que el paquete 'keyring' esté instalado
  if (!requireNamespace("keyring", quietly = TRUE)) {
    stop("El paquete 'keyring' es necesario. Por favor, instálalo con install.packages('keyring')")
  }
  
  message(paste("--- Configurando el servicio:", nombre_servicio, "---"))
  
  # --- 1. Guardar el Servidor/IP ---
  message("\n1. Guardando el SERVIDOR (IP/Hostname)...")
  message("Por favor, introduce el valor para 'db_server' (ej. 192.168.11.32)")
  keyring::key_set(
    service = nombre_servicio,
    username = "db_server" # Etiqueta para el servidor
  )
  message("✅ Servidor guardado.")
  
  # --- 2. Guardar el Usuario de la BD ---
  message("\n2. Guardando el USUARIO de la BD...")
  message("Por favor, introduce el valor para 'db_user' (ej. UsuarioVistaSSO)")
  keyring::key_set(
    service = nombre_servicio,
    username = "db_user" # Etiqueta para el usuario
  )
  message("✅ Usuario guardado.")
  
  # --- 3. Guardar la Contraseña ---
  message("\n3. Guardando la CONTRASEÑA de la BD...")
  message("Por favor, introduce el valor para 'db_pwd'")
  keyring::key_set(
    service = nombre_servicio,
    username = "db_pwd" # Etiqueta para la contraseña
  )
  message("✅ Contraseña guardada.")
  
  message(paste("\n¡Configuración completada para el servicio '", nombre_servicio, "'!", sep=""))
}