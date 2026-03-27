#' Configura credenciales y conexi\u00f3n para una base de datos
#'
#' Guarda usuario/contrase\u00f1a en el keyring y opcionalmente escribe/reemplaza la configuraci\u00f3n
#' en config.yml. Valida que no se guarden credenciales vac\u00edas.
#'
#' @param nombre_servicio Nombre del servicio (ej. "SICO").
#' @param db_type (Opcional) Tipo de BD ("sqlserver", "postgres", "mysql", "oracle").
#' @param driver (Opcional) Nombre exacto del driver ODBC. Obligatorio si db_type es "oracle" o "sqlserver".
#' @param server (Opcional) IP o Host.
#' @param database (Opcional) Nombre de la base de datos o Service Name.
#' @param port (Opcional) Puerto.
#' @param file Ruta al archivo (default: "config.yml").
#' @export 
configurar_credenciales_db <- function(nombre_servicio, 
                                       db_type = NULL, 
                                       driver = NULL,
                                       server = NULL, 
                                       database = NULL, 
                                       port = NULL,
                                       file = "config.yml") {
  
  if (!requireNamespace("keyring", quietly = TRUE)) stop("El paquete 'keyring' es necesario.")
  
  if (!interactive()) stop("Esta funci\u00f3n requiere una sesi\u00f3n interactiva para capturar credenciales de forma segura.")

  if (!is.null(db_type)) {
    tipos_validos <- c("sqlserver", "postgres", "mysql", "oracle")
    if (!tolower(db_type) %in% tipos_validos) {
      stop(sprintf("Error: db_type '%s' no es v\u00e1lido.\nOpciones permitidas: %s", 
                   db_type, paste(tipos_validos, collapse = ", ")))
    }
    
    # Validaci\u00f3n estricta (Fail-Fast) para obligar a usar el driver en motores ODBC
    if (tolower(db_type) %in% c("oracle", "sqlserver") && is.null(driver)) {
      stop(sprintf("El par\u00e1metro 'driver' es obligatorio para el tipo de base de datos '%s'.", db_type))
    }
  }
  
  message(sprintf("--- Configurando: %s ---", nombre_servicio))
  credenciales_ok <- FALSE
  
  tryCatch({
    message("1. Usuario (db_user):")
    keyring::key_set(service = nombre_servicio, username = "db_user")
    if (nchar(keyring::key_get(service = nombre_servicio, username = "db_user")) == 0) stop("El usuario no puede estar vac\u00edo.")
    
    message("2. Contrase\u00f1a (db_pwd):")
    keyring::key_set(service = nombre_servicio, username = "db_pwd")
    if (nchar(keyring::key_get(service = nombre_servicio, username = "db_pwd")) == 0) stop("La contrase\u00f1a no puede estar vac\u00eda.")
    
    message("[OK] Credenciales guardadas y verificadas en el sistema.")
    credenciales_ok <- TRUE
    
  }, error = function(e) {
    message(sprintf("[!] Proceso interrumpido: %s", conditionMessage(e)))
  })
  
  if (!credenciales_ok) return(invisible(NULL))
  
  # GESTI\u00d3N DE YAML
  if (!is.null(db_type) && !is.null(server) && !is.null(database)) {
    
    if (!file.exists(file)) {
      cat("default:\n", file = file)
      message(sprintf("[Info] Se cre\u00f3 el archivo '%s'.", file))
    }
    
    lineas <- readLines(file, warn = FALSE)
    patron_inicio <- paste0("^", nombre_servicio, "\\s*:")
    idx_start <- grep(patron_inicio, lineas)[1]
    
    if (!is.na(idx_start)) {
      respuesta <- tolower(readline(prompt = sprintf("El servicio '%s' ya existe en %s. \u00bfDeseas reemplazarlo? (s/n): ", nombre_servicio, file)))
      if (respuesta != "s") {
        message("[Info] Operaci\u00f3n cancelada. No se modific\u00f3 el YAML.")
        return(invisible(NULL))
      }
      
      idx_end <- length(lineas)
      if (idx_start < length(lineas)) {
        siguientes_claves <- grep("^[a-zA-Z0-9_-]+\\s*:", lineas[(idx_start + 1):length(lineas)])
        if (length(siguientes_claves) > 0) {
          idx_end <- idx_start + siguientes_claves[1] - 1
        }
      }
      
      lineas <- lineas[-(idx_start:idx_end)]
      writeLines(lineas, file)
      message("[Info] Entrada anterior eliminada.")
    }
    
    # Construcci\u00f3n del bloque nuevo
    indent <- "  "
    bloque <- c(
      "", 
      paste0(nombre_servicio, ":"),
      paste0(indent, 'db_type: "', db_type, '"')
    )
    
    # Se inyecta el driver si existe
    if (!is.null(driver)) bloque <- c(bloque, paste0(indent, 'driver: "', driver, '"'))
    
    bloque <- c(bloque,
      paste0(indent, 'server: ', server),
      paste0(indent, 'database: "', database, '"')
    )
    
    if (!is.null(port)) bloque <- c(bloque, paste0(indent, 'port: ', port))
    
    bloque <- c(bloque, 
      paste0(indent, 'user: !expr keyring::key_get(service = "', nombre_servicio, '", username = "db_user")'),
      paste0(indent, 'pwd: !expr keyring::key_get(service = "', nombre_servicio, '", username = "db_pwd")')
    )
    
    cat(paste(bloque, collapse = "\n"), "\n", file = file, append = TRUE)
    message(sprintf("[OK] Configuraci\u00f3n actualizada en %s.", file))
    
  } else {
    message("\n[Info] Credenciales actualizadas correctamente. Faltan par\u00e1metros para modificar el YAML.")
  }
}