#' Configura credenciales y conexi\u00f3n para una base de datos
#'
#' Guarda usuario/contrase\u00f1a en el keyring y opcionalmente escribe/reemplaza la configuraci\u00f3n
#' en config.yml. Valida que no se guarden credenciales vac\u00edas.
#'
#' @param nombre_servicio Nombre del servicio (ej. "SICO").
#' @param db_type (Opcional) Tipo de BD ("sqlserver", "postgres", "mysql").
#' @param server (Opcional) IP o Host.
#' @param database (Opcional) Nombre de la base de datos.
#' @param port (Opcional) Puerto.
#' @param file Ruta al archivo (default: "config.yml").
#' @export 
configurar_credenciales_db <- function(nombre_servicio, 
                                       db_type = NULL, 
                                       server = NULL, 
                                       database = NULL, 
                                       port = NULL,
                                       file = "config.yml") {
  
  if (!requireNamespace("keyring", quietly = TRUE)) {
    stop("El paquete 'keyring' es necesario.")
  }

  if (!is.null(db_type)) {
    tipos_validos <- c("sqlserver", "postgres", "mysql")
    if (!tolower(db_type) %in% tipos_validos) {
      stop(sprintf(
        "Error: db_type '%s' no es v\u00e1lido.\nOpciones permitidas: %s", 
        db_type, paste(tipos_validos, collapse = ", ")
      ))
    }
  }
  
  message(sprintf("--- Configurando: %s ---", nombre_servicio))
  
  # verificamos que las credenciales sean ingresadas
  credenciales_ok <- FALSE
  
  tryCatch({
    # Usuario
    message("1. Usuario (db_user):")
    keyring::key_set(service = nombre_servicio, username = "db_user")
    
    # Validamos usuario
    test_user <- keyring::key_get(service = nombre_servicio, username = "db_user")
    if (nchar(test_user) == 0) stop("El usuario no puede estar vac\u00edo.")
    
    # Contrase\u00f1a
    message("2. Contrase\u00f1a (db_pwd):")
    keyring::key_set(service = nombre_servicio, username = "db_pwd")
    
    # Validamos contrase\u00f1a
    test_pwd <- keyring::key_get(service = nombre_servicio, username = "db_pwd")
    if (nchar(test_pwd) == 0) stop("La contrase\u00f1a no puede estar vac\u00eda.")
    
    message("[Credenciales guardadas y verificadas.")
    credenciales_ok <- TRUE
    
  }, error = function(e) {
    # si no esta valdiado no cargamos el registro en yaml
    message(sprintf("[!] Proceso interrumpido: %s", conditionMessage(e)))
    message("[!] No se guard\u00f3 ninguna configuraci\u00f3n.")
  })
  
  # Si fallaron las credenciales, CORTAMOS la ejecuci\u00f3n aqu\u00ed.
  if (!credenciales_ok) return(invisible(NULL))
  
  
  # GESTI\u00d3N DE YAML
  # Solo procedemos si hay datos de conexi\u00f3n para escribir
  if (!is.null(db_type) && !is.null(server) && !is.null(database)) {
    
    # Crear cabecera si no existe
    if (!file.exists(file)) {
      cat("default:\n  server:\n", file = file)
      message(sprintf("[Info] Se cre\u00f3 el archivo '%s'.", file))
    }
    
    # Leer el archivo actual para chequear duplicados
    lineas <- readLines(file)
    patron_inicio <- paste0("^", nombre_servicio, ":")
    indices_inicio <- grep(patron_inicio, lineas)
    
    # proceso de reemplazo
    if (length(indices_inicio) > 0) {
      msg <- sprintf("El servicio '%s' ya existe en %s. \u00bfqueres reemplazarlo? (s/n): ", nombre_servicio, file)
      respuesta <- tolower(readline(prompt = msg))
      
      if (respuesta != "s") {
        message("[Info] Cancelaste la operaci\u00f3n. No se modific\u00f3 el YAML.")
        return(invisible(NULL))
      }
      
      # en caso de ser positivo eliminamos la entrada anterior
      idx_start <- indices_inicio[1]
      n_lines <- length(lineas)
      idx_end <- n_lines
      
      # Buscamos d\u00f3nde termina el bloque 
      if (idx_start < n_lines) {
        for (i in (idx_start + 1):n_lines) {
          # Si encontramos una l\u00ednea que empieza con letra/n\u00famero (nueva key), ah\u00ed termina el bloque anterior
          if (grepl("^[a-zA-Z0-9]", lineas[i])) {
            idx_end <- i - 1
            break
          }
        }
      }
      
      # Eliminamos las l\u00edneas viejas de la memoria y reescribimos el archivo "limpio"
      lineas <- lineas[-(idx_start:idx_end)]
      writeLines(lineas, file)
      message("[Info] Entrada anterior eliminada. Reemplazando datos ...")
    }
    
    # Construcci\u00f3n del bloque nuevo
    indent <- "  "
    bloque <- c(
      "", 
      paste0(nombre_servicio, ":"),
      paste0(indent, 'db_type: "', db_type, '"'),
      paste0(indent, 'server: ', server),
      paste0(indent, 'database: "', database, '"')
    )
    
    if (!is.null(port)) {
      bloque <- c(bloque, paste0(indent, 'port: ', port))
    }
    
    # generamos las !expr
    bloque <- c(bloque, 
      paste0(indent, 'user: !expr keyring::key_get(service = "', nombre_servicio, '", username = "db_user")'),
      paste0(indent, 'pwd: !expr keyring::key_get(service = "', nombre_servicio, '", username = "db_pwd")')
    )
    
    # hacemos append al archivo
    con <- file(file, open = "at") # 'at' = append text mode
    tryCatch({
      writeLines(c("", bloque), con)
    }, finally = {
      close(con)
    })
    message(sprintf("[OK] Configuración actualizada en %s.", file))
  } else {
    message("\n[Info] Credenciales actualizadas correctamente. No se modific\u00f3 el YAML (faltan par\u00e1metros de conexi\u00f3n).")
  }
}
