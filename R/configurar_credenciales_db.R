#' Configura credenciales y conexi\u00f3n para una base de datos
#'
#' Guarda usuario/contrase\u00f1a en el keyring y opcionalmente escribe/reemplaza la configuraci\u00f3n
#' en config.yml. Valida que no se guarden credenciales vac\u00edas.
#'
#' @param nombre_servicio Nombre del servicio (ej. "SICO").
#' @param db_type (Opcional) Tipo de BD ("sqlserver", "postgres", "mysql", "oracle").
#' @param driver (Opcional) Nombre exacto del driver ODBC. Obligatorio si db_type es
#'   "sqlserver", o si db_type es "oracle" con driver_type = "odbc".
#' @param driver_type (Opcional) Para Oracle: "jdbc" (usa RJDBC descargando el driver
#'   oficial de Oracle a una cach\u00e9 local, sin necesidad de instalar nada m\u00e1s all\u00e1 de una
#'   JVM) u "odbc" (requiere un driver ODBC de Oracle ya instalado en el sistema).
#'   Obligatorio si db_type es "oracle".
#' @param server (Opcional) IP o Host.
#' @param database (Opcional) Nombre de la base de datos o Service Name.
#' @param port (Opcional) Puerto.
#' @param file Ruta al archivo (default: "config.yml").
#' @export
#' @importFrom yaml yaml.load_file as.yaml write_yaml
configurar_credenciales_db <- function(nombre_servicio,
                                       db_type = NULL,
                                       driver = NULL,
                                       driver_type = NULL,
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

    if (tolower(db_type) == "sqlserver") {
      # Validaci\u00f3n estricta (Fail-Fast) para obligar a usar el driver ODBC
      if (is.null(driver)) {
        stop(sprintf("El par\u00e1metro 'driver' es obligatorio para el tipo de base de datos '%s'.", db_type))
      }
    }

    if (tolower(db_type) == "oracle") {
      # Sin default: hay que elegir expl\u00edcitamente el mecanismo de conexi\u00f3n.
      if (is.null(driver_type) || !tolower(driver_type) %in% c("jdbc", "odbc")) {
        stop("El par\u00e1metro 'driver_type' es obligatorio para Oracle y debe ser 'jdbc' u 'odbc'.")
      }
      if (tolower(driver_type) == "odbc" && is.null(driver)) {
        stop("El par\u00e1metro 'driver' es obligatorio para Oracle cuando driver_type = 'odbc'.")
      }
    }
  }

  message(sprintf("--- Configurando: %s ---", nombre_servicio))
  credenciales_ok <- FALSE

  tryCatch({
    message("1. Usuario (db_user):")
    .guardar_credencial(service = nombre_servicio, username = "db_user")
    if (nchar(.leer_credencial(service = nombre_servicio, username = "db_user")) == 0) stop("El usuario no puede estar vac\u00edo.")

    message("2. Contrase\u00f1a (db_pwd):")
    .guardar_credencial(service = nombre_servicio, username = "db_pwd")
    if (nchar(.leer_credencial(service = nombre_servicio, username = "db_pwd")) == 0) stop("La contrase\u00f1a no puede estar vac\u00eda.")

    message("[OK] Credenciales guardadas y verificadas en el sistema.")
    credenciales_ok <- TRUE

  }, error = function(e) {
    message(sprintf("[!] Proceso interrumpido: %s", conditionMessage(e)))
  })

  if (!credenciales_ok) return(invisible(NULL))

  # GESTI\u00d3N DE YAML
  if (!is.null(db_type) && !is.null(server) && !is.null(database)) {

    if (!file.exists(file)) {
      yaml::write_yaml(list(default = list()), file)
      message(sprintf("[Info] Se cre\u00f3 el archivo '%s'.", file))
    }

    config_actual <- .leer_config_bruto(file)
    if (is.null(config_actual$default)) config_actual$default <- list()

    if (!is.null(config_actual[[nombre_servicio]])) {
      respuesta <- tolower(readline(prompt = sprintf("El servicio '%s' ya existe en %s. \u00bfDeseas reemplazarlo? (s/n): ", nombre_servicio, file)))
      if (respuesta != "s") {
        message("[Info] Operaci\u00f3n cancelada. No se modific\u00f3 el YAML.")
        return(invisible(NULL))
      }
    }

    config_actual[[nombre_servicio]] <- .construir_bloque_servicio(
      nombre_servicio, db_type, driver, driver_type, server, database, port
    )
    .escribir_config(config_actual, file)
    message(sprintf("[OK] Configuraci\u00f3n actualizada en %s.", file))

  } else {
    message("\n[Info] Credenciales actualizadas correctamente. Faltan par\u00e1metros para modificar el YAML.")
  }
}

# Wrappers delgados sobre keyring: permiten mockearlos en los tests sin
# tocar el namespace de keyring (ver testthat::local_mocked_bindings).
.guardar_credencial <- function(service, username) {
  keyring::key_set(service = service, username = username)
}

.leer_credencial <- function(service, username) {
  keyring::key_get(service = service, username = username)
}

# Stubs para poder mockear funciones base en los tests (ver
# testthat::local_mocked_bindings: para mockear una funci\u00f3n base hace falta
# tener ya un binding con ese nombre en el namespace del paquete). No cambia
# el comportamiento real: al llamarla, R busca el binding m\u00e1s cercano que
# sea funci\u00f3n y salta los que no lo son, as\u00ed que en uso normal esto sigue
# resolviendo a base::interactive/readline.
interactive <- NULL
readline <- NULL

# Arma la lista (R, no texto) con los datos de un servicio para el YAML de
# configuraci\u00f3n. user/pwd quedan marcados como "verbatim_expr" para que
# .escribir_config() los emita como tags `!expr` sin comillas, que es lo que
# config::get() necesita para evaluarlos en vez de tratarlos como texto.
.construir_bloque_servicio <- function(nombre_servicio, db_type, driver, driver_type, server, database, port) {
  bloque <- list(db_type = db_type)

  if (tolower(db_type) == "oracle") bloque$driver_type <- tolower(driver_type)
  if (!is.null(driver)) bloque$driver <- driver

  bloque$server <- server
  bloque$database <- database

  if (!is.null(port)) bloque$port <- as.integer(port)

  bloque$user <- structure(
    sprintf('keyring::key_get(service = "%s", username = "db_user")', nombre_servicio),
    class = "verbatim_expr"
  )
  bloque$pwd <- structure(
    sprintf('keyring::key_get(service = "%s", username = "db_pwd")', nombre_servicio),
    class = "verbatim_expr"
  )

  bloque
}

# Lee config.yml preservando las etiquetas `!expr` como texto crudo (no las
# eval\u00faa), para poder reescribir el archivo sin ejecutar el c\u00f3digo que
# contienen.
.leer_config_bruto <- function(path) {
  yaml::yaml.load_file(
    path,
    eval.expr = FALSE,
    handlers = list(expr = function(x) structure(x, class = "verbatim_expr"))
  )
}

# Serializa la config de vuelta a YAML real, restituyendo las etiquetas
# `!expr` a partir de los objetos marcados como "verbatim_expr".
.escribir_config <- function(config, path) {
  yml <- yaml::as.yaml(config, handlers = list(
    verbatim_expr = function(v) structure(v, class = "verbatim", tag = "!expr")
  ))
  writeLines(yml, path)
}