#' Crea una o más conexiones a bases de datos
#'
#' Esta función toma uno o más nombres de configuración y devuelve
#' una lista nombrada de conexiones DBI.
#'
#' @param configs Un vector de caracteres con los nombres de las
#'   configuraciones a cargar.
#' @param file La ruta al archivo de configuración. Por defecto "config.yml".
#'
#' @return Una lista nombrada, donde cada elemento es un objeto `DBIConnection`.
#' @export
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
#' @importFrom RMariaDB MariaDB
#' @importFrom odbc odbc
#' @importFrom config get
crear_conexiones <- function(configs, file = "config.yml") {

  .crear_una_conexion <- function(config_name, file_path) {
    conf <- config::get(config = config_name, file = file_path)
    tipo_db <- tolower(conf$db_type)

    args <- .construir_args_conexion(conf, tipo_db, config_name)

    tryCatch({
      do.call(.conectar_db, args)
    }, error = function(e) {
      stop(sprintf("Error conectando a '%s': %s", config_name, e$message))
    })
  }

  conns <- lapply(configs, .crear_una_conexion, file_path = file)
  names(conns) <- configs
  return(conns)
}

# Wrapper delgado sobre DBI::dbConnect: permite mockearlo en los tests sin
# tocar el namespace de DBI (ver testthat::local_mocked_bindings).
.conectar_db <- function(...) {
  DBI::dbConnect(...)
}

# Arma la lista de argumentos para dbConnect según el motor de base de datos.
# No llama a config::get() ni abre conexión real, así que cada rama se
# puede testear de forma pura.
.construir_args_conexion <- function(conf, tipo_db, config_name = tipo_db) {

  if (tipo_db == "oracle") {
    return(.construir_args_oracle(conf, config_name))
  }

  drv <- switch(tipo_db,
    "postgres"  = RPostgres::Postgres(),
    "mysql"     = RMariaDB::MariaDB(),
    "mariadb"   = RMariaDB::MariaDB(),
    "sqlserver" = odbc::odbc(),
    stop(sprintf("Tipo de base de datos no soportado en '%s': %s", config_name, tipo_db))
  )

  args <- list(drv = drv)

  if (tipo_db == "sqlserver") {

    # Validación estricta: No hay default para Oracle (ver .construir_args_oracle).
    if (is.null(conf$driver)) {
      # Mantenemos el fallback para SQL Server solo como conveniencia heredada, avisando al usuario.
      driver_sistema <- "ODBC Driver 17 for SQL Server"
      warning(sprintf("Driver no especificado en %s. Usando default: %s", config_name, driver_sistema))
      args$Driver <- driver_sistema
    } else {
      args$Driver <- conf$driver
    }

    args$UID      <- conf$user
    args$PWD      <- conf$pwd
    args$Server   <- conf$server
    args$Database <- conf$database
    if (!is.null(conf$port)) args$Port <- conf$port

  } else {
    # Motores nativos (Postgres/MySQL)
    args$host     <- conf$server
    args$dbname   <- conf$database
    args$user     <- conf$user
    args$password <- conf$pwd
    if (!is.null(conf$port)) args$port <- conf$port
  }

  args
}

# Oracle admite dos mecanismos de conexión, elegidos explícitamente por
# `driver_type` en el YAML (sin default: ver validación fail-fast abajo):
# - "jdbc": vía RJDBC, descargando y cacheando el driver oficial de Oracle
#   (ojdbc8.jar, publicado en Maven Central) en vez de exigir copiarlo a mano
#   en cada proyecto. Requiere una JVM instalada (dependencia de rJava/RJDBC).
# - "odbc": vía el paquete odbc, contra un driver ODBC de Oracle ya
#   instalado y registrado en el sistema operativo (comportamiento histórico).
.construir_args_oracle <- function(conf, config_name) {
  driver_type <- if (is.null(conf$driver_type)) "" else tolower(conf$driver_type)

  if (!driver_type %in% c("jdbc", "odbc")) {
    stop(sprintf(
      "Error en '%s': el par\u00e1metro 'driver_type' es obligatorio para Oracle y debe ser 'jdbc' u 'odbc'.",
      config_name
    ))
  }

  puerto <- if (!is.null(conf$port)) conf$port else "1521"

  if (driver_type == "jdbc") {
    drv <- .crear_driver_oracle_jdbc(.obtener_jar_oracle_jdbc())
    url <- paste0("jdbc:oracle:thin:@//", conf$server, ":", puerto, "/", conf$database)
    return(list(drv = drv, url = url, user = conf$user, password = conf$pwd))
  }

  # driver_type == "odbc"
  # Ojo: usamos [[..., exact = TRUE]] porque `conf$driver` hace partial
  # matching y, al existir también `driver_type` en la misma lista, resuelve
  # (mal) al valor de `driver_type` cuando `driver` no está presente.
  driver_odbc <- conf[["driver", exact = TRUE]]
  if (is.null(driver_odbc)) {
    stop(sprintf(
      "Error en '%s': el par\u00e1metro 'driver' es obligatorio para Oracle cuando driver_type = 'odbc'.",
      config_name
    ))
  }

  list(
    drv    = odbc::odbc(),
    Driver = driver_odbc,
    UID    = conf$user,
    PWD    = conf$pwd,
    # Sintaxis EZConnect requerida por el driver ODBC de Oracle
    DBQ    = paste0(conf$server, ":", puerto, "/", conf$database)
  )
}

# Descarga (una única vez por máquina) y cachea el driver JDBC oficial de
# Oracle en la carpeta de caché de usuario de R, para no depender de copiar
# el .jar a mano en cada proyecto. Oracle publica ojdbc8 en Maven Central con
# licencia de uso libre desde hace varias versiones (sin click-through OTN).
.obtener_jar_oracle_jdbc <- function(version = "23.8.0.25.04") {
  nombre_jar <- sprintf("ojdbc8-%s.jar", version)
  jar_path   <- file.path(.dir_cache_utilidades3F(), nombre_jar)

  if (!file.exists(jar_path)) {
    dir.create(dirname(jar_path), recursive = TRUE, showWarnings = FALSE)
    message(sprintf("Descargando driver JDBC de Oracle (%s) a la cach\u00e9 local...", nombre_jar))
    url_jar <- sprintf(
      "https://repo1.maven.org/maven2/com/oracle/database/jdbc/ojdbc8/%s/ojdbc8-%s.jar",
      version, version
    )
    .descargar_jar(url_jar, jar_path)
  }

  jar_path
}

# Wrapper delgado sobre download.file: permite mockearlo en los tests sin
# hacer descargas de red reales (ver testthat::local_mocked_bindings).
.descargar_jar <- function(url, destfile) {
  utils::download.file(url, destfile = destfile, mode = "wb", quiet = TRUE)
}

# Wrapper delgado sobre RJDBC::JDBC: permite mockearlo en los tests sin
# necesitar una JVM real ni un .jar válido de Oracle.
.crear_driver_oracle_jdbc <- function(jar_path) {
  RJDBC::JDBC("oracle.jdbc.OracleDriver", classPath = jar_path)
}

# Wrapper delgado sobre tools::R_user_dir: permite mockear la carpeta de
# caché en los tests sin escribir en la caché real de usuario.
.dir_cache_utilidades3F <- function() {
  tools::R_user_dir("utilidades3F", which = "cache")
}
