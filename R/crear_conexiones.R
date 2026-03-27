#' Crea una o m\u00e1s conexiones a bases de datos
#'
#' Esta funci\u00f3n toma uno o m\u00e1s nombres de configuraci\u00f3n y devuelve
#' una lista nombrada de conexiones DBI.
#'
#' @param configs Un vector de caracteres con los nombres de las
#'   configuraciones a cargar.
#' @param file La ruta al archivo de configuraci\u00f3n. Por defecto "config.yml".
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
    
    drv <- switch(tipo_db,
      "postgres"  = RPostgres::Postgres(),
      "mysql"     = RMariaDB::MariaDB(),
      "mariadb"   = RMariaDB::MariaDB(),
      "sqlserver" = odbc::odbc(),
      "oracle"    = odbc::odbc(),
      stop(sprintf("Tipo de base de datos no soportado en '%s': %s", config_name, tipo_db))
    )
    
    args <- list(drv = drv)
    
    if (tipo_db %in% c("sqlserver", "oracle")) {
      
      # Validaci\u00f3n estricta: No hay default para Oracle.
      if (is.null(conf$driver)) {
        if (tipo_db == "oracle") {
          stop(sprintf("Error en '%s': El par\u00e1metro 'driver' es obligatorio en el YAML para conexiones Oracle.", config_name))
        } else {
          # Mantenemos el fallback para SQL Server solo como conveniencia heredada, avisando al usuario.
          driver_sistema <- "ODBC Driver 17 for SQL Server"
          warning(sprintf("Driver no especificado en %s. Usando default: %s", config_name, driver_sistema))
          args$Driver <- driver_sistema
        }
      } else {
        args$Driver <- conf$driver
      }
      
      args$UID <- conf$user
      args$PWD <- conf$pwd
      
      if (tipo_db == "sqlserver") {
        args$Server   <- conf$server
        args$Database <- conf$database
        if (!is.null(conf$port)) args$Port <- conf$port
        
      } else if (tipo_db == "oracle") {
        # Sintaxis EZConnect requerida por el driver ODBC de Oracle
        puerto <- if (!is.null(conf$port)) conf$port else "1521"
        args$DBQ <- paste0(conf$server, ":", puerto, "/", conf$database)
      }
      
    } else {
      # Motores nativos (Postgres/MySQL)
      args$host     <- conf$server
      args$dbname   <- conf$database
      args$user     <- conf$user
      args$password <- conf$pwd
      if (!is.null(conf$port)) args$port <- conf$port
    }
    
    tryCatch({
      do.call(DBI::dbConnect, args)
    }, error = function(e) {
      stop(sprintf("Error conectando a '%s': %s", config_name, e$message))
    })
  }
  
  conns <- lapply(configs, .crear_una_conexion, file_path = file)
  names(conns) <- configs
  return(conns)
}