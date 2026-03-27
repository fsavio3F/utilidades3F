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
    
    # 1. Definir el objeto Driver de R (drv)
    drv <- switch(tipo_db,
      "postgres"  = RPostgres::Postgres(),
      "mysql"     = RMariaDB::MariaDB(),
      "mariadb"   = RMariaDB::MariaDB(),
      "sqlserver" = odbc::odbc(),
      "oracle"    = odbc::odbc(),
      stop(sprintf("Tipo de base de datos no soportado: %s", tipo_db))
    )
    
    # 2. Construir argumentos din\u00e1micamente
    args <- list(drv = drv)
    
    if (tipo_db %in% c("sqlserver", "oracle")) {
      # Validaci\u00f3n y asignaci\u00f3n de driver ODBC
      if (is.null(conf$driver)) {
        driver_sistema <- switch(tipo_db,
          "sqlserver" = "ODBC Driver 17 for SQL Server",
          "oracle"    = "Oracle in instantclient_19_8" # Ajustar a la versi\u00f3n de tu SO
        )
        warning(sprintf("Driver no especificado en %s. Usando default: %s", config_name, driver_sistema))
      } else {
        driver_sistema <- conf$driver
      }
      
      args$Driver <- driver_sistema
      args$UID    <- conf$user
      args$PWD    <- conf$pwd
      
      if (tipo_db == "sqlserver") {
        args$Server   <- conf$server
        args$Database <- conf$database
        if (!is.null(conf$port)) args$Port <- conf$port
        
      } else if (tipo_db == "oracle") {
        # Oracle ODBC usa sintaxis EZConnect en el par\u00e1metro DBQ
        puerto <- if (!is.null(conf$port)) conf$port else "1521"
        args$DBQ <- paste0(conf$server, ":", puerto, "/", conf$database)
      }
      
    } else {
      # Argumentos est\u00e1ndar DBI (Postgres/MySQL)
      args$host     <- conf$server
      args$dbname   <- conf$database
      args$user     <- conf$user
      args$password <- conf$pwd
      if (!is.null(conf$port)) args$port <- conf$port
    }
    
    # 3. Conectar
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