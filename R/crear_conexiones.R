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
      "sqlserver" = odbc::odbc()
    )
    
    # 2. Construir argumentos dinámicamente
    args <- list(drv = drv)
    
    if (tipo_db == "sqlserver") {
      # Si el YAML no tiene 'driver', usamos un default común para Windows/Linux.
      # Esto permite que config.yml quede limpio.
      driver_sistema <- if (!is.null(conf$driver)) conf$driver else "ODBC Driver 17 for SQL Server"
      
      args$Driver   <- driver_sistema
      args$Server   <- conf$server
      args$Database <- conf$database
      args$UID      <- conf$user
      args$PWD      <- conf$pwd
      
      # Puerto opcional (SQL Server usa 1433 por defecto si no se pasa nada)
      if (!is.null(conf$port)) args$Port <- conf$port
      
    } else {
      # Argumentos estándar DBI (Postgres/MySQL)
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
      stop(paste0("Error conectando a '", config_name, "': ", e$message))
    })
  }
  
  conns <- lapply(configs, .crear_una_conexion, file_path = file)
  names(conns) <- configs
  return(conns)
}