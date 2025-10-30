#' Crea una o más conexiones a bases de datos
#'
#' Esta función toma uno o más nombres de configuración y devuelve
#' una lista nombrada de conexiones DBI.
#'
#' @param configs Un vector de caracteres con los nombres de las
#'   configuraciones a cargar (ej: `"SSO_vista"` o `c("SSO_vista", "otra_db")`).
#' @param file La ruta al archivo de configuración. Por defecto, busca
#'   un archivo "config.yml" en el directorio de trabajo actual.
#'
#' @return Una lista nombrada, donde cada elemento es un objeto `DBIConnection`.
#'
#' @examples
#' \dontrun{
#'   # --- Ejemplo con una sola conexión ---
#'   con <- crear_conexiones("produccion")
#'   # Se accede así:
#'   DBI::dbListTables(con$produccion)
#'
#'   # --- Ejemplo con múltiples conexiones ---
#'   conns <- crear_conexiones(
#'     configs = c("produccion", "testing"),
#'     file = "D:/Mis cosas/Credenciales/config.yml"
#'   )
#'   # Se accede de la misma forma:
#'   DBI::dbListTables(conns$produccion)
#'   DBI::dbListTables(conns$testing)
#' }
#'
#' @export
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
#' @importFrom RMariaDB MariaDB
#' @importFrom config get
crear_conexiones <- function(configs, file = "config.yml") {
  
  # La función interna para manejar una sola config
  .crear_una_conexion <- function(config_name, file_path) {
    conf <- config::get(config = config_name, file = file_path)
    
    driver <- switch(conf$db_type,
      "postgres" = Postgres(),
      "mysql"    = MariaDB(),
      stop(paste("db_type desconocido para", config_name)) 
    )
    
    con <- dbConnect(
      driver,
      host = conf$server,
      port = conf$port,
      dbname = conf$database,
      user = conf$user,
      password = conf$pwd
    )
    return(con)
  }
  
  # 1. Usamos lapply para iterar sobre el vector 'configs'.
  #    Esto funciona igual de bien para 1 o N elementos.
  conns <- lapply(configs, .crear_una_conexion, file_path = file)
  
  # 2. Ponemos los nombres para acceder fácilmente
  names(conns) <- configs
  
  return(conns)
}