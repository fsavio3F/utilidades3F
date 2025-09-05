# plumber.R
library(plumber)
library(DBI)
library(RPostgres)
library(pool)
library(jsonlite)

#* @apiTitle Geolocalizador Intersecciones & Alturas
#* @apiVersion 1.0.0

.db_pool <- NULL
get_db_pool <- function() {
  if (is.null(.db_pool)) {
    .db_pool <<- dbPool(
      drv = RPostgres::Postgres(),
      host = Sys.getenv("PGHOST", "localhost"),
      port = as.integer(Sys.getenv("PGPORT", "5432")),
      dbname = Sys.getenv("PGDB", "postgres"),
      user = Sys.getenv("PGUSER", "postgres"),
      password = Sys.getenv("PGPASSWORD", ""),
      bigint = "integer"
    )
  }
  .db_pool
}

close_db_pool <- function() {
  if (!is.null(.db_pool)) {
    try(pool::poolClose(.db_pool), silent = TRUE)
    .db_pool <<- NULL
  }
}

#* Health
#* @get /health
function(){
  list(status = "ok", time = as.character(Sys.time()))
}

#* Sugerencias
#* @param q Texto a buscar (min 2)
#* @param localidad Filtrar por localidad (opcional)
#* @get /sugerencias
function(q = "", localidad = NULL){
  if (nchar(q) < 2) return(list(results = list()))
  con <- get_db_pool()
  res <- dbGetQuery(con, "SELECT * FROM suggest_calles($1, $2)", params = list(q, localidad))
  list(results = res)
}

#* Geocode por altura
#* @param calle
#* @param altura:int
#* @param localidad
#* @get /geocode/altura
function(calle, altura, localidad = NULL){
  if (missing(calle) || missing(altura)) return(list(error = "Parámetros requeridos: calle, altura"))
  con <- get_db_pool()
  altura <- as.integer(altura)
  res <- dbGetQuery(con, "SELECT * FROM geocode_altura($1, $2, $3)", params = list(calle, altura, localidad))
  if (nrow(res) == 0) return(list(error = "No encontrada"))
  res[1,]
}

#* Intersección N calles (2+). Acepta ?calle=A&calle=B o CSV ?calle=A,B
#* @param calle
#* @param localidad
#* @get /geocode/interseccion
function(calle, localidad = NULL){
  if (missing(calle)) return(list(error = "Proveé al menos dos valores 'calle'"))
  calles <- if (length(calle) == 1 && grepl(",", calle)) trimws(strsplit(calle, ",")[[1]]) else as.character(calle)
  if (length(calles) < 2) return(list(error = "Proveé al menos dos calles"))
  con <- get_db_pool()
  res <- dbGetQuery(con, "SELECT * FROM geocode_interseccion($1::text[], $2)", params = list(calles, localidad))
  if (nrow(res) == 0) return(list(error = "Sin coincidencias para esas calles"))
  res
}
