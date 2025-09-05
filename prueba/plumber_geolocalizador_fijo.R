
# plumber_geolocalizador_fijo.R (anotaciones @param corregidas)
library(plumber)
library(DBI)
library(RPostgres)
library(pool)
library(jsonlite)
library(stringi)

# ---- Config fija (sin variables de entorno) ----
PGHOST    <- "localhost"
PGPORT    <- 5432
PGDB      <- "postgres"
PGUSER    <- "postgres"
PGPASSWORD<- "4840"
PGSCHEMA  <- "public"

.db_pool <- NULL
get_db_pool <- function() {
  if (is.null(.db_pool)) {
    .db_pool <<- dbPool(
      drv = RPostgres::Postgres(),
      host = PGHOST,
      port = PGPORT,
      dbname = PGDB,
      user = PGUSER,
      password = PGPASSWORD,
      bigint = "integer"
    )
    # Opcional: setear search_path
    try(DBI::dbExecute(.db_pool, paste0("SET search_path TO ", PGSCHEMA, ", public")), silent = TRUE)
  }
  .db_pool
}

# ---- Helpers ----
norm_txt <- function(x) stringi::stri_trans_general(trimws(tolower(x)), "Latin-ASCII")
qtbl <- function(tbl) {
  as.character(dbQuoteIdentifier(get_db_pool(), Id(schema = PGSCHEMA, table = tbl)))
}
unaccent_match <- function(col, param_index) {
  sprintf("unaccent(lower(%s)) ILIKE unaccent(lower($%d))", col, param_index)
}

#* @apiTitle Geolocalizador fijo
#* @apiVersion 1.0.2

#* Healthcheck
#* @get /health
function(){
  list(status = "ok", schema = PGSCHEMA, time = as.character(Sys.time()))
}

#* Sugerencias de calles
#* @param q string Texto a buscar
#* @param limit integer Máximo de resultados (1..50, default 20)
#* @get /sugerencias
function(q = "", limit = 20){
  q <- norm_txt(q)
  if (nchar(q) < 2) return(list(items = list(), count = 0))
  limit <- max(1, min(as.integer(limit), 50))
  pool <- get_db_pool()
  sql <- sprintf("
    SELECT DISTINCT numero_cal, nombre_cal
    FROM %s
    WHERE %s
    ORDER BY nombre_cal
    LIMIT $2;
  ", qtbl("callejero_geolocalizador"),
     unaccent_match("nombre_cal", 1))
  res <- dbGetQuery(pool, sql, params = list(paste0('%', q, '%'), limit))
  list(items = res, count = nrow(res))
}

#* Geocodificar por calle + altura
#* @param calle string Nombre de la calle (opcional si envía numero_cal)
#* @param altura integer Altura (entero)
#* @param numero_cal integer Código de calle (desambiguación)
#* @get /geocode_direccion
function(calle = "", altura = NA, numero_cal = NA){
  if (is.na(altura)) return(list(error = "Debe enviar 'altura'."))
  pool <- get_db_pool()

  if (!is.na(numero_cal) && nchar(numero_cal) > 0) {
    filtro_sql <- "numero_cal = $1"
    params <- list(as.integer(numero_cal), as.integer(altura))
  } else if (nchar(calle) > 0) {
    filtro_sql <- unaccent_match("nombre_cal", 1)
    params <- list(paste0('%', norm_txt(calle), '%'), as.integer(altura))
  } else {
    return(list(error = "Debe enviar 'calle' o 'numero_cal'."))
  }

  sql <- sprintf("
    WITH candidatos AS (
      SELECT id, geom, numero_cal, nombre_cal,
             alt_ini_pa, alt_ini_im, alt_fin_pa, alt_fin_im,
             GREATEST(LEAST(alt_ini_pa, alt_fin_pa), 0) AS min_par,
             LEAST(GREATEST(alt_ini_pa, alt_fin_pa), 999999) AS max_par,
             GREATEST(LEAST(alt_ini_im, alt_fin_im), 0) AS min_impar,
             LEAST(GREATEST(alt_ini_im, alt_fin_im), 999999) AS max_impar
      FROM %s
      WHERE %s
    ),
    con_rango AS (
      SELECT *,
             CASE WHEN $2 %% 2 = 0 THEN
               CASE WHEN max_par = min_par THEN 0.5
                    ELSE (LEAST(GREATEST($2, min_par), max_par) - min_par)::float
                         / NULLIF((max_par - min_par)::float, 0)
               END
             ELSE
               CASE WHEN max_impar = min_impar THEN 0.5
                    ELSE (LEAST(GREATEST($2, min_impar), max_impar) - min_impar)::float
                         / NULLIF((max_impar - min_impar)::float, 0)
               END
             END AS t
      FROM candidatos
      WHERE
        ($2 %% 2 = 0 AND $2 BETWEEN LEAST(min_par, max_par) AND GREATEST(min_par, max_par))
        OR
        ($2 %% 2 = 1 AND $2 BETWEEN LEAST(min_impar, max_impar) AND GREATEST(min_impar, max_impar))
    ),
    mejor AS (
      SELECT *, ROW_NUMBER() OVER (
        ORDER BY CASE WHEN $2 %% 2 = 0 THEN (max_par - min_par) ELSE (max_impar - min_impar) END ASC
      ) AS rn
      FROM con_rango
    )
    SELECT id, numero_cal, nombre_cal, $2 AS altura,
           ST_AsText(ST_LineInterpolatePoint(geom, GREATEST(0.0, LEAST(1.0, t)))) AS wkt,
           ST_AsGeoJSON(ST_Transform(ST_LineInterpolatePoint(geom, GREATEST(0.0, LEAST(1.0, t))), 4326)) AS geojson
    FROM mejor
    WHERE rn = 1;
  ", qtbl("callejero_geolocalizador"), filtro_sql)

  res <- dbGetQuery(pool, sql, params = params)
  if (nrow(res) == 0) return(list(success = FALSE, message = "Sin coincidencias en el rango/paridad."))
  list(success = TRUE, result = res[1,], srid = 4326)
}

#* Geocodificar por intersección
#* @param calle1 string Nombre o número de calle 1
#* @param calle2 string Nombre o número de calle 2
#* @get /geocode_interseccion
function(calle1 = "", calle2 = ""){
  if (nchar(calle1) == 0 || nchar(calle2) == 0)
    return(list(error = "Debe enviar 'calle1' y 'calle2'."))
  pool <- get_db_pool()
  is_num1 <- grepl("^[0-9]+$", calle1)
  is_num2 <- grepl("^[0-9]+$", calle2)

  if (is_num1 && is_num2) {
    sql <- sprintf("
      WITH inter AS (
        SELECT id, geom, regexp_split_to_array(regexp_replace(num_calle, '\\s+', '', 'g'), ';')::text[] AS nums
        FROM %s
      )
      SELECT ST_AsGeoJSON(ST_Transform(geom, 4326)) AS geojson
      FROM inter
      WHERE ARRAY[ $1::text, $2::text ] <@ nums
      LIMIT 1;
    ", qtbl("intersecciones_geolocalizador"))
    params <- list(calle1, calle2)
  } else {
    sql <- sprintf("
      WITH inter AS (
        SELECT id, geom, regexp_split_to_array(calles, ';') AS arr_calles
        FROM %s
      )
      SELECT ST_AsGeoJSON(ST_Transform(i.geom, 4326)) AS geojson
      FROM inter i
      WHERE EXISTS (
        SELECT 1 FROM unnest(i.arr_calles) c1 WHERE %s
      )
      AND EXISTS (
        SELECT 1 FROM unnest(i.arr_calles) c2 WHERE %s
      )
      LIMIT 1;
    ", qtbl("intersecciones_geolocalizador"),
       unaccent_match("c1", 1),
       unaccent_match("c2", 2))
    params <- list(paste0('%', norm_txt(calle1), '%'), paste0('%', norm_txt(calle2), '%'))
  }
  res <- dbGetQuery(pool, sql, params = params)
  if (nrow(res) == 0) return(list(success = FALSE, message = "Intersección no encontrada."))
  list(success = TRUE, result = jsonlite::fromJSON(res$geojson[1]), srid = 4326)
}

#* @plumber
function(pr) {
  pr$registerHooks(list(
    exit = function(){
      if (!is.null(.db_pool)) pool::poolClose(.db_pool)
    }
  ))
  pr
}
