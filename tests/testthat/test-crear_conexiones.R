test_that(".construir_args_conexion arma los args correctos para postgres", {
  conf <- list(server = "host1", database = "db1", user = "u", pwd = "p", port = 5432)
  args <- .construir_args_conexion(conf, "postgres")

  expect_s4_class(args$drv, "PqDriver")
  expect_equal(args$host, "host1")
  expect_equal(args$dbname, "db1")
  expect_equal(args$user, "u")
  expect_equal(args$password, "p")
  expect_equal(args$port, 5432)
})

test_that(".construir_args_conexion arma los args correctos para mysql/mariadb", {
  conf <- list(server = "host1", database = "db1", user = "u", pwd = "p")
  args_mysql <- .construir_args_conexion(conf, "mysql")
  args_mariadb <- .construir_args_conexion(conf, "mariadb")

  expect_s4_class(args_mysql$drv, "MariaDBDriver")
  expect_s4_class(args_mariadb$drv, "MariaDBDriver")
  expect_null(args_mysql$port)
})

test_that(".construir_args_conexion usa el driver default para sqlserver sin driver, con warning", {
  conf <- list(server = "host1", database = "db1", user = "u", pwd = "p")

  expect_warning(
    args <- .construir_args_conexion(conf, "sqlserver", "SICO"),
    "Driver no especificado"
  )

  expect_s4_class(args$drv, "OdbcDriver")
  expect_equal(args$Driver, "ODBC Driver 17 for SQL Server")
  expect_equal(args$Server, "host1")
  expect_equal(args$Database, "db1")
  expect_equal(args$UID, "u")
  expect_equal(args$PWD, "p")
})

test_that(".construir_args_conexion respeta el driver custom para sqlserver", {
  conf <- list(server = "host1", database = "db1", user = "u", pwd = "p",
               driver = "Mi Driver Custom", port = 1433)

  args <- expect_no_warning(.construir_args_conexion(conf, "sqlserver", "SICO"))

  expect_equal(args$Driver, "Mi Driver Custom")
  expect_equal(args$Port, 1433)
})

test_that(".construir_args_conexion arma DBQ con EZConnect para oracle/odbc, con puerto default", {
  conf <- list(server = "host1", database = "ORCL", user = "u", pwd = "p",
               driver_type = "odbc", driver = "Oracle in OraClient")

  args <- .construir_args_conexion(conf, "oracle", "SICO")

  expect_s4_class(args$drv, "OdbcDriver")
  expect_equal(args$DBQ, "host1:1521/ORCL")
  expect_equal(args$UID, "u")
  expect_equal(args$PWD, "p")
})

test_that(".construir_args_conexion respeta el puerto custom para oracle/odbc", {
  conf <- list(server = "host1", database = "ORCL", user = "u", pwd = "p",
               driver_type = "odbc", driver = "Oracle in OraClient", port = 1522)

  args <- .construir_args_conexion(conf, "oracle", "SICO")

  expect_equal(args$DBQ, "host1:1522/ORCL")
})

test_that(".construir_args_conexion exige driver para oracle/odbc (fail-fast, sin default)", {
  conf <- list(server = "host1", database = "ORCL", user = "u", pwd = "p", driver_type = "odbc")

  expect_error(
    .construir_args_conexion(conf, "oracle", "SICO"),
    "el parámetro 'driver' es obligatorio"
  )
})

test_that(".construir_args_conexion exige driver_type para oracle (fail-fast, sin default)", {
  conf <- list(server = "host1", database = "ORCL", user = "u", pwd = "p")

  expect_error(
    .construir_args_conexion(conf, "oracle", "SICO"),
    "el parámetro 'driver_type' es obligatorio"
  )
})

test_that(".construir_args_conexion rechaza un driver_type inválido para oracle", {
  conf <- list(server = "host1", database = "ORCL", user = "u", pwd = "p", driver_type = "jdbcx")

  expect_error(
    .construir_args_conexion(conf, "oracle", "SICO"),
    "el parámetro 'driver_type' es obligatorio"
  )
})

test_that(".construir_args_conexion arma la url JDBC para oracle/jdbc, usando el jar cacheado", {
  conf <- list(server = "host1", database = "ORCL", user = "u", pwd = "p", driver_type = "jdbc")

  testthat::local_mocked_bindings(
    .obtener_jar_oracle_jdbc = function(...) "ruta/falsa/ojdbc8.jar",
    .crear_driver_oracle_jdbc = function(jar_path) paste0("drv-jdbc:", jar_path)
  )

  args <- .construir_args_conexion(conf, "oracle", "SICO")

  expect_equal(args$drv, "drv-jdbc:ruta/falsa/ojdbc8.jar")
  expect_equal(args$url, "jdbc:oracle:thin:@//host1:1521/ORCL")
  expect_equal(args$user, "u")
  expect_equal(args$password, "p")
})

test_that(".construir_args_conexion respeta el puerto custom para oracle/jdbc", {
  conf <- list(server = "host1", database = "ORCL", user = "u", pwd = "p",
               driver_type = "jdbc", port = 1522)

  testthat::local_mocked_bindings(
    .obtener_jar_oracle_jdbc = function(...) "ruta/falsa/ojdbc8.jar",
    .crear_driver_oracle_jdbc = function(jar_path) paste0("drv-jdbc:", jar_path)
  )

  args <- .construir_args_conexion(conf, "oracle", "SICO")

  expect_equal(args$url, "jdbc:oracle:thin:@//host1:1522/ORCL")
})

test_that(".obtener_jar_oracle_jdbc descarga el jar solo si no está cacheado", {
  dir_cache_falso <- withr::local_tempdir()
  descargas <- list()

  testthat::local_mocked_bindings(
    .descargar_jar = function(url, destfile) {
      descargas[[length(descargas) + 1]] <<- url
      writeLines("contenido-jar-falso", destfile)
    },
    .dir_cache_utilidades3F = function() dir_cache_falso
  )

  ruta1 <- .obtener_jar_oracle_jdbc(version = "99.0.0.0")
  expect_true(file.exists(ruta1))
  expect_length(descargas, 1)

  # Segunda llamada: el jar ya está cacheado, no se vuelve a descargar.
  ruta2 <- .obtener_jar_oracle_jdbc(version = "99.0.0.0")
  expect_equal(ruta1, ruta2)
  expect_length(descargas, 1)
})

test_that(".construir_args_conexion rechaza motores no soportados", {
  conf <- list(server = "host1", database = "db1", user = "u", pwd = "p")

  expect_error(
    .construir_args_conexion(conf, "sqlite", "SICO"),
    "Tipo de base de datos no soportado"
  )
})

test_that("crear_conexiones arma la conexión correcta a partir de un config.yml real", {
  dir <- withr::local_tempdir()
  archivo <- file.path(dir, "config.yml")
  writeLines(c(
    "default:",
    "  db_type: \"postgres\"",
    "SICO:",
    "  db_type: \"oracle\"",
    "  driver_type: \"odbc\"",
    "  driver: \"Oracle in OraClient\"",
    "  server: dbhost",
    "  database: ORCL",
    "  port: 1521",
    "  user: usuario_fijo",
    "  pwd: clave_fija"
  ), archivo)

  args_capturados <- NULL
  testthat::local_mocked_bindings(
    .conectar_db = function(...) {
      args_capturados <<- list(...)
      "conexion_falsa"
    }
  )

  conns <- crear_conexiones("SICO", file = archivo)

  expect_equal(conns$SICO, "conexion_falsa")
  expect_equal(args_capturados$DBQ, "dbhost:1521/ORCL")
  expect_equal(args_capturados$UID, "usuario_fijo")
  expect_equal(args_capturados$PWD, "clave_fija")
})
