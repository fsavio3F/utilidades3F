test_that("valida db_type contra la lista permitida", {
  testthat::local_mocked_bindings(interactive = function() TRUE)

  expect_error(
    configurar_credenciales_db("SICO", db_type = "sqlite"),
    "no es válido"
  )
})

test_that("exige 'driver' para sqlserver", {
  testthat::local_mocked_bindings(interactive = function() TRUE)

  expect_error(
    configurar_credenciales_db("SICO", db_type = "sqlserver"),
    "El parámetro 'driver' es obligatorio"
  )
})

test_that("exige 'driver_type' para oracle (sin default)", {
  testthat::local_mocked_bindings(interactive = function() TRUE)

  expect_error(
    configurar_credenciales_db("SICO", db_type = "oracle"),
    "El parámetro 'driver_type' es obligatorio"
  )
  expect_error(
    configurar_credenciales_db("SICO", db_type = "oracle", driver_type = "xml"),
    "El parámetro 'driver_type' es obligatorio"
  )
})

test_that("exige 'driver' para oracle cuando driver_type = 'odbc', pero no cuando es 'jdbc'", {
  testthat::local_mocked_bindings(
    interactive = function() TRUE,
    .guardar_credencial = function(service, username) invisible(NULL),
    .leer_credencial = function(service, username) "no_vacio"
  )

  expect_error(
    configurar_credenciales_db("SICO", db_type = "oracle", driver_type = "odbc"),
    "El parámetro 'driver' es obligatorio"
  )
  expect_error(
    configurar_credenciales_db("SICO", db_type = "oracle", driver_type = "jdbc"),
    NA
  )
})

test_that("un config.yml nuevo queda evaluable de inmediato por config::get() (regresión bug default: null)", {
  withr::local_options(keyring_backend = "env")
  dir <- withr::local_tempdir()
  archivo <- file.path(dir, "config.yml")

  keyring::key_set_with_value(service = "SICO_TEST", username = "db_user", password = "usuario_fake")
  keyring::key_set_with_value(service = "SICO_TEST", username = "db_pwd", password = "clave_fake")
  withr::defer({
    keyring::key_delete(service = "SICO_TEST", username = "db_user")
    keyring::key_delete(service = "SICO_TEST", username = "db_pwd")
  })

  testthat::local_mocked_bindings(
    interactive = function() TRUE,
    .guardar_credencial = function(service, username) invisible(NULL),
    .leer_credencial = function(service, username) "no_vacio"
  )

  configurar_credenciales_db(
    nombre_servicio = "SICO_TEST",
    db_type = "postgres",
    server = "host1",
    database = "db1",
    file = archivo
  )

  cfg <- config::get(config = "SICO_TEST", file = archivo)
  expect_equal(cfg$db_type, "postgres")
  expect_equal(cfg$server, "host1")
  expect_equal(cfg$database, "db1")
  expect_equal(cfg$user, "usuario_fake")
  expect_equal(cfg$pwd, "clave_fake")
})

test_that("agrega un servicio nuevo a un config.yml existente sin tocar los demás", {
  dir <- withr::local_tempdir()
  archivo <- file.path(dir, "config.yml")
  writeLines(c(
    "default: {}",
    "OTRO:",
    "  db_type: postgres",
    "  server: otroserver",
    "  database: otradb",
    '  user: !expr keyring::key_get(service = "OTRO", username = "db_user")',
    '  pwd: !expr keyring::key_get(service = "OTRO", username = "db_pwd")'
  ), archivo)

  testthat::local_mocked_bindings(
    interactive = function() TRUE,
    .guardar_credencial = function(service, username) invisible(NULL),
    .leer_credencial = function(service, username) "no_vacio"
  )

  configurar_credenciales_db(
    nombre_servicio = "SICO",
    db_type = "oracle",
    driver_type = "odbc",
    driver = "Oracle in OraClient",
    server = "dbhost",
    database = "ORCL",
    port = 1521,
    file = archivo
  )

  cfg <- .leer_config_bruto(archivo)
  expect_true("OTRO" %in% names(cfg))
  expect_true("SICO" %in% names(cfg))
  expect_equal(cfg$SICO$db_type, "oracle")
  expect_equal(cfg$SICO$driver_type, "odbc")
  expect_equal(cfg$SICO$driver, "Oracle in OraClient")
  expect_equal(cfg$SICO$port, 1521L)
  expect_s3_class(cfg$SICO$user, "verbatim_expr")
  expect_equal(
    as.character(cfg$SICO$user),
    'keyring::key_get(service = "SICO", username = "db_user")'
  )
})

test_that("reemplaza un servicio existente si el usuario confirma ('s')", {
  dir <- withr::local_tempdir()
  archivo <- file.path(dir, "config.yml")
  writeLines(c(
    "default: {}",
    "SICO:",
    "  db_type: postgres",
    "  server: viejo",
    "  database: viejadb",
    '  user: !expr keyring::key_get(service = "SICO", username = "db_user")',
    '  pwd: !expr keyring::key_get(service = "SICO", username = "db_pwd")'
  ), archivo)

  testthat::local_mocked_bindings(
    interactive = function() TRUE,
    readline = function(...) "s",
    .guardar_credencial = function(service, username) invisible(NULL),
    .leer_credencial = function(service, username) "no_vacio"
  )

  configurar_credenciales_db(
    nombre_servicio = "SICO",
    db_type = "oracle",
    driver_type = "odbc",
    driver = "Oracle in OraClient",
    server = "nuevo",
    database = "nuevadb",
    file = archivo
  )

  cfg <- .leer_config_bruto(archivo)
  expect_equal(cfg$SICO$db_type, "oracle")
  expect_equal(cfg$SICO$server, "nuevo")
  expect_equal(cfg$SICO$database, "nuevadb")
})

test_that("cancela el reemplazo si el usuario responde que no ('n')", {
  dir <- withr::local_tempdir()
  archivo <- file.path(dir, "config.yml")
  contenido_original <- c(
    "default: {}",
    "SICO:",
    "  db_type: postgres",
    "  server: viejo",
    "  database: viejadb",
    '  user: !expr keyring::key_get(service = "SICO", username = "db_user")',
    '  pwd: !expr keyring::key_get(service = "SICO", username = "db_pwd")'
  )
  writeLines(contenido_original, archivo)

  testthat::local_mocked_bindings(
    interactive = function() TRUE,
    readline = function(...) "n",
    .guardar_credencial = function(service, username) invisible(NULL),
    .leer_credencial = function(service, username) "no_vacio"
  )

  configurar_credenciales_db(
    nombre_servicio = "SICO",
    db_type = "oracle",
    driver_type = "odbc",
    driver = "Oracle in OraClient",
    server = "nuevo",
    database = "nuevadb",
    file = archivo
  )

  expect_equal(readLines(archivo), contenido_original)
})
