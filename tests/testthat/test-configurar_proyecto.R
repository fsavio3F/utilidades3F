test_that("advierte si 'versionar' contiene carpetas fuera de 'categorias'", {
  dir <- withr::local_tempdir()

  expect_warning(
    configurar_proyecto(
      version = "20240101",
      categorias = c("insumos", "productos"),
      versionar = c("otra_carpeta"),
      ruta_base = dir,
      nombre_funcion = NULL
    ),
    "no están en 'categorias'"
  )
})

test_that("crea las carpetas estáticas y versionadas según corresponda", {
  dir <- withr::local_tempdir()

  suppressMessages(configurar_proyecto(
    version = "20240101",
    categorias = c("insumos", "productos"),
    versionar = "productos",
    ruta_base = dir,
    nombre_funcion = NULL
  ))

  expect_true(dir.exists(file.path(dir, "insumos")))
  expect_true(dir.exists(file.path(dir, "productos", "20240101")))
  expect_false(dir.exists(file.path(dir, "insumos", "20240101")))
})

test_that("el closure retornado arma rutas con sufijo de versión y extensión", {
  dir <- withr::local_tempdir()

  dir_guardado <- suppressMessages(configurar_proyecto(
    version = "20240101",
    categorias = c("insumos", "productos"),
    ruta_base = dir,
    nombre_funcion = NULL
  ))

  ruta <- dir_guardado("reporte", ext = "csv")
  expect_equal(ruta, file.path(dir, "productos", "reporte_20240101.csv"))

  ruta_sin_sufijo <- dir_guardado("reporte", ext = ".csv", sufijo = FALSE)
  expect_equal(ruta_sin_sufijo, file.path(dir, "productos", "reporte.csv"))

  ruta_insumos <- dir_guardado("base", directorio = "insumos", sufijo = FALSE)
  expect_equal(ruta_insumos, file.path(dir, "insumos", "base"))
})

test_that("el closure falla si se pide un directorio no configurado", {
  dir <- withr::local_tempdir()

  dir_guardado <- suppressMessages(configurar_proyecto(
    ruta_base = dir,
    nombre_funcion = NULL
  ))

  expect_error(dir_guardado("x", directorio = "no_existe"), "no está configurado")
})

test_that("no asigna nada en .GlobalEnv si nombre_funcion es NULL", {
  dir <- withr::local_tempdir()

  antes <- ls(envir = .GlobalEnv)
  suppressMessages(configurar_proyecto(ruta_base = dir, nombre_funcion = NULL))
  despues <- ls(envir = .GlobalEnv)

  expect_equal(antes, despues)
})

test_that("asigna la función constructora en .GlobalEnv cuando se especifica nombre_funcion", {
  dir <- withr::local_tempdir()
  nombre <- "dir_guardado_test_utilidades3F"
  withr::defer(rm(list = nombre, envir = .GlobalEnv))

  suppressMessages(configurar_proyecto(ruta_base = dir, nombre_funcion = nombre))

  expect_true(exists(nombre, envir = .GlobalEnv, inherits = FALSE))
  fn <- base::get(nombre, envir = .GlobalEnv)
  expect_true(is.function(fn))
})
