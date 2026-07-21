test_that("advierte si no detecta marcadores de proyecto R", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  expect_warning(estructurar_directorio(), "No se detectó ningún archivo")
})

test_that("no advierte si detecta un marcador de proyecto (ej. DESCRIPTION)", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  file.create(file.path(dir, "DESCRIPTION"))

  expect_message(estructurar_directorio(), "Proyecto detectado correctamente")
})

test_that("no valida nada si verificar_proyecto = FALSE", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  expect_no_warning(estructurar_directorio(verificar_proyecto = FALSE))
})

test_that("crea las carpetas de insumos y productos si no existen", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  suppressWarnings(estructurar_directorio())

  expect_true(dir.exists(file.path(dir, "insumos")))
  expect_true(dir.exists(file.path(dir, "productos")))
})

test_that("respeta nombres de carpetas personalizados", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  suppressWarnings(
    estructurar_directorio(nombre_insumos = "datos_crudos", nombre_productos = "salidas")
  )

  expect_true(dir.exists(file.path(dir, "datos_crudos")))
  expect_true(dir.exists(file.path(dir, "salidas")))
})

test_that("informa cuando el directorio ya existe", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  dir.create(file.path(dir, "insumos"))
  dir.create(file.path(dir, "productos"))

  expect_message(suppressWarnings(estructurar_directorio()), "ya existe")
})
