test_that("formatea columnas numéricas sin notación científica antes de mostrarlas", {
  x <- data.frame(id = 123456789012, nombre = "abc", stringsAsFactors = FALSE)

  capturado <- NULL
  testthat::local_mocked_bindings(
    .mostrar_tabla = function(x, ...) {
      capturado <<- x
    }
  )

  ver_tabla(x)

  expect_type(capturado$id, "character")
  expect_false(grepl("e", capturado$id, ignore.case = TRUE))
  expect_equal(capturado$nombre, "abc")
})

test_that("respeta el parámetro 'digits' para formatear con decimales fijos", {
  x <- data.frame(valor = 3.14159)

  capturado <- NULL
  testthat::local_mocked_bindings(
    .mostrar_tabla = function(x, ...) {
      capturado <<- x
    }
  )

  ver_tabla(x, digits = 2)

  expect_equal(trimws(capturado$valor), "3.14")
})

test_that("pasa argumentos adicionales (...) a .mostrar_tabla", {
  x <- data.frame(a = 1)

  args_capturados <- NULL
  testthat::local_mocked_bindings(
    .mostrar_tabla = function(x, ...) {
      args_capturados <<- list(...)
    }
  )

  ver_tabla(x, title = "Mi Tabla")

  expect_equal(args_capturados$title, "Mi Tabla")
})
