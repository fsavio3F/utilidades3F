fixture_localidades <- function() {
  poligono <- sf::st_polygon(list(rbind(
    c(-58.6, -34.6), c(-58.5, -34.6), c(-58.5, -34.5), c(-58.6, -34.5), c(-58.6, -34.6)
  )))
  sf::st_sf(
    cod_ent = "01",
    nombre = "Caseros",
    geometry = sf::st_sfc(poligono, crs = 4326)
  )
}

fixture_puntos <- function() {
  sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(sf::st_point(c(-58.55, -34.55)), crs = 4326)
  )
}

test_that("exige que 'df' no sea NULL cuando ya se provee 'localidades'", {
  expect_error(
    normalizar_localidades(df = NULL, localidades = fixture_localidades()),
    "'df' no puede ser NULL"
  )
})

test_that("normaliza localidades uniendo espacialmente con la capa de referencia (mockeada)", {
  testthat::local_mocked_bindings(
    obtener_capa = function(...) fixture_localidades()
  )

  resultado <- normalizar_localidades(df = fixture_puntos())

  expect_true("localidad_normalizada" %in% names(resultado))
  expect_equal(resultado$localidad_normalizada, "Caseros")
})

test_that("informa un error claro si falla la obtención de la capa de localidades por defecto", {
  testthat::local_mocked_bindings(
    obtener_capa = function(...) stop("servidor caído")
  )

  expect_error(
    normalizar_localidades(df = fixture_puntos()),
    "Fallo al obtener la capa de localidades"
  )
})
