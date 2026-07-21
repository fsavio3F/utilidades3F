test_that("valida argumentos mutuamente excluyentes antes de tocar la red", {
  df <- data.frame(dummy = 1)

  expect_error(geocodificar_df(df, country = "AR", country_col = "pais"), "country")
  expect_error(geocodificar_df(df, state = "BA", state_col = "prov"), "state")
  expect_error(geocodificar_df(df, city = "3F", city_col = "ciudad"), "city")
  expect_error(geocodificar_df(df, postalcode = "1000", postalcode_col = "cp"), "postalcode")
  expect_error(
    geocodificar_df(df, full_address_col = "dir", street_name_col = "calle"),
    "full_address_col"
  )
  expect_error(
    geocodificar_df(df, street_name_col = "calle"),
    "street_name_col"
  )
})

test_that("completa las columnas geocodificadas con el resultado (mockeado) de obtener_coordenadas", {
  df <- data.frame(
    direccion = c("Calle Falsa 123", "Av. Siempre Viva 742"),
    stringsAsFactors = FALSE
  )

  mock_data <- list(
    lat = "-34.6", lon = "-58.5",
    type = "house", class = "building",
    display_name = "Calle Falsa 123, Tres de Febrero",
    address = list(postcode = "1687", town = "Tres de Febrero")
  )

  testthat::local_mocked_bindings(
    obtener_coordenadas = function(...) mock_data
  )

  resultado <- geocodificar_df(df, full_address_col = "direccion", city = "Tres de Febrero")

  expect_equal(resultado$nomi_lat_y, c("-34.6", "-34.6"))
  expect_equal(resultado$nomi_lon_x, c("-58.5", "-58.5"))
  expect_equal(resultado$nomi_cod_post, c("1687", "1687"))
  expect_equal(resultado$nomi_localidad, c("Tres de Febrero", "Tres de Febrero"))
})

test_that("informa la tasa de éxito de geocodificación", {
  df <- data.frame(direccion = "Calle Falsa 123", stringsAsFactors = FALSE)

  testthat::local_mocked_bindings(
    obtener_coordenadas = function(...) NULL
  )

  expect_output(
    geocodificar_df(df, full_address_col = "direccion"),
    "0 %"
  )
})
