library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

DOMAIN_URL <- 'http://localhost:8080'


request_requests_json <- function(url_prefix, params) {
  res <- GET(url_prefix, query = params)
  content(res, type = "application/json")
  # content(res, as = "text") %>%
  # fromJSON(flatten = TRUE)
}

get_coordinates_url <- function(country = NULL, state = NULL, city = NULL,
                                street = NULL, postalcode = NULL, addressdetails = NULL) {
  params <- list()
  if (!is.null(country)) params$country <- country
  if (!is.null(state)) params$state <- state
  if (!is.null(city)) params$city <- city
  if (!is.null(street)) params$street <- street
  if (!is.null(postalcode)) params$postalcode <- postalcode
  if (is.null(addressdetails)) params$addressdetails <- '1'
  params$format <- 'json'

  url_prefix <- paste0(DOMAIN_URL, '/search?')

  return(list(url_prefix = url_prefix, params = params))
}

get_coordinates <- function(country = NULL, state = NULL, city = NULL,
                            street = NULL, postalcode = NULL) {
  url <- get_coordinates_url(country, state, city, street, postalcode)
  coordinates <- request_requests_json(url=url$url_prefix, params = url$params)
  tryCatch(
    coordinates[[1]],
    # coordinates[1,],
    error = function(e) NULL
  )
}


#' geocode_df
#'
#' geolocalizar direcciónes dentro de una base de datos
#' @param df base de datos
#' @param country cadena que contenga el país
#' @param country_col columna de país en caso de que este varíe
#' @param state cadena que contenga la provincia/estado
#' @param state_col columna de la provincia/estado en caso de que esta varíe
#' @param city cadena que contenga la ciudad
#' @param city_col columna que contenga la ciudad en caso de que varie
#' @param postalcode cadena que contenga el código postal
#' @param postalcode_col columna que contenga el código postal en caso de que varie
#' @param full_address_col columna que contenga la dirección completa "Calle y Altura"
#' @param street_name_col columna que contenga el nombre de la calle
#' @param street_number_col columna que contenga la altura
#' @return Devuelve la latitud y longitud expresada en EPSG 4326 además del código postal y la dirección normalizada
#' @examples
#' geolocalizado <- geocode_df(df, country_col = 'PAIS', city = 'Tres de Febrero', state = 'Buenos AIres', full_address_col = 'direccion_completa');
#' @export

geocode_df <- function(df, country = NULL, country_col = NULL, state = NULL, state_col = NULL, city = NULL, city_col = NULL,
                       postalcode = NULL, postalcode_col = NULL, full_address_col = NULL, street_name_col = NULL,
                       street_number_col = NULL) {

  # browser()

  if (!is.null(country) && !is.null(country_col)) {
    stop("Only one of 'country' and 'country_col' should be provided.")
  }
  if (!is.null(state) && !is.null(state_col)) {
    stop("Only one of 'state' and 'state_col' should be provided.")
  }
  if (!is.null(city) && !is.null(city_col)) {
    stop("Only one of 'city' and 'city_col' should be provided.")
  }
  if (!is.null(postalcode) && !is.null(postalcode_col)) {
    stop("Only one of 'postalcode' and 'postalcode_col' should be provided.")
  }

  if (!is.null(full_address_col) && (!is.null(street_name_col) || !is.null(street_number_col))) {
    stop("If 'full_address_col' is provided, 'street_name_col' and 'street_number_col' should not be provided.")
  }

  if ((!is.null(street_name_col) && is.null(street_number_col)) || (is.null(street_name_col) && !is.null(street_number_col))) {
    stop("Both 'street_name_col' and 'street_number_col' should be provided together, or not provided at all.")
  }

  df$nomi_lat_y <- NA
  df$nomi_lon_x <- NA
  df$nomi_type <- NA
  df$nomi_category <- NA
  df$nomi_direc_norm <- NA
  df$nomi_cod_post <- NA
  df$nomi_localidad <- NA
  successful_geocodes <- 0

  for (idx in seq_len(nrow(df))) {
    if (!is.null(full_address_col)) {
      street <- df[idx, full_address_col]
    } else if (!is.null(street_name_col) && !is.null(street_number_col)) {
      street <- paste(df[idx, street_name_col], df[idx, street_number_col])
    }

    data <- get_coordinates(
      country = if (is.null(country_col)) country else df[idx, country_col],
      state = if (is.null(state_col)) state else df[idx, state_col],
      city = if (is.null(city_col)) city else df[idx, city_col],
      street = street,
      postalcode = if (is.null(postalcode_col)) postalcode else df[idx, postalcode_col]
    )

    if (is.null(data)) {
      next
    }

    df$nomi_lat_y[idx] <- data$lat
    df$nomi_lon_x[idx] <- data$lon
    df$nomi_type[idx] <- data$type
    df$nomi_category[idx] <- data$class
    df$nomi_direc_norm[idx] <- data$display_name
    df$nomi_cod_post[idx] <- if (is.null(data$address$postcode)) NA else data$address$postcode
    df$nomi_localidad[idx] <- if (is.null(data$address$town)) NA else data$address$town
    successful_geocodes <- successful_geocodes + 1
  }

  success_percentage <- round((successful_geocodes / nrow(df)) * 100)
  cat("Geocoding success rate:", success_percentage, "%\n")

  return(df)
}
