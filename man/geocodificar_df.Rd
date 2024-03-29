% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geolocalizar.R
\name{geocodificar_df}
\alias{geocodificar_df}
\title{geocodificar_df}
\usage{
geocodificar_df(
  df,
  country = NULL,
  country_col = NULL,
  state = NULL,
  state_col = NULL,
  city = NULL,
  city_col = NULL,
  postalcode = NULL,
  postalcode_col = NULL,
  full_address_col = NULL,
  street_name_col = NULL,
  street_number_col = NULL
)
}
\arguments{
\item{df}{base de datos}

\item{country}{cadena que contenga el país}

\item{country_col}{columna de país en caso de que este varíe}

\item{state}{cadena que contenga la provincia/estado}

\item{state_col}{columna de la provincia/estado en caso de que esta varíe}

\item{city}{cadena que contenga la ciudad}

\item{city_col}{columna que contenga la ciudad en caso de que varie}

\item{postalcode}{cadena que contenga el código postal}

\item{postalcode_col}{columna que contenga el código postal en caso de que varie}

\item{full_address_col}{columna que contenga la dirección completa "Calle y Altura"}

\item{street_name_col}{columna que contenga el nombre de la calle}

\item{street_number_col}{columna que contenga la altura}
}
\value{
Devuelve la latitud y longitud expresada en EPSG 4326 además del código postal y la dirección normalizada
}
\description{
geolocalizar direcciónes dentro de una base de datos
}
\examples{
geolocalizado <- geocodificar_df(df, country_col = 'PAIS', city = 'Tres de Febrero', state = 'Buenos AIres', full_address_col = 'direccion_completa');
}
