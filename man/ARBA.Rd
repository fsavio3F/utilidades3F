% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ARBA.R
\name{ARBA}
\alias{ARBA}
\title{ARBA}
\usage{
ARBA(nombre_de_capa)
}
\arguments{
\item{nombre_de_capa}{nombre de la capa a descargar}
}
\value{
devuelve la capa deseada, en caso de no conocer el nombre utilizar la función de ARBA_inventario()
}
\description{
geolocalizar direcciónes dentro de una base de datos
}
\examples{
capa <- ARBA(nombre_de_capa = "Parcela");
}
