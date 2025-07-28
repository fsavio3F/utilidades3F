#' validar_paquetes
#'
#' permite validar si los paquetes necesarios en un script se encuentran instalados
#' en caso de no estar instalados, se instalan
#' @param paquetes vector con los paquetes a instalar
#' @examples
#' validar_paquetes(paquetes);
#' @export
validar_paquetes <- function(paquetes) {
  for (i in paquetes) {
    if (!requireNamespace(i, quietly = TRUE)) {
      cat(i, "no está instalado \n")
      install.packages(i)
    } else {
      cat(i, "está instalado \n")
    }
    
    library(i, character.only = TRUE)
  }
  # borrar el listado de paquetes
  rm(paquetes)
}

