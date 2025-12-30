#' Validar e instalar paquetes necesarios
#'
#' Verifica si una lista de paquetes está instalada. Si alguno falta, lo instala automáticamente.
#' Finalmente, carga los paquetes en la sesión actual.
#'
#' @param paquetes Vector de caracteres con los nombres de los paquetes.
#' @examples
#' \dontrun{
#'   validar_paquetes(c("dplyr", "sf"))
#' }
#' @export
validar_paquetes <- function(paquetes) {
  for (pkg in paquetes) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("[!] El paquete '", pkg, "' no est\u00e1 instalado. Instalando...")
      
      utils::install.packages(pkg)
      
      if (!requireNamespace(pkg, quietly = TRUE)) {
        warning("[X] No se pudo instalar el paquete: ", pkg)
        next
      }
    } else {
      message("[OK] '", pkg, "' ya est\u00e1 instalado.")
    }
    
    library(pkg, character.only = TRUE)
  }
}