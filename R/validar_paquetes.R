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
      message("📦 El paquete '", pkg, "' no está instalado. Instalando...")
      
      # Usamos utils:: explícitamente para calmar al R CMD check
      utils::install.packages(pkg)
      
      if (!requireNamespace(pkg, quietly = TRUE)) {
        warning("⚠️ No se pudo instalar el paquete: ", pkg)
        next
      }
    } else {
      message("✅ '", pkg, "' ya está instalado.")
    }
    
    library(pkg, character.only = TRUE)
  }
}