#' actualizar_utilidades3F
#'
#' Reinstala el paquete `utilidades3F` desde el repositorio oficial de GitHub.
#'
#' @param ref Rama o tag a instalar. Por defecto usa "main".
#' @param ... Argumentos adicionales que se pasan a `devtools::install_github()`.
#'
#' @return Mensaje de instalación o error.
#' @examples
#' actualizar_utilidades3F()  # Instala desde main
#' actualizar_utilidades3F(ref = "desarrollo")  # Instala desde rama desarrollo
#' @export
actualizar_utilidades3F <- function(ref = "main", ...) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    message("🚨 El paquete 'devtools' es necesario para actualizar desde GitHub.")
    message("👉 Instalalo con: install.packages('devtools')")
    stop("Proceso cancelado: devtools no está instalado.")
  }
  
  repo <- "Datos-3F/utilidades3F"
  message("⬇️ Instalando utilidades3F desde GitHub: ", repo, " (ref: ", ref, ")...")
  
  tryCatch(
    {
      devtools::install_github(repo = repo, ref = ref, upgrade = "never", force = TRUE, ...)
      message("✅ Paquete 'utilidades3F' actualizado correctamente desde GitHub.")
    },
    error = function(e) {
      message("❌ Error al instalar desde GitHub: ", conditionMessage(e))
      stop("Falló la instalación del paquete.")
    }
  )
  
  invisible(TRUE)
}
