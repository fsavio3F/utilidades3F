#' actualizar_utilidades3F
#'
#' Reinstala el paquete `utilidades3F` desde el repositorio oficial de GitHub.
#'
#' @param ref Rama o tag a instalar. Por defecto usa "estable".
#' @param ... Argumentos adicionales que se pasan a `devtools::install_github()`.
#' @examples
#' \dontrun{
#'   actualizar_utilidades3F()  # Instala desde rama "estable"
#'   actualizar_utilidades3F(ref = "otra-rama") 
#' }
#' @return Mensaje de instalación o error.
#' @export
actualizar_utilidades3F <- function(ref = "estable", ...) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    message("[!] El paquete 'remotes' es necesario para actualizar desde GitHub.")
    message("-> Instalalo con: install.packages('remotes')")
    stop("Proceso cancelado: remotes no est\u00e1 instalado.")
  }
  
  repo <- "fsavio3F/utilidades3F"
  message("-> Instalando utilidades3F desde GitHub: ", repo, " (ref: ", ref, ")...")
  
  tryCatch(
    {
      remotes::install_github(repo = repo, ref = ref, upgrade = "never", force = TRUE, ...)
      message("[OK] Paquete 'utilidades3F' actualizado correctamente.")
    },
    error = function(e) {
      message("[ERROR] Fall\u00f3 al instalar desde GitHub: ", conditionMessage(e))
      stop("Fall\u00f3 la instalaci\u00f3n del paquete.")
    }
  )
  invisible(TRUE)
}
