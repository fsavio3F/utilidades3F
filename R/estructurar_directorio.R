#' estructurar_directorio
#'
#' Permite validar si existe un proyecto en el directorio de trabajo actual
#' y verificar que exista una subcarpeta de insumos y otra de productos en el mismo.
#' En caso de que las carpetas no existan las crea.
#'
#' @param verificar_proyecto  TRUE o FALSE para activar la verificación de proyecto.
#' @param nombre_insumos string con el nombre de la carpeta de insumos.
#' @param nombre_productos string con el nombre de la carpeta de productos.
#' @return Verificará si existe un proyecto en el directorio de trabajo actual y
#'  creará las carpetas de insumos y productos si no existen.
#' @examples
#' estructurar_directorio(verificar_proyecto = TRUE,
#'  nombre_insumos = "insumos",
#'  nombre_productos = "productos")
#' @export
estructurar_directorio <- function(
    verificar_proyecto = TRUE,
    nombre_insumos = "insumos",
    nombre_productos = "productos"
) {
  # Función interna: verifica si estás en un proyecto R
esta_en_proyecto_r <- function() {
    # Lista de archivos/carpetas que indican la raíz de un proyecto
    marcadores <- c("\\.Rproj$", "^\\.git$", "^DESCRIPTION$", "^NAMESPACE$", "^README\\.md$")
    patron_combinado <- paste(marcadores, collapse = "|")
    
    # Buscamos si alguno existe en el directorio actual
    archivos_raiz <- list.files(getwd(), pattern = patron_combinado, all.files = TRUE, ignore.case = TRUE)
    
    return(length(archivos_raiz) > 0)
  }
  # Chequeo de proyecto (si está activado)
  if (verificar_proyecto && !esta_en_proyecto_r()) {
    warning("⚠️ No se detectó ningún archivo '.Rproj' ni 'README.md', ni un proyecto activo en RStudio. ¿Estás en la raíz del proyecto?")
  } else if (verificar_proyecto) {
    message("📁 Proyecto detectado correctamente en el directorio actual.")
  }
  
  # Lista de directorios a crear, según parámetros
  directorios <- c(nombre_insumos, nombre_productos)
  
  invisible(lapply(directorios, function(nombre) {
    ruta <- file.path(getwd(), nombre)
    if (!dir.exists(ruta)) {
      dir.create(ruta, recursive = TRUE)
      message("✅ Se creó el directorio: '", nombre, "'.")
    } else {
      message("✔️ El directorio ya existe: '", nombre, "'.")
    }
  }))
}
