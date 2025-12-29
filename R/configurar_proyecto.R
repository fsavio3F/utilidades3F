#' Configurar estructura selectiva y obtener función de guardado
#'
#' Crea la estructura de directorios. Permite mezclar carpetas estáticas (sin fecha)
#' con carpetas versionadas (con subcarpeta de fecha). Devuelve una función para
#' generar rutas de archivos que respetan esa estructura automáticamente.
#'
#' @param version String. Identificador de la versión (ej. fecha "20251229").
#' @param categorias Vector. Todas las carpetas que necesita el proyecto.
#' @param versionar Vector. Subconjunto de 'categorias' que llevarán subcarpeta interna con la fecha.
#' @param ruta_base String. Ruta raíz del proyecto.
#'
#' @return Una función (closure) `dir_guardado(archivo, directorio, ...)`
#' @export
configurar_proyecto <- function(
    version = format(Sys.Date(), "%Y%m%d"),
    categorias = c("insumos", "productos"),
    versionar = NULL, # Por defecto, insumos es fijo, productos varía
    ruta_base = getwd()
) {
  
  # Validamos consistencia
  if (!all(versionar %in% categorias)) {
    warning("⚠️ Atención: Hay carpetas en 'versionar' que no están en 'categorias'.")
  }

  # 1. Crear estructura física y Mapear rutas
  # Guardamos un mapa interno (lista) de: nombre_categoria -> ruta_fisica_real
  mapa_rutas <- list()
  
  for (cat in categorias) {
    # Definir ruta física según si es versionada o estática
    ruta_real <- if (cat %in% versionar) {
      file.path(ruta_base, cat, version)
    } else {
      file.path(ruta_base, cat)
    }
    
    # Crear directorio si no existe
    if (!dir.exists(ruta_real)) {
      dir.create(ruta_real, recursive = TRUE)
      tipo <- if (cat %in% versionar) "(Versionado)" else "(Estático)"
      message("✅ Creado ", tipo, ": ", ruta_real)
    }
    
    # guarda el mapa de rutas
    mapa_rutas[[cat]] <- ruta_real
  }
  
  # 2. Definir la función de guardado (closure)
  # Esta función tiene acceso a 'mapa_rutas' y 'version'
    dir_guardado <- function(nombre, ext = NULL, directorio = "productos", sufijo = TRUE) {
    
    # Validación de directorio
    if (is.null(mapa_rutas[[directorio]])) {
      stop("❌ El directorio '", directorio, "' no está configurado.")
    }
    
    # Construcción del nombre base (sin extensión aún)
    nombre_base <- if (sufijo) {
      paste0(nombre, "_", version)
    } else {
      nombre
    }
    
    # Agregado de extensión (Manejo robusto del punto)
    nombre_final <- nombre_base
    if (!is.null(ext) && ext != "") {
      # Eliminamos el punto inicial si el usuario lo puso (ej. ".csv" -> "csv")
      ext_limpia <- sub("^\\.", "", ext) 
      nombre_final <- paste0(nombre_base, ".", ext_limpia)
    }
    
    # Ruta completa
    return(file.path(mapa_rutas[[directorio]], nombre_final))
  }
  
  return(dir_guardado)
}