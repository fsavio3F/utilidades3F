#' Configurar estructura y exportar función de guardado
#'
#' Crea directorios e "inyecta" la función de guardado directamente en el entorno
#' de trabajo para no tener que asignarla manualmente.
#'
#' @param version String. Identificador (ej. "20251229").
#' @param categorias Vector. Carpetas del proyecto.
#' @param versionar Vector. Carpetas que llevan subcarpeta de fecha.
#' @param ruta_base String. Ruta raíz.
#' @param nombre_funcion String o NULL. Nombre con el que se creará la función en el entorno global.
#'        Si es NULL, la función solo retorna el closure y no crea nada en el entorno.
#'
#' @return Invisible. Retorna la función constructora (closure) silenciosamente.
#' @export
configurar_proyecto <- function(
    version = format(Sys.Date(), "%Y%m%d"),
    categorias = c("insumos", "productos"),
    versionar = NULL, 
    ruta_base = getwd(),
    nombre_funcion = "dir_guardado" 
) {
  
  # 1. Crear estructura física
  if (!is.null(versionar) && !all(versionar %in% categorias)) {
    warning("[!] Atenci\u00f3n: Hay carpetas en 'versionar' que no est\u00e1n en 'categorias'.")
  }

  mapa_rutas <- list()
  
  for (cat in categorias) {
    ruta_real <- if (cat %in% versionar) {
      file.path(ruta_base, cat, version)
    } else {
      file.path(ruta_base, cat)
    }
    
    if (!dir.exists(ruta_real)) {
      dir.create(ruta_real, recursive = TRUE)
      tipo <- if (cat %in% versionar) "(Versionado)" else "(Est\u00e1tico)"
      message("[OK] Creado ", tipo, ": ", ruta_real)
    }
    mapa_rutas[[cat]] <- ruta_real
  }
  
  # 2. Definir el closure
  dir_guardado_fn <- function(nombre, ext = NULL, directorio = "productos", sufijo = TRUE) {
    if (is.null(mapa_rutas[[directorio]])) {
      stop("[Error] El directorio '", directorio, "' no est\u00e1 configurado.")
    }
    
    nombre_base <- if (sufijo) paste0(nombre, "_", version) else nombre
    
    nombre_final <- nombre_base
    if (!is.null(ext) && ext != "") {
      ext_limpia <- sub("^\\.", "", ext) 
      nombre_final <- paste0(nombre_base, ".", ext_limpia)
    }
    
    return(file.path(mapa_rutas[[directorio]], nombre_final))
  }
  
  # Exporta la funcion de directorio de guardado al entorno global si se especifica un nombre
  if (!is.null(nombre_funcion)) {
    assign(nombre_funcion, dir_guardado_fn, envir = .GlobalEnv)
    message("[*] Funci\u00f3n '", nombre_funcion, "()' lista para usar.")
  }
  
  return(invisible(dir_guardado_fn))
}
