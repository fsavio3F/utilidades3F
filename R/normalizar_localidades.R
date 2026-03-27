#' Permite normalizar y corregir los nombres de las localidades en base a la superposición geométrica
#' 
#' @param df Un objeto `sf` (capa) a normalizar.
#' @param localidades Un objeto `sf` con las localidades de referencia. Si es `NULL` (por defecto), 
#'   se descargará automáticamente la capa "localidades" del servidor.
#' @return Devuelve un objeto `sf` con la geometría unida espacialmente.
#' @examples
#' \dontrun{
#' capa <- obtener_capa(nombre_de_capa = "puntos_interes")
#' capa_normalizada <- normalizar_localidades(df = capa)
#' }
#' @export
normalizar_localidades <- function(df = NULL, localidades = NULL) {
  
  # Si el usuario no provee la capa de localidades, la vamos a buscar al servidor en runtime
  if (is.null(localidades)) {
    tryCatch({
      capa_cruda <- obtener_capa("localidades", ignorar_SSL = TRUE)
      
      localidades <- capa_cruda |> 
        dplyr::select(c("cod_ent", "nombre", "geometry")) |> 
        dplyr::rename(localidad_normalizada = nombre)
        
    }, error = function(e) {
      # Manejo de error claro para el usuario final si el servidor falla durante la ejecución
      stop("Fallo al obtener la capa de localidades por defecto del servidor: ", e$message, call. = FALSE)
    })
  }
  
  # Validaciones defensivas recomendadas antes de operar
  if (is.null(df)) {
    stop("El argumento 'df' no puede ser NULL.", call. = FALSE)
  }
  
  sf::st_join(df, localidades, join = sf::st_within)
}