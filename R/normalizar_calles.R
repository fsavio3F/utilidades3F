diccionario_calles <- utilidades3F::obtener_capa("callejero_normalizado") |> 
  sf::st_drop_geometry() |> 
  dplyr::mutate(nombre_simp = stringi::stri_trans_general(tolower(nombre_cal),"Latin-ASCII")) |>
  dplyr::select(nombre_simp, nombre_cal) |>
  dplyr::group_by(nombre_simp) |>
  dplyr::summarise(nombre_cal = dplyr::first(nombre_cal)) |>
  dplyr::filter(!is.na(nombre_cal))



# tokenizacion y emparejamiento
tokens_similitud <- function(nombre_org, nombres_normalizados) {
  # tokenizacion de nombres originales
  nombre_org <- stringi::stri_trans_general(tolower(nombre_org),"Latin-ASCII")
  tokens_org <- unlist(stringr::str_split(nombre_org, " "))
  
  # asignacion de puntaje
  mejor_empareja <- ""
  mejor_puntaje <- -1
  
  # revision de nombres normalizados
  for (nombre_norm in nombres_normalizados) {
    # tokenizacion de nombres correctos
    token_norm <- unlist(stringr::str_split(nombre_norm, " "))
    
    # encontrar token compartidos entre los nombres originales y normalizados
    tokens_comunes <- intersect(tolower(tokens_org), tolower(token_norm))
    
    # calculo de similitud
    puntaje <- length(tokens_comunes) / length(token_norm)
    
    # actualizacion de mejor emparejamiento
    if (puntaje > mejor_puntaje) {
      mejor_empareja <- nombre_norm
      mejor_puntaje <- puntaje
    }
  }
  print(mejor_puntaje)
  
  return(mejor_empareja)
}


#' normalizar_calles
#'
#' geolocalizar direcciónes dentro de una base de datos
#' @param df base de datos
#' @param nombre_calles columna que contenga los nombres de calles
#' @return devuelve la base de datos con una columna adicional con los nombres de calles normalizados
#' @examples
#' nombre_normalizados <- normalizar_calles(df, nombre_calles = "calle_nom");
#' @export

normalizar_calles <- function(df, nombre_calles) {
  df <- df |>
    dplyr::rowwise()|>
    dplyr::mutate(nombre_normalizado = tokens_similitud(!!dplyr::sym(nombre_calles), diccionario_calles$nombre_simp))
  return(df)
}  
