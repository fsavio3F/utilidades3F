# Obtengo el diccionario de calles extraído desde el geoportal municipal

diccionario_calles <- utilidades3F::obtener_capa("callejero_normalizado") |> 
  sf::st_drop_geometry() |> 
  dplyr::mutate(nombre_simp = stringi::stri_trans_general(tolower(nombre_cal),"Latin-ASCII")) |>
  dplyr::select(nombre_simp, nombre_cal) |>
  dplyr::group_by(nombre_simp) |>
  dplyr::summarise(nombre_cal = dplyr::first(nombre_cal)) |>
  dplyr::filter(!is.na(nombre_cal))

wd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)
nombres_comunes <- readxl::read_excel("nombre de calles comunes.xlsx", col_names = TRUE)
nombres_comunes$`nombre simplificado` <- stringi::stri_trans_general(tolower(nombres_comunes$`nombre simplificado`), "Latin-ASCII")

# tokenizacion y emparejamiento
tokens_similitud <- function(nombre_org, nombres_normalizados) {
  # tokenización de nombres originales
  nombre_org <- stringi::stri_trans_general(tolower(nombre_org), "Latin-ASCII")
  tokens_org <- unlist(stringr::str_split(nombre_org, " "))
  
  # asignación de puntaje
  mejor_empareja <- ""
  mejor_puntaje <- -1
  puntaje_token <- -1
  puntaje_distancia <- -1
  saltea_token <- TRUE
  
  mejor_empareja <- ifelse(nombre_org %in% nombres_comunes$`nombre simplificado`, 
                           nombres_comunes$nombre_completo[which(nombres_comunes$`nombre simplificado` == nombre_org)], saltea_token <- FALSE)

  
  
  
  # revisión de nombres normalizados
  if(saltea_token == FALSE){   
    for (nombre_norm in nombres_normalizados) {
      # tokenización de nombres correctos
      token_norm <- unlist(stringr::str_split(nombre_norm, " "))
      
      # encontrar tokens compartidos entre los nombres originales y normalizados
      tokens_comunes <- intersect(tolower(tokens_org), tolower(token_norm))
      
      # cálculo de similitud
      puntaje_tokens <- length(tokens_comunes) / length(token_norm)
      
      # cálculo de similitud basado en distancia de cadena (Levenshtein)
      distancia <- stringdist::stringdist(nombre_org, nombre_norm, method = "lv")
      puntaje_distancia <- 1 / (1 + distancia) 
      
      # puntaje final
      puntaje <- (puntaje_tokens + (puntaje_distancia*0.2)) / 2
      
      # actualización del mejor emparejamiento y puntaje
      if (puntaje > mejor_puntaje) {
        mejor_empareja <- nombre_norm
        mejor_puntaje <- puntaje
        puntaje_token <- puntaje_tokens
        puntaje_distancia <- puntaje_distancia
      }
    }
  }
  
  
  # Devolver una lista con el mejor emparejamiento y el mejor puntaje
  print(puntaje_token)
  return(list(mejor_empareja = mejor_empareja, mejor_puntaje = mejor_puntaje, puntaje_token = puntaje_token, puntaje_distancia = puntaje_distancia))
}


#' normalizar_calles
#'
#' Permite normalizar los nombres de calles de una base de datos para el municipio de Tres de Febrero
#' @param df base de datos
#' @param nombre_calles columna que contenga los nombres de calles
#' @return devuelve la base de datos con una columna adicional con los nombres de calles normalizados
#' @examples
#' nombre_normalizados <- normalizar_calles(df, nombre_calles = "calle_nombre");
#' @export

normalizar_calles <- function(df, nombre_calles, debug = FALSE) {
  if (debug == TRUE) {
    df <- df |>
      dplyr::rowwise() |>
      dplyr::mutate(
        resultado = list(tokens_similitud(!!dplyr::sym(nombre_calles), diccionario_calles$nombre_simp)),
        nombre_normalizado = resultado$mejor_empareja,
        puntaje_mejor = resultado$mejor_puntaje,
        puntaje_distancia = resultado$puntaje_distancia,
        puntaje_tokens = resultado$puntaje_token
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-resultado) # Elimina la columna temporal 'resultado'
  } else {
    df <- df |>
      dplyr::rowwise() |>
      dplyr::mutate(
        resultado = list(tokens_similitud(!!dplyr::sym(nombre_calles), diccionario_calles$nombre_simp))
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-resultado) # Elimina la columna temporal 'resultado'
  }
  return(df)
}

