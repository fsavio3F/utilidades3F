diccionario_calles <- obtener_capa("callejero_normalizado") |> 
  sf::st_drop_geometry() |> 
  dplyr::mutate(nombre_simp = stringi::stri_trans_general(tolower(nombre_cal),"Latin-ASCII")) |>
  dplyr::select(nombre_simp, nombre_cal) |>
  dplyr::group_by(nombre_simp) |>
  dplyr::summarise(nombre_cal = dplyr::first(nombre_cal))


diccionario_calles<- diccionario_calles %>%  filter(!is.na(diccionario_calles$nombre_simp) & diccionario_calles$nombre_simp!="")

# tokenizacion y emparejamiento
tokens_similitud <- function(nombre_org, nombres_normalizados){
  # tokenizacion de nombres originales
  
  #creo un data framme con el nombre comun y su correspondiente calle
  nombres_comunes <- c("general paz", "lacroze", "zavatarro", "j.b. justo", "ruta 8", "perez galdos", 
                       "wernicke", "cafferata", "lincol", "gral lavalle", "padre elizalde",
                       "avenida urquiza", "j d peron", "avenida alvear", "eva peron", "w del tata")
 
   nombres_reales <- c("avenida general jose maria paz","federico lacroze","pedro jose luis zavatarro","avenida juan b justo",
                      "avenida eva duarte de peron","benito perez galdos","german wernicke","agustin cafferata", "abraham lincoln",
                      "general juan galo lavalle", "padre agustin gabriel bonney elizalde", "justo jose de urquiza", 
                      "avenida presidente juan domingo peron", "avenida marcelo torcuato de alvear", "avenida eva duarte de peron", "doctor wenceslao de tata")
  
  nombres_coloquiales <- data.frame(nombres_comunes = nombres_comunes, nombres_reales = nombres_reales)
  rm(nombres_comunes, nombres_reales)
  
  #agrego los nombres comunes en la lista de nombres normalizados asi los utiliza en la comparacion posterior 
  nombres_normalizados <- c(nombres_normalizados, nombres_coloquiales$nombres_comunes)
  
  #modifico los nombres a corregir, para que esten en minusculas, sin tildes ni puntos para una mejor comparacion
  nombre_org <- stringi::stri_trans_general(tolower(nombre_org),"Latin-ASCII")
  nombre_org <- gsub("\\.", " ", nombre_org)
  
  #modifico abreviaciones con su forma completa para tener mas similitudes con su nombre completo 
  #y no lo compare con otro erroneo
  nombre_org <- gsub("pte ", "presidente ", nombre_org)
  nombre_org <- gsub("av ", "avenida ", nombre_org)
  nombre_org <- gsub("gral ", "general ", nombre_org)
  nombre_org <- gsub("pres ", "presidente ", nombre_org)
  tokens_org <- unlist(stringr::str_split(nombre_org, " "))
  
  #asignación de puntaje
  mejor_empareja <- ""
  mejor_puntaje <- -1
  
  # revisión de nombres normalizados
  for (nombre_norm in 1:length(nombres_normalizados)) {
    # tokenizacion de nombres correctos
    token_norm <- unlist(stringr::str_split(nombres_normalizados[nombre_norm], " "))
    
    # encontrar token compartidos entre los nombres originales y normalizados
    tokens_en_comun <- intersect(tolower(tokens_org), tolower(token_norm))
    
    # calculo de similitud
    puntaje_tokens <-  length(tokens_en_comun) / (length(token_norm))
    
    # Cálculo de similitud basado en distancia de cadena (Levenshtein)
    
    distancia <- stringdist::stringdist(nombre_org, nombres_normalizados[nombre_norm], method = "lv")
    puntaje_distancia <- 1/(1+distancia)
    
    nombres_normalizados[nombre_norm] <- ifelse(nombres_normalizados[nombre_norm] %in% nombres_coloquiales$nombres_comunes, 
                                                nombres_coloquiales$nombres_reales[nombres_coloquiales$nombres_comunes == nombres_normalizados[nombre_norm]],
                                                nombres_normalizados[nombre_norm])
    #puntaje final
    
    puntaje <- (puntaje_tokens + puntaje_distancia) / 2
    
    # actualización de mejor emparejamiento
    if (puntaje > mejor_puntaje) {
      mejor_empareja <- nombres_normalizados[nombre_norm]
      mejor_puntaje <- puntaje
    }
  }
  return(mejor_empareja)
}


#
## tokenizacion y emparejamiento
#tokens_similitud <- function(nombre_org, nombres_normalizados) {
#  # tokenizacion de nombres originales
#  nombre_org <- stringi::stri_trans_general(tolower(nombre_org),"Latin-ASCII")
#  tokens_org <- unlist(stringr::str_split(nombre_org, " "))
#  
#  # asignacion de puntaje
#  mejor_empareja <- ""
#  mejor_puntaje <- -1
#  
#  # revision de nombres normalizados
#  for (nombre_norm in nombres_normalizados) {
#    # tokenizacion de nombres correctos
#    token_norm <- unlist(stringr::str_split(nombre_norm, " "))
#    
#    # encontrar token compartidos entre los nombres originales y normalizados
#    tokens_comunes <- intersect(tolower(tokens_org), tolower(token_norm))
#    
#    # calculo de similitud
#    puntaje <- length(tokens_comunes) / length(token_norm)
#    
#    # actualizacion de mejor emparejamiento
#    if (puntaje > mejor_puntaje) {
#      mejor_empareja <- nombre_norm
#      mejor_puntaje <- puntaje
#    }
#  }
#  return(mejor_empareja)
#}
#
#
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

