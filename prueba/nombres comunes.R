library(xlsx)
diccionario_calles <- utilidades3F::obtener_capa("callejero_normalizado") |> 
  sf::st_drop_geometry() |> 
  dplyr::mutate(nombre_simp = stringi::stri_trans_general(tolower(nombre_cal),"Latin-ASCII")) |>
  dplyr::select(nombre_simp, nombre_cal) |>
  dplyr::group_by(nombre_simp) |>
  dplyr::summarise(nombre_cal = dplyr::first(nombre_cal)) |>
  dplyr::filter(!is.na(nombre_cal))

calles_simplificadas <- data.frame()
for(calle_actual in 1:nrow(diccionario_calles)){
  token_nombres <- unlist(stringr::str_split(diccionario_calles$nombre_cal[calle_actual], " "))
  calles_simplificadas <- rbind(calles_simplificadas, token_nombres[length(token_nombres)])
}
calles_simplificadas <- calles_simplificadas %>% mutate(nombre_completo = diccionario_calles$nombre_cal)
colnames(calles_simplificadas)[1] <- "nombre simplificado"

write.csv(calles_simplificadas, row.names = FALSE ,"nombre de calles comunes.csv", fileEncoding = "windows-1252")
openxlsx::write.xlsx(calles_simplificadas,"nombre de calles comunes.xlsx" )
