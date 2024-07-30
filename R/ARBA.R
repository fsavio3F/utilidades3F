#' ARBA
#'
#' @param nombre_de_capa nombre de la capa a descargar
#' @return devuelve la capa deseada, en caso de no conocer el nombre utilizar la función de ARBA_inventario()
#' @examples
#' capa <- ARBA(nombre_de_capa = "Parcela");
#' @export
ARBA <- function(nombre_de_capa){
  # URL del servicio del GeoPortal
  url_base <- "https://geo.arba.gov.ar/datoabierto/datos/117/"
  # Construir la consulta usando el nombre de la capa requerida
  agregacion <- c("Subparcela","Departamento","Medida Lado","Circunscripcion","Fraccion","Parcela","Seccion Catastral","Manzana")
  codigo_agregacion <- c("110108","070121","110109","110107","110106","110101","110103","110102")
  gz_file <- paste(nombre_de_capa,".gz",sep = "")
  df <- data.frame(agregacion,codigo_agregacion)
  dir <- "ARBA"
  dir.create(dir)
  url <- paste(url_base,df$codigo_agregacion[df$agregacion == nombre_de_capa],"/",sep = "")
  response <- httr::GET(url, httr::write_disk(paste(dir,gz_file, sep = "/"), overwrite = TRUE))
  
  # Descargar capa
  untar(paste(dir, gz_file, sep="/"), exdir = dir)
  shapefile_path <- list.files(dir, pattern = "\\.shp$", full.names = TRUE)
  print(paste0("Descargando '", nombre_de_capa, "' del URL: '", url, "'"))
  capa_prov <- sf::read_sf(shapefile_path)
  sf::write_sf(capa_prov, paste(dir,"/",nombre_de_capa,".geojson",sep = ""), delete_dsn = TRUE)
  capa <- sf::read_sf(paste(dir,"/",nombre_de_capa,".geojson",sep = ""))
  cod_agr <- df$codigo_agregacion[df$agregacion == nombre_de_capa]
  unlink(c(paste(dir,"/",gz_file, sep = ""),
           paste(dir,"/",cod_agr,".shp",sep = ""),
           paste(dir,"/",cod_agr,".shx",sep = ""),
           paste(dir,"/",cod_agr,".prj",sep = ""),
           paste(dir,"/",cod_agr,".dbf",sep = ""),
           paste(dir,"/",cod_agr,".cpg",sep = "")))
  return(capa)

}

#' ARBA_inventario
#'
#' @return devuelve un listado de las capa disponibles para descargar de los geoservicios de ARBA
#' @export

ARBA_inventario<- function() {
  nombre_capa<- c("Subparcela","Departamento","Medida Lado","Circunscripcion","Fraccion","Parcela","Seccion Catastral","Manzana")
  descripcion <- c("La propiedad de la unidad funcional o subparcela comprende la parte indivisa del terreno, de las cosas y partes de uso común del inmueble o indispensables para mantener su seguridad, y puede abarcar una o más unidades complementarias destinadas a servirla.",
                   "División político administrativa de segundo orden. Incluye partido y comuna.",
                   "Valor numérico del lado de un polígono referido a un objeto catastral y que representa su longitud.",
                   "El Departamento (Partido) se dividirá en circunscripciones, pudiendo contener en conjunto o separadamente plantas urbanas, suburbanas y rurales.",
                   "No tienen especificaciones de ningún tipo dentro de la Ley 10707, es por ello que podemos encontrarlas como macizos puros, o como subdivisiones de algún macizo puro.",
                   "Se denomina parcela a la cosa inmueble de extensión territorial continua, deslindado por una poligonal cerrada, perteneciente a un propietario o a varios en condominio, o poseído por una persona o por varias en común, cuya existencia y elementos esenciales consten en un plano registrado en el organismo catastral.",
                   "Las plantas urbanas y suburbanas se dividirán en secciones llevando sus límites por calles, si es posible principales o avenidas y a falta de calles por un deslinde inconfundible de propiedad, no debiendo contener en general cada sección urbana un número mayor de cien manzanas.",
                   "Extensión de territorio cuya superficie no debe exceder las 1.5 hectáreas y están totalmente rodeadas de vías de comunicación.")
  inventario <- data.frame(nombre_capa,descripcion)
  return(View(inventario))
}

