#' obtener_capa_ARBA
#'
#' @param nombre_de_capa nombre de la capa a descargar
#' @return devuelve la capa deseada, en caso de no conocer el nombre utilizar la función de inventario_obtener_capa_ARBA()
#' @examples
#' \dontrun{
#' capa <- obtener_capa_ARBA(nombre_de_capa = "Parcela");
#' }
#' @export
obtener_capa_ARBA <- function(nombre_de_capa){
  # URL del servicio del GeoPortal
  url_base <- "https://geo.ARBA.gov.ar/datoabierto/datos/117/"
  # Construir la consulta usando el nombre de la capa requerida
  agregacion <- c("Subparcela","Departamento","Medida Lado","Circunscripcion","Fraccion","Parcela","Seccion Catastral","Manzana")
  codigo_agregacion <- c("110108","070121","110109","110107","110106","110101","110103","110102")
  gz_file <- paste(nombre_de_capa,".gz",sep = "")
  df <- data.frame(agregacion,codigo_agregacion)
  dir <- "insumos/ARBA"
  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  url <- paste(url_base,df$codigo_agregacion[df$agregacion == nombre_de_capa],"/",sep = "")
  
  httr::GET(url, httr::write_disk(paste(dir,gz_file, sep = "/"), overwrite = TRUE))
  
  utils::untar(paste(dir, gz_file, sep="/"), exdir = dir)
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

#' inventario_ARBA
#'
#' @return devuelve un listado de las capa disponibles para descargar de los geoservicios de ARBA
#' @export
inventario_ARBA<- function() {
  nombre_capa<- c("Subparcela","Departamento","Medida Lado","Circunscripcion","Fraccion","Parcela","Seccion Catastral","Manzana")
  
  # Textos escapados a Unicode
  descripcion <- c(
    "La propiedad de la unidad funcional o subparcela comprende la parte indivisa del terreno, de las cosas y partes de uso com\u00fan del inmueble o indispensables para mantener su seguridad, y puede abarcar una o m\u00e1s unidades complementarias destinadas a servirla.",
    "Divisi\u00f3n pol\u00edtico administrativa de segundo orden. Incluye partido y comuna.",
    "Valor num\u00e9rico del lado de un pol\u00edgono referido a un objeto catastral y que representa su longitud.",
    "El Departamento (Partido) se dividir\u00e1 en circunscripciones, pudiendo contener en conjunto o separadamente plantas urbanas, suburbanas y rurales.",
    "No tienen especificaciones de ning\u00fan tipo dentro de la Ley 10707, es por ello que podemos encontrarlas como macizos puros, o como subdivisiones de alg\u00fan macizo puro.",
    "Se denomina parcela a la cosa inmueble de extensi\u00f3n territorial continua, deslindado por una poligonal cerrada, perteneciente a un propietario o a varios en condominio, o pose\u00eddo por una persona o por varias en com\u00fan, cuya existencia y elementos esenciales consten en un plano registrado en el organismo catastral.",
    "Las plantas urbanas y suburbanas se dividir\u00e1n en secciones llevando sus l\u00edmites por calles, si es posible principales o avenidas y a falta de calles por un deslinde inconfundible de propiedad, no debiendo contener en general cada secci\u00f3n urbana un n\u00famero mayor de cien manzanas.",
    "Extensi\u00f3n de territorio cuya superficie no debe exceder las 1.5 hect\u00e1reas y est\u00e1n totalmente rodeadas de v\u00edas de comunicaci\u00f3n."
  )
  inventario <- data.frame(nombre_capa,descripcion)
  return(utils::View(inventario))
}
