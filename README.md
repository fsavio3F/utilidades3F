# **utilidades3F**

**Autor y mantenimiento:** *Federico Savio*

## Tabla de contenidos

1. [Objetivo](#Objetivo)
   
2. [Funciones disponibles](#Funciones-disponibles)

3. [Requisitos](#Requisitos)
  

# **Objetivo**

Utilidades3F tiene como objetivoc centralziar un conjunto de funciones de uso frecuente dentro del Departamento de Datos de la municipalidad de Tres de Febrero en un único paquete


# **Funciones disponibles**

* **inventario_cpas()**: Nos permite obtener un listado de las capas públicas disponibles en el [geoportal municipal](https://geoportal.tresdefebrero.gob.ar/).

* **obtener_capa()**: Nos permite obtener una capa pública del [geoportal municipal](https://geoportal.tresdefebrero.gob.ar/) utilizando el nombre salido de la función *inventario_capas()*.

* **geocode_df()**: Nos permite obtener las coordenadas, dirección normalizada, y código postal para un  conjunto de datos del cual dispongamos la calle y la altura utilizando los servicios de nominatim. **¹**

* **normalizar_localidades()**: Nos permite normalizar los nombres de las localidades del partido de Tres de Febrero mediante la unión espacial de los [polígonos de las localidades disponibles en el geoportal municipal](https://geoportal.tresdefebrero.gob.ar/layers/geonode_data:geonode:localidades) basandonos en la ordenanza [Nº 2096](https://geoportal.tresdefebrero.gob.ar/documents/807).


# **Requisitos**

Para instalar el paquete de utilidades3F es necesario primero contar con la librerría de devtools en R la cual contiene la función *install_github()* que nos permite instalar paquetes de R almacenados en github.

```sh
install.packages("devtools")
devtools::install_github("fsavio3f/utilidades3F")
```

Una vez realizado este paso podremos comenzar a utilizar las funciones disponibles en el paquete.**¹**

**¹ IMPORTANTE**: Para poder utilizar la función *geocode_df()* es necesario correr localmente un servidor de nominatim siguiendo estas [instrucciones](https://github.com/fsavio3F/OpenGeocoding).
