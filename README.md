# **utilidades3Fa**

**Autores y mantenimiento:**

- *[Federico Savio](https://github.com/fsavio3F)*
 
- *[Agustín Saavedra](https://github.com/agustinSaavedra)*
 
## Tabla de contenidos

1. [Objetivo](#objetivo)
2. [Funciones disponibles](#funciones-disponibles)
3. [Requisitos](#requisitos)

# **Objetivo**

El paquete `utilidades3F` busca centralizar en un solo lugar un conjunto de funciones que usamos frecuentemente en el Departamento de Datos de la Municipalidad de Tres de Febrero.

# **Funciones disponibles**

### Autenticación y descarga desde Geoportal 3F

- **autenticar_geoportal()**: Inicia sesión en el geoportal mediante OAuth2 y guarda el token en un directorio persistente del usuario.
- **obtener_inventario()**: Lista las capas disponibles del geoportal. Si se habilita la autenticación, incluye capas privadas.
- **obtener_capa()**: Descarga una capa pública o privada del geoportal como objeto `sf`. Valida que la capa exista en el inventario. El acceso sin autenticación se ejecuta en una sesión separada por seguridad.

### Funciones geoespaciales

- **geocodificar_df()**: Devuelve coordenadas y dirección normalizada usando un servidor local de Nominatim. **¹**
- **normalizar_localidades()**: Normaliza nombres de localidades del partido según la ordenanza N° 2096, usando la capa `localidades` del geoportal.
- **normalizar_calles()**: Normaliza nombres de calles con base en el censo vial.

### Funciones para ARBA

- **obtener_inventario_ARBA()**: Lista las capas disponibles del geoportal de ARBA para el partido.
- **obtener_capa_ARBA()**: Descarga capas específicas del geoportal de ARBA.

### Organización y validación

- **validar_paquetes()**: Verifica si los paquetes necesarios están instalados. Si no, los instala automáticamente.
- **estructurar_directorio()**: Revisa si existe un proyecto y crea subcarpetas `/insumos` y `/productos` si no están.
- **actualizar_utilidades3F()**: Reinstala el paquete desde GitHub. Se puede especificar una rama (por defecto: `estable`).

# **Requisitos**

Para instalar el paquete `utilidades3F` es necesario tener la librería `devtools`:

```r
install.packages("devtools")
devtools::install_github("Datos-3F/utilidades3F")
```

Una vez hecho eso, ya se pueden usar las funciones normalmente.

**¹ IMPORTANTE**: Para usar `geocodificar_df()` es necesario tener un servidor local de Nominatim. Las instrucciones están disponibles en este [repositorio de GitHub](https://github.com/fsavio3F/OpenGeocoding).
