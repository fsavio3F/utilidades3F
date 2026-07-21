# utilidades3F 0.2.0 (sin publicar)

## Nuevas funcionalidades

* `crear_conexiones()` y `configurar_credenciales_db()` ahora soportan
  `db_type = "oracle"`: validación de `db_type` contra una lista cerrada de
  motores soportados, y exigencia obligatoria (fail-fast) del parámetro
  `driver` para `sqlserver`.
* Oracle admite dos mecanismos de conexión elegidos explícitamente por el
  parámetro `driver_type` (obligatorio, sin default, para `db_type =
  "oracle"`):
  - `"jdbc"`: vía `RJDBC`, descargando y cacheando automáticamente el driver
    oficial de Oracle (`ojdbc8.jar`, publicado en Maven Central) en la
    caché de usuario de R (`tools::R_user_dir()`), sin necesidad de copiar
    el `.jar` a mano en cada proyecto. Requiere una JVM instalada
    (dependencia de `rJava`/`RJDBC`).
  - `"odbc"`: vía el paquete `odbc`, contra un driver ODBC de Oracle ya
    instalado y registrado en el sistema (comportamiento previo), exigiendo
    también el parámetro `driver` (fail-fast) y construyendo
    automáticamente la sintaxis EZConnect (`host:puerto/service_name`)
    para el parámetro `DBQ`.

## Correcciones de errores

* `configurar_credenciales_db()` reemplaza la construcción manual de bloques
  YAML por `paste0()`/`cat()` por serialización real con el paquete `yaml`,
  preservando las etiquetas `!expr keyring::key_get(...)` que
  `config::get()` necesita evaluar. Esto elimina el riesgo de generar YAML
  inválido cuando `server`/`database` contienen caracteres especiales, y
  reemplaza el borrado de líneas por rango (basado en una expresión regular
  frágil) por un merge estructurado sobre el árbol YAML real.
* Se corrige la creación de un `config.yml` nuevo: la clave `default:` se
  escribe ahora como mapa vacío (`default: {}`) en lugar de un valor nulo,
  evitando que `config::get()` falle con
  "You must provide a default configuration" la primera vez que se usa la
  función sobre un proyecto sin `config.yml` previo.

## Otros cambios

* Se agrega infraestructura de tests automatizados (`testthat`, 3ª
  edición), cubriendo la conversión a términos reales, el armado de
  argumentos de conexión por motor de base de datos, y la escritura/fusión
  de YAML de credenciales. Las funciones que dependen de red (geoportal de
  3F, ARBA, INDEC, Nominatim) o de flujos interactivos/OAuth2 quedan
  explícitamente fuera del alcance automatizado; ver
  `tests/testthat/README.md` para las notas de verificación manual.
* El workflow de CI (`R CMD check`) corre ahora también contra las
  versiones `release` y `oldrel-1` de R además de la versión `4.5.1` ya
  fijada, y se agrega `ubuntu-latest` y `macos-latest` a la matriz (antes
  corría exclusivamente en `windows-latest`). El paso de instalación de
  dependencias pasa a `r-lib/actions/setup-r-dependencies`, que resuelve
  los requisitos de sistema por SO (GDAL/GEOS/PROJ para `sf`, unixODBC para
  `odbc`, JDK para `rJava`/`RJDBC`, etc.).

# utilidades3F 0.1.2

* Primera versión con historial de cambios documentado.
