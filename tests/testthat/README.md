# Qué no está cubierto por tests automatizados (y cómo verificarlo a mano)

Estas funciones dependen de red real, de flujos interactivos u OAuth2, o de
mutar el sistema (instalar paquetes, reinstalar el propio paquete). No se
mockean acá porque el costo de la infraestructura de mocking no compensa el
valor, o porque el side effect es justamente lo que hay que probar contra el
servicio real. Verificación manual sugerida antes de un release:

- **`generar_tabla_ipc()`**: corre la función con conexión a internet y
  confirmá que la tabla resultante tiene columnas `indice_tiempo`/`valor_ipc`
  y que el mes con `valor_ipc == 1` coincide con el mes base esperado.
- **`obtener_capa_ARBA()` / `inventario_ARBA()`**: corré en un directorio de
  prueba (no en el repo) y confirmá que descarga y descomprime el shapefile
  en `insumos/ARBA`, y que `inventario_ARBA()` lista las capas disponibles.
- **`autenticar_geoportal()`**: corré el flujo OAuth2 completo una vez,
  confirmá que el token queda cacheado en el directorio de usuario
  (`tools::R_user_dir`) y que una segunda llamada reutiliza el token sin
  pedir login de nuevo. Ojo: reescribe `~/.Renviron`, no correr contra un
  `~/.Renviron` que tenga configuración que no se pueda perder.
- **`obtener_capa()` / `obtener_inventario()`**: probar tanto el camino
  autenticado como el anónimo (este último corre en un subproceso vía
  `callr::r()`); confirmar que devuelve un objeto `sf` válido para al menos
  una capa pública y una privada.
- **`actualizar_utilidades3F()`**: correr contra una rama de prueba y
  confirmar que reinstala el paquete y que `packageVersion("utilidades3F")`
  refleja la nueva versión.
- **`validar_paquetes()`**: correr en un entorno sin alguno de los paquetes
  requeridos y confirmar que los instala antes de continuar. No se mockea
  `install.packages()`/`library()` para no sumar más stubs de funciones base
  sin necesidad concreta.

Si en el futuro se justifica automatizar alguno de estos casos (por ejemplo
extrayendo la lógica de armado de URL/parseo de `generar_tabla_ipc()` en una
función pura, separada de la descarga), agregar el test correspondiente acá
en vez de una nota manual.
