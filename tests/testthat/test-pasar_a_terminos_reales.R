tabla_ipc_base <- data.frame(
  indice_tiempo = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
  valor_ipc = c(1, 1.1, 1.2)
)

test_that("exige que tabla_ipc tenga las columnas esperadas", {
  tabla_incompleta <- data.frame(indice_tiempo = as.Date("2024-01-01"))

  expect_error(
    pasar_a_terminos_reales(
      data.frame(fecha = as.Date("2024-01-01"), valor = 100),
      "fecha", "valor", tabla_incompleta
    ),
    "no tiene el formato esperado"
  )
})

test_that("convierte valores nominales a reales usando la tabla de IPC", {
  df <- data.frame(
    fecha = as.Date(c("2024-01-01", "2024-02-01")),
    valor = c(100, 220)
  )

  resultado <- suppressMessages(
    pasar_a_terminos_reales(df, "fecha", "valor", tabla_ipc_base)
  )

  expect_equal(resultado$valor_tr, c(100, 200))
})

test_that("acepta fechas en formato 'YYYY-MM' además de fechas completas", {
  df <- data.frame(
    fecha = c("2024-01", "2024-02"),
    valor = c(100, 220)
  )

  resultado <- suppressMessages(
    pasar_a_terminos_reales(df, "fecha", "valor", tabla_ipc_base)
  )

  expect_equal(resultado$valor_tr, c(100, 200))
})

test_that("avisa cuando valores no numéricos se convierten en NA", {
  df <- data.frame(
    fecha = as.Date(c("2024-01-01", "2024-02-01")),
    valor = c("100", "no-es-numero")
  )

  expect_warning(
    suppressMessages(pasar_a_terminos_reales(df, "fecha", "valor", tabla_ipc_base)),
    "se convirtieron en NA"
  )
})

test_that("si no hay un mes base con valor_ipc == 1, mes_base queda NA (sin dividir por él)", {
  # OJO: dplyr::first() sobre un resultado vacío devuelve NA (no un vector de
  # largo 0), así que el chequeo `length(mes_base) == 0` del código nunca se
  # cumple y el warning documentado ("No se detectó un mes base...") es
  # código muerto. Este test documenta el comportamiento real, no lo que
  # dice el mensaje de warning.
  tabla_sin_base <- data.frame(
    indice_tiempo = as.Date(c("2024-02-01", "2024-03-01")),
    valor_ipc = c(1.1, 1.2)
  )
  df <- data.frame(fecha = as.Date("2024-02-01"), valor = 110)

  expect_no_warning(
    resultado <- suppressMessages(
      pasar_a_terminos_reales(df, "fecha", "valor", tabla_sin_base)
    )
  )
  # El cálculo en sí sigue andando bien (usa el coeficiente de su propio mes),
  # lo único afectado es el "Base: ..." informativo del mensaje.
  expect_equal(resultado$valor_tr, round(110 / 1.1, 2))
  expect_message(
    pasar_a_terminos_reales(df, "fecha", "valor", tabla_sin_base),
    "Base: NA"
  )
})

test_that("reporta registros con fecha válida pero sin dato de IPC correspondiente", {
  df <- data.frame(
    fecha = as.Date(c("2024-01-01", "2099-01-01")),
    valor = c(100, 500)
  )

  expect_message(
    pasar_a_terminos_reales(df, "fecha", "valor", tabla_ipc_base),
    "no encontraron dato en tabla_ipc"
  )
})
