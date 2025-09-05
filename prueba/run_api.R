r <- plumber::plumb("prueba/plumber_geolocalizador_fijo.R")
r$run(host="0.0.0.0", port=8000)
