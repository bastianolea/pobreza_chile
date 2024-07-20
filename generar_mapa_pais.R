library(sf)
library(chilemapas)
library(readr)
library(dplyr)

# obtener mapa
mapa_region <- chilemapas::generar_regiones() |>
  # simplificar geometrías
  mutate(geometry = rmapshaper::ms_simplify(geometry, keep = 0.05)) |> 
  mutate(codigo_region = as.numeric(codigo_region))

write_rds(mapa_region, "app/datos/mapa_pais.rds")
