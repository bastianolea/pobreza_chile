library(sf)
library(chilemapas)
library(readr)
library(dplyr)

# chilemapas::codigos_territoriales |> 
#   select(codigo_region, nombre_region) |> 
#   distinct() |> 
#   datapasta::dpasta()

nombres_regiones <- tibble::tribble(
  ~codigo_region,                    ~nombre_region,
  "01",                                  "Tarapacá",
  "02",                               "Antofagasta",
  "03",                                   "Atacama",
  "04",                                  "Coquimbo",
  "05",                                "Valparaíso",
  "06",       "Libertador Gral. Bernardo O'Higgins",
  "07",                                     "Maule",
  "08",                                    "Biobío",
  "09",                              "La Araucanía",
  "10",                                 "Los Lagos",
  "11",   "Aysén del Gral. Carlos Ibáñez del Campo",
  "12",      "Magallanes y de la Antártica Chilena",
  "13",                 "Metropolitana de Santiago",
  "14",                                  "Los Ríos",
  "15",                        "Arica y Parinacota",
  "16",                                     "Ñuble"
)

# obtener mapa
mapa_region <- chilemapas::generar_regiones() |>
  # simplificar geometrías
  mutate(geometry = rmapshaper::ms_simplify(geometry, keep = 0.05)) |> 
  # adjuntar nombres de regiones
  left_join(nombres_regiones) |> 
  mutate(codigo_region = as.numeric(codigo_region)) |> 
  relocate(nombre_region, .after = 1)

# guardar
write_rds(mapa_region, "app/datos/mapa_pais.rds")
