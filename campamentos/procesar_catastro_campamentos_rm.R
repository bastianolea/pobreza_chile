library(dplyr)
library(sf)
library(janitor)
library(readr)

# datos campamentos ----
# carga los datos de campamentos descargados

geo <- read_sf("campamentos/datos_originales/catastro_campamentos_2024.geojson")

# limpiar datos y convertir polígonos a puntos
campamentos <- geo |> 
  janitor::clean_names() |> 
  # filter(cut_r == 13) |> 
  select(nombre, cut_r, cut, comuna, n_hog, hectareas, geometry) |> 
  mutate(geometry = st_make_valid(geometry)) |> 
  mutate(geometry = st_simplify(geometry)) |> 
  # crear puntos a partir de los polígonos
  mutate(punto = geometry |> 
           st_simplify() |> 
           st_centroid(of_largest_polygon = TRUE))

# guardar sólo puntos
campamentos |> 
  select(-geometry) |> 
  write_rds("campamentos/datos_procesados/campamentos_chile_puntos.rds")

campamentos |> 
  write_rds("campamentos/datos_procesados/campamentos_chile_poligonos.rds")

# guardar en app
campamentos |> 
  select(-geometry) |> 
  write_rds("app/datos/campamentos_chile_puntos.rds")
