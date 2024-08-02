library(dplyr)
library(stringr)
library(ggplot2)
library(sf)
library(chilemapas)

# mapa urbano ----
mapa_zonas_urbanas <- chilemapas::mapa_zonas |> 
  filter(codigo_region == 13) |> 
  left_join(chilemapas::codigos_territoriales |> 
              select(matches("comuna")))

islas_urbanas <- c("13124071004", "13124071005", "13124081001", "13124071001", "13124071002", "13124071003", #Pudahuel
                   "13401121001", #San Bernardo
                   "13119131001", #Maipú
                   "13203031000", "13203031001", "13203031002", "13203011001", "13203011002" #San José de Maipo
)

# crear nuevo mapa
mapa_urbano <- mapa_zonas_urbanas |> 
  # dejar solo dos provincias, incluir San Bernardo y sacar Pirque
  filter(codigo_provincia %in% c(131, 132) | nombre_comuna == "San Bernardo", nombre_comuna != "Pirque") |>
  # filtrar islas urbanas
  filter(!geocodigo %in% islas_urbanas) |>
  # unir comunas
  group_by(nombre_comuna, codigo_comuna) %>%
  summarise(geometry = st_union(geometry)) |>
  ungroup()
# simplificar bordes del mapa (opcional)
# mutate(geometry = rmapshaper::ms_simplify(geometry,  keep = 0.4))


# datos campamentos ----
# carga los datos de campamentos descargados

# geo <- read_sf("datos/2024/FL_Actualizaci%C3%B3n_Campamentos_2024.geojson")
geo <- read_sf("campamentos/datos_originales/catastro_campamentos_2024.geojson")

# limpiar datos y convertir polígonos a puntos
campamentos_rm <- geo |> 
  janitor::clean_names() |> 
  filter(cut_r == 13) |> 
  select(nombre, cut_r, cut, comuna, n_hog, hectareas, geometry) |> 
  # crear puntos a partir de los polígonos
  mutate(punto = geometry |> st_simplify() |> st_centroid(of_largest_polygon = TRUE))

campamentos_rm

# intersectar para que solo sean los del gran santiago
st_crs(mapa_urbano$geometry) <- 4326

st_crs(campamentos_rm$geometry) <- 4326

campamentos_rm_2 <- campamentos_rm |> 
  st_intersection(mapa_urbano$geometry)

# nombres de comunas para el mapa, que se repelan de los puntos del mapa
etiquetas <- mapa_urbano |> 
  filter(codigo_comuna %in% campamentos_rm_2$cut) |> 
  bind_rows(
    campamentos_rm_2 |> 
      select(geometry) |> 
      mutate(nombre_comuna = "")) |> 
  mutate(nombre_comuna = recode(nombre_comuna, 
                                "Maipu" = "Maipú",
                                "Conchali" = "Conchalí",
                                "Estacion Central" = "Estación Central",
                                "San Jose de Maipo" = "San José de Maipo"))