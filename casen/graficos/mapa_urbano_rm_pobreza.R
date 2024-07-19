library(sf)
library(chilemapas)
library(ggplot2)
library(readr)
library(dplyr)
library(haven)

# cargar datos
casen_comuna <- readr::read_rds("casen/datos_procesados/casen_pobreza_comuna.rds")

# obtener mapa
mapa_zonas_urbanas <- chilemapas::mapa_zonas |>
  filter(codigo_region == 13) |> 
  left_join(chilemapas::codigos_territoriales |> 
              select(matches("comuna")))
  # simplificar geometrías
  # mutate(geometry = rmapshaper::ms_simplify(geometry, keep = 0.05))

object.size(mapa_zonas_urbanas) |> 
  format(units = "auto")


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
  group_by(codigo_comuna) %>%
  summarise(geometry = st_union(geometry)) |> 
  ungroup()
  
# unir mapa con datos
mapa_urbano_datos <- mapa_urbano |> 
  mutate(codigo_comuna = as.numeric(codigo_comuna)) |> 
  left_join(casen_comuna |> 
              mutate(cut_comuna = as.numeric(cut_comuna)), 
            by = c("codigo_comuna" = "cut_comuna"))

# visualizar
mapa_urbano_datos |> 
  ggplot(aes(fill = pobreza_p, geometry = geometry)) +
  geom_sf(color = "white") +
  scale_fill_gradient(low = "grey80", high = "#581695", limits = c(0, NA)) +
  theme_void() +
  guides(fill = guide_none())
  coord_sf(xlim = c(-76, -67))
