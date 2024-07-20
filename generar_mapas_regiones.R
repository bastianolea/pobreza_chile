library(dplyr)
library(ggplot2)
library(sf)
library(chilemapas)
library(rmapshaper) #simplificar geometrías
library(smoothr) #eliminar islas chicas

# mapas de regiones ----
mapas_regiones <- chilemapas::mapa_comunas |> 
  left_join(chilemapas::codigos_territoriales |> 
              select(matches("comuna")), 
            by = "codigo_comuna") |> 
  mutate(codigo_region = as.numeric(codigo_region)) |> 
  filter(!codigo_comuna %in% c("05104", "05201")) #excluir islas

# simplificar geometrías
mapas_regiones_simplificados <- mapas_regiones |> 
  mutate(geometry = rmapshaper::ms_simplify(geometry, keep = 0.06))

# object.size(mapas_regiones) |> format(units = "auto")
# object.size(mapas_regiones_simplificados) |> format(units = "auto")

# separar en una lista por región
mapas_regiones_split <- mapas_regiones_simplificados |>
  group_by(codigo_region) |> 
  group_split()


# correcciones manuales ----

# eliminar islas muy chicas
mapas_regiones_split[[5]] <- mapas_regiones_split[[5]] |> 
  mutate(geometry = drop_crumbs(geometry, threshold = units::set_units(30, km^2)))

# eliminar islas muy chicas
mapas_regiones_split[[12]] <- mapas_regiones_split[[12]] |>
  mutate(geometry = drop_crumbs(geometry, threshold = units::set_units(100, km^2)))


# visualizar
mapas_regiones_split[[10]] |> 
  ggplot(aes(geometry = geometry)) +
  geom_sf()


# guardar ----
saveRDS(mapas_regiones_split, "app/datos/mapas_regiones.rds", compress = FALSE)



# revisar regiones con islas ----
# mapas_regiones_split[[5]] |> 
#   # filter(codigo_comuna == "05101") |> 
#   # eliminar islas muy chicas
#   mutate(geometry = drop_crumbs(geometry, threshold = units::set_units(30, km^2))) |>
#   ggplot(aes(geometry = geometry)) +
#   geom_sf()
# 
# # area_thresh <- units::set_units(200, km^2)
# # p_dropped <- drop_crumbs(p, threshold = area_thresh)
# 
# mapas_regiones_split[[5]] |> 
#   filter(codigo_comuna != "05101") |> 
#   print(n=Inf) |> 
#   ggplot(aes(geometry = geometry)) +
#   geom_sf(aes(fill = codigo_comuna)) +
#   geom_sf_text(aes(label = codigo_comuna), check_overlap = F) +
#   coord_sf(clip = "off") +
#   theme(legend.position = "bottom")


# mapas_regiones_split[[12]] |>
#   # eliminar islas muy chicas
#   mutate(geometry = drop_crumbs(geometry, threshold = units::set_units(100, km^2))) |>
#   ggplot(aes(geometry = geometry)) +
#   geom_sf()
