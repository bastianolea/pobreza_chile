library(sf)
library(chilemapas)
library(ggplot2)
library(readr)
library(dplyr)
library(haven)

# cargar datos
casen_comuna <- readr::read_rds("casen/datos_procesados/casen_pobreza_comuna.rds")

# obtener mapa
mapa_comuna <- chilemapas::mapa_comunas |> 
  filter(codigo_region == 11) |> 
  # simplificar geometrías
  mutate(geometry = rmapshaper::ms_simplify(geometry, keep = 0.05))

object.size(mapa_comuna) |> 
  format(units = "auto")

# unir mapa con datos
mapa_comuna_datos <- mapa_comuna |> 
  mutate(codigo_comuna = as.numeric(codigo_comuna)) |> 
  left_join(casen_comuna |> 
              mutate(cut_comuna = as.numeric(cut_comuna)), 
            by = c("codigo_comuna" = "cut_comuna"))

# visualizar
mapa_comuna_datos |> 
  ggplot(aes(fill = pobreza_p, geometry = geometry)) +
  geom_sf(color = "white") +
  scale_fill_gradient(low = "grey80", high = "#581695", na.value = "white", limits = c(0, NA)) +
  theme_void() +
  guides(fill = guide_none())
