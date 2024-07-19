library(sf)
library(chilemapas)
library(ggplot2)
library(readr)
library(dplyr)
library(haven)

# cargar datos
casen_region <- readr::read_rds("casen/datos_procesados/casen_pobreza_region.rds")

# obtener mapa
mapa_region <- chilemapas::generar_regiones() |>
  # simplificar geometrías
  mutate(geometry = rmapshaper::ms_simplify(geometry, keep = 0.05))

object.size(mapa_region) |> 
  format(units = "auto")

# unir mapa con datos
mapa_region_datos <- mapa_region |> 
  mutate(codigo_region = as.numeric(codigo_region)) |> 
  left_join(casen_region |> 
              mutate(region = as.numeric(region)), 
            by = c("codigo_region" = "region"))

# visualizar
mapa_region_datos |> 
  ggplot(aes(fill = pobreza_p, geometry = geometry)) +
  geom_sf(color = "white") +
  scale_fill_gradient(low = "grey80", high = "#581695", limits = c(0, NA)) +
  theme_void() +
  guides(fill = guide_none()) +
  coord_sf(xlim = c(-76, -67))


# https://gist.github.com/ryanpeek/99c6935ae51429761f5f73cf3b027da2
# rotate function (see here: https://r-spatial.github.io/sf/articles/sf3.html#affine-transformations
rotate <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

# make a new geometry that is rotated 90 degrees (but retain original geom)
mapa_region_datos_rotado <- mapa_region_datos %>% 
  mutate(geometry = geometry * rotate(-pi/2)) 

mapa_region_datos_rotado |> 
  ggplot(aes(fill = pobreza_p, geometry = geometry)) +
  geom_sf(color = "white") +
  scale_fill_gradient(low = "grey80", high = "#581695", limits = c(0, NA)) +
  theme_void() +
  guides(fill = guide_none()) +
  coord_sf(ylim = c(-76, -67))
