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
  coord_sf(xlim = c(-76, -67)) +
  labs(title = "Mapa simplificado",
       subtitle = paste("Memoria:", object.size(mapa_region) |> 
                          format(units = "auto")))

# library(patchwork)
# p2 + plot_spacer() + p1 + plot_layout(widths = c(2, .1, 2))
