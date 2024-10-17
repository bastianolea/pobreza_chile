# script que carga el dato de pobreza limpiado en obtencion_pobreza.R
# y genera una tabla usango {gt}
# este script nos servirá como base para nuestra app

library(dplyr)
library(gt)

pobreza <- readr::read_rds("pobreza_comunal/datos_procesados/pobreza_comunal.rds") |> 
  filter(!is.na(region))

regiones <- unique(pobreza$region)

# pseudo aplicación
pobreza_filtrado <- pobreza |> 
  filter(region == regiones[7]) |> 
  arrange(desc(pobreza_p))

tabla <- pobreza_filtrado |> 
  select(nombre_comuna, poblacion, pobreza_n, pobreza_p) |> 
  gt() |> 
  gt::fmt_percent(pobreza_p, decimals = 1) |> 
  gt::fmt_number(c(poblacion, pobreza_n), 
                 decimals = 0, sep_mark = ".") |> 
  gt::data_color(columns = c(pobreza_n, pobreza_p),
                 method = "numeric", palette = "viridis")
