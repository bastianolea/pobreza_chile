# descarga una tabla de excel y la limpia para su posterior uso en tabla_pobreza.R
# y en la aplicación app_pobreza.R

# https://bidat.midesof.cl
# https://bidat.midesof.cl/details/ficha/dataset/b317bf52-693e-44a9-b75e-ffaa72b9482d/ficha-organization/eyJpdiI6IlFSV1pUMGh1N1YxcGlwK25KMEFnb0E9PSIsInZhbHVlIjoiTmtpaHJhaFNPTmd2aC9CYWRyWm9SeUY1Uld1aUVRMjFjcWFCME43ZkczU21TK1hsL3hwd1M2dXNMTmFWR29hVU1pYjVSQ2FndGt1YmdVL1Y1bWp1OGJTM3lvQVBYejZTeWFPR1pNZ1ZMNlE9IiwibWFjIjoiM2Q0YzQ0YzU0YTRkZmQ4OGY0M2NjYmYwNzQ3YjQ4MmQ5MDQ2ZGU1ODZkMDVjOWM4ODUwYjFjZmJlY2Q5NjY3ZiIsInRhZyI6IiJ9

# descargar datos
download.file("https://bid-ckan.ministeriodesarrollosocial.gob.cl/dataset/b317bf52-693e-44a9-b75e-ffaa72b9482d/resource/71997eac-b660-4fd3-9f5d-61b5656491ad/download/estimaciones_indice_pobreza_multidimensional_comunas_2022.xlsx",
              "pobreza_comunal/datos_originales/pobreza_comunal.xlsx")

library(readxl)
library(janitor)
library(dplyr)

# cargar dato de excel
dato <- readxl::read_excel("pobreza_comunal/datos_originales/pobreza_comunal.xlsx")

# limpiar
dato_2 <- dato |> 
  janitor::row_to_names(2) |> 
  janitor::clean_names() |> 
  rename(poblacion = 4,
         pobreza_n = 5, 
         pobreza_p = 6) |> 
  select(1:8) |> 
  filter(!is.na(region))

# convertir columnas a su formato correcto
dato_3 <- dato_2 |> 
  mutate(across(c(poblacion, pobreza_n, pobreza_p, starts_with("limite")),
                as.numeric))

# guardar resultado
readr::write_rds(dato_3, "pobreza_comunal/datos_procesados/pobreza_comunal.rds")
readr::write_rds(dato_3, "app/datos/pobreza_comunal.rds")
