# descargar datos de la Actualizacion Catastro campamentos 2024
# https://geoportal-open-data-minvu-2-minvu.hub.arcgis.com/datasets/MINVU::actualizacion-catastro-campamentos-2024-1/about

url_geojson <- "https://geoide.minvu.cl/server/rest/services/Catastros/Actualizacion_Catastro_Campamentos_2024/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

# descargar ----

# descargar en formato geojson
download.file(url_geojson,
              "campamentos/datos/catastro_campamentos_2024.geojson")

# probar
library(sf)

sf::read_sf("campamentos/datos/catastro_campamentos_2024.geojson")
