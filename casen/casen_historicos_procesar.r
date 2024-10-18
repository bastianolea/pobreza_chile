# carga todas las casen desde 2009, adjunta sus bases de ingresos,
# y calcula población por nivel de pobreza, región y comuna

library(dplyr)
library(haven)
library(tidyr)
library(purrr)

# datos ----

cut_comunas <- readr::read_csv2("otros/comunas_chile_cut.csv") |> 
  mutate(cut_comuna = as.character(cut_comuna),
         cut_region = as.character(cut_region))

## casen 2009 ----
casen_09_pob <- read_dta("casen/datos_originales/ingresos_mn_2009.dta",
                         col_select = c(segmento, idviv, hogar, o, starts_with("pobreza")))


casen_09 <- read_dta("casen/datos_originales/casen2009.dta",
                     col_select = c(segmento, idviv, hogar, o, 
                                    region, comuna, zona,
                                    starts_with("expr"), starts_with("expc"), 
                                    starts_with("pobreza"))
)


## casen 2011 ----
casen_11_pob <- read_dta("casen/datos_originales/ingresos_mn_2011.dta",
                         col_select = c(folio, o, starts_with("pobreza")))

casen_11 <- read_dta("casen/datos_originales/casen2011stata_03122013stata.dta", 
                     encoding = "latin1",
                     col_select = c(folio, o, region, comuna, zona,
                                    starts_with("expr"), starts_with("expc"), 
                                    starts_with("pobreza"))
)


## casen 2013 ----
casen_13_pob <- read_dta("casen/datos_originales/casen_2013_ymt.dta", 
                         col_select = c(folio, o, starts_with("pobreza")))

casen_13 <- read_dta("casen/datos_originales/casen_2013_mn_b_principal.dta",
                     col_select = c(folio, o, region, comuna, zona, expr, expc, 
                                    starts_with("pobreza"))
)


## casen 2015 ----
casen_15 <- read_dta("casen/datos_originales/casen2015.dta",
                     col_select = c(region, provincia, comuna, zona, expr, expc, 
                                    starts_with("pobreza"))
)


## casen 2017 ----
casen_17 <- read_dta("casen/datos_originales/casen2017.dta", 
                     col_select = c(region, provincia, comuna, zona, expr, expc, 
                                    starts_with("pobreza"))
) 


## casen 2022 ----
casen_22 <- readr::read_rds("casen/datos_procesados/casen2022.rds") |> 
  select(region, provincia, comuna, expr, expc, 
         starts_with("pobreza"))

# calcular comunas ----

## pobreza 2009 ----
pobreza_09_com <- casen_09 |> 
  left_join(casen_09_pob, by = c("segmento", "idviv", "hogar", "o")) |> 
  rename(pobreza = pobreza_mn) |> 
  filter(!is.na(expc_p),
         !is.na(pobreza)) |> 
  uncount(expc_p) |> 
  group_by(region, comuna) |> 
  # expansión
  count(pobreza)|> 
  ungroup() |> 
  mutate(año = 2009)

## pobreza 2011 ----
pobreza_11_com <- casen_11 |> 
  left_join(casen_11_pob, by = c("folio", "o")) |> 
  rename(pobreza = pobreza_mn) |> 
  filter(!is.na(expc_r2),
         !is.na(pobreza)) |>
  # expansión
  uncount(expc_r2) |> 
  group_by(region, comuna) |> 
  count(pobreza)|> 
  ungroup() |> 
  mutate(año = 2011)

## pobreza 2013 ----
pobreza_13_com <- casen_13 |> 
  left_join(casen_13_pob, by = c("folio", "o")) |> 
  rename(pobreza = pobreza_mt) |> 
  filter(!is.na(expc),
         !is.na(pobreza)) |> 
  # expansión
  uncount(expc) |> 
  group_by(region, comuna) |> 
  count(pobreza)|> 
  ungroup() |> 
  mutate(año = 2013)

## pobreza 2015 ----
pobreza_15_com <- casen_15 |> 
  filter(!is.na(expc),
         !is.na(pobreza)) |> 
  # expansión
  uncount(expc) |> 
  group_by(region, comuna) |> 
  count(pobreza)|> 
  ungroup() |> 
  mutate(año = 2015)

## pobreza 2017 ----
pobreza_17_com <- casen_17 |> 
  filter(!is.na(pobreza)) |> 
  # expansión
  uncount(expc) |> 
  group_by(region, comuna) |> 
  count(pobreza)|> 
  ungroup() |> 
  mutate(año = 2017)

## pobreza 2022 ----
pobreza_22_com <- casen_22 |> 
  filter(!is.na(pobreza)) |> 
  # expansión
  uncount(expc) |> 
  group_by(region, comuna) |> 
  count(pobreza)|> 
  ungroup() |> 
  mutate(año = 2022)


# calcular regiones ----

## pobreza 2009 ----
pobreza_09_reg <- casen_09 |> 
  left_join(casen_09_pob, by = c("segmento", "idviv", "hogar", "o")) |> 
  rename(pobreza = pobreza_mn) |> 
  filter(!is.na(expr_p),
         !is.na(pobreza)) |> 
  uncount(expr_p) |> 
  group_by(region) |> 
  # expansión
  count(pobreza)|> 
  ungroup() |> 
  mutate(año = 2009)

## pobreza 2011 ----
pobreza_11_reg <- casen_11 |> 
  left_join(casen_11_pob, by = c("folio", "o")) |> 
  rename(pobreza = pobreza_mn) |> 
  filter(!is.na(expr_r2),
         !is.na(pobreza)) |>
  # expansión
  uncount(expr_r2) |> 
  group_by(region) |> 
  count(pobreza)|> 
  ungroup() |> 
  mutate(año = 2011)

## pobreza 2013 ----
pobreza_13_reg <- casen_13 |> 
  left_join(casen_13_pob, by = c("folio", "o")) |> 
  rename(pobreza = pobreza_mt) |> 
  filter(!is.na(expr),
         !is.na(pobreza)) |> 
  # expansión
  uncount(expr) |> 
  group_by(region) |> 
  count(pobreza)|> 
  ungroup() |> 
  mutate(año = 2013)

## pobreza 2015 ----
pobreza_15_reg <- casen_15 |> 
  filter(!is.na(expr),
         !is.na(pobreza)) |> 
  # expansión
  uncount(expr) |> 
  group_by(region) |> 
  count(pobreza)|> 
  ungroup() |> 
  mutate(año = 2015)

## pobreza 2017 ----
pobreza_17_reg <- casen_17 |> 
  filter(!is.na(pobreza)) |> 
  # expansión
  uncount(expr) |> 
  group_by(region) |> 
  count(pobreza)|> 
  ungroup() |> 
  mutate(año = 2017)

## pobreza 2022 ----
pobreza_22_reg <- casen_22 |> 
  filter(!is.na(pobreza)) |> 
  # expansión
  uncount(expr) |> 
  group_by(region) |> 
  count(pobreza)|> 
  ungroup() |> 
  mutate(año = 2022)

# unir ----
# ls(pattern = "pobreza") |> cat(sep = ", ")
pobreza_comuna_0 <- list(pobreza_09_com, pobreza_11_com, pobreza_13_com, pobreza_15_com, pobreza_17_com, pobreza_22_com)

pobreza_region_0 <- list(pobreza_09_reg, pobreza_11_reg, pobreza_13_reg, pobreza_15_reg, pobreza_17_reg, pobreza_22_reg)

pobreza_comuna_1 <- pobreza_comuna_0 |> 
  map(~mutate(.x, across(c(region, comuna, pobreza), as.character))) |> 
  list_rbind() |> 
  mutate(pobreza = case_match(pobreza, 
                              "1" ~ "Pobres extremos", 
                              "2" ~ "Pobres no extremos",
                              "3" ~ "No pobres"),
         pobreza = as_factor(pobreza))

pobreza_region_1 <- pobreza_region_0 |> 
  map(~mutate(.x, across(c(region, pobreza), as.character))) |> 
  list_rbind() |> 
  mutate(pobreza = case_match(pobreza, 
                              "1" ~ "Pobres extremos", 
                              "2" ~ "Pobres no extremos",
                              "3" ~ "No pobres"),
         pobreza = as_factor(pobreza))

pobreza_comuna_1
pobreza_region_1


# calcular ----

## pobreza nacional ----
pobreza_nacional <- pobreza_region_1 |> 
  group_by(pobreza, año) |> 
  summarize(n = sum(n)) |> 
  # calcular porcentajes
  group_by(año) |> 
  mutate(p = n/sum(n),
         p = round(p, 3)) |> 
  # calcular diferencias anuales 
  arrange(año) |> 
  group_by(pobreza) |> 
  mutate(d = p - lag(p),
         d = ifelse(is.na(d), 0, d),
         d = round(d, 2)) |> 
  ungroup()

## regional: porcentajes, cambio ----
pobreza_region_2 <- pobreza_region_1 |> 
  group_by(region, pobreza, año) |> 
  summarize(n = sum(n)) |> 
  # calcular porcentajes
  group_by(region, año) |> 
  mutate(p = n/sum(n),
         p = round(p, 2)) |> 
  ungroup() |> 
  # calcular diferencias anuales 
  arrange(region, año) |> 
  group_by(region, pobreza) |> 
  mutate(d = p - lag(p),
         d = ifelse(is.na(d), 0, d),
         d = round(d, 2)) |> 
  ungroup() |> 
  rename(cut_region = region) |> 
  left_join(cut_comunas |> select(3, 4) |> distinct(), by = join_by(cut_region))


## comunal: porcentajes, cambio ----
pobreza_comuna_2 <- pobreza_comuna_1 |> 
  # calcular porcentajes
  group_by(region, comuna, año) |> 
  mutate(p = n/sum(n),
         p = round(p, 2)) |> 
  ungroup() |> 
  # calcular diferencias anuales 
  arrange(region, comuna, año) |> 
  group_by(region, comuna, pobreza) |> 
  mutate(d = p - lag(p),
         d = ifelse(is.na(d), 0, d),
         d = round(d, 2)) |> 
  ungroup() |> 
  rename(cut_comuna = comuna, cut_region = region) |> 
  left_join(cut_comunas, by = join_by(cut_comuna, cut_region))


# guardar ----
pobreza_historico <- bind_rows(
  pobreza_nacional |> mutate(nivel = "pais"),
  pobreza_region_2 |> mutate(nivel = "region"),
  pobreza_comuna_2 |> mutate(nivel = "comuna")
)

readr::write_rds(pobreza_historico, "casen/datos_procesados/pobreza_historico.rds")

# copiar a la app
readr::write_rds(pobreza_historico, "app/datos/pobreza_historico.rds")
