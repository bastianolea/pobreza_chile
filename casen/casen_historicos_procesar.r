# carga todas las casen desde 2009, adjunta sus bases de ingresos,
# y calcula población por nivel de pobreza, región y comuna

library(dplyr)
library(haven)
library(tidyr)
library(purrr)

# datos ----

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


# casen 2022 ----
casen_22 <- readr::read_rds("casen/datos_procesados/casen2022.rds") |> 
  select(region, provincia, comuna, expr, expc, 
         starts_with("pobreza"))

# calcular ----

## pobreza 2009 ----
pobreza_09 <- casen_09 |> 
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
pobreza_11 <- casen_11 |> 
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
pobreza_13 <- casen_13 |> 
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
pobreza_15 <- casen_15 |> 
  filter(!is.na(expc),
         !is.na(pobreza)) |> 
  # expansión
  uncount(expc) |> 
  group_by(region, comuna) |> 
  count(pobreza)|> 
  ungroup() |> 
  mutate(año = 2015)

## pobreza 2017 ----
pobreza_17 <- casen_17 |> 
  filter(!is.na(pobreza)) |> 
  # expansión
  uncount(expc) |> 
  group_by(region, comuna) |> 
  count(pobreza)|> 
  ungroup() |> 
  mutate(año = 2017)

## pobreza 2022 ----
pobreza_22 <- casen_22 |> 
  filter(!is.na(pobreza)) |> 
  # expansión
  uncount(expc) |> 
  group_by(region, comuna) |> 
  count(pobreza)|> 
  ungroup() |> 
  mutate(año = 2022)

# unir ----
# ls(pattern = "pobreza") |> cat(sep = ", ")
pobreza_0 <- list(pobreza_09, pobreza_11, pobreza_13, pobreza_15, pobreza_17, pobreza_22)

pobreza <- pobreza_0 |> 
  map(~mutate(.x, across(c(region, comuna, pobreza), as.character))) |> 
  list_rbind() |> 
  mutate(pobreza = case_match(pobreza, 
                              "1" ~ "Pobres extremos", 
                              "2" ~ "Pobres no extremos",
                              "3" ~ "No pobres"),
         pobreza = as_factor(pobreza))

pobreza

# calcular ----

# pobreza nacional
pobreza |> 
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

# regional: porcentajes, cambio
pobreza |> 
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
  ungroup()


# comunal: porcentajes, cambio
pobreza |> 
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
  ungroup()
