library(dplyr)
library(haven)
library(tidyr)

casen_09 <- read_dta("casen/datos_originales/casen2009.dta",
                     col_select = c(region, comuna, zona,
                                    starts_with("expr"), starts_with("expc"), 
                                    starts_with("pobreza"))
) |> 
  mutate(año == 2009)

casen_11 <- read_dta("casen/datos_originales/casen2011.dta",
                     col_select = c(region, comuna, zona,
                                    starts_with("expr"), starts_with("expc"), 
                                    starts_with("pobreza"))
) |> 
  mutate(año == 2011)

casen_15 <- read_dta("casen/datos_originales/casen2015.dta",
                     col_select = c(region, provincia, comuna, zona, expr, expc, 
                                    starts_with("pobreza"))
) |> 
  mutate(año == 2015)

casen_17 <- read_dta("casen/datos_originales/casen2017.dta", 
                     col_select = c(region, provincia, comuna, zona, expr, expc, 
                                    starts_with("pobreza"))
) |> 
  mutate(año == 2017)




casen_17 |> 
  uncount(expc) |> 
  group_by(region, comuna) |> 
  count(pobreza)
