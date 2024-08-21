# probar calculo de casen con survey versus solo aplicar expansión de filas

library(dplyr)
library(haven)
library(survey)

casen <- read_dta("casen/datos_originales/Base de datos Casen 2022 STATA.dta")

casen2 <- mutate(casen, 
                 pobreza_extrema = case_when( pobreza==1 ~1, TRUE~ 0),
                 pobreza_no_extrema = case_when( pobreza==2 ~1, TRUE~ 0),
                 no_pobreza = case_when( pobreza==3 ~1, TRUE~ 0))

casen2 |> count(pobreza_no_extrema)

disenio = svydesign(id=~varunit, # Etiquetas UPM
                    strata=~varstrat, #Estratos
                    check.strata=TRUE, # Comprueba que los clusters est?n anidados en los estratos
                    weights=~expr, # Ponderador
                    data=casen2)

options(survey.lonely.psu="remove") 



pobreza2 <- svyby(~pobreza_no_extrema, 
                 by=~region,
                 data=casen2,
                 drop.empty.groups=FALSE,
                 na.rm.all=FALSE,
                 disenio,
                 svytotal,
                 vartype=c("se","cv"))

pobreza3 <- svyby(~no_pobreza, 
                  by=~region,
                  data=casen2,
                  drop.empty.groups=FALSE,
                  na.rm.all=FALSE,
                  disenio,
                  svytotal,
                  vartype=c("se","cv"))

pobreza1 <- svyby(~pobreza_extrema, 
                  by=~region,
                  data=casen2,
                  drop.empty.groups=FALSE,
                  na.rm.all=FALSE,
                  disenio,
                  svytotal,
                  vartype=c("se","cv"))

pobreza_data <- bind_rows(
pobreza1 |> 
  rename(n = 2) |> 
  mutate(pobreza = 1),
pobreza2 |> 
  rename(n = 2) |> 
  mutate(pobreza = 2),
pobreza3 |> 
  rename(n = 2) |> 
  mutate(pobreza = 3)
) |> 
  tibble()

pobreza_a <- pobreza_data |> 
  group_by(region) |> 
  mutate(p = n/sum(n),
         p = round(p, 3)) |> 
  filter(pobreza == 2)

pobreza_b <- pobreza_22 |> 
  group_by(region, pobreza, año) |> 
  summarize(n = sum(n)) |> 
  # calcular porcentajes
  group_by(region, año) |> 
  mutate(p = n/sum(n),
         p = round(p, 3)) |> 
  filter(pobreza == 2)


pobreza_a
pobreza_b

# las diferencias son de 1%