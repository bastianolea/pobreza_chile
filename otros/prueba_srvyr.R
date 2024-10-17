library(dplyr)
library(haven)
library(srvyr)

casen <- read_dta("casen/datos_originales/Base de datos Casen 2022 STATA.dta")


casen2 <- mutate(casen, 
                 pobreza_total = ifelse(pobreza %in% c(1, 2), 1, 0),
                 pobreza_extrema = ifelse(pobreza == 1, 1, 0),
                 pobreza_no_extrema = ifelse(pobreza == 2, 1, 0),
                 no_pobreza = ifelse(pobreza == 3, 1, 0))

# sin nada
casen2 |> 
  count(pobreza_no_extrema) |> 
  mutate(p = n/sum(n))

# con uncount
casen2 |> 
  select(expr, starts_with("pobreza")) |> 
  tidyr::uncount(expr) |> 
  count(pobreza_no_extrema) |> 
  mutate(p = n/sum(n))


# con survey
svy_casen <- casen2 |> 
  as_survey(weights = expr, strata = estrato, 
            ids = id_persona, nest = TRUE)

svy_casen |> 
  group_by(pobreza_no_extrema) |> 
  summarize(n = survey_total(),
            p = survey_mean()) #vartype = c("se", "ci")))
