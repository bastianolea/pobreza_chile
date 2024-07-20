library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(ggtext)

canasta <- read_csv2("canasta_basica/datos_procesados/canasta_basica_alimentos_2018-2024.csv")


linea_pobreza <- canasta |> 
  filter(variable %in% c("LP", "LPE")) |> 
  mutate(variable = recode(variable,
                           "LP" = "Línea de pobreza",
                           "LPE" = "Línea de pobreza extrema"))

  
linea_pobreza |> 
  filter(year(fecha) >= 2020) |> 
  ggplot(aes(fecha, valor, color = variable)) +
  geom_line(linewidth = 1.2) +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","), 
                     n.breaks = 8) +
  scale_color_manual(values = c("Línea de pobreza" = "red",
                                "Línea de pobreza extrema" = "red4")) +
  guides(color = guide_legend(position = "bottom")) +
  theme_classic() +
  theme(panel.grid.major.x = element_line()) +
  theme(plot.title = ggtext::element_textbox()) +
  labs(title = "Líneas de <span style='color:red;'>pobreza</span> y <span style='color:red4'>pobreza extrema</span>")
  
