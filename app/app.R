library(shiny)
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(ggiraph)
library(bslib)
library(thematic)

# cargar datos ----
casen_pais <- read_rds("datos/casen_pobreza_pais.rds")
casen_comuna <- read_rds("datos/casen_pobreza_comuna.rds")
casen_region <- read_rds("datos/casen_pobreza_region.rds")

mapa_pais <- read_rds("datos/mapa_pais.rds")
mapas_regiones <- read_rds("datos/mapas_regiones.rds")

colores <- list("fondo" = "#181818",
                "texto" = "black",
                "bajo" = "#999999")


ui <- fluidPage(
  theme = bs_theme(bg = colores$fondo, fg = colores$texto),
  
  fluidRow(
    column(4, #style = "border: 1px red solid;",
           div(#style = "max-height: 600px;",
           girafeOutput("mapa_pais", height = 700)#, width = 200, height = 600)
           )
    ),
    column(8, #style = "border: 1px blue solid;",
           div(#style = "max-height: 200px;",
           girafeOutput("mapa_region", height = 400)#, width = 300, height = 400)
           )
    )
  )
)



server <- function(input, output) {
  
  # mapas ----
  
  ## mapa país ----
  output$mapa_pais <- renderGirafe({
    # unir mapa con datos
    mapa_pais_datos <- mapa_pais |> 
      left_join(casen_region,
                by = c("codigo_region" = "region"))
    
    # gráfico
    p <- mapa_pais_datos |> 
      ggplot(aes(fill = pobreza_p, 
                 geometry = geometry,
                 data_id = codigo_region) #variable de la que depende el hover y selección
      ) + 
      geom_sf_interactive(color = colores$fondo) +
      scale_fill_gradient(low = colores$bajo, high = "#581695", limits = c(0, NA)) +
      theme_void() +
      guides(fill = guide_none()) +
      coord_sf(xlim = c(-76, -67), expand = FALSE)
    
    # crear gráfico interactivo
    girafe(ggobj = p,
           bg = colores$fondo,
           width_svg = 2,
           height_svg = 8,
           options = list(
             # estilos de selección y hover
             opts_selection(css = "", type = "single", selected = 13),
             opts_hover_inv(css = "opacity:0.5;"),
             opts_hover(css = ""),
             # otros estilos
             opts_sizing(rescale = TRUE),
             opts_toolbar(hidden = "selection", saveaspng = FALSE))
    ) 
  })
  
  # recibir selección de región desde el mapa
  # observeEvent(input$mapa_pais_selected, {
  #   message("Seleccionado ", input$mapa_pais_selected)
  # })
  
  
  ## mapa región ----
  ## selector de mapas ----
  # si la región elegida es el gran santiago, pasa como 99 y elige el mapa específico;
  # de lo contrario, simplemente carga el mapa de la región correspondiente
  region_seleccionada <- reactive({
    req(input$mapa_pais_selected != "",
        length(input$mapa_pais_selected) == 1,
        !is.na(input$mapa_pais_selected),
        !is.null(input$mapa_pais_selected))
    
      message("Seleccionado ", input$mapa_pais_selected)
      input$mapa_pais_selected
  })
  
  # elegir región filtrando la lista
  mapa_region <- reactive({
    message("Mapa region ", region_seleccionada())
    mapas_regiones[[as.numeric(region_seleccionada())]]
  })
  
  # unir mapa con datos
  mapa_region_datos <- reactive({
    req(mapa_region())
    # browser()
    mapa_region() |>
      mutate(codigo_comuna = as.numeric(codigo_comuna)) |> 
      left_join(casen_comuna |> 
                  mutate(cut_comuna = as.numeric(cut_comuna)),
                by = c("codigo_comuna" = "cut_comuna"))
  })
  
  
  output$mapa_region <- renderGirafe({
    req(mapa_region_datos())
    
    p <- mapa_region_datos() |> 
      ggplot(aes(fill = pobreza_p, 
        geometry = geometry,
        data_id = codigo_comuna) #variable de la que depende el hover y selección
      ) + 
      geom_sf_interactive(color = colores$fondo) +
      coord_sf(expand = FALSE) +
      scale_fill_gradient(low = colores$bajo, high = "#581695", limits = c(0, NA)) +
      theme_void() +
      guides(fill = guide_none()) +
      theme(plot.margin = unit(rep(0, 4), "mm"))
    
    # crear gráfico interactivo
    girafe(ggobj = p,
           bg = colores$fondo,
           width_svg = 4,
           height_svg = 4,
           options = list(
             # estilos de selección y hover
             opts_selection(css = "", type = "single", selected = 13),
             opts_hover_inv(css = "opacity:0.5;"),
             opts_hover(css = ""),
             # otros estilos
             opts_sizing(rescale = TRUE),
             opts_toolbar(hidden = "selection", saveaspng = FALSE))
    ) 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
