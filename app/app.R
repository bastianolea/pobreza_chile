library(shiny)
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(ggiraph)
library(bslib)
library(thematic)
library(shinycssloaders)
library(htmltools)

# cargar datos ----
casen_pais <- read_rds("datos/casen_pobreza_pais.rds")
casen_comuna <- read_rds("datos/casen_pobreza_comuna.rds")
casen_region <- read_rds("datos/casen_pobreza_region.rds")

mapa_pais <- read_rds("datos/mapa_pais.rds")
mapas_regiones <- read_rds("datos/mapas_regiones.rds")

# colores ----
colores <- list("fondo" = "#181818",
                "texto" = "white",
                "bajo" = "#999999",
                "principal" = "#581695")

# opciones ----
options(spinner.type = 8, spinner.color = colores$principal)

# ui ----
ui <- fluidPage(
  ## tema ----
  theme = bs_theme(bg = colores$fondo, 
                   fg = colores$texto),
  
  ## header ----
  fluidRow(
    column(12, style = css(margin_top = "12px"),
           h1("Pobreza en Chile"),
           em("Bastián Olea Herrera"),
           
           p("Dashboard sobre datos de pobreza"),
           hr()
    )
  ),

  
  fluidRow(
    column(4, #style = "border: 1px red solid;",
           div(#style = "max-height: 600px;",
           girafeOutput("mapa_pais", height = 700) |> withSpinner(proxy.height = 400)
           )
    ),
    column(8, #style = "border: 1px blue solid;",
           div(#style = "max-height: 200px;",
           girafeOutput("mapa_region", height = 400) |> withSpinner()
           )
    )
  )
)


#server ----
server <- function(input, output) {
  
  # mapas ----
  
  # unir mapa con datos
  mapa_pais_datos <- reactive({
    message("datos mapa país...")
    
    mapa_pais |> 
    left_join(casen_region,
              by = c("codigo_region" = "region"))
  })
  
  ## mapa país ----
  output$mapa_pais <- renderGirafe({
    req(mapa_pais_datos())
    message("gráfico mapa país...")
    
    # gráfico
    p <- mapa_pais_datos() |> 
      ggplot(aes(fill = pobreza_p, 
                 geometry = geometry,
                 data_id = codigo_region) #variable de la que depende el hover y selección
      ) + 
      geom_sf_interactive(color = colores$fondo) +
      scale_fill_gradient(low = colores$bajo, high = colores$principal, limits = c(0, NA)) +
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
  
  
  ## mapa región ----
  ## selector de mapas ----
  # si la región elegida es el gran santiago, pasa como 99 y elige el mapa específico;
  # de lo contrario, simplemente carga el mapa de la región correspondiente
  region_seleccionada <- reactive({
    req(input$mapa_pais_selected != "",
        length(input$mapa_pais_selected) == 1,
        !is.na(input$mapa_pais_selected),
        !is.null(input$mapa_pais_selected))
    
      message("región seleccionada: ", input$mapa_pais_selected)
      input$mapa_pais_selected
  })
  
  # elegir región filtrando la lista
  mapa_region <- reactive({
    req(region_seleccionada())
    
    message("mapa region...")
    mapas_regiones[[as.numeric(region_seleccionada())]]
  })
  
  # unir mapa con datos
  mapa_region_datos <- reactive({
    req(mapa_region())
    message("datos mapa region...")
    
    mapa_region() |>
      mutate(codigo_comuna = as.numeric(codigo_comuna)) |> 
      left_join(casen_comuna |> 
                  mutate(cut_comuna = as.numeric(cut_comuna)),
                by = c("codigo_comuna" = "cut_comuna"))
  })
  
  
  output$mapa_region <- renderGirafe({
    req(mapa_pais_datos())
    req(region_seleccionada())
    req(mapa_region_datos())
    message("gráfico mapa region...")
    
    p <- mapa_region_datos() |> 
      ggplot(aes(fill = pobreza_p, 
        geometry = geometry,
        data_id = codigo_comuna) #variable de la que depende el hover y selección
      ) + 
      geom_sf_interactive(color = colores$fondo) +
      coord_sf(expand = FALSE) +
      scale_fill_gradient(low = colores$bajo, high = colores$principal, limits = c(0, NA)) +
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

shinyApp(ui = ui, server = server)
