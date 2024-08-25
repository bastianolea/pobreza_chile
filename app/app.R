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

source("funciones.R")

# cargar datos ----
pobreza_casen <- readr::read_rds("datos/casen_2022_pobreza.rds")
ingresos_casen <- readr::read_rds("datos/casen_2022_ingresos.rds")

# pobreza_historico <- readr::read_rds("app/datos/pobreza_historico.rds")
pobreza_historico <- readr::read_rds("datos/pobreza_historico.rds")

# setwd("app")
mapa_pais <- read_rds("datos/mapa_pais.rds")
mapas_regiones <- read_rds("datos/mapas_regiones.rds")

# colores ----
# colores <- list("fondo" = "#181818",
#                 "texto" = "white",
#                 "bajo" = "#999999",
#                 "principal" = "#581695")

colores <- list("fondo" = "#909090",
                "texto" = "#181818",
                "bajo" = "#999999",
                "alto" = "#F22222",
                "principal" = "#F22222")

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
    ## torta ----
    column(6,
           div(
             plotOutput("torta")
           )
    ),
    
    column(6,
           div(
             # plotOutput("torta")
           )
    )
  ),
    
  
  fluidRow(
    ## mapa país ----
    column(4, #style = "border: 1px red solid;",
           div(#style = "max-height: 600px;",
             girafeOutput("mapa_pais", height = 700) |> withSpinner(proxy.height = 400)
           )
    ),
    ## mapa región ----
    column(8, #style = "border: 1px blue solid;",
           div(#style = "max-height: 200px;",
             girafeOutput("mapa_region", height = 400) |> withSpinner()
           ),
           
           div(
             # selectInput("selector_evolucion", label = "ver", choices = c("Región", "Comuna")),
             
             # el gráfico muestra datos de región al elegir una región, o de comuna si se aprieta una comuna, y vuelve a mostrar región si se cambia de región
             plotOutput("grafico_evolucion") |> withSpinner()
           )
    )
  ),
  
  # barras
)


#server ----
server <- function(input, output) {
  
  casen_pais <- reactive(pobreza_casen |> filter(nivel == "pais"))
  casen_region <- reactive(pobreza_casen |> filter(nivel == "region"))
  casen_comuna <- reactive(pobreza_casen |> filter(nivel == "comuna"))
  
  # gráfico torta ----
  output$torta <- renderPlot({
    
    data <- tribble(~"valor", ~"tipo",
                    casen_pais()$pobreza_p, "pobreza",
                    1 - casen_pais()$pobreza_p, "total")
    
    data_2 <- data |> 
      arrange(desc(valor)) %>%
      mutate(prop = valor / sum(data$valor)) %>%
      mutate(ypos = cumsum(prop)- 0.5*prop) |> 
      mutate(etiqueta = valor |> round(3),
             etiqueta = etiqueta * 100,
             etiqueta = etiqueta |> 
               format(trim = TRUE, decimal.mark = ",", big.mark = "."),
             etiqueta = paste0(etiqueta, "%"))
    
    ggplot(data_2, aes(x="", y = valor, fill = tipo)) +
      geom_bar(stat = "identity", 
               width = 1, color = "white") +
      geom_text(aes(y = ypos, label = etiqueta), 
                nudge_x = .7,
                color = "black", size = 6) +
      coord_polar("y", start=2.5) +
      scale_fill_manual(values = c(colores$alto, colores$bajo)) +
      theme_void() +
      theme(plot.margin = margin(unit(rep(-40, 4), "cm"))) +
      guides(fill = guide_none()) +
      theme(plot.background = element_rect(fill = colores$fondo, color = colores$fondo),
            panel.background = element_rect(fill = colores$fondo, color = colores$fondo))
  }, bg = colores$fondo)
  
  # mapas ----
  
  # unir mapa con datos
  mapa_pais_datos <- reactive({
    message("datos mapa país...")
    
    mapa_pais |> 
      left_join(casen_region(),
                by = c("codigo_region" = "region"))
  })
  
  
  ## mapa país ----
  output$mapa_pais <- renderGirafe({
    req(mapa_pais_datos())
    message("gráfico mapa país...")
    
    # browser()
    # gráfico
    p <- mapa_pais_datos() |> 
      ggplot(aes(fill = pobreza_p, 
                 geometry = geometry,
                 data_id = codigo_region, #variable de la que depende el hover y selección
                 tooltip = paste0(nombre_region, ": ",
                                  porcentaje(pobreza_p)
                 )
      )
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
             # opts_hover(css = ""),
             opts_hover(css = paste0("fill: ", colores$principal, ";")),
             opts_tooltip(
               opacity = 0.8,
               css = paste0("background-color: ", colores$fondo, "; color: ", colores$texto, ";
                                        padding: 4px; max-width: 200px; border-radius: 4px; font-size: 80%;")),
             # otros estilos
             opts_sizing(rescale = TRUE),
             opts_toolbar(hidden = "selection", saveaspng = FALSE))
    ) 
    # girafe(ggobj = grafico(), 
    #        bg = colores$fondo,
    #        width_svg = 7,
    #        height_svg = 6,
    #        options = list(
    #          opts_sizing(rescale = TRUE),
    #          opts_toolbar(hidden = "selection", saveaspng = FALSE),
    #          opts_hover(css = paste0("fill: ", colores$principal, ";")),
    #          opts_tooltip(
    #            opacity = 0.8,
    #            css = paste0("background-color: ", colores$fondo, "; color: ", colores$texto, ";
    #                            padding: 4px; max-width: 200px; border-radius: 4px; font-size: 80%;")) 
    #        ))
  })
  
  
  
  ### selector de mapas ----
  # si la región elegida es el gran santiago, pasa como 99 y elige el mapa específico;
  # de lo contrario, simplemente carga el mapa de la región correspondiente
  region_seleccionada <- reactive({
    req(input$mapa_pais_selected != "",
        length(input$mapa_pais_selected) == 1,
        !is.na(input$mapa_pais_selected),
        !is.null(input$mapa_pais_selected))
    
    message("región seleccionada: ", input$mapa_pais_selected)
    return(input$mapa_pais_selected)
  })
  
  ## mapa región ----
  
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
      left_join(casen_comuna() |> 
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
                 data_id = codigo_comuna, #variable de la que depende el hover y selección
                 tooltip = paste0(nombre_comuna, ": ",
                                  porcentaje(pobreza_p)
                 ))
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
             opts_selection(css = "", type = "single", selected = NULL),
             # opts_hover_inv(css = "opacity:0.5;"),
             # opts_hover(css = ""),
             opts_hover(css = paste0("fill: ", colores$principal, ";")),
             opts_tooltip(
               opacity = 0.8,
               css = paste0("background-color: ", colores$fondo, "; color: ", colores$texto, ";
                                        padding: 4px; max-width: 200px; border-radius: 4px; font-size: 80%;")),
             # otros estilos
             opts_sizing(rescale = TRUE),
             opts_toolbar(hidden = "selection", saveaspng = FALSE))
    ) 
  })
  
  
  # comuna seleccionada ----
  # inicialmente es NULL, si se elige una comuna adquiere su código único, y si se cambia de región vuelve a ser NULL
  seleccion <- reactiveValues(comuna = NULL)
  
  observeEvent(region_seleccionada(), {
    seleccion$comuna <- NULL
  })
  
  observeEvent(input$mapa_region_selected, {
    req(input$mapa_region_selected != "",
        length(input$mapa_region_selected) == 1,
        !is.na(input$mapa_region_selected),
        !is.null(input$mapa_region_selected))
  
    message("comuna seleccionada: ", input$mapa_region_selected)
    
    seleccion$comuna <- input$mapa_region_selected
  })
  
  # gráficos
  ## evolución region/comuna ----
  # el gráfico muestra datos de región al elegir una región, o de comuna si se aprieta una comuna, y vuelve a mostrar región si se cambia de región
  
  output$grafico_evolucion <- renderPlot({
    
    # if (input$selector_evolucion == "Región") {
    if (is.null(seleccion$comuna) || seleccion$comuna == "") {
    pobreza_historico |> 
      filter(nivel == "region") |> 
      filter(cut_region == region_seleccionada()) |> 
      filter(pobreza != "No pobres") |> 
      ggplot(aes(año, p, fill = pobreza)) +
      geom_area()
      
    } else {
      pobreza_historico |> 
        filter(nivel == "comuna") |> 
        filter(cut_comuna == seleccion$comuna) |> 
        filter(pobreza != "No pobres") |> 
        ggplot(aes(año, p, fill = pobreza)) +
        geom_area()
    }
  })
  
}

shinyApp(ui = ui, server = server)
