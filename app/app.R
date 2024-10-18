library(shiny)
library(readr)
library(dplyr)
library(lubridate)
library(sf)
library(ggplot2)
library(ggiraph)
library(bslib)
library(thematic)
library(shinycssloaders)
library(htmltools)
library(gt)



# cargar datos ----
# setwd("app")
pobreza_casen <- readr::read_rds("datos/casen_2022_pobreza.rds")
ingresos_casen <- readr::read_rds("datos/casen_2022_ingresos.rds")

pobreza_historico <- readr::read_rds("datos/pobreza_historico.rds")

mapa_pais <- read_rds("datos/mapa_pais.rds")
mapas_regiones <- read_rds("datos/mapas_regiones.rds")

campamentos <- read_rds("datos/campamentos_chile_puntos.rds")

linea_pobreza <- read_rds("datos/canasta_basica_linea_pobreza.rds")

cut_comunas <- read_csv2("datos/comunas_chile_cut.csv", col_types = c("cccc"))


# colores ----
# colores <- list("fondo" = "#181818",
#                 "texto" = "white",
#                 "bajo" = "#999999",
#                 "principal" = "#581695")

colores <- list("fondo" = "#909090",
                "texto" = "#181818",
                "detalle" = "#404040",
                "bajo" = "#999999",
                "alto" = "#F22222",
                "principal" = "#F22222")

thematic_shiny(font = "auto", accent = colores$principal)

# funciones ----
source("funciones.R")

# opciones ----
options(spinner.type = 8, spinner.color = colores$principal)

# ui ----
ui <- page_fluid(
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
           
           h3("Porcentaje de pobreza en Chile"),
           div("Porcentaje de la población que vive en situación de pobreza."),
           
           div(
             plotOutput("g_torta_pais") |> withSpinner()
           ),
           p("Texto explicativo"),
           div(class = "fuente",
               "Fuente:")
    ),
    
    
    ### evolución ----
    column(6,
           
           h3("Evolución de la pobreza en Chile"),
           h4("Porcentaje de la población que vive en situación de pobreza, según nivel de pobreza, desde 2009 a la fecha."),
           div(
             plotOutput("g_evolucion_pais") |> withSpinner()
           ),
           p("Texto explicativo"),
           div(class = "fuente",
               "Fuente:")
    )
  ),
  
  ### Línea de pobreza ----
  fluidRow(
    column(12,
           h3("Línea de pobreza"),
           h4("Subtítulo"),
           div(
             plotOutput("g_linea_pobreza") |> withSpinner()      
           ),
           p("Texto explicativo"),
           div(class = "fuente",
               "Fuente:")
    )
  ),
  
  
  fluidRow(
    
    column(12, style = css(margin_top = "20px"),
           h3("Pobreza por regiones"),
           h4("Subtítulo"),
           p("Presione una región del mapa nacional para ver la región con su comunas en detalle.")
    ),
    
    ## mapa país ----
    column(4, #style = "border: 1px red solid;",
           
           
           div(#style = "max-height: 600px;",
             girafeOutput("mapa_pais", height = 700) |> withSpinner(proxy.height = 400)
           )
    ),
    ## mapa región ----
    column(8, #style = "border: 1px blue solid;",
           htmlOutput("titulo_mapa_region"),
           
           div(#style = "max-height: 200px;",
             
             girafeOutput("mapa_region", height = 400) |> withSpinner()
           ),
           
           
           ### evolución
           div(
             # selectInput("selector_evolucion", label = "ver", choices = c("Región", "Comuna")),
             
             htmlOutput("titulo_grafico_evolucion"),
             
             # el gráfico muestra datos de región al elegir una región, o de comuna si se aprieta una comuna, y vuelve a mostrar región si se cambia de región
             plotOutput("grafico_evolucion") |> withSpinner()
           ), 
           
           gt_output("tabla_evolucion") |> withSpinner()
           
    )
  ),
  
  ## campamentos ----
  fluidRow(
    column(12,
           h3("Campamentos"),
           h4("Subtítulo"),
           
           selectInput("region_campamentos",
                       "Región",
                       choices = NULL),
           
           girafeOutput("mapa_region_campamentos", height = 600) |> withSpinner(),
           
           dataTableOutput("tabla_campamentos") |> withSpinner()
    )
  )
)


#server ----
server <- function(input, output) {
  
  casen_pais <- reactive(pobreza_casen |> filter(nivel == "pais"))
  casen_region <- reactive(pobreza_casen |> filter(nivel == "region"))
  casen_comuna <- reactive(pobreza_casen |> filter(nivel == "comuna"))
  
  # gráfico torta ----
  output$g_torta_pais <- renderPlot({
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
  
  pobreza_historico_pais <- reactive({
    pobreza_historico |> 
      filter(nivel == "pais")
  })
  
  #grafico evolución ----
  output$g_evolucion_pais <- renderPlot({
    # browser()
    pobreza_historico_pais() |> 
      mutate(pobreza = forcats::fct_rev(pobreza)) |> 
      ggplot(aes(as.factor(año), p, fill = pobreza)) +
      geom_col()
    # theme(plot.background = element_rect(fill = colores$fondo, color = colores$fondo),
    # panel.background = element_rect(fill = colores$fondo, color = colores$fondo))
  }, bg = colores$fondo)
  
  # gráfico línea de probreza ----
  
  output$g_linea_pobreza <- renderPlot({
    # browser()
    linea_pobreza |> 
      filter(variable %in% c("LP", "LPE")) |>
      mutate(variable = recode(variable,
                               "LP" = "Línea de pobreza",
                               "LPE" = "Línea de pobreza extrema")) |> 
      filter(year(fecha) >= 2019) |> 
      ggplot(aes(fecha, valor, color = variable)) +
      geom_line(linewidth = 1.2) +
      scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","), 
                         n.breaks = 8) +
      scale_color_manual(values = c("Línea de pobreza" = "red",
                                    "Línea de pobreza extrema" = "red4")) +
      guides(color = guide_legend(position = "bottom")) +
      theme_classic() +
      theme(panel.grid.major.x = element_line()) +
      # theme(plot.title = ggtext::element_textbox()) +
      # labs(title = "Líneas de <span style='color:red;'>pobreza</span> y <span style='color:red4'>pobreza extrema</span>") +
      theme(plot.background = element_rect(fill = colores$fondo, color = colores$fondo),
            panel.background = element_rect(fill = colores$fondo, color = colores$fondo))
  }, bg = colores$fondo)
  
  
  
  # —-----
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
  
  
  
  ### selectores  ----
  
  #### region ----
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

  region_seleccionada_nombre <- reactive({
    unique(cut_comunas$region[cut_comunas$cut_region == region_seleccionada()])
  })
  
  #### comuna ----

  # inicialmente es NULL, si se elige una comuna adquiere su código único, y si se cambia de región vuelve a ser NULL
  seleccion <- reactiveValues(activa = NULL, comuna = NULL)
  
  observeEvent(region_seleccionada(), {
    seleccion$comuna <- NULL
    seleccion$activa <- "region"
    
    message("selección activa: ", seleccion$activa)
  })
  
  observeEvent(input$mapa_region_selected, {
    req(input$mapa_region_selected != "",
        length(input$mapa_region_selected) == 1,
        !is.na(input$mapa_region_selected),
        !is.null(input$mapa_region_selected))
    
    message("comuna seleccionada: ", input$mapa_region_selected)
    
    seleccion$comuna <- input$mapa_region_selected
    seleccion$activa <- "comuna"
    
    message("selección activa: ", seleccion$activa)
  })
  
  comuna_seleccionada_nombre <- reactive({
    cut_comunas$comuna[cut_comunas$cut_comuna == seleccion$comuna]
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
  
  ### titulo ----
  output$titulo_mapa_region <- renderUI({
    req(length(seleccion$activa) > 0)
    # browser()
    
    # if (seleccion$activa == "region") {
    div(
      h3("Región de ", region_seleccionada_nombre())
    )
    # } else {
    #   div(
    #     h3("Evolución de la pobreza en la comuna de ", comuna_seleccionada_nombre()),
    #     h4("Región de ", region_seleccionada_nombre())
    #   )
    # }
  })
  
  
  ## datos campamentos ----
  
  datos_campamentos <- reactive({
    req(region_seleccionada() != "")
    
    campamentos |> 
      filter(as.numeric(cut_r) == as.numeric(region_seleccionada())) |> 
      filter(n_hog > 0)
  })
  
  ## mapa region campamentos ----
  output$mapa_region_campamentos <- renderGirafe({
    req(region_seleccionada())
    message("mapa region campamentos...")
    
    # browser()
    p <- datos_campamentos() |> 
      ggplot() + 
      geom_sf(data = mapa_region(),
              aes(geometry = geometry),
              fill = colores$bajo, color = colores$fondo) +
      geom_sf_interactive(aes(geometry = punto,
                              data_id = nombre, #variable de la que depende el hover y selección
                              size = n_hog,
                              tooltip = paste0(nombre, ": ",
                                               n_hog, " hogares")
      ),
      color = colores$alto, alpha = 0.5) +
      coord_sf(expand = FALSE, clip = "off") +
      scale_size(range = c(2, 15)) +
      theme_void() +
      guides(fill = guide_none()) +
      theme(plot.margin = unit(rep(0, 4), "mm")) +
      theme(plot.background = element_rect(fill = colores$fondo, color = colores$fondo),
            panel.background = element_rect(fill = colores$fondo, color = colores$fondo))
    
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
  
  
  
  
  # —----
  
  ## gráfico evolución bajo mapa ----
  # el gráfico muestra datos de región al elegir una región, o de comuna si se aprieta una comuna, y vuelve a mostrar región si se cambia de región
  datos_evolucion <- reactive({
    req(length(seleccion$activa) > 0)
    
    # if (is.null(seleccion$comuna) || seleccion$comuna == "") {
    if (seleccion$activa == "region") {
      dato <- pobreza_historico |> 
        filter(nivel == "region") |> 
        filter(cut_region == region_seleccionada())
      
      
    } else {
      # browser()
      dato <- pobreza_historico |> 
        filter(nivel == "comuna") |>
        # filter(comuna == "Tiltil")
        filter(cut_comuna == seleccion$comuna)
    }
    return(dato)
  })
  
  
  
  
  ## evolución region/comuna ----
  # el gráfico muestra datos de región al elegir una región, o de comuna si se aprieta una comuna, y vuelve a mostrar región si se cambia de región
  
  output$grafico_evolucion <- renderPlot({
    datos_evolucion() |> 
      filter(pobreza != "No pobres") |> 
      ggplot(aes(año, p, fill = pobreza)) +
      geom_area() +
      theme_minimal() +
      theme(plot.background = element_rect(fill = colores$fondo, color = colores$fondo),
            panel.background = element_rect(fill = colores$fondo, color = colores$fondo))
  })
  
  ### titulo  ----
  
  output$titulo_grafico_evolucion <- renderUI({
    req(length(seleccion$activa) > 0)
    # browser()
    
    if (seleccion$activa == "region") {
      div(
        h3("Evolución de la pobreza en la región de ", region_seleccionada_nombre())
      )
    } else {
      div(
        h3("Evolución de la pobreza en la comuna de ", comuna_seleccionada_nombre()),
        h4("Región de ", region_seleccionada_nombre())
      )
    }
  })
  
  # tablas ----
  
  ## tabla evolución ----
  output$tabla_evolucion <- render_gt({
    # if (seleccion$activa == "region") {
      
      dato <- datos_evolucion() |> 
        filter(pobreza != "No pobres") |> 
        select(pobreza, año, p)
      
      dato |> 
        tidyr::pivot_wider(names_from = año, values_from = p) |> 
        gt() |> 
        fmt_percent(columns = where(is.numeric), decimals = 0) |> 
        data_color(columns = where(is.numeric), rows = 1:2, 
                   method = "numeric", 
                   palette = c(colores$bajo, colores$alto),
                   domain = c(0, max(dato$p)),
                   direction = "column") |> 
        tab_options(table.background.color = colores$fondo,
                    table_body.hlines.color = colores$detalle,
                    table_body.border.top.color = colores$detalle,
                    column_labels.border.top.color = colores$detalle, 
                    column_labels.border.bottom.color = colores$detalle, 
                    table_body.border.bottom.color = colores$detalle) |> 
        cols_label(pobreza = "")
      
    })
  
  ## tabla campamentos ----
  output$tabla_campamentos <- renderDataTable({
    req(region_seleccionada())
    req(nrow(datos_campamentos()) > 0)
    message("tabla campamentos...")
    # browser()
    datos_campamentos()
  })
  }
  
  shinyApp(ui = ui, server = server)
  