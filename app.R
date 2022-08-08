#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(sf)
library(stringr)
library(htmltools)
library(shinyWidgets)
library(shinycustomloader)
library(DT)


agri <- readRDS("estimaciones agricolas1.RDS") 
ganado <- readRDS("ganado.RDS")
equinos <- readRDS("equinos.RDS")
ind <- readRDS("agrupamientos_ind.RDS")
ind <- transform(ind, longitud = as.numeric(longitud))
ind <- transform(ind, latitud = as.numeric(latitud))
### filtro ganadero
gana_bovino <- ganado %>% filter(tipo_ganado == "Bovinos")
gana_ovino <- ganado %>% filter(tipo_ganado == "Ovino")
gana_porcino <- ganado %>% filter(tipo_ganado == "Porcino")
### filtro agri
avena <- agri %>% filter(cultivo == "Avena")
cebada <- agri %>% filter(cultivo == "Cebada total")
centeno <- agri %>% filter(cultivo == "Centeno")
girasol <- agri %>% filter(cultivo == "Girasol")
maiz <- agri %>% filter(cultivo == "Maíz")
soja <- agri %>% filter(cultivo == "Soja total")
trigo <- agri %>% filter(cultivo == "Trigo total")
### filtro tabla equi
table_equi <- as.data.frame(equinos) %>% select(distrito, anio, padrillos, caballos, yeguas, potrillos_potrillas, mulas, burros_asnos)

ui <- dashboardPage(
    dashboardHeader(title = "Panel de control - Buenos Aires", titleWidth = 350), 
    dashboardSidebar(width = 350,
                     sidebarMenu(                                  
                         id = "sidebar", 
                         menuItem(text = "Producción", tabName = "prod", icon = icon("tractor")), 
                         conditionalPanel("input.sidebar == 'prod' && input.t3 == 'val_agri'", selectInput(inputId = "filter_agri", "Seleccione un cultivo (2020/21)", choices = unique(c("Avena", "Cebada total", "Centeno", "Girasol", "Maíz", "Soja total", "Trigo total")))),
                         conditionalPanel("input.sidebar == 'prod' && input.t3 == 'val_gana'", sliderInput("gana_fecha", "Seleccione un año", min = min(gana_bovino$anio), max = max(gana_bovino$anio), value = min(gana_bovino$anio))),
                         conditionalPanel("input.sidebar == 'prod' && input.t3 == 'val_gana'", prettyRadioButtons(inputId = "filter_gana", label = "Tipo de ganado:", c("Bovino" = "Bovino", "Ovino" = "Ovino", "Porcino" = "Porcino"), animation = "pulse")),
                         conditionalPanel("input.sidebar == 'prod' && input.t3 == 'val_equi'", selectInput(inputId = "equi_fecha", "Seleccione un año", choices = unique(equinos$anio)))
                     ),
                     div(style="display:inline-block;width:32%;text-align: center;", actionButton("primera", label = NULL, style = "width: 300px; height: 110px;
background: url('https://lh3.googleusercontent.com/9sNfiMppl_ZzNOhjdwDZK2xvZRwINIHhRSU8UsHPfwW0hxYifkK2Z8ClL7_sygq_ojMm6fquxFuXmVmAikcLX8n7L5slWgoe1NN3E6X4vg0v8xxV2JcLjGlWTi7pZn5haxJ76Y-N2cFGM44Qig');  background-size: cover; background-position: center;"))),                      
    dashboardBody(
        
            
            tabItem(
                
                #pestaña de produccion                                                                                   
                
                tabName = "prod", 
                tabBox(id = "t3", width = 15, 
                       tabPanel(title = "Estimaciones agricola por municipio", value = "val_agri", withLoader(leafletOutput("mapagri"), type="html", loader="pacman"), box(title =  strong("Ranking de cultivos"), width = "100%", status = "info", solidHeader = T, collapsible = T, plotOutput("Gra_agri"))), 
                       tabPanel(title = "Stock ganadero por municipio", value = "val_gana", withLoader(leafletOutput("mapgana"), type="html", loader="pacman"),  box(title =  strong("Stock ganadero"), width = "100%", status = "info", solidHeader = T, collapsible = T, plotOutput("Plot_gana"))), 
                       tabPanel(title = "Existencias equinas por municipio", value = "val_equi", withLoader(leafletOutput("mapequi"), type="html", loader="pacman"), DT::dataTableOutput("table_equimap", width = "100%")),
                       tabPanel(title = "Agrupamientos industriales", value = "val_ind", withLoader(leafletOutput("mapind"), type="html", loader="pacman")))),

            
        
    )
)





server <- function(input, output) {
    ### Agricultura
    
    
    output$mapagri <- renderLeaflet({
        
       popup_avena = paste(
         "<B>Distrito</B>: ",  avena$distrito, "<BR>",
         "<B>Fecha</B>: ", avena$fecha, "<BR>",
         "<B>Producción</B>: ", avena$produccion, "<BR>", 
         "<B>Superficie sembrada</B>: ", avena$superficie_sembrada, "<BR>", 
         "<B>Superficie cocechada</B>: ", avena$superficie_cosechada, "<BR>",
         "<B>Rendimiento</B>: ", avena$rendimiento)
       
       popup_cebada <- paste("<B>Distrito</B>: ",  cebada$distrito, "<BR>",
                             "<B>Fecha</B>: ", cebada$fecha, "<BR>",
                             "<B>Producción</B>: ", cebada$produccion, "<BR>", 
                             "<B>Superficie sembrada</B>: ", cebada$superficie_sembrada, "<BR>", 
                             "<B>Superficie cocechada</B>: ", cebada$superficie_cosechada, "<BR>",
                             "<B>Rendimiento</B>: ", cebada$rendimiento) 
       
       popup_centeno <- paste("<B>Distrito</B>: ",  centeno$distrito, "<BR>",
                              "<B>Fecha</B>: ", centeno$fecha, "<BR>",
                              "<B>Producción</B>: ", centeno$produccion, "<BR>", 
                              "<B>Superficie sembrada</B>: ", centeno$superficie_sembrada, "<BR>", 
                              "<B>Superficie cocechada</B>: ", centeno$superficie_cosechada, "<BR>",
                              "<B>Rendimiento</B>: ", centeno$rendimiento) %>% lapply(HTML)
       
       popup_girasol <- paste("<B>Distrito</B>: ",  girasol$distrito, "<BR>",
                              "<B>Fecha</B>: ", girasol$fecha, "<BR>",
                              "<B>Producción</B>: ", girasol$produccion, "<BR>", 
                              "<B>Superficie sembrada</B>: ", girasol$superficie_sembrada, "<BR>", 
                              "<B>Superficie cocechada</B>: ", girasol$superficie_cosechada, "<BR>",
                              "<B>Rendimiento</B>: ", girasol$rendimiento) %>% lapply(HTML)
       
       popup_maiz <- paste("<B>Distrito</B>: ",  maiz$distrito, "<BR>",
                           "<B>Fecha</B>: ", maiz$fecha, "<BR>",
                           "<B>Producción</B>: ", maiz$produccion, "<BR>", 
                           "<B>Superficie sembrada</B>: ", maiz$superficie_sembrada, "<BR>", 
                           "<B>Superficie cocechada</B>: ", maiz$superficie_cosechada, "<BR>",
                           "<B>Rendimiento</B>: ", maiz$rendimiento) %>% lapply(HTML)
       
       popup_soja <- paste("<B>Distrito</B>: ",  soja$distrito, "<BR>",
                           "<B>Fecha</B>: ", soja$fecha, "<BR>",
                           "<B>Producción</B>: ", soja$produccion, "<BR>", 
                           "<B>Superficie sembrada</B>: ", soja$superficie_sembrada, "<BR>", 
                           "<B>Superficie cocechada</B>: ", soja$superficie_cosechada, "<BR>",
                           "<B>Rendimiento</B>: ", soja$rendimiento) %>% lapply(HTML)
       
       popup_trigo <- paste("<B>Distrito</B>: ",  trigo$distrito, "<BR>",
                            "<B>Fecha</B>: ", trigo$fecha, "<BR>",
                            "<B>Producción</B>: ", trigo$produccion, "<BR>", 
                            "<B>Superficie sembrada</B>: ", trigo$superficie_sembrada, "<BR>", 
                            "<B>Superficie cocechada</B>: ", trigo$superficie_cosechada, "<BR>",
                            "<B>Rendimiento</B>: ", trigo$rendimiento) %>% lapply(HTML)
        
      pal_avena <- colorBin(palette = "YlGnBu", domain = avena$produccion, n = 9)
        
        pal_cebada <- colorBin(palette = "YlGnBu", domain = cebada$produccion, n = 9)
        
        pal_centeno <- colorBin(palette = "YlGnBu", domain = centeno$produccion, n = 9)
        
        pal_girasol <- colorBin(palette = "YlGnBu", domain = girasol$produccion, n = 9)
        
        pal_maiz <- colorBin(palette = "YlGnBu", domain = maiz$produccion, n = 9)
        
        pal_soja <- colorBin(palette = "YlGnBu", domain = soja$produccion, n = 9)
        
        pal_trigo <- colorBin(palette = "YlGnBu", domain = trigo$produccion, n = 9)
        
        
        switch(input$filter_agri,
               "Avena" =
                 leaflet() %>% 
                 addTiles() %>%
                 addPolygons(data = avena, weight = 1, color = "black", label = paste0("Municipio: ", as.character(avena$distrito), 
                                                                                           " | Producción: ", as.character(format(avena$produccion, big.mark = "."))),
                             popup = popup_avena,
                             fillColor = ~pal_avena(avena$produccion), 
                             fillOpacity = 0.8, 
                             highlight = highlightOptions(weight = 3, 
                                                          color = "black", 
                                                          bringToFront = T), 
                             layerId = ~distrito) %>%
                 addLegend(position = "bottomright", 
                           pal = pal_avena,
                           values = avena$produccion,
                           title = "Producción de Avena"), 
                "Cebada total" = 
                 leaflet() %>% 
                 addTiles() %>%
                 addPolygons(data = cebada, weight = 1, color = "black", label = paste0("Municipio: ", as.character(cebada$distrito), 
                                                                                           " | Producción: ", as.character(format(cebada$produccion, big.mark = "."))),
                             popup = popup_cebada,
                             fillColor = ~pal_cebada(cebada$produccion), 
                             fillOpacity = 0.8, 
                             highlight = highlightOptions(weight = 3, 
                                                          color = "black", 
                                                          bringToFront = T), 
                             layerId = ~distrito) %>%
                 addLegend(position = "bottomright", 
                           pal = pal_cebada,
                           values = cebada$produccion,
                           title = "Producción de Cebada"), 
               "Centeno" = 
                 leaflet() %>% 
                 addTiles() %>%
                 addPolygons(data = centeno, weight = 1, color = "black", label = paste0("Municipio: ", as.character(centeno$distrito), 
                                                                                          " | Producción: ", as.character(format(centeno$produccion, big.mark = "."))),
                             popup = popup_centeno,
                             fillColor = ~pal_centeno(centeno$produccion), 
                             fillOpacity = 0.8, 
                             highlight = highlightOptions(weight = 3, 
                                                          color = "black", 
                                                          bringToFront = T), 
                             layerId = ~distrito) %>%
                 addLegend(position = "bottomright", 
                           pal = pal_centeno,
                           values = centeno$produccion,
                           title = "Producción de Centeno"),
               "Girasol" = 
                 leaflet() %>% 
                 addTiles() %>%
                 addPolygons(data = girasol, weight = 1, color = "black", label = paste0("Municipio: ", as.character(girasol$distrito), 
                                                                                              " | producción: ", as.character(format(girasol$produccion, big.mark = "."))),
                             popup = popup_girasol,
                             fillColor = ~pal_girasol(girasol$produccion), 
                             fillOpacity = 0.8, 
                             highlight = highlightOptions(weight = 3, 
                                                          color = "black", 
                                                          bringToFront = T), 
                             layerId = ~distrito) %>%
                 addLegend(position = "bottomright", 
                           pal = pal_girasol,
                           values = girasol$produccion,
                           title = "Producción de Girasol"), 
               "Maíz" = 
                 leaflet() %>% 
                 addTiles() %>%
                 addPolygons(data = maiz, weight = 1, color = "black", label = paste0("Municipio: ", as.character(maiz$distrito), 
                                                                                           " | producción: ", as.character(format(maiz$produccion, big.mark = "."))),
                             popup = popup_maiz,
                             fillColor = ~pal_maiz(maiz$produccion), 
                             fillOpacity = 0.8, 
                             highlight = highlightOptions(weight = 3, 
                                                          color = "black", 
                                                          bringToFront = T), 
                             layerId = ~distrito) %>%
                 addLegend(position = "bottomright", 
                           pal = pal_maiz,
                           values = maiz$produccion,
                           title = "Producción de Maíz"),
               "Soja total" = 
                 leaflet() %>% 
                 addTiles() %>%
                 addPolygons(data = soja, weight = 1, color = "black", label = paste0("Municipio: ", as.character(soja$distrito), 
                                                                                          " | producción: ", as.character(format(soja$produccion, big.mark = "."))),
                             popup = popup_soja,
                             fillColor = ~pal_soja(soja$produccion), 
                             fillOpacity = 0.8, 
                             highlight = highlightOptions(weight = 3, 
                                                          color = "black", 
                                                          bringToFront = T), 
                             layerId = ~distrito) %>%
                 addLegend(position = "bottomright", 
                           pal = pal_soja,
                           values = soja$produccion,
                           title = "Producción de Soja"), 
               "Trigo total" = 
                 leaflet() %>% 
                 addTiles() %>%
                 addPolygons(data = trigo, weight = 1, color = "black", label = paste0("Municipio: ", as.character(trigo$distrito), 
                                                                                           " | producción: ", as.character(format(trigo$produccion, big.mark = "."))),
                             popup = popup_trigo,
                             fillColor = ~pal_trigo(trigo$produccion), 
                             fillOpacity = 0.8, 
                             highlight = highlightOptions(weight = 3, 
                                                          color = "black", 
                                                          bringToFront = T), 
                             layerId = ~distrito) %>%
                 addLegend(position = "bottomright", 
                           pal = pal_trigo,
                           values = trigo$produccion,
                           title = "Producción de trigo")
               
        )
    })
    
    
    agri_graf <- agri %>% group_by(distrito, cultivo) %>% mutate(produccion = sum(produccion)) %>% 
        distinct(distrito, cultivo, produccion)
    
    ggplot_agri <- reactive({
        site <- input$mapagri_shape_click$id
        agri_graf[agri_graf$distrito %in% site,]})
    
    output$Gra_agri <- renderPlot({
        
        pal_plot_agri <- c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0', '#225ea8')
        
        ggplot(data = ggplot_agri(), mapping = aes(x = reorder(cultivo, produccion), 
                                                   y = produccion)) +
            geom_bar(stat = "identity", fill = pal_plot_agri, colour = "black") + coord_flip() + theme_classic() + 
            xlab(NULL) + ylab("Ranking por produccion") + 
            labs(title = req(ggplot_agri()$distrito)) + 
            theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) + 
            geom_label(
                aes(label = format(produccion, big.mark = ".")), 
                nudge_x = 0.25, 
                nudge_y = 0.25 
                
            ) 
    })
    ### Ganaderia
    
    graf_map <- reactive({
        lv <- ganado %>% filter(tipo_ganado == input$gana_tipo)
        return(lv)})
  
    bovino <- reactive({
      lv <- gana_bovino %>% filter(anio == input$gana_fecha)
      return(lv)
    })
    
    porcino <- reactive({
      lv <- gana_porcino %>% filter(anio == input$gana_fecha)
      return(lv)
    })
    
    ovino <- reactive({
      lv <- gana_ovino %>% filter(anio == input$gana_fecha)
      return(lv)
    })      
    
    output$mapgana <- renderLeaflet({
        
        pal_bovino <- colorBin(palette = "YlGnBu", domain = gana_bovino$stock, n = 9)
        
        pal_porcino <- colorBin(palette = "YlGnBu", domain = gana_porcino$stock, n = 9)
        
        pal_ovino <- colorBin(palette = "YlGnBu", domain = gana_ovino$stock, n = 9)
        
        switch(input$filter_gana,
               "Bovino" = 
                   leaflet() %>% 
                   addTiles() %>%
                   addPolygons(data = bovino(), weight = 1, color = "black", label = 
                                   paste0(as.character(bovino()$distrito),
                                          " | Stock: ", as.character(format(bovino()$stock, big.mark = "."))),
                               fillColor = ~pal_bovino(bovino()$stock), 
                               fillOpacity = 0.8, 
                               highlight = highlightOptions(weight = 3, 
                                                            color = "black", 
                                                            bringToFront = T), 
                               layerId = ~distrito) %>%
                   addLegend(position = "bottomright", 
                             pal = pal_bovino,
                             values = gana_bovino$stock,
                             title = "Stock ganadero"), 
               "Ovino" = 
                   leaflet() %>% 
                   addTiles() %>%
                   addPolygons(data = ovino(), weight = 1, color = "black", label = 
                                   paste0(as.character(ovino()$distrito),
                                          " | Stock: ", as.character(format(ovino()$stock, big.mark = "."))),
                               fillColor = ~pal_ovino(ovino()$stock), 
                               fillOpacity = 0.8, 
                               highlight = highlightOptions(weight = 3, 
                                                            color = "black", 
                                                            bringToFront = T), 
                               layerId = ~distrito) %>%
                   addLegend(position = "bottomright", 
                             pal = pal_ovino,
                             values = gana_ovino$stock,
                             title = "Stock ganadero"),
               "Porcino" = 
                   leaflet() %>% 
                   addTiles() %>%
                   addPolygons(data = porcino(), weight = 1, color = "black", label = 
                                   paste0(as.character(porcino()$distrito),
                                          " | Stock: ", as.character(format(porcino()$stock, big.mark = "."))),
                               fillColor = ~pal_porcino(porcino()$stock), 
                               fillOpacity = 0.8, 
                               highlight = highlightOptions(weight = 3, 
                                                            color = "black", 
                                                            bringToFront = T), 
                               layerId = ~distrito) %>%
                   addLegend(position = "bottomright", 
                             pal = pal_porcino,
                             values = gana_porcino$stock,
                             title = "Stock ganadero")
               
               
        )})
    
    gana_graf <- ganado %>% as_tibble() %>% group_by(distrito, tipo_ganado) %>% mutate(stock = sum(stock)) %>% 
        distinct(distrito, stock, tipo_ganado)
    
    ggplot_gana <- reactive({
        site <- input$mapgana_shape_click$id
        gana_graf[gana_graf$distrito %in% site,]})
    
    output$Plot_gana <- renderPlot({
        
        pal_plot_gana <- c('#edf8b1','#7fcdbb','#2c7fb8')
        
        ggplot(data = ggplot_gana(), mapping = aes(x = reorder(tipo_ganado, stock), 
                                                   y = stock)) +
            geom_bar(stat = "identity", fill = pal_plot_gana, colour = "black") + coord_flip() + theme_classic() + 
            xlab(NULL) + ylab("Ranking por ganado") + 
            labs(title = req(ggplot_gana()$distrito)) + 
            theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) + 
            geom_label(
                aes(label = format(stock, big.mark = ".")), 
                nudge_x = 0.25, 
                nudge_y = 0.25 
                
            ) 
    })
    
    equi <- reactive({
      lv <- equinos %>% filter(anio == input$equi_fecha)
      return(lv)
    })   
    
    output$mapequi <- renderLeaflet({
        
        popup_equi = paste(
          "<B>Distrito</B>: ", equi()$distrito, "<BR>",
          "<B>Cantidad de caballos</B>: ", equi()$caballos, "<BR>",
          "<B>Cantidad de yeguas</B>: ", equi()$yeguas, "<BR>", 
          "<B>Cantidad de potrillos</B>: ", equi()$potrillos_potrillas, "<BR>", 
          "<B>Cantidad de mulas</B>: ", equi()$mulas, "<BR>",
          "<B>Cantidad de burros</B>: ", equi()$burros_asnos)
        
        pal_equi <- colorBin(palette = "YlGnBu", domain = equinos$caballos, n = 9)
        
        leaflet() %>% 
            addTiles() %>%
            addPolygons(data = equi(), weight = 1, color = "black", label = 
                            paste0(as.character(equi()$distrito),
                                   " | Caballos: ", as.character(format(equi()$caballos, big.mark = "."))),
                        popup = popup_equi,
                        fillColor = ~pal_equi(equi()$caballos), 
                        fillOpacity = 0.8, 
                        highlight = highlightOptions(weight = 3, 
                                                     color = "black", 
                                                     bringToFront = T), 
                        layerId = ~distrito) %>%
            addLegend(position = "bottomright", 
                      pal = pal_equi,
                      values = equinos$caballos,
                      title = "Cantidad de caballos")})
    
    observeEvent(input$mapequi_shape_click, {
      
      #capture the info of the clicked polygon
      click <- input$mapequi_shape_click
      
      #subset your table with the id of the clicked polygon 
      selected <- table_equi[table_equi$distrito == click$id,]
      
      #if click id isn't null render the table
      if(!is.null(click$id)){
        output$table_equimap = DT::renderDataTable({
          DT::datatable(selected, options = 
                          list(paging = TRUE,scrollX = T, scrollY = T,
                              dom = 'Bfrtip', buttons = c('csv', 'excel'), 
                               pageLength = 150, 
                               lengthMenu = c(15,50,100)), 
                        rownames = FALSE, extensions = 'Buttons', selection = 'single')
        })
      } 
    }) 
    
    output$mapind <- renderLeaflet({
      
        
        popup_ind = paste(
          "<B>Distrito</B>: ", ind$localidad, "<BR>",
          "<B>Superficie en hectareas</B>: ", ind$superficie_hectareas, "<BR>",
          "<B>Infraestructura</B>: ", ind$infraestructura_basica, "<BR>", 
          "<B>Número de empresas</B>: ", ind$numero_empresas, "<BR>", 
          "<B>Empleados estimados</B>: ", ind$numero_empleados_estimado)
      
        icon_ind <- "https://cdn-icons-png.flaticon.com/512/2942/2942169.png"
        
        leaflet() %>% 
            addTiles() %>%
            addMarkers(lat = ind$latitud, lng = ind$longitud,
                       label = 
                           paste0(as.character(ind$nombre_agrupamiento_promotor),
                                  " | Numero de empresas: ", as.character(ind$numero_empresas)),
                       popup = popup_ind,
                       icon = list(
                           iconUrl = icon_ind, 
                           iconSize = c(20, 20)
                       ))}) 
    
   
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
