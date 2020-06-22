#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Cargar bibliotecas

library(tidyverse)
library(ggthemes)
library(shiny)
library(DT)
library(GGally)

data_municipios <- read_csv("./planea_nacional_municipios.csv")

data_municipios <- data_municipios %>% 
    filter(planea_semaforo <= 4) %>%        #solo escuelas con resultados que permiten evaluar
    select(-localidad,-planea_semaforo) %>% 
    tidyr::drop_na() %>% 
    mutate_at(vars(contains("escuela")),~(./100)) #convierte a % todas las columnas que necesito

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    
    #Creo el input dinamico
    output$entidadOutput <- renderUI({
        selectInput("entidadInput", "Entidad Federativa",
                    sort(unique(data_municipios$entidad)),
                    selected = "Aguascalientes")
    }) #output$entidadOutput
    
    municipios_entidad <- reactive({
        
        # Esta parte es para que no muestre error mientras carga
        # if (is.null(input$entidadInput)) {
        #     return(NULL)
        # } 
        
        data_municipios %>% 
            filter(entidad == input$entidadInput) %>% 
            select(municipio) %>% 
            distinct(municipio) %>% 
            arrange(municipio)
    }) #municipios_entidad
    
    
    
    #Creo el input dinamico
    output$municipioOutput <- renderUI({
       
        if (is.null(input$entidadInput)) {
             return(NULL)
         }
        
        
        selectInput("municipioInput", "Municipio",
                    municipios_entidad(),
                    )
    }) #output$entidadOutput
    
    # Creo una variable reactiva con los registros del municpio que necesito
    filtered <- reactive({
        
        # Esta parte es para que no muestre error mientras carga
        if (is.null(input$entidadInput)) {
            return(NULL)
        }  
        
        if (is.null(input$municipioInput)) {
            return(NULL)
        } 
        
        data_municipios %>% 
            filter(entidad == input$entidadInput,
                   municipio==input$municipioInput,
                   nivel == input$nivel) %>% 
            dplyr::arrange(planea_rank_entidad) %>% 
            dplyr::rename( CCT = cct,
                          Nombre = nombre) %>% 
            tibble::rowid_to_column("Ranking_Municipal") %>% 
            mutate(grupo= ifelse(Ranking_Municipal<=10,"Escuela Top 10","Fuera Top 10")) %>% 
            filter(Ranking_Municipal <= 300) %>% 
            dplyr::rename ('Ranking Municipal' = Ranking_Municipal)
    }) #filtered
    


    output$linePlotMate <- renderPlot({
        # Esto es para que no muestre error mientras carga
        if (is.null(filtered())) {
            return()
        }
        
        columnas = c( 8,9,10,11)

        filtered() %>%
            dplyr::rename('Insuficiente en Matemáticas' = matematicas_insuficiente_escuela,
                          'Indispensable en Matemáticas' = matematicas_indispensable_escuela,
                          'Satisfactorio en Matemáticas' = matematicas_satisfactorio_escuela,
                          'Sobresaliente en Matemáticas' = matematicas_sobresalientes_escuela) %>% 
            ggparcoord(
                columns = columnas,
                groupColumn = "grupo",
                scale="globalminmax",
                order= columnas,
                alphaLines = 0.8,
                showPoints = TRUE,
                splineFactor = 10  #ondula la línea
            )  +
            xlab("")+
            ylab("Porcentaje")+
            scale_y_continuous(labels = scales::percent)+
            labs(caption="Grafica hecha por @nerudista con datos de la evaluacion PLANEA 2017 Y 2018",
                 title="Porcentaje de Alumnos por Nivel de Conocimiento en Matemáticas",
                 subtitle= paste("Resultados PLANEA ", ifelse(input$nivel == "Primaria","2017","2018"),
                                 "para la Localidad: ",input$municipioInput )
            )+
            scale_color_manual(values=c("#244F94","#E0E0E0"))+
            theme_minimal()+
            theme(legend.title = element_blank())



    }) #output$linePlotMate

    output$linePlotEspa <- renderPlot({
        # Esto es para que no muestre error mientras carga
        if (is.null(filtered())) {
            return()
        }
        
        columnas = c( 12,13,14,15)
        
        filtered() %>%
            dplyr::rename('Insuficiente en Español' = espaniol_insuficiente_escuela,
                          'Indispensable en Español' = espaniol_indispensable_escuela,
                          'Satisfactorio en Español' = espaniol_satisfactorio_escuela,
                          'Sobresaliente en Español' = espaniol_sobresalientes_escuela) %>% 
            ggparcoord(
                columns = columnas,
                groupColumn = "grupo",
                scale="globalminmax",
                order= columnas,
                alphaLines = 0.8,
                showPoints = TRUE,
                splineFactor = 10  #ondula la línea
            )  +
            xlab("")+
            ylab("Porcentaje")+
            scale_y_continuous(labels = scales::percent)+
            labs(caption="Grafica hecha por @nerudista con datos de la evaluacion PLANEA 2017 Y 2018",
                 title="Porcentaje de Alumnos por Nivel de Conocimiento en Español",
                 subtitle= paste("Resultados PLANEA ", ifelse(input$nivel == "Primaria","2017","2018"),
                                 "para la Localidad: ",input$municipioInput )
            )+
            scale_color_manual(values=c("#244F94","#E0E0E0"))+
            theme_minimal()+
            theme(legend.title = element_blank())
        
        
        
    }) #output$linePlotMate
    
    
    
        
    output$dynamicTable <- renderDataTable({
        datatable(filtered() %>% 
                  select(-planea_year,-municipio,-nivel,-entidad) %>% 
                  select('Ranking Municipal',planea_rank_entidad, everything()) %>% 
                  dplyr::rename(
                      'Ranking Entidad'=planea_rank_entidad,
                      Grupo=grupo,
                      'Insuficiente en Matemáticas' = matematicas_insuficiente_escuela,
                      'Indispensable en Matemáticas' = matematicas_indispensable_escuela,
                      'Satisfactorio en Matemáticas' = matematicas_satisfactorio_escuela,
                      'Sobresaliente en Matemáticas' = matematicas_sobresalientes_escuela,
                      'Insuficiente en Español' = espaniol_insuficiente_escuela,
                      'Indispensable en Español' = espaniol_indispensable_escuela,
                      'Satisfactorio en Español' = espaniol_satisfactorio_escuela,
                      'Sobresaliente en Español' = espaniol_sobresalientes_escuela
                      ) 
                  ,
                  rownames= FALSE # quita la numeración default en los rows
                  
                  ) %>%  #datatable
            formatPercentage(c("Insuficiente en Matemáticas",
                               'Indispensable en Matemáticas',
                               'Satisfactorio en Matemáticas',
                               'Sobresaliente en Matemáticas',
                               'Insuficiente en Español',
                               'Indispensable en Español',
                               'Satisfactorio en Español' ,
                               'Sobresaliente en Español'
                               ))
    }) # output$dynamicMate
    
   

})

