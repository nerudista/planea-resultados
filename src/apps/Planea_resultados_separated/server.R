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
    dplyr::rename(Insuficiente=matematicas_insuficiente_escuela,
                  Indispensable =  matematicas_indispensable_escuela,
                  Satisfactorio = matematicas_satisfactorio_escuela,
                  Sobresaliente = matematicas_sobresalientes_escuela) 

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    
    #Creo el input dinamico
    output$entidadOutput <- renderUI({
        selectInput("entidadInput", "Entidad Federativa",
                    sort(unique(data_municipios$entidad)),
                    selected = "Aguascalientes")
    }) #fin de output$entidadOutput
    
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
    })
    
    
    
    #Creo el input dinamico
    output$municipioOutput <- renderUI({
       
        if (is.null(input$entidadInput)) {
             return(NULL)
         }
        
        
        selectInput("municipioInput", "Municipio",
                    municipios_entidad(),
                    )
    }) #fin de output$entidadOutput
    
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
            filter(entidad == "Aguascalientes",# input$entidadInput,
                   municipio==input$municipioInput,
                   nivel == input$nivel) %>% 
            dplyr::arrange(planea_rank_entidad) %>% 
            tibble::rowid_to_column("Ranking_Municipal") %>% 
            mutate(grupo= ifelse(Ranking_Municipal<=10,"Escuela Top 10","Fuera Top 10")) %>% 
            filter(Ranking_Municipal <= 300)  
        
        
    }) # fin de filtered
    


    output$linePlot <- renderPlot({
        # Esto es para que no meustre error mientras carga
        if (is.null(filtered())) {
            return()
        }
        
        columnas = c( 8,9,10,11)

        filtered() %>%
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
            labs(caption="Grafica hecha por @nerudista con datos de la evaluacion PLANEA 2017 Y 2018",
                 title="Porcentaje de Alumnos por Nivel de Conocimiento en Matematicas",
                 subtitle= paste("Resultados PLANEA ", ifelse(input$nivel == "Primaria","2017","2018"),
                                 "para la Localidad: ",input$municipioInput )
            )+
            scale_color_manual(values=c("#244F94","#E0E0E0"))+
            theme_minimal()+
            theme(legend.title = element_blank())



    }) #fin de renderPlot

    
    output$dynamic <- renderDataTable({
        datatable(filtered())
    })

})
