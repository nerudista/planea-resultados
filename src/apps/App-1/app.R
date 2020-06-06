#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# Cargar bibliotecas
pacman::p_load(tidyverse,
               ggthemes,
               shiny,
               GGally)



setwd("~/GitHub/planea-resultados")
#cargo todos los archivos
data_municipios <- read_csv("./datos/crudos/planea_nacional_municipios.csv")


data_municipios <- data_municipios %>% 
  filter(planea_semaforo <= 4) %>%        #solo escuelas con resultados que permiten evaluar
  select(-localidad,-planea_semaforo) %>% 
  tidyr::drop_na() %>% 
  dplyr::rename(Insuficiente=matematicas_insuficiente_escuela,
                Indispensable =  matematicas_indispensable_escuela,
                Satisfactorio = matematicas_satisfactorio_escuela,
                Sobresaliente = matematicas_sobresalientes_escuela)


entidades <- data_municipios %>% 
   distinct(entidad) 
         

 
 
#choices = setNames(entidades$entidad,entidades$entidad)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Rendimiento por Escuela en Evaluaciones PLANEA"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        helpText("Grafica "),
        selectInput("entidad", h6("Selecciona una Entidad Federativa"), 
                    choices = unique(data_municipios$entidad)),
        
         selectInput("municipio", h6("Selecciona un municipio"), 
                     choices = NULL),
         selectInput("nivel", h6("Selecciona un nivel"), 
                      choices = list("Primaria" = "Primaria",
                                    "Secundaria" = "Secundaria"))
      ),
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("linePlot"),
        
        #tableOutput("dynamic")
        tableOutput("dynamic")
      )
     )
   )



# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  ###
  
  entidad_reactive <- reactive({
    filter(data_municipios, entidad == input$entidad)
  })
  
  observeEvent(entidad_reactive(), {
    choices <- unique(entidad_reactive()$municipio) %>% 
      sort()
    
    updateSelectInput(session, "municipio", choices = choices) 
  })
  
  ###
  
  # observeEvent(input$nivel,{
  #   nivel_seleccionado <- input$nivel
  #     
  # })
  # 
  # observeEvent( input$municipio,{
  #   localidad_seleccionada <- input$municipio
  # })
  ###
  
  columnas = c( 8,9,10,11)
  
  
  
  output$linePlot <- renderPlot({
    
    localidad_seleccionada <- input$municipio
    nivel_seleccionado <- input$nivel
    
    data_grafica <- data_municipios %>% 
      filter(municipio==localidad_seleccionada,
             nivel == nivel_seleccionado) %>% 
      dplyr::arrange(planea_rank_entidad) %>% 
      tibble::rowid_to_column("Ranking_Municipal") %>% 
      mutate(grupo= ifelse(Ranking_Municipal<=10,"Escuela Top 10","Fuera Top 10")) %>% 
      filter(Ranking_Municipal <= 200) 
    
    
    
    data_grafica   %>%
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
      labs(caption="@nerudista",
           title="Porcentaje de Alumnos por Nivel de Conocimiento en Matematicas",
           subtitle= paste("Resultados PLANEA ", ifelse(nivel_seleccionado == "Primaria","2017","2018"),
                           "para la Localidad: ",localidad_seleccionada )
      )+
      scale_color_manual(values=c("#244F94","#E0E0E0"))+
      theme_minimal()+
      theme(legend.title = element_blank())

  })
  
  
  
  # selected <- reactive({
  #   localidad_seleccionada <- input$municipio
  #   nivel_seleccionado <- input$nivel
  #   
  #   data_tabla <- data_municipios %>% 
  #     filter(municipio==localidad_seleccionada,
  #            nivel == nivel_seleccionado) %>% 
  #     dplyr::arrange(planea_rank_entidad) %>% 
  #     tibble::rowid_to_column("Ranking_Municipal") %>% 
  #     filter(Ranking_Municipal <= 20) %>% 
  #     select(Ranking_Municipal,nombre,municipio,Insuficiente,
  #            Indispensable,Satisfactorio,Sobresaliente)
  #                      })
  
  #output$dynamic <- renderTable(head(selected(),10))
  output$dynamic <- renderTable({
      localidad_seleccionada <- input$municipio
      nivel_seleccionado <- input$nivel
      
      data_tabla <- data_municipios %>% 
            filter(municipio==localidad_seleccionada,
                   nivel == nivel_seleccionado ) %>%  #nivel_seleccionado) %>%
            dplyr::arrange(planea_rank_entidad) %>%
            tibble::rowid_to_column("Ranking_Municipal") %>%
            filter(Ranking_Municipal <= 20) %>%
            select(Ranking_Municipal,nombre,municipio,Insuficiente,
                   Indispensable,Satisfactorio,Sobresaliente) %>% 
            rename("Ranking Municipal"=Ranking_Municipal,
                   Nombre = nombre,
                   Municipio = municipio)
                             
      
    
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

