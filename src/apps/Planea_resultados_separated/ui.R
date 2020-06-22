#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Cargar bibliotecas


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Rendimiento por Escuela en Evaluaciones PLANEA",
               windowTitle = "PLANEA by nerudista"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("Selecciona entre las opciones siguientes "),
            uiOutput("entidadOutput"),
            uiOutput("municipioOutput"),
            selectInput("nivel", ("Selecciona un nivel"), 
                        choices = list("Primaria" = "Primaria",
                                       "Secundaria" = "Secundaria"))
        ),
        # Show a plot 
        mainPanel(
            tabsetPanel( 
                tabPanel( title = "Resultados en Matemáticas" ,
                          plotOutput("linePlotMate")
                          ),
                tabPanel( title = "Resultados en Español",
                          plotOutput("linePlotEspa")
                ),
                tabPanel( title = "Tabla de Datos",
                          DT::dataTableOutput("dynamicTable")  #especifico DT:: para evitar error en shinyapps.io
                          )
            )
        ) # mainPanel
    ) #sidebarLayout
)
)
