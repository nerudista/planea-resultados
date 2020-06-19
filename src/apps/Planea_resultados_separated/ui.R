#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Rendimiento por Escuela en Evaluaciones PLANEA"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("Selecciona entre las opciones siguientes "),
            #selectInput("entidad", h6("Selecciona una Entidad Federativa"), 
            #            choices = unique(data_municipios$entidad)),
            uiOutput("entidadOutput"),
            #selectInput("municipio", h6("Selecciona un municipio"), 
            #            choices = NULL),
            uiOutput("municipioOutput"),
            selectInput("nivel", h6("Selecciona un nivel"), 
                        choices = list("Primaria" = "Primaria",
                                       "Secundaria" = "Secundaria"))
        ),
        # Show a plot 
        mainPanel(
            plotOutput("linePlot"),
            br(), br(),
            dataTableOutput("dynamic")
        )
    )
)
)
