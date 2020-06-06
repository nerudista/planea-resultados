pacman::p_load(tidyverse,
               ggthemes,
               GGally)

data_municipios <- read_csv("./datos/crudos/planea_nacional_municipios.csv")


data_municipios <- data_municipios %>% 
  dplyr::rename(Insuficiente=matematicas_insuficiente_escuela,
                Indispensable =  matematicas_indispensable_escuela,
                Satisfactorio = matematicas_satisfactorio_escuela,
                Sobresaliente = matematicas_sobresalientes_escuela)

columnas = c( 10,11,12,13)
localidad_seleccionada <- "Azcapotzalco"
nivel_seleccionado <- "Primaria"



data_grafica <- data_municipios %>% 
  filter(municipio==localidad_seleccionada,
         nivel == nivel_seleccionado) %>% 
  dplyr::arrange(planea_rank_entidad) %>% 
  tibble::rowid_to_column("Ranking Municipal") %>% 
  mutate(grupo= ifelse(id<=10,"Escuela Top 10","Fuera Top 10")) %>% 
  filter(id <= 50) 

data_grafica   %>%  
  ggparcoord(
    columns = columnas,
    groupColumn = "grupo",
    scale="globalminmax",
    order= columnas,
    alphaLines = 0.8,
    showPoints = TRUE,
    splineFactor = 10  #ondula la línea
  )+
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
