pacman::p_load(tidyverse,reshape)

values = c("GENERAL","TELESECUNDARIA","COMUNITARIO","INDÍGENA","INICIAL INDÍGENA")

data <- readr::read_csv("./datos/crudos/cct/cct_NACIONAL.csv",
                 #fileEncoding = "UTF-8",
                 col_names=FALSE,
                 cols_only(X1 ="c",  # clave cct
                           X26 ="c",  # municipio
                           X4="c",   # tipo de centro
                           X87="c"   # Tipo de escuela
                           )
                 ) %>% 
  dplyr::filter(X4=="ESCUELA" &
                X87 %in% values) 




#cargo todos los archivos de planea par ahacer el join
files <- list.files(path = "./datos/crudos/planea-webpage/", 
                    pattern = "*.csv", 
                    full.names = T)

data_planea <- plyr::ldply(files, read.table,sep = ";",encoding = "UTF-8",fill=TRUE, header = TRUE)

columnas_usar = c("cct","nombre","localidad","entidad","nivel","planea_semaforo","planea_year",
                  "planea_rank_entidad","matematicas_insuficiente_escuela","matematicas_indispensable_escuela",
                  "matematicas_satisfactorio_escuela","matematicas_sobresalientes_escuela")

data_localidad <- data_planea %>% 
  select(columnas_usar)

data_municipios <- data_localidad %>% 
  dplyr::left_join(data,
                   by= c("cct"="X1")) %>% 
  select(-c(X87,X4))


names(data_municipios)[names(data_municipios)=="X26"] <- "municipio"

colnames(data_municipios)

data_municipios$municipio <- str_to_title(data_municipios$municipio)

write.csv(data_municipios,"./datos/crudos/cct/planea_nacional_municipios.csv", 
          sep = ";",
          fileEncoding = "UTF-8",
          row.names=FALSE #no index
          )
