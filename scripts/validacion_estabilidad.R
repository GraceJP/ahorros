##########################################################################################################
############################ MODELO DE CHURN - CUENTA DE AHORROS #########################################
############################    Analytics - Banco de Bogota      ######################################### 
##########################################################################################################

############################ VALIDACION DE ESTABILIDAD DEL MODELO ########################################

memory.limit(size = 25000) #Ayudar a la memoria
rm(list = ls()) #Borrar ambiente
options(scipen=999) #Quitar notacion cientifica
gc() #Limpieza de RAM
cat("\014") #Limpieza de consola
#.rs.restartR()

###########################################################################################################
#                                          PARAMETROS                                                     #
###########################################################################################################

### CUALES SON LOS DOS MESES QUE SE VAN A COMPARAR
Mes_score_1 <- "2019_01"
Mes_score_2 <- "2019_04"

#Seleccion del modelo. Debe ser modelo_1, modelo_2, modelo_3 o modelo_4. 
#El Script Seleccion_modelo tiene la parametrizacion de cada uno.
modelo <- "modelo_4"

#Filtros relacionados al negocio
filtro_oficinas <- c("Banca PMP Antioquia", "Banca PMP Bogota Norte", "Banca PMP Bogota Sur", 
                     "Banca PMP Central", "Banca PMP Costa", "Banca PMP Occidente", "Banca PMP Oriente", 
                     "BANCA PREMIUM")
filtro_cafeteras <- 097
filtro_tipoid <- c("N", "I")

#Lista de variables de fecha que vienen en formato que debe ser cambiado:
Fechas_arreglar <- c("AAHO_FEC_ULT_DEP") #"AAHO_FEC_POBLA" y "AAHO_FEC_CIERRE" se excluyen del analisis

##########################################################################################################

#Llamar las funciones principales

source("Z:/personales/Santiago/Funciones.R")
cargar_librerias() #Cargue de paquetes necesarios
source("Z:/personales/Santiago/Funciones_graficas.R")
source(paste0(getwd(),"/Scripts/Funciones_basicas_churn.R"), encoding = "utf-8")
source(paste0(getwd(),"/Scripts/Funciones_grandes_churn.R"))
source(paste0(getwd(),"/Scripts/Funciones_principales_churn.R"))
source("Z:/personales/Santiago/xgboost.R")
source(paste0(getwd(),"/Scripts/Seleccion_modelo.R"))

##########################################################################################################

#Creacion de la primera tabla
Ahorros_score_1 <- crear_tabla_score(Mes_score = Mes_score_1, 
                                     Meses_adelante =  Meses_adelante, 
                                     filtro_oficinas =  filtro_oficinas, 
                                     filtro_cafeteras =  filtro_cafeteras, 
                                     filtro_tipoid =  filtro_tipoid, 
                                     Fechas_arreglar =  Fechas_arreglar, 
                                     estados_mes_inicial =  estados_mes_inicial,
                                     estados_mes_final =  estados_mes_final, 
                                     variable_respuesta =  variable_respuesta, 
                                     variable_respuesta_num =  variable_respuesta_num,
                                     Tipo_corrida = "scorear",
                                     estabilidad = T)
#Creacion de la segunda tabla
Ahorros_score_2 <- crear_tabla_score(Mes_score = Mes_score_2, 
                                     Meses_adelante =  Meses_adelante, 
                                     filtro_oficinas =  filtro_oficinas, 
                                     filtro_cafeteras =  filtro_cafeteras, 
                                     filtro_tipoid =  filtro_tipoid, 
                                     Fechas_arreglar =  Fechas_arreglar, 
                                     estados_mes_inicial =  estados_mes_inicial,
                                     estados_mes_final =  estados_mes_final, 
                                     variable_respuesta =  variable_respuesta, 
                                     variable_respuesta_num =  variable_respuesta_num,
                                     Tipo_corrida = "scorear",
                                     estabilidad = T)

#Cargar el modelo
model <- xgb.load(paste0(getwd(), "/Modelo/", "xgboost_", modelo, ".model"))

#Probabilidades predichas para cada uno de los meses
pgorro_1 <- Probabilidades_predichas(Test = Ahorros_score_1, model = model, variables_seleccionadas = variables_seleccionadas)
pgorro_2 <- Probabilidades_predichas(Test = Ahorros_score_2, model = model, variables_seleccionadas = variables_seleccionadas)

#rm(Ahorros_score_1, Ahorros_score_2)

################################################################################################################

### Graficas y pruebas ###

#Estetico
meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
Mes_score_1_nombre <- paste0(meses[as.numeric(substr(Mes_score_1, 6, 7))], " de ", substr(Mes_score_1, 1, 4))
Mes_score_2_nombre <- paste0(meses[as.numeric(substr(Mes_score_2, 6, 7))], " de ", substr(Mes_score_2, 1, 4))

#Crear tabla con la informacion de los dos meses
mes1 <- data.table(prob = pgorro_1, mes = Mes_score_1_nombre)
mes2 <- data.table(prob = pgorro_2, mes = Mes_score_2_nombre)
tabla <- rbind(mes1, mes2)

#Primera grafica: distribucion de densidad de las probabilidades - entre meses
plot1 <- ggplot(tabla, aes(x = prob, group = mes, fill = mes)) + 
  geom_density(alpha=0.5) + 
  xlab("Probabilidad estimada por el modelo") +
  ylab("Densidad") + 
  labs(title = "Distribución de probabilidades predichas por el modelo - para dos meses diferentes",
       fill = "Mes") + 
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

#Segunda grafica: distribucion de densidad ACUMULADA de las probabilidades - entre meses
plot2 <- ggplot(tabla, aes(x = prob, colour = mes)) + 
  stat_ecdf(size = 1.2) + 
  xlab("Probabilidad estimada por el modelo") +
  ylab("Densidad acumulada") + 
  labs(title = "Distribución acumulada de probabilidades predichas por el modelo - para dos meses diferentes",
       colour = "Mes") + 
  theme(plot.title = element_text(size = 15), #, hjust = 0.5),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

#Exportar las graficas
ggsave(paste0(getwd(), "/Graficas/", modelo, "/Estabilidad/densidad.png"), plot = plot1, width=10, height=5)
ggsave(paste0(getwd(), "/Graficas/", modelo, "/Estabilidad/acumulada.png"), plot = plot2, width=10, height=5)
print("Graficas de densidad exportadas.")

### INDICE DE ESTABILIDAD

if (length(pgorro_1) > length(pgorro_2)) {
  id <- sort(sample(length(pgorro_1), length(pgorro_2)))
  pgorro_1 <- pgorro_1[id]
  rm(id)
} else if (length(pgorro_1) < length(pgorro_2)) {
  id <- sort(sample(length(pgorro_2), length(pgorro_1)))
  pgorro_2 <- pgorro_2[id]
  rm(id)
}

dataset <- data.frame(pgorro_1, pgorro_2)
indice <- cal_psi(data = dataset, bench = "pgorro_1", target = "pgorro_2")

#' Si PSI < 0.1 entonces el modelo es bueno y presenta estabilidad en relación al estudio
#' con la póblación de entrenamiento o base
#' Si el PSI > 0.1 y < 0.25 entonces estabilidad media 
#' Si el PSI > 0.25 entonces no hay estabilidad
resultado <- data.table(Indice = indice, Conclusion = ifelse(indice < 0.1, "El modelo presenta estabilidad", 
                                                             ifelse(indice < 0.25, "El modelo presenta estabilidad media",
                                                                    ifelse(indice > 0.25, "El modelo no presenta estabilidad", "No se pudo obtener el indice."))))

#Exportar la tabla con el indice y su conclusion
ggsave(paste0(getwd(), "/Graficas/", modelo, "/Estabilidad/indice_estabilidad_poblacion.png"), plot = grid.table(resultado, rows = NULL), width=10, height=5)
print("Tabla de indice de estabilidad exportado.")

################################################################################################################
