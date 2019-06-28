
##########################################################################################################
############################ MODELO DE CHURN - CUENTA DE AHORROS #########################################
############################    Analytics - Banco de Bogota      ######################################### 
##########################################################################################################

memory.limit(size = 25000) #Ayudar a la memoria
rm(list = ls()) #Borrar ambiente
options(scipen=999) #Quitar notacion cientifica
gc() #Limpieza de RAM
cat("\014") #Limpieza de consola
#.rs.restartR()

###########################################################################################################
#                                          PARAMETROS                                                     #
###########################################################################################################

#Este codigo sirve para 5 diferentes funcionalidades:
#1- "entrenar_scorear_mismoperiodo": coge unos periodos de Training (parametro Meses) y scorea con los mismos meses. NO USA MES_SCORE.
#2- "entrenar_scorear_diferentesperiodos": coge unos periodos de Training (parametro Meses) y scorea con Mes_score.
#3- "entrenar": solo exporta a la ruta, en la carpeta "Modelo", el modelo. Usa el vector Meses y NO USA MES_SCORE.
#4- "scorear": carga el modelo de la ruta y scorea segun Mes_score. NO USA EL VECTOR MESES.
#5- "evaluar_modelo": carga el modelo de la ruta, scorea segun Mes_score y analiza desempeno predictivo. NO USA EL VECTOR MESES
Tipo_corrida <- "scorear"

#Meses considerados: asi sea solo un mes, ponerlo en un vector. Ejemplo: Meses <- c("2018_12")
#Si es "entrenar_scorear_mismoperiodo", sirve de training y test. Si es "entrenar_scorear_diferentes periodos", de training.
#No sirve para "scorear"
Meses <- c("2019_05")

#Corresponde a el mes al que se le van a generar los scores.
#Solo aplica si el Tipo de corrida es "entrenar_scorear_diferentesperiodos" o "scorear". Si no, no se va a usar.
Mes_score <- "2019_05"

#Seleccion del modelo. Debe ser modelo_1, modelo_2, modelo_3 o modelo_4. 
#El Script Seleccion_modelo tiene la parametrizacion de cada uno.
modelo <- "modelo_4"

##########################################################################################################

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

if (Tipo_corrida %!in% c("scorear", "evaluar_modelo")){
Ahorros_principal <- crear_tabla_principal(Meses = Meses, 
                                           filtro_oficinas = filtro_oficinas, 
                                           filtro_cafeteras = filtro_cafeteras, 
                                           filtro_tipoid = filtro_tipoid, 
                                           Fechas_arreglar = Fechas_arreglar, 
                                           estados_mes_inicial = estados_mes_inicial, 
                                           estados_mes_final = estados_mes_final, 
                                           variable_respuesta = variable_respuesta, 
                                           variable_respuesta_num = variable_respuesta_num,
                                           Tipo_corrida = Tipo_corrida)} else {Ahorros_principal <- NULL}


if (Tipo_corrida %in% c("entrenar_scorear_diferentesperiodos", "scorear", "evaluar_modelo")){
  Ahorros_score <- crear_tabla_score(Mes_score = Mes_score, 
                                     Meses_adelante =  Meses_adelante, 
                                     filtro_oficinas =  filtro_oficinas, 
                                     filtro_cafeteras =  filtro_cafeteras, 
                                     filtro_tipoid =  filtro_tipoid, 
                                     Fechas_arreglar =  Fechas_arreglar, 
                                     estados_mes_inicial =  estados_mes_inicial,
                                     estados_mes_final =  estados_mes_final, 
                                     variable_respuesta =  variable_respuesta, 
                                     variable_respuesta_num =  variable_respuesta_num,
                                     Tipo_corrida = Tipo_corrida)} else {Ahorros_score <- NULL}


if (Tipo_corrida %in% c("entrenar_scorear_mismoperiodo", "entrenar")){
  Transicion_modelo(Ahorros = Ahorros_principal, 
                    Subtitulo = Subtitulo, 
                    variable_respuesta = variable_respuesta, 
                    estados_mes_final = estados_mes_final, 
                    Meses_adelante = Meses_adelante, 
                    Nombre_inicial = Nombre_inicial,
                    modelo = modelo)

} else if (Tipo_corrida %in% c("entrenar_scorear_diferentesperiodos", "evaluar_modelo")){
  
  Subtitulo_score <- paste0(meses[as.numeric(substr(Mes_score, 6, 7))], " de ", substr(Mes_score, 1, 4))
  Transicion_modelo(Ahorros = Ahorros_score, 
                    Subtitulo = Subtitulo_score, 
                    variable_respuesta = variable_respuesta, 
                    estados_mes_final = estados_mes_final, 
                    Meses_adelante = Meses_adelante, 
                    Nombre_inicial = Nombre_inicial,
                    modelo = modelo)
}

###########################################################################################################
#                               Analisis relacionado al modelo especifico                                 # 
###########################################################################################################

modelo_churn(Ahorros_principal = Ahorros_principal, 
             Ahorros_score = Ahorros_score, 
             variables_seleccionadas = variables_seleccionadas, 
             Tipo_corrida = Tipo_corrida, 
             Mes_score = Mes_score, 
             modelo = modelo)

############################################################################################################

