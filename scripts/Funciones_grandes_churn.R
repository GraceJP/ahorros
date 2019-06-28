########################################################
# Funciones grandes para el modelo de Churn de Ahorros #
########################################################


#' Tratamiento de la tabla de Ahorros Fin Mes
#'
#' @param Ahorros La tabla.
#' @param Mes El mes de corte. Debe estar en formato "YYYY_MM".
#' @param filtro_oficinas Las regiones consideradas.
#' @param filtro_cafeteras Se quita el codigo de SUBPRODUCTO de las cuentas cafeteras/agremiadas.
#' @param filtro_tipoid Valores de tipo de identificacion que deben ser excluidas por ser personas juridicas.
#' @param Fechas_arreglar Las variables en formato fecha que deben ser cambiadas a formato as.Date.
#'
#' @return La tabla con el tratamiento hecho.
#'
#' @examples
#' Tratamiento_tabla_ahorros(Ahorros = Ahorros, Mes = Mes, filtro_oficinas = filtro_oficinas, filtro_cafeteras = filtro_cafeteras, filtro_tipoid = filtro_tipoid, Fechas_arreglar = Fechas_arreglar)
#' 
Tratamiento_tabla_ahorros <- function(Ahorros, Mes, filtro_oficinas, filtro_cafeteras, filtro_tipoid, Fechas_arreglar) {
  
  Ahorros <- Ahorros %>% dplyr::select(AAHO_ESTADO, AAHO_SAL_MINIMO, AAHO_FEC_APERTURA, AAHO_PRO_SEM, AAHO_EXEN_GMF, AAHO_NUM_CUENTA, AAHO_FEC_ULT_DEP, AAHO_TIP_IDEN, AAHO_SAL_DISPONIB, AAHO_REGION, AAHO_CANAL_APERTURA, AAHO_NUM_IDEN, AAHO_TIP_NOMIN, AAHO_FEC_ULT_MOV, AAHO_PRO_MES, AAHO_SAL_HOY, AAHO_COD_SUB_PRO)
  
  #Arreglos basicos
  Ahorros$AAHO_NUM_CUENTA <- as.numeric(Ahorros$AAHO_NUM_CUENTA)
  Ahorros$AAHO_NUM_IDEN <- as.numeric(Ahorros$AAHO_NUM_IDEN)
  
  ###Ahorros$AAHO_TIP_NOMIN <- factor(Ahorros$AAHO_TIP_NOMIN)
  Ahorros <- Filtros(Ahorros = Ahorros, filtro_oficinas = filtro_oficinas, filtro_cafeteras = filtro_cafeteras, filtro_tipoid = filtro_tipoid)
  Ahorros <- Arreglo_fecha_multiple(Data = Ahorros, Variables = Fechas_arreglar)
  #Incorporacion de variables (variable cuando la funcion agrega 1, variables cuando agrega mas de 1)
  #NOTA: en la funcion Variable_ESTADO TAMBIEN SE EJECUTAN FILTROS
  Ahorros <- Variable_ESTADO(Ahorros = Ahorros)
  Ahorros <- Variables_antiguedad(Ahorros = Ahorros, Mes = Mes)
  Ahorros <- Variables_dias_ult_mov(Ahorros = Ahorros, Mes = Mes)
  Ahorros <- Variable_dias_ult_dep(Ahorros = Ahorros)
  Ahorros <- Variable_GMF(Ahorros = Ahorros)
  Ahorros <- Variable_nomina(Ahorros = Ahorros)

  return(Ahorros)
}




#' Creacion de la variable respuesta del modelo (en categoria y en dummy). Ademas, filtra por los estados que requiera el modelo
#'
#' @param Ahorros La tabla principal.
#' @param Mes_futuro Mes en formato "YYYY_MM" del mes futuro.
#' @param Cierres_futuro Parámetro sobre cuales son los cierres de mes en el ano del mes futuro.
#' @param estados_mes_inicial Vector con los estados relevantes para el modelo en el mes inicial - en terminos de dias de inactividad.
#' @param estados_mes_final Vector con los estados relevantes para el modelo en el mes final. - en terminos de dias de inactividad.
#' @param variable_respuesta Vector con los estados relevantes para el modelo en el mes final - en terminos de estados del modelo.
#' @param variable_respuesta_num Vector utilizado para la asignacion de 0s y 1s para la creacion de la dummy segun variable_respuesta.
#'
#' @return La tabla con las variables respuesta en categoria y en dummy, incorporadas.
#'
#' @examples
#' Ahorros <- Variables_respuesta(Ahorros = Ahorros, Mes_futuro = Mes_futuro, Cierres_futuro = Cierres_futuro, estados_mes_inicial = estados_mes_inicial, estados_mes_final = estados_mes_final, variable_respuesta = variable_respuesta, variable_respuesta_num = variable_respuesta_num)
#' 
Variables_respuesta <- function(Ahorros, Mes_futuro, Cierres_futuro, estados_mes_inicial, estados_mes_final, variable_respuesta, variable_respuesta_num) {
  
  #Traer la informacion de n meses adelante para pegarle el estado futuro
  suppressWarnings(Ahorros_futuro <- fread(file = paste0("Z:/modelos_fuertes/Ahorros/aho_fin_mes_", Mes_futuro,".csv"), integer64 = "character", showProgress = F))
  
  #Fecha de cierre de mes
  Cierre_mes_futuro <- as.Date(paste0(substr(Mes_futuro, 1, 4), "/", substr(Mes_futuro, 6, 7), "/", Cierres_futuro[as.numeric(substr(Mes_futuro, 6, 7))]))
  
  #Crear variable de dias desde el ultimo movimiento
  Ahorros_futuro[, Dias_ult_mov := as.numeric(Cierre_mes_futuro - as.Date(AAHO_FEC_ULT_MOV))] 
  
  #######
  # Necesidad de ajustar con ultimo deposito sin intereses, porque ultimo movimiento no incluye depositos
  #######
  
  suppressWarnings(ultimo_deposito_futuro <- fread(file = paste0(getwd(), "/ultimo_deposito_", substr(Mes_futuro, 1, 4), substr(Mes_futuro, 6, 7), ".csv")))
  ultimo_deposito_futuro$AAHO_NUM_CUENTA <- as.numeric(ultimo_deposito_futuro$AAHO_NUM_CUENTA)
  #ultimo_deposito_futuro <- Arreglo_fecha_multiple(Data = ultimo_deposito_futuro, Variables = c("MAX_of_AAHO_FEC_ULT_DEP"))
  ultimo_deposito_futuro[, MAX_of_AAHO_FEC_ULT_DEP := as.Date(ultimo_deposito_futuro$AAHO_FEC_ULT_DEP, format = "%d/%m/%Y")]
  ultimo_deposito_futuro <- ultimo_deposito_futuro %>% dplyr::select(AAHO_NUM_CUENTA, MAX_of_AAHO_FEC_ULT_DEP)
  #ultimo_deposito_futuro <- ultimo_deposito_futuro[, max(MAX_of_AAHO_FEC_ULT_DEP), by = AAHO_NUM_CUENTA]
  names(ultimo_deposito_futuro) <- c("AAHO_NUM_CUENTA", "MAX_of_AAHO_FEC_ULT_DEP")
  
  if(length(unique(ultimo_deposito_futuro$AAHO_NUM_CUENTA)) < nrow(ultimo_deposito_futuro)) {stop(paste0("Hay registros duplicados en la informacion de ultimo deposito del mes ", Mes_futuro))}
  Ahorros_futuro <- left_join(Ahorros_futuro, ultimo_deposito_futuro, by = "AAHO_NUM_CUENTA")
  Ahorros_futuro <- data.table(Ahorros_futuro)
  
  Ahorros_futuro[, Dias_ult_dep_sinintereses := as.numeric(Cierre_mes_futuro - as.Date(MAX_of_AAHO_FEC_ULT_DEP))]
  
  ###
  Ahorros_futuro[!is.na(Dias_ult_dep_sinintereses),# & MAX_of_AAHO_FEC_ULT_DEP != "", 
          Dias_ult_mov := pmin(Dias_ult_mov, Dias_ult_dep_sinintereses)]
  ######Ahorros_futuro[is.na(MAX_of_AAHO_FEC_ULT_DEP) & is.na(AAHO_FEC_ULT_DEP), 
  ######        Dias_ult_mov := Dias_ult_mov]
  ####Ahorros_futuro[is.na(MAX_of_AAHO_FEC_ULT_DEP), ##### & !is.na(AAHO_FEC_ULT_DEP) & AAHO_ESTADO %in% c(4, 5, 7),
  ####        Dias_ult_mov := 200]
  ######Ahorros_futuro[is.na(MAX_of_AAHO_FEC_ULT_DEP) & !is.na(AAHO_FEC_ULT_DEP) & ESTADO != "INACTIVA" & Dias_ult_mov <= 180,
  ######        Dias_ult_mov := Dias_ult_mov]
  
  #####Ahorros_futuro[is.na(MAX_of_AAHO_FEC_ULT_DEP) & !is.na(AAHO_FEC_ULT_DEP) & AAHO_ESTADO %!in% c(4, 5, 7) & Dias_ult_mov > 180,
  #####        Dias_ult_mov := 150]
  
  #####Ahorros_futuro[AAHO_ESTADO %in% c(4, 5, 7), Dias_ult_mov := 200]
  
  #######
  
  #Armar categorias
  Ahorros_futuro[, Dias_ult_mov_cat := ifelse(Dias_ult_mov <= 60, "Menos de 61 dias", 
                                              ifelse(Dias_ult_mov <= 180, "De 61 a 180 dias",
                                                     ifelse(Dias_ult_mov > 180, "Mas de 180 dias", "Missing")))]
  Ahorros_futuro[AAHO_ESTADO == 3 | AAHO_ESTADO == 4 | AAHO_ESTADO == 5]$Dias_ult_mov_cat <- "Cancelada"
  Ahorros_futuro$Dias_ult_mov_cat <- factor(Ahorros_futuro$Dias_ult_mov_cat, 
                                            levels = c("Menos de 61 dias", "De 61 a 180 dias", "Mas de 180 dias", "Cancelada", "Missing"))
  
  #Solo nos interesa el estado futuro y el numero de cuenta para el merge
  Ahorros_futuro <- Ahorros_futuro %>% dplyr::select(AAHO_NUM_CUENTA, Dias_ult_mov_cat)
  names(Ahorros_futuro) <- c("AAHO_NUM_CUENTA", "Dias_ult_mov_cat_futuro")
  
  #Validacion
  if (length(unique(Ahorros_futuro$AAHO_NUM_CUENTA)) != nrow(Ahorros_futuro)) { stop(paste0("Hay registros duplicados en la tabla de ahorros futuros. Reviselo teniendo en cuenta que Mes_futuro vale ", Mes_futuro)) }
  
  #Pegarla a la tabla principal
  Ahorros <- left_join(Ahorros, Ahorros_futuro, by = "AAHO_NUM_CUENTA")
  Ahorros <- data.table(Ahorros)
  Ahorros[is.na(Dias_ult_mov_cat_futuro)]$Dias_ult_mov_cat_futuro <- "Cancelada" #Para las que no hicieron merge
  
  # FILTROS RELACIONADOS AL MODELO
  Ahorros <- Ahorros[Dias_ult_mov_cat %in% estados_mes_inicial]
  #Ahorros <- ifelse(Mes != Mes_score, Ahorros[Dias_ult_mov_cat_futuro %in% estados_mes_final], Ahorros)
  Ahorros <- Ahorros[Dias_ult_mov_cat_futuro %in% estados_mes_final]
  
  # Construccion de la variable respuesta en categorias y numerica
  Diccionario_auxiliar <- data.table(estados_mes_final, variable_respuesta, variable_respuesta_num)
  suppressWarnings(Ahorros <- left_join(Ahorros, Diccionario_auxiliar, by = c("Dias_ult_mov_cat_futuro" = "estados_mes_final")))
  Ahorros <- data.table(Ahorros)
  
  Ahorros$variable_respuesta <- factor(Ahorros$variable_respuesta, levels = variable_respuesta)
  
  return(Ahorros)
}




#' Agregar variables que vienen de otras fuentes de informacion
#'
#' @param Ahorros La tabla principal.
#' @param Mes El mes de corte. Debe estar en formato "YYYY_MM".
#'
#' @return La tabla con las variables incorporadas.
#'
#' @examples
#' Ahorros <- Pegar_otras_tablas(Ahorros = Ahorros, Mes = Mes)
#' 
Pegar_otras_tablas <- function(Ahorros, Mes) {
  
  Ahorros <- Variables_PQRS(Ahorros = Ahorros, Mes = Mes)
  Ahorros <- Variables_PQRS_cambios(Ahorros = Ahorros, Mes = Mes)
  print("Se han creado las variables de PQRS.")
  Ahorros <- Variables_tenencia(Ahorros = Ahorros, Mes = Mes)
  Ahorros <- Variables_tenencia_rezagadas(Ahorros = Ahorros, Mes = Mes)
  print("Se han creado las variables de Tenencia.")
  if (modelo %in% c("modelo_1")){
    Ahorros <- Variables_CRM(Ahorros = Ahorros, Mes = Mes)
    print("Se han creado las variables de CRM.")
  }
  Ahorros <- Variables_ahorros_rezagados(Ahorros = Ahorros, Mes = Mes)
  Ahorros <- Variables_saldos_adicionales(Ahorros = Ahorros)
  Ahorros <- Variables_saldos_cambios(Ahorros = Ahorros)
  if (modelo %in% c("modelo_2", "modelo_4")){Ahorros <- Variables_inactividad_rezagadas(Ahorros = Ahorros)}
  print("Se han creado las variables de ahorros adicionales.")
  
  return(Ahorros)
}



#' Graficas descriptivas de la tabla principal - no del modelo
#'
#' @param Ahorros La tabla principal.
#' @param Subtitulo Parámetro que dice a qué mes corresponde la gráfica. Ya viene creado en la parametrización.
#' @param Tipo_corrida Parametro fundamental donde se establece que tipo de corrida va a ser. Ver script Ahorros para mas detalles.
#' 
#' @return En la ruta, carpeta "Graficas", se encuentran las imagenes con las descriptivas.
#'
#' @examples
#' Descriptivas(Ahorros = Ahorros, Subtitulo = Subtitulo, Tipo_corrida = Tipo_corrida)
#' 
Descriptivas <- function(Ahorros, Subtitulo, Tipo_corrida) {
  
  if (Mes == Meses[length(Meses)] | Tipo_corrida %in% c("entrenar_scorear_diferentesperiodos", "scorear", "evaluar_modelo")){ 
    #Solo para el ultimo mes del vector o para el mes de score en el caso de scorear diferentes periodos, o solo scorear.
    
  #Barras_ESTADO
  ggsave(paste0(getwd(), "/Graficas/Barras_ESTADO.png"), plot = Barras_ESTADO(Ahorros = Ahorros, Subtitulo = Subtitulo), width=10, height=5)
  #Barras_Inactividad
  ggsave(paste0(getwd(), "/Graficas/Barras_Inactividad.png"), plot = Barras_inactividad(Ahorros = Ahorros, Subtitulo = Subtitulo), width=10, height=5)
  #Barras_Antiguedad
  ggsave(paste0(getwd(), "/Graficas/Barras_Antiguedad.png"), plot = Barras_antiguedad(Ahorros = Ahorros, Subtitulo = Subtitulo), width=10, height=5)
  #Tabla_saldo_promedio
  suppressWarnings(ggsave(paste0(getwd(), "/Graficas/Tabla_saldo_promedio.png"), plot = Tabla_saldo_promedio(Ahorros = Ahorros), width=10, height=5))
  #Tabla_saldo_promedio para activos
  suppressWarnings(ggsave(paste0(getwd(), "/Graficas/Tabla_saldo_promedio_activos.png"), plot = Tabla_saldo_promedio_activos(Ahorros = Ahorros), width=10, height=5))

  print(paste0("Se exportaron las imagenes de descriptivas generales, para el mes ", Mes, ".")) 
  
  }  
}



#' Grafica de transicion de estados de inactividad - relacionado al modelo
#'
#' @param Ahorros La tabla principal.
#' @param Subtitulo Parámetro que dice a qué mes corresponde la gráfica. Ya viene creado en la parametrización.
#' @param variable_respuesta Vector con los estados relevantes para el modelo en el mes final - en terminos de estados del modelo.
#' @param estados_mes_final Vector con los estados relevantes para el modelo en el mes final. - en terminos de dias de inactividad.
#' @param Meses_adelante El numero de meses adelante que define el modelo. Se genera con la parametrizacion del modelo.
#' @param Nombre_inicial Nombre que define en terminos de estados de inactividad el estado inicial del modelo. Se genera con la parametrizacion del modelo.
#' @param modelo que modelo se esta corriendo, solo por razones esteticas.
#'
#' @return En la ruta, carpeta "Graficas/modelo_#/Descriptivas", se encuentran la imagen.
#'
#' @examples
#' Transicion_modelo(Ahorros = Ahorros, Subtitulo = Subtitulo, variable_respuesta = variable_respuesta, estados_mes_final = estados_mes_final, Meses_adelante = Meses_adelante, Nombre_inicial = Nombre_inicial)
#' 
Transicion_modelo <- function(Ahorros, Subtitulo, variable_respuesta, estados_mes_final, Meses_adelante, Nombre_inicial, modelo) {
  
  vjust_text <- rep(-1, times = length(unique(Ahorros$Dias_ult_mov_cat_futuro)))
  vjust_text[1] <- 3.5
  if(modelo == "modelo_4"){vjust_text[2] <- 3.5}
  hjust_text <- rep(0.5, times = length(unique(Ahorros$Dias_ult_mov_cat_futuro)))
  hjust_text[length(hjust_text)] <- 0.6
  
  color_text <- rep("black", times = length(unique(Ahorros$Dias_ult_mov_cat_futuro)))
  color_text[1] <- "white"
  if(modelo == "modelo_4"){color_text[2] <- "white"}
  color_text2 <- color_text
  
  Subtitle <- paste0("Total de Cuentas de Ahorros relevantes para el modelo: ", format(nrow(Ahorros), big.mark = ","), "\n ", Subtitulo)
  
  Ahorros2 <- Ahorros
  for (i in 1:length(variable_respuesta)){
    Ahorros2[Dias_ult_mov_cat_futuro == estados_mes_final[i]]$Dias_ult_mov_cat_futuro <- variable_respuesta[i]
  }
  
  positions <- variable_respuesta
  #positions <- c("Activo Productivo", "Activo Improductivo", "Inactivo", "Cancelada")
  
  plot <- Grafica_barras(Data = Ahorros2, 
                         Variable = "Dias_ult_mov_cat_futuro", 
                         Title = paste0("Distribucion de estado en ", Meses_adelante, " meses"),
                         Caption = paste0("\n Nota: Se consideran las cuentas con estado inicial ", Nombre_inicial, "."),
                         hjust_caption = 1,
                         Frecuencia = "Absoluta_Relativa",
                         vjust_text = vjust_text,
                         hjust_text = hjust_text,
                         color_text = color_text,
                         color_text2 = color_text2,
                         size_title = 15,
                         Subtitle = Subtitle) + scale_x_discrete(limits = positions)
  
  ggsave(paste0(getwd(), "/Graficas/", modelo, "/Descriptivas/Grafica_transiciones.png"), plot = plot, width=8, height=5)
  
  print(paste0("Se ha exportado la imagen con las transiciones asociadas al ", modelo, "."))
}




#' Estimacion del modelo con Training, prediccion con Test y extraer medidas de desempeno predictivo
#'
#' @param Ahorros_principal La tabla principal.
#' @param variables_seleccionadas Vector con los nombres de las variables explicativas, entre comillas cada una.
#' Este parametro se define en el script Seleccion_modelo.
#' @param Ahorros_score La tabla de score.
#' @param Tipo_corrida Parametro fundamental donde se establece que tipo de corrida va a ser. Ver script Ahorros para mas detalles.
#'
#' @return Exporta una lista de cuatro elementos: el modelo y las tabla de Training, Validation y Test.
#'
#' @examples
#' Estimacion_resultados_modelo(Ahorros_principal = Ahorros_principal, variables_seleccionadas = variables_seleccionadas, Ahorros_score = Ahorros_score, Tipo_corrida = "entrenar_scorear_mismoperiodo")
#' 
Crear_modelo_test <- function(Ahorros_principal, variables_seleccionadas, Ahorros_score, Tipo_corrida) {

  if (Tipo_corrida == "entrenar_scorear_mismoperiodo") {
    
    Tabla_con_todo <- Training_validation_test(Ahorros = Ahorros_principal, 
                                               variables_seleccionadas = variables_seleccionadas, 
                                               porc_training = 0.7, 
                                               porc_validation_restante = 0.5)
    
  } else if (Tipo_corrida == "entrenar_scorear_diferentesperiodos") {
    
    Tabla_con_todo_aux <- Training_validation_test(Ahorros = Ahorros_principal, 
                                                   variables_seleccionadas = variables_seleccionadas, 
                                                   porc_training = 0.7, 
                                                   porc_validation_restante = 1)
    
    Tabla_con_todo <- list(Training = Tabla_con_todo_aux$Training,
                           Validation = Tabla_con_todo_aux$Validation,
                           Test = Ahorros_score)
    
  } else if (Tipo_corrida == "entrenar") {
    
    Tabla_con_todo <- Training_validation_test(Ahorros = Ahorros_principal, 
                                               variables_seleccionadas = variables_seleccionadas, 
                                               porc_training = 0.7, 
                                               porc_validation_restante = 1)
  } else if (Tipo_corrida == "scorear") {
    
    Tabla_con_todo <- list(Training = NULL,
                           Validation = NULL,
                           Test = Ahorros_score)
      
  } else if (Tipo_corrida == "evaluar_modelo"){
    
    Tabla_con_todo <- list(Training = NULL,
                           Validation = NULL,
                           Test = Ahorros_score)
    
  }
  
  Training   <- Tabla_con_todo$Training
  Validation <- Tabla_con_todo$Validation
  Test       <- Tabla_con_todo$Test
  
  
  if(Tipo_corrida %in% c("scorear", "evaluar_modelo")) {
    model <- NULL
  } else {

   model <- xgboost_bdb(Training = Training,
                        Validation = Validation,
                        variables_seleccionadas = variables_seleccionadas,
                        target_nombre = "variable_respuesta_num",
                        grilla_max_depth = c(10),
                        grilla_eta = c(0.05),
                        grilla_gamma = c(0),
                        grilla_alpha = c(1),
                        grilla_early_stopping_round = c(30),
                        grilla_nrounds = c(1000),
                        criterio = "aucpr")
  
  }
  
  #Si es solo entrenar, que exporte el modelo
  if (Tipo_corrida == "entrenar") {
  xgb.save(model, paste0(getwd(), "/Modelo/xgboost_", modelo, ".model"))
  print("Se ha exportado el modelo a la ruta especificada.")
  }
  
  return(list(model = model, Training = Training, Validation = Validation, Test = Test))

}    


#' Estimar modelo con Training y sacar probabilidades predichas para Test
#'
#' @param variables_seleccionadas Vector con los nombres de las variables explicativas, entre comillas cada una.
#' Este parametro se define en el script Seleccion_modelo.
#' @param Test La tabla con la que se construyen las probabilidades predichas.
#' @param model El modelo construido en el training o cargado de la ruta.
#'
#' @return Vector de tamaño el numero de registros de Test, con la probabilidad de ser 1 para cada uno, dadas sus variables explicativas.
#'
#' @examples
#' pgorro <- Probabilidades_predichas(variables_seleccionadas = c("Antiguedad", "Dias_ult_mov"), Training = Training, Validation = Validation, Test = Test)
#' 
Probabilidades_predichas <- function(Test, model, variables_seleccionadas) {
  
  dtest <- as.dgcmatrix(Test, 
                        variables_seleccionadas = variables_seleccionadas, 
                        target_nombre = "variable_respuesta_num", 
                        test_logical = T)
  
  pgorro <- predict(model, dtest)
  
  return(pgorro)
}



  
#' Exportar las medidas de desempeno predictivo del modelo
#'
#' @param pgorro Vector de probabilidades predichas de Test.
#' @param Test La tabla con la que se construyen las probabilidades predichas.
#' @param modelo el numero de modelo (1, 2, 3 o 4). Se usa solamente para la ruta donde se exporta.
#'
#' @return En la ruta especificada, exporta las graficas y tablas de desempeno predictivo.
#'
#' @examples
#' Medidas_desempeno(pgorro = pgorro, Test = Test, modelo = "modelo_1")
#' 
Medidas_desempeno <- function(pgorro, Test, modelo) {
    
  Extraer_ROC(pgorro = pgorro, Test = Test) 
  conf_matrix <- Confusion_matrix(predict = pgorro, response = Test$variable_respuesta_num) 
  write.csv(conf_matrix, file = paste0(getwd(), "/Graficas/", modelo, "/Confusion_matrix.csv"))
  print("Se ha exportado la matriz de confusion.")
  
  medidas_optimo <- data.table(Accuracy = (conf_matrix[1,1] + conf_matrix[2,2])/(conf_matrix[1,1] + conf_matrix[1,2] + conf_matrix[2,1] + conf_matrix[2,2]), 
                               Recall = Recall(conf_matrix), 
                               Precision = Precision(conf_matrix))
  write.csv(medidas_optimo, file = paste0(getwd(), "/Graficas/", modelo, "/medidas_optimo.csv"))
  print("Se han exportado las medidas del umbral optimo.")
  
  Grafica_uplift(pgorro = pgorro, percentiles = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), Test = Test, modelo = modelo)
  Grafica_recall(pgorro = pgorro, percentiles = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), Test = Test)
  Grafica_precision(pgorro = pgorro, percentiles = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), Test = Test, modelo = modelo)

}


#' Exporta tabla scoreada de las cuentas
#'
#' @param Test Tabla de test, insumo principal del producto que genera el modelo
#' @param prob_retorno probabilidad de que dado que se esta improductivo, vuelva a ser productivo.
#' Solo aplica para el modelo 1, si es otro modelo entonces no importa este parametro
#' @param Mes_score Mes correspondiente a la tabla de test
#' @param modelo que modelo se esta scoreando
#' @param Tipo_corrida Parametro fundamental donde se establece que tipo de corrida va a ser. Ver script Ahorros para mas detalles.
#' @param pgorro vector con probabilidades predichas
#' @return exporta en la ruta el excel 
#'
#' @examples
#' crear_tabla_score(Test = Test, prob_retorno = 0.167, Mes_score = Mes_score, modelo = modelo, Tipo_corrida = Tipo_corrida)
#' 
crear_tabla_scoreada <- function(Test, pgorro, prob_retorno, Mes_score, modelo, Tipo_corrida){

  Tabla <- Test[, .(AAHO_NUM_CUENTA, AAHO_TIP_IDEN, AAHO_NUM_IDEN, AAHO_PRO_MES, AAHO_SAL_MINIMO, AAHO_SAL_HOY, Dias_ult_mov, AAHO_COD_SUB_PRO)]
  Tabla[, prob_churn := pgorro]
  Tabla[, saldo_esperado := prob_churn * AAHO_PRO_MES * (1 - prob_retorno)]
  if (Tipo_corrida %in% c("entrenar_scorear_mismoperiodo", "entrenar_scorear_diferentesperiodos", "evaluar_modelo")){
  Tabla[, target := Test$variable_respuesta_num]
  }
  
  ###

  Tabla_aux <- Tabla[saldo_esperado > 1000]
  
  saldo_esperado_percentil_33 <- ifelse(modelo == "modelo_1", 4296, 
                                 ifelse(modelo == "modelo_2", 6334,
                                 ifelse(modelo == "modelo_3", 2222,
                                 ifelse(modelo == "modelo_4", 12546, "error"))))
  
  saldo_esperado_percentil_66 <- ifelse(modelo == "modelo_1", 25872, 
                                 ifelse(modelo == "modelo_2", 78113,
                                 ifelse(modelo == "modelo_3", 2222,
                                 ifelse(modelo == "modelo_4", 296534, "error"))))
  
  dias_ult_mov_percentil_25 <- ifelse(modelo == "modelo_1", 3, 
                               ifelse(modelo == "modelo_2", 82,
                               ifelse(modelo == "modelo_3", 2222,
                               ifelse(modelo == "modelo_4", 73, "error"))))
  
  dias_ult_mov_percentil_50 <- ifelse(modelo == "modelo_1", 9, 
                               ifelse(modelo == "modelo_2", 109,
                               ifelse(modelo == "modelo_3", 2222,
                               ifelse(modelo == "modelo_4", 94, "error"))))  
  
  dias_ult_mov_percentil_75 <- ifelse(modelo == "modelo_1", 20, 
                               ifelse(modelo == "modelo_2", 140,
                               ifelse(modelo == "modelo_3", 2222,
                               ifelse(modelo == "modelo_4", 129, "error"))))
  
  
  Tabla[, saldo_esperado_categoria := ifelse(saldo_esperado < 1000, "Menor a $1000", 
                                                ifelse(saldo_esperado < saldo_esperado_percentil_33, paste0("Entre $1000 y $", format(round(saldo_esperado_percentil_33) - 1, big.mark = ",")), 
                                                       ifelse(saldo_esperado < saldo_esperado_percentil_66, paste0("Entre $", format(round(saldo_esperado_percentil_33), big.mark = ","), " y $", format(round(saldo_esperado_percentil_66) - 1, big.mark = ",")), 
                                                              ifelse(saldo_esperado >= saldo_esperado_percentil_66, paste0("Mayor o igual a $", format(round(saldo_esperado_percentil_66), big.mark = ",")), "No hay informacion"))))]
  Tabla$saldo_esperado_categoria <- factor(Tabla$saldo_esperado_categoria, levels = c("Menor a $1000",
                                                                                      paste0("Entre $1000 y $", format(round(saldo_esperado_percentil_33) - 1, big.mark = ",")),
                                                                                      paste0("Entre $", format(round(saldo_esperado_percentil_33), big.mark = ","), " y $", format(round(saldo_esperado_percentil_66) - 1, big.mark = ",")),
                                                                                      paste0("Mayor o igual a $", format(round(saldo_esperado_percentil_66), big.mark = ",")),
                                                                                      "No hay informacion"))
  
  Tabla[, dias_ult_mov_categoria := ifelse(Dias_ult_mov < dias_ult_mov_percentil_25, paste0("Menor a ", round(dias_ult_mov_percentil_25), " dias"),
                                           ifelse(Dias_ult_mov < dias_ult_mov_percentil_50, paste0("Entre ", round(dias_ult_mov_percentil_25), " y ", round(dias_ult_mov_percentil_50) - 1, " dias"),
                                                  ifelse(Dias_ult_mov < dias_ult_mov_percentil_75, paste0("Entre ", round(dias_ult_mov_percentil_50), " y ", round(dias_ult_mov_percentil_75) - 1, " dias"), 
                                                         ifelse(Dias_ult_mov >= dias_ult_mov_percentil_75, paste0("Mayor a ", dias_ult_mov_percentil_75, " dias"), "No hay informacion"))))]
  Tabla$dias_ult_mov_categoria <- factor(Tabla$dias_ult_mov_categoria, levels = c(paste0("Menor a ", round(dias_ult_mov_percentil_25), " dias"),
                                                                                  paste0("Entre ", round(dias_ult_mov_percentil_25), " y ", round(dias_ult_mov_percentil_50) - 1, " dias"),
                                                                                  paste0("Entre ", round(dias_ult_mov_percentil_50), " y ", round(dias_ult_mov_percentil_75) - 1, " dias"),
                                                                                  paste0("Mayor a ", dias_ult_mov_percentil_75, " dias"), 
                                                                                  "No hay informacion"))
  
  diccionario_priorizacion <- fread(file = paste0("Diccionario/priorizacion_", modelo, ".csv"))
  Tabla <- merge(Tabla, diccionario_priorizacion, 
                 by.x = c("saldo_esperado_categoria", "dias_ult_mov_categoria"),
                 by.y = c("saldo_esperado_categoria", "dias_ult_mov_categoria"),
                 all.x = T, all.y = F)
  
  #esto es lo nuevo, que se pego de exportar_prueba.R
  
  Tabla <- Tabla[,
                   .(AAHO_NUM_CUENTA, AAHO_TIP_IDEN, AAHO_NUM_IDEN, AAHO_COD_SUB_PRO, AAHO_PRO_MES, prob_churn, saldo_esperado, Dias_ult_mov, categoria_priorizacion)]
  names(Tabla) <- c("num_cuenta_ahorros", "tipo_id_cliente", "num_id_cliente", "cod_subproducto", "saldo_promedio_mensual", "prob_rodamiento", "saldo_esperado", "dias_ultima_tx", "categoria_priorizacion")
  Tabla <- Tabla[order(categoria_priorizacion)]
  
  #hasta aca
  
  tabla_contingencia <- Tabla[, .(total_saldo_esperado = sum(saldo_esperado),
                                  conteo = .N),
                              by = .(saldo_esperado_categoria, dias_ult_mov_categoria)]
  
  
  
  tabla_contingencia$saldo_esperado_categoria <- factor(tabla_contingencia$saldo_esperado_categoria,
                                                    levels = c("Menor a $1000",
                                                               paste0("Entre $1000 y $", format(round(saldo_esperado_percentil_33) - 1, big.mark = ",")),
                                                               paste0("Entre $", format(round(saldo_esperado_percentil_33), big.mark = ","), " y $", format(round(saldo_esperado_percentil_66) - 1, big.mark = ",")),
                                                               paste0("Mayor o igual a $", format(round(saldo_esperado_percentil_66), big.mark = ",")),
                                                               "No hay informacion"))

  tabla_contingencia$dias_ult_mov_categoria <- factor(tabla_contingencia$dias_ult_mov_categoria,
                                                      levels = c(paste0("Menor a ", round(dias_ult_mov_percentil_25), " dias"),
                                                                 paste0("Entre ", round(dias_ult_mov_percentil_25), " y ", round(dias_ult_mov_percentil_50) - 1, " dias"),
                                                                 paste0("Entre ", round(dias_ult_mov_percentil_50), " y ", round(dias_ult_mov_percentil_75) - 1, " dias"),
                                                                 paste0("Mayor a ", dias_ult_mov_percentil_75, " dias"), 
                                                                 "No hay informacion"))

  tabla_total_saldo_esperado <- dcast(tabla_contingencia, saldo_esperado_categoria ~ dias_ult_mov_categoria, value.var = "total_saldo_esperado")
  tabla_conteo <- dcast(tabla_contingencia, saldo_esperado_categoria ~ dias_ult_mov_categoria, value.var = "conteo")

  ###
  write.csv(Tabla, paste0(getwd(), "/Resultados/", modelo, "/score_", Mes_score, ".csv"), row.names=FALSE)
  write.csv(tabla_total_saldo_esperado, paste0(getwd(), "/Resultados/", modelo, "/suma_saldo_esperado_", Mes_score, ".csv"), row.names=FALSE)
  write.csv(tabla_conteo, paste0(getwd(), "/Resultados/", modelo, "/conteo_", Mes_score, ".csv"), row.names=FALSE)
  
  print("Archivos planos exportados en la ruta de resultados.")

}        
          
          
          