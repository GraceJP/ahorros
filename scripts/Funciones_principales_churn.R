
#' Creacion de la tabla principal
#'
#' @param Meses Meses con los que se construye la tabla principal.
#' @param filtro_oficinas Las regiones consideradas.
#' @param filtro_cafeteras Se quita el codigo de SUBPRODUCTO de las cuentas cafeteras/agremiadas.
#' @param filtro_tipoid Valores de tipo de identificacion que deben ser excluidas por ser personas juridicas.
#' @param Fechas_arreglar Las variables en formato fecha que deben ser cambiadas a formato as.Date.
#' @param estados_mes_inicial Pertenece a la parametrizacion de Seleccion_modelo. Corresponde a los estados considerados en el mes inicial.
#' @param estados_mes_final Pertenece a la parametrizacion de Seleccion_modelo. Corresponde a los estados considerados en el mes final.
#' @param variable_respuesta Vector con los estados relevantes para el modelo en el mes final - en terminos de estados del modelo.
#' @param variable_respuesta_num Vector con los estados relevantes para el modelo en el mes final - en terminos de estados del modelo. Queda en dummy (0 o 1).
#' @param Tipo_corrida Parametro fundamental donde se establece que tipo de corrida va a ser. Ver script Ahorros para mas detalles.
#' 
#' @return
#' Genera la tabla principal con todos los meses considerados, despues del tratamiento, construccion de variable respuesta, pegarle otras tablas.
#' Ademas, genera graficas de descriptivas para el ultimo mes de Meses.
#'
#' @examples
#' Ahorros <- crear_tabla_principal(Meses = Meses, filtro_oficinas = filtro_oficinas, filtro_cafeteras = filtro_cafeteras, filtro_tipoid = filtro_tipoid, Fechas_arreglar = Fechas_arreglar, Subtitulo = Subtitulo, Mes_futuro = Mes_futuro, Cierres_futuro = Cierres_futuro, estados_mes_inicial = estados_mes_inicial, estados_mes_final = estados_mes_final, variable_respuesta = variable_respuesta, variable_respuesta_num = variable_respuesta_num, Tipo_corrida = Tipo_corrida)
#' 
crear_tabla_principal <- function(Meses, filtro_oficinas, filtro_cafeteras, filtro_tipoid, Fechas_arreglar, estados_mes_inicial, estados_mes_final, variable_respuesta, variable_respuesta_num, Tipo_corrida) {

  #Validacion inicial
  if (Tipo_corrida %!in% c("entrenar_scorear_mismoperiodo", "entrenar_scorear_diferentesperiodos", "entrenar", "scorear", "evaluar_modelo")) {stop("Revise la parametrizacion de Tipo_corrida.")}
  
  # En Tabla se van a guardar las tablas para cada uno de los meses, aplicandoles las funciones de procesamiento establecidas
  Tabla <- list()
  
  for (i in 1:length(Meses)){
    
    Mes <<- Meses[i]
    
    #Parametros dependientes
    source(paste0(getwd(),"/Scripts/Parametros_dependientes.R"))
    
    #Cargue y limpieza de tabla principal
    suppressWarnings(Ahorros <- fread(file = paste0(getwd(), "/aho_fin_mes_", Mes,".csv"), integer64 = "character"))
    
    #Validacion: que no haya registros duplicados
    if(length(unique(Ahorros$AAHO_NUM_CUENTA)) < nrow(Ahorros)) {stop(paste0("Hay cuentas de ahorros duplicadas en la data del mes ", Mes))}
    
    #Tratamiento de la tabla: arreglos iniciales, filtros, crear otras variables
    Ahorros <- Tratamiento_tabla_ahorros(Ahorros = Ahorros, 
                                         Mes = Mes, 
                                         filtro_oficinas = filtro_oficinas, 
                                         filtro_cafeteras = filtro_cafeteras,
                                         filtro_tipoid = filtro_tipoid,
                                         Fechas_arreglar = Fechas_arreglar)
    
    #Extraer graficas de descriptivas. Se genera para el ultimo mes que esta en el vector Meses
    if(Tipo_corrida %!in% c("entrenar_scorear_diferentesperiodos", "scorear", "evaluar_modelo")) {
    Descriptivas(Ahorros = Ahorros, Subtitulo = Subtitulo, Tipo_corrida = Tipo_corrida)
    }
    
    #Creacion de la variable respuesta (en nombres de las dos categorias y en dummy)
    Ahorros <- Variables_respuesta(Ahorros = Ahorros, 
                                   Mes_futuro = Mes_futuro, 
                                   Cierres_futuro = Cierres_futuro, 
                                   estados_mes_inicial = estados_mes_inicial, 
                                   estados_mes_final = estados_mes_final, 
                                   variable_respuesta = variable_respuesta, 
                                   variable_respuesta_num = variable_respuesta_num)
    print("Se ha creado el Target.")
    
    #Adicionar variables a la tabla principal, utilizando diversas fuentes de informacion
    Ahorros <- Pegar_otras_tablas(Ahorros = Ahorros, 
                                  Mes = Mes)
    
    
    Tabla[[i]] <- Ahorros
    rm(Ahorros)
  }
  
  Ahorros <- Tabla[[1]] #Crear tabla de Ahorros con toda la informacion. Primer paso: poner la primera tabla
  if (length(Meses) >= 2){
    for (i in 2:length(Meses)){
      Ahorros <- rbind(Ahorros, Tabla[[i]]) #Pegarle a la tabla los meses siguientes
    }
  }
  rm(Tabla)
  Ahorros <- data.table(Ahorros) #Formato requerido
  
  print("Se ha creado la tabla principal.")
  return(Ahorros)

}




#' Creacion de la tabla de Score. Sirve para "entrenar_scorear_diferentesperiodos" y "scorear".
#'
#' @param Mes_score El mes correspondiente a la tabla. En el caso de "entrenar_scorear_diferentesperiodos", es el mes donde
#' se usa para el Test y evaluar poder predictivo. En el caso de "scorear", solo genera el vector de probabilidades sin ver resultados.
#' @param Meses_adelante El numero de meses adelante que define el modelo. Se genera con la parametrizacion del modelo.
#' @param filtro_oficinas Las regiones consideradas.
#' @param filtro_cafeteras Se quita el codigo de SUBPRODUCTO de las cuentas cafeteras/agremiadas.
#' @param filtro_tipoid Valores de tipo de identificacion que deben ser excluidas por ser personas juridicas.
#' @param Fechas_arreglar Las variables en formato fecha que deben ser cambiadas a formato as.Date.
#' @param estados_mes_inicial Pertenece a la parametrizacion de Seleccion_modelo. Corresponde a los estados considerados en el mes inicial.
#' @param estados_mes_final Pertenece a la parametrizacion de Seleccion_modelo. Corresponde a los estados considerados en el mes final.
#' @param variable_respuesta Vector con los estados relevantes para el modelo en el mes final - en terminos de estados del modelo.
#' @param variable_respuesta_num Vector con los estados relevantes para el modelo en el mes final - en terminos de estados del modelo. Queda en dummy (0 o 1).
#' @param Tipo_corrida Parametro fundamental donde se establece que tipo de corrida va a ser. Ver script Ahorros para mas detalles.
#' @param estabilidad Se mantiene en falso (no es necesario asignarlo), a menos que se trate de validacion de 
#' estabilidad del modelo. Ahi seria T, lo que hace es no exportar las descriptivas.
#' 
#' @return Genera la tabla mencionada, despues del tratamiento, construccion de variable respuesta si requiere, pegarle otras tablas.
#' 
#' @examples
#' Ahorros_score <- crear_tabla_score(Mes_score = Mes_score, Meses_adelante =  Meses_adelante, filtro_oficinas =  filtro_oficinas, filtro_cafeteras =  filtro_cafeteras, filtro_tipoid =  filtro_tipoid, Fechas_arreglar =  Fechas_arreglar, estados_mes_inicial =  estados_mes_inicial, estados_mes_final =  estados_mes_final, variable_respuesta =  variable_respuesta, variable_respuesta_num =  variable_respuesta_num, Tipo_corrida = Tipo_corrida)
#' 
crear_tabla_score <- function(Mes_score, Meses_adelante, filtro_oficinas, filtro_cafeteras, filtro_tipoid, Fechas_arreglar, estados_mes_inicial, estados_mes_final, variable_respuesta, variable_respuesta_num, Tipo_corrida, estabilidad = F){

  #Validacion inicial
  if (Tipo_corrida %!in% c("entrenar_scorear_mismoperiodo", "entrenar_scorear_diferentesperiodos", "entrenar", "scorear", "evaluar_modelo")) {stop("Revise la parametrizacion de Tipo_corrida.")}
  
  Mes <<- Mes_score
  
  meses <<- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  Cierres <<- c(31,        28     ,  31    ,   30   ,  31   ,  30   ,   31   ,   31     ,     30        ,   31     ,   30        , 31)
  if (as.numeric(substr(Mes, 1, 4)) %% 4 == 0) { Cierres[2] <- 29}
  
  Cierre_mes <<- as.Date(paste0(substr(Mes, 1, 4), "/", substr(Mes, 6, 7), "/", Cierres[as.numeric(substr(Mes, 6, 7))]))
  Mes_futuro <<- Mes_adelante_n(Mes, Meses_adelante)
  Cierres_futuro <<- Cierres
  if (as.numeric(substr(Mes_futuro, 1, 4)) %% 4 == 0) { Cierres_futuro[2] <- 29}
  
  #Cargue y limpieza de tabla principal
  suppressWarnings(Ahorros <- fread(file = paste0(getwd(), "/aho_fin_mes_", Mes,".csv"), integer64 = "character"))
  
  #Validacion: que no haya registros duplicados
  if(length(unique(Ahorros$AAHO_NUM_CUENTA)) < nrow(Ahorros)) {stop(paste0("Hay cuentas de ahorros duplicadas en la data del mes ", Mes))}
  
  #Tratamiento de la tabla: arreglos iniciales, filtros, crear otras variables
  Ahorros <- Tratamiento_tabla_ahorros(Ahorros = Ahorros, 
                                       Mes = Mes, 
                                       filtro_oficinas = filtro_oficinas, 
                                       filtro_cafeteras = filtro_cafeteras,
                                       filtro_tipoid = filtro_tipoid,
                                       Fechas_arreglar = Fechas_arreglar)

  #Extraer graficas de descriptivas. Se genera para el ultimo mes que esta en el vector Meses
  if(Tipo_corrida  %in% c("entrenar_scorear_diferentesperiodos", "scorear", "evaluar_modelo") & estabilidad == F) {
    Subtitulo_score <- paste0(meses[as.numeric(substr(Mes_score, 6, 7))], " de ", substr(Mes_score, 1, 4))
    Descriptivas(Ahorros = Ahorros, Subtitulo = Subtitulo_score, Tipo_corrida = Tipo_corrida)
  }
  
  
  #Creacion de la variable respuesta (en nombres de las dos categorias y en dummy)
  #Si solo vamos a scorear, no se ve la respuesta
  if(Tipo_corrida %in% c("entrenar_scorear_diferentesperiodos", "evaluar_modelo")) {
    Ahorros <- Variables_respuesta(Ahorros = Ahorros, 
                                   Mes_futuro = Mes_futuro, 
                                   Cierres_futuro = Cierres_futuro, 
                                   estados_mes_inicial = estados_mes_inicial, 
                                   estados_mes_final = estados_mes_final, 
                                   variable_respuesta = variable_respuesta, 
                                   variable_respuesta_num = variable_respuesta_num)
    print("Se ha creado el Target.")
  } else {
    Ahorros <- Ahorros[Dias_ult_mov_cat %in% estados_mes_inicial]
  }
  
  #Adicionar variables a la tabla principal, utilizando diversas fuentes de informacion
  Ahorros <- Pegar_otras_tablas(Ahorros = Ahorros, 
                                Mes = Mes)
  
  print("Se ha creado la tabla score.")
  return(Ahorros)

}





#' Generacion del modelo con sus resultados
#'
#' @param Ahorros_principal La tabla principal.
#' @param variables_seleccionadas Vector con los nombres de las variables explicativas, entre comillas cada una.
#' Este parametro se define en el script Seleccion_modelo.
#' @param Ahorros_score La tabla de score.
#' @param Tipo_corrida Parametro fundamental donde se establece que tipo de corrida va a ser. Ver script Ahorros para mas detalles.
#' @param Mes_score El mes correspondiente a la tabla. En el caso de "entrenar_scorear_diferentesperiodos", es el mes donde
#' se usa para el Test y evaluar poder predictivo. En el caso de "scorear", solo genera el vector de probabilidades sin ver resultados.#' 
#' @param modelo el numero de modelo (1, 2, 3 o 4). Se usa solamente para la ruta donde se exporta.
#'
#' @examples
#' modelo_churn(Ahorros_principal = Ahorros_principal, variables_seleccionadas = variables_seleccionadas, Ahorros_score = Ahorros_score, Tipo_corrida = Tipo_corrida, Mes_score = Mes_score, modelo = modelo)
#' 
modelo_churn <- function(Ahorros_principal, variables_seleccionadas, Ahorros_score, Tipo_corrida, Mes_score, modelo){
  
  modelo_test <- Crear_modelo_test(Ahorros_principal = Ahorros_principal, variables_seleccionadas = variables_seleccionadas, Ahorros_score = Ahorros_score, Tipo_corrida = Tipo_corrida)
  model <- modelo_test$model
  Test <- modelo_test$Test
  
  #Si solo scorea, requiere cargar el modelo - no lo construye
  if (Tipo_corrida %in% c("scorear", "evaluar_modelo")){
  model <-  xgb.load(paste0(getwd(), "/Modelo/", "xgboost_", modelo, ".model"))
  } else {
  extraer_feature_importance(model = model)
  }
  
  if (Tipo_corrida %!in% c("entrenar")){
  pgorro <- Probabilidades_predichas(Test = Test, model = model, variables_seleccionadas = variables_seleccionadas)
  }
  
  ####Modelo 4: regla de negocio
  ###if (modelo %in% "modelo_4") {
  ###pgorro <- rep(1, times = nrow(Test))
  ###}
  
  #Si es solo scorear, exporta el vector de pgorro con las identificaciones y demas variables. 
  #Si no, exportar las graficas relacionadas con el desempeno.

  prob_retorno <-  ifelse(modelo == "modelo_1", 0.167, 
                        ifelse(modelo == "modelo_2", 0.009,
                               ifelse(modelo == "modelo_3", 0.009,
                                      ifelse(modelo == "modelo_4", 0.129, "error"))))

  if (Tipo_corrida %in% c("scorear")){ 
    crear_tabla_scoreada(Test = Test, pgorro = pgorro, prob_retorno = prob_retorno, Mes_score = Mes_score, modelo = modelo, Tipo_corrida = Tipo_corrida)
  } else if (Tipo_corrida %in% c("entrenar_scorear_mismoperiodo", "entrenar_scorear_diferentesperiodos", "evaluar_modelo")){
    Medidas_desempeno(pgorro = pgorro, Test = Test, modelo = modelo)
    crear_tabla_scoreada(Test = Test, pgorro = pgorro, prob_retorno = prob_retorno, Mes_score = Mes_score, modelo = modelo, Tipo_corrida = Tipo_corrida)
  } 
    
}



