################################
### Configuracion del modelo ###
################################



#' Seleccion del modelo 
#' 
#' En esta funcion se seleccionan los parametros fundamentales del modelo. De que estados empezar y cuales estados finales considerar,
#' estableciendo cuales estados corresponden a 0 y cuales a 1.
#'
#' @param modelo puede ser "modelo_1", "modelo_2", "modelo_3" o "modelo_4"
#'
#' @return trae los parametros fundamentales:
#' i) Meses_adelante: Cuantos meses adelante se establece el rodamiento de estado
#' ii) estados_mes_inicial: Cual es el estado inicial, en terminos de los dias de inactividad
#' iii) estados_mes_final: Cuales son los posibles estados finales, en terminos de los dias de inactividad 
#' iv) variable_respuesta: Cuales son los estados finales en terminos del nombre del estado
#' v) variable_respuesta_num: Establecer cuales son 1 y cuales son 0
#'
#' @examples modelo <- "modelo_1"
#' 

if (modelo == "modelo_1"){
    
    #cuantos meses adelante para variable respuesta?
    Meses_adelante <- 2
    
    #estados_mes_inicial <- c("Menos de 61 dias", "De 61 a 180 dias", "Mas de 180 dias", "Cancelada", "Missing")
    estados_mes_inicial <- c("Menos de 61 dias")
    Nombre_inicial <-      c("Activo Productivo")
    
    #estados_mes_final <- c("Menos de 61 dias", "De 61 a 180 dias", "Mas de 180 dias", "Cancelada", "Missing")
    estados_mes_final <- c("Menos de 61 dias", "De 61 a 180 dias")
    
    #variable_respuesta <- c("Activo Productivo", "Activo Improductivo", "Inactivo", "Cancelada", "Missing")
    variable_respuesta <- c(      "Activo Productivo"   ,  "Activo Improductivo"   )
    variable_respuesta_num <- c(     0                  ,               1          )
    
    #variables_seleccionadas <- c("Dias_ult_mov", "Dias_ult_dep", "CANT_CTAS_AHORRO_diferencia", "Edad", "CRM_GRUPO_SEGMENTO", "AAHO_TIP_NOMIN", "AAHO_EXEN_GMF", "TOTAL_PRODUCTOS_ACTIVO_menos1", "Antiguedad", "TOTAL_PRODUCTOS_menos1", "AAHO_SAL_HOY_menos_max_pasado3", "TOTAL_PRODUCTOS_diferencia", "Region", "CRM_GENERO", "SO_AH_diff", "AAHO_SAL_HOY", "TOTAL_PRODUCTOS", "CRM_ESTRATO", "SO_AH", "CANT_CTAS_AHORRO_menos1", "CANT_CTAS_AHORRO", "QR_AH", "Novedades_diff", "CANT_TARJ_CREDITO", "TOTAL_PRODUCTOS_ACTIVO_diferencia", "TOTAL_PRODUCTOS_ACTIVO", "QR_AH_diff", "CANT_TARJ_CREDITO_diferencia", "AAHO_SAL_MINIMO_menos_max_pasado3", "AAHO_SAL_MINIMO_sobre_max_pasado3", "dias_ult_mov_sobre_antiguedad", "AAHO_SAL_HOY_estables", "AAHO_SAL_HOY_bajadas")
    variables_seleccionadas <- c("Dias_ult_mov", 
                                 "AAHO_SAL_HOY", 
                                 "dias_ult_mov_sobre_antiguedad", 
                                 "AAHO_SAL_HOY_estables", 
                                 "AAHO_TIP_NOMIN", 
                                 "AAHO_SAL_HOY_menos_max_pasado3", 
                                 "AAHO_SAL_MINIMO_menos_max_pasado3", 
                                 "AAHO_SAL_MINIMO_sobre_max_pasado3", 
                                 "Edad", 
                                 "Antiguedad", 
                                 "AAHO_SAL_HOY_bajadas", 
                                 "AAHO_EXEN_GMF", 
                                 "Dias_ult_dep", 
                                 "CANT_CTAS_AHORRO_diferencia")
        
  } else if (modelo == "modelo_2"){
      
    #cuantos meses adelante para variable respuesta?
    Meses_adelante <- 4
    
    #estados_mes_inicial <- c("Menos de 61 dias", "De 61 a 180 dias", "Mas de 180 dias", "Cancelada", "Missing")
    estados_mes_inicial <- c("De 61 a 180 dias")
    Nombre_inicial <-      c("Activo Improductivo")
    
    #estados_mes_final <- c("Menos de 61 dias", "De 61 a 180 dias", "Mas de 180 dias", "Cancelada", "Missing")
    estados_mes_final <- c("Menos de 61 dias", "De 61 a 180 dias", "Mas de 180 dias", "Cancelada")
    
    #variable_respuesta <- c("Activo Productivo", "Activo Improductivo", "Inactivo", "Cancelada", "Missing")
    variable_respuesta <- c("Activo Productivo", "Activo Improductivo"   ,  "Inactivo", "Cancelada")
    variable_respuesta_num <- c(     0       ,        0          ,              1      ,       1   )
    
    variables_seleccionadas <- c("AAHO_SAL_HOY",
                                 "Dias_ult_mov",
                                 "dias_ult_mov_sobre_antiguedad",
                                 "AAHO_TIP_NOMIN",
                                 "AAHO_SAL_HOY_menos_max_pasado5",
                                 "AAHO_SAL_MINIMO",
                                 "Antiguedad",
                                 "fue_inactiva",
                                 "AAHO_SAL_HOY_sobre_max_pasado5",
                                 "veces_productivo",
                                 "AAHO_SAL_MINIMO_siendoproductivo",
                                 "AAHO_SAL_MINIMO_sobre_max_pasado5",
                                 "AAHO_SAL_MINIMO_menos_max_pasado5",
                                 "AAHO_EXEN_GMF",
                                 "TOTAL_PRODUCTOS_ACTIVO")#,
#                                 "CANT_CTAS_AHORRO_menos2",
 #                                "tenencia_menos_min_tenencia5")
    
    
  } else if (modelo == "modelo_3"){
      
    #cuantos meses adelante para variable respuesta?
    Meses_adelante <- 6
    
    #estados_mes_inicial <- c("Menos de 61 dias", "De 61 a 180 dias", "Mas de 180 dias", "Cancelada", "Missing")
    estados_mes_inicial <- c("Menos de 61 dias", "De 61 a 180 dias")
    ###estados_mes_inicial <- c("Menos de 61 dias")
    Nombre_inicial <-      c("Activo (Productivo o Improductivo)")
    ###Nombre_inicial <-      c("Activo Productivo")
    
    #estados_mes_final <- c("Menos de 61 dias", "De 61 a 180 dias", "Mas de 180 dias", "Cancelada", "Missing")
    estados_mes_final <- c("Menos de 61 dias", "De 61 a 180 dias", "Mas de 180 dias", "Cancelada")
    
    #variable_respuesta <- c("Activo Productivo", "Activo Improductivo", "Inactivo", "Cancelada", "Missing")
    variable_respuesta <- c("Activo Productivo", "Activo Improductivo", "Inactivo", "Cancelada")
    variable_respuesta_num <- c(0               ,        0            ,     1     ,    1       )
    
    variables_seleccionadas <- c("Dias_ult_mov", 
                                 "Dias_ult_dep",
                                 "dias_ult_mov_sobre_antiguedad",
                                 "Antiguedad", 
                                 "AAHO_SAL_DISPONIB",
                                 "CANT_CTAS_AHORRO",
                                 "TOTAL_PRODUCTOS_menos1",
                                 "AAHO_EXEN_GMF", 
                                 "TOTAL_PRODUCTOS_diferencia",
                                 "AAHO_TIP_NOMIN", 
                                 "AAHO_SAL_HOY_menos_max_pasado3", 
                                 "Novedades", 
                                 "AAHO_SAL_MINIMO_sumaponderada", 
                                 "CANT_CTAS_AHORRO_diferencia",
                                 "AAHO_SAL_HOY_estables", 
                                 "AAHO_SAL_HOY_bajadas",
                                 "AAHO_SAL_MINIMO_estables",
                                 "AAHO_SAL_MINIMO_bajadas")
    
  } else if (modelo == "modelo_4"){
    
    #cuantos meses adelante para variable respuesta?
    Meses_adelante <- 2
    
    #estados_mes_inicial <- c("Menos de 61 dias", "De 61 a 180 dias", "Mas de 180 dias", "Cancelada", "Missing")
    estados_mes_inicial <- c("De 61 a 180 dias")
    Nombre_inicial <-      c("Activo Improductivo")
    
    estados_mes_final <- c("Menos de 61 dias", "De 61 a 180 dias", "Mas de 180 dias", "Cancelada")#, "Missing")
  
    #variable_respuesta <- c("Activo Productivo", "Activo Improductivo", "Inactivo", "Cancelada", "Missing")
    variable_respuesta <- c("Activo Productivo", "Activo Improductivo", "Inactivo", "Cancelada")
    variable_respuesta_num <- c(     1         ,     0                ,    0       ,    0      )  
    
    variables_seleccionadas <- c("AAHO_SAL_HOY",
                                 "Dias_ult_mov",
                                 "dias_ult_mov_sobre_antiguedad",
                                 "AAHO_TIP_NOMIN",
                                 "AAHO_SAL_HOY_menos_max_pasado5",
                                 "AAHO_SAL_MINIMO",
                                 "Antiguedad",
                                 "fue_inactiva",
                                 "AAHO_SAL_HOY_sobre_max_pasado5",
                                 "veces_productivo",
                                 "AAHO_SAL_MINIMO_siendoproductivo",
                                 "AAHO_SAL_MINIMO_sobre_max_pasado5",
                                 "AAHO_SAL_MINIMO_menos_max_pasado5",
                                 "AAHO_EXEN_GMF",
                                 "TOTAL_PRODUCTOS_ACTIVO")#,
#                                 "CANT_CTAS_AHORRO_menos2",
 #                                "tenencia_menos_min_tenencia5")
    
  } else {
    stop("La parametrizacion del modelo no es correcta. El valor de modelo debe ser de la forma -modelo_#-.")
}



