########################################################
# Funciones básicas para el modelo de Churn de Ahorros #
########################################################


#' Filtro por regiones - Churn Ahorros
#'
#' @param Ahorros La tabla.
#' @param filtro_oficinas Vector con cuales oficinas son incluidas en el analisis.
#'
#' @return La tabla principal con el filtro aplicado.
#'
#' @examples
#' Ahorros <- Filtro_regiones(Ahorros = Ahorros, filtro_oficinas = filtro_oficinas)
#' 
Filtro_regiones <- function(Ahorros, filtro_oficinas) {
  
  Diccionario_regiones <- fread(file = "Z:/Informacion_Centralizada/Data_Analytics/Tablas_de_referencia/SARC_REGIONES.csv")
  suppressWarnings(Diccionario_regiones$BREGI_CODIGO <- as.numeric(Diccionario_regiones$BREGI_CODIGO))
  Ahorros$AAHO_REGION <- as.numeric(Ahorros$AAHO_REGION)
  names(Diccionario_regiones)[2] <- "Region" 
  Diccionario_regiones <- Diccionario_regiones[is.na(BREGI_CODIGO) == FALSE]
  Ahorros <- left_join(Ahorros, Diccionario_regiones, by = c("AAHO_REGION" = "BREGI_CODIGO"))
  Ahorros <- data.table(Ahorros)
  
  print(paste0("Se eliminaron ", sum(!(Ahorros$Region %in% filtro_oficinas)), " cuentas con una region no considerada por el modelo."))
  Ahorros <- Ahorros[Region %in% filtro_oficinas]
  Ahorros$Region <- factor(Ahorros$Region)

  return(Ahorros)
}


#' Filtro de cuentas cafeteras - Churn Ahorros
#'
#' @param Ahorros La tabla.
#' @param filtro_cafeteras Valores de SUBPRODUCTO que deben ser excluidas por ser cafeteras/agremiadas.
#'
#' @return La tabla principal con el filtro aplicado.
#'
#' @examples
#' Ahorros <- Filtro_cafeteras(Ahorros = Ahorros, filtro_cafeteras = filtro_cafeteras)
#' 
Filtro_cafeteras <- function(Ahorros, filtro_cafeteras) {
  print(paste0("Se eliminaron ", sum(Ahorros$AAHO_COD_SUB_PRO %in% filtro_cafeteras), " cuentas cafeteras/agremiadas."))
  Ahorros <- Ahorros[AAHO_COD_SUB_PRO %!in% filtro_cafeteras]
  return(Ahorros)
}


#' Filtro de personas juridicas - Churn Ahorros
#'
#' @param Ahorros La tabla.
#' @param filtro_tipoid Valores de tipo de identificacion que deben ser excluidas por ser personas juridicas.
#'
#' @return La tabla principal con el filtro aplicado.
#'
#' @examples
#' Ahorros <- Filtro_tipoid(Ahorros = Ahorros, filtro_tipoid = filtro_tipoid)
#' 
Filtro_tipoid <- function(Ahorros, filtro_tipoid) {
  print(paste0("Se eliminaron ", sum(Ahorros$AAHO_TIP_IDEN %in% filtro_tipoid), " cuentas de personas juridicas."))
  Ahorros <- Ahorros[AAHO_TIP_IDEN %!in% filtro_tipoid]
  return(Ahorros)
}



#' Aplicar los filtros a la tabla de Ahorros
#'
#' @param Ahorros La tabla.
#' @param filtro_oficinas Vector con cuales oficinas son incluidas en el analisis.
#' @param filtro_cafeteras Valores de SUBPRODUCTO que deben ser excluidas por ser cafeteras/agremiadas.
#' @param filtro_tipoid Valores de tipo de identificacion que deben ser excluidas por ser personas juridicas.
#'
#' @return La tabla principal con los filtros aplicados.
#'
#' @examples
#' Ahorros <- Filtros(Ahorros = Ahorros, filtro_oficinas = filtro_oficinas, filtro_cafeteras = filtro_cafeteras, filtro_tipoid = filtro_tipoid)
#' 
Filtros <- function(Ahorros, filtro_oficinas, filtro_cafeteras, filtro_tipoid) {
  Ahorros <- Filtro_regiones(Ahorros = Ahorros, filtro_oficinas = filtro_oficinas)
  Ahorros <- Filtro_cafeteras(Ahorros = Ahorros, filtro_cafeteras = filtro_cafeteras)
  Ahorros <- Filtro_tipoid(Ahorros = Ahorros, filtro_tipoid = filtro_tipoid)
  return(Ahorros)
}



#' Uso del diccionario para la variable ESTADO, y eliminar los registros de cuentas CONGELADAS y EMBARGADAS
#'
#' @param Ahorros La tabla.
#'
#' @return La tabla principal con la variable ESTADO.
#'
#' @examples
#' Ahorros <- Variable_estado(Ahorros = Ahorros)
#' 
Variable_ESTADO <- function(Ahorros) {
  
  Diccionario_estado <- fread(file = "Z:/Informacion_Centralizada/Data_Analytics/Tablas_de_referencia/OTROS/ahorros_estado.csv", integer64 = "character")
  Diccionario_estado$AAHO_ESTADO <- as.numeric(Diccionario_estado$AAHO_ESTADO)
  Ahorros$AAHO_ESTADO <- as.numeric(Ahorros$AAHO_ESTADO)
  Ahorros <- left_join(Ahorros, Diccionario_estado, by = "AAHO_ESTADO")
  Ahorros <- data.table(Ahorros)
  Ahorros$ESTADO <- gsub("[[:space:]]", "", Ahorros$ESTADO)
  
  #Filtrar Congeladas y embargadas
  print(paste0("Se eliminaron ", sum(Ahorros$ESTADO == "CONGELADA" | Ahorros$ESTADO == "EMBARGADA"), " cuentas Congeladas y Embargadas."))
  Ahorros <- Ahorros[ESTADO != "CONGELADA" & ESTADO != "EMBARGADA"]
  return(Ahorros)
}



#' Creacion de la variable Antiguedad y Antiguedad_categoria
#'
#' @param Ahorros La tabla principal.
#' @param Mes El mes de corte. Debe estar en formato "YYYY_MM".
#'
#' @return La tabla con las variables Antiguedad y Antiguedad_categoria, incorporadas.
#'
#' @examples
#' Ahorros <- Variables_antiguedad(Ahorros = Ahorros, Mes = Mes)
#' 
Variables_antiguedad <- function(Ahorros, Mes){
  
  Cierres <- c(31,        28     ,  31    ,   30   ,  31   ,  30   ,   31   ,   31     ,     30        ,   31     ,   30        , 31)
  if (as.numeric(substr(Mes, 1, 4)) %% 4 == 0) { Cierres[2] <- 29}
  Cierre_mes <- as.Date(paste0(substr(Mes, 1, 4), "/", substr(Mes, 6, 7), "/", Cierres[as.numeric(substr(Mes, 6, 7))]))
  
  #Antiguedad en anos
  Ahorros[, Antiguedad := (as.numeric(Cierre_mes - as.Date(AAHO_FEC_APERTURA)))/365] 
  
  #Antiguedad categorica
  Ahorros[, Antiguedad_categoria := floor(Antiguedad)]
  Ahorros[Antiguedad_categoria >= 10]$Antiguedad_categoria <- 10
  Ahorros$Antiguedad_categoria <- as.character(Ahorros$Antiguedad_categoria)
  Ahorros[Antiguedad_categoria == "10"]$Antiguedad_categoria <- "10 o mas"
  Ahorros$Antiguedad_categoria <- factor(Ahorros$Antiguedad_categoria,
                                         levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10 o mas"))
  return(Ahorros)
}



#' Creacion de variables de dias desde el ultimo movimiento
#'
#' @param Ahorros La tabla principal.
#' @param Mes El mes de corte. Debe estar en formato "YYYY_MM".
#'
#' @return La tabla con las variables Dias desde el ultimo movimiento en dias y en categorica, incorporadas.
#'
#' @examples
#' Ahorros <- Variables_dias_ult_mov(Ahorros = Ahorros, Mes = Mes)
#' 
Variables_dias_ult_mov <- function(Ahorros, Mes) {
  
  Cierres <- c(31,        28     ,  31    ,   30   ,  31   ,  30   ,   31   ,   31     ,     30        ,   31     ,   30        , 31)
  if (as.numeric(substr(Mes, 1, 4)) %% 4 == 0) { Cierres[2] <- 29}
  Cierre_mes <- as.Date(paste0(substr(Mes, 1, 4), "/", substr(Mes, 6, 7), "/", Cierres[as.numeric(substr(Mes, 6, 7))]))
  
  #Dias desde el ultimo movimiento
  Ahorros[, Dias_ult_mov := as.numeric(Cierre_mes - as.Date(AAHO_FEC_ULT_MOV))] 
  
  #######
  # Necesidad de ajustar con ultimo deposito sin intereses, porque ultimo movimiento no incluye depositos
  #######
  
  suppressWarnings(ultimo_deposito <- fread(file = paste0(getwd(), "/Data/Original/Ultimo_deposito/ULTIMO_DEPOSITO_", substr(Mes, 1, 4), substr(Mes, 6, 7), ".csv")))
  ultimo_deposito$AAHO_NUM_CUENTA <- as.numeric(ultimo_deposito$AAHO_NUM_CUENTA)
  #ultimo_deposito <- Arreglo_fecha_multiple(Data = ultimo_deposito, Variables = c("MAX_of_AAHO_FEC_ULT_DEP"))
  ultimo_deposito[, MAX_of_AAHO_FEC_ULT_DEP := as.Date(ultimo_deposito$AAHO_FEC_ULT_DEP, format = "%d/%m/%Y")]
  ultimo_deposito <- ultimo_deposito %>% dplyr::select(AAHO_NUM_CUENTA, MAX_of_AAHO_FEC_ULT_DEP)
  #ultimo_deposito <- ultimo_deposito[, max(MAX_of_AAHO_FEC_ULT_DEP), by = AAHO_NUM_CUENTA]
  names(ultimo_deposito) <- c("AAHO_NUM_CUENTA", "MAX_of_AAHO_FEC_ULT_DEP")
  
  if(length(unique(ultimo_deposito$AAHO_NUM_CUENTA)) < nrow(ultimo_deposito)) {stop(paste0("Hay registros duplicados en la informacion de ultimo deposito del mes ", Mes))}
  Ahorros <- left_join(Ahorros, ultimo_deposito, by = "AAHO_NUM_CUENTA")
  Ahorros <- data.table(Ahorros)
  
  Ahorros[, Dias_ult_dep_sinintereses := as.numeric(Cierre_mes - as.Date(MAX_of_AAHO_FEC_ULT_DEP))]
  
  ###
  Ahorros[!is.na(Dias_ult_dep_sinintereses),# & MAX_of_AAHO_FEC_ULT_DEP != "", 
          Dias_ult_mov := pmin(Dias_ult_mov, Dias_ult_dep_sinintereses)]
  ######Ahorros[is.na(MAX_of_AAHO_FEC_ULT_DEP) & is.na(AAHO_FEC_ULT_DEP), 
  ######        Dias_ult_mov := Dias_ult_mov]
  ####Ahorros[is.na(MAX_of_AAHO_FEC_ULT_DEP) | MAX_of_AAHO_FEC_ULT_DEP == "", ###### & !is.na(AAHO_FEC_ULT_DEP) & ESTADO == "INACTIVA",
  ####        Dias_ult_mov := 200]
  ######Ahorros[is.na(MAX_of_AAHO_FEC_ULT_DEP) & !is.na(AAHO_FEC_ULT_DEP) & ESTADO != "INACTIVA" & Dias_ult_mov <= 180,
  ######        Dias_ult_mov := Dias_ult_mov]
  
  #####Ahorros[is.na(MAX_of_AAHO_FEC_ULT_DEP) & !is.na(AAHO_FEC_ULT_DEP) & ESTADO != "INACTIVA" & Dias_ult_mov > 180,
  #####        Dias_ult_mov := 150]
  
  #####Ahorros[ESTADO == "INACTIVA", Dias_ult_mov := 200]

  #######
  
  #Armar categorias
  Ahorros[, Dias_ult_mov_cat := ifelse(Dias_ult_mov <= 60, "Menos de 61 dias", 
                                       ifelse(Dias_ult_mov <= 180, "De 61 a 180 dias",
                                              ifelse(Dias_ult_mov > 180, "Mas de 180 dias", "Missing")))]
  Ahorros[AAHO_ESTADO %in% c(3, 4, 5)]$Dias_ult_mov_cat <- "Cancelada" #Las purgadas y cerradas se van a canceladas
  Ahorros$Dias_ult_mov_cat <- factor(Ahorros$Dias_ult_mov_cat, 
                                     levels = c("Menos de 61 dias", "De 61 a 180 dias", "Mas de 180 dias", "Cancelada", "Missing"))

  #######
  
  #Dias ult movimiento sobre antiguedad
  Ahorros[, dias_ult_mov_sobre_antiguedad := Dias_ult_mov/Antiguedad]
  Ahorros[Antiguedad <= 0, dias_ult_mov_sobre_antiguedad := 0]
  
  return(Ahorros)
}



#' Creacion de la variable Dias desde el ultimo deposito
#'
#' @param Ahorros La tabla principal.
#'
#' @return La tabla con la variable Dias desde el ultimo deposito, incorporada.
#'
#' @examples
#' Ahorros <- Variable_dias_ult_dep(Ahorros = Ahorros)
#' 
Variable_dias_ult_dep <- function(Ahorros){
  
  Ahorros[, Dias_ult_dep := as.numeric(Cierre_mes - as.Date(AAHO_FEC_ULT_DEP))] 
  
  Ahorros[is.na(Dias_ult_dep), Dias_ult_dep := 1]
  Ahorros[Dias_ult_dep != 0, Dias_ult_dep := 1]
  
  Ahorros$Dias_ult_dep <- factor(Ahorros$Dias_ult_dep,
                                 levels = c(0, 1))
  
  return(Ahorros)
}





#' Creacion de variables de si esta excento a Movimientos Gravamen Financiero
#'
#' @param Ahorros La tabla principal.
#'
#' @return La tabla con la variable incorporada.
#'
#' @examples
#' Ahorros <- Variable_GMF(Ahorros = Ahorros)
#' 
Variable_GMF <- function(Ahorros) {
  
  Ahorros$AAHO_EXEN_GMF <- as.character(Ahorros$AAHO_EXEN_GMF)
  Ahorros[AAHO_EXEN_GMF == "" | AAHO_EXEN_GMF == ":" | AAHO_EXEN_GMF == "0"]$AAHO_EXEN_GMF <- "0"
  Ahorros[AAHO_EXEN_GMF != "" & AAHO_EXEN_GMF != ":" & AAHO_EXEN_GMF != "0"]$AAHO_EXEN_GMF <- "1"
  Ahorros$AAHO_EXEN_GMF <- as.numeric(Ahorros$AAHO_EXEN_GMF)
  Ahorros$AAHO_EXEN_GMF <- factor(Ahorros$AAHO_EXEN_GMF, levels = c(0, 1))
  
  return(Ahorros)
}


#' Creacion de variables de si es de nomina o no, segun el codigo de subproducto.
#' Reemplaza la variable AAHO_TIP_NOMIN
#'
#' @param Ahorros La tabla principal.
#'
#' @return La tabla con la variable incorporada.
#'
#' @examples
#' Ahorros <- Variable_nomina(Ahorros = Ahorros)
#' 
Variable_nomina <- function(Ahorros){
  
  Codigo_nomina <- 10
  
  Ahorros[, AAHO_TIP_NOMIN := NULL]
  Ahorros[, AAHO_TIP_NOMIN := ifelse(AAHO_COD_SUB_PRO == 10, 1, 0)]
  Ahorros$AAHO_TIP_NOMIN <- factor(Ahorros$AAHO_TIP_NOMIN)
  
  return(Ahorros)
}



#' Creacion de las variables relacionadas a PQRS
#'
#' @param Ahorros La tabla principal.
#' @param Mes El mes de corte. Debe estar en formato "YYYY_MM".
#'
#' @return La tabla con las variables de PQRS, incorporadas.
#'
#' @examples
#' Ahorros <- Variables_PQRS(Ahorros = Ahorros, Mes = Mes)
#' 
Variables_PQRS <- function(Ahorros, Mes) {
  
  #Meses atras para conteo de PQRS
  Meses_PQRS <- 6
  
  #Generar vector con los ultimos meses
  Mes_aux <- (as.numeric(substr(Mes, 6, 7)) - Meses_PQRS + 1)%%12
  if(Mes_aux == 0){Mes_aux <- 12}
  if(Mes_aux < 10){Mes_aux <- paste0(0, Mes_aux)}
  Ano_aux <- as.numeric(substr(Mes, 1, 4)) - ceiling((-1*(as.numeric(substr(Mes, 6, 7)) - Meses_PQRS + 1) + 1)/12)
  Mes_inicial_PQRS <- paste0(Ano_aux, "_", Mes_aux) #Mes con el que empezamos
  
  Ultimos_meses <- Mes_inicial_PQRS
  if(Meses_PQRS > 1){
    for (i in 2:Meses_PQRS){
      Ultimos_meses <- c(Ultimos_meses, Mes_adelante_n(Ultimos_meses[length(Ultimos_meses)], 1)) 
    }
  } #Le va pegando los siguientes meses
  
  #Lista con las tablas
  Bases <- list()
  Nombres <- list()
  for (t in Ultimos_meses) {
    suppressWarnings(Bases[[t]] <- fread(
      file = paste(
        "Z:/Informacion_Centralizada/Data_Analytics/PQRS/ODS_PQRS_",
        t, #aca va variando entre los valores del vector Ultimos_3meses
        ".csv",
        sep = ""
      ), integer64 = "character", showProgress = F
    ))
    Bases[[t]]$Periodo <- t
    Nombres[[t]] <- names(Bases[[t]])
  }
  names(Bases) <- paste("PQRS_", names(Bases), sep = "") #Ponerle nombre a los elementos de la lista segun el periodo
  
  #Revision de que todas las bases tengan las mismas variables en el mismo orden:
  Iguales <- c()
  for (i in 2:length(Bases)){
    Iguales[i] <- identical(Nombres[[i]], Nombres[[1]])
  }
  Iguales <- Iguales[-1]
  
  if (sum(Iguales) != length(Iguales)) { stop(paste0("Hay alguna tabla de PQRS con variables diferentes o en diferente orden. Reviselo teniendo en cuenta que Mes vale ", Mes)) }
  
  #Pegar las tablas en una sola
  PQRS <- Bases[[1]]
  for (i in 2:length(Bases)){
    PQRS <- rbind(PQRS, Bases[[i]])
  }
  PQRS <- data.table(PQRS) #Clase: data table
  
  ###Agrupar por cliente para todas las quejas + solicitudes
  
  #Solo QR y SO
  PQRS <- PQRS[tipo_pqr == "QUEJA/RECLAMO"| tipo_pqr == "SOLICITUD"]
  
  #Pegar producto:
  Dicc_productos <- fread(file = "Z:/Informacion_Centralizada/Data_Analytics/Tablas_de_referencia/OTROS/Diccionario_productos_PQRS.csv", header = T, integer64 = "character")
  PQRS <- left_join(PQRS, Dicc_productos, 
                    by = c("NOV_TIPO_PRO" = "PRODUCT_ID")) #pegarle el producto segun el diccionario
  PQRS <- data.table(PQRS)
  
  #Generar tabla con data table
  Novedades_cliente_producto <- PQRS[, .N,
                                     by = .(tipo_pqr, NOV_TIPO_DOCU_CLI, NOV_NUM_DOCU_CLI, producto)][order(-N)]
  
  #Tabla que queremos: por cliente, cuantas QR, SO, QR(AH), SO(AH) tiene
  PQRS_cliente <- Novedades_cliente_producto[, 
                                             .(QR = sum(tipo_pqr == "QUEJA/RECLAMO"),
                                               SO = sum(tipo_pqr == "SOLICITUD"),
                                               QR_AH = sum(tipo_pqr == "QUEJA/RECLAMO" & producto == "Cuenta Ahorros"), 
                                               SO_AH = sum(tipo_pqr == "SOLICITUD" & producto == "Cuenta Ahorros")), 
                                             by = .(NOV_TIPO_DOCU_CLI, NOV_NUM_DOCU_CLI)]
  PQRS_cliente[is.na(PQRS_cliente)] <- 0
  
  ###Pegarle a la data la informacion de PQRS
  Ahorros$AAHO_NUM_IDEN <- as.numeric(Ahorros$AAHO_NUM_IDEN)
  suppressWarnings(PQRS_cliente$NOV_NUM_DOCU_CLI <- as.numeric(PQRS_cliente$NOV_NUM_DOCU_CLI))
  
  Ahorros <- left_join(Ahorros, PQRS_cliente, by = c("AAHO_TIP_IDEN" = "NOV_TIPO_DOCU_CLI", 
                                                     "AAHO_NUM_IDEN" = "NOV_NUM_DOCU_CLI"))
  Ahorros <- data.table(Ahorros)
  
  Ahorros[is.na(SO)]$SO <- 0
  Ahorros[is.na(QR)]$QR <- 0
  Ahorros[is.na(SO_AH)]$SO_AH <- 0
  Ahorros[is.na(QR_AH)]$QR_AH <- 0
  
  Ahorros[, Novedades := SO + QR]
  
  return(Ahorros)
}




#' Creacion de las variables relacionadas a PQRS - valores del mes actual menos valores del mes pasado
#'
#' @param Ahorros La tabla principal.
#' @param Mes El mes de corte. Debe estar en formato "YYYY_MM". Se hara la resta de los valores de Mes, menos Mes-1.
#'
#' @return La tabla con las variables de PQRS en diferencias mensuales, incorporadas.
#'
#' @examples
#' Ahorros <- Variables_PQRS_cambios(Ahorros = Ahorros, Mes = Mes)
#' 
Variables_PQRS_cambios <- function(Ahorros, Mes) {
  
  Meses_PQRS <- 2 #Traer el mes actual y el mes anterior
  
  #Generar vector con los ultimos meses
  Mes_aux <- (as.numeric(substr(Mes, 6, 7)) - Meses_PQRS + 1)%%12
  if(Mes_aux == 0){Mes_aux <- 12}
  if(Mes_aux < 10){Mes_aux <- paste0(0, Mes_aux)}
  Ano_aux <- as.numeric(substr(Mes, 1, 4)) - ceiling((-1*(as.numeric(substr(Mes, 6, 7)) - Meses_PQRS + 1) + 1)/12)
  Mes_inicial_PQRS <- paste0(Ano_aux, "_", Mes_aux) #Mes con el que empezamos
  
  Ultimos_meses <- Mes_inicial_PQRS
  if(Meses_PQRS > 1){
    for (i in 2:Meses_PQRS){
      Ultimos_meses <- c(Ultimos_meses, Mes_adelante_n(Ultimos_meses[length(Ultimos_meses)], 1)) 
    }
  } #Le va pegando los siguientes meses
  
  #Lista con las tablas
  Bases <- list()
  Nombres <- list()
  for (t in Ultimos_meses) {
    suppressWarnings(Bases[[t]] <- fread(
      file = paste(
        "Z:/Informacion_Centralizada/Data_Analytics/PQRS/ODS_PQRS_",
        t, #aca va variando entre los valores del vector Ultimos_3meses
        ".csv",
        sep = ""
      ), integer64 = "character", showProgress = F
    ))
    Bases[[t]]$Periodo <- t
    Nombres[[t]] <- names(Bases[[t]])
  }
  names(Bases) <- paste("PQRS_", names(Bases), sep = "") #Ponerle nombre a los elementos de la lista segun el periodo
  
  #Revision de que todas las bases tengan las mismas variables en el mismo orden:
  Iguales <- c()
  for (i in 2:length(Bases)){
    Iguales[i] <- identical(Nombres[[i]], Nombres[[1]])
  }
  Iguales <- Iguales[-1]
  
  if (sum(Iguales) != length(Iguales)) { stop(paste0("Hay alguna tabla de PQRS con variables diferentes o en diferente orden. Reviselo teniendo en cuenta que Mes vale ", Mes)) }
  
  #Pegar las tablas en una sola
  PQRS <- Bases[[1]]
  for (i in 2:length(Bases)){
    PQRS <- rbind(PQRS, Bases[[i]])
  }
  PQRS <- data.table(PQRS) #Clase: data table
  
  ###Agrupar por cliente para todas las quejas + solicitudes
  
  #Solo QR y SO
  PQRS <- PQRS[tipo_pqr == "QUEJA/RECLAMO"| tipo_pqr == "SOLICITUD"]
  
  #Pegar producto:
  Dicc_productos <- fread(file = "Z:/Informacion_Centralizada/Data_Analytics/Tablas_de_referencia/OTROS/Diccionario_productos_PQRS.csv", header = T, integer64 = "character")
  PQRS <- left_join(PQRS, Dicc_productos, 
                    by = c("NOV_TIPO_PRO" = "PRODUCT_ID")) #pegarle el producto segun el diccionario
  PQRS <- data.table(PQRS)
  
  #Generar tabla con data table
  Novedades_cliente_producto <- PQRS[, .N,
                                     by = .(tipo_pqr, NOV_TIPO_DOCU_CLI, NOV_NUM_DOCU_CLI, producto, Periodo)][order(-N)]
  
  #Tabla que queremos: por cliente, cuantas QR, SO, QR(AH), SO(AH) tiene
  PQRS_cliente <- Novedades_cliente_producto[, 
                                             .(QR = sum(tipo_pqr == "QUEJA/RECLAMO"),
                                               SO = sum(tipo_pqr == "SOLICITUD"),
                                               QR_AH = sum(tipo_pqr == "QUEJA/RECLAMO" & producto == "Cuenta Ahorros"), 
                                               SO_AH = sum(tipo_pqr == "SOLICITUD" & producto == "Cuenta Ahorros")), 
                                             by = .(NOV_TIPO_DOCU_CLI, NOV_NUM_DOCU_CLI, Periodo)]
  PQRS_cliente[is.na(PQRS_cliente)] <- 0
  
  
  PQRS_cliente_SO <- dcast(setDT(PQRS_cliente), formula = NOV_TIPO_DOCU_CLI + NOV_NUM_DOCU_CLI ~ Periodo, value.var = c("SO"))
  PQRS_cliente_SO <- data.table(PQRS_cliente_SO)
  PQRS_cliente_SO[is.na(PQRS_cliente_SO)] <- 0
  PQRS_cliente_SO[, SO_inicial :=  mget(Mes_atras_n(Mes, 1))]
  PQRS_cliente_SO[, SO_final :=  mget(Mes)]
  PQRS_cliente_SO[, SO_diff := SO_final - SO_inicial]
  PQRS_cliente_SO <- PQRS_cliente_SO[, .(NOV_TIPO_DOCU_CLI, NOV_NUM_DOCU_CLI, SO_diff)]
  
  PQRS_cliente_QR <- dcast(setDT(PQRS_cliente), formula = NOV_TIPO_DOCU_CLI + NOV_NUM_DOCU_CLI ~ Periodo, value.var = c("QR"))
  PQRS_cliente_QR <- data.table(PQRS_cliente_QR)
  PQRS_cliente_QR[is.na(PQRS_cliente_QR)] <- 0
  PQRS_cliente_QR[, QR_inicial :=  mget(Mes_atras_n(Mes, 1))]
  PQRS_cliente_QR[, QR_final :=  mget(Mes)]
  PQRS_cliente_QR[, QR_diff := QR_final - QR_inicial]  
  PQRS_cliente_QR <- PQRS_cliente_QR[, .(NOV_TIPO_DOCU_CLI, NOV_NUM_DOCU_CLI, QR_diff)]
  
  PQRS_cliente_SO_AH <- dcast(setDT(PQRS_cliente), formula = NOV_TIPO_DOCU_CLI + NOV_NUM_DOCU_CLI ~ Periodo, value.var = c("SO_AH"))
  PQRS_cliente_SO_AH <- data.table(PQRS_cliente_SO_AH)
  PQRS_cliente_SO_AH[is.na(PQRS_cliente_SO_AH)] <- 0
  PQRS_cliente_SO_AH[, SO_AH_inicial :=  mget(Mes_atras_n(Mes, 1))]
  PQRS_cliente_SO_AH[, SO_AH_final :=  mget(Mes)]
  PQRS_cliente_SO_AH[, SO_AH_diff := SO_AH_final - SO_AH_inicial]  
  PQRS_cliente_SO_AH <- PQRS_cliente_SO_AH[, .(NOV_TIPO_DOCU_CLI, NOV_NUM_DOCU_CLI, SO_AH_diff)]
  
  PQRS_cliente_QR_AH <- dcast(setDT(PQRS_cliente), formula = NOV_TIPO_DOCU_CLI + NOV_NUM_DOCU_CLI ~ Periodo, value.var = c("QR_AH"))
  PQRS_cliente_QR_AH <- data.table(PQRS_cliente_QR_AH)
  PQRS_cliente_QR_AH[is.na(PQRS_cliente_QR_AH)] <- 0
  PQRS_cliente_QR_AH[, QR_AH_inicial :=  mget(Mes_atras_n(Mes, 1))]
  PQRS_cliente_QR_AH[, QR_AH_final :=  mget(Mes)]
  PQRS_cliente_QR_AH[, QR_AH_diff := QR_AH_final - QR_AH_inicial]  
  PQRS_cliente_QR_AH <- PQRS_cliente_QR_AH[, .(NOV_TIPO_DOCU_CLI, NOV_NUM_DOCU_CLI, QR_AH_diff)]
  
  PQRS_cliente_diff <- full_join(PQRS_cliente_SO, PQRS_cliente_QR, by = c("NOV_TIPO_DOCU_CLI", "NOV_NUM_DOCU_CLI"))
  PQRS_cliente_diff <- full_join(PQRS_cliente_diff, PQRS_cliente_SO_AH, by = c("NOV_TIPO_DOCU_CLI", "NOV_NUM_DOCU_CLI"))  
  PQRS_cliente_diff <- full_join(PQRS_cliente_diff, PQRS_cliente_QR_AH, by = c("NOV_TIPO_DOCU_CLI", "NOV_NUM_DOCU_CLI"))  
  
  ###Pegarle a la data la informacion de PQRS
  Ahorros$AAHO_NUM_IDEN <- as.numeric(Ahorros$AAHO_NUM_IDEN)
  suppressWarnings(PQRS_cliente_diff$NOV_NUM_DOCU_CLI <- as.numeric(PQRS_cliente_diff$NOV_NUM_DOCU_CLI))
  
  Ahorros <- left_join(Ahorros, PQRS_cliente_diff, by = c("AAHO_TIP_IDEN" = "NOV_TIPO_DOCU_CLI", 
                                                          "AAHO_NUM_IDEN" = "NOV_NUM_DOCU_CLI"))
  Ahorros <- data.table(Ahorros)
  
  Ahorros[is.na(SO_diff)]$SO_diff <- 0
  Ahorros[is.na(QR_diff)]$QR_diff <- 0
  Ahorros[is.na(SO_AH_diff)]$SO_AH_diff <- 0
  Ahorros[is.na(QR_AH_diff)]$QR_AH_diff <- 0
  
  Ahorros[, Novedades_diff := SO_diff + QR_diff]
  
  Ahorros[SO_diff >= 1]$SO_diff <- 1
  Ahorros[SO_diff <= -1]$SO_diff <- -1
  Ahorros$SO_diff <- as.character(Ahorros$SO_diff)
  Ahorros[SO_diff == "1"]$SO_diff <- "1 o mas"
  Ahorros[SO_diff == "-1"]$SO_diff <- "-1 o menos"
  Ahorros$SO_diff <- factor(Ahorros$SO_diff, levels = c("-1 o menos", "0", "1 o mas"))
  
  Ahorros[QR_diff >= 1]$QR_diff <- 1
  Ahorros[QR_diff <= -1]$QR_diff <- -1
  Ahorros$QR_diff <- as.character(Ahorros$QR_diff)
  Ahorros[QR_diff == "1"]$QR_diff <- "1 o mas"
  Ahorros[QR_diff == "-1"]$QR_diff <- "-1 o menos"
  Ahorros$QR_diff <- factor(Ahorros$QR_diff, levels = c("-1 o menos", "0", "1 o mas"))  

  Ahorros[SO_AH_diff >= 1]$SO_AH_diff <- 1
  Ahorros[SO_AH_diff <= -1]$SO_AH_diff <- -1
  Ahorros$SO_AH_diff <- as.character(Ahorros$SO_AH_diff)
  Ahorros[SO_AH_diff == "1"]$SO_AH_diff <- "1 o mas"
  Ahorros[SO_AH_diff == "-1"]$SO_AH_diff <- "-1 o menos"
  Ahorros$SO_AH_diff <- factor(Ahorros$SO_AH_diff, levels = c("-1 o menos", "0", "1 o mas"))  

  Ahorros[QR_AH_diff >= 1]$QR_AH_diff <- 1
  Ahorros[QR_AH_diff <= -1]$QR_AH_diff <- -1
  Ahorros$QR_AH_diff <- as.character(Ahorros$QR_AH_diff)
  Ahorros[QR_AH_diff == "1"]$QR_AH_diff <- "1 o mas"
  Ahorros[QR_AH_diff == "-1"]$QR_AH_diff <- "-1 o menos"
  Ahorros$QR_AH_diff <- factor(Ahorros$QR_AH_diff, levels = c("-1 o menos", "0", "1 o mas"))  
  
  Ahorros[Novedades_diff >= 1]$Novedades_diff <- 1
  Ahorros[Novedades_diff <= -1]$Novedades_diff <- -1
  Ahorros$Novedades_diff <- as.character(Ahorros$Novedades_diff)
  Ahorros[Novedades_diff == "1"]$Novedades_diff <- "1 o mas"
  Ahorros[Novedades_diff == "-1"]$Novedades_diff <- "-1 o menos"
  Ahorros$Novedades_diff <- factor(Ahorros$Novedades_diff, levels = c("-1 o menos", "0", "1 o mas"))
  
  return(Ahorros)
}




#' Creacion de las variables de tenencia
#'
#' @param Ahorros La tabla principal.
#' @param Mes El mes de corte. Debe estar en formato "YYYY_MM".
#'
#' @return La tabla con las variables de Tenencia, incorporadas.
#'
#' @examples
#' Ahorros <- Variables_tenencia(Ahorros = Ahorros, Mes = Mes)
#' 
Variables_tenencia <- function(Ahorros, Mes) {
  
  Tenencia <- fread(file = paste0("Z:/Informacion_Centralizada/Data_Analytics/Tenencia_Productos/TENENCIA_", Mes, ".csv"), integer64 = "character", showProgress = F)
  Tenencia$NUMERO_IDENTIFICACION <- as.numeric(Tenencia$NUMERO_IDENTIFICACION)
  
  #Cambiar los NA por 0
  Cantidades <- grep("CANT_", names(Tenencia), value = T)  ##Sacar el VALOR de las variables que empiecen con "cant_"
  Tenencia2 <- Tenencia[, ..Cantidades]
  Tenencia2[is.na(Tenencia2)] <- 0
  Tenencia <- cbind(Tenencia$TIPO_IDENTIFICACION, Tenencia$NUMERO_IDENTIFICACION, Tenencia2)
  names(Tenencia)[1:2] <- c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION")
  
  #Variables realmente relevantes: Cantidad total de productos y Cantidad total de productos del activo
  Tenencia[, TOTAL_PRODUCTOS := rowSums(Tenencia2)]
  Productos_activo <- paste0("CANT_", c("TARJ_CREDITO", "CREDISERVICE", "ORDINARIO", "LIBRANZA", "LIBRE_DESTINO", "VEHICULO", "LEASING", "VIVIENDA", "MICROCREDITO"))
  Tenencia3 <- Tenencia[, ..Productos_activo]
  Tenencia[, TOTAL_PRODUCTOS_ACTIVO := rowSums(Tenencia3)]
  
  Tenencia[, CANT_CTAS_AHORRO := CANT_CTAS_AHORRO + CANT_CTAS_NOMINA]
  
  Tenencia <- Tenencia[, .(TIPO_IDENTIFICACION, NUMERO_IDENTIFICACION, TOTAL_PRODUCTOS, TOTAL_PRODUCTOS_ACTIVO, CANT_TARJ_CREDITO, CANT_CTAS_AHORRO)]
  
  #Pegarle a la data la informacion de Tenencia
  Tenencia$NUMERO_IDENTIFICACION <- as.numeric(Tenencia$NUMERO_IDENTIFICACION)
  Ahorros <- left_join(Ahorros, Tenencia, by = c("AAHO_TIP_IDEN" = "TIPO_IDENTIFICACION", 
                                                 "AAHO_NUM_IDEN" = "NUMERO_IDENTIFICACION"))
  Ahorros <- data.table(Ahorros)
  
  Ahorros[is.na(TOTAL_PRODUCTOS)]$TOTAL_PRODUCTOS <- 1
  Ahorros[is.na(TOTAL_PRODUCTOS_ACTIVO)]$TOTAL_PRODUCTOS_ACTIVO <- 0
  Ahorros[is.na(CANT_TARJ_CREDITO)]$CANT_TARJ_CREDITO <- 0
  Ahorros[is.na(CANT_CTAS_AHORRO)]$CANT_CTAS_AHORRO <- 1
  
  return(Ahorros)
}


#' Creacion de las variables de tenencia, rezagadas 1 mes
#'
#' @param Ahorros La tabla principal.
#' @param Mes El mes de corte. Debe estar en formato "YYYY_MM". Ojo: no poner el Mes rezagado. Es el mismo mes de Variables_tenencia.
#'
#' @return La tabla con las variables de Tenencia rezagadas, incorporadas.
#'
#' @examples
#' Ahorros <- Variables_tenencia_rezagadas(Ahorros = Ahorros, Mes = Mes)
#' 
Variables_tenencia_rezagadas <- function(Ahorros, Mes) {
  
  Tenencia <- fread(file = paste0("Z:/Informacion_Centralizada/Data_Analytics/Tenencia_Productos/TENENCIA_", Mes_atras_n(Mes, 1), ".csv"), integer64 = "character", showProgress = F)
  Tenencia$NUMERO_IDENTIFICACION <- as.numeric(Tenencia$NUMERO_IDENTIFICACION)
  
  #Cambiar los NA por 0
  Cantidades <- grep("CANT_", names(Tenencia), value = T)  ##Sacar el VALOR de las variables que empiecen con "cant_"
  Tenencia2 <- Tenencia[, ..Cantidades]
  Tenencia2[is.na(Tenencia2)] <- 0
  Tenencia <- cbind(Tenencia$TIPO_IDENTIFICACION, Tenencia$NUMERO_IDENTIFICACION, Tenencia2)
  names(Tenencia)[1:2] <- c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION")
  
  #Variables realmente relevantes: Cantidad total de productos y Cantidad total de productos del activo
  Tenencia[, TOTAL_PRODUCTOS := rowSums(Tenencia2)]
  Productos_activo <- paste0("CANT_", c("TARJ_CREDITO", "CREDISERVICE", "ORDINARIO", "LIBRANZA", "LIBRE_DESTINO", "VEHICULO", "LEASING", "VIVIENDA", "MICROCREDITO"))
  Tenencia3 <- Tenencia[, ..Productos_activo]
  Tenencia[, TOTAL_PRODUCTOS_ACTIVO := rowSums(Tenencia3)]
  
  Tenencia[, CANT_CTAS_AHORRO := CANT_CTAS_AHORRO + CANT_CTAS_NOMINA]
  
  Tenencia <- Tenencia[, .(TIPO_IDENTIFICACION, NUMERO_IDENTIFICACION, TOTAL_PRODUCTOS, TOTAL_PRODUCTOS_ACTIVO, CANT_TARJ_CREDITO, CANT_CTAS_AHORRO)]
  names(Tenencia) <- c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION", "TOTAL_PRODUCTOS_menos1", "TOTAL_PRODUCTOS_ACTIVO_menos1", "CANT_TARJ_CREDITO_menos1", "CANT_CTAS_AHORRO_menos1")
  
  #Pegarle a la data la informacion de Tenencia
  Tenencia$NUMERO_IDENTIFICACION <- as.numeric(Tenencia$NUMERO_IDENTIFICACION)
  Ahorros <- left_join(Ahorros, Tenencia, by = c("AAHO_TIP_IDEN" = "TIPO_IDENTIFICACION", 
                                                 "AAHO_NUM_IDEN" = "NUMERO_IDENTIFICACION"))
  Ahorros <- data.table(Ahorros)
  
  Ahorros[is.na(TOTAL_PRODUCTOS_menos1) & Antiguedad >= as.numeric(substr(Cierre_mes, 9, 10))/365]$TOTAL_PRODUCTOS_menos1 <- 1
  Ahorros[is.na(TOTAL_PRODUCTOS_menos1) & Antiguedad < as.numeric(substr(Cierre_mes, 9, 10))/365]$TOTAL_PRODUCTOS_menos1 <- 0
  Ahorros[is.na(TOTAL_PRODUCTOS_ACTIVO_menos1)]$TOTAL_PRODUCTOS_ACTIVO_menos1 <- 0
  Ahorros[is.na(CANT_TARJ_CREDITO_menos1)]$CANT_TARJ_CREDITO_menos1 <- 0
  Ahorros[is.na(CANT_CTAS_AHORRO_menos1) & Antiguedad >= as.numeric(substr(Cierre_mes, 9, 10))/365]$CANT_CTAS_AHORRO_menos1 <- 1
  Ahorros[is.na(CANT_CTAS_AHORRO_menos1) & Antiguedad < as.numeric(substr(Cierre_mes, 9, 10))/365]$CANT_CTAS_AHORRO_menos1 <- 0
  
  Ahorros[, TOTAL_PRODUCTOS_diferencia := TOTAL_PRODUCTOS - TOTAL_PRODUCTOS_menos1]
  Ahorros[, TOTAL_PRODUCTOS_ACTIVO_diferencia := TOTAL_PRODUCTOS_ACTIVO - TOTAL_PRODUCTOS_ACTIVO_menos1]
  Ahorros[, CANT_TARJ_CREDITO_diferencia := CANT_TARJ_CREDITO - CANT_TARJ_CREDITO_menos1]
  Ahorros[, CANT_CTAS_AHORRO_diferencia := CANT_CTAS_AHORRO - CANT_CTAS_AHORRO_menos1]

  Ahorros[, TOTAL_PRODUCTOS_num := TOTAL_PRODUCTOS]
  Ahorros[TOTAL_PRODUCTOS >= 5]$TOTAL_PRODUCTOS <- 5
  Ahorros$TOTAL_PRODUCTOS <- as.character(Ahorros$TOTAL_PRODUCTOS)
  Ahorros[TOTAL_PRODUCTOS == "5"]$TOTAL_PRODUCTOS <- "5 o mas"
  Ahorros$TOTAL_PRODUCTOS <- factor(Ahorros$TOTAL_PRODUCTOS, levels = c("1", "2", "3", "4", "5 o mas"))
  
  Ahorros[TOTAL_PRODUCTOS_ACTIVO >= 1]$TOTAL_PRODUCTOS_ACTIVO <- 1
  Ahorros$TOTAL_PRODUCTOS_ACTIVO <- as.character(Ahorros$TOTAL_PRODUCTOS_ACTIVO)
  Ahorros[TOTAL_PRODUCTOS_ACTIVO == "1"]$TOTAL_PRODUCTOS_ACTIVO <- "1 o mas"
  Ahorros$TOTAL_PRODUCTOS_ACTIVO <- factor(Ahorros$TOTAL_PRODUCTOS_ACTIVO, levels = c("0", "1 o mas"))
  
  Ahorros[CANT_TARJ_CREDITO >= 1]$CANT_TARJ_CREDITO <- 1
  Ahorros$CANT_TARJ_CREDITO <- as.character(Ahorros$CANT_TARJ_CREDITO)
  Ahorros[CANT_TARJ_CREDITO == "1"]$CANT_TARJ_CREDITO <- "1 o mas"
  Ahorros$CANT_TARJ_CREDITO <- factor(Ahorros$CANT_TARJ_CREDITO, levels = c("0", "1 o mas"))
  
  Ahorros[CANT_CTAS_AHORRO >= 2]$CANT_CTAS_AHORRO <- 2
  Ahorros$CANT_CTAS_AHORRO <- as.character(Ahorros$CANT_CTAS_AHORRO)
  Ahorros[CANT_CTAS_AHORRO == "2"]$CANT_CTAS_AHORRO <- "2 o mas"
  Ahorros[CANT_CTAS_AHORRO < 2]$CANT_CTAS_AHORRO <- "1 o menos"
  Ahorros$CANT_CTAS_AHORRO <- factor(Ahorros$CANT_CTAS_AHORRO, levels = c("1 o menos", "2 o mas"))
  
  Ahorros[TOTAL_PRODUCTOS_menos1 >= 5]$TOTAL_PRODUCTOS_menos1 <- 5
  Ahorros$TOTAL_PRODUCTOS_menos1 <- as.character(Ahorros$TOTAL_PRODUCTOS_menos1)
  Ahorros[TOTAL_PRODUCTOS_menos1 == "5"]$TOTAL_PRODUCTOS_menos1 <- "5 o mas"
  Ahorros$TOTAL_PRODUCTOS_menos1 <- factor(Ahorros$TOTAL_PRODUCTOS_menos1, levels = c("0", "1", "2", "3", "4", "5 o mas"))
  
  Ahorros[TOTAL_PRODUCTOS_ACTIVO_menos1 >= 1]$TOTAL_PRODUCTOS_ACTIVO_menos1 <- 1
  Ahorros$TOTAL_PRODUCTOS_ACTIVO_menos1 <- as.character(Ahorros$TOTAL_PRODUCTOS_ACTIVO_menos1)
  Ahorros[TOTAL_PRODUCTOS_ACTIVO_menos1 == "1"]$TOTAL_PRODUCTOS_ACTIVO_menos1 <- "1 o mas"
  Ahorros$TOTAL_PRODUCTOS_ACTIVO_menos1 <- factor(Ahorros$TOTAL_PRODUCTOS_ACTIVO_menos1, levels = c("0", "1 o mas"))

  Ahorros[CANT_TARJ_CREDITO_menos1 >= 1]$CANT_TARJ_CREDITO_menos1 <- 1
  Ahorros$CANT_TARJ_CREDITO_menos1 <- as.character(Ahorros$CANT_TARJ_CREDITO_menos1)
  Ahorros[CANT_TARJ_CREDITO_menos1 == "1"]$CANT_TARJ_CREDITO_menos1 <- "1 o mas"
  Ahorros$CANT_TARJ_CREDITO_menos1 <- factor(Ahorros$CANT_TARJ_CREDITO_menos1, levels = c("0", "1 o mas"))
  
  Ahorros[CANT_CTAS_AHORRO_menos1 >= 2]$CANT_CTAS_AHORRO_menos1 <- 2
  Ahorros$CANT_CTAS_AHORRO_menos1 <- as.character(Ahorros$CANT_CTAS_AHORRO_menos1)
  Ahorros[CANT_CTAS_AHORRO_menos1 == "2"]$CANT_CTAS_AHORRO_menos1 <- "2 o mas"
  Ahorros[CANT_CTAS_AHORRO_menos1 < 2]$CANT_CTAS_AHORRO_menos1 <- "1 o menos"
  Ahorros$CANT_CTAS_AHORRO_menos1 <- factor(Ahorros$CANT_CTAS_AHORRO_menos1, levels = c("1 o menos", "2 o mas"))
  
  
  Ahorros[TOTAL_PRODUCTOS_diferencia >= 1]$TOTAL_PRODUCTOS_diferencia <- 1
  Ahorros[TOTAL_PRODUCTOS_diferencia <= -1]$TOTAL_PRODUCTOS_diferencia <- -1
  Ahorros$TOTAL_PRODUCTOS_diferencia <- as.character(Ahorros$TOTAL_PRODUCTOS_diferencia)
  Ahorros[TOTAL_PRODUCTOS_diferencia == "1"]$TOTAL_PRODUCTOS_diferencia <- "1 o mas"
  Ahorros[TOTAL_PRODUCTOS_diferencia == "-1"]$TOTAL_PRODUCTOS_diferencia <- "-1 o menos"
  Ahorros$TOTAL_PRODUCTOS_diferencia <- factor(Ahorros$TOTAL_PRODUCTOS_diferencia, levels = c("-1 o menos", "0", "1 o mas"))
  
  Ahorros[TOTAL_PRODUCTOS_ACTIVO_diferencia >= 1]$TOTAL_PRODUCTOS_ACTIVO_diferencia <- 1
  Ahorros[TOTAL_PRODUCTOS_ACTIVO_diferencia <= -1]$TOTAL_PRODUCTOS_ACTIVO_diferencia <- -1
  Ahorros$TOTAL_PRODUCTOS_ACTIVO_diferencia <- as.character(Ahorros$TOTAL_PRODUCTOS_ACTIVO_diferencia)
  Ahorros[TOTAL_PRODUCTOS_ACTIVO_diferencia == "1"]$TOTAL_PRODUCTOS_ACTIVO_diferencia <- "1 o mas"
  Ahorros[TOTAL_PRODUCTOS_ACTIVO_diferencia == "-1"]$TOTAL_PRODUCTOS_ACTIVO_diferencia <- "-1 o menos"
  Ahorros$TOTAL_PRODUCTOS_ACTIVO_diferencia <- factor(Ahorros$TOTAL_PRODUCTOS_ACTIVO_diferencia, levels = c("-1 o menos", "0", "1 o mas"))
  
  Ahorros[CANT_TARJ_CREDITO_diferencia >= 1]$CANT_TARJ_CREDITO_diferencia <- 1
  Ahorros[CANT_TARJ_CREDITO_diferencia <= -1]$CANT_TARJ_CREDITO_diferencia <- -1
  Ahorros$CANT_TARJ_CREDITO_diferencia <- as.character(Ahorros$CANT_TARJ_CREDITO_diferencia)
  Ahorros[CANT_TARJ_CREDITO_diferencia == "1"]$CANT_TARJ_CREDITO_diferencia <- "1 o mas"
  Ahorros[CANT_TARJ_CREDITO_diferencia == "-1"]$CANT_TARJ_CREDITO_diferencia <- "-1 o menos"
  Ahorros$CANT_TARJ_CREDITO_diferencia <- factor(Ahorros$CANT_TARJ_CREDITO_diferencia, levels = c("-1 o menos", "0", "1 o mas"))
  
  Ahorros[CANT_CTAS_AHORRO_diferencia >= 1]$CANT_CTAS_AHORRO_diferencia <- 1
  Ahorros[CANT_CTAS_AHORRO_diferencia <= -1]$CANT_CTAS_AHORRO_diferencia <- -1
  Ahorros$CANT_CTAS_AHORRO_diferencia <- as.character(Ahorros$CANT_CTAS_AHORRO_diferencia)
  Ahorros[CANT_CTAS_AHORRO_diferencia == "1"]$CANT_CTAS_AHORRO_diferencia <- "1 o mas" 
  Ahorros[CANT_CTAS_AHORRO_diferencia == "-1"]$CANT_CTAS_AHORRO_diferencia <- "-1 o menos" 
  Ahorros$CANT_CTAS_AHORRO_diferencia <- factor(Ahorros$CANT_CTAS_AHORRO_diferencia, levels = c("-1 o menos", "0", "1 o mas"))
  
  
  #############################################################################################################################
  
  #Traer tenencia de los ultimos 5 meses y sacar dos variables
  #1 - Tenencia actual sobre minimo de tenencia de los ultimos 5 meses
  #2 - Tenencia de Cuentas Ahorros dos periodos atras
  
  Tenencia_menos1 <- fread(file = paste0("Z:/Informacion_Centralizada/Data_Analytics/Tenencia_Productos/TENENCIA_", Mes_atras_n(Mes, 1), ".csv"), integer64 = "character", showProgress = F)
  Tenencia_menos2 <- fread(file = paste0("Z:/Informacion_Centralizada/Data_Analytics/Tenencia_Productos/TENENCIA_", Mes_atras_n(Mes, 2), ".csv"), integer64 = "character", showProgress = F)
  Tenencia_menos3 <- fread(file = paste0("Z:/Informacion_Centralizada/Data_Analytics/Tenencia_Productos/TENENCIA_", Mes_atras_n(Mes, 3), ".csv"), integer64 = "character", showProgress = F)
  Tenencia_menos4 <- fread(file = paste0("Z:/Informacion_Centralizada/Data_Analytics/Tenencia_Productos/TENENCIA_", Mes_atras_n(Mes, 4), ".csv"), integer64 = "character", showProgress = F)
  Tenencia_menos5 <- fread(file = paste0("Z:/Informacion_Centralizada/Data_Analytics/Tenencia_Productos/TENENCIA_", Mes_atras_n(Mes, 5), ".csv"), integer64 = "character", showProgress = F)
  
  #Cambiar los NA por 0
  Cantidades <- grep("CANT_", names(Tenencia_menos1), value = T)  ##Sacar el VALOR de las variables que empiecen con "cant_"  
  
  Tenencia2_menos1 <- Tenencia_menos1[, ..Cantidades]
  Tenencia2_menos1[is.na(Tenencia2_menos1)] <- 0
  Tenencia_menos1 <- cbind(Tenencia_menos1$TIPO_IDENTIFICACION, Tenencia_menos1$NUMERO_IDENTIFICACION, Tenencia2_menos1)
  names(Tenencia_menos1)[1:2] <- c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION")
  Tenencia2_menos2 <- Tenencia_menos2[, ..Cantidades]
  Tenencia2_menos2[is.na(Tenencia2_menos2)] <- 0
  Tenencia_menos2 <- cbind(Tenencia_menos2$TIPO_IDENTIFICACION, Tenencia_menos2$NUMERO_IDENTIFICACION, Tenencia2_menos2)
  names(Tenencia_menos2)[1:2] <- c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION")
  Tenencia2_menos3 <- Tenencia_menos3[, ..Cantidades]
  Tenencia2_menos3[is.na(Tenencia2_menos3)] <- 0
  Tenencia_menos3 <- cbind(Tenencia_menos3$TIPO_IDENTIFICACION, Tenencia_menos3$NUMERO_IDENTIFICACION, Tenencia2_menos3)
  names(Tenencia_menos3)[1:2] <- c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION")
  Tenencia2_menos4 <- Tenencia_menos4[, ..Cantidades]
  Tenencia2_menos4[is.na(Tenencia2_menos4)] <- 0
  Tenencia_menos4 <- cbind(Tenencia_menos4$TIPO_IDENTIFICACION, Tenencia_menos4$NUMERO_IDENTIFICACION, Tenencia2_menos4)
  names(Tenencia_menos4)[1:2] <- c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION")
  Tenencia2_menos5 <- Tenencia_menos5[, ..Cantidades]
  Tenencia2_menos5[is.na(Tenencia2_menos5)] <- 0
  Tenencia_menos5 <- cbind(Tenencia_menos5$TIPO_IDENTIFICACION, Tenencia_menos5$NUMERO_IDENTIFICACION, Tenencia2_menos5)
  names(Tenencia_menos5)[1:2] <- c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION")
  
  Tenencia_menos1[, TOTAL_PRODUCTOS := rowSums(Tenencia_menos1[, mget(Cantidades)])]
  Tenencia_menos2[, TOTAL_PRODUCTOS := rowSums(Tenencia_menos2[, mget(Cantidades)])]
  Tenencia_menos3[, TOTAL_PRODUCTOS := rowSums(Tenencia_menos3[, mget(Cantidades)])]
  Tenencia_menos4[, TOTAL_PRODUCTOS := rowSums(Tenencia_menos4[, mget(Cantidades)])]
  Tenencia_menos5[, TOTAL_PRODUCTOS := rowSums(Tenencia_menos5[, mget(Cantidades)])]
  Tenencia_menos2[, CANT_CTAS_AHORRO := CANT_CTAS_AHORRO + CANT_CTAS_NOMINA]
  
  Tenencia_menos1 <- Tenencia_menos1[, .(TIPO_IDENTIFICACION, NUMERO_IDENTIFICACION, TOTAL_PRODUCTOS)]
  Tenencia_menos2 <- Tenencia_menos2[, .(TIPO_IDENTIFICACION, NUMERO_IDENTIFICACION, TOTAL_PRODUCTOS, CANT_CTAS_AHORRO)]
  Tenencia_menos3 <- Tenencia_menos3[, .(TIPO_IDENTIFICACION, NUMERO_IDENTIFICACION, TOTAL_PRODUCTOS)]
  Tenencia_menos4 <- Tenencia_menos4[, .(TIPO_IDENTIFICACION, NUMERO_IDENTIFICACION, TOTAL_PRODUCTOS)]
  Tenencia_menos5 <- Tenencia_menos5[, .(TIPO_IDENTIFICACION, NUMERO_IDENTIFICACION, TOTAL_PRODUCTOS)]
  
  names(Tenencia_menos1) <- c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION", "TOTAL_PRODUCTOS_menos1")
  names(Tenencia_menos2) <- c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION", "TOTAL_PRODUCTOS_menos2", "CANT_CTAS_AHORRO_menos2")
  names(Tenencia_menos3) <- c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION", "TOTAL_PRODUCTOS_menos3")
  names(Tenencia_menos4) <- c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION", "TOTAL_PRODUCTOS_menos4")
  names(Tenencia_menos5) <- c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION", "TOTAL_PRODUCTOS_menos5")
  
  Tenencia_rezagada <- merge(Tenencia_menos1, Tenencia_menos2,
                             by.x = c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION"),
                             by.y = c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION"),
                             all.x = T, all.y = T)
  Tenencia_rezagada <- merge(Tenencia_rezagada, Tenencia_menos3,
                             by.x = c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION"),
                             by.y = c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION"),
                             all.x = T, all.y = T)
  Tenencia_rezagada <- merge(Tenencia_rezagada, Tenencia_menos4,
                             by.x = c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION"),
                             by.y = c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION"),
                             all.x = T, all.y = T)
  Tenencia_rezagada <- merge(Tenencia_rezagada, Tenencia_menos5,
                             by.x = c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION"),
                             by.y = c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION"),
                             all.x = T, all.y = T)
  
  
  Tenencia_a_pegar <- Tenencia_rezagada[, .(MINIMO_TENENCIA_ULTIMOS5 = pmin(TOTAL_PRODUCTOS_menos1, TOTAL_PRODUCTOS_menos2, TOTAL_PRODUCTOS_menos3, TOTAL_PRODUCTOS_menos4, TOTAL_PRODUCTOS_menos5, na.rm = T)), by = .(TIPO_IDENTIFICACION, NUMERO_IDENTIFICACION)]
  Tenencia_menos2 <-  Tenencia_menos2[, .(TIPO_IDENTIFICACION, NUMERO_IDENTIFICACION, CANT_CTAS_AHORRO_menos2)]
  Tenencia_a_pegar <- merge(Tenencia_a_pegar, Tenencia_menos2, 
                            by.x = c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION"),
                            by.y = c("TIPO_IDENTIFICACION", "NUMERO_IDENTIFICACION"),
                            all.x = T, all.y = F)
  
  #Pegarle a la data la informacion 
  Tenencia_a_pegar$NUMERO_IDENTIFICACION <- as.numeric(Tenencia_a_pegar$NUMERO_IDENTIFICACION)
  Ahorros <- left_join(Ahorros, Tenencia_a_pegar, by = c("AAHO_TIP_IDEN" = "TIPO_IDENTIFICACION", 
                                                         "AAHO_NUM_IDEN" = "NUMERO_IDENTIFICACION"))
  Ahorros <- data.table(Ahorros)
  
  Ahorros[(MINIMO_TENENCIA_ULTIMOS5 == 0 | is.na(MINIMO_TENENCIA_ULTIMOS5)) & Antiguedad < 150/365, MINIMO_TENENCIA_ULTIMOS5 := 0]
  Ahorros[(MINIMO_TENENCIA_ULTIMOS5 == 0 | is.na(MINIMO_TENENCIA_ULTIMOS5)) & Antiguedad > 150/365, MINIMO_TENENCIA_ULTIMOS5 := 1]

  Ahorros[(CANT_CTAS_AHORRO_menos2 == 0 | is.na(CANT_CTAS_AHORRO_menos2)) & Antiguedad < 60/365, CANT_CTAS_AHORRO_menos2 := 0]
  Ahorros[(CANT_CTAS_AHORRO_menos2 == 0 | is.na(CANT_CTAS_AHORRO_menos2)) & Antiguedad > 60/365, CANT_CTAS_AHORRO_menos2 := 1]  
    
  Ahorros[, tenencia_menos_min_tenencia5 := TOTAL_PRODUCTOS_num - MINIMO_TENENCIA_ULTIMOS5]
  
  Ahorros[tenencia_menos_min_tenencia5 > 0, tenencia_menos_min_tenencia5 := 1]
  Ahorros[tenencia_menos_min_tenencia5 < 0, tenencia_menos_min_tenencia5 := -1]
  
  Ahorros$tenencia_menos_min_tenencia5 <- factor(Ahorros$tenencia_menos_min_tenencia5, levels = c(-1, 0, 1))
  
  return(Ahorros)
}


#' Creacion de las variables de CRM
#'
#' @param Ahorros La tabla principal.
#' @param Mes El mes de corte. Debe estar en formato "YYYY_MM".
#'
#' @return La tabla con las variables de Tenencia, incorporadas.
#'
#' @examples
#' Ahorros <- Variables_CRM(Ahorros = Ahorros, Mes = Mes)
#' 
Variables_CRM <- function(Ahorros, Mes) {
  
  #No hay informacion previa a noviembre de 2018, por lo que si el mes de corte es anterior a esto, pegarle la informacion de 2018_11
  if (as.numeric(substr(Mes, 1, 4)) >= 2019 | (as.numeric(substr(Mes, 1, 4)) == 2018 & as.numeric(substr(Mes, 6, 7)) >= 11)) {
    suppressWarnings(CRM <- fread(file = paste0("Z:/Informacion_Centralizada/Data_Analytics/CRM_Persona_Natural/CRM_PN_", Mes, ".csv"), integer64 = "character", showProgress = F))
  } else {
    suppressWarnings(CRM <- fread(file = "Z:/Informacion_Centralizada/Data_Analytics/CRM_Persona_Natural/CRM_PN_2018_11.csv", integer64 = "character", showProgress = F))
  }
  
  CRM <- CRM %>% dplyr::select(CRM_NUMERO_IDENTIFICACION,
                               CRM_TIPO_IDENTIFICACION,
                               CRM_FECHA_NACIMIENTO,
                               CRM_GENERO,
                               CRM_ESTRATO,
                               CRM_GRUPO_SEGMENTO) #escoger solo las variables que nos interesan
  
  CRM$CRM_NUMERO_IDENTIFICACION <- as.numeric(CRM$CRM_NUMERO_IDENTIFICACION)
  
  CRM <- CRM[!duplicated(CRM[, c("CRM_NUMERO_IDENTIFICACION", "CRM_TIPO_IDENTIFICACION")]), ] #Quitar duplicados de numero y tipo de identificacion
  
  #Arreglar formato fecha para 2018_11 o 2018_12
  if (as.numeric(substr(Mes, 1, 4)) <= 2018) {
    CRM$CRM_FECHA_NACIMIENTO <- as.Date(CRM$CRM_FECHA_NACIMIENTO, "%d/%m/%Y")
  } else {
    CRM$CRM_FECHA_NACIMIENTO <- as.Date(CRM$CRM_FECHA_NACIMIENTO)
  }
  
  Ahorros$AAHO_NUM_IDEN <- as.numeric(Ahorros$AAHO_NUM_IDEN)
  Ahorros <- left_join(Ahorros, CRM, 
                       by = c("AAHO_NUM_IDEN" = "CRM_NUMERO_IDENTIFICACION",
                              "AAHO_TIP_IDEN" = "CRM_TIPO_IDENTIFICACION"))
  Ahorros <- data.table(Ahorros)
  
  #Cuantos missing hay despues de cruzar?
  if (sum(is.na(Ahorros$CRM_FECHA_NACIMIENTO))/nrow(Ahorros) > 0.05) { stop(paste0("Revise el cruce con CRM. Existen bastantes NAs. Tenga en cuenta que Mes vale ", Mes))}
  
  #Creacion de variable edad
  ###Ahorros <- Arreglo_fecha(Data = Ahorros, Variable = "CRM_FECHA_NACIMIENTO")
  Ahorros[, Edad := as.numeric(Cierre_mes - CRM_FECHA_NACIMIENTO)/365] #en anos

  #Imputar variable edad para los que no cruzaron (entonces en genero saldra NA)
  Ahorros[is.na(CRM_GENERO)]$Edad <- median(Ahorros$Edad, na.rm = T)
    
  if (sum(Ahorros$CRM_GENERO %in% c("F", "M")) > 0.95*nrow(Ahorros)) {
    Ahorros[CRM_GENERO %!in% c("F", "M")]$CRM_GENERO <- "Vacio"
  } else {
    stop(paste0("Revise CRM: hay muchos missing en genero. Tenga en cuenta que Mes vale ", Mes))
  }
  
  if (sum(Ahorros$CRM_ESTRATO %in% c(0, 1, 2, 3, 4, 5, 6)) > 0.95*nrow(Ahorros)) {
    Ahorros[CRM_ESTRATO %!in% c(0, 1, 2, 3, 4, 5, 6)]$CRM_ESTRATO <- 0
  } else {
    stop(paste0("Revise CRM: Hay muchos missing en estrato. Tenga en cuenta que Mes vale ", Mes))
  }
  
  if (sum(is.na(Ahorros$CRM_GRUPO_SEGMENTO) | Ahorros$CRM_GRUPO_SEGMENTO == "") <= 0.05*nrow(Ahorros)) {
    Ahorros[CRM_GRUPO_SEGMENTO == "" | is.na(CRM_GRUPO_SEGMENTO)]$CRM_GRUPO_SEGMENTO <- "Vacio"
  } else {
    stop(paste0("Revise CRM: Hay muchos missing en segmento. Tenga en cuenta que Mes vale ", Mes))
  }
  
  #poner en categorica las demas variables de CRM
  Ahorros$CRM_GENERO <- factor(Ahorros$CRM_GENERO)
  Ahorros$CRM_ESTRATO <- factor(Ahorros$CRM_ESTRATO)
  Ahorros$CRM_GRUPO_SEGMENTO <- factor(Ahorros$CRM_GRUPO_SEGMENTO)
  
  #Agrupar segmentos
  Diccionario_segmentos <- fread(file = paste0(getwd(), "/Diccionario/Dicc_gruposegmento.csv"))
  suppressWarnings(Ahorros <- left_join(Ahorros, Diccionario_segmentos, by = "CRM_GRUPO_SEGMENTO"))
  Ahorros <- data.table(Ahorros)
  Ahorros$GRUPO_SEGMENTO <- factor(Ahorros$GRUPO_SEGMENTO)
  Ahorros$CRM_GRUPO_SEGMENTO <- factor(Ahorros$CRM_GRUPO_SEGMENTO)
  
  return(Ahorros)
}



#' Creacion de las variables de saldos rezagadas e inactividad rezagadas
#'
#' @param Ahorros La tabla principal.
#' @param Mes El mes de corte. Debe estar en formato "YYYY_MM".
#'
#' @return La tabla con las variables de saldos e inactividad rezagadas, incorporadas.
#'
#' @examples
#' Ahorros <- Variables_ahorros_rezagados(Ahorros = Ahorros, Mes = Mes)
#' 
Variables_ahorros_rezagados <- function(Ahorros, Mes) {
  
  #Cargue
  suppressWarnings(Ahorros_menos1 <- fread(file = paste0(getwd(), "/Data/Original/Fin_mes/aho_fin_mes_", Mes_atras_n(Mes, 1),".csv"), integer64 = "character", showProgress = F))
  suppressWarnings(Ahorros_menos2 <- fread(file = paste0(getwd(), "/Data/Original/Fin_mes/aho_fin_mes_", Mes_atras_n(Mes, 2),".csv"), integer64 = "character", showProgress = F))
  suppressWarnings(Ahorros_menos3 <- fread(file = paste0(getwd(), "/Data/Original/Fin_mes/aho_fin_mes_", Mes_atras_n(Mes, 3),".csv"), integer64 = "character", showProgress = F))
  suppressWarnings(Ahorros_menos4 <- fread(file = paste0(getwd(), "/Data/Original/Fin_mes/aho_fin_mes_", Mes_atras_n(Mes, 4),".csv"), integer64 = "character", showProgress = F))
  suppressWarnings(Ahorros_menos5 <- fread(file = paste0(getwd(), "/Data/Original/Fin_mes/aho_fin_mes_", Mes_atras_n(Mes, 5),".csv"), integer64 = "character", showProgress = F))
  
  #Solo variables que nos interesan
  variables <- c("AAHO_FEC_APERTURA", "AAHO_ESTADO", "AAHO_NUM_CUENTA", "AAHO_PRO_MES", "AAHO_SAL_DISPONIB", "AAHO_SAL_HOY", "AAHO_SAL_MINIMO", "AAHO_FEC_ULT_MOV")
  Ahorros_menos1 <- Ahorros_menos1[, ..variables]
  Ahorros_menos2 <- Ahorros_menos2[, ..variables]
  Ahorros_menos3 <- Ahorros_menos3[, ..variables]
  Ahorros_menos4 <- Ahorros_menos4[, ..variables]
  Ahorros_menos5 <- Ahorros_menos5[, ..variables]
  
  if (modelo %in% c("modelo_2", "modelo_4")){
  Ahorros_menos1 <- Variables_antiguedad(Ahorros = Ahorros_menos1, Mes = Mes_atras_n(Mes, 1))
  Ahorros_menos1 <- Variables_dias_ult_mov(Ahorros = Ahorros_menos1, Mes = Mes_atras_n(Mes, 1))
  Ahorros_menos2 <- Variables_antiguedad(Ahorros = Ahorros_menos2, Mes = Mes_atras_n(Mes, 2))
  Ahorros_menos2 <- Variables_dias_ult_mov(Ahorros = Ahorros_menos2, Mes = Mes_atras_n(Mes, 2))
  Ahorros_menos3 <- Variables_antiguedad(Ahorros = Ahorros_menos3, Mes = Mes_atras_n(Mes, 3))
  Ahorros_menos3 <- Variables_dias_ult_mov(Ahorros = Ahorros_menos3, Mes = Mes_atras_n(Mes, 3))
  Ahorros_menos4 <- Variables_antiguedad(Ahorros = Ahorros_menos4, Mes = Mes_atras_n(Mes, 4))
  Ahorros_menos4 <- Variables_dias_ult_mov(Ahorros = Ahorros_menos4, Mes = Mes_atras_n(Mes, 4))
  Ahorros_menos5 <- Variables_antiguedad(Ahorros = Ahorros_menos5, Mes = Mes_atras_n(Mes, 5))
  Ahorros_menos5 <- Variables_dias_ult_mov(Ahorros = Ahorros_menos5, Mes = Mes_atras_n(Mes, 5))
  }
  
  if (modelo %in% c("modelo_2", "modelo_4")){
    variables <- c("AAHO_NUM_CUENTA", "AAHO_PRO_MES", "AAHO_SAL_DISPONIB", "AAHO_SAL_HOY", "AAHO_SAL_MINIMO", "Dias_ult_mov")
  } else {
    variables <- c("AAHO_NUM_CUENTA", "AAHO_PRO_MES", "AAHO_SAL_DISPONIB", "AAHO_SAL_HOY", "AAHO_SAL_MINIMO")
  }
  Ahorros_menos1 <- Ahorros_menos1[, ..variables]
  Ahorros_menos2 <- Ahorros_menos2[, ..variables]
  Ahorros_menos3 <- Ahorros_menos3[, ..variables]
  Ahorros_menos4 <- Ahorros_menos4[, ..variables]
  Ahorros_menos5 <- Ahorros_menos5[, ..variables]
  
  Ahorros_menos1$AAHO_NUM_CUENTA <- as.numeric(Ahorros_menos1$AAHO_NUM_CUENTA)
  Ahorros_menos2$AAHO_NUM_CUENTA <- as.numeric(Ahorros_menos2$AAHO_NUM_CUENTA)
  Ahorros_menos3$AAHO_NUM_CUENTA <- as.numeric(Ahorros_menos3$AAHO_NUM_CUENTA)
  Ahorros_menos4$AAHO_NUM_CUENTA <- as.numeric(Ahorros_menos4$AAHO_NUM_CUENTA)
  Ahorros_menos5$AAHO_NUM_CUENTA <- as.numeric(Ahorros_menos5$AAHO_NUM_CUENTA)
  
  if (modelo %in% c("modelo_2", "modelo_4")) {
    names(Ahorros_menos1) <- c("AAHO_NUM_CUENTA", "AAHO_PRO_MES_menos1", "AAHO_SAL_DISPONIB_menos1", "AAHO_SAL_HOY_menos1", "AAHO_SAL_MINIMO_menos1", "Dias_ult_mov_menos1")
    names(Ahorros_menos2) <- c("AAHO_NUM_CUENTA", "AAHO_PRO_MES_menos2", "AAHO_SAL_DISPONIB_menos2", "AAHO_SAL_HOY_menos2", "AAHO_SAL_MINIMO_menos2", "Dias_ult_mov_menos2")
    names(Ahorros_menos3) <- c("AAHO_NUM_CUENTA", "AAHO_PRO_MES_menos3", "AAHO_SAL_DISPONIB_menos3", "AAHO_SAL_HOY_menos3", "AAHO_SAL_MINIMO_menos3", "Dias_ult_mov_menos3")
    names(Ahorros_menos4) <- c("AAHO_NUM_CUENTA", "AAHO_PRO_MES_menos4", "AAHO_SAL_DISPONIB_menos4", "AAHO_SAL_HOY_menos4", "AAHO_SAL_MINIMO_menos4", "Dias_ult_mov_menos4")
    names(Ahorros_menos5) <- c("AAHO_NUM_CUENTA", "AAHO_PRO_MES_menos5", "AAHO_SAL_DISPONIB_menos5", "AAHO_SAL_HOY_menos5", "AAHO_SAL_MINIMO_menos5", "Dias_ult_mov_menos5")
  } else {
    names(Ahorros_menos1) <- c("AAHO_NUM_CUENTA", "AAHO_PRO_MES_menos1", "AAHO_SAL_DISPONIB_menos1", "AAHO_SAL_HOY_menos1", "AAHO_SAL_MINIMO_menos1")
    names(Ahorros_menos2) <- c("AAHO_NUM_CUENTA", "AAHO_PRO_MES_menos2", "AAHO_SAL_DISPONIB_menos2", "AAHO_SAL_HOY_menos2", "AAHO_SAL_MINIMO_menos2")
    names(Ahorros_menos3) <- c("AAHO_NUM_CUENTA", "AAHO_PRO_MES_menos3", "AAHO_SAL_DISPONIB_menos3", "AAHO_SAL_HOY_menos3", "AAHO_SAL_MINIMO_menos3")
    names(Ahorros_menos4) <- c("AAHO_NUM_CUENTA", "AAHO_PRO_MES_menos4", "AAHO_SAL_DISPONIB_menos4", "AAHO_SAL_HOY_menos4", "AAHO_SAL_MINIMO_menos4")
    names(Ahorros_menos5) <- c("AAHO_NUM_CUENTA", "AAHO_PRO_MES_menos5", "AAHO_SAL_DISPONIB_menos5", "AAHO_SAL_HOY_menos5", "AAHO_SAL_MINIMO_menos5")
  }
  
  #Validacion
  if(length(unique(Ahorros_menos1$AAHO_NUM_CUENTA)) != nrow(Ahorros_menos1) | 
     length(unique(Ahorros_menos2$AAHO_NUM_CUENTA)) != nrow(Ahorros_menos2) | 
     length(unique(Ahorros_menos3$AAHO_NUM_CUENTA)) != nrow(Ahorros_menos3) |
     length(unique(Ahorros_menos4$AAHO_NUM_CUENTA)) != nrow(Ahorros_menos4) |
     length(unique(Ahorros_menos5$AAHO_NUM_CUENTA)) != nrow(Ahorros_menos5)) { stop(paste0("Hay registros duplicados en tablas de Ahorros pasadas. Tenga en cuenta que Mes vale ", Mes))}
  
  #Merge
  Ahorros <- left_join(Ahorros, Ahorros_menos1, by = "AAHO_NUM_CUENTA")
  Ahorros <- left_join(Ahorros, Ahorros_menos2, by = "AAHO_NUM_CUENTA")
  Ahorros <- left_join(Ahorros, Ahorros_menos3, by = "AAHO_NUM_CUENTA")
  Ahorros <- left_join(Ahorros, Ahorros_menos4, by = "AAHO_NUM_CUENTA")
  Ahorros <- left_join(Ahorros, Ahorros_menos5, by = "AAHO_NUM_CUENTA")
  
  Ahorros <- data.table(Ahorros)
  
  return(Ahorros)
}



#' Creacion de otras variables de saldos
#'
#' @param Ahorros La tabla principal.
#'
#' @return La tabla con las variables de saldos actuales en relacion a los pasados, incorporadas.
#'
#' @examples
#' Ahorros <- Variables_saldos_adicionales(Ahorros = Ahorros)
#' 
Variables_saldos_adicionales <- function(Ahorros) {
  
  #Imputar datos de saldos faltantes -> SOLO PARA EL SALDO DEL MISMO MES. Se espera que no haya. Si es menos del 0.5%, seguir.
  if(sum(is.na(Ahorros[, AAHO_PRO_MES] | Ahorros[, AAHO_PRO_MES] == "")/nrow(Ahorros)) > 0.005) {
    stop(paste0("Hay muchos missing en AAHO_PRO_MES. Tenga en cuenta que mes vale "), Mes, ".")
  } else {
    Ahorros[, AAHO_PRO_MES := replace(AAHO_PRO_MES, is.na(AAHO_PRO_MES), median(AAHO_PRO_MES, na.rm = T))]
  }
  if(sum(is.na(Ahorros[, AAHO_SAL_DISPONIB] | Ahorros[, AAHO_SAL_DISPONIB] == "")/nrow(Ahorros)) > 0.005) {
    stop(paste0("Hay muchos missing en AAHO_SAL_DISPONIB. Tenga en cuenta que mes vale "), Mes, ".")
  } else {
    Ahorros[, AAHO_SAL_DISPONIB := replace(AAHO_SAL_DISPONIB, is.na(AAHO_SAL_DISPONIB), median(AAHO_SAL_DISPONIB, na.rm = T))]
  }
  if(sum(is.na(Ahorros[, AAHO_SAL_HOY] | Ahorros[, AAHO_SAL_HOY] == "")/nrow(Ahorros)) > 0.005) {
    stop(paste0("Hay muchos missing en AAHO_SAL_HOY. Tenga en cuenta que mes vale "), Mes, ".")
  } else {
    Ahorros[, AAHO_SAL_HOY := replace(AAHO_SAL_HOY, is.na(AAHO_SAL_HOY), median(AAHO_SAL_HOY, na.rm = T))]
  }
  if(sum(is.na(Ahorros[, AAHO_SAL_MINIMO] | Ahorros[, AAHO_SAL_MINIMO] == "")/nrow(Ahorros)) > 0.005) {
    stop(paste0("Hay muchos missing en AAHO_SAL_MINIMO. Tenga en cuenta que mes vale "), Mes, ".")
  } else {
    Ahorros[, AAHO_SAL_MINIMO := replace(AAHO_SAL_MINIMO, is.na(AAHO_SAL_MINIMO), median(AAHO_SAL_MINIMO, na.rm = T))]
  }
  
  ###############################################################################################################################

  #Crear variable de (promedio del mes_-1, mes_-2, mes_-3) de saldos. Para minimo tambien se crea sino minimo
  Ahorros[, AAHO_PRO_MES_prom_pasado3 := rowMeans(cbind(AAHO_PRO_MES_menos3, AAHO_PRO_MES_menos2, AAHO_PRO_MES_menos1), na.rm = T)]
  Ahorros[, AAHO_SAL_DISPONIB_prom_pasado3 := rowMeans(cbind(AAHO_SAL_DISPONIB_menos3, AAHO_SAL_DISPONIB_menos2, AAHO_SAL_DISPONIB_menos1), na.rm = T)]
  Ahorros[, AAHO_SAL_HOY_prom_pasado3 := rowMeans(cbind(AAHO_SAL_HOY_menos3, AAHO_SAL_HOY_menos2, AAHO_SAL_HOY_menos1), na.rm = T)]
  Ahorros[, AAHO_SAL_MINIMO_prom_pasado3 := rowMeans(cbind(AAHO_SAL_MINIMO_menos3, AAHO_SAL_MINIMO_menos2, AAHO_SAL_MINIMO_menos1), na.rm = T)]
  
  Ahorros[, AAHO_SAL_MINIMO_min_pasado3 := pmin(AAHO_SAL_MINIMO_menos3, AAHO_SAL_MINIMO_menos2, AAHO_SAL_MINIMO_menos1, na.rm = T)]
  Ahorros[is.na(AAHO_SAL_MINIMO_min_pasado3), AAHO_SAL_MINIMO_min_pasado3 := 0]
  
  #Crear variable de maximo saldo en el ultimo trimestre
  Ahorros[, AAHO_PRO_MES_max_pasado3 := pmax(AAHO_PRO_MES_menos3, AAHO_PRO_MES_menos2, AAHO_PRO_MES_menos1, na.rm = T)]
  Ahorros[, AAHO_SAL_DISPONIB_max_pasado3 := pmax(AAHO_SAL_DISPONIB_menos3, AAHO_SAL_DISPONIB_menos2, AAHO_SAL_DISPONIB_menos1, na.rm = T)]
  Ahorros[, AAHO_SAL_HOY_max_pasado3 := pmax(AAHO_SAL_HOY_menos3, AAHO_SAL_HOY_menos2, AAHO_SAL_HOY_menos1, na.rm = T)]
  Ahorros[, AAHO_SAL_MINIMO_max_pasado3 := pmax(AAHO_SAL_MINIMO_menos3, AAHO_SAL_MINIMO_menos2, AAHO_SAL_MINIMO_menos1, na.rm = T)]
  
  #Si no hay informacion en los ultimos 3 meses, ponerle el actual. Nota: se hizo validacion de que estos NAs son los que tienen Antiguedad < 31/365
  Ahorros[is.na(AAHO_PRO_MES_max_pasado3), AAHO_PRO_MES_max_pasado3 := AAHO_PRO_MES]
  Ahorros[is.na(AAHO_SAL_DISPONIB_max_pasado3), AAHO_SAL_DISPONIB_max_pasado3 := AAHO_SAL_DISPONIB]
  Ahorros[is.na(AAHO_SAL_HOY_max_pasado3), AAHO_SAL_HOY_max_pasado3 := AAHO_SAL_HOY]
  Ahorros[is.na(AAHO_SAL_MINIMO_max_pasado3), AAHO_SAL_MINIMO_max_pasado3 := AAHO_SAL_MINIMO]
  Ahorros[is.na(AAHO_PRO_MES_prom_pasado3), AAHO_PRO_MES_prom_pasado3 := AAHO_PRO_MES]
  Ahorros[is.na(AAHO_SAL_DISPONIB_prom_pasado3), AAHO_SAL_DISPONIB_prom_pasado3 := AAHO_SAL_DISPONIB]
  Ahorros[is.na(AAHO_SAL_HOY_prom_pasado3), AAHO_SAL_HOY_prom_pasado3 := AAHO_SAL_HOY]
  Ahorros[is.na(AAHO_SAL_MINIMO_prom_pasado3), AAHO_SAL_MINIMO_prom_pasado3 := AAHO_SAL_MINIMO]

  #Crear variable de (promedio del mes_-1, mes_-2, mes_-3, mes_-4, mes_-5) de saldos. Para minimo tambien se crea sino minimo
  Ahorros[, AAHO_PRO_MES_prom_pasado5 := rowMeans(cbind(AAHO_PRO_MES_menos5, AAHO_PRO_MES_menos4, AAHO_PRO_MES_menos3, AAHO_PRO_MES_menos2, AAHO_PRO_MES_menos1), na.rm = T)]
  Ahorros[, AAHO_SAL_DISPONIB_prom_pasado5 := rowMeans(cbind(AAHO_SAL_DISPONIB_menos5, AAHO_SAL_DISPONIB_menos4, AAHO_SAL_DISPONIB_menos3, AAHO_SAL_DISPONIB_menos2, AAHO_SAL_DISPONIB_menos1), na.rm = T)]
  Ahorros[, AAHO_SAL_HOY_prom_pasado5 := rowMeans(cbind(AAHO_SAL_HOY_menos5, AAHO_SAL_HOY_menos4, AAHO_SAL_HOY_menos3, AAHO_SAL_HOY_menos2, AAHO_SAL_HOY_menos1), na.rm = T)]
  Ahorros[, AAHO_SAL_MINIMO_prom_pasado5 := rowMeans(cbind(AAHO_SAL_MINIMO_menos5, AAHO_SAL_MINIMO_menos4, AAHO_SAL_MINIMO_menos3, AAHO_SAL_MINIMO_menos2, AAHO_SAL_MINIMO_menos1), na.rm = T)]
  
  Ahorros[, AAHO_SAL_MINIMO_min_pasado5 := pmin(AAHO_SAL_MINIMO_menos5, AAHO_SAL_MINIMO_menos4, AAHO_SAL_MINIMO_menos3, AAHO_SAL_MINIMO_menos2, AAHO_SAL_MINIMO_menos1, na.rm = T)]
  Ahorros[is.na(AAHO_SAL_MINIMO_min_pasado5), AAHO_SAL_MINIMO_min_pasado5 := 0]
    
  #Crear variable de maximo saldo en los ultimos 5 meses
  Ahorros[, AAHO_PRO_MES_max_pasado5 := pmax(AAHO_PRO_MES_menos5, AAHO_PRO_MES_menos4, AAHO_PRO_MES_menos3, AAHO_PRO_MES_menos2, AAHO_PRO_MES_menos1, na.rm = T)]
  Ahorros[, AAHO_SAL_DISPONIB_max_pasado5 := pmax(AAHO_SAL_DISPONIB_menos5, AAHO_SAL_DISPONIB_menos4, AAHO_SAL_DISPONIB_menos3, AAHO_SAL_DISPONIB_menos2, AAHO_SAL_DISPONIB_menos1, na.rm = T)]
  Ahorros[, AAHO_SAL_HOY_max_pasado5 := pmax(AAHO_SAL_HOY_menos5, AAHO_SAL_HOY_menos4, AAHO_SAL_HOY_menos3, AAHO_SAL_HOY_menos2, AAHO_SAL_HOY_menos1, na.rm = T)]
  Ahorros[, AAHO_SAL_MINIMO_max_pasado5 := pmax(AAHO_SAL_MINIMO_menos5, AAHO_SAL_MINIMO_menos4, AAHO_SAL_MINIMO_menos3, AAHO_SAL_MINIMO_menos2, AAHO_SAL_MINIMO_menos1, na.rm = T)]
  
  #Si no hay informacion en los ultimos 5 meses, ponerle el actual. Nota: se hizo validacion de que estos NAs son los que tienen Antiguedad < 31/365
  Ahorros[is.na(AAHO_PRO_MES_max_pasado5), AAHO_PRO_MES_max_pasado5 := AAHO_PRO_MES]
  Ahorros[is.na(AAHO_SAL_DISPONIB_max_pasado5), AAHO_SAL_DISPONIB_max_pasado5 := AAHO_SAL_DISPONIB]
  Ahorros[is.na(AAHO_SAL_HOY_max_pasado5), AAHO_SAL_HOY_max_pasado5 := AAHO_SAL_HOY]
  Ahorros[is.na(AAHO_SAL_MINIMO_max_pasado5), AAHO_SAL_MINIMO_max_pasado5 := AAHO_SAL_MINIMO]
  Ahorros[is.na(AAHO_PRO_MES_prom_pasado5), AAHO_PRO_MES_prom_pasado5 := AAHO_PRO_MES]
  Ahorros[is.na(AAHO_SAL_DISPONIB_prom_pasado5), AAHO_SAL_DISPONIB_prom_pasado5 := AAHO_SAL_DISPONIB]
  Ahorros[is.na(AAHO_SAL_HOY_prom_pasado5), AAHO_SAL_HOY_prom_pasado5 := AAHO_SAL_HOY]
  Ahorros[is.na(AAHO_SAL_MINIMO_prom_pasado5), AAHO_SAL_MINIMO_prom_pasado5 := AAHO_SAL_MINIMO]  
  
  ###############################################################################################################################
  
  #Promedio actual contra maximo pasado
  Ahorros[, AAHO_PRO_MES_sobre_max_pasado3 := AAHO_PRO_MES/AAHO_PRO_MES_max_pasado3]
  #A los que estuvieron en el pasado y presente en 0, ponerles 1 (constante)
  Ahorros[AAHO_PRO_MES_max_pasado3 == 0 & AAHO_PRO_MES == 0]$AAHO_PRO_MES_sobre_max_pasado3 <- 1
  #A los que estuvieron en 0 en el pasado y ahora no, ponerles el percentil 90 de los valores
  Ahorros[AAHO_PRO_MES_sobre_max_pasado3 == Inf]$AAHO_PRO_MES_sobre_max_pasado3 <- quantile(Ahorros[AAHO_PRO_MES_sobre_max_pasado3 < Inf & AAHO_PRO_MES_sobre_max_pasado3 > -Inf]$AAHO_PRO_MES_sobre_max_pasado3, 0.9, na.rm = T)
  #A los que estuvieron en 0 en el pasado y ahora negativo, ponerles el percentil 10 de los valores
  Ahorros[AAHO_PRO_MES_sobre_max_pasado3 == -Inf]$AAHO_PRO_MES_sobre_max_pasado3 <- quantile(Ahorros[AAHO_PRO_MES_sobre_max_pasado3 < Inf & AAHO_PRO_MES_sobre_max_pasado3 > -Inf]$AAHO_PRO_MES_sobre_max_pasado3, 0.1, na.rm = T)
  
  #Promedio actual contra promedio pasado
  Ahorros[, AAHO_PRO_MES_sobre_prom_pasado3 := AAHO_PRO_MES/AAHO_PRO_MES_prom_pasado3]
  #A los que estuvieron en el pasado y presente en 0, ponerles 1 (constante)
  Ahorros[AAHO_PRO_MES_prom_pasado3 == 0 & AAHO_PRO_MES == 0]$AAHO_PRO_MES_sobre_prom_pasado3 <- 1
  #A los que estuvieron en 0 en el pasado y ahora no, ponerles el percentil 90 de los valores
  Ahorros[AAHO_PRO_MES_sobre_prom_pasado3 == Inf]$AAHO_PRO_MES_sobre_prom_pasado3 <- quantile(Ahorros[AAHO_PRO_MES_sobre_prom_pasado3 < Inf & AAHO_PRO_MES_sobre_prom_pasado3 > -Inf]$AAHO_PRO_MES_sobre_prom_pasado3, 0.9, na.rm = T)
  #A los que estuvieron en 0 en el pasado y ahora negativo, ponerles el percentil 10 de los valores
  Ahorros[AAHO_PRO_MES_sobre_prom_pasado3 == -Inf]$AAHO_PRO_MES_sobre_prom_pasado3 <- quantile(Ahorros[AAHO_PRO_MES_sobre_prom_pasado3 < Inf & AAHO_PRO_MES_sobre_prom_pasado3 > -Inf]$AAHO_PRO_MES_sobre_prom_pasado3, 0.1, na.rm = T)
  
  #Promedio actual contra maximo pasado
  Ahorros[, AAHO_PRO_MES_menos_max_pasado3 := AAHO_PRO_MES - AAHO_PRO_MES_max_pasado3]

  #Promedio actual contra promedio pasado
  Ahorros[, AAHO_PRO_MES_menos_prom_pasado3 := AAHO_PRO_MES - AAHO_PRO_MES_prom_pasado3]

  #Promedio actual contra maximo pasado
  Ahorros[, AAHO_PRO_MES_sobre_max_pasado5 := AAHO_PRO_MES/AAHO_PRO_MES_max_pasado5]
  #A los que estuvieron en el pasado y presente en 0, ponerles 1 (constante)
  Ahorros[AAHO_PRO_MES_max_pasado5 == 0 & AAHO_PRO_MES == 0]$AAHO_PRO_MES_sobre_max_pasado5 <- 1
  #A los que estuvieron en 0 en el pasado y ahora no, ponerles el percentil 90 de los valores
  Ahorros[AAHO_PRO_MES_sobre_max_pasado5 == Inf]$AAHO_PRO_MES_sobre_max_pasado5 <- quantile(Ahorros[AAHO_PRO_MES_sobre_max_pasado5 < Inf & AAHO_PRO_MES_sobre_max_pasado5 > -Inf]$AAHO_PRO_MES_sobre_max_pasado5, 0.9, na.rm = T)
  #A los que estuvieron en 0 en el pasado y ahora negativo, ponerles el percentil 10 de los valores
  Ahorros[AAHO_PRO_MES_sobre_max_pasado5 == -Inf]$AAHO_PRO_MES_sobre_max_pasado5 <- quantile(Ahorros[AAHO_PRO_MES_sobre_max_pasado5 < Inf & AAHO_PRO_MES_sobre_max_pasado5 > -Inf]$AAHO_PRO_MES_sobre_max_pasado5, 0.1, na.rm = T)
  
  #Promedio actual contra promedio pasado
  Ahorros[, AAHO_PRO_MES_sobre_prom_pasado5 := AAHO_PRO_MES/AAHO_PRO_MES_prom_pasado5]
  #A los que estuvieron en el pasado y presente en 0, ponerles 1 (constante)
  Ahorros[AAHO_PRO_MES_prom_pasado5 == 0 & AAHO_PRO_MES == 0]$AAHO_PRO_MES_sobre_prom_pasado5 <- 1
  #A los que estuvieron en 0 en el pasado y ahora no, ponerles el percentil 90 de los valores
  Ahorros[AAHO_PRO_MES_sobre_prom_pasado5 == Inf]$AAHO_PRO_MES_sobre_prom_pasado5 <- quantile(Ahorros[AAHO_PRO_MES_sobre_prom_pasado5 < Inf & AAHO_PRO_MES_sobre_prom_pasado5 > -Inf]$AAHO_PRO_MES_sobre_prom_pasado5, 0.9, na.rm = T)
  #A los que estuvieron en 0 en el pasado y ahora negativo, ponerles el percentil 10 de los valores
  Ahorros[AAHO_PRO_MES_sobre_prom_pasado5 == -Inf]$AAHO_PRO_MES_sobre_prom_pasado5 <- quantile(Ahorros[AAHO_PRO_MES_sobre_prom_pasado5 < Inf & AAHO_PRO_MES_sobre_prom_pasado5 > -Inf]$AAHO_PRO_MES_sobre_prom_pasado5, 0.1, na.rm = T)
  
  #Promedio actual contra maximo pasado
  Ahorros[, AAHO_PRO_MES_menos_max_pasado5 := AAHO_PRO_MES - AAHO_PRO_MES_max_pasado5]
  
  #Promedio actual contra promedio pasado
  Ahorros[, AAHO_PRO_MES_menos_prom_pasado5 := AAHO_PRO_MES - AAHO_PRO_MES_prom_pasado5]
  
  ###############################################################################################################################
  
  #Promedio actual contra maximo pasado
  Ahorros[, AAHO_SAL_DISPONIB_sobre_max_pasado3 := AAHO_SAL_DISPONIB/AAHO_SAL_DISPONIB_max_pasado3]
  #A los que estuvieron en el pasado y presente en 0, ponerles 1 (constante)
  Ahorros[AAHO_SAL_DISPONIB_max_pasado3 == 0 & AAHO_SAL_DISPONIB == 0]$AAHO_SAL_DISPONIB_sobre_max_pasado3 <- 1
  #A los que estuvieron en 0 en el pasado y ahora no, ponerles el percentil 90 de los valores
  Ahorros[AAHO_SAL_DISPONIB_sobre_max_pasado3 == Inf]$AAHO_SAL_DISPONIB_sobre_max_pasado3 <- quantile(Ahorros[AAHO_SAL_DISPONIB_sobre_max_pasado3 < Inf & AAHO_SAL_DISPONIB_sobre_max_pasado3 > -Inf]$AAHO_SAL_DISPONIB_sobre_max_pasado3, 0.9, na.rm = T)
  #A los que estuvieron en 0 en el pasado y ahora negativo, ponerles el percentil 10 de los valores
  Ahorros[AAHO_SAL_DISPONIB_sobre_max_pasado3 == -Inf]$AAHO_SAL_DISPONIB_sobre_max_pasado3 <- quantile(Ahorros[AAHO_SAL_DISPONIB_sobre_max_pasado3 < Inf & AAHO_SAL_DISPONIB_sobre_max_pasado3 > -Inf]$AAHO_SAL_DISPONIB_sobre_max_pasado3, 0.1, na.rm = T)
  
  #Promedio actual contra promedio pasado
  Ahorros[, AAHO_SAL_DISPONIB_sobre_prom_pasado3 := AAHO_SAL_DISPONIB/AAHO_SAL_DISPONIB_prom_pasado3]
  #A los que estuvieron en el pasado y presente en 0, ponerles 1 (constante)
  Ahorros[AAHO_SAL_DISPONIB_prom_pasado3 == 0 & AAHO_SAL_DISPONIB == 0]$AAHO_SAL_DISPONIB_sobre_prom_pasado3 <- 1
  #A los que estuvieron en 0 en el pasado y ahora no, ponerles el percentil 90 de los valores
  Ahorros[AAHO_SAL_DISPONIB_sobre_prom_pasado3 == Inf]$AAHO_SAL_DISPONIB_sobre_prom_pasado3 <- quantile(Ahorros[AAHO_SAL_DISPONIB_sobre_prom_pasado3 < Inf & AAHO_SAL_DISPONIB_sobre_prom_pasado3 > -Inf]$AAHO_SAL_DISPONIB_sobre_prom_pasado3, 0.9, na.rm = T)
  #A los que estuvieron en 0 en el pasado y ahora negativo, ponerles el percentil 10 de los valores
  Ahorros[AAHO_SAL_DISPONIB_sobre_prom_pasado3 == -Inf]$AAHO_SAL_DISPONIB_sobre_prom_pasado3 <- quantile(Ahorros[AAHO_SAL_DISPONIB_sobre_prom_pasado3 < Inf & AAHO_SAL_DISPONIB_sobre_prom_pasado3 > -Inf]$AAHO_SAL_DISPONIB_sobre_prom_pasado3, 0.1, na.rm = T)
  
  #Promedio actual contra maximo pasado
  Ahorros[, AAHO_SAL_DISPONIB_menos_max_pasado3 := AAHO_SAL_DISPONIB - AAHO_SAL_DISPONIB_max_pasado3]
  
  #Promedio actual contra promedio pasado
  Ahorros[, AAHO_SAL_DISPONIB_menos_prom_pasado3 := AAHO_SAL_DISPONIB - AAHO_SAL_DISPONIB_prom_pasado3]
  
  #Promedio actual contra maximo pasado
  Ahorros[, AAHO_SAL_DISPONIB_sobre_max_pasado5 := AAHO_SAL_DISPONIB/AAHO_SAL_DISPONIB_max_pasado5]
  #A los que estuvieron en el pasado y presente en 0, ponerles 1 (constante)
  Ahorros[AAHO_SAL_DISPONIB_max_pasado5 == 0 & AAHO_SAL_DISPONIB == 0]$AAHO_SAL_DISPONIB_sobre_max_pasado5 <- 1
  #A los que estuvieron en 0 en el pasado y ahora no, ponerles el percentil 90 de los valores
  Ahorros[AAHO_SAL_DISPONIB_sobre_max_pasado5 == Inf]$AAHO_SAL_DISPONIB_sobre_max_pasado5 <- quantile(Ahorros[AAHO_SAL_DISPONIB_sobre_max_pasado5 < Inf & AAHO_SAL_DISPONIB_sobre_max_pasado5 > -Inf]$AAHO_SAL_DISPONIB_sobre_max_pasado5, 0.9, na.rm = T)
  #A los que estuvieron en 0 en el pasado y ahora negativo, ponerles el percentil 10 de los valores
  Ahorros[AAHO_SAL_DISPONIB_sobre_max_pasado5 == -Inf]$AAHO_SAL_DISPONIB_sobre_max_pasado5 <- quantile(Ahorros[AAHO_SAL_DISPONIB_sobre_max_pasado5 < Inf & AAHO_SAL_DISPONIB_sobre_max_pasado5 > -Inf]$AAHO_SAL_DISPONIB_sobre_max_pasado5, 0.1, na.rm = T)
  
  #Promedio actual contra promedio pasado
  Ahorros[, AAHO_SAL_DISPONIB_sobre_prom_pasado5 := AAHO_SAL_DISPONIB/AAHO_SAL_DISPONIB_prom_pasado5]
  #A los que estuvieron en el pasado y presente en 0, ponerles 1 (constante)
  Ahorros[AAHO_SAL_DISPONIB_prom_pasado5 == 0 & AAHO_SAL_DISPONIB == 0]$AAHO_SAL_DISPONIB_sobre_prom_pasado5 <- 1
  #A los que estuvieron en 0 en el pasado y ahora no, ponerles el percentil 90 de los valores
  Ahorros[AAHO_SAL_DISPONIB_sobre_prom_pasado5 == Inf]$AAHO_SAL_DISPONIB_sobre_prom_pasado5 <- quantile(Ahorros[AAHO_SAL_DISPONIB_sobre_prom_pasado5 < Inf & AAHO_SAL_DISPONIB_sobre_prom_pasado5 > -Inf]$AAHO_SAL_DISPONIB_sobre_prom_pasado5, 0.9, na.rm = T)
  #A los que estuvieron en 0 en el pasado y ahora negativo, ponerles el percentil 10 de los valores
  Ahorros[AAHO_SAL_DISPONIB_sobre_prom_pasado5 == -Inf]$AAHO_SAL_DISPONIB_sobre_prom_pasado5 <- quantile(Ahorros[AAHO_SAL_DISPONIB_sobre_prom_pasado5 < Inf & AAHO_SAL_DISPONIB_sobre_prom_pasado5 > -Inf]$AAHO_SAL_DISPONIB_sobre_prom_pasado5, 0.1, na.rm = T)
  
  #Promedio actual contra maximo pasado
  Ahorros[, AAHO_SAL_DISPONIB_menos_max_pasado5 := AAHO_SAL_DISPONIB - AAHO_SAL_DISPONIB_max_pasado5]
  
  #Promedio actual contra promedio pasado
  Ahorros[, AAHO_SAL_DISPONIB_menos_prom_pasado5 := AAHO_SAL_DISPONIB - AAHO_SAL_DISPONIB_prom_pasado5]
  
  ###############################################################################################################################
  
  #Promedio actual contra maximo pasado
  Ahorros[, AAHO_SAL_HOY_sobre_max_pasado3 := AAHO_SAL_HOY/AAHO_SAL_HOY_max_pasado3]
  #A los que estuvieron en el pasado y presente en 0, ponerles 1 (constante)
  Ahorros[AAHO_SAL_HOY_max_pasado3 == 0 & AAHO_SAL_HOY == 0]$AAHO_SAL_HOY_sobre_max_pasado3 <- 1
  #A los que estuvieron en 0 en el pasado y ahora no, ponerles el percentil 90 de los valores
  Ahorros[AAHO_SAL_HOY_sobre_max_pasado3 == Inf]$AAHO_SAL_HOY_sobre_max_pasado3 <- quantile(Ahorros[AAHO_SAL_HOY_sobre_max_pasado3 < Inf & AAHO_SAL_HOY_sobre_max_pasado3 > -Inf]$AAHO_SAL_HOY_sobre_max_pasado3, 0.9, na.rm = T)
  #A los que estuvieron en 0 en el pasado y ahora negativo, ponerles el percentil 10 de los valores
  Ahorros[AAHO_SAL_HOY_sobre_max_pasado3 == -Inf]$AAHO_SAL_HOY_sobre_max_pasado3 <- quantile(Ahorros[AAHO_SAL_HOY_sobre_max_pasado3 < Inf & AAHO_SAL_HOY_sobre_max_pasado3 > -Inf]$AAHO_SAL_HOY_sobre_max_pasado3, 0.1, na.rm = T)
  
  #Promedio actual contra promedio pasado
  Ahorros[, AAHO_SAL_HOY_sobre_prom_pasado3 := AAHO_SAL_HOY/AAHO_SAL_HOY_prom_pasado3]
  #A los que estuvieron en el pasado y presente en 0, ponerles 1 (constante)
  Ahorros[AAHO_SAL_HOY_prom_pasado3 == 0 & AAHO_SAL_HOY == 0]$AAHO_SAL_HOY_sobre_prom_pasado3 <- 1
  #A los que estuvieron en 0 en el pasado y ahora no, ponerles el percentil 90 de los valores
  Ahorros[AAHO_SAL_HOY_sobre_prom_pasado3 == Inf]$AAHO_SAL_HOY_sobre_prom_pasado3 <- quantile(Ahorros[AAHO_SAL_HOY_sobre_prom_pasado3 < Inf & AAHO_SAL_HOY_sobre_prom_pasado3 > -Inf]$AAHO_SAL_HOY_sobre_prom_pasado3, 0.9, na.rm = T)
  #A los que estuvieron en 0 en el pasado y ahora negativo, ponerles el percentil 10 de los valores
  Ahorros[AAHO_SAL_HOY_sobre_prom_pasado3 == -Inf]$AAHO_SAL_HOY_sobre_prom_pasado3 <- quantile(Ahorros[AAHO_SAL_HOY_sobre_prom_pasado3 < Inf & AAHO_SAL_HOY_sobre_prom_pasado3 > -Inf]$AAHO_SAL_HOY_sobre_prom_pasado3, 0.1, na.rm = T)
  
  #Promedio actual contra maximo pasado
  Ahorros[, AAHO_SAL_HOY_menos_max_pasado3 := AAHO_SAL_HOY - AAHO_SAL_HOY_max_pasado3]
  
  #Promedio actual contra promedio pasado
  Ahorros[, AAHO_SAL_HOY_menos_prom_pasado3 := AAHO_SAL_HOY - AAHO_SAL_HOY_prom_pasado3]
  
  #Promedio actual contra maximo pasado
  Ahorros[, AAHO_SAL_HOY_sobre_max_pasado5 := AAHO_SAL_HOY/AAHO_SAL_HOY_max_pasado5]
  #A los que estuvieron en el pasado y presente en 0, ponerles 1 (constante)
  Ahorros[AAHO_SAL_HOY_max_pasado5 == 0 & AAHO_SAL_HOY == 0]$AAHO_SAL_HOY_sobre_max_pasado5 <- 1
  #A los que estuvieron en 0 en el pasado y ahora no, ponerles el percentil 90 de los valores
  Ahorros[AAHO_SAL_HOY_sobre_max_pasado5 == Inf]$AAHO_SAL_HOY_sobre_max_pasado5 <- quantile(Ahorros[AAHO_SAL_HOY_sobre_max_pasado5 < Inf & AAHO_SAL_HOY_sobre_max_pasado5 > -Inf]$AAHO_SAL_HOY_sobre_max_pasado5, 0.9, na.rm = T)
  #A los que estuvieron en 0 en el pasado y ahora negativo, ponerles el percentil 10 de los valores
  Ahorros[AAHO_SAL_HOY_sobre_max_pasado5 == -Inf]$AAHO_SAL_HOY_sobre_max_pasado5 <- quantile(Ahorros[AAHO_SAL_HOY_sobre_max_pasado5 < Inf & AAHO_SAL_HOY_sobre_max_pasado5 > -Inf]$AAHO_SAL_HOY_sobre_max_pasado5, 0.1, na.rm = T)
  
  #Promedio actual contra promedio pasado
  Ahorros[, AAHO_SAL_HOY_sobre_prom_pasado5 := AAHO_SAL_HOY/AAHO_SAL_HOY_prom_pasado5]
  #A los que estuvieron en el pasado y presente en 0, ponerles 1 (constante)
  Ahorros[AAHO_SAL_HOY_prom_pasado5 == 0 & AAHO_SAL_HOY == 0]$AAHO_SAL_HOY_sobre_prom_pasado5 <- 1
  #A los que estuvieron en 0 en el pasado y ahora no, ponerles el percentil 90 de los valores
  Ahorros[AAHO_SAL_HOY_sobre_prom_pasado5 == Inf]$AAHO_SAL_HOY_sobre_prom_pasado5 <- quantile(Ahorros[AAHO_SAL_HOY_sobre_prom_pasado5 < Inf & AAHO_SAL_HOY_sobre_prom_pasado5 > -Inf]$AAHO_SAL_HOY_sobre_prom_pasado5, 0.9, na.rm = T)
  #A los que estuvieron en 0 en el pasado y ahora negativo, ponerles el percentil 10 de los valores
  Ahorros[AAHO_SAL_HOY_sobre_prom_pasado5 == -Inf]$AAHO_SAL_HOY_sobre_prom_pasado5 <- quantile(Ahorros[AAHO_SAL_HOY_sobre_prom_pasado5 < Inf & AAHO_SAL_HOY_sobre_prom_pasado5 > -Inf]$AAHO_SAL_HOY_sobre_prom_pasado5, 0.1, na.rm = T)
  
  #Promedio actual contra maximo pasado
  Ahorros[, AAHO_SAL_HOY_menos_max_pasado5 := AAHO_SAL_HOY - AAHO_SAL_HOY_max_pasado5]
  
  #Promedio actual contra promedio pasado
  Ahorros[, AAHO_SAL_HOY_menos_prom_pasado5 := AAHO_SAL_HOY - AAHO_SAL_HOY_prom_pasado5]
  
  ###############################################################################################################################
  
  #Promedio actual contra maximo pasado
  Ahorros[, AAHO_SAL_MINIMO_sobre_max_pasado3 := AAHO_SAL_MINIMO/AAHO_SAL_MINIMO_max_pasado3]
  #A los que estuvieron en el pasado y presente en 0, ponerles 1 (constante)
  Ahorros[AAHO_SAL_MINIMO_max_pasado3 == 0 & AAHO_SAL_MINIMO == 0]$AAHO_SAL_MINIMO_sobre_max_pasado3 <- 1
  #A los que estuvieron en 0 en el pasado y ahora no, ponerles el percentil 90 de los valores
  Ahorros[AAHO_SAL_MINIMO_sobre_max_pasado3 == Inf]$AAHO_SAL_MINIMO_sobre_max_pasado3 <- quantile(Ahorros[AAHO_SAL_MINIMO_sobre_max_pasado3 < Inf & AAHO_SAL_MINIMO_sobre_max_pasado3 > -Inf]$AAHO_SAL_MINIMO_sobre_max_pasado3, 0.9, na.rm = T)
  #A los que estuvieron en 0 en el pasado y ahora negativo, ponerles el percentil 10 de los valores
  Ahorros[AAHO_SAL_MINIMO_sobre_max_pasado3 == -Inf]$AAHO_SAL_MINIMO_sobre_max_pasado3 <- quantile(Ahorros[AAHO_SAL_MINIMO_sobre_max_pasado3 < Inf & AAHO_SAL_MINIMO_sobre_max_pasado3 > -Inf]$AAHO_SAL_MINIMO_sobre_max_pasado3, 0.1, na.rm = T)
  
  #Promedio actual contra promedio pasado
  Ahorros[, AAHO_SAL_MINIMO_sobre_prom_pasado3 := AAHO_SAL_MINIMO/AAHO_SAL_MINIMO_prom_pasado3]
  #A los que estuvieron en el pasado y presente en 0, ponerles 1 (constante)
  Ahorros[AAHO_SAL_MINIMO_prom_pasado3 == 0 & AAHO_SAL_MINIMO == 0]$AAHO_SAL_MINIMO_sobre_prom_pasado3 <- 1
  #A los que estuvieron en 0 en el pasado y ahora no, ponerles el percentil 90 de los valores
  Ahorros[AAHO_SAL_MINIMO_sobre_prom_pasado3 == Inf]$AAHO_SAL_MINIMO_sobre_prom_pasado3 <- quantile(Ahorros[AAHO_SAL_MINIMO_sobre_prom_pasado3 < Inf & AAHO_SAL_MINIMO_sobre_prom_pasado3 > -Inf]$AAHO_SAL_MINIMO_sobre_prom_pasado3, 0.9, na.rm = T)
  #A los que estuvieron en 0 en el pasado y ahora negativo, ponerles el percentil 10 de los valores
  Ahorros[AAHO_SAL_MINIMO_sobre_prom_pasado3 == -Inf]$AAHO_SAL_MINIMO_sobre_prom_pasado3 <- quantile(Ahorros[AAHO_SAL_MINIMO_sobre_prom_pasado3 < Inf & AAHO_SAL_MINIMO_sobre_prom_pasado3 > -Inf]$AAHO_SAL_MINIMO_sobre_prom_pasado3, 0.1, na.rm = T)
  
  #Promedio actual contra maximo pasado
  Ahorros[, AAHO_SAL_MINIMO_menos_max_pasado3 := AAHO_SAL_MINIMO - AAHO_SAL_MINIMO_max_pasado3]
  
  #Promedio actual contra promedio pasado
  Ahorros[, AAHO_SAL_MINIMO_menos_prom_pasado3 := AAHO_SAL_MINIMO - AAHO_SAL_MINIMO_prom_pasado3]
  
  #Promedio actual contra maximo pasado
  Ahorros[, AAHO_SAL_MINIMO_sobre_max_pasado5 := AAHO_SAL_MINIMO/AAHO_SAL_MINIMO_max_pasado5]
  #A los que estuvieron en el pasado y presente en 0, ponerles 1 (constante)
  Ahorros[AAHO_SAL_MINIMO_max_pasado5 == 0 & AAHO_SAL_MINIMO == 0]$AAHO_SAL_MINIMO_sobre_max_pasado5 <- 1
  #A los que estuvieron en 0 en el pasado y ahora no, ponerles el percentil 90 de los valores
  Ahorros[AAHO_SAL_MINIMO_sobre_max_pasado5 == Inf]$AAHO_SAL_MINIMO_sobre_max_pasado5 <- quantile(Ahorros[AAHO_SAL_MINIMO_sobre_max_pasado5 < Inf & AAHO_SAL_MINIMO_sobre_max_pasado5 > -Inf]$AAHO_SAL_MINIMO_sobre_max_pasado5, 0.9, na.rm = T)
  #A los que estuvieron en 0 en el pasado y ahora negativo, ponerles el percentil 10 de los valores
  Ahorros[AAHO_SAL_MINIMO_sobre_max_pasado5 == -Inf]$AAHO_SAL_MINIMO_sobre_max_pasado5 <- quantile(Ahorros[AAHO_SAL_MINIMO_sobre_max_pasado5 < Inf & AAHO_SAL_MINIMO_sobre_max_pasado5 > -Inf]$AAHO_SAL_MINIMO_sobre_max_pasado5, 0.1, na.rm = T)
  
  #Promedio actual contra promedio pasado
  Ahorros[, AAHO_SAL_MINIMO_sobre_prom_pasado5 := AAHO_SAL_MINIMO/AAHO_SAL_MINIMO_prom_pasado5]
  #A los que estuvieron en el pasado y presente en 0, ponerles 1 (constante)
  Ahorros[AAHO_SAL_MINIMO_prom_pasado5 == 0 & AAHO_SAL_MINIMO == 0]$AAHO_SAL_MINIMO_sobre_prom_pasado5 <- 1
  #A los que estuvieron en 0 en el pasado y ahora no, ponerles el percentil 90 de los valores
  Ahorros[AAHO_SAL_MINIMO_sobre_prom_pasado5 == Inf]$AAHO_SAL_MINIMO_sobre_prom_pasado5 <- quantile(Ahorros[AAHO_SAL_MINIMO_sobre_prom_pasado5 < Inf & AAHO_SAL_MINIMO_sobre_prom_pasado5 > -Inf]$AAHO_SAL_MINIMO_sobre_prom_pasado5, 0.9, na.rm = T)
  #A los que estuvieron en 0 en el pasado y ahora negativo, ponerles el percentil 10 de los valores
  Ahorros[AAHO_SAL_MINIMO_sobre_prom_pasado5 == -Inf]$AAHO_SAL_MINIMO_sobre_prom_pasado5 <- quantile(Ahorros[AAHO_SAL_MINIMO_sobre_prom_pasado5 < Inf & AAHO_SAL_MINIMO_sobre_prom_pasado5 > -Inf]$AAHO_SAL_MINIMO_sobre_prom_pasado5, 0.1, na.rm = T)
  
  #Promedio actual contra maximo pasado
  Ahorros[, AAHO_SAL_MINIMO_menos_max_pasado5 := AAHO_SAL_MINIMO - AAHO_SAL_MINIMO_max_pasado5]
  
  #Promedio actual contra promedio pasado
  Ahorros[, AAHO_SAL_MINIMO_menos_prom_pasado5 := AAHO_SAL_MINIMO - AAHO_SAL_MINIMO_prom_pasado5]
  
  ###############################################################################################################################
  
  #Suma ponderada: primero poner en 0 los saldos rezagados
  Ahorros[is.na(AAHO_PRO_MES_menos1), AAHO_PRO_MES_menos1 := 0]
  Ahorros[is.na(AAHO_PRO_MES_menos2), AAHO_PRO_MES_menos2 := 0]
  Ahorros[is.na(AAHO_PRO_MES_menos3), AAHO_PRO_MES_menos3 := 0]
  Ahorros[is.na(AAHO_PRO_MES_menos4), AAHO_PRO_MES_menos4 := 0]
  Ahorros[is.na(AAHO_PRO_MES_menos5), AAHO_PRO_MES_menos5 := 0]
  
  Ahorros[is.na(AAHO_SAL_DISPONIB_menos1), AAHO_SAL_DISPONIB_menos1 := 0]
  Ahorros[is.na(AAHO_SAL_DISPONIB_menos2), AAHO_SAL_DISPONIB_menos2 := 0]
  Ahorros[is.na(AAHO_SAL_DISPONIB_menos3), AAHO_SAL_DISPONIB_menos3 := 0]
  Ahorros[is.na(AAHO_SAL_DISPONIB_menos4), AAHO_SAL_DISPONIB_menos4 := 0]
  Ahorros[is.na(AAHO_SAL_DISPONIB_menos5), AAHO_SAL_DISPONIB_menos5 := 0]

  Ahorros[is.na(AAHO_SAL_HOY_menos1), AAHO_SAL_HOY_menos1 := 0]
  Ahorros[is.na(AAHO_SAL_HOY_menos2), AAHO_SAL_HOY_menos2 := 0]
  Ahorros[is.na(AAHO_SAL_HOY_menos3), AAHO_SAL_HOY_menos3 := 0]
  Ahorros[is.na(AAHO_SAL_HOY_menos4), AAHO_SAL_HOY_menos4 := 0]
  Ahorros[is.na(AAHO_SAL_HOY_menos5), AAHO_SAL_HOY_menos5 := 0]
  
  Ahorros[is.na(AAHO_SAL_MINIMO_menos1), AAHO_SAL_MINIMO_menos1 := 0]
  Ahorros[is.na(AAHO_SAL_MINIMO_menos2), AAHO_SAL_MINIMO_menos2 := 0]
  Ahorros[is.na(AAHO_SAL_MINIMO_menos3), AAHO_SAL_MINIMO_menos3 := 0]
  Ahorros[is.na(AAHO_SAL_MINIMO_menos4), AAHO_SAL_MINIMO_menos4 := 0]
  Ahorros[is.na(AAHO_SAL_MINIMO_menos5), AAHO_SAL_MINIMO_menos5 := 0]
  
  #Construir suma ponderada
  Ahorros[, AAHO_PRO_MES_sumaponderada := AAHO_PRO_MES + (1/2)*AAHO_PRO_MES_menos1 + (1/4)*AAHO_PRO_MES_menos2 + (1/8)*AAHO_PRO_MES_menos3]
  Ahorros[, AAHO_SAL_DISPONIB_sumaponderada := AAHO_SAL_DISPONIB + (1/2)*AAHO_SAL_DISPONIB_menos1 + (1/4)*AAHO_SAL_DISPONIB_menos2 + (1/8)*AAHO_SAL_DISPONIB_menos3]
  Ahorros[, AAHO_SAL_HOY_sumaponderada := AAHO_SAL_HOY + (1/2)*AAHO_SAL_HOY_menos1 + (1/4)*AAHO_SAL_HOY_menos2 + (1/8)*AAHO_SAL_HOY_menos3]
  Ahorros[, AAHO_SAL_MINIMO_sumaponderada := AAHO_SAL_MINIMO + (1/2)*AAHO_SAL_MINIMO_menos1 + (1/4)*AAHO_SAL_MINIMO_menos2 + (1/8)*AAHO_SAL_MINIMO_menos3]
  
  return(Ahorros)
}



#' Creacion de otras variables de saldos
#'
#' @param Ahorros La tabla principal.
#'
#' @return La tabla con las variables de cuantas veces ha subido, mantenido o bajado el saldo.
#'
#' @examples
#' Ahorros <- Variables_saldos_cambios(Ahorros = Ahorros)
#' 
Variables_saldos_cambios <- function(Ahorros) {
  
###################################################################################################
  
  Ahorros[, AAHO_SAL_HOY_t0_menos_t1 := AAHO_SAL_HOY - AAHO_SAL_HOY_menos1]
  Ahorros[AAHO_SAL_HOY_t0_menos_t1 < 0, AAHO_SAL_HOY_t0_menos_t1 := -1]
  Ahorros[AAHO_SAL_HOY_t0_menos_t1 > 0, AAHO_SAL_HOY_t0_menos_t1 := 1]
  Ahorros$AAHO_SAL_HOY_t0_menos_t1 <- factor(Ahorros$AAHO_SAL_HOY_t0_menos_t1, levels = c(-1, 0, 1))

  Ahorros[, AAHO_SAL_HOY_t1_menos_t2 := AAHO_SAL_HOY_menos1 - AAHO_SAL_HOY_menos2]
  Ahorros[AAHO_SAL_HOY_t1_menos_t2 < 0, AAHO_SAL_HOY_t1_menos_t2 := -1]
  Ahorros[AAHO_SAL_HOY_t1_menos_t2 > 0, AAHO_SAL_HOY_t1_menos_t2 := 1]
  Ahorros$AAHO_SAL_HOY_t1_menos_t2 <- factor(Ahorros$AAHO_SAL_HOY_t1_menos_t2, levels = c(-1, 0, 1))
  
  Ahorros[, AAHO_SAL_HOY_t2_menos_t3 := AAHO_SAL_HOY_menos2 - AAHO_SAL_HOY_menos3]
  Ahorros[AAHO_SAL_HOY_t2_menos_t3 < 0, AAHO_SAL_HOY_t2_menos_t3 := -1]
  Ahorros[AAHO_SAL_HOY_t2_menos_t3 > 0, AAHO_SAL_HOY_t2_menos_t3 := 1]
  Ahorros$AAHO_SAL_HOY_t2_menos_t3 <- factor(Ahorros$AAHO_SAL_HOY_t2_menos_t3, levels = c(-1, 0, 1))
  
  Ahorros[, AAHO_SAL_HOY_t3_menos_t4 := AAHO_SAL_HOY_menos3 - AAHO_SAL_HOY_menos4]
  Ahorros[AAHO_SAL_HOY_t3_menos_t4 < 0, AAHO_SAL_HOY_t3_menos_t4 := -1]
  Ahorros[AAHO_SAL_HOY_t3_menos_t4 > 0, AAHO_SAL_HOY_t3_menos_t4 := 1]
  Ahorros$AAHO_SAL_HOY_t3_menos_t4 <- factor(Ahorros$AAHO_SAL_HOY_t3_menos_t4, levels = c(-1, 0, 1))
  
  Ahorros[, AAHO_SAL_HOY_t4_menos_t5 := AAHO_SAL_HOY_menos4 - AAHO_SAL_HOY_menos5]
  Ahorros[AAHO_SAL_HOY_t4_menos_t5 < 0, AAHO_SAL_HOY_t4_menos_t5 := -1]
  Ahorros[AAHO_SAL_HOY_t4_menos_t5 > 0, AAHO_SAL_HOY_t4_menos_t5 := 1]
  Ahorros$AAHO_SAL_HOY_t4_menos_t5 <- factor(Ahorros$AAHO_SAL_HOY_t4_menos_t5, levels = c(-1, 0, 1))
  
###################################################################################################
  
  Ahorros[, AAHO_SAL_DISPONIB_t0_menos_t1 := AAHO_SAL_DISPONIB - AAHO_SAL_DISPONIB_menos1]
  Ahorros[AAHO_SAL_DISPONIB_t0_menos_t1 < 0, AAHO_SAL_DISPONIB_t0_menos_t1 := -1]
  Ahorros[AAHO_SAL_DISPONIB_t0_menos_t1 > 0, AAHO_SAL_DISPONIB_t0_menos_t1 := 1]
  Ahorros$AAHO_SAL_DISPONIB_t0_menos_t1 <- factor(Ahorros$AAHO_SAL_DISPONIB_t0_menos_t1, levels = c(-1, 0, 1))
  
  Ahorros[, AAHO_SAL_DISPONIB_t1_menos_t2 := AAHO_SAL_DISPONIB_menos1 - AAHO_SAL_DISPONIB_menos2]
  Ahorros[AAHO_SAL_DISPONIB_t1_menos_t2 < 0, AAHO_SAL_DISPONIB_t1_menos_t2 := -1]
  Ahorros[AAHO_SAL_DISPONIB_t1_menos_t2 > 0, AAHO_SAL_DISPONIB_t1_menos_t2 := 1]
  Ahorros$AAHO_SAL_DISPONIB_t1_menos_t2 <- factor(Ahorros$AAHO_SAL_DISPONIB_t1_menos_t2, levels = c(-1, 0, 1))
  
  Ahorros[, AAHO_SAL_DISPONIB_t2_menos_t3 := AAHO_SAL_DISPONIB_menos2 - AAHO_SAL_DISPONIB_menos3]
  Ahorros[AAHO_SAL_DISPONIB_t2_menos_t3 < 0, AAHO_SAL_DISPONIB_t2_menos_t3 := -1]
  Ahorros[AAHO_SAL_DISPONIB_t2_menos_t3 > 0, AAHO_SAL_DISPONIB_t2_menos_t3 := 1]
  Ahorros$AAHO_SAL_DISPONIB_t2_menos_t3 <- factor(Ahorros$AAHO_SAL_DISPONIB_t2_menos_t3, levels = c(-1, 0, 1))
  
  Ahorros[, AAHO_SAL_DISPONIB_t3_menos_t4 := AAHO_SAL_DISPONIB_menos3 - AAHO_SAL_DISPONIB_menos4]
  Ahorros[AAHO_SAL_DISPONIB_t3_menos_t4 < 0, AAHO_SAL_DISPONIB_t3_menos_t4 := -1]
  Ahorros[AAHO_SAL_DISPONIB_t3_menos_t4 > 0, AAHO_SAL_DISPONIB_t3_menos_t4 := 1]
  Ahorros$AAHO_SAL_DISPONIB_t3_menos_t4 <- factor(Ahorros$AAHO_SAL_DISPONIB_t3_menos_t4, levels = c(-1, 0, 1))
  
  Ahorros[, AAHO_SAL_DISPONIB_t4_menos_t5 := AAHO_SAL_DISPONIB_menos4 - AAHO_SAL_DISPONIB_menos5]
  Ahorros[AAHO_SAL_DISPONIB_t4_menos_t5 < 0, AAHO_SAL_DISPONIB_t4_menos_t5 := -1]
  Ahorros[AAHO_SAL_DISPONIB_t4_menos_t5 > 0, AAHO_SAL_DISPONIB_t4_menos_t5 := 1]
  Ahorros$AAHO_SAL_DISPONIB_t4_menos_t5 <- factor(Ahorros$AAHO_SAL_DISPONIB_t4_menos_t5, levels = c(-1, 0, 1))
  
###################################################################################################
  
  Ahorros[, AAHO_SAL_MINIMO_t0_menos_t1 := AAHO_SAL_MINIMO - AAHO_SAL_MINIMO_menos1]
  Ahorros[AAHO_SAL_MINIMO_t0_menos_t1 < 0, AAHO_SAL_MINIMO_t0_menos_t1 := -1]
  Ahorros[AAHO_SAL_MINIMO_t0_menos_t1 > 0, AAHO_SAL_MINIMO_t0_menos_t1 := 1]
  Ahorros$AAHO_SAL_MINIMO_t0_menos_t1 <- factor(Ahorros$AAHO_SAL_MINIMO_t0_menos_t1, levels = c(-1, 0, 1))
  
  Ahorros[, AAHO_SAL_MINIMO_t1_menos_t2 := AAHO_SAL_MINIMO_menos1 - AAHO_SAL_MINIMO_menos2]
  Ahorros[AAHO_SAL_MINIMO_t1_menos_t2 < 0, AAHO_SAL_MINIMO_t1_menos_t2 := -1]
  Ahorros[AAHO_SAL_MINIMO_t1_menos_t2 > 0, AAHO_SAL_MINIMO_t1_menos_t2 := 1]
  Ahorros$AAHO_SAL_MINIMO_t1_menos_t2 <- factor(Ahorros$AAHO_SAL_MINIMO_t1_menos_t2, levels = c(-1, 0, 1))
  
  Ahorros[, AAHO_SAL_MINIMO_t2_menos_t3 := AAHO_SAL_MINIMO_menos2 - AAHO_SAL_MINIMO_menos3]
  Ahorros[AAHO_SAL_MINIMO_t2_menos_t3 < 0, AAHO_SAL_MINIMO_t2_menos_t3 := -1]
  Ahorros[AAHO_SAL_MINIMO_t2_menos_t3 > 0, AAHO_SAL_MINIMO_t2_menos_t3 := 1]
  Ahorros$AAHO_SAL_MINIMO_t2_menos_t3 <- factor(Ahorros$AAHO_SAL_MINIMO_t2_menos_t3, levels = c(-1, 0, 1))
  
  Ahorros[, AAHO_SAL_MINIMO_t3_menos_t4 := AAHO_SAL_MINIMO_menos3 - AAHO_SAL_MINIMO_menos4]
  Ahorros[AAHO_SAL_MINIMO_t3_menos_t4 < 0, AAHO_SAL_MINIMO_t3_menos_t4 := -1]
  Ahorros[AAHO_SAL_MINIMO_t3_menos_t4 > 0, AAHO_SAL_MINIMO_t3_menos_t4 := 1]
  Ahorros$AAHO_SAL_MINIMO_t3_menos_t4 <- factor(Ahorros$AAHO_SAL_MINIMO_t3_menos_t4, levels = c(-1, 0, 1))
  
  Ahorros[, AAHO_SAL_MINIMO_t4_menos_t5 := AAHO_SAL_MINIMO_menos4 - AAHO_SAL_MINIMO_menos5]
  Ahorros[AAHO_SAL_MINIMO_t4_menos_t5 < 0, AAHO_SAL_MINIMO_t4_menos_t5 := -1]
  Ahorros[AAHO_SAL_MINIMO_t4_menos_t5 > 0, AAHO_SAL_MINIMO_t4_menos_t5 := 1]
  Ahorros$AAHO_SAL_MINIMO_t4_menos_t5 <- factor(Ahorros$AAHO_SAL_MINIMO_t4_menos_t5, levels = c(-1, 0, 1))

###################################################################################################
  
  Ahorros[, AAHO_PRO_MES_t0_menos_t1 := AAHO_PRO_MES - AAHO_PRO_MES_menos1]
  Ahorros[AAHO_PRO_MES_t0_menos_t1 < 0, AAHO_PRO_MES_t0_menos_t1 := -1]
  Ahorros[AAHO_PRO_MES_t0_menos_t1 > 0, AAHO_PRO_MES_t0_menos_t1 := 1]
  Ahorros$AAHO_PRO_MES_t0_menos_t1 <- factor(Ahorros$AAHO_PRO_MES_t0_menos_t1, levels = c(-1, 0, 1))
  
  Ahorros[, AAHO_PRO_MES_t1_menos_t2 := AAHO_PRO_MES_menos1 - AAHO_PRO_MES_menos2]
  Ahorros[AAHO_PRO_MES_t1_menos_t2 < 0, AAHO_PRO_MES_t1_menos_t2 := -1]
  Ahorros[AAHO_PRO_MES_t1_menos_t2 > 0, AAHO_PRO_MES_t1_menos_t2 := 1]
  Ahorros$AAHO_PRO_MES_t1_menos_t2 <- factor(Ahorros$AAHO_PRO_MES_t1_menos_t2, levels = c(-1, 0, 1))
  
  Ahorros[, AAHO_PRO_MES_t2_menos_t3 := AAHO_PRO_MES_menos2 - AAHO_PRO_MES_menos3]
  Ahorros[AAHO_PRO_MES_t2_menos_t3 < 0, AAHO_PRO_MES_t2_menos_t3 := -1]
  Ahorros[AAHO_PRO_MES_t2_menos_t3 > 0, AAHO_PRO_MES_t2_menos_t3 := 1]
  Ahorros$AAHO_PRO_MES_t2_menos_t3 <- factor(Ahorros$AAHO_PRO_MES_t2_menos_t3, levels = c(-1, 0, 1))
  
  Ahorros[, AAHO_PRO_MES_t3_menos_t4 := AAHO_PRO_MES_menos3 - AAHO_PRO_MES_menos4]
  Ahorros[AAHO_PRO_MES_t3_menos_t4 < 0, AAHO_PRO_MES_t3_menos_t4 := -1]
  Ahorros[AAHO_PRO_MES_t3_menos_t4 > 0, AAHO_PRO_MES_t3_menos_t4 := 1]
  Ahorros$AAHO_PRO_MES_t3_menos_t4 <- factor(Ahorros$AAHO_PRO_MES_t3_menos_t4, levels = c(-1, 0, 1))
  
  Ahorros[, AAHO_PRO_MES_t4_menos_t5 := AAHO_PRO_MES_menos4 - AAHO_PRO_MES_menos5]
  Ahorros[AAHO_PRO_MES_t4_menos_t5 < 0, AAHO_PRO_MES_t4_menos_t5 := -1]
  Ahorros[AAHO_PRO_MES_t4_menos_t5 > 0, AAHO_PRO_MES_t4_menos_t5 := 1]
  Ahorros$AAHO_PRO_MES_t4_menos_t5 <- factor(Ahorros$AAHO_PRO_MES_t4_menos_t5, levels = c(-1, 0, 1))
  
  ###################################################################################################
  ###################################################################################################
  
  Ahorros[, AAHO_SAL_HOY_subidas := (AAHO_SAL_HOY_t0_menos_t1 == 1) + (AAHO_SAL_HOY_t1_menos_t2 == 1) + (AAHO_SAL_HOY_t2_menos_t3 == 1) + (AAHO_SAL_HOY_t3_menos_t4 == 1) + (AAHO_SAL_HOY_t4_menos_t5 == 1)]
  Ahorros[, AAHO_SAL_HOY_bajadas := (AAHO_SAL_HOY_t0_menos_t1 == -1) + (AAHO_SAL_HOY_t1_menos_t2 == -1) + (AAHO_SAL_HOY_t2_menos_t3 == -1) + (AAHO_SAL_HOY_t3_menos_t4 == -1) + (AAHO_SAL_HOY_t4_menos_t5 == -1)]
  Ahorros[, AAHO_SAL_HOY_estables := (AAHO_SAL_HOY_t0_menos_t1 == 0) + (AAHO_SAL_HOY_t1_menos_t2 == 0) + (AAHO_SAL_HOY_t2_menos_t3 == 0) + (AAHO_SAL_HOY_t3_menos_t4 == 0) + (AAHO_SAL_HOY_t4_menos_t5 == 0)]
  Ahorros$AAHO_SAL_HOY_subidas <- factor(Ahorros$AAHO_SAL_HOY_subidas)
  Ahorros$AAHO_SAL_HOY_bajadas <- factor(Ahorros$AAHO_SAL_HOY_bajadas)
  Ahorros$AAHO_SAL_HOY_estables <- factor(Ahorros$AAHO_SAL_HOY_estables)

  Ahorros[, AAHO_SAL_MINIMO_subidas := (AAHO_SAL_MINIMO_t0_menos_t1 == 1) + (AAHO_SAL_MINIMO_t1_menos_t2 == 1) + (AAHO_SAL_MINIMO_t2_menos_t3 == 1) + (AAHO_SAL_MINIMO_t3_menos_t4 == 1) + (AAHO_SAL_MINIMO_t4_menos_t5 == 1)]
  Ahorros[, AAHO_SAL_MINIMO_bajadas := (AAHO_SAL_MINIMO_t0_menos_t1 == -1) + (AAHO_SAL_MINIMO_t1_menos_t2 == -1) + (AAHO_SAL_MINIMO_t2_menos_t3 == -1) + (AAHO_SAL_MINIMO_t3_menos_t4 == -1) + (AAHO_SAL_MINIMO_t4_menos_t5 == -1)]
  Ahorros[, AAHO_SAL_MINIMO_estables := (AAHO_SAL_MINIMO_t0_menos_t1 == 0) + (AAHO_SAL_MINIMO_t1_menos_t2 == 0) + (AAHO_SAL_MINIMO_t2_menos_t3 == 0) + (AAHO_SAL_MINIMO_t3_menos_t4 == 0) + (AAHO_SAL_MINIMO_t4_menos_t5 == 0)]
  Ahorros$AAHO_SAL_MINIMO_subidas <- factor(Ahorros$AAHO_SAL_MINIMO_subidas)
  Ahorros$AAHO_SAL_MINIMO_bajadas <- factor(Ahorros$AAHO_SAL_MINIMO_bajadas)
  Ahorros$AAHO_SAL_MINIMO_estables <- factor(Ahorros$AAHO_SAL_MINIMO_estables)
  
  Ahorros[, AAHO_SAL_DISPONIB_subidas := (AAHO_SAL_DISPONIB_t0_menos_t1 == 1) + (AAHO_SAL_DISPONIB_t1_menos_t2 == 1) + (AAHO_SAL_DISPONIB_t2_menos_t3 == 1) + (AAHO_SAL_DISPONIB_t3_menos_t4 == 1) + (AAHO_SAL_DISPONIB_t4_menos_t5 == 1)]
  Ahorros[, AAHO_SAL_DISPONIB_bajadas := (AAHO_SAL_DISPONIB_t0_menos_t1 == -1) + (AAHO_SAL_DISPONIB_t1_menos_t2 == -1) + (AAHO_SAL_DISPONIB_t2_menos_t3 == -1) + (AAHO_SAL_DISPONIB_t3_menos_t4 == -1) + (AAHO_SAL_DISPONIB_t4_menos_t5 == -1)]
  Ahorros[, AAHO_SAL_DISPONIB_estables := (AAHO_SAL_DISPONIB_t0_menos_t1 == 0) + (AAHO_SAL_DISPONIB_t1_menos_t2 == 0) + (AAHO_SAL_DISPONIB_t2_menos_t3 == 0) + (AAHO_SAL_DISPONIB_t3_menos_t4 == 0) + (AAHO_SAL_DISPONIB_t4_menos_t5 == 0)]
  Ahorros$AAHO_SAL_DISPONIB_subidas <- factor(Ahorros$AAHO_SAL_DISPONIB_subidas)
  Ahorros$AAHO_SAL_DISPONIB_bajadas <- factor(Ahorros$AAHO_SAL_DISPONIB_bajadas)
  Ahorros$AAHO_SAL_DISPONIB_estables <- factor(Ahorros$AAHO_SAL_DISPONIB_estables)
  
  Ahorros[, AAHO_PRO_MES_subidas := (AAHO_PRO_MES_t0_menos_t1 == 1) + (AAHO_PRO_MES_t1_menos_t2 == 1) + (AAHO_PRO_MES_t2_menos_t3 == 1) + (AAHO_PRO_MES_t3_menos_t4 == 1) + (AAHO_PRO_MES_t4_menos_t5 == 1)]
  Ahorros[, AAHO_PRO_MES_bajadas := (AAHO_PRO_MES_t0_menos_t1 == -1) + (AAHO_PRO_MES_t1_menos_t2 == -1) + (AAHO_PRO_MES_t2_menos_t3 == -1) + (AAHO_PRO_MES_t3_menos_t4 == -1) + (AAHO_PRO_MES_t4_menos_t5 == -1)]
  Ahorros[, AAHO_PRO_MES_estables := (AAHO_PRO_MES_t0_menos_t1 == 0) + (AAHO_PRO_MES_t1_menos_t2 == 0) + (AAHO_PRO_MES_t2_menos_t3 == 0) + (AAHO_PRO_MES_t3_menos_t4 == 0) + (AAHO_PRO_MES_t4_menos_t5 == 0)]
  Ahorros$AAHO_PRO_MES_subidas <- factor(Ahorros$AAHO_PRO_MES_subidas)
  Ahorros$AAHO_PRO_MES_bajadas <- factor(Ahorros$AAHO_PRO_MES_bajadas)
  Ahorros$AAHO_PRO_MES_estables <- factor(Ahorros$AAHO_PRO_MES_estables)
  
  return(Ahorros)
}



#' Incorporacion de variables sobre inactividad rezagadas: si en los anteriores 5 meses fue inactivo,
#' cuantas veces fue productivo, cuanto era su saldo la ultima vez que estuvo productivo
#'
#' @param Ahorros La tabla a la que se van a incorporar las variables.
#'
#' @return La tabla con las variables incorporadas.
#'
#' @examples
#' Ahorros <- Variables_inactividad_rezagadas(Ahorros = Ahorros)
#' 
Variables_inactividad_rezagadas <- function(Ahorros) {
  
  ###Ahorros[, dias_ult_mov_t0_menos_t1 := Dias_ult_mov - Dias_ult_mov_menos1]
  ###Ahorros[, dias_ult_mov_t1_menos_t2 := Dias_ult_mov_menos1 - Dias_ult_mov_menos2]
  ###Ahorros[, dias_ult_mov_t2_menos_t3 := Dias_ult_mov_menos2 - Dias_ult_mov_menos3]
  ###Ahorros[, dias_ult_mov_t3_menos_t4 := Dias_ult_mov_menos3 - Dias_ult_mov_menos4]
  ###Ahorros[, dias_ult_mov_t4_menos_t5 := Dias_ult_mov_menos4 - Dias_ult_mov_menos5]
  
  #Ahorros[is.na(Dias_ult_mov_menos1), Dias_ult_mov_menos1 := "No aparece"]
  Ahorros[is.na(Dias_ult_mov_menos2), Dias_ult_mov_menos2 := 1000000000000]
  Ahorros[is.na(Dias_ult_mov_menos3), Dias_ult_mov_menos3 := 1000000000000]
  Ahorros[is.na(Dias_ult_mov_menos4), Dias_ult_mov_menos4 := 1000000000000]
  Ahorros[is.na(Dias_ult_mov_menos5), Dias_ult_mov_menos5 := 1000000000000]

  Ahorros[, veces_productivo := as.numeric((Dias_ult_mov_menos1 < 61) + (Dias_ult_mov_menos2 < 61) + (Dias_ult_mov_menos3 < 61) + (Dias_ult_mov_menos4 < 61) + (Dias_ult_mov_menos5 < 61))]
  Ahorros[, fue_inactiva := as.numeric((Dias_ult_mov_menos1 < 1000000000000 & Dias_ult_mov_menos1 > 180) | (Dias_ult_mov_menos2 < 1000000000000 & Dias_ult_mov_menos2 > 180) | (Dias_ult_mov_menos3 < 1000000000000 & Dias_ult_mov_menos3 > 180) + (Dias_ult_mov_menos4 < 1000000000000 & Dias_ult_mov_menos4 > 180) + (Dias_ult_mov_menos5 < 1000000000000 & Dias_ult_mov_menos5 > 180))]
  
  Ahorros$veces_productivo <- as.factor(Ahorros$veces_productivo)
  Ahorros$fue_inactiva <- as.factor(Ahorros$fue_inactiva)
  
  Ahorros[, AAHO_SAL_HOY_siendoproductivo := ifelse(Dias_ult_mov_menos1 < 61, AAHO_SAL_HOY_menos1, 
                                                    ifelse(Dias_ult_mov_menos2 < 61, AAHO_SAL_HOY_menos2,
                                                           ifelse(Dias_ult_mov_menos3 < 61, AAHO_SAL_HOY_menos3,
                                                                  ifelse(Dias_ult_mov_menos4 < 61, AAHO_SAL_HOY_menos4,
                                                                         ifelse(Dias_ult_mov_menos5 < 61, AAHO_SAL_HOY_menos5, "Nunca fue productivo")))))]
  
  Ahorros[, AAHO_SAL_MINIMO_siendoproductivo := ifelse(Dias_ult_mov_menos1 < 61, AAHO_SAL_MINIMO_menos1, 
                                                    ifelse(Dias_ult_mov_menos2 < 61, AAHO_SAL_MINIMO_menos2,
                                                           ifelse(Dias_ult_mov_menos3 < 61, AAHO_SAL_MINIMO_menos3,
                                                                  ifelse(Dias_ult_mov_menos4 < 61, AAHO_SAL_MINIMO_menos4,
                                                                         ifelse(Dias_ult_mov_menos5 < 61, AAHO_SAL_MINIMO_menos5, "Nunca fue productivo")))))]
  
  Ahorros[, AAHO_SAL_DISPONIB_siendoproductivo := ifelse(Dias_ult_mov_menos1 < 61, AAHO_SAL_DISPONIB_menos1, 
                                                    ifelse(Dias_ult_mov_menos2 < 61, AAHO_SAL_DISPONIB_menos2,
                                                           ifelse(Dias_ult_mov_menos3 < 61, AAHO_SAL_DISPONIB_menos3,
                                                                  ifelse(Dias_ult_mov_menos4 < 61, AAHO_SAL_DISPONIB_menos4,
                                                                         ifelse(Dias_ult_mov_menos5 < 61, AAHO_SAL_DISPONIB_menos5, "Nunca fue productivo")))))]
  
  Ahorros[, AAHO_PRO_MES_siendoproductivo := ifelse(Dias_ult_mov_menos1 < 61, AAHO_PRO_MES_menos1, 
                                                         ifelse(Dias_ult_mov_menos2 < 61, AAHO_PRO_MES_menos2,
                                                                ifelse(Dias_ult_mov_menos3 < 61, AAHO_PRO_MES_menos3,
                                                                       ifelse(Dias_ult_mov_menos4 < 61, AAHO_PRO_MES_menos4,
                                                                              ifelse(Dias_ult_mov_menos5 < 61, AAHO_PRO_MES_menos5, "Nunca fue productivo")))))]
  
  Ahorros$AAHO_SAL_HOY_siendoproductivo <- as.numeric(Ahorros$AAHO_SAL_HOY_siendoproductivo)
  Ahorros$AAHO_SAL_MINIMO_siendoproductivo <- as.numeric(Ahorros$AAHO_SAL_MINIMO_siendoproductivo)
  Ahorros$AAHO_SAL_DISPONIB_siendoproductivo <- as.numeric(Ahorros$AAHO_SAL_DISPONIB_siendoproductivo)
  Ahorros$AAHO_PRO_MES_siendoproductivo <- as.numeric(Ahorros$AAHO_PRO_MES_siendoproductivo)
  
  return(Ahorros)
}


#########################################################################################################################
#########################################################################################################################

### GRAFICAS PARA DESCRIPTIVAS ###

#' Grafica de barras para los ESTADOS
#'
#' @param Ahorros La tabla principal.
#' @param Subtitulo Parámetro que dice a qué mes corresponde la gráfica. Ya viene creado en la parametrización.
#'
#' @return Objeto tipo ggplot con la gráfica especificada.
#'
#' @examples
#' Barras_ESTADO(Ahorros = Ahorros, Subtitulo = Subtitulo)
#' 
Barras_ESTADO <- function(Ahorros, Subtitulo) {
  
  vjust_text <- rep(-1, times = length(unique(Ahorros$ESTADO)))
  vjust_text[1] <- 3.5
  hjust_text <- rep(0.5, times = length(unique(Ahorros$ESTADO)))
  hjust_text[length(hjust_text)] <- 0.8
  
  color_text <- rep("black", times = length(unique(Ahorros$ESTADO)))
  color_text[1] <- "white"
  color_text2 <- color_text
  
  Subtitle <- paste0("Total de Cuentas de Ahorros: ", format(nrow(Ahorros), big.mark = ","), "\n ", Subtitulo)
  
  plot <- Grafica_barras(Data = Ahorros, 
                   Variable = "ESTADO", 
                   Title = "Distribucion de Estados en Cuenta de Ahorros",
                   Frecuencia = "Absoluta_Relativa",
                   Caption = "\n Nota: Se excluyen las cuentas Embargadas y Congeladas, \n ademas de las cuentas Cafeteras/Agremiadas.",
                   hjust_caption = 1, 
                   vjust_text = vjust_text,
                   hjust_text = hjust_text,
                   color_text = color_text,
                   color_text2 = color_text2,
                   size_title = 15,
                   Subtitle = Subtitle)
  return(plot)
}


#' Grafica de barras para los Estados de Inactividad
#'
#' @param Ahorros La tabla principal.
#' @param Subtitulo Parámetro que dice a qué mes corresponde la gráfica. Ya viene creado en la parametrización.
#'
#' @return Objeto tipo ggplot con la gráfica especificada.
#'
#' @examples
#' Barras_inactividad(Ahorros = Ahorros, Subtitulo = Subtitulo)
#' 
Barras_inactividad <- function(Ahorros, Subtitulo) {
  
  vjust_text <- rep(-1, times = length(unique(Ahorros$Dias_ult_mov_cat)))
  vjust_text[1] <- 3.5
  hjust_text <- rep(0.5, times = length(unique(Ahorros$Dias_ult_mov_cat)))
  hjust_text[length(hjust_text)] <- 0.6
  
  color_text <- rep("black", times = length(unique(Ahorros$Dias_ult_mov_cat)))
  color_text[1] <- "white"
  color_text2 <- color_text
  
  Subtitle <- paste0("Total de Cuentas de Ahorros: ", format(nrow(Ahorros), big.mark = ","), "\n ", Subtitulo)
  
  positions <- factor(sort(unique(Ahorros$Dias_ult_mov_cat)))
  
  plot <- Grafica_barras(Data = Ahorros, 
                         Variable = "Dias_ult_mov_cat", 
                         Title = "Distribucion de dias de inactividad en Cuenta de Ahorros",
                         Caption = "\n Nota: Se excluyen las cuentas Embargadas y Congeladas, \n ademas de las cuentas Cafeteras/Agremiadas.",
                         hjust_caption = 1,
                         Frecuencia = "Absoluta_Relativa",
                         vjust_text = vjust_text,
                         hjust_text = hjust_text,
                         color_text = color_text,
                         color_text2 = color_text2,
                         size_title = 15,
                         Subtitle = Subtitle) + scale_x_discrete(limits = positions)
  
  return(plot)
}


#' Grafica de barras para los Estados de Antiguedad
#'
#' @param Ahorros La tabla principal.
#' @param Subtitulo Parámetro que dice a qué mes corresponde la gráfica. Ya viene creado en la parametrización.
#'
#' @return Objeto tipo ggplot con la gráfica especificada.
#'
#' @examples
#' Barras_antiguedad(Ahorros = Ahorros, Subtitulo = Subtitulo)
#' 
Barras_antiguedad <- function(Ahorros, Subtitulo) {
  
  vjust_text <- rep(-1, times = length(unique(Ahorros$Antiguedad_categoria)))
  vjust_text[1] <- 3.5
  #hjust_text <- rep(0.5, times = length(unique(Ahorros$ESTADO)))
  #hjust_text[length(hjust_text)] <- 0.8
  
  color_text <- rep("black", times = length(unique(Ahorros$Antiguedad_categoria)))
  color_text[1] <- "white"
  color_text2 <- color_text
  
  Subtitle <- paste0("Total de Cuentas de Ahorros: ", format(nrow(Ahorros), big.mark = ","), "\n ", Subtitulo)
  
  positions <- factor(sort(unique(Ahorros$Antiguedad_categoria)))
  
  plot <-  Grafica_barras(Data = Ahorros, 
                          Variable = "Antiguedad_categoria", 
                          Title = "Distribucion de Antiguedad en años",
                          Caption = "\n Nota: Se excluyen las cuentas Embargadas y Congeladas, \n ademas de las cuentas Cafeteras/Agremiadas.",
                          hjust_caption = 1,
                          Frecuencia = "Absoluta_Relativa",
                          vjust_text = vjust_text,
                          #hjust_text = hjust_text,
                          size_text = 4,
                          size_text2 = 4,
                          color_text = color_text,
                          color_text2 = color_text2,
                          size_title = 15,
                          Subtitle = Subtitle) + scale_x_discrete(limits = positions)
  
  return(plot)
}


#' Tabla con la distribucion acumulada del saldo promedio mensual
#'
#' @param Ahorros La tabla principal.
#'
#' @return Tabla especificada lista para ser mostrada en imagen.
#'
#' @examples
#' Tabla_saldo_promedio(Ahorros = Ahorros)
#' 
Tabla_saldo_promedio <- function(Ahorros) {

  cortes <- c(0, 1, 100, 1000, 10000, 100000, 1000000, 10000000)
  porcentajes <- c()
  for (i in 1:length(cortes)){
    porcentajes[i] <- sum(Ahorros$AAHO_PRO_MES < cortes[i])/nrow(Ahorros)
  }
  DT <- data.table(cortes, porcentajes)[order(cortes)]
  DT$cortes <- format(DT$cortes, big.mark = ",")
  DT$porcentajes <- paste0(format(DT$porcentajes*100, digits = 0), "%")
  names(DT) <- c("Saldo promedio mensual", "Porcentaje de clientes con menos de ese saldo")
  
  return(grid.table(DT, rows = NULL))
}


#' Tabla con la distribucion acumulada del saldo promedio mensual - para Cuentas Activas
#'
#' @param Ahorros La tabla principal.
#'
#' @return Tabla especificada lista para ser mostrada en imagen.
#'
#' @examples
#' Tabla_saldo_promedio(Ahorros = Ahorros)
#' 
Tabla_saldo_promedio_activos <- function(Ahorros) {
  
  cortes <- c(0, 1, 100, 1000, 10000, 100000, 1000000, 10000000)
  porcentajes <- c()
  for (i in 1:length(cortes)){
    porcentajes[i] <- sum(Ahorros[Dias_ult_mov < 61]$AAHO_PRO_MES < cortes[i])/nrow(Ahorros[Dias_ult_mov < 61])
  }
  DT <- data.table(cortes, porcentajes)[order(cortes)]
  DT$cortes <- format(DT$cortes, big.mark = ",")
  DT$porcentajes <- paste0(format(DT$porcentajes*100, digits = 0), "%")
  names(DT) <- c("Saldo promedio mensual", "Porcentaje de clientes con menos de ese saldo \n - Cuentas Activas")
  
  return(grid.table(DT, rows = NULL))
}



#########################################################################################################################
#########################################################################################################################

### FUNCIONES RELACIONADAS AL MODELO ###


#' Separacion de la data en Training, Validation y Test
#'
#' @param Ahorros La tabla a utilizar para el modelo.
#' @param variables_seleccionadas Las variables que en la parametrizacion de Seleccion_modelo, van a ser las explicativas.
#' @param porc_training Que porcentaje de los registros van a training. 0.7 esta por default.
#' @param porc_validation_restante De las observaciones restantes (no training), que porcentaje van a validation. 
#' Ejemplo: si porc_training es 0.7 y porc_validation_restante es 0.5, significa que del 30% que no es training,
#' la mitad va a validation. Los porcentajes serian 70-15-15.
#'
#' @return Lista de tres objetos: la tabla de training, la de validation y la de test.
#'
#' @examples
#' Tablas_separadas <- Training_validation_test(Ahorros = Ahorros, variables_seleccionadas = variables_seleccionadas)
#' Training <- Training_validation_test(Ahorros = Ahorros, variables_seleccionadas = variables_seleccionadas, porc_training = 0.8)$Training
#' 
Training_validation_test <- function(Ahorros, variables_seleccionadas, porc_training = 0.7, porc_validation_restante = 0.5) {
  
  set.seed(1905)
  
  variable_respuesta_num <- Ahorros$variable_respuesta_num
  Features <- Ahorros[, mget(variables_seleccionadas)]
  
  Ahorros <- data.table(variable_respuesta_num, Features)
  
  ID_training <- sort(sample(nrow(Ahorros), nrow(Ahorros)*porc_training))
  Training <- Ahorros[ID_training]
  Otros <- Ahorros[-ID_training]
  
  ID_validation <- sort(sample(nrow(Otros), nrow(Otros)*porc_validation_restante))
  Validation <- Otros[ID_validation]
  
  if (porc_validation_restante == 1){
    Test <- NULL
  } else {
    Test <- Otros[-ID_validation]
  }
  
  
  if(porc_validation_restante < 1) {
    a <- nrow(Test)
  } else if (porc_validation_restante == 1) {
    a <- 0
  } else {
    stop("La parametrizacion de porc_validation_restante no es correcta.") }
    # Si se pone en la primera linea de validacion "nrow(Test)" cuando Test es NULL, salta error porque no tiene numero de filas.
  
  #Validacion
  if(nrow(Training) + nrow(Validation) + a != nrow(Ahorros)) {
    stop("Hay un error en la separacion de data.")
  }
  
  return(list(Training = Training, Validation = Validation, Test = Test))
  
}



#' Extraer imagen con el Feature Importance
#'
#' @param model modelo tipo xgboost 
#'
#' @return exporta la imagen mencionada
#'
#' @examples
#' extraer_feature_importance(model = model)
#' 
extraer_feature_importance <- function(model) {
  
  cols <- model$feature_names
  importance_matrix <-
    xgb.importance(feature_names = cols, model = model)
  
  plot <- xgb.ggplot.importance(importance_matrix, top_n = 20)
  plot <- plot + 
    labs(title = "Importancia de los features")+
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste0(getwd(), "/Graficas/", modelo, "/Feature_importance.png"), plot = plot, width=10, height=5)
  
  print("Se ha exportado la grafica de Feature Importance.")
  
}



#' Extraer grafica del ROC relacionado a Test, con AUC graficado
#'
#' @param pgorro vector de probabilidades predichas para Test.
#' @param Test vector de realizaciones del proceso de la variable dependiente para Test.
#'
#' @return En la ruta, carpeta "Graficas/modelo_#", se presenta la grafica mencionada.
#'
#' @examples
#' Extraer_ROC(pgorro = pgorro, Test = Test)
#' 
Extraer_ROC <- function(pgorro, Test) {
  
  ROCRpred <- prediction(pgorro, Test$variable_respuesta_num)
  ROCRperf <- performance(ROCRpred, 'tpr','fpr')
  auc_ROCR <- performance(ROCRpred, measure = "auc")
  auc_ROCR <- auc_ROCR@y.values[[1]]
  
  png(paste0(getwd(), "/Graficas/", modelo, "/ROC.png"))
  plot(ROCRperf, colorize = F, text.adj = c(-0.2,1.7), xlab = "1 - Specificity", ylab = "Recall") 
  legend(0.2, 0.6, paste0("AUC = ", format(auc_ROCR, digits = 4)), border="white", cex=1, box.col = "white")
  dev.off()
  
  print("Se ha exportado la grafica del ROC.")
  
}




#' Exporta tabla con matriz de confusion
#'
#' @param predict vector de probabilidades predichas.
#' @param response vector con realizaciones. Ojo: no la tabla, sino el vector.
#' @param threshold debe ser un valor entre 0 y 1. Define con que probabilidad es el limite entre decir que una probabilidad 
#' es mas cercana a 0 o 1. Si no se pone nada, se asume el thresold optimo.
#' Esto afecta a la matriz de confusion principalmente.
#'
#' @return Regresa la matrix.
#'
#' @examples
#' conf_matrix <- Confusion_matrix(predict = pgorro, response = Test$variable_respuesta_num)
#' conf_matrix <- Confusion_matrix(predict = pgorro, response = Test$variable_respuesta_num, threshold = 0.1)
#' 
Confusion_matrix <- function(predict, response, threshold = "opt_threshold") {
  
  perf <- ROCR::performance(ROCR::prediction(predict, response), "sens", "spec")
  df <- data.frame(cut = perf@alpha.values[[1]], sens = perf@x.values[[1]], spec = perf@y.values[[1]])

  if(threshold == "opt_threshold"){
  opt_threshold <- df[which.max(df$sens + df$spec), "cut"]
  #Exportar el umbral optimo calculado
  write.csv(opt_threshold, file = paste0(getwd(), "/Graficas/", modelo, "/opt_threshold.csv"), row.names = F)
  print("Se ha exportado el umbral optimo en la ruta del modelo.")
  } else {
  opt_threshold <- threshold
  }
  conf <- confusion.matrix(obs = response, pred = predict, threshold = opt_threshold)
  colnames(conf) <- c("Obs 0", "Obs 1")
  rownames(conf) <- c("Pred 0", "Pred 1")
  
  return(conf)
}


#' Devuelve el uplift relacionado a un percentil
#'
#' @param conf_matrix matriz de confusion arrojada por el modelo.
#'
#' @return el valor del uplift.
#'
#' @examples
#' Uplft10 <- Uplift(conf_matrix = conf_matrix)
#' 
Uplift <- function(conf_matrix){
  conf <- conf_matrix
  UPLIFT <- (conf[2,2]/(conf[2,2] + conf[2,1]))/((conf[1,2] + conf[2,2])/(conf[1,1] + conf[1,2] + conf[2,1] + conf[2,2]))
  return(UPLIFT)
}


#' Devuelve el recall relacionado a un percentil
#'
#' @param conf_matrix matriz de confusion arrojada por el modelo.
#'
#' @return el valor del recall.
#'
#' @examples
#' Recall10 <- Recall(conf_matrix = conf_matrix)
#' 
Recall <- function(conf_matrix){
  conf <- conf_matrix
  RECALL <- conf[2,2]/(conf[2,2] + conf[1,2])
  return(RECALL)
}


#' Devuelve el precision relacionado a un percentil
#'
#' @param conf_matrix matriz de confusion arrojada por el modelo.
#'
#' @return el valor del precision.
#'
#' @examples
#' Precision10 <- Precision(conf_matrix = conf_matrix)
#' 
Precision <- function(conf_matrix){
  conf <- conf_matrix
  PRECISION <- conf[2,2]/(conf[2,2] + conf[2,1])
  return(PRECISION)
}


#' Exporta grafica con Uplift para diferentes percentiles
#'
#' @param pgorro vector con probabilidades predichas.
#' @param Test la tabla de Test.
#' @param percentiles los percentiles se quieren graficar. Estan por default todos los deciles.
#' @param modelo que modelo se esta corriendo. Solo por razones esteticas
#'
#' @return Exporta la grafica mencionada.
#'
#' @examples
#' Grafica_uplift(pgorro = pgorro, conf_matrix = conf_matrix, Test = Test)
#' 
Grafica_uplift <- function(pgorro, Test, percentiles = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), modelo) {
  
  Tabla_uplift <- matrix(NA, nrow = length(percentiles), ncol = 2)
  Tabla_uplift[, 1] <- percentiles*100
  colnames(Tabla_uplift) <- c("Percentil", "Uplift")
  
  for (i in 1:length(percentiles)){
    Tabla_uplift[i, 2] <- Uplift(conf_matrix = Confusion_matrix(predict = pgorro, 
                                                                response = Test$variable_respuesta_num, 
                                                                threshold = quantile(pgorro, probs = 1 - percentiles[i])))
  }
  
  Tabla_uplift <- data.table(Tabla_uplift)
  
  if (modelo == "modelo_2"){
    vjust_aux <- c(1.5, rep(1.5, times = length(percentiles) - 1))
    color_aux <- c("white", rep("white", times = length(percentiles) - 1))
  } else {
    vjust_aux <- c(1.5, rep(-1, times = length(percentiles) - 1))
    color_aux <- c("white", rep("black", times = length(percentiles) - 1))
  }
  
  plot <- ggplot(data=Tabla_uplift, aes(x= Percentil, y=Uplift, fill = -Uplift)) +
    geom_bar(stat="identity")+
    theme(plot.title = element_text(hjust = 0.5, size = 15), legend.position = "none",
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          plot.caption = element_text(size = 12)) + 
    labs(title = "Uplift para cada decil de probabilidades",
         caption = "\n Uplift: el modelo cuantas veces mejoran el Recall y Precision, con respecto a una seleccion aleatoria") + scale_x_continuous(breaks = percentiles*100) +
      geom_text(label = round(Tabla_uplift$Uplift, digits = 2) , size = 7, vjust = vjust_aux, color = color_aux, hjust = 0.5)
 
  
  ggsave(paste0(getwd(), "/Graficas/", modelo, "/Uplift.png"), plot = plot, width=10, height=5)  
  
  print("Se ha exportado la grafica del Uplift.")
}


#' Exporta grafica con Recall para diferentes percentiles
#'
#' @param pgorro vector con probabilidades predichas.
#' @param Test la tabla de Test.
#' @param percentiles los percentiles se quieren graficar. Estan por default todos los deciles.
#'
#' @return Exporta la grafica mencionada.
#'
#' @examples
#' Grafica_recall(pgorro = pgorro, conf_matrix = conf_matrix, Test = Test)
#' 
Grafica_recall <- function(pgorro, Test, percentiles = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
  
  Tabla_recall <- matrix(NA, nrow = length(percentiles), ncol = 2)
  Tabla_recall[, 1] <- percentiles*100
  colnames(Tabla_recall) <- c("Percentil", "Recall")
  
  for (i in 1:length(percentiles)){
    Tabla_recall[i, 2] <- Recall(conf_matrix = Confusion_matrix(predict = pgorro, 
                                                                response = Test$variable_respuesta_num, 
                                                                threshold = quantile(pgorro, probs = 1 - percentiles[i])))
  }
  
  Tabla_recall <- data.table(Tabla_recall)
  plot <- ggplot(data=Tabla_recall, aes(x= Percentil, y=Recall, fill = -Recall)) +
    geom_bar(stat="identity")+
    geom_text(label = paste0(round(Tabla_recall$Recall*100, digits = 0), "%") , size = 7, vjust = c(1.5, rep(1.5, times = length(percentiles) - 1)), color = c("white", rep("white", times = length(percentiles) - 1)), hjust = 0.5) + 
    theme(plot.title = element_text(hjust = 0.5, size = 15), legend.position = "none",
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          plot.caption = element_text(size = 12)) + 
    labs(title = "Recall para cada decil de probabilidades",
         caption = "\n Recall: qué porcentaje de los Observados Positivos son identificados por el modelo") + scale_x_continuous(breaks = percentiles*100) + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  ggsave(paste0(getwd(), "/Graficas/", modelo, "/Recall.png"), plot = plot, width=10, height=5)  
  
  print("Se ha exportado la grafica del Recall.")
}



#' Exporta grafica con Precision para diferentes percentiles
#'
#' @param pgorro vector con probabilidades predichas.
#' @param Test la tabla de Test.
#' @param percentiles los percentiles se quieren graficar. Estan por default todos los deciles.
#' @param modelo que modelo se esta corriendo. Solo por razones esteticas
#'
#' @return Exporta la grafica mencionada.
#'
#' @examples
#' Grafica_precision(pgorro = pgorro, conf_matrix = conf_matrix, Test = Test)
#' 
Grafica_precision <- function(pgorro, Test, percentiles = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), modelo) {
  
  Tabla_precision <- matrix(NA, nrow = length(percentiles), ncol = 2)
  Tabla_precision[, 1] <- percentiles*100
  colnames(Tabla_precision) <- c("Percentil", "Precision")
  
  for (i in 1:length(percentiles)){
    Tabla_precision[i, 2] <- Precision(conf_matrix = Confusion_matrix(predict = pgorro, 
                                                                response = Test$variable_respuesta_num, 
                                                                threshold = quantile(pgorro, probs = 1 - percentiles[i])))
  }
  
  Tabla_precision <- data.table(Tabla_precision)
  
  if (modelo == "modelo_2"){
    vjust_aux <- c(1.5, rep(1.5, times = length(percentiles) - 1))
    color_aux <- c("white", rep("white", times = length(percentiles) - 1))
  } else {
    vjust_aux <- c(1.5, rep(-1, times = length(percentiles) - 1))
    color_aux <- c("white", rep("black", times = length(percentiles) - 1))
  }
  
  plot <- ggplot(data=Tabla_precision, aes(x= Percentil, y=Precision, fill = -Precision)) +
    geom_bar(stat="identity")+
    geom_text(label = paste0(round(Tabla_precision$Precision*100, digits = 0), "%") , size = 7, vjust = vjust_aux, color = color_aux, hjust = 0.5) + 
    theme(plot.title = element_text(hjust = 0.5, size = 15), legend.position = "none",
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          plot.caption = element_text(size = 12)) + 
    labs(title = "Precision para cada decil de probabilidades",
         caption = "\n Precision: qué porcentaje de los que el modelo clasifica como positivos, efectivamente lo son.") + scale_x_continuous(breaks = percentiles*100) + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  ggsave(paste0(getwd(), "/Graficas/", modelo, "/Precision.png"), plot = plot, width=10, height=5)  
  
  print("Se ha exportado la grafica del Precision.")
}



#############################################################################################################
#############################################################################################################

#' Calculo del indice de estabilidad de una variable
#'
#' @param data tabla (data frame) con la informacion de la variable para los dos grupos
#' @param bench poblacion 1
#' @param target poblacion 2
#' @param bin numero de grupos. Default: 10.
#'
#' @return el indice de estabilidad mencionado
#' Si PSI < 0.1 entonces el modelo es bueno y presenta estabilidad en relación al estudio
#' con la póblación de entrenamiento o base
#' Si el PSI > 0.1 y < 0.25 entonces estabilidad media 
#' Si el PSI > 0.25 entonces no hay estabilidad
#'
#' @examples
#' indice <- cal_psi(data = data, bench = "mes1", target = "mes2")
#' 
cal_psi <- function(data, bench, target, bin = 10) {

  ben<-sort(data[,bench])
  tar<-sort(data[,target])
  # get and sort benchmark and target variable
  ttl_bench<-length(tar)
  ttl_target<-length(ben)
  # get total num obs for benchmark and target
  n<-ttl_bench%/%bin #Num of obs per bin
  psi_bin<-rep(0,times=bin) #initialize PSI=0 for each bin
  
  for (i in 1:bin) # calculate PSI for ith bin
  {
    
    lower_cut<-ben[(i-1)*n+1];
    if(i!=bin){upper_cut<-ben[(i-1)*n+n]; pct_ben<-n/ttl_bench} else
    {upper_cut<-ben[ttl_bench];
    pct_ben<(ttl_bench-n*(bin-1))/ttl_bench}
    #last bin should have all remaining obs
    
    pct_tar<-length(tar[tar>lower_cut&tar<=upper_cut])/ttl_target
    psi_bin[i]<-(pct_tar-pct_ben)*log(pct_tar/pct_ben)
  }

  psi <- sum(psi_bin)

  return(psi)
}


