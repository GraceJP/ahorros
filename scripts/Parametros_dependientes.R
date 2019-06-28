##################################################################
#   Parametros que dependen de los hiperparametros - esteticos   #
##################################################################

meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
Subtitulo <- paste0(meses[as.numeric(substr(Mes, 6, 7))], " de ", substr(Mes, 1, 4))
#Subtitulo_score <<- paste0(meses[as.numeric(substr(Mes_score, 6, 7))], " de ", substr(Mes_score, 1, 4))
  
Cierres <- c(31,        28     ,  31    ,   30   ,  31   ,  30   ,   31   ,   31     ,     30        ,   31     ,   30        , 31)
if (as.numeric(substr(Mes, 1, 4)) %% 4 == 0) { Cierres[2] <- 29}
Cierre_mes <- as.Date(paste0(substr(Mes, 1, 4), "/", substr(Mes, 6, 7), "/", Cierres[as.numeric(substr(Mes, 6, 7))]))
  
Mes_futuro <- Mes_adelante_n(Mes, Meses_adelante)
  
Cierres_futuro <- Cierres
if (as.numeric(substr(Mes_futuro, 1, 4)) %% 4 == 0) { Cierres_futuro[2] <- 29}

#Parametro que dice cuales son los estados relacionados a 1

if (length(variable_respuesta[variable_respuesta_num == 1]) == 1){
  Nombre1 <- variable_respuesta[variable_respuesta_num == 1]
} else if (length(variable_respuesta[variable_respuesta_num == 1]) == 2) {
  aux <- c(variable_respuesta[variable_respuesta_num == 1])
  Nombre1 <- paste0(aux[1], " o ", aux[2])
  rm(aux)
} else {
  stop("Revise la parametrizacion del modelo. Hay mas de 2 estados futuros marcados como 1 en la variable respuesta.")
}
