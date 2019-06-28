### DEFENSA DEL MODELO

memory.limit(size = 25000) #Ayudar a la memoria
rm(list = ls()) #Borrar ambiente
options(scipen=999) #Quitar notacion cientifica
gc() #Limpieza de RAM
cat("\014") #Limpieza de consola
#.rs.restartR()

source("Z:/personales/Santiago/Funciones.R")
cargar_librerias() #Cargue de paquetes necesarios

load(file = "C:/Users/smunoz6/Desktop/Modelo1.RData")

#######################################################################

Test[, pgorro := pgorro]
Test[, Dias_ult_mov_aux := ifelse(Dias_ult_mov > 30, "Mayor", "Menor")]

#Distribucion de dias desde el ultimo movimiento
Test[, .N, by = Dias_ult_mov_aux]
#Distribucion de target para menores de un mes
Test[Dias_ult_mov_aux == "Menor", .N, by = variable_respuesta_num]
#Distribucion de target para mayores de un mes
Test[Dias_ult_mov_aux == "Mayor", .N, by = variable_respuesta_num]

#Que tan bien discriminan las probabilidades para los que llevan menos de un mes inactivos?
ggplot(Test[Dias_ult_mov_aux == "Menor"], aes(x = pgorro, y = factor(variable_respuesta_num))) + 
  geom_vline(xintercept = quantile(Test[Dias_ult_mov_aux == "Menor"]$pgorro, probs = 0.9), color = "red") + 
  ggridges::geom_density_ridges() +
  xlab("Probabilidad predicha") +
  ylab("Target (rodamiento)") +
  labs(title = "Distribución de Probabilidad estimada",
       subtitle = "Por valores observado de la target, solo para cuentas con menos de un mes de inactividad",
       caption = "La linea roja representa el percentil 90 de las probabilidades predichas, para las cuentas con menos de un mes de inactividad") +
  theme(plot.caption = element_text(size = 15),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        plot.title = element_text(size = 15, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))



##########################################################################

