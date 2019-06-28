
ggplot(Tabla2, aes(x = AAHO_PRO_MES, y = ganancia_esperada)) + geom_point(alpha = 0.1) +
  scale_x_continuous(labels = scales::comma) + 
  scale_y_continuous(labels = scales::comma)

lm(data = Tabla2, formula = ganancia_esperada ~ AAHO_PRO_MES)
#Coefficients:
#  (Intercept)  AAHO_PRO_MES  
#-54639.93972       0.07563 



nrow(Tabla2)
#245,470

(12915809656+23507761871+34137869600+85256708050)/245470
#634,775

summary(Tabla2$AAHO_PRO_MES)
#Min.    1st Qu.     Median       Mean             3rd Qu.            Max. 
#35,176     556,472    2,050,890    9,115,310    7,620,999    9,898,234,881 

summary(Tabla2$ganancia_esperada)
#Min.    1st Qu.     Median            Mean        3rd Qu.           Max. 
#25,872      48,146     105,249     634,775       340,132        1,451,099,911 


(634775/9115310)/0.167
#0.416996

mean(Tabla2$prob_churn)
#0.1701056



quantile(Tabla2$ganancia_esperada, probs = 0.99)
#8,607,005 
sum(Tabla2$ganancia_esperada >= quantile(Tabla2$ganancia_esperada, probs = 0.99))
#2,455

8607005*2455
#21,130,197,275
21130197275/(12915809656+23507761871+34137869600+85256708050)
#0.1356081

mean(Tabla2[ganancia_esperada >= quantile(Tabla2$ganancia_esperada, probs = 0.99)]$prob_churn)
#0.2893316




quantile(Tabla2$ganancia_esperada, probs = 0.995)
#13,240,663
sum(Tabla2$ganancia_esperada >= quantile(Tabla2$ganancia_esperada, probs = 0.995))
#1,228

13240663*1228
#16,259,534,164
16259534164/(12915809656+23507761871+34137869600+85256708050)
#0.1043494

mean(Tabla2[ganancia_esperada >= quantile(Tabla2$ganancia_esperada, probs = 0.995)]$prob_churn)
#0.295178




quantile(Tabla2$ganancia_esperada, probs = 0.999)
#34,194,354
sum(Tabla2$ganancia_esperada >= quantile(Tabla2$ganancia_esperada, probs = 0.999))
#246

34194354*246
#8,411,811,084
8411811084/(12915809656+23507761871+34137869600+85256708050)
#0.0539848

mean(Tabla2[ganancia_esperada >= quantile(Tabla2$ganancia_esperada, probs = 0.999)]$prob_churn)
#0.2974119




quantile(Tabla2$ganancia_esperada, probs = 0.9999)
#125,378,601
sum(Tabla2$ganancia_esperada >= quantile(Tabla2$ganancia_esperada, probs = 0.9999))
#25

25*125378601
#3,134,465,025
3134465025/(12915809656+23507761871+34137869600+85256708050)
#0.02011617

mean(Tabla2[ganancia_esperada >= quantile(Tabla2$ganancia_esperada, probs = 0.9999)]$prob_churn)
#0.2988308





Tabla2[order(-ganancia_esperada), ganancia_esperada][1:25]
#1,451,099,911  
#907,825,227  
#788,573991  
#727,549641  
#527,083240  
#417,297275  
#295,041997  
#287,055966  
#285,761074  
#285,118528  
#253,309126
#212,083559  
#209,471584  
#197,809291  
#175,172045  
#164,642696  
#164,639864  
#159,062101  
#149,444427  
#142,323275  
#140,804965  
#132,539615
#132,358306  
#130,667373  
#127,320744


