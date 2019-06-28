


output <- output[,
                 .(AAHO_NUM_CUENTA, AAHO_TIP_IDEN, AAHO_NUM_IDEN, AAHO_COD_SUB_PRO, AAHO_PRO_MES, prob_churn, saldo_esperado, Dias_ult_mov, categoria_priorizacion)]

names(output) <- c("num_cuenta_ahorros", "tipo_id_cliente", "num_id_cliente", "cod_subproducto", "saldo_promedio_mensual", "prob_rodamiento", "saldo_esperado", "dias_ultima_tx", "categoria_priorizacion")

output <- output[order(categoria_priorizacion)]

output1 <- output[categoria_priorizacion %in% 1:8]
output2 <- output[categoria_priorizacion %in% 9:16]

write.csv(output1, "Z:/modelos_fuertes/Ahorros/Campanas/recuperacion/201905/recup_201905_categ_1-8.csv", row.names = F)
write.csv(output2, "Z:/modelos_fuertes/Ahorros/Campanas/recuperacion/201905/recup_201905_categ_9-16.csv", row.names = F)
 


