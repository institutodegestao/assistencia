library(dplyr) # comandos pip
library(DT) # tabelas dinâmicas
library(flexdashboard) # dashboard
library(formattable) # formatações
library(gplots) # gráficos
library(lubridate) # datas
library(plotly) # gráficos
library(reshape2) # transformação de dados
library(hms) # tratamento de horas
library(zoo)

# sessionInfo()

### base 1 = leito_motivo
leito_motivo <- read.csv2('bases/leito_motivo.csv', sep = ';', encoding = 'UTF-8') # carga

# deixar os nomes das colunas mais amigáveis para o R
colnames(leito_motivo) <- c('cod_executante', 'motivo_alteracao_leito','status_entrada_alteracao', 'status_saida_alteracao', 'desc_saida_alteracao', 'data_alteracao', 'hora_alteracao', 'num_leitos_movimentados', 'cod_leito', 'cod_reg_pmr','nome_leito','tp_leito_executante', 'espec_leito_executante', 'sexo_leito_executante', 'macroreg_executante', 'geres_executante', 'mun_executante', 'un_executante', 'num_leito') # nomes de colunas

# tratamentos de colunas
leito_motivo$data_alteracao <- as.Date(leito_motivo$data_alteracao, format = "%d/%m/%y")

# calcular data máxima e mínima
max_dt_alt <- max(leito_motivo$data_alteracao)
min_dt_alt <- min(leito_motivo$data_alteracao)

###########################################
### base 2 = historico_solicitacao_nova
###########################################
hist_solic_nova <- read.csv2('bases/historico_solicitacao_nova.csv', sep = ';', encoding = 'UTF-8') # carga

# deixar os nomes das colunas mais amigáveis para o R
colnames(hist_solic_nova) <- c('cod_solicitacao', 'data_solicitacao', 'hora_solicitacao', 'numero_solicitacao', 'status_solicitacao', 'unidade_solicitante', 'prioridade_solicitacao', 'tipo_leito_solicitante', 'tipo_leito_sol2', 'especialidade_leito_solicitante', 'tipo_leito_regulador', 'tipo_leito_reg2', 'especialidade_leito_regulador', 'cont_solicitacao', 'data_informacao_historico', 'data_historico_solicitacao', 'status_historico_solicitacao', 'tempo_status_solicitacao', 'posicao_fila_espera', 'cont_histotico', 'cont_lista', 'cont_lista_dia', 'macro2',	'geres2',	'municipio2')# nomes de colunas

# tratamentos de colunas
hist_solic_nova$data_solicitacao <- as.Date(hist_solic_nova$data_solicitacao, format = "%d/%m/%y")

#hist_solic_nova$hora_solicitacao <- as.hms(hist_solic_nova$hora_solicitacao)

hist_solic_nova$data_informacao_historico <- as.Date(hist_solic_nova$data_informacao_historico, format = "%d/%m/%y")

hist_solic_nova$data_historico_solicitacao <- as.Date(hist_solic_nova$data_historico_solicitacao, format = "%d/%m/%y")


# calcular data máxima e mínima
max_dt_sol <- max(hist_solic_nova$data_solicitacao)
min_dt_sol <- min(hist_solic_nova$data_solicitacao)

#Acrecenta uma coluna para ajudar na contagem
hist_solic_nova["cont"]=1

#CRIA O DATAFRAME PARA PLOTAR O GRAFICO
grafico_hist_solic_nova = hist_solic_nova[ , c(6,2, 9, 26)]

#AGRUPA O DATAFRAME ANTES DE PIVOTAR PARA WIDE. ASSIM, EVITA ERROS DE DUPLICATAS
grafico_hist_solic_nova <- grafico_hist_solic_nova %>% group_by(unidade_solicitante, data_solicitacao, tipo_leito_sol2) %>% summarise(cont = sum(cont))

###PIVOTA O DATA FRAME PARA FORMATO LONGO, COM BASE NA COLUNA TIPO DE LEITO
grafico <-  grafico_hist_solic_nova %>% tidyr::pivot_wider(names_from = c(tipo_leito_sol2), values_from = cont)

grafico <- grafico %>% replace(is.na(.), 0)
#grafico = dcast(grafico_hist_solic_nova, data_solicitacao ~ tipo_leito_sol2, value.var = "cont")
grafico["TOTAL"] = grafico["ENFERMARIA"] + grafico["UTI"]
#grafico

grafico <- grafico %>%
  mutate(geralMM7 = round(rollmean(x = TOTAL, 7, align = "right", fill = NA),2))

grafico <- grafico %>%
  mutate(enfMM7 = round(rollmean(x = ENFERMARIA, 7, align = "right", fill = NA),2))

grafico <- grafico %>%
  mutate(utiMM7 = round(rollmean(x = UTI, 7, align = "right", fill = NA),2))

grafico <- grafico %>% replace(is.na(.), 0)

##########################
### base 3 = solicitacao
##########################
sol_pac <- read.csv2('bases/solicitacao.csv', sep = ';', encoding = 'UTF-8') # carga

# deixar os nomes das colunas mais amigáveis para o R
colnames(sol_pac) <- c('k_sol', 'k_pac', 'k_censo', 'nro_sol', 'macro', 'geres', 'municipio', 'uni_sol', 'prioridade', 'status_sol', 'tipo_leito', 'tipo2', 'esp_leito', 'data_sol', 'risco', 'sol', 'unidade_int', 'hospital_int', 'macro2', 'geres2', 'municipio2') # nomes de colunas

# tratamentos de colunas
sol_pac$data_sol <- as.Date(sol_pac$data_sol, format = "%d/%m/%y")

# calcular data máxima e mínima
max_dt_sol <- max(sol_pac$data_sol)
min_dt_sol <- min(sol_pac$data_sol)

#LABLES DOS VALUE BOXES
lable_ultima = paste("Solicitacoes em ", format(max_dt_sol, "%d/%m/%Y"))
lable_ultima_uti = paste("Solicitacoes de UTI em ", format(max_dt_sol, "%d/%m/%Y"))
lable_ultima_enf = paste("Solicitacoes de ENFERMARIA em ", format(max_dt_sol, "%d/%m/%Y"))


grafico_sol = sol_pac[ , c(8,12, 14, 16)]

grafico_sol <- grafico_sol %>% group_by(uni_sol, tipo2, data_sol) %>% summarise(sol = sum(sol))

grafico_sol <-  grafico_sol %>% tidyr::pivot_wider(names_from = c(tipo2), values_from = sol)

grafico_sol <- grafico_sol %>% replace(is.na(.), 0)
grafico_sol["TOTAL"] = grafico_sol["ENFERMARIA"] + grafico_sol["UTI"]

# sol_por_data <- grafico_sol %>% group_by(data_sol) %>% summarize(total = sum(TOTAL))

#sol_por_data <- sol_pac %>% group_by(data_sol)  %>% filter(uni_sol == "HOSPITAL OSWALDO CRUZ - RECIFE") %>% summarize(total = sum(sol))

###########################################################################
# #CALCULA A QUANTIDADE DE SOLICITAÇÕES DA ÚLTIMA DATA CADASTRADA
###########################################################################
# sol_ultima_data = hist_solic_nova[hist_solic_nova$data_solicitacao==max_dt_sol,]
# num_sol = as.numeric(nrow(sol_ultima_data))
# num_sol_antes = as.numeric(nrow(hist_solic_nova[hist_solic_nova$data_solicitacao==max_dt_sol-1,]))
# as.character(format(max_dt_sol, "%d/%m/%Y"))
# lable_ultima = paste("Solicitacoes em ", format(max_dt_sol, "%d/%m/%Y"))
# lable_ultima_anterior = paste("Solicitacoes em ", format(max_dt_sol-1, "%d/%m/%Y"))
# #sol_por_data <- hist_solic_nova %>% group_by(data_solicitacao) %>% summarize(total = sum(cont))
# 
# 
# #hist_solic_nova_fila = hist_solic_nova[, c(6,8,9,12,19,2,26)]
# 
# #AGREGA AS DATAS DAS SOLICITAÇOES POR CODIGO E APLICA A FUNCAO MIN PARA TRAZER A POSICAO
# #DA FILA NO 1º DIA DA SOLICITAÇAO
# agrega = aggregate(x=hist_solic_nova$data_informacao_historico, by=list(cod=hist_solic_nova$cod_solicitacao), FUN=min)
# agrega$chave = paste(agrega$cod, agrega$x)
# 
# hist_solic_nova$chave = paste(hist_solic_nova$cod_solicitacao, hist_solic_nova$data_informacao_historico)
# 
# #hist_solic_nova[hist_solic_nova$chave==agrega$chave]
# 
# hist_solic_nova_final = hist_solic_nova %>% filter(chave %in% agrega$chave)
# 
# calculo <- hist_solic_nova_final %>% group_by(data_solicitacao) %>% summarize(totals = round(median(posicao_fila_espera),digits = 0 ))
# calculo
# calculo1 <- hist_solic_nova_final %>% group_by(data_solicitacao, tipo_leito_reg2) %>% summarize(totals = round(median(posicao_fila_espera),digits = 0 ))
# calculo1
# calculo1 %>% filter(tipo_leito_reg2 == "UTI")

#https://github.com/institutodegestao/assistencia.git
