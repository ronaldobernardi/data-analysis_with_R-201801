library(tidyverse)
library(lubridate)

## Nesta atividade você deve utilizar o resultado do exercício 01 da Atividade da aula 03 (remuneração em dólares convertida para reais)
## Utilize o código daquele exercício como ponto de partida para esta atividade. 
## Sempre utilize o caminho relativo, não o caminho absoluto, pois não funcionará na correção do exercício.

### IMPORTANTE ###
## Se você utilizar alguma função própria ou do material de aula, o código da(s) função(ões) deve estar neste arquivo da atividade.

salarios <- read_csv("aula-03/data/201802_dados_salarios_servidores.csv.gz")

cotacao_dolar <- 3.2428

salarios %>%
  mutate(REMUNERACAO_FINAL = REMUNERACAO_REAIS + (REMUNERACAO_DOLARES * cotacao_dolar)) %>%
  filter(REMUNERACAO_FINAL >= 900) -> salarios

### 1 ####
## 
## Correlação de ano de ingresso por cargo
## - Determine o coeficiente de correlação entre o tempo em anos desde a DATA_INGRESSO_ORGAO e o tempo em anos desde a DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO
##   para todos os cargos que possuem no mínimo 200 servidores.
## - Crie uma coluna que determina se a correlação é positiva ou negativa, e outra coluna que define a força da correlação de acordo com 
##   o material visto em aula sobre interpretação do coeficiente.
## - O resultado desta atividade deve ser um Data Frame com as variáveis de Cargo, Coeficiente de Correlação, Direção da Correlação e Força da Correlação
## 
### # ####

salarios %>%
  group_by(DESCRICAO_CARGO) %>%
  summarise(qtd = n()) %>%
  filter(qtd >= 200) %>%
  ungroup() %>%
  pull(DESCRICAO_CARGO) -> cargos_acima_200

salarios %>%
  filter(DESCRICAO_CARGO %in% cargos_acima_200) %>%
  group_by(DESCRICAO_CARGO) %>%
  summarise(COEFICIENTE_CORRELACAO = cor(x = year(DATA_INGRESSO_ORGAO), y = year(DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO ))) %>%
  ungroup() %>%
  mutate(CORRELACAO_DIRECAO = if_else(sign(COEFICIENTE_CORRELACAO) == 1, 'POSITIVA', 'NEGATIVA'),
         CORRELACAO_FORCA = case_when(abs(COEFICIENTE_CORRELACAO) >= 0.0 & abs(COEFICIENTE_CORRELACAO) < 0.3 ~ 'DESPREZIVEL',
                                      abs(COEFICIENTE_CORRELACAO) >= 0.3 & abs(COEFICIENTE_CORRELACAO) < 0.5 ~ 'FRACA',
                                      abs(COEFICIENTE_CORRELACAO) >= 0.5 & abs(COEFICIENTE_CORRELACAO) < 0.7 ~ 'MODERADA',
                                      abs(COEFICIENTE_CORRELACAO) >= 0.7 & abs(COEFICIENTE_CORRELACAO) < 0.9 ~ 'FORTE',
                                      abs(COEFICIENTE_CORRELACAO) >= 0.9 ~ 'MUITO FORTE')) -> correlacao_ingresso_diploma

### 2 ###
##
## - A partir do dataset do exercício anterior, selecione os 10 cargos de correlação mais forte (seja positiva ou negativa) e os 
##   10 cargos de correlação mais fraca (de novo, independente de ser positiva ou negativa)
## - Para estes 20 cargos, determine a Moda do órgão de lotação (ORGSUP_LOTACAO) e de exercício (ORGSUP_EXERCICIO)
## - Reponda se existe diferença entre as modas e se existe relação entre a Força da Correlação e a diferença entre as modas 
##   (caso haja diferença)
##
### # ###

rbind(
  correlacao_ingresso_diploma %>%
    filter(CORRELACAO_FORCA == 'DESPREZIVEL') %>%
    arrange(COEFICIENTE_CORRELACAO) %>%
    head(COEFICIENTE_CORRELACAO, n = 10),
  
  correlacao_ingresso_diploma %>%
    filter(CORRELACAO_FORCA == 'MUITO FORTE') %>%
    arrange(desc(COEFICIENTE_CORRELACAO)) %>%
    head(COEFICIENTE_CORRELACAO, n = 10)
) -> correlacao_max_min

salarios %>%
  filter(DESCRICAO_CARGO %in% correlacao_max_min$DESCRICAO_CARGO) %>%
  group_by(DESCRICAO_CARGO, ORGSUP_EXERCICIO) %>%
  summarise(qtd = n()) %>%
  mutate(qtd_max = max(qtd)) %>%
  ungroup() %>%
  filter(qtd_max == qtd) %>%
  transmute(DESCRICAO_CARGO, ORGSUP_EXERCICIO, moda_exercicio = qtd_max)

salarios %>%
  filter(DESCRICAO_CARGO %in% correlacao_max_min$DESCRICAO_CARGO) %>%
  group_by(DESCRICAO_CARGO, ORGSUP_LOTACAO) %>%
  summarise(qtd = n()) %>%
  mutate(qtd_max = max(qtd)) %>%
  ungroup() %>%
  filter(qtd_max == qtd) %>%
  transmute(DESCRICAO_CARGO, ORGSUP_LOTACAO, moda_lotacao = qtd_max)
