library(tidyverse)
library(lubridate)
library(dplyr)
## Nesta atividade você deve utilizar o resultado do exercício 01 da Atividade da aula 03 (remuneração em dólares convertida para reais)
## Utilize o código daquele exercício como ponto de partida para esta atividade. 
## Sempre utilize o caminho relativo, não o caminho absoluto, pois não funcionará na correção do exercício.

### IMPORTANTE ###
## Se você utilizar alguma função própria ou do material de aula, o código da(s) função(ões) deve estar neste arquivo da atividade.


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
salarios <- read_csv("aula-03/data/201802_dados_salarios_servidores.csv.gz")

cotacaoDolar = 3.24
salarios %>%
  mutate(REMUNERACAO_FINAL = REMUNERACAO_REAIS + (REMUNERACAO_DOLARES * cotacaoDolar)) %>%
  group_by(DESCRICAO_CARGO) %>%
  summarize(qtdServidoresCargo = n()) %>%
  ungroup() %>%
  filter(qtdServidoresCargo > 200) %>%
  pull(DESCRICAO_CARGO) -> cargos_200


salarios %>%
  filter(DESCRICAO_CARGO %in% cargos_200) %>%
  group_by(DESCRICAO_CARGO) %>%
  summarize(correlacao = cor(year(DATA_INGRESSO_ORGAO), year(DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO))) %>%
  ungroup() %>%
  mutate(positivo_negativo = if_else(correlacao > 0,"Positivo","Negativo")
         ,grau_correlacao = case_when(
           abs(correlacao) >= 0.9 ~ "MUITO FORTE",
           abs(correlacao) >= 0.7 ~ "FORTE",
           abs(correlacao) >= 0.5 ~ "MODERADO",
           abs(correlacao) >= 0.3 ~ "FRACO",
           abs(correlacao) >= 0.0 ~ "DESPREZÍVEL"
         )
  ) -> df_Correlacao


### 2 ###
##
## - A partir do dataset do exercício anterior, selecione os 10 cargos de correlação mais forte (seja positiva ou negativa) e os 
##   10 cargos de correlação mais fraca (de novo, independente de ser positiva ou negativa)
## - Para estes 20 cargos, determine a Moda do órgão de lotação (ORGSUP_LOTACAO) e de exercício (ORGSUP_EXERCICIO)
## - Reponda se existe diferença entre as modas e se existe relação entre a Força da Correlação e a diferença entre as modas 
##   (caso haja diferença)
##
### # ###

union(
  df_Correlacao %>%
    arrange(abs(correlacao)) %>%
    head(10) %>% ## 10 mais fracos
    pull(DESCRICAO_CARGO),
  df_Correlacao %>%
    arrange(abs(correlacao)) %>%
    tail(10) %>% ## 10 mais fortes
    pull(DESCRICAO_CARGO)
) -> correlacoes_20


salarios %>%
  filter(DESCRICAO_CARGO %in% correlacoes_20) %>%
  group_by(DESCRICAO_CARGO, ORGSUP_LOTACAO) %>%
  summarize(moda_OrgaoLotacao = n()) %>% 
  View()

salarios %>%
  filter(DESCRICAO_CARGO %in% correlacoes_20) %>%
  group_by(DESCRICAO_CARGO, ORGSUP_EXERCICIO) %>%
  summarize(moda_OrgaoExercicio = n()) %>%
  View()