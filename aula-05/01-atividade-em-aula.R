# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serão carregadas junto ao tidyverse
library(tidyverse)
library(lubridate)
library(dplyr)

# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz. 
ted <- read_csv("aula-05/data/ted_main.csv.gz")


# Visualize o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas.
# As variáveis duration, film_date e published_date estão no tipo de dados apropriado?

summary(ted)


# Converta as seguintes variáveis utilizando o pacote Lubridate:
#     * duration, para duração (em segundos). Experimente utilizar as funções as.duration e duration. Mantenha aquela que considerar mais apropriada.
#     * film_date, para data, com a função as_datetime.
#     * published_date, para data, com a função as_datetime..
ted %>%
  mutate(duration = as.duration(duration),
         film_date = as_datetime(film_date),
         published_date = as_datetime(published_date)
  ) -> ted


# Converta as seguintes variáveis character para variáveis categóricas com a função factor.
#     * event
#     * speaker_occupation

ted %>%
  mutate(event = factor(event),
         speaker_occupation = factor(speaker_occupation)) -> ted



# Retire do dataframe a variável name
ted %>%
  select(-name) -> ted



# Visualize novamente o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas. Verifique as contagens das variáveis categóricas
summary(ted)



# Verifique quais registros possuem a menor quantidade de línguas. Corrija para que possuam no mínimo 1 idioma.
ted %>%
  mutate(languages = if_else(languages == as.integer(0),as.integer(1),languages)) %>% View()



# Verifique os 15 registros com menor data de filmagem. 
ted %>%
  arrange(film_date) %>%
  head(15)


# Crie um dataframe com a contagem de apresentações por ano de filmagem e visualize todo o seu conteúdo
ted %>%
  group_by(year_film_date = year(film_date)) %>%
  summarize(cont_Apresentacoes = n()) %>%
  ungroup() -> ted_Apresentacoes


# Analise os 10 quantis da quantidade de apresentações por ano.
# Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.
quarto_quantil = c(quantile(ted_Apresentacoes$cont_Apresentacoes,  probs = seq(0,1,0.1)))[5]

ted %>%
  group_by(year(film_date)) %>%
  mutate(cont_apresentacoes = n()) %>%
  ungroup() %>%
  filter(cont_apresentacoes > quarto_quantil) -> ted_maiorQuartil

ted_maiorQuartil %>%
  View()


# Verifique novamente o resumo dos dados do dataframe

summary(ted_maiorQuartil)


# Verifique os 10 registros com maior duração.
ted_maiorQuartil %>%
  arrange(desc(duration)) %>%
  head(10) %>%
  View()



# Existem apresentações com duração maior que 3 desvios padrão acima da média? Liste elas
tresDesvios = as.duration(mean(ted_maiorQuartil$duration) + sd(ted_maiorQuartil$duration)*3)

ted_maiorQuartil %>%
  filter(as.duration(duration) > tresDesvios) %>%
  View()


# Calcule os 4 quartis e o IQR da duração das apresentações. Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil
duracao_quartil = as.duration(quantile(as.integer(ted_maiorQuartil$duration), probs = seq(0, 1, 0.25)))

ted %>%
  filter(duration > 1.5 * IQR(ted_maiorQuartil$duration) + duracao_quartil[4]) %>%
  View()

# Visualize os 10 quantis da quantidade de visualizações
quantile(ted_maiorQuartil$views, probs = seq(0, 1, 0.1))


# Compare as seguintes estatísticas descritivas da quantidade de visualizações:
#   * Média e Mediana. Qual é maior?
summary(ted_maiorQuartil$views)

# Resposta: média

#   * Desvio Absoluto da Mediana e Desvio Padrão. Qual é maior?

median(abs(ted_maiorQuartil$views - median(ted_maiorQuartil$views)))

sd(ted_maiorQuartil$views)

#Resposta DAM é maior


#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR é maior que o Desvio Absoluto da Mediana?
mediana = median(abs(ted_maiorQuartil$views - median(ted_maiorQuartil$views)))

qtd_Vezes = IQR(ted_maiorQuartil$views) / mediana

# Resposta: 2.19


#   * Com base na média e na mediana, e na razão entre o IQR e o Desvio Absoluto da Mediana, 
#     você conclui que as quantidades de visualização estão distribuidas de forma simétrica em torno da média?




# Calcule a média, o desvio padrão, a mediana e o IQR da quantidade de línguas dos seguintes grupos:
#     * 10% de vídeos com maior número de visualizações
#     * 10% de vídeos com menor número de visualizações

menores_10 <- ted_maiorQuartil %>%
  arrange(views) %>%
  head(count(ted_maiorQuartil) * 0.1) %>%
  mutate(views_group = '10% menores') %>%
  group_by(views_group) %>%
  summarise(media = mean(languages),
            desvioPadrao = sd(languages),
            mediana = median(languages),
            IQR_vl = IQR(languages)) %>%
  ungroup() %>%
  arrange(desc(views_group))

maiores_10 <- ted_maiorQuartil %>%
  arrange(views) %>%
  tail(count(ted_maiorQuartil) * 0.1) %>%
  mutate(views_group = '10% maiores') %>%
  group_by(views_group) %>%
  summarise(media = mean(languages),
            desvioPadrao = sd(languages),
            mediana = median(languages),
            IQR_vl = IQR(languages)) %>%
  ungroup() %>%
  arrange(desc(views_group))

rbind(menores_10, maiores_10) %>% View()


# Determine a quantidade de apresentações por evento cujo nome inicie com TED. Utilize a função str_detect para este filtro
qtdApresentacoes <- ted_maiorQuartil %>%
  filter(str_detect(event, "TED")) %>%
  distinct(event)

qtdApresentacoes %>%
  mutate(contagem = n()) %>%
  distinct(contagem) %>%
  View()


# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizações dos vídeos foi maior que a mediana calculada anteriormente.
#   * a quantidade de apresentações resultante do filtro, por evento
ted_maiorQuartil %>%
  filter(str_detect(event, "TED") & views > mediana) %>%
  count(event) %>%
  filter(n() > 10) %>%
  View()

#   * o ano do evento (utilizar o menor ano da data de publicação)
ted_maiorQuartil %>%
  filter(str_detect(event, "TED") & views > mediana) %>%
  group_by(event) %>%
  summarise(menorAnoPublicacao = min(year(published_date)), qtd = n()) %>%
  ungroup() %>%
  filter(qtd > 10) %>%
  View()

#   * a quantidade média de línguas das apresentações
ted_maiorQuartil %>%
  filter(str_detect(event, "TED") & views > views_mean) %>%
  summarise(mediaLinguas = mean(languages))

#   * o desvio padrão da quantidade de línguas
ted_maiorQuartil %>%
  filter(str_detect(event, "TED") & views > views_mean) %>%
  summarise(desvioPadraoLinguas = sd(languages))

#   * o coeficiente de variação da quantidade de línguas
ted_maiorQuartil  %>%
  filter(str_detect(event, "TED") & views > views_mean) %>%
  summarise(coeficienteVariacao = sd(languages) / mean(languages))

### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÇÕES
ted_maiorQuartil %>%
  filter(cont_apresentacoes > 10) -> ted_maior10Apresentacoes

# Calcule e classifique as seguintes correlações
#     * Quantidade de visualizações e Quantidade de línguas
ted_maior10Apresentacoes %>%
  summarise(correlacao = cor(views, languages)) %>%
  mutate(case_when(
    abs(correlacao) >= 0.9 ~ "MUITO FORTE",
    abs(correlacao) >= 0.7 ~ "FORTE",
    abs(correlacao) >= 0.5 ~ "MODERADO",
    abs(correlacao) >= 0.3 ~ "FRACO",
    abs(correlacao) >= 0.0 ~ "DESPREZÍVEL"
  ))

#     * Quantidade de visualizações e Duração
ted_maior10Apresentacoes %>%
  summarise(correlacao = cor(views, duration)) %>%
  mutate(case_when(
    abs(correlacao) >= 0.9 ~ "MUITO FORTE",
    abs(correlacao) >= 0.7 ~ "FORTE",
    abs(correlacao) >= 0.5 ~ "MODERADO",
    abs(correlacao) >= 0.3 ~ "FRACO",
    abs(correlacao) >= 0.0 ~ "DESPREZÍVEL"
  ))

#     * Quantidade de visualizações e Quantidade de Comentários
ted_maior10Apresentacoes %>%
  summarise(correlacao = cor(views, comments)) %>%
  mutate(case_when(
    abs(correlacao) >= 0.9 ~ "MUITO FORTE",
    abs(correlacao) >= 0.7 ~ "FORTE",
    abs(correlacao) >= 0.5 ~ "MODERADO",
    abs(correlacao) >= 0.3 ~ "FRACO",
    abs(correlacao) >= 0.0 ~ "DESPREZÍVEL"
  ))

#     * Quantidade de Comentários e Quantidade de línguas
ted_maior10Apresentacoes %>%
  summarise(correlacao = cor(comments, languages)) %>%
  mutate(case_when(
    abs(correlacao) >= 0.9 ~ "MUITO FORTE",
    abs(correlacao) >= 0.7 ~ "FORTE",
    abs(correlacao) >= 0.5 ~ "MODERADO",
    abs(correlacao) >= 0.3 ~ "FRACO",
    abs(correlacao) >= 0.0 ~ "DESPREZÍVEL"
  ))



# Descarte os vídeos cuja duração seja maior que 3 desvios padrões da média. Calcule novamente as 5 correlações solicitadas

ted_maior10Apresentacoes %>%
  filter(duration < tresDesvios) -> ted_menor3desvioes


# Utilizando o data frame original, crie um dataframe com a mediana da duração dos vídeos por ano de filmagem. Calcule a correlação entre o ano e a mediana da duração
# e interprete o resultado

ted %>%
  group_by(ano_filme = year(film_date)) %>%
  summarise(media_duracao = median(duration)) -> ted_media %>% View()

cor(ted_media$ano_filme, ted_media$media_duracao)

# O valor da correlação fica em 0.49, o que é fraco, porém, ao analisar os dados, 
# parece que ao passar do tempo a duração diminui  