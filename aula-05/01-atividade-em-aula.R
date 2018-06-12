# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serão carregadas junto ao tidyverse
library(tidyverse)

# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz. 
ted_talks <- read_csv("aula-05/data/ted_main.csv.gz")

# Visualize o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas.
summary(ted_talks)
# As variáveis duration, film_date e published_date estão no tipo de dados apropriado?
# Não, duration é apenas um valor inteiro e film_date e published_date devem ser convertidas para tipo datetime.

# Converta as seguintes variáveis utilizando o pacote Lubridate:
#     * duration, para duração (em segundos). Experimente utilizar as funções as.duration e duration. Mantenha aquela que considerar mais apropriada.
#     * film_date, para data, com a função as_datetime.
#     * published_date, para data, com a função as_datetime..
ted_talks %>%
  mutate(duration = as.duration(duration),
         film_date = as_datetime(film_date),
         published_date = as_datetime(published_date)) -> ted_talks

# Converta as seguintes variáveis character para variáveis categóricas com a função factor.
#     * event
#     * speaker_occupation
ted_talks %>%
  mutate(event = factor(event),
         speaker_occupation = factor(speaker_occupation)) -> ted_talks

# Retire do dataframe a variável name
ted_talks$name <- NULL

# Visualize novamente o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas. Verifique as contagens das variáveis categóricas
summary(ted_talks)

# Verifique quais registros possuem a menor quantidade de línguas. Corrija para que possuam no mínimo 1 idioma.
ted_talks %>%
  mutate(languages = replace(languages, languages == 0, 1)) -> ted_talks

# Verifique os 15 registros com menor data de filmagem. 
ted_talks %>%
  arrange(film_date) %>%
  head(n = 15)

# Crie um dataframe com a contagem de apresentações por ano de filmagem e visualize todo o seu conteúdo
ted_talks %>%
  group_by(ano_filmagem = year(film_date)) %>%
  summarise(qtd = n()) %>%
  ungroup() -> apresentacoes_porano

apresentacoes_porano

# Analise os 10 quantis da quantidade de apresentações por ano.
# Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.
ted_talks %>%
  group_by(year(film_date)) %>%
  mutate(qtd = n()) %>%
  ungroup() %>%
  filter(qtd > quantile(apresentacoes_porano$qtd, probs = seq(0, 1, 0.1))[5]) -> ted_talks

# Verifique novamente o resumo dos dados do dataframe
summary(ted_talks)

# Verifique os 10 registros com maior duração.
ted_talks %>%
  arrange(desc(duration)) %>%
  head(n = 10)

# Existem apresentações com duração maior que 3 desvios padrão acima da média? Liste elas
ted_talks %>%
  summarise(media = as.duration(mean(duration)),
            desvio_padrao = as.duration(sd(duration))) -> duracao_med_sd

ted_talks %>%
  filter(duration > duracao_med_sd$desvio_padrao * 3)

# Calcule os 4 quartis e o IQR da duração das apresentações. Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil
quantile(ted_talks$duration, probs = seq(0, 1, 0.25))

IQR(ted_talks$duration)

ted_talks %>%
  filter(duration > (1.5 * IQR(duration) + quantile(duration, probs = seq(0, 1, 0.25))[3] ) )

# Visualize os 10 quantis da quantidade de visualizações
quantile(ted_talks$views, probs = seq(0, 1, 0.1))

# Compare as seguintes estatísticas descritivas da quantidade de visualizações:
#   * Média e Mediana. Qual é maior?
#   * Desvio Absoluto da Mediana e Desvio Padrão. Qual é maior?
#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR é maior que o Desvio Absoluto da Mediana?
#   * Com base na média e na mediana, e na razão entre o IQR e o Desvio Absoluto da Mediana, 
#     você conclui que as quantidades de visualização estão distribuidas de forma simétrica em torno da média?
ted_talks %>%
  summarise(media = mean(views),
            mediana = median(views),
            desvio_padrao = sd(views),
            desvio_abs_mediana = median( abs( views - median( views ))),
            iqr = IQR(views))

# Calcule a média, o desvio padrão, a mediana e o IQR da quantidade de línguas dos seguintes grupos:
#     * 10% de vídeos com maior número de visualizações
#     * 10% de vídeos com menor número de visualizações
rbind(
  ted_talks %>%
    arrange(desc(views)) %>% 
    head(count(ted_talks) * 0.1) %>%
    summarise(grupo = 'maior views',
              media = mean(languages),
              desvio_padrao = sd(languages),
              mediana = median(languages),
              iqr = IQR(languages))
  ,
  ted_talks %>%
    arrange(views) %>% 
    head(count(ted_talks) * 0.1) %>%
    summarise(grupo = 'menor views',
              media = mean(languages),
              desvio_padrao = sd(languages),
              mediana = median(languages),
              iqr = IQR(languages))
)

# Determine a quantidade de apresentações por evento cujo nome inicie com TED. Utilize a função str_detect para este filtro
ted_talks %>%
  group_by(event) %>%
  summarise(qtd = n()) %>%
  filter(str_detect(event, 'TED')) %>%
  ungroup()

# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizações dos vídeos foi maior que a mediana calculada anteriormente.
#   * a quantidade de apresentações resultante do filtro, por evento
#   * o ano do evento (utilizar o menor ano da data de publicação)
#   * a quantidade média de línguas das apresentações
#   * o desvio padrão da quantidade de línguas
#   * o coeficiente de variação da quantidade de línguas
### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÇÕES
ted_talks %>%
  filter(str_detect(event, 'TED') & views > median(views)) %>%
  group_by(event) %>%
  summarise(qtd_apresentacoes = n(),
            ano = min(year(published_date)),
            media_linguas = mean(languages),
            desvio_padrao = sd(languages),
            coef_variacao = desvio_padrao / media_linguas) %>%
  ungroup()

# Calcule e classifique as seguintes correlações
#     * Quantidade de visualizações e Quantidade de línguas
#     * Quantidade de visualizações e Duração
#     * Quantidade de visualizações e Quantidade de Comentários
#     * Quantidade de Comentários e Quantidade de línguas
ted_talks %>%
  summarise(corr_vis_lin = cor(x = views, y = languages),
            corr_vis_lin_clas = case_when(abs(corr_vis_lin) >= 0.0 & abs(corr_vis_lin) < 0.3 ~ 'DESPREZIVEL',
                                          abs(corr_vis_lin) >= 0.3 & abs(corr_vis_lin) < 0.5 ~ 'FRACA',
                                          abs(corr_vis_lin) >= 0.5 & abs(corr_vis_lin) < 0.7 ~ 'MODERADA',
                                          abs(corr_vis_lin) >= 0.7 & abs(corr_vis_lin) < 0.9 ~ 'FORTE',
                                          abs(corr_vis_lin) >= 0.9 ~ 'MUITO FORTE'),
            corr_vis_dur = cor(x = views, y = duration),
            corr_vis_dur_clas = case_when(abs(corr_vis_dur) >= 0.0 & abs(corr_vis_dur) < 0.3 ~ 'DESPREZIVEL',
                                          abs(corr_vis_dur) >= 0.3 & abs(corr_vis_dur) < 0.5 ~ 'FRACA',
                                          abs(corr_vis_dur) >= 0.5 & abs(corr_vis_dur) < 0.7 ~ 'MODERADA',
                                          abs(corr_vis_dur) >= 0.7 & abs(corr_vis_dur) < 0.9 ~ 'FORTE',
                                          abs(corr_vis_dur) >= 0.9 ~ 'MUITO FORTE'),
            corr_vis_com = cor(x = views, y = comments),
            corr_vis_com_clas = case_when(abs(corr_vis_com) >= 0.0 & abs(corr_vis_com) < 0.3 ~ 'DESPREZIVEL',
                                          abs(corr_vis_com) >= 0.3 & abs(corr_vis_com) < 0.5 ~ 'FRACA',
                                          abs(corr_vis_com) >= 0.5 & abs(corr_vis_com) < 0.7 ~ 'MODERADA',
                                          abs(corr_vis_com) >= 0.7 & abs(corr_vis_com) < 0.9 ~ 'FORTE',
                                          abs(corr_vis_com) >= 0.9 ~ 'MUITO FORTE'),
            corr_com_lin = cor(x = comments, y = languages),
            corr_com_lin_clas = case_when(abs(corr_com_lin) >= 0.0 & abs(corr_com_lin) < 0.3 ~ 'DESPREZIVEL',
                                          abs(corr_com_lin) >= 0.3 & abs(corr_com_lin) < 0.5 ~ 'FRACA',
                                          abs(corr_com_lin) >= 0.5 & abs(corr_com_lin) < 0.7 ~ 'MODERADA',
                                          abs(corr_com_lin) >= 0.7 & abs(corr_com_lin) < 0.9 ~ 'FORTE',
                                          abs(corr_com_lin) >= 0.9 ~ 'MUITO FORTE'))

# Descarte os vídeos cuja duração seja maior que 3 desvios padrões da média. Calcule novamente as 5 correlações solicitadas
ted_talks %>%
  summarise(media = as.duration(mean(duration)),
            desvio_padrao = as.duration(sd(duration))) -> duracao_med_sd

ted_talks %>%
  filter(duration <= duracao_med_sd$desvio_padrao * 3) -> ted_talks_menor3_sd

ted_talks_menor3_sd %>%
  summarise(corr_vis_lin = cor(x = views, y = languages),
            corr_vis_lin_clas = case_when(abs(corr_vis_lin) >= 0.0 & abs(corr_vis_lin) < 0.3 ~ 'DESPREZIVEL',
                                          abs(corr_vis_lin) >= 0.3 & abs(corr_vis_lin) < 0.5 ~ 'FRACA',
                                          abs(corr_vis_lin) >= 0.5 & abs(corr_vis_lin) < 0.7 ~ 'MODERADA',
                                          abs(corr_vis_lin) >= 0.7 & abs(corr_vis_lin) < 0.9 ~ 'FORTE',
                                          abs(corr_vis_lin) >= 0.9 ~ 'MUITO FORTE'),
            corr_vis_dur = cor(x = views, y = duration),
            corr_vis_dur_clas = case_when(abs(corr_vis_dur) >= 0.0 & abs(corr_vis_dur) < 0.3 ~ 'DESPREZIVEL',
                                          abs(corr_vis_dur) >= 0.3 & abs(corr_vis_dur) < 0.5 ~ 'FRACA',
                                          abs(corr_vis_dur) >= 0.5 & abs(corr_vis_dur) < 0.7 ~ 'MODERADA',
                                          abs(corr_vis_dur) >= 0.7 & abs(corr_vis_dur) < 0.9 ~ 'FORTE',
                                          abs(corr_vis_dur) >= 0.9 ~ 'MUITO FORTE'),
            corr_vis_com = cor(x = views, y = comments),
            corr_vis_com_clas = case_when(abs(corr_vis_com) >= 0.0 & abs(corr_vis_com) < 0.3 ~ 'DESPREZIVEL',
                                          abs(corr_vis_com) >= 0.3 & abs(corr_vis_com) < 0.5 ~ 'FRACA',
                                          abs(corr_vis_com) >= 0.5 & abs(corr_vis_com) < 0.7 ~ 'MODERADA',
                                          abs(corr_vis_com) >= 0.7 & abs(corr_vis_com) < 0.9 ~ 'FORTE',
                                          abs(corr_vis_com) >= 0.9 ~ 'MUITO FORTE'),
            corr_com_lin = cor(x = comments, y = languages),
            corr_com_lin_clas = case_when(abs(corr_com_lin) >= 0.0 & abs(corr_com_lin) < 0.3 ~ 'DESPREZIVEL',
                                          abs(corr_com_lin) >= 0.3 & abs(corr_com_lin) < 0.5 ~ 'FRACA',
                                          abs(corr_com_lin) >= 0.5 & abs(corr_com_lin) < 0.7 ~ 'MODERADA',
                                          abs(corr_com_lin) >= 0.7 & abs(corr_com_lin) < 0.9 ~ 'FORTE',
                                          abs(corr_com_lin) >= 0.9 ~ 'MUITO FORTE'))

# Utilizando o data frame original, crie um dataframe com a mediana da duração dos vídeos por ano de filmagem. Calcule a correlação entre o ano e a mediana da duração
# e interprete o resultado
ted_talks %>%
  group_by(ano = year(film_date)) %>%
  summarise(mediana = median(duration)) %>%
  ungroup() -> ted_talks_mediana_ano

ted_talks_mediana_ano %>%
  group_by(ano) %>%
  summarise(correl = cor(x = ano, y = mediana))