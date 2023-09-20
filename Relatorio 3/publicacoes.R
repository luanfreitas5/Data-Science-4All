#Leitura de arquivos do publicacoes.json
#Resumo de Scripts para leitura do arquivo

library(tidyverse)
library(jsonlite)
library(listviewer)
library(scales)
library(dplyr)
library(readxl)
library(readr)
library(readtext)
library(ggplot2)
library(igraph)
library(knitr)

#Arquivo com funcionalidades que transformam o arquivo formato list em DataFrames
source("elattes.ls2df.R")

#Ler o arquivo publicacoes
file.info("Geotecnia/publication.json")
publicacoes <- fromJSON("Geotecnia/publication.json")

#Visualizar o arquivo no formato list
listviewer::jsonedit(publicacoes)

table(unlist(sapply(profile, function(x)
  (x$producao_bibiografica$ARTIGO_ACEITO$ano))))

#Dados Descritivos Gerais ####
# Tipos de objetos armazenados
names(publicacoes)

# Periodo analisado
names(publicacoes[[1]])

names(publicacoes$PERIODICO$`2012`)

head(sort(table(publicacoes$PERIODICO$`2017`$periodico), decreasing = TRUE), 10)

head(sort(
  table(publicacoes$LIVRO$`2015`$nome_da_editora),
  decreasing = TRUE
), 10)

# Visualizar a estrutura do arquivo de Publicacao

#Criando um data-frame com todos os anos
dataFramePublicacoes <- data.frame()
for (i in 1:length(publicacoes[[1]]))
  dataFramePublicacoes <-
  rbind(dataFramePublicacoes, publicacoes$PERIODICO[[i]])
glimpse(dataFramePublicacoes)

# Limpando o data-frame de listas
dataFramePublicacoes$autores <-
  gsub("\",\"|\", \"", "; ", dataFramePublicacoes$autores)
dataFramePublicacoes$autores <-
  gsub("\"|c\\(|\\)", "", dataFramePublicacoes$autores)
dataFramePublicacoes$`autores-endogeno` <-
  gsub(",", ";", dataFramePublicacoes$`autores-endogeno`)
dataFramePublicacoes$`autores-endogeno` <-
  gsub("\"|c\\(|\\)", "", dataFramePublicacoes$`autores-endogeno`)
glimpse(dataFramePublicacoes)


# NÃºmero de resultados (publicaÃ§Ãµes e outros) por Tipo e Ano
# Por Tipo Fixo (PERIODICO)
publicacoes$PERIODICO %>%
  sapply(function(x)
    length(x$autores))

# Por Ano Fixo (2010)
publicacoes %>%
  sapply(function(x)
    length(x$"2010"$autores))

# Por Tipo e Ano Geral
publicacoes %>%
  sapply(function(x)
    sapply(x, function(x)
      length(x$autores)))

#Periodicos ####
#Periodicos e o seu total geral e por ano
# Nome dos (100 primeiros) periodicos em ordem alfabÃ©tica
publicacoes$PERIODICO %>%
  sapply(function(x)
    (x$periodico)) %>%
  unlist() %>% unique() %>%
  sort() %>% head(100)

# NÃºmero total de periÃ³dicos geral
publicacoes$PERIODICO %>%
  sapply(function(x)
    (x$periodico)) %>%
  unlist() %>% unique() %>% length()

# NÃºmero total de periÃ³dicos por ano
publicacoes$PERIODICO %>%
  sapply(function(x)
    length(unique(unlist(x$periodico))))

#Periodicos mais frequentes (10) por ano
for (i in 1:length(publicacoes$PERIODICO)) {
  print(names(publicacoes$PERIODICO[i]))
  publicacoes$PERIODICO[[i]]$periodico %>%
    table() %>% sort(decreasing = TRUE) %>%
    head(10) %>% print()
}

#Periodicos mais frequentes (20) geral
publicacoes$PERIODICO %>%
  sapply(function(x)
    (x$periodico)) %>%
  unlist() %>% table() %>% sort(decreasing = TRUE) %>%
  head(20)

#Lista de revistas
head(sort(table(publicacoes$PERIODICO$`2011`$periodico), decreasing = TRUE), 20)

#AnÃ¡lise de um periodico especÃ?fico (Plos One)
publicacoes$PERIODICO %>%
  sapply(function(x)
    (x$periodico)) %>% unlist() %in% "Plos One" %>% sum()

#Livros e CapÃ?tulos ####
# NÃºmero total de livros por ano e tipo
publicacoes$LIVRO %>%
  sapply(function(x)
    table(x$tipo))

# NÃºmero total de livros por ano e natureza
publicacoes$LIVRO %>%
  sapply(function(x)
    table(x$natureza))

# Livros publicacoesados por Pais por ano
publicacoes$LIVRO %>%
  sapply(function(x)
    sort(table(x$pais_de_publicacao), decreasing = TRUE))

# Livros publicados por autor geral
publicacoes$LIVRO %>%
  sapply(function(x)
    (x$"autores-endogeno")) %>%
  unlist() %>% table() %>%
  sort(decreasing = TRUE) %>% head(40)

# Editora de publicaÃ§Ã£o
publicacoes$LIVRO %>%
  sapply(function(x)
    (x$nome_da_editora)) %>%
  unlist() %>% table() %>%
  sort(decreasing = TRUE) %>% head(20)

#Evento ####
# ParticipaÃ§Ã£o em Evento por Pais por ano
publicacoes$EVENTO %>%
  sapply(function(x)
    sort(table(x$pais_do_evento), decreasing = TRUE))

# ParticipaÃ§Ã£o em Evento por Cidade (40) Geral
publicacoes$EVENTO %>%
  sapply(function(x)
    (x$cidade_do_evento)) %>%
  unlist() %>% table() %>%
  sort(decreasing = TRUE) %>% head()

# Nome de evento em cidade especÃ?fica (Gramado)
(publicacoes$EVENTO %>%
    sapply(function(x)
      (x$nome_do_evento)) %>%
    unlist())[publicacoes$EVENTO %>%
                sapply(function(x)
                  (x$cidade_do_evento)) %>%
                unlist() %in% "Gramado"]

# Formato Data Frame  ####
# AnÃ¡lise dos dados no formato DF
#Periodico = 1; Livro = 2; CapÃ?tulo = 3; Jornais = 4;
#Evento = 5; Artigo aceto = 6; Demais produÃ§Ãµes = 7
dataFramePublicacoesPeriodico <- pub.ls2df(publicacoes, 1) #artigos
dataFramePublicacoesPeriodico <-
  dataFramePublicacoesPeriodico %>% mutate(t_pub = "periodico")

dataFramePublicacoesLivros <- pub.ls2df(publicacoes, 2) #livros
dataFramePublicacoesLivros <-
  dataFramePublicacoesLivros %>% mutate(t_pub = "livro")

dataFramePublicacoesCapitulos <-
  pub.ls2df(publicacoes, 3) #capitulos de livros
dataFramePublicacoesCapitulos <-
  dataFramePublicacoesCapitulos %>% mutate(t_pub = "capitulo")

dataFramePublicacoesJornais <-
  pub.ls2df(publicacoes, 4) #texto em jornais
dataFramePublicacoesJornais <-
  dataFramePublicacoesJornais %>% mutate(t_pub = "jornal")

dataFramePublicacoesEventos <- pub.ls2df(publicacoes, 5) #eventos
dataFramePublicacoesEventos <-
  dataFramePublicacoesEventos %>% mutate(t_pub = "evento") %>% rename(ano = ano_do_trabalho)

dataFramePublicacoesAceitas <-
  pub.ls2df(publicacoes, 6) #artigo aceito
dataFramePublicacoesAceitas <-
  dataFramePublicacoesAceitas %>% mutate(t_pub = "artigo aceito")

dataFramePublicacoesDemais <-
  pub.ls2df(publicacoes, 7) #demais tipos de publis
dataFramePublicacoesDemais <-
  dataFramePublicacoesDemais %>% mutate(t_pub = "demais tipos")


#Publicações por ano
dataFramePublicacoesPeriodico %>%
  ggplot(aes(x = ano)) + geom_bar(aes(fill = ano)) +
  geom_text(stat = "count", aes(label = formatC(..count.., big.mark = ",")), vjust =
              -0.1) +
  theme_bw() + labs(title = "Publicações por ano", x = "Ano", y = "Quantidade de Periódicos") +
  scale_y_continuous(labels = comma)

#20 revistas mais publicadas
#Mesma visÃ£o que anterior mas agora trabalhando no DataFrame
dataFramePublicacoesPeriodico %>% select(periodico) %>% table() %>% as.data.frame() %>% arrange(desc(Freq)) %>%
  head(20) %>% ggplot(aes(x = reorder(., (Freq)), y = Freq)) + geom_col(fill = "orange1") + coord_flip() +
  labs(title = "Os 20 Periódicos com Maior Publicações",
       y = "Número de Publicações", x = "Revistas") + geom_text(
         aes(label = comma(Freq)),
         hjust = -0.2,
         vjust = 0.3,
         size = 3.5
       ) + theme_bw() +
  scale_y_continuous(limits = c(0, 400))

#publicaÃ§Ã£o de livros fora do Brasil
dataFramePublicacoesLivros %>%
  group_by(pais_de_publicacao) %>%
  summarise(Quantidade = n()) %>%
  filter(pais_de_publicacao != "Brasil") %>%
  ggplot(aes(x = reorder(pais_de_publicacao, (Quantidade)), y = Quantidade)) +
  geom_col(fill = "chocolate") + geom_text(
    aes(label = comma(Quantidade)),
    hjust = -0.2,
    vjust = 0.3,
    size = 3.5
  ) + coord_flip() +
  labs(title = "Publicações de Livros em Países Estrangeiros", x = "Países", y = "Quantidade de Livros") +
  theme_bw()

dataFramePublicacoesLivros %>%
  filter(
    pais_de_publicacao %in% c(
      "Brasil",
      "Estados Unidos",
      "Holanda",
      "Grã-Bretanha",
      "Alemanha",
      "Suiça"
    )
  ) %>%
  group_by(ano, pais_de_publicacao) %>%
  ggplot(aes(x = ano, y = pais_de_publicacao, color = pais_de_publicacao)) +
  xlab("Ano") + ylab("País") + geom_point() + geom_jitter() +
  ggtitle("Livros publicados por ano") + 
  labs(color = "País de Publicações")

#Eventos
dataFramePublicacoesEventos %>%
  group_by(pais_do_evento) %>%
  summarise(Quantidade = n()) %>%
  filter(pais_do_evento != "Brasil") %>%
  ggplot(aes(x = reorder(pais_do_evento, (Quantidade)), y = Quantidade)) +
  geom_col(fill = "aquamarine") + geom_text(
    aes(label = comma(Quantidade)),
    hjust = -0.2,
    vjust = 0.3,
    size = 2.5
  ) + coord_flip() +
  labs(title = "Participação de Eventos em Países Estrangeiros", x = "Países", y = "Quantidade de Livros") +
  theme_bw()

topper <- dataFramePublicacoesPeriodico %>%
  full_join(dataFramePublicacoesLivros) %>%
  full_join(dataFramePublicacoesCapitulos) %>%
  full_join(dataFramePublicacoesJornais) %>%
  full_join(dataFramePublicacoesEventos) %>%
  full_join(dataFramePublicacoesAceitas) %>%
  full_join(dataFramePublicacoesDemais)

#############################################
# MEU PLOT
#############################################

topper %>%
  group_by(ano, t_pub) %>%
  summarise(Quantidade = n()) %>%
  ggplot(aes(x = ano, y = Quantidade, col = t_pub)) +
  geom_bar(stat = "identity") +
  facet_grid(t_pub ~ .)

ggplot(topper, aes(x = ano , y = t_pub)) +
  geom_jitter() 