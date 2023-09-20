#Leitura de arquivos do publication.json
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

#Ler o arquivo orientacoes
file.info("Geotecnia/advise.json")
orientacoes <- fromJSON("Geotecnia/advise.json")

#Visualizar o arquivo no formato list
listviewer::jsonedit(orientacoes)

#Dados Descritivos Gerais ####
# Tipos de objetos armazenados
names(orientacoes)
names(orientacoes$ORIENTACAO_CONCLUIDA_DOUTORADO)

length(orientacoes$ORIENTACAO_CONCLUIDA_DOUTORADO$`2016`$natureza)

head(sort(
  table(orientacoes$ORIENTACAO_CONCLUIDA_DOUTORADO$`2017`$curso),
  decreasing = TRUE
), 10)

head(sort(
  table(orientacoes$ORIENTACAO_CONCLUIDA_MESTRADO$`2017`$curso),
  decreasing = TRUE
), 10)

# Periodo analisado
names(orientacoes[[1]])

# NÃºmero de resultados (orientaÃ§Ãµes) por Tipo e Ano
# Por Tipo Fixo (ORIENTACAO_CONCLUIDA_DOUTORADO)
orientacoes$ORIENTACAO_CONCLUIDA_DOUTORADO %>%
  sapply(function(x)
    length(x$natureza))

# Por Ano Fixo (2010)
orientacoes %>%
  sapply(function(x)
    length(x$"2010"$natureza))

# Por Tipo e Ano Geral
orientacoes %>%
  sapply(function(x)
    sapply(x, function(x)
      length(x$natureza)))

# Ano - Todas orientaÃ§Ãµes por tipo, por ano e para todos os anos
for (i in 1:length(orientacoes)) {
  print(names(orientacoes[i]))
  print(orientacoes[[i]] %>%
          sapply(function(x)
            length(x$ano)) %>% sum())
}

for (i in 1:length(orientacoes[[1]])) {
  print(names(orientacoes[[1]][i]))
  print(orientacoes %>%
          sapply(function(x)
            length(x[[i]]$ano)) %>% sum())
}

orientacoes %>%
  sapply(function(x)
    sapply(x, function(x)
      length(x$natureza))) %>% sum()

# Detalhe em OUTRAS_ORIENTACOES_CONCLUIDAS
orientacoes$OUTRAS_ORIENTACOES_CONCLUIDAS %>%
  sapply(function(x)
    table(x$natureza))

#OreintaÃ§Ã£o ####
#AvaliaÃ§Ã£o do segundo nÃ?vel hierÃ¡rquico
# Titulo - Busca por palavra no tÃ?tulo (Zika) ####
# Resultado TÃ?tulo
(orientacoes %>%
   sapply(function(x)
     sapply(x, function(x)
       (x$titulo))) %>%
   unlist())[orientacoes %>%
               sapply(function(x)
                 sapply(x, function(x)
                   (x$titulo))) %>%
               unlist() %>% grepl(pattern = "Zika")]
# Resultado Autor
(orientacoes %>%
    sapply(function(x)
      sapply(x, function(x)
        (x$nome_aluno))) %>%
    unlist())[orientacoes %>%
                sapply(function(x)
                  sapply(x, function(x)
                    (x$titulo))) %>%
                unlist() %>% grepl(pattern = "Zika")]
# Resultado Tipo de orientaÃ§Ã£o / trabalho
(orientacoes %>%
    sapply(function(x)
      sapply(x, function(x)
        (x$natureza))) %>%
    unlist())[orientacoes %>%
                sapply(function(x)
                  sapply(x, function(x)
                    (x$titulo))) %>%
                unlist() %>% grepl(pattern = "Zika")] %>%
  table()

# Resultado - AnÃ¡lise em Tipo e perÃ?odo especÃ?fico
orientacoes$OUTRAS_ORIENTACOES_CONCLUIDAS$`2017`$titulo %>%
  grepl(pattern = "Zika")

# Por instituiÃ§Ã£o ####
#InstituiÃ§Ã£o onde os pesquisadores / professores orientam
orientacoes %>%
  sapply(function(x)
    sapply(x, function(x)
      (x$instituicao))) %>%
  unlist() %>% table() %>%
  sort(decreasing = TRUE) %>% head(20)

# Por Curso ####
#Cursos mais orientados dos pesquisadores / professores
orientacoes %>%
  sapply(function(x)
    sapply(x, function(x)
      (x$curso))) %>%
  unlist() %>% table() %>%
  sort(decreasing = TRUE) %>% head(40)

# Por Agencia Financiadora ####
#Agencias que mais financiaram
orientacoes %>%
  sapply(function(x)
    sapply(x, function(x)
      (x$agencia_financiadora))) %>%
  unlist() %>% table() %>%
  sort(decreasing = TRUE) %>% head(30)

#Orientação
#Visualizar a estrutura do json no painel Viewer
#jsonedit(orientacoes)
#Reunir todos os anos e orientações concluidas em um mesmo data-frame
dataframeTipoOrientacoes <- data.frame()
dataframeOrientacoes <- data.frame()
for (i in 1:length(orientacoes[[1]]))
  dataframeTipoOrientacoes <-
  rbind(dataframeTipoOrientacoes,
        orientacoes$ORIENTACAO_CONCLUIDA_POS_DOUTORADO[[i]])
dataframeOrientacoes <-
  rbind(dataframeOrientacoes, dataframeTipoOrientacoes)
dataframeTipoOrientacoes <- data.frame()
for (i in 1:length(orientacoes[[1]]))
  dataframeTipoOrientacoes <-
  rbind(dataframeTipoOrientacoes,
        orientacoes$ORIENTACAO_CONCLUIDA_DOUTORADO[[i]])
dataframeOrientacoes <-
  rbind(dataframeOrientacoes, dataframeTipoOrientacoes)
dataframeTipoOrientacoes <- data.frame()
for (i in 1:length(orientacoes[[1]]))
  dataframeTipoOrientacoes <-
  rbind(dataframeTipoOrientacoes,
        orientacoes$ORIENTACAO_CONCLUIDA_MESTRADO[[i]])
dataframeOrientacoes <-
  rbind(dataframeOrientacoes, dataframeTipoOrientacoes)
glimpse(dataframeOrientacoes)

#Transformar as colunas de listas em caracteres eliminando c("")
dataframeOrientacoes$nome_orientadores <-
  gsub("\"|c\\(|\\)", "", dataframeOrientacoes$nome_orientadores)
dataframeOrientacoes$id_lattes_orientadores <-
  gsub("\"|c\\(|\\)",
       "",
       dataframeOrientacoes$id_lattes_orientadores)

#Separar as colunas com dois orientadores
dataframeOrientacoes <-
  separate(
    dataframeOrientacoes,
    nome_orientadores,
    into = c("ori1", "ori2"),
    sep = ","
  )

dataframeOrientacoes <-
  separate(
    dataframeOrientacoes,
    id_lattes_orientadores,
    into = c("idLattes1", "idLattes2"),
    sep = ","
  )

#Numero de orientacoes por ano
table(dataframeOrientacoes$ano)

#Tabela com nome de professor e numero de orientacoes
head(sort(table(
  rbind(dataframeOrientacoes$ori1, dataframeOrientacoes$ori2)
), decreasing = TRUE), 20)


# Formato Data Frame  ####
# AnÃ¡lise dos dados no formato DF
#ORIENTACAO_EM_ANDAMENTO_DE_POS_DOUTORADO = 1; ORIENTACAO_EM_ANDAMENTO_DOUTORADO = 2;
#ORIENTACAO_EM_ANDAMENTO_MESTRADO = 3; ORIENTACAO_EM_ANDAMENTO_GRADUACAO = 4;
#ORIENTACAO_EM_ANDAMENTO_INICIACAO_CIENTIFICA = 5; ORIENTACAO_CONCLUIDA_POS_DOUTORADO = 6;
#ORIENTACAO_CONCLUIDA_DOUTORADO = 7; ORIENTACAO_CONCLUIDA_MESTRADO = 8;
#OUTRAS_ORIENTACOES_CONCLUIDAS = 9

dataFrameOrientacoesPosDoutorado <-
  ori.ls2df(orientacoes, 6) #pos-Doutorado concluÃ?do
dataFrameOrientacoesDoutorado <-
  ori.ls2df(orientacoes, 7) #Doutorado concluÃ?do
dataFrameOrientacoesMestrado <-
  ori.ls2df(orientacoes, 8) #Mestrado concluÃ?do

dataFrameOrientacoes <-
  rbind(
    rbind(
      dataFrameOrientacoesPosDoutorado,
      dataFrameOrientacoesDoutorado
    ),
    dataFrameOrientacoesMestrado
  )

orientacoesSum <- group_by(dataFrameOrientacoes, ano, natureza) %>%
  summarise(n = n())

ggplot(dataFrameOrientacoes, aes(ano, fill = factor(natureza))) +
  geom_bar(stat = "count", position = "dodge") +
  ggtitle("Natureza das Orientações Completas Por Ano") +
  theme(legend.position = "right", legend.text = element_text(size = 7)) +
  guides(fill = guide_legend(
    nrow = 5,
    byrow = TRUE,
    title.position = "top"
  )) +
  labs(x = "Ano", y = "Quantidade") + labs(fill = "Natureza") + theme_bw() +
  geom_text(
    hjust = 0.6,
    vjust = -0.4,
    size = 3,
    color = 'black',
    position = position_dodge(width = 0.9),
    stat = "count",
    aes(
      group = factor(natureza),
      label = formatC(..count.., big.mark = ",")
    ),
    check_overlap = TRUE
  )


ggplot(orientacoesSum,
       aes(
         x = ano,
         y = n,
         group = natureza,
         color = natureza
       )) +
  geom_line(alpha = 0.6) +
  geom_point(alpha = 0.6)

#Orientaçoes por ano
dataFrameOrientacoes %>%
  ggplot(aes(x = ano)) + geom_bar(aes(fill = ano)) +
  geom_text(stat = "count", aes(label = formatC(..count.., big.mark = ",")), vjust =
              -0.1) +
  theme_bw() + labs(title = "Orientações por ano", x = "Ano", y = "Quantidade de Periódicos") +
  scale_y_continuous(labels = comma)