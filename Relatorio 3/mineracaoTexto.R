#AnÃ¡lise inicial dos arquivos geraos pelo e-Lattes

#Pacotes para serem ativados
library(tidyverse)
library(jsonlite)
library(listviewer)
library(igraph)
library(ggplot2)
library(dplyr)
library(tm)
library(SnowballC)
library(qdap)
library(wordcloud)

#upload de arquivo com funÃ§Ãµes para transformar listas em Data Frames e objeto igraph
source("elattes.ls2df.R")

perfis <- fromJSON("Geotecnia/profile.json")

cores <- c('chocolate', 'gold', 'forestgreen', 'blue', 'red')

limparCorpus <- function(x) {
  corpusLimpo <- x %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(stripWhitespace) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeWords, stopwords("en")) %>%
    tm_map(removeWords, stopwords("pt")) %>%
    tm_map(
      removeWords,
      c(
        "brasil",
        "brazil",
        "sociologia",
        "comunicacao",
        "comunicação",
        "ciência",
        "informação",
        "serviço social",
        "sobre",
        "n",
        "vol",
        "pesquisa",
        "universidade",
        "sobre"
      )
    )
  return(corpusLimpo)
}

obterTitulos <- function(dataFramePublicacoes) {
  titulos <- c()
  for (i in 1:nrow(dataFramePublicacoes)) {
    if (is.na(dataFramePublicacoes$titulo[[i]])) {
      titulos[[i]] <- dataFramePublicacoes$titulo_do_livro[[i]]
    }
    else{
      titulos[[i]] <- dataFramePublicacoes$titulo[[i]]
    }
  }
  return(titulos)
}


obterCurriculos <- function(dataFrameProfessores) {
  ## vetor de curriculos
  curriculos <- c()
  ## popula o vetor de curriculos
  for (i in 1:nrow(dataFrameProfessores)) {
    curriculos[[i]] <- dataFrameProfessores$resumo_cv[[i]]
  }
  return(curriculos)
}

# Analise de Perfils ####


dataFrameProfessores <- extrai.perfis(perfis)

curriculos <- obterCurriculos(dataFrameProfessores)

## criar o corpus
vectorCurriculos <- VectorSource(curriculos)
corpusCurriculos <- VCorpus(vectorCurriculos)
corpusCurriculos <- limparCorpus(corpusCurriculos)

curriculosTDM <- TermDocumentMatrix(corpusCurriculos)
curriculosMatriz <- as.matrix(curriculosTDM)

## nuvem de palavras para visualizar as palavras mais frequentes
wordcloud(
  corpusCurriculos,
  scale = c(4, 0.25),
  max.words = 150,
  random.order = FALSE,
  rot.per = 0.5,
  use.r.layout = FALSE,
  colors = cores
)


# brewer.pal(8, "Dark2")

# Analise de Publicações ####


dataFramePublicacoes <- extrai.producoes(perfis)

titulos <- obterTitulos(dataFramePublicacoes)

##criar o corpus
corpusTitulos <- VCorpus(VectorSource(titulos))
corpusTitulos <- limparCorpus(corpusTitulos)

## criaÃ§Ã£o da nuvem de palavras
wordcloud(
  corpusTitulos,
  scale = c(3.5, 0.25),
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.5,
  use.r.layout = FALSE,
  colors = cores
)
