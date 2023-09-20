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

#Ler o arquivo publication
file.info("Geotecnia/publication.json")
publication <- fromJSON("Geotecnia/publication.json")

#Criando um data-frame com todos os anos
publication.df <- data.frame()
for (i in 1:length(publication[[1]]))
  publication.df <-
  rbind(publication.df, publication$PERIODICO[[i]])

# Limpando o data-frame de listas
publication.df$autores <-
  gsub("\",\"|\", \"", "; ", publication.df$autores)
publication.df$autores <-
  gsub("\"|c\\(|\\)", "", publication.df$autores)
publication.df$`autores-endogeno` <-
  gsub(",", ";", publication.df$`autores-endogeno`)
publication.df$`autores-endogeno` <-
  gsub("\"|c\\(|\\)", "", publication.df$`autores-endogeno`)


# Formato Data Frame  ####
# Análise dos dados no formato DF
#Periodico = 1; Livro = 2; Cap�?tulo = 3; Jornais = 4;
#Evento = 5; Artigo aceto = 6; Demais produções = 7
publication.periodico.df <- pub.ls2df(publication, 1) #artigos
publication.periodico.df <-
  publication.periodico.df %>% mutate(t_pub = "periodico")

publication.livros.df <- pub.ls2df(publication, 2) #livros
publication.livros.df <-
  publication.livros.df %>% mutate(t_pub = "livro")

publication.capitulos.df <-
  pub.ls2df(publication, 3) #capitulos de livros
publication.capitulos.df <-
  publication.capitulos.df %>% mutate(t_pub = "capitulo")

publication.jornais.df <-
  pub.ls2df(publication, 4) #texto em jornais
publication.jornais.df <-
  publication.jornais.df %>% mutate(t_pub = "jornal")

publication.eventos.df <- pub.ls2df(publication, 5) #eventos
publication.eventos.df <-
  publication.eventos.df %>% mutate(t_pub = "evento") %>% rename(ano = ano_do_trabalho)

publication.aceitos.df <- pub.ls2df(publication, 6) #artigo aceito
publication.aceitos.df <-
  publication.aceitos.df %>% mutate(t_pub = "artigo aceito")

publication.demais.df <-
  pub.ls2df(publication, 7) #demais tipos de publis
publication.demais.df <-
  publication.demais.df %>% mutate(t_pub = "demais tipos")
