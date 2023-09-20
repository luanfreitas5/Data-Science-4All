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

advise <- fromJSON("Geotecnia/advise.json")

#Orientação
#Visualizar a estrutura do json no painel Viewer
#jsonedit(advise)
#Reunir todos os anos e orientações concluidas em um mesmo data-frame
advise.tipo.df <- data.frame()
advise.df <- data.frame()
for (i in 1:length(advise[[1]]))
  advise.tipo.df <-
  rbind(advise.tipo.df, advise$ORIENTACAO_CONCLUIDA_POS_DOUTORADO[[i]])
advise.df <-
  rbind(advise.df, advise.tipo.df)
advise.tipo.df <- data.frame()
for (i in 1:length(advise[[1]]))
  advise.tipo.df <-
  rbind(advise.tipo.df, advise$ORIENTACAO_CONCLUIDA_DOUTORADO[[i]])
advise.df <-
  rbind(advise.df, advise.tipo.df)
advise.tipo.df <- data.frame()
for (i in 1:length(advise[[1]]))
  advise.tipo.df <-
  rbind(advise.tipo.df, advise$ORIENTACAO_CONCLUIDA_MESTRADO[[i]])
advise.df <- rbind(advise.df, advise.tipo.df)


#Transformar as colunas de listas em caracteres eliminando c("")
advise.df$nome_orientadores <-
  gsub("\"|c\\(|\\)", "", advise.df$nome_orientadores)
advise.df$id_lattes_orientadores <-
  gsub("\"|c\\(|\\)", "", advise.df$id_lattes_orientadores)

#Separar as colunas com dois orientadores
advise.df <-
  separate(advise.df,
           nome_orientadores,
           into = c("ori1", "ori2"),
           sep = ",")

advise.df <-
  separate(
    advise.df,
    id_lattes_orientadores,
    into = c("idLattes1", "idLattes2"),
    sep = ","
  )

# Formato Data Frame  ####
# AnÃ¡lise dos dados no formato DF
#ORIENTACAO_EM_ANDAMENTO_DE_POS_DOUTORADO = 1; ORIENTACAO_EM_ANDAMENTO_DOUTORADO = 2;
#ORIENTACAO_EM_ANDAMENTO_MESTRADO = 3; ORIENTACAO_EM_ANDAMENTO_GRADUACAO = 4;
#ORIENTACAO_EM_ANDAMENTO_INICIACAO_CIENTIFICA = 5; ORIENTACAO_CONCLUIDA_POS_DOUTORADO = 6;
#ORIENTACAO_CONCLUIDA_DOUTORADO = 7; ORIENTACAO_CONCLUIDA_MESTRADO = 8;
#OUTRAS_ORIENTACOES_CONCLUIDAS = 9
orient.posdoutorado.df <-
  ori.ls2df(advise, 6) #pos-Doutorado concluÃ?do
orient.doutorado.df <- ori.ls2df(advise, 7) #Doutorado concluÃ?do
orient.mestrado.df <- ori.ls2df(advise, 8) #Mestrado concluÃ?do

orient.posdoutorado_p.df <-
  ori.ls2df(advise, 1) #pos-Doutorado concluÃ?do
orient.doutorado_p.df <- ori.ls2df(advise, 2) #Doutorado concluÃ?do
orient.mestrado_p.df <- ori.ls2df(advise, 3) #Mestrado concluÃ?do

orient.df <-
  rbind(rbind(orient.posdoutorado.df, orient.doutorado.df),
        orient.mestrado.df)
orient_p.df <-
  rbind(rbind(orient.posdoutorado_p.df, orient.doutorado_p.df),
        orient.mestrado_p.df)

orient_sum <- group_by(orient.df, ano, natureza) %>%
  summarise(n = n())
orient_p_sum <- group_by(orient_p.df, ano, natureza) %>%
  summarise(n = n())
