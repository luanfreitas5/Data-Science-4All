#Leitura de arquivos do profile.json
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

#Ler o arquivo profile
profile <- fromJSON("Geotecnia/profile.json")

# Data Frame ####
# AnÃ¡lise dos dados em formato Data Frame
# Usar as funcionalidades que estão no arquivo elatttes.ls2df.R
#Arquivo Profile por currículo
# extrai perfis dos professores 
# extrai perfis dos professores 
profile.df.professores <- extrai.perfis(profile)
# extrai producao bibliografica de todos os professores 
profile.df.publicacoes <- extrai.producoes(profile) %>%
  select(tipo_producao, everything()) %>% arrange(tipo_producao)
profile.df.publicacoes$tipo_producao[grepl("DEMAIS",profile.df.publicacoes$tipo_producao)] <- "OUTRAS_PRODUCOES"
profile.df.publicacoes %>% select(pais_de_publicacao) %>% distinct()
#extrai orientacoes 
profile.df.orientacoes <- extrai.orientacoes(profile) %>%
  select(id_lattes_orientadores, natureza, ano, orientacao, everything()) %>%
  mutate(Status = ifelse(grepl("CONCLUIDA", orientacao), "Concluída", "Em andamento")) %>%
  mutate(Natureza = case_when(grepl("MESTRADO", str_to_upper(natureza)) ~ "Mestrado",
                              grepl("PÓS-DOUTORADO", str_to_upper(natureza)) ~ "Pós-doutorado",
                              grepl("DOUTORADO", str_to_upper(natureza)) ~ "Doutorado",
                              grepl("INICIACAO", str_to_upper(natureza)) ~ "Iniciação Científica",
                              grepl("INICIAÇÃO", str_to_upper(natureza)) ~ "Iniciação Científica",
                              TRUE ~ "Outras naturezas"))           
profile.df.orientacoes %>% select(natureza, Natureza) %>% distinct() %>% arrange(Natureza)
str_to_upper("pós-doutorado")
#extrai areas de atuacao 
profile.df.areas.de.atuacao <- extrai.areas.atuacao(profile) %>%
  select(idLattes, everything())
#cria arquivo com dados quantitativos para analise
profile.df <- data.frame()
profile.df <- profile.df.professores %>% 
  select(idLattes, nome, resumo_cv, senioridade) %>% 
  left_join(
    profile.df.orientacoes %>% 
      select(orientacao, idLattes) %>% 
      filter(!grepl("EM_ANDAMENTO", orientacao)) %>% 
      group_by(idLattes) %>% 
      count(orientacao) %>% 
      spread(key = orientacao, value = n), 
    by = "idLattes") %>% 
  left_join(
    profile.df.publicacoes %>% 
      select(tipo_producao, idLattes) %>% 
      filter(!grepl("ARTIGO_ACEITO", tipo_producao)) %>% 
      group_by(idLattes) %>% 
      count(tipo_producao) %>% 
      spread(key = tipo_producao, value = n), 
    by = "idLattes") %>% 
  left_join(
    profile.df.areas.de.atuacao %>% 
      select(area, idLattes) %>% 
      group_by(idLattes) %>% 
      summarise(num_areas = n_distinct(area)), 
    by = "idLattes")

grande_areas_atuacao <- profile %>%
  sapply(function(x)
    unique(x$areas_de_atuacao$grande_area)) %>%
  unlist() %>% table() %>% sort(decreasing = TRUE) %>%
  as.data.frame() %>% filter(!. == "")

quantidade_grande_areas_atuacao <- sum(grande_areas_atuacao$Freq)
grande_areas_atuacao <-
  mutate(grande_areas_atuacao,
         percent = round(grande_areas_atuacao$Freq / quantidade_grande_areas_atuacao * 100, 0))
colnames(grande_areas_atuacao) <- c("GrandeAreas", "Quantidade", "Porcentagem")

areas_atuacao <- profile %>%
  sapply(function(x)
    unique(x$areas_de_atuacao$area)) %>%
  unlist() %>% table() %>% sort(decreasing = TRUE) %>%
  as.data.frame() %>% filter(!. == "")

quantidade_areas_atuacao <- sum(areas_atuacao$Freq)
areas_atuacao <-
  mutate(areas_atuacao,
         percent = round(areas_atuacao$Freq / quantidade_areas_atuacao * 100, 0))
colnames(areas_atuacao) <- c("Areas", "Quantidade", "Porcentagem")

subarea <- profile %>%
  sapply(function(x)
    (x$areas_de_atuacao$sub_area)) %>%
  unlist() %>% table() %>%
  sort(decreasing = TRUE) %>%   as.data.frame() %>%
  filter(!. == "") %>% head(6)

quantidade_subarea <- sum(subarea$Freq)
subarea <-
  mutate(subarea, percent = round(subarea$Freq / quantidade_subarea * 100, 0))
colnames(subarea) <- c("Subarea", "Quantidade", "Porcentagem")

especialidades_frequentes <- profile %>%
  sapply(function(x)
    unique(x$areas_de_atuacao$especialidade)) %>%
  unlist() %>% table() %>% sort(decreasing = TRUE) %>%
  as.data.frame() %>% filter(!. == "") %>% head(10)

quantidade_especialidades <- sum(especialidades_frequentes$Freq)
especialidades_frequentes <-
  mutate(
    especialidades_frequentes,
    percent = round(
      especialidades_frequentes$Freq / quantidade_especialidades * 100,
      0
    )
  )
colnames(especialidades_frequentes) <-
  c("Especialidade", "Quantidade", "Porcentagem")

profile.areas <- profile.df.areas.de.atuacao %>%
  left_join(profile.df, by = "idLattes") %>%
  rowwise() %>% #realizar sum() corretamente
  mutate(
    orientacoes_concluidas = sum(
      ORIENTACAO_CONCLUIDA_DOUTORADO,
      ORIENTACAO_CONCLUIDA_POS_DOUTORADO,
      ORIENTACAO_CONCLUIDA_MESTRADO,
      OUTRAS_ORIENTACOES_CONCLUIDAS,
      na.rm = TRUE
    )
  ) %>%
  mutate(
    publicacoes = sum(
      CAPITULO_DE_LIVRO,
      EVENTO,
      PERIODICO,
      LIVRO,
      TEXTO_EM_JORNAIS,
      OUTRAS_PRODUCOES,
      na.rm = TRUE
    )
  ) %>%
  select(
    idLattes,
    grande_area,
    area,
    sub_area,
    especialidade,
    orientacoes_concluidas,
    publicacoes
  )
class(profile.areas) <- c("tbl_df", "data.frame") #desfazer rowwise