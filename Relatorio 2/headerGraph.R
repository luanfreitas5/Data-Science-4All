#Leitura dos arquivos formato graph e análise dos dados

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

#Ler o arquivo profile e transformar no formato igraph
graph <- fromJSON("Geotecnia/graph.json")
g <- g.ls2ig(graph)

geotecnia_nodes <- graph$nodes
geotecnia_relations <- graph$links

igraph <-
  graph_from_data_frame(geotecnia_relations,
                        directed = TRUE,
                        vertices = geotecnia_nodes)

colnames(geotecnia_nodes) <- c("IdLattes",
                               "Index",
                               "Docente")

# extrai perfis dos professores
profile <- jsonlite::fromJSON("Geotecnia/profile.json")
profile.df.professores <- extrai.perfis(profile)

# extrai producao bibliografica de todos os professores
profile.df.publicacoes <- extrai.producoes(profile)

#extrai orientacoes
profile.df.orientacoes <- extrai.orientacoes(profile)

#extrai areas de atuacao
profile.df.areas.de.atuacao <- extrai.areas.atuacao(profile)

#cria arquivo com dados quantitativos para análise
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
    by = "idLattes"
  ) %>%
  left_join(
    profile.df.publicacoes %>%
      select(tipo_producao, idLattes) %>%
      filter(!grepl("ARTIGO_ACEITO", tipo_producao)) %>%
      group_by(idLattes) %>%
      count(tipo_producao) %>%
      spread(key = tipo_producao, value = n),
    by = "idLattes"
  ) %>%
  left_join(
    profile.df.areas.de.atuacao %>%
      select(area, idLattes) %>%
      group_by(idLattes) %>%
      summarise(n_distinct(area)),
    by = "idLattes"
  )

#Adicionando informações ao grafo
V(g)$orient_dout <- profile.df$ORIENTACAO_CONCLUIDA_DOUTORADO
V(g)$orient_mest <- profile.df$ORIENTACAO_CONCLUIDA_MESTRADO
V(g)$publicacao <- profile.df$PERIODICO
V(g)$eventos <- profile.df$EVENTO

V(g)$degree <- degree(g)
V(g)$betweenness <- betweenness(g, normalized = TRUE)
V(g)$closeness <- closeness(g, normalized = TRUE)
V(g)$eigen <- eigen_centrality(g)$vector
V(g)$cluster <- cluster_leading_eigen(g)$membership


#Filtrando somente os pesquisadores com Grande Área Ciências Exatas e da Terra
sub.graph <- (
  profile.df.areas.de.atuacao %>%
    filter(grande_area == "CIENCIAS_EXATAS_E_DA_TERRA") %>%
    select(idLattes) %>% unique()
)[, 1]

g2 <- induced_subgraph(g, sub.graph)

# Perform fast-greedy community detection on network graph
kc = fastgreedy.community(g)
