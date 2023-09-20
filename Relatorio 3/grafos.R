#Leitura dos arquivos formato grafos e an√°lise dos dados

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
file.info("Geotecnia/graph.json")
grafos <- fromJSON("Geotecnia/graph.json")
igraph <- g.ls2ig(grafos)

geotecniaNodes <- grafos$nodes
geotecniaLinks <- grafos$links

head(geotecniaNodes)
head(geotecniaLinks)

colnames(geotecniaNodes) <- c("IdLattes",
                              "Index",
                              "Docente")

kable(geotecniaNodes[, c(2, 1, 3)], caption = "Tabela de referÍncia da an·lise de redes")

dataFramegrafos <-
  graph_from_data_frame(geotecniaLinks,
                        directed = TRUE,
                        vertices = grafos$nodes)
plot(dataFramegrafos)

#Cria arquivo com quantitativo de dados para adicionar ao grafo
#Arquivo perfis por Curr√?culo
# An√°lise dos dados em formato Data Frame
# Usar as funcionalidades que est„o no arquivo elatttes.ls2df.R

perfis <- fromJSON("Geotecnia/profile.json")
# extrai perfis dos professores
dataFrameProfessores <- extrai.perfis(perfis)
# extrai producao bibliografica de todos os professores
dataFramePublicacoes <- extrai.producoes(perfis) %>%
  select(tipo_producao, everything()) %>% arrange(tipo_producao)
dataFramePublicacoes$tipo_producao[grepl("DEMAIS", dataFramePublicacoes$tipo_producao)] <-
  "OUTRAS_PRODUCOES"
dataFramePublicacoes %>% select(pais_de_publicacao) %>% distinct()
#extrai orientacoes
dataFrameOrientacoes <- extrai.orientacoes(perfis) %>%
  select(id_lattes_orientadores,
         natureza,
         ano,
         orientacao,
         everything()) %>%
  mutate(Status = ifelse(grepl("CONCLUIDA", orientacao), "ConcluÌda", "Em andamento")) %>%
  mutate(
    Natureza = case_when(
      grepl("MESTRADO", str_to_upper(natureza)) ~ "Mestrado",
      grepl("P”S-DOUTORADO", str_to_upper(natureza)) ~ "PÛs-doutorado",
      grepl("DOUTORADO", str_to_upper(natureza)) ~ "Doutorado",
      grepl("INICIACAO", str_to_upper(natureza)) ~ "IniciaÁ„o CientÌfica",
      grepl("INICIA«√O", str_to_upper(natureza)) ~ "IniciaÁ„o CientÌfica",
      TRUE ~ "Outras naturezas"
    )
  )
dataFrameOrientacoes %>% select(natureza, Natureza) %>% distinct() %>% arrange(Natureza)
str_to_upper("pÛs-doutorado")
#extrai areas de atuacao
dataFrameAreasAtuacao <- extrai.areas.atuacao(perfis) %>%
  select(idLattes, everything())
#cria arquivo com dados quantitativos para analise
dataFramePerfis <- data.frame()
dataFramePerfis <- dataFrameProfessores %>%
  select(idLattes, nome, resumo_cv, senioridade) %>%
  left_join(
    dataFrameOrientacoes %>%
      select(orientacao, idLattes) %>%
      filter(!grepl("EM_ANDAMENTO", orientacao)) %>%
      group_by(idLattes) %>%
      count(orientacao) %>%
      spread(key = orientacao, value = n),
    by = "idLattes"
  ) %>%
  left_join(
    dataFramePublicacoes %>%
      select(tipo_producao, idLattes) %>%
      filter(!grepl("ARTIGO_ACEITO", tipo_producao)) %>%
      group_by(idLattes) %>%
      count(tipo_producao) %>%
      spread(key = tipo_producao, value = n),
    by = "idLattes"
  ) %>%
  left_join(
    dataFrameAreasAtuacao %>%
      select(area, idLattes) %>%
      group_by(idLattes) %>%
      summarise(num_areas = n_distinct(area)),
    by = "idLattes"
  )
listviewer::jsonedit(dataFramePerfis)

#Adicionando informa√ß√µes ao grafo
V(igraph)$orient_dout <-
  dataFramePerfis$ORIENTACAO_CONCLUIDA_DOUTORADO
V(igraph)$orient_mest <-
  dataFramePerfis$ORIENTACAO_CONCLUIDA_MESTRADO
V(igraph)$publicacao <- dataFramePerfis$PERIODICO
V(igraph)$eventos <- dataFramePerfis$EVENTO

V(igraph)$degree <- degree(igraph)
V(igraph)$betweenness <- betweenness(igraph, normalized = TRUE)
V(igraph)$closeness <- closeness(igraph, normalized = TRUE)
V(igraph)$eigen <- eigen_centrality(igraph)$vector
V(igraph)$cluster <- cluster_leading_eigen(igraph)$membership

#Escrever o grafo para leitura em outro software
write.graph(igraph, "grafos.lattes.graphml", format = "graphml")

#Filtrando somente os pesquisadores com Grande √Årea Ci√™ncias Exatas e da Terra
grafosAreas <- (
  dataFrameAreasAtuacao %>%
    filter(grande_area == "CIENCIAS_EXATAS_E_DA_TERRA") %>%
    select(idLattes) %>% unique()
)[, 1]

igrafosAreas <- induced_subgraph(igraph, grafosAreas)

plot(igraph)
glimpse(V(igraph)$orient_dout)


# Grafo de colaboraÁ„o por quantidade de relaÁıes
plot(
  igraph,
  vertex.size = V(igraph)$degree * 1.5,
  vertex.label = geotecniaNodes$Index,
  layout = layout_nicely(igraph),
  vertex.label.cex = 0.6
)

# Grafo de colaboraÁ„o por quantidade de publicaÁıes
plot(
  igraph,
  vertex.size = na.omit(round(V(igraph)$publicacao / 7) * 3),
  vertex.label = geotecniaNodes$Index,
  layout = layout_nicely(igraph),
  vertex.label.cex = 0.6
)
# Perform fast-greedy community detection on network grafos
grafosComunidades = fastgreedy.community(igraph)

# Determine sizes of each community
sizes(grafosComunidades)

# Determine which individuals belong to which community
membership(grafosComunidades)

# Comunidades geradas pelas colaboraÁıes
plot(
  grafosComunidades,
  igraph,
  vertex.label = geotecniaNodes$Index,
  layout = layout_nicely(igraph)
)
