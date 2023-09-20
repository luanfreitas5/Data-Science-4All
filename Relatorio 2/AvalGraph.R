#Leitura dos arquivos formato graph e an√°lise dos dados

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
graph <- fromJSON("Geotecnia/graph.json")
g <- g.ls2ig(graph)

geotecnia_nodes <- graph$nodes
geotecnia_relations <- graph$links

head(geotecnia_nodes)
head(geotecnia_relations)

igraph <-
  graph_from_data_frame(geotecnia_relations,
                        directed = TRUE,
                        vertices = geotecnia_nodes)
plot(igraph)

colnames(geotecnia_nodes) <- c("IdLattes",
                               "Index",
                               "Docente")

kable(geotecnia_nodes[, c(2, 1, 3)], caption = "Tabela de referÍncia da an·lise de redes")

#Cria arquivo com quantitativo de dados para adicionar ao grafo
#Arquivo Profile por Curr√?culo
# extrai perfis dos professores
profile <- jsonlite::fromJSON("Geotecnia/profile.json")
#str(profile)
profile.df.professores <- extrai.perfis(profile)
glimpse(profile.df.professores)

# extrai producao bibliografica de todos os professores
profile.df.publicacoes <- extrai.producoes(profile)
glimpse(profile.df.publicacoes)

#extrai orientacoes
profile.df.orientacoes <- extrai.orientacoes(profile)
glimpse(profile.df.orientacoes)

#extrai areas de atuacao
profile.df.areas.de.atuacao <- extrai.areas.atuacao(profile)
glimpse(profile.df.areas.de.atuacao)

#cria arquivo com dados quantitativos para an√°lise
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

glimpse(profile.df)
listviewer::jsonedit(profile.df)

#Adicionando informa√ß√µes ao grafo
V(g)$orient_dout <- profile.df$ORIENTACAO_CONCLUIDA_DOUTORADO
V(g)$orient_mest <- profile.df$ORIENTACAO_CONCLUIDA_MESTRADO
V(g)$publicacao <- profile.df$PERIODICO
V(g)$eventos <- profile.df$EVENTO

V(g)$degree <- degree(g)
V(g)$betweenness <- betweenness(g, normalized = TRUE)
V(g)$closeness <- closeness(g, normalized = TRUE)
V(g)$eigen <- eigen_centrality(g)$vector
V(g)$cluster <- cluster_leading_eigen(g)$membership

#Escrever o grafo para leitura em outro software
write.graph(g, "graph.lattes.graphml", format = "graphml")

#Filtrando somente os pesquisadores com Grande √Årea Ci√™ncias Exatas e da Terra
sub.graph <- (
  profile.df.areas.de.atuacao %>%
    filter(grande_area == "CIENCIAS_EXATAS_E_DA_TERRA") %>%
    select(idLattes) %>% unique()
)[, 1]

g2 <- induced_subgraph(g, sub.graph)

plot(V(g))
glimpse(V(g)$orient_dout)


# Grafo de colaboraÁ„o por quantidade de relaÁıes
plot(
  g,
  vertex.size = V(g)$degree * 1.5,
  vertex.label = geotecnia_nodes$label,
  layout = layout_nicely(g),
  vertex.label.cex = 0.6
)

# Grafo de colaboraÁ„o por quantidade de publicaÁıes
plot(
  g,
  vertex.size = na.omit(round(V(g)$publicacao / 7) * 3),
  vertex.label = geotecnia_nodes$label,
  layout = layout_nicely(g),
  vertex.label.cex = 0.6
)
# Perform fast-greedy community detection on network graph
kc = fastgreedy.community(g)

# Determine sizes of each community
sizes(kc)

# Determine which individuals belong to which community
membership(kc)

# Comunidades geradas pelas colaboraÁıes
plot(kc,
     g,
     vertex.label = geotecnia_nodes$label,
     layout = layout_nicely(g))
