#Leitura de arquivos do perfis.json
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
colnames(perfis$areas_de_atuacao) <- perfis$nomes
#Ler o arquivo perfis
file.info("Geotecnia/profile.json")
perfis <- fromJSON("Geotecnia/profile.json")
#glimpse(perfis[[1]], width = 30)

#str(perfis)

#Visualizar o arquivo no formato list
listviewer::jsonedit(perfis)

#número de arquivos / pessoas / pesquisadores
length(perfis)

#### Nome do Pesquisador
# Nome de um pesquisador específico ou de um conjunto de pesquisadores
perfis[[1]]$nome
perfis$`0000507838194708`$nome

head(sapply(perfis, function(x)
  (x$nome)), 10)


for (i in 1:5) {
  print(names(perfis[i]))
  print(perfis[[i]]$nome)
}

# Número de áreas de atuação cumulativo
sum(sapply(perfis, function(x)
  nrow(x$areas_de_atuacao)))

# Número de áreas de atuação por pessoa
table(unlist(sapply(perfis, function(x)
  nrow(x$areas_de_atuacao))))

# Número de pessoas por grande area
table(unlist(sapply(perfis, function(x)
  (x$areas_de_atuacao$grande_area))))

# Número de pessoas que produziram os específicos tipos de produção
table(unlist(sapply(perfis, function(x)
  names(x$producao_bibiografica))))

# Número de publicações por tipo
sum(sapply(perfis, function(x)
  length(x$producao_bibiografica$ARTIGO_ACEITO$ano)))

sum(sapply(perfis, function(x)
  length(x$producao_bibiografica$CAPITULO_DE_LIVRO$ano)))

sum(sapply(perfis, function(x)
  length(x$producao_bibiografica$LIVRO$ano)))

sum(sapply(perfis, function(x)
  length(x$producao_bibiografica$PERIODICO$ano)))

sum(sapply(perfis, function(x)
  length(x$producao_bibiografica$TEXTO_EM_JORNAIS$ano)))

# Número de pessoas por quantitativo de produções por pessoa 0 = 1; 1 = 2...
table(unlist(sapply(perfis, function(x)
  length(x$producao_bibiografica$ARTIGO_ACEITO$ano))))

table(unlist(sapply(perfis, function(x)
  length(x$producao_bibiografica$CAPITULO_DE_LIVRO$ano))))

table(unlist(sapply(perfis, function(x)
  length(x$producao_bibiografica$LIVRO$ano))))

table(unlist(sapply(perfis, function(x)
  length(x$producao_bibiografica$PERIODICO$ano))))

table(unlist(sapply(perfis, function(x)
  length(x$producao_bibiografica$TEXTO_EM_JORNAIS$ano))))

# Número de produções por ano
table(unlist(sapply(perfis, function(x)
  (x$producao_bibiografica$ARTIGO_ACEITO$ano))))

table(unlist(sapply(perfis, function(x)
  (x$producao_bibiografica$CAPITULO_DE_LIVRO$ano))))

table(unlist(sapply(perfis, function(x)
  (x$producao_bibiografica$LIVRO$ano))))

table(unlist(sapply(perfis, function(x)
  (x$producao_bibiografica$PERIODICO$ano))))

table(unlist(sapply(perfis, function(x)
  (x$producao_bibiografica$TEXTO_EM_JORNAIS$ano))))

# Número de pessoas que realizaram diferentes tipos de orientações
length(unlist(sapply(perfis, function(x)
  names(x$orientacoes_academicas))))

# Número de pessoas por tipo de orientação
table(unlist(sapply(perfis, function(x)
  names(x$orientacoes_academicas))))

#Número de orientações concluidas
sum(sapply(perfis, function(x)
  length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$ano)))

sum(sapply(perfis, function(x)
  length(
    x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano
  )))

sum(sapply(perfis, function(x)
  length(
    x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_POS_DOUTORADO$ano
  )))

table(unlist(sapply(perfis, function(x)
  length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$ano))))

table(unlist(sapply(perfis, function(x)
  length(
    x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano
  ))))

table(unlist(sapply(perfis, function(x)
  length(
    x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_POS_DOUTORADO$ano
  ))))

table(unlist(sapply(perfis, function(x)
  (x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$ano))))

table(unlist(sapply(perfis, function(x)
  (
    x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano
  ))))

table(unlist(sapply(perfis, function(x)
  (
    x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_POS_DOUTORADO$ano
  ))))


# Resumos #####
# Resumos do currículo de um pesquisador ou de um conjunto
perfis[[1]]$resumo_cv
perfis$`0000507838194708`$resumo_cv

head(sapply(perfis, function(x)
  (x$resumo_cv)), 2)

ids <- c("0000507838194708", "0010973626622666", "0017467628165816")
sapply(perfis[(names(perfis) %in% ids)], function(x)
  (x$resumo_cv))

for (i in 1:5) {
  print(names(perfis[i]))
  print(perfis[[i]]$resumo_cv)
}

# Areas de Atuação #####
# Nome dos subitens de "areas_de_atuacao"
names(perfis[[1]]$areas_de_atuacao)

# Nomes e contagem dos itens armazenados em cada subitem
unique(unlist(sapply(perfis, function(x)
  (x$areas_de_atuacao$grande_area))))
perfis %>%
  sapply(function(x)
    unique(x$areas_de_atuacao$grande_area)) %>%
  unlist() %>% table() %>% sort(decreasing = TRUE) %>% as.data.frame() %>% filter(!. == "")

length(unique(unlist(
  sapply(perfis, function(x)
    x$areas_de_atuacao$area)
)))
perfis %>%
  sapply(function(x)
    unique(x$areas_de_atuacao$area)) %>%
  unlist() %>% table() %>% sort(decreasing = TRUE) %>%
  as.data.frame() %>% filter(!. == "")

perfis %>%
  sapply(function(x)
    unique(x$areas_de_atuacao$sub_area)) %>%
  unlist() %>% table() %>% sort(decreasing = TRUE) %>%
  as.data.frame() %>% filter(!. == "") %>% head(40)

perfis %>%
  sapply(function(x)
    unique(x$areas_de_atuacao$especialidade)) %>%
  unlist() %>% table() %>% sort(decreasing = TRUE) %>%
  as.data.frame() %>% filter(!. == "") %>% head(40)

# Avaliação de Areas de conjunto de pesquisadores
perfis[[1]]$areas_de_atuacao

#com repetição de itens
head(sapply(perfis, function(x)
  unlist(x$areas_de_atuacao$area)), 2)

ids <-
  c("0000507838194708",
    "0010973626622666",
    "0011938955607677",
    "0037568024594010")
#sem repetição de itens
sapply(perfis[(names(perfis) %in% ids)], function(x)
  unique(x$areas_de_atuacao$grande_area))
#em outro formato usando magrittr
perfis[(names(perfis) %in% ids)] %>% sapply(function(x)
  (x$areas_de_atuacao$grande_area)) %>% sapply(unique)

#Número de "areas_de_atuação" Especialidade... por pessoa
perfis[(names(perfis) %in% ids)] %>%
  sapply(function(x)
    unlist(x$areas_de_atuacao$especialidade)) %>%
  sapply(unique) %>%
  sapply(length)

# Número de "areas_de_atuação" acumulado
perfis %>%
  sapply(function(x)
    unlist(x$areas_de_atuacao$especialidade)) %>%
  sapply(unique) %>%
  sapply(length) %>%
  table()

# EndereÃ§o Profissional #####
## Nome dos subitens de "endereco_profissional"
names(perfis[[1]]$endereco_profissional)

#lista de instituiÃ§Ãµes nos currí?culos
unique(unlist(sapply(perfis, function(x)
  (x$endereco_profissional$instituicao))))
#Número de currí?culos com instituicao com Universidade de BrasÃ?lia
perfis %>%
  sapply(function(x)
    (x$endereco_profissional$instituicao)) %in% "Universidade de BrasÃ?lia" %>%
  sum()

#Lista de Unidades
head(sort(unique(unlist(
  sapply(perfis, function(x)
    (x$endereco_profissional$unidade))
)), decreasing = TRUE), 40)

#Contagem de OrgÃ£os / Institutos
head(sort(table(unlist(
  sapply(perfis, function(x)
    (x$endereco_profissional$orgao))
)), decreasing = TRUE), 20)

# ProduÃ§Ã£o BibliogrÃ¡fica #####
#AnÃ¡lise da ProduÃ§Ã£o BibliogrÃ¡fica
#Todos os tipos de produÃ§Ã£o
unique(unlist(sapply(perfis, function(x)
  names(x$producao_bibiografica))))

# Media de produÃ§Ã£o por pesquisador
# Aqui Ã© usado a media pois existem duplicaÃ§Ãµes caso pesquisadores tenham escrito o mesmo artigo
(perfis %>%
    sapply(
      function(x)
        length(x$producao_bibiografica$PERIODICO$natureza)
    ) %>%
    sum()) / length(perfis)

(perfis %>%
    sapply(function(x)
      length(
        x$producao_bibiografica$EVENTO$natureza
      )) %>%
    sum()) / length(perfis)

(perfis %>%
    sapply(function(x)
      length(x$producao_bibiografica$LIVRO$natureza)) %>%
    sum()) / length(perfis)

# Número de pessoas por quantitativo de produções
perfis %>%
  sapply(function(x)
    length(x$producao_bibiografica$PERIODICO$natureza)) %>%
  unlist() %>% table()

perfis %>%
  sapply(function(x)
    length(x$producao_bibiografica$EVENTO$natureza)) %>%
  unlist() %>% table()

# Nome de pesquisadores com quantitativo de produÃ§Ã£o especÃ?fica (20)
perfis[(perfis %>%
          sapply(
            function(x)
              length(x$producao_bibiografica$PERIODICO$natureza)
          )) %in% 20] %>%
  sapply(function(x)
    (x$nome))

# Nome de pesquisadores com busca por palavra em tÃ?tulo de publicaÃ§Ã£o
perfis[perfis %>%
         sapply(function(x)
           (x$producao_bibiografica$PERIODICO$titulo)) %>%
         grepl(pattern = "GDF")] %>%
  sapply(function(x)
    (x$nome))

# Quantitativo de eventos por pais
#perfis%>%
#  sapply(function(x)
#    (x$producao_bibiografica$EVENTO$pais_do_evento)) %>%
#  unlist() %>% summary()
#  ggplot(aes())

# Nome de pesquisadores com busca por pais de evento
perfis[perfis %>%
         sapply(function(x)
           (x$producao_bibiografica$EVENTO$pais_do_evento)) %>%
         sapply('%in%', "RomÃªnia") %>%
         sapply(any)] %>%
  sapply(function(x)
    (x$nome))

# Número de produções em eventos por ano
perfis %>%
  sapply(function(x)
    (x$producao_bibiografica$EVENTO$ano)) %>%
  unlist() %>% table()
# em outro formato para livros
table(unlist(sapply(perfis, function(x)
  (x$producao_bibiografica$LIVRO$ano))))

# Número de produções por ano para os 100 primeiros pesquisadores da lista
perfis[1:100] %>%
  sapply(function(x)
    (x$producao_bibiografica$TEXTO_EM_JORNAIS$ano)) %>%
  unlist() %>% table()

# Orientações #####
#AnÃ¡lise das Orientações
# Tipos de orientaÃ§Ã£o
perfis %>%
  sapply(function(x)
    names(x$orientacoes_academicas)) %>%
  unlist() %>% unique()

# Media de Orientações por pesquisador
# Aqui são usado a media pois existem duplicaÃ§Ãµes caso pesquisadores tenham orientado o mesmo trabalho
(perfis %>%
    sapply(
      function(x)
        length(
          x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$natureza
        )
    ) %>%
    sum()) / length(perfis)

(perfis %>%
    sapply(
      function(x)
        length(
          x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$natureza
        )
    ) %>%
    sum()) / length(perfis)

# Número de pessoas por quantitativo de produção
perfis %>%
  sapply(
    function(x)
      length(
        x$orientacoes_academicas$OUTRAS_ORIENTACOES_CONCLUIDAS$natureza
      )
  ) %>%
  unlist() %>% table()

# Número de pessoas por tipo de orientaÃ§Ã£o
perfis %>%
  sapply(function(x)
    names(x$orientacoes_academicas)) %>%
  unlist() %>% table()

# Número de produções em Orientações por ano
perfis %>%
  sapply(function(x)
    (
      x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano
    )) %>%
  unlist() %>% table()

# Data Frame ####
# AnÃ¡lise dos dados em formato Data Frame
# Usar as funcionalidades que estão no arquivo elatttes.ls2df.R
#Arquivo perfis por currículo
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
  mutate(Status = ifelse(grepl("CONCLUIDA", orientacao), "Concluída", "Em andamento")) %>%
  mutate(
    Natureza = case_when(
      grepl("MESTRADO", str_to_upper(natureza)) ~ "Mestrado",
      grepl("PÓS-DOUTORADO", str_to_upper(natureza)) ~ "Pós-doutorado",
      grepl("DOUTORADO", str_to_upper(natureza)) ~ "Doutorado",
      grepl("INICIACAO", str_to_upper(natureza)) ~ "Iniciação Científica",
      grepl("INICIAÇÃO", str_to_upper(natureza)) ~ "Iniciação Científica",
      TRUE ~ "Outras naturezas"
    )
  )
dataFrameOrientacoes %>% select(natureza, Natureza) %>% distinct() %>% arrange(Natureza)
str_to_upper("pós-doutorado")
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
glimpse(dataFramePerfis)

grandeAreasAtuacao <- perfis %>%
  sapply(function(x)
    unique(x$areas_de_atuacao$grande_area)) %>%
  unlist() %>% table() %>% sort(decreasing = TRUE) %>%
  as.data.frame() %>% filter(!. == "")

quantidadeGrandeAreasAtuacao <- sum(grandeAreasAtuacao$Freq)
grandeAreasAtuacao <-
  mutate(
    grandeAreasAtuacao,
    percent = round(grandeAreasAtuacao$Freq / quantidadeGrandeAreasAtuacao * 100,
                    0)
  )
colnames(grandeAreasAtuacao) <-
  c("GrandeAreas", "Quantidade", "Porcentagem")

areasAtuacao <- perfis %>%
  sapply(function(x)
    unique(x$areas_de_atuacao$area)) %>%
  unlist() %>% table() %>% sort(decreasing = TRUE) %>%
  as.data.frame() %>% filter(!. == "")

quantidadeAreasAtuacao <- sum(areasAtuacao$Freq)
areasAtuacao <-
  mutate(areasAtuacao,
         percent = round(areasAtuacao$Freq / quantidadeAreasAtuacao * 100, 0))
colnames(areasAtuacao) <- c("Areas", "Quantidade", "Porcentagem")

subArea <- perfis %>%
  sapply(function(x)
    (x$areas_de_atuacao$sub_area)) %>%
  unlist() %>% table() %>%
  sort(decreasing = TRUE) %>%   as.data.frame() %>%
  filter(!. == "") %>% head(6)

quantidadeSubArea <- sum(subArea$Freq)
subArea <-
  mutate(subArea, percent = round(subArea$Freq / quantidadeSubArea * 100, 0))
colnames(subArea) <- c("subArea", "Quantidade", "Porcentagem")

especialidadesFrequentes <- perfis %>%
  sapply(function(x)
    unique(x$areas_de_atuacao$especialidade)) %>%
  unlist() %>% table() %>% sort(decreasing = TRUE) %>%
  as.data.frame() %>% filter(!. == "") %>% head(10)

quantidadeEspecialidades <- sum(especialidadesFrequentes$Freq)
especialidadesFrequentes <-
  mutate(
    especialidadesFrequentes,
    percent = round(
      especialidadesFrequentes$Freq / quantidadeEspecialidades * 100,
      0
    )
  )
colnames(especialidadesFrequentes) <-
  c("Especialidade", "Quantidade", "Porcentagem")

dataFrameAreas <- dataFrameAreasAtuacao %>%
  left_join(dataFramePerfis, by = "idLattes") %>%
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
class(dataFrameAreas) <- c("tbl_df", "data.frame") #desfazer rowwise

# Visualização ####
# Número de pessoas por area de atuação (grande Ã¡rea, area, sub_area, especialidade)
perfis %>%
  sapply(function(x)
    unique(x$areas_de_atuacao$grande_area)) %>%
  unlist() %>% table() %>% sort() %>% as.data.frame() %>% filter(!. == "") %>%
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "red1",
                                          alpha = 0.8,
                                          width = 0.8) + coord_flip() + geom_text(aes(label = Freq),
                                                                                  hjust = -0.2,
                                                                                  vjust = 0.5,
                                                                                  size = 3.5) +
  labs(title = "Número de Pessoas por Grande Área Atuação", y = "Quantidade", x =
         "Grande Área") + theme_bw() + scale_y_continuous() +
  scale_x_discrete(
    labels = c(
      'CIENCIAS_DA_SAUDE' = 'Ciências da Saúde',
      'CIENCIAS_BIOLOGICAS' = 'Ciências Biológicas',
      'CIENCIAS_HUMANAS' = 'Ciências Humanas',
      "CIENCIAS_EXATAS_E_DA_TERRA" = "Ciências Exatas e da Terra",
      "CIENCIAS_SOCIAIS_APLICADAS" = "Ciências Sociais Aplicadas",
      "CIENCIAS_AGRARIAS" = "Ciências Agrárias",
      "OUTROS" = "Outros",
      "ENGENHARIAS" = "Engenharias",
      "LINGUISTICA_LETRAS_E_ARTES" = "Linguística, Letras e Artes"
    )
  )

ggplot(grandeAreasAtuacao,
       aes(x = "", y = Quantidade, fill = GrandeAreas)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0, direction = -1) +
  labs(title = "Número de Pessoas por Grande Área Atuação") +
  geom_text(
    data = grandeAreasAtuacao,
    aes(
      x = "",
      y = Quantidade,
      label = paste(Porcentagem, "%")
    ),
    position = position_stack(vjust = 0.5),
    size = 3
  )

perfis %>%
  sapply(function(x)
    unique(x$areas_de_atuacao$area)) %>%
  unlist() %>% table() %>% sort() %>% as.data.frame() %>% filter(!. == "") %>% tail(20) %>%
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "red1",
                                          alpha = 0.8,
                                          width = 0.8) + coord_flip() +
  labs(title = "Número de Pessoas por Área Atuação", x = "Área de atuação", y =
         "Quantidade") +
  geom_text(aes(label = Freq),
            hjust = -0.2,
            vjust = 0.3,
            size = 3.5) +
  scale_y_continuous(limits = c(0, 800)) + theme_bw()

ggplot(areasAtuacao, aes(x = "", y = Quantidade, fill = Areas)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0, direction = -1) +
  labs(title = "Número de Pessoas por Área Atuação") +
  geom_text(
    data = areasAtuacao,
    aes(
      x = "",
      y = Quantidade,
      label = paste(Porcentagem, "%")
    ),
    position = position_stack(vjust = 0.5),
    size = 3
  )

perfis %>%
  sapply(function(x)
    unique(x$areas_de_atuacao$sub_area)) %>%
  unlist() %>% table() %>% sort() %>% as.data.frame() %>% filter(!. == "") %>% tail(30) %>%
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "red1",
                                          alpha = 0.8,
                                          width = 0.8) + coord_flip() +
  geom_text(aes(label = Freq),
            hjust = -0.2,
            vjust = 0.3,
            size = 3.5) +
  labs(title = "Número de Pessoas por Sub Área atuação", x = "Sub Área", y =
         "Quantidade") +
  scale_y_continuous(limits = c(0, 300)) + theme_bw()


ggplot(subArea, aes(x = "", y = Quantidade, fill = subArea)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0, direction = -1) +
  labs(title = "Número de Pessoas por Sub Área atuação") +
  geom_text(
    data = subArea,
    aes(
      x = "",
      y = Quantidade,
      label = paste(Porcentagem, "%")
    ),
    position = position_stack(vjust = 0.5),
    size = 3
  )

perfis %>%
  sapply(function(x)
    unique(x$areas_de_atuacao$especialidade)) %>%
  unlist() %>% table() %>% sort() %>% as.data.frame() %>% filter(!. == "") %>% tail(29) %>%
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "red1",
                                          alpha = 0.8,
                                          width = 0.8) + coord_flip() +
  labs(title = "Número de Pessoas por Especialidade", x = "Especialidade", y =
         "Quantidade") +
  geom_text(aes(label = Freq),
            hjust = -0.2,
            vjust = 0.3,
            size = 3.0) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 150))


ggplot(especialidadesFrequentes,
       aes(x = "", y = Quantidade, fill = Especialidade)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0, direction = -1) +
  labs(title = "Número de Pessoas por Especialidade") +
  geom_text(
    data = especialidadesFrequentes,
    aes(
      x = "",
      y = Quantidade,
      label = paste(Porcentagem, "%")
    ),
    position = position_stack(vjust = 0.5),
    size = 3
  )

ggplot(especialidadesFrequentes,
       aes(x = "", y = Quantidade, fill = Especialidade)) +
  labs(title = "Número de Pessoas por Especialidade") +
  geom_bar(width = 1, stat = "identity") +
  geom_text(
    data = especialidadesFrequentes,
    aes(x = "", y = Quantidade, label = Quantidade),
    position = position_stack(vjust = 0.5),
    size = 3
  )

# Número de Ã¡reas de atuação por pessoa (grande Ã¡rea, area, sub_area, especialidade)
perfis %>%
  sapply(function(x)
    length(unique(x$areas_de_atuacao$grande_area))) %>%
  unlist() %>% table() %>% sort() %>% rev() %>% as.data.frame() %>%
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "blue1", alpha = 0.9) +
  geom_text(aes(label = Freq), size = 3.5, vjust = -1) +
  labs(title = "Número de Pessoas por Quantitativo de Grande Áreas",
       y = "Número de Pessoas", x = "Quantitativo de Grande Áreas") + theme_bw()

perfis %>%
  sapply(function(x)
    length(unique(x$areas_de_atuacao$area))) %>%
  unlist() %>% table() %>% sort() %>% rev() %>% as.data.frame() %>%
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "blue1", alpha = 0.9) +
  geom_text(aes(label = Freq), size = 3.5, vjust = -1) +
  labs(title = "Número de Pessoas por Quantitativo de Áreas",
       y = "Número de Pessoas", x = "Quantitativo de Áreas") + theme_bw()

perfis %>%
  sapply(function(x)
    length(unique(x$areas_de_atuacao$sub_area))) %>%
  unlist() %>% table() %>% sort() %>% rev() %>% as.data.frame() %>%
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "blue1", alpha = 0.9) +
  geom_text(aes(label = Freq), size = 3.5, vjust = -1) +
  labs(title = "Número de Pessoas por Quantitativo de Sub Áreas",
       y = "Número de Pessoas", x = "Quantitativo de Sub Áreas") + theme_bw()

perfis %>%
  sapply(function(x)
    length(unique(x$areas_de_atuacao$especialidade))) %>%
  unlist() %>% table() %>% sort() %>% rev() %>% as.data.frame() %>%
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "blue1") +
  geom_text(aes(label = Freq), size = 3.5, vjust = -1) +
  labs(title = "Número de Pessoas por Quantitativo de Especialidades",
       y = "Número de Pessoas", x = "Quantitativo de Especialidades") + theme_bw()

# Número de pessoas por quantitativo de produções por pessoa
perfis %>%
  sapply(function(x)
    length(x$producao_bibiografica$PERIODICO$ano)) %>%
  unlist() %>% table()   %>% rev() %>% as.data.frame() %>%
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "forestgreen", width = 0.6) +
  labs(title = "Número de Artigos Publicados por Quantitativo de pessoas", y = "Pessoas", x = "Publicações") +
  theme_bw() + coord_flip()

perfis %>%
  sapply(function(x)
    length(x$producao_bibiografica$CAPITULO_DE_LIVRO$ano)) %>%
  unlist() %>% table() %>% rev() %>% as.data.frame() %>%
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "forestgreen", width = 0.8) + coord_flip() +
  labs(title = "Número de Capítulos de Livros por Quantitativo de pessoas", y = "Pessoas", x = "Publicações") +
  scale_x_discrete() + theme_bw() + geom_text(aes(label = Freq),
                                              hjust = -0.3,
                                              vjust = 0.3,
                                              size = 3.1) + scale_y_continuous(limits = c(0, 500))

perfis %>%
  sapply(function(x)
    length(x$producao_bibiografica$LIVRO$ano)) %>%
  unlist() %>% table() %>% rev() %>% as.data.frame() %>%
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "forestgreen", width = 0.5) + coord_flip() +
  labs(title = "Número de Livros por Quantitativo de pessoas", y = "Pessoas", x = "Publicações") +
  theme_bw() + geom_text(aes(label = Freq),
                         hjust = -0.3,
                         vjust = 0.3,
                         size = 3.1) + scale_y_continuous(limits = c(0, 400))


# Número de pessoas por quantitativo de Orientações por pessoa
perfis %>%
  sapply(function(x)
    length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$ano)) %>%
  unlist() %>% table() %>% rev() %>% as.data.frame() %>%
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "gold", width = 0.5) + coord_flip() +
  labs(title = "Número de Orientações de Mestrado por Quantitativo de pessoas",
       y = "Pessoas", x = "Orientações") + scale_y_continuous(limits = c(0, 400)) +
  theme_bw() +
  geom_text(aes(label = Freq),
            hjust = -0.3,
            vjust = 0.3,
            size = 3.1)

perfis %>%
  sapply(function(x)
    length(
      x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano
    )) %>%
  unlist() %>% table() %>% rev() %>% as.data.frame() %>%
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "gold", width = 0.5) + coord_flip() +
  labs(title = "Número de Orientações de Doutorado por Quantitativo de pessoas",
       y = "Pessoas", x = "Orientações") + scale_y_continuous(limits = c(0, 300)) +
  theme_bw() +
  geom_text(aes(label = Freq),
            hjust = -0.3,
            vjust = 0.3,
            size = 3.1)


perfis %>%
  sapply(
    function(x)
      length(
        x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_POS_DOUTORADO$ano
      )
  ) %>%
  unlist() %>% table() %>% rev() %>% as.data.frame() %>%
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "gold", width = 0.5) + coord_flip() +
  labs(title = "Número de Orientações de Pós-Doutorado por Quantitativo de pessoas",
       y = "Pessoas", x = "Orientações") + scale_y_continuous(limits = c(0, 200)) +
  theme_bw() +
  geom_text(aes(label = Freq),
            hjust = -0.3,
            vjust = 0.3,
            size = 3.1)

#Graficos ignorando especialidade e subArea
dataFrameAreas %>%
  select(-sub_area, -especialidade) %>%
  distinct() %>%
  group_by(publicacoes) %>%
  ggplot(aes(publicacoes, orientacoes_concluidas, color = area)) +
  geom_point(shape = 2, size = .8) + geom_jitter(shape = 2, size = .8) +
  ggtitle('Relação de Orientações Concluídas e Publicações') +
  labs(x = 'Publicações', y = 'Orientações concluídas') + facet_wrap(. ~ grande_area, ncol = 2)

dataFrameOrientacoes %>% group_by(ano, Status) %>%
  ggplot(aes(x = ano, y = Status, color = Status)) +
  labs(title = "Natureza das orientações por tipo de orientação") +
  geom_point(shape = 1) + geom_jitter(shape = 1) + facet_wrap(. ~ Natureza)

dataFramePublicacoes %>%
  filter((tipo_producao %in% c('LIVRO', 'CAPITULO_DE_LIVRO'))) %>%
  group_by(tipo_producao, pais_de_publicacao) %>%
  ggplot(aes(ano, tipo_producao, col = pais_de_publicacao)) +
  labs(title = "Número de Publicações de capítulos de livros por ano/país") +
  geom_point(alpha = 0.7) + geom_jitter() +
  labs(x = 'Tipo de produção', y = 'País')
