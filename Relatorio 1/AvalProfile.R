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

#Arquivo com funcionalidades que transformam o arquivo formato list em DataFrames
source("elattes.ls2df.R")

#Definir o local onde estão os arquivos json
#setwd("~/UnBPosGeral")

#Ler o arquivo profile
file.info("Geotecnia/profile.json")
profile <- fromJSON("Geotecnia/profile.json")
glimpse(profile[[1]], width = 30)

str(profile)

#Visualizar o arquivo no formato list
listviewer::jsonedit(profile)

#número de arquivos / pessoas / pesquisadores
length(profile)

#### Nome do Pesquisador
# Nome de um pesquisador específico ou de um conjunto de pesquisadores
profile[[1]]$nome
profile$`0000507838194708`$nome

head(sapply(profile, function(x) (x$nome)), 10)

ids <- c("0000507838194708", "0010973626622666", "0017467628165816", "0029536556461484")
sapply(profile[(names(profile) %in% ids)], function(x) (x$nome))

for (i in 1:5){
  print(names(profile[i]))
  print(profile[[i]]$nome)
}

# Número de áreas de atuação cumulativo
sum(sapply(profile, function(x) nrow(x$areas_de_atuacao)))

# Número de áreas de atuação por pessoa
table(unlist(sapply(profile, function(x) nrow(x$areas_de_atuacao))))

# Número de pessoas por grande area
table(unlist(sapply(profile, function(x) (x$areas_de_atuacao$grande_area))))

# Número de pessoas que produziram os específicos tipos de produção
table(unlist(sapply(profile, function(x) names(x$producao_bibiografica))))

# Número de publicações por tipo
sum(sapply(profile, function(x) length(x$producao_bibiografica$ARTIGO_ACEITO$ano)))

sum(sapply(profile, function(x) length(x$producao_bibiografica$CAPITULO_DE_LIVRO$ano)))

sum(sapply(profile, function(x) length(x$producao_bibiografica$LIVRO$ano)))

sum(sapply(profile, function(x) length(x$producao_bibiografica$PERIODICO$ano)))

sum(sapply(profile, function(x) length(x$producao_bibiografica$TEXTO_EM_JORNAIS$ano)))

# Número de pessoas por quantitativo de produções por pessoa 0 = 1; 1 = 2...
table(unlist(sapply(profile, function(x) length(x$producao_bibiografica$ARTIGO_ACEITO$ano))))

table(unlist(sapply(profile, function(x) length(x$producao_bibiografica$CAPITULO_DE_LIVRO$ano))))

table(unlist(sapply(profile, function(x) length(x$producao_bibiografica$LIVRO$ano))))

table(unlist(sapply(profile, function(x) length(x$producao_bibiografica$PERIODICO$ano))))

table(unlist(sapply(profile, function(x) length(x$producao_bibiografica$TEXTO_EM_JORNAIS$ano))))

# Número de produções por ano
table(unlist(sapply(profile, function(x) (x$producao_bibiografica$ARTIGO_ACEITO$ano))))

table(unlist(sapply(profile, function(x) (x$producao_bibiografica$CAPITULO_DE_LIVRO$ano))))

table(unlist(sapply(profile, function(x) (x$producao_bibiografica$LIVRO$ano))))

table(unlist(sapply(profile, function(x) (x$producao_bibiografica$PERIODICO$ano))))

table(unlist(sapply(profile, function(x) (x$producao_bibiografica$TEXTO_EM_JORNAIS$ano))))

# Número de pessoas que realizaram diferentes tipos de orientações
length(unlist(sapply(profile, function(x) names(x$orientacoes_academicas))))

# Número de pessoas por tipo de orientação
table(unlist(sapply(profile, function(x) names(x$orientacoes_academicas))))

#Número de orientações concluidas
sum(sapply(profile, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$ano)))

sum(sapply(profile, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano)))

sum(sapply(profile, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_POS_DOUTORADO$ano)))

table(unlist(sapply(profile, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$ano))))

table(unlist(sapply(profile, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano))))

table(unlist(sapply(profile, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_POS_DOUTORADO$ano))))

table(unlist(sapply(profile, function(x) (x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$ano))))

table(unlist(sapply(profile, function(x) (x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano))))

table(unlist(sapply(profile, function(x) (x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_POS_DOUTORADO$ano))))


# Resumos #####
# Resumos do currículo de um pesquisador ou de um conjunto
profile[[1]]$resumo_cv
profile$`0000507838194708`$resumo_cv

head(sapply(profile, function(x) (x$resumo_cv)), 2)

ids <- c("0000507838194708", "0010973626622666", "0017467628165816")
sapply(profile[(names(profile) %in% ids)], function(x) (x$resumo_cv))

for (i in 1:5){
  print(names(profile[i]))
  print(profile[[i]]$resumo_cv)
}

# Areas de Atuação #####
# Nome dos subitens de "areas_de_atuacao"
names(profile[[1]]$areas_de_atuacao)

# Nomes e contagem dos itens armazenados em cada subitem
unique(unlist(sapply(profile, function(x) (x$areas_de_atuacao$grande_area))))
profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$grande_area)) %>% 
  unlist() %>% table() %>% sort(decreasing = TRUE) %>% as.data.frame() %>% filter(!. == "")

length(unique(unlist(sapply(profile, function(x) 
  x$areas_de_atuacao$area))))
profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$area)) %>% 
  unlist() %>% table() %>% sort(decreasing = TRUE) %>% 
  as.data.frame() %>% filter(!. == "")

profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$sub_area)) %>% 
  unlist() %>% table() %>% sort(decreasing = TRUE) %>% 
  as.data.frame() %>% filter(!. == "") %>% head(40)

profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$especialidade)) %>% 
  unlist() %>% table() %>% sort(decreasing = TRUE) %>% 
  as.data.frame() %>% filter(!. == "") %>% head(40)

# Avaliação de Areas de conjunto de pesquisadores
profile[[1]]$areas_de_atuacao
    
    #com repetição de itens
head(sapply(profile, function(x) unlist(x$areas_de_atuacao$area)), 2)

ids <- c("0000507838194708", "0010973626622666", "0011938955607677", "0037568024594010")
    #sem repetição de itens
sapply(profile[(names(profile) %in% ids)], function(x) unique(x$areas_de_atuacao$grande_area))
    #em outro formato usando magrittr
profile[(names(profile) %in% ids)] %>% sapply(function(x) (x$areas_de_atuacao$grande_area)) %>% sapply(unique)

#Número de "areas_de_atuação" Especialidade... por pessoa
profile[(names(profile) %in% ids)] %>% 
  sapply(function(x) unlist(x$areas_de_atuacao$especialidade)) %>% 
  sapply(unique) %>% 
  sapply(length)
  
# Número de "areas_de_atuação" acumulado
profile %>% 
  sapply(function(x) unlist(x$areas_de_atuacao$especialidade)) %>% 
  sapply(unique) %>% 
  sapply(length) %>% 
  table()

# EndereÃ§o Profissional #####
## Nome dos subitens de "endereco_profissional"
names(profile[[1]]$endereco_profissional)

  #lista de instituiÃ§Ãµes nos currí?culos
unique(unlist(sapply(profile, function(x) (x$endereco_profissional$instituicao))))
  #Número de currí?culos com instituicao com Universidade de BrasÃ?lia
profile %>% 
  sapply(function(x) 
    (x$endereco_profissional$instituicao)) %in% "Universidade de BrasÃ?lia" %>% 
  sum()

  #Lista de Unidades
head(sort(unique(unlist(sapply(profile, function(x) 
  (x$endereco_profissional$unidade)))), decreasing = TRUE), 40)

  #Contagem de OrgÃ£os / Institutos
head(sort(table(unlist(sapply(profile, function(x) 
  (x$endereco_profissional$orgao)))), decreasing = TRUE), 20)

# ProduÃ§Ã£o BibliogrÃ¡fica #####
#AnÃ¡lise da ProduÃ§Ã£o BibliogrÃ¡fica
  #Todos os tipos de produÃ§Ã£o
unique(unlist(sapply(profile, function(x) names(x$producao_bibiografica))))

  # Media de produÃ§Ã£o por pesquisador
  # Aqui Ã© usado a media pois existem duplicaÃ§Ãµes caso pesquisadores tenham escrito o mesmo artigo
(profile %>% 
  sapply(function(x) 
    length(x$producao_bibiografica$PERIODICO$natureza)) %>% 
  sum()) / length(profile)

(profile %>% 
    sapply(function(x) 
      length(x$producao_bibiografica$EVENTO$natureza)) %>% 
    sum()) / length(profile)

(profile %>% 
    sapply(function(x) 
      length(x$producao_bibiografica$LIVRO$natureza)) %>% 
    sum()) / length(profile)

  # Número de pessoas por quantitativo de produções
profile %>% 
  sapply(function(x) 
    length(x$producao_bibiografica$PERIODICO$natureza)) %>% 
  unlist() %>% table()

profile %>% 
  sapply(function(x) 
    length(x$producao_bibiografica$EVENTO$natureza)) %>% 
  unlist() %>% table()

  # Nome de pesquisadores com quantitativo de produÃ§Ã£o especÃ?fica (20)
profile[(profile %>% 
  sapply(function(x) 
    length(x$producao_bibiografica$PERIODICO$natureza))) %in% 20] %>% 
  sapply(function(x) (x$nome))

  # Nome de pesquisadores com busca por palavra em tÃ?tulo de publicaÃ§Ã£o
profile[profile %>% 
    sapply(function(x) 
      (x$producao_bibiografica$PERIODICO$titulo)) %>% 
  grepl(pattern = "GDF")] %>% 
  sapply(function(x) (x$nome))

  # Quantitativo de eventos por pais
#profile%>% 
#  sapply(function(x)
#    (x$producao_bibiografica$EVENTO$pais_do_evento)) %>% 
#  unlist() %>% summary()
#  ggplot(aes())

  # Nome de pesquisadores com busca por pais de evento
profile[profile %>% 
          sapply(function(x) 
            (x$producao_bibiografica$EVENTO$pais_do_evento)) %>% 
          sapply('%in%', "RomÃªnia") %>% 
          sapply(any)] %>% 
  sapply(function(x) (x$nome))

  # Número de produções em eventos por ano
profile %>% 
  sapply(function(x) 
    (x$producao_bibiografica$EVENTO$ano)) %>% 
  unlist() %>% table()
    # em outro formato para livros
table(unlist(sapply(profile, function(x) (x$producao_bibiografica$LIVRO$ano))))

  # Número de produções por ano para os 100 primeiros pesquisadores da lista
profile[1:100] %>% 
  sapply(function(x) 
    (x$producao_bibiografica$TEXTO_EM_JORNAIS$ano)) %>% 
  unlist() %>% table()

# Orientações #####
#AnÃ¡lise das Orientações
  # Tipos de orientaÃ§Ã£o
profile %>% 
  sapply(function(x)
    names(x$orientacoes_academicas)) %>% 
  unlist() %>% unique()

  # Media de Orientações por pesquisador
  # Aqui são usado a media pois existem duplicaÃ§Ãµes caso pesquisadores tenham orientado o mesmo trabalho
(profile %>% 
   sapply(function(x) 
     length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$natureza)) %>% 
   sum()) / length(profile)

(profile %>% 
    sapply(function(x) 
      length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$natureza)) %>% 
    sum()) / length(profile)

# Número de pessoas por quantitativo de produção
profile %>% 
  sapply(function(x) 
    length(x$orientacoes_academicas$OUTRAS_ORIENTACOES_CONCLUIDAS$natureza)) %>% 
  unlist() %>% table()

# Número de pessoas por tipo de orientaÃ§Ã£o
profile %>% 
  sapply(function(x) 
    names(x$orientacoes_academicas)) %>% 
  unlist() %>% table()

# Número de produções em Orientações por ano
profile %>% 
  sapply(function(x) 
    (x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano)) %>% 
  unlist() %>% table()

# Data Frame ####
  # AnÃ¡lise dos dados em formato Data Frame
  # Usar as funcionalidades que estão no arquivo elatttes.ls2df.R
#Arquivo Profile por currículo
# extrai perfis dos professores 
profile.df.pesq <- extrai.perfis(profile)
glimpse(profile.df.pesq)

# extrai producao bibliografica de todos os professores 
profile.df.pub <- extrai.producoes(profile)
glimpse(profile.df.pub)

#extrai orientacoes 
profile.df.ori <- extrai.orientacoes(profile)
glimpse(profile.df.ori)

#extrai areas de atuacao 
profile.df.areas <- extrai.areas.atuacao(profile)
glimpse(profile.df.areas)


#Eventos por paÃ?s
#profile.df.pub %>%
#  filter(tipo_producao == 'EVENTO') %>%
#  group_by(pais_do_evento) %>%
#  summarise(quantidade = n()) %>%
#  ggplot(aes(x = reorder(pais_do_evento, quantidade)), y = quantidade) + geom_bar(aes(color = 'blue')) + coord_flip()

str(profile.df.pub)

#cria arquivo com dados quantitativos para analise
profile.df <- data.frame()
profile.df <- profile.df.pesq %>% 
  select(idLattes, nome, resumo_cv, instituicao, orgao, unidade, cidade, senioridade) %>% 
  left_join(
    profile.df.pub %>% 
      select(tipo_producao, idLattes) %>% 
      filter(!grepl("ARTIGO_ACEITO", tipo_producao)) %>% 
      group_by(idLattes) %>% 
      count(tipo_producao) %>% 
      spread(key = tipo_producao, value = n), 
    by = "idLattes") %>% 
  left_join(
    profile.df.ori %>% 
      select(orientacao, idLattes) %>% 
      filter(!grepl("EM_ANDAMENTO", orientacao)) %>% 
      group_by(idLattes) %>% 
      count(orientacao) %>% 
      spread(key = orientacao, value = n), 
    by = "idLattes") %>% 
  left_join(
    profile.df.areas %>% 
      select(area, idLattes) %>% 
      group_by(idLattes) %>% 
      summarise(n_distinct(area)), 
    by = "idLattes")

glimpse(profile.df)

# Visualização ####
# Número de pessoas por area de atuação (grande Ã¡rea, area, sub_area, especialidade)
profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$grande_area)) %>% 
  unlist() %>% table() %>% sort() %>% as.data.frame() %>% filter(!. == "") %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "green4",alpha=0.8,width=0.8) + coord_flip() + geom_text(aes(label=Freq),hjust=-0.2,vjust=0.5,size=3.5) +
  labs(title = "Número de Pessoas por Grande Área Atuação", y="Quantidade",x="Grande Área") + theme_bw() + scale_y_continuous()+
  scale_x_discrete(labels = c('CIENCIAS_DA_SAUDE' = 'Ciências da Saúde',
                              'CIENCIAS_BIOLOGICAS' = 'Ciências Biológicas',
                              'CIENCIAS_HUMANAS' = 'Ciências Humanas',
                              "CIENCIAS_EXATAS_E_DA_TERRA" = "Ciências Exatas e da Terra",
                              "CIENCIAS_SOCIAIS_APLICADAS" = "Ciências Sociais Aplicadas",
                              "CIENCIAS_AGRARIAS" = "Ciências Agrárias",
                              "OUTROS" = "Outros",
                              "ENGENHARIAS" = "Engenharias",
                              "LINGUISTICA_LETRAS_E_ARTES" = "Linguística, Letras e Artes"))

profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$area)) %>% 
  unlist() %>% table() %>% sort() %>% as.data.frame() %>% filter(!. == "") %>% tail(20) %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "green4",alpha=0.8,width=0.8) + coord_flip() +
  labs(title = "Número de Pessoas por Área Atuação",x="Área de atuação", y="Quantidade") +
  geom_text(aes(label=Freq),hjust=-0.2,vjust=0.3,size=3.5) +
  scale_y_continuous(limits=c(0,800))+theme_bw()

profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$sub_area)) %>% 
  unlist() %>% table() %>% sort() %>% as.data.frame() %>% filter(!. == "") %>% tail(30) %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "green4",alpha=0.8,width=0.8) + coord_flip() + 
  geom_text(aes(label=Freq),hjust=-0.2,vjust=0.3,size=3.5) +
  labs(title = "Número de Pessoas por Sub Área atuação",x="Sub Área",y="Quantidade") +
  scale_y_continuous(limits=c(0,300))+theme_bw()

profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$especialidade)) %>% 
  unlist() %>% table() %>% sort() %>% as.data.frame() %>% filter(!. == "") %>% tail(29) %>%  
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "green4",alpha=0.8,width=0.8) + coord_flip() +
  labs(title = "Número de Pessoas por Especialidade",x="Especialidade",y="Quantidade")+
  geom_text(aes(label=Freq),hjust=-0.2,vjust=0.3,size=3.0)+
  theme_bw()+
  scale_y_continuous(limits=c(0,150))

# Número de Ã¡reas de atuação por pessoa (grande Ã¡rea, area, sub_area, especialidade)
profile %>% 
  sapply(function(x) length(unique(x$areas_de_atuacao$grande_area))) %>% 
  unlist() %>% table() %>% sort() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "orange",alpha=0.9) + 
  geom_text(aes(label=Freq),size=3.5,vjust=-1) +
  labs(title = "Número de Pessoas por Quantitativo de Grande Áreas", 
       y = "Número de Pessoas", x = "Quantitativo de Grande Áreas") + theme_bw()

profile %>% 
  sapply(function(x) length(unique(x$areas_de_atuacao$area))) %>% 
  unlist() %>% table() %>% sort() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "orange",alpha=0.9) + 
  geom_text(aes(label=Freq),size=3.5,vjust=-1) +
  labs(title = "Número de Pessoas por Quantitativo de Áreas", 
       y = "Número de Pessoas", x = "Quantitativo de Áreas") + theme_bw()

profile %>% 
  sapply(function(x) length(unique(x$areas_de_atuacao$sub_area))) %>% 
  unlist() %>% table() %>% sort() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "orange",alpha=0.9) + 
  geom_text(aes(label=Freq),size=3.5,vjust=-1) +
  labs(title = "Número de Pessoas por Quantitativo de Sub Áreas", 
       y = "Número de Pessoas", x = "Quantitativo de Sub Áreas") + theme_bw()

profile %>% 
  sapply(function(x) length(unique(x$areas_de_atuacao$especialidade))) %>% 
  unlist() %>% table() %>% sort() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "orange") + 
  geom_text(aes(label=Freq),size=3.5,vjust=-1) +
  labs(title = "Número de Pessoas por Quantitativo de Especialidades", 
       y = "Número de Pessoas", x = "Quantitativo de Especialidades") + theme_bw()

# Número de pessoas por quantitativo de produções por pessoa
profile %>% 
  sapply(function(x) length(x$producao_bibiografica$PERIODICO$ano)) %>% 
  unlist() %>% table()   %>% rev() %>% as.data.frame() %>%
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "green4", width = 0.6) +
  labs(title = "Número de Artigos Publicados por Quantitativo de pessoas", y = "Pessoas", x = "Publicações")+
  theme_bw()+coord_flip()

profile %>% 
  sapply(function(x) length(x$producao_bibiografica$CAPITULO_DE_LIVRO$ano)) %>% 
  unlist() %>% table() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "green4", width = 0.8) + coord_flip() + 
  labs(title = "Número de Capítulos de Livros por Quantitativo de pessoas", y = "Pessoas", x = "Publicações")+
  scale_x_discrete()+theme_bw()+geom_text(aes(label=Freq),hjust=-0.3,vjust=0.3,size=3.1)+scale_y_continuous(limits=c(0,500))

profile %>% 
  sapply(function(x) length(x$producao_bibiografica$LIVRO$ano)) %>% 
  unlist() %>% table() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "green4", width = 0.5) + coord_flip() + 
  labs(title = "Número de Livros por Quantitativo de pessoas", y = "Pessoas", x = "Publicações")+
  theme_bw()+geom_text(aes(label=Freq),hjust=-0.3,vjust=0.3,size=3.1)+scale_y_continuous(limits=c(0,400))

# Número de pessoas por quantitativo de Orientações por pessoa
profile %>% 
  sapply(function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$ano)) %>% 
  unlist() %>% table() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "blue4", width = 0.5) + coord_flip() + 
  labs(title = "Número de Orientações de Mestrado por Quantitativo de pessoas", 
       y = "Pessoas", x = "Orientações") + scale_y_continuous(limits = c(0, 400))+theme_bw()+
  geom_text(aes(label=Freq),hjust=-0.3,vjust=0.3,size=3.1)

profile %>% 
  sapply(function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano)) %>% 
  unlist() %>% table() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "blue", width = 0.5) + coord_flip() + 
  labs(title = "Número de Orientações de Doutorado por Quantitativo de pessoas", 
       y = "Pessoas", x = "Orientações") + scale_y_continuous(limits = c(0, 300))+theme_bw()+
  geom_text(aes(label=Freq),hjust=-0.3,vjust=0.3,size=3.1)


profile %>% 
  sapply(function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_POS_DOUTORADO$ano)) %>% 
  unlist() %>% table() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "blue", width = 0.5) + coord_flip() + 
  labs(title = "Número de Orientações de Pós-Doutorado por Quantitativo de pessoas", 
       y = "Pessoas", x = "Orientações") + scale_y_continuous(limits = c(0, 200))+theme_bw()+
  geom_text(aes(label=Freq),hjust=-0.3,vjust=0.3,size=3.1)