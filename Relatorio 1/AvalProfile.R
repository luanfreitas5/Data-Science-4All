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

#Definir o local onde est�o os arquivos json
#setwd("~/UnBPosGeral")

#Ler o arquivo profile
file.info("Geotecnia/profile.json")
profile <- fromJSON("Geotecnia/profile.json")
glimpse(profile[[1]], width = 30)

str(profile)

#Visualizar o arquivo no formato list
listviewer::jsonedit(profile)

#n�mero de arquivos / pessoas / pesquisadores
length(profile)

#### Nome do Pesquisador
# Nome de um pesquisador espec�fico ou de um conjunto de pesquisadores
profile[[1]]$nome
profile$`0000507838194708`$nome

head(sapply(profile, function(x) (x$nome)), 10)

ids <- c("0000507838194708", "0010973626622666", "0017467628165816", "0029536556461484")
sapply(profile[(names(profile) %in% ids)], function(x) (x$nome))

for (i in 1:5){
  print(names(profile[i]))
  print(profile[[i]]$nome)
}

# N�mero de �reas de atua��o cumulativo
sum(sapply(profile, function(x) nrow(x$areas_de_atuacao)))

# N�mero de �reas de atua��o por pessoa
table(unlist(sapply(profile, function(x) nrow(x$areas_de_atuacao))))

# N�mero de pessoas por grande area
table(unlist(sapply(profile, function(x) (x$areas_de_atuacao$grande_area))))

# N�mero de pessoas que produziram os espec�ficos tipos de produ��o
table(unlist(sapply(profile, function(x) names(x$producao_bibiografica))))

# N�mero de publica��es por tipo
sum(sapply(profile, function(x) length(x$producao_bibiografica$ARTIGO_ACEITO$ano)))

sum(sapply(profile, function(x) length(x$producao_bibiografica$CAPITULO_DE_LIVRO$ano)))

sum(sapply(profile, function(x) length(x$producao_bibiografica$LIVRO$ano)))

sum(sapply(profile, function(x) length(x$producao_bibiografica$PERIODICO$ano)))

sum(sapply(profile, function(x) length(x$producao_bibiografica$TEXTO_EM_JORNAIS$ano)))

# N�mero de pessoas por quantitativo de produ��es por pessoa 0 = 1; 1 = 2...
table(unlist(sapply(profile, function(x) length(x$producao_bibiografica$ARTIGO_ACEITO$ano))))

table(unlist(sapply(profile, function(x) length(x$producao_bibiografica$CAPITULO_DE_LIVRO$ano))))

table(unlist(sapply(profile, function(x) length(x$producao_bibiografica$LIVRO$ano))))

table(unlist(sapply(profile, function(x) length(x$producao_bibiografica$PERIODICO$ano))))

table(unlist(sapply(profile, function(x) length(x$producao_bibiografica$TEXTO_EM_JORNAIS$ano))))

# N�mero de produ��es por ano
table(unlist(sapply(profile, function(x) (x$producao_bibiografica$ARTIGO_ACEITO$ano))))

table(unlist(sapply(profile, function(x) (x$producao_bibiografica$CAPITULO_DE_LIVRO$ano))))

table(unlist(sapply(profile, function(x) (x$producao_bibiografica$LIVRO$ano))))

table(unlist(sapply(profile, function(x) (x$producao_bibiografica$PERIODICO$ano))))

table(unlist(sapply(profile, function(x) (x$producao_bibiografica$TEXTO_EM_JORNAIS$ano))))

# N�mero de pessoas que realizaram diferentes tipos de orienta��es
length(unlist(sapply(profile, function(x) names(x$orientacoes_academicas))))

# N�mero de pessoas por tipo de orienta��o
table(unlist(sapply(profile, function(x) names(x$orientacoes_academicas))))

#N�mero de orienta��es concluidas
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
# Resumos do curr�culo de um pesquisador ou de um conjunto
profile[[1]]$resumo_cv
profile$`0000507838194708`$resumo_cv

head(sapply(profile, function(x) (x$resumo_cv)), 2)

ids <- c("0000507838194708", "0010973626622666", "0017467628165816")
sapply(profile[(names(profile) %in% ids)], function(x) (x$resumo_cv))

for (i in 1:5){
  print(names(profile[i]))
  print(profile[[i]]$resumo_cv)
}

# Areas de Atua��o #####
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

# Avalia��o de Areas de conjunto de pesquisadores
profile[[1]]$areas_de_atuacao
    
    #com repeti��o de itens
head(sapply(profile, function(x) unlist(x$areas_de_atuacao$area)), 2)

ids <- c("0000507838194708", "0010973626622666", "0011938955607677", "0037568024594010")
    #sem repeti��o de itens
sapply(profile[(names(profile) %in% ids)], function(x) unique(x$areas_de_atuacao$grande_area))
    #em outro formato usando magrittr
profile[(names(profile) %in% ids)] %>% sapply(function(x) (x$areas_de_atuacao$grande_area)) %>% sapply(unique)

#N�mero de "areas_de_atua��o" Especialidade... por pessoa
profile[(names(profile) %in% ids)] %>% 
  sapply(function(x) unlist(x$areas_de_atuacao$especialidade)) %>% 
  sapply(unique) %>% 
  sapply(length)
  
# N�mero de "areas_de_atua��o" acumulado
profile %>% 
  sapply(function(x) unlist(x$areas_de_atuacao$especialidade)) %>% 
  sapply(unique) %>% 
  sapply(length) %>% 
  table()

# Endereço Profissional #####
## Nome dos subitens de "endereco_profissional"
names(profile[[1]]$endereco_profissional)

  #lista de instituições nos curr�?culos
unique(unlist(sapply(profile, function(x) (x$endereco_profissional$instituicao))))
  #N�mero de curr�?culos com instituicao com Universidade de Bras�?lia
profile %>% 
  sapply(function(x) 
    (x$endereco_profissional$instituicao)) %in% "Universidade de Bras�?lia" %>% 
  sum()

  #Lista de Unidades
head(sort(unique(unlist(sapply(profile, function(x) 
  (x$endereco_profissional$unidade)))), decreasing = TRUE), 40)

  #Contagem de Orgãos / Institutos
head(sort(table(unlist(sapply(profile, function(x) 
  (x$endereco_profissional$orgao)))), decreasing = TRUE), 20)

# Produção Bibliográfica #####
#Análise da Produção Bibliográfica
  #Todos os tipos de produção
unique(unlist(sapply(profile, function(x) names(x$producao_bibiografica))))

  # Media de produção por pesquisador
  # Aqui é usado a media pois existem duplicações caso pesquisadores tenham escrito o mesmo artigo
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

  # N�mero de pessoas por quantitativo de produ��es
profile %>% 
  sapply(function(x) 
    length(x$producao_bibiografica$PERIODICO$natureza)) %>% 
  unlist() %>% table()

profile %>% 
  sapply(function(x) 
    length(x$producao_bibiografica$EVENTO$natureza)) %>% 
  unlist() %>% table()

  # Nome de pesquisadores com quantitativo de produção espec�?fica (20)
profile[(profile %>% 
  sapply(function(x) 
    length(x$producao_bibiografica$PERIODICO$natureza))) %in% 20] %>% 
  sapply(function(x) (x$nome))

  # Nome de pesquisadores com busca por palavra em t�?tulo de publicação
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
          sapply('%in%', "Romênia") %>% 
          sapply(any)] %>% 
  sapply(function(x) (x$nome))

  # N�mero de produ��es em eventos por ano
profile %>% 
  sapply(function(x) 
    (x$producao_bibiografica$EVENTO$ano)) %>% 
  unlist() %>% table()
    # em outro formato para livros
table(unlist(sapply(profile, function(x) (x$producao_bibiografica$LIVRO$ano))))

  # N�mero de produ��es por ano para os 100 primeiros pesquisadores da lista
profile[1:100] %>% 
  sapply(function(x) 
    (x$producao_bibiografica$TEXTO_EM_JORNAIS$ano)) %>% 
  unlist() %>% table()

# Orienta��es #####
#Análise das Orienta��es
  # Tipos de orientação
profile %>% 
  sapply(function(x)
    names(x$orientacoes_academicas)) %>% 
  unlist() %>% unique()

  # Media de Orienta��es por pesquisador
  # Aqui s�o usado a media pois existem duplicações caso pesquisadores tenham orientado o mesmo trabalho
(profile %>% 
   sapply(function(x) 
     length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$natureza)) %>% 
   sum()) / length(profile)

(profile %>% 
    sapply(function(x) 
      length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$natureza)) %>% 
    sum()) / length(profile)

# N�mero de pessoas por quantitativo de produ��o
profile %>% 
  sapply(function(x) 
    length(x$orientacoes_academicas$OUTRAS_ORIENTACOES_CONCLUIDAS$natureza)) %>% 
  unlist() %>% table()

# N�mero de pessoas por tipo de orientação
profile %>% 
  sapply(function(x) 
    names(x$orientacoes_academicas)) %>% 
  unlist() %>% table()

# N�mero de produ��es em Orienta��es por ano
profile %>% 
  sapply(function(x) 
    (x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano)) %>% 
  unlist() %>% table()

# Data Frame ####
  # Análise dos dados em formato Data Frame
  # Usar as funcionalidades que est�o no arquivo elatttes.ls2df.R
#Arquivo Profile por curr�culo
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


#Eventos por pa�?s
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

# Visualiza��o ####
# N�mero de pessoas por area de atua��o (grande área, area, sub_area, especialidade)
profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$grande_area)) %>% 
  unlist() %>% table() %>% sort() %>% as.data.frame() %>% filter(!. == "") %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "green4",alpha=0.8,width=0.8) + coord_flip() + geom_text(aes(label=Freq),hjust=-0.2,vjust=0.5,size=3.5) +
  labs(title = "N�mero de Pessoas por Grande �rea Atua��o", y="Quantidade",x="Grande �rea") + theme_bw() + scale_y_continuous()+
  scale_x_discrete(labels = c('CIENCIAS_DA_SAUDE' = 'Ci�ncias da Sa�de',
                              'CIENCIAS_BIOLOGICAS' = 'Ci�ncias Biol�gicas',
                              'CIENCIAS_HUMANAS' = 'Ci�ncias Humanas',
                              "CIENCIAS_EXATAS_E_DA_TERRA" = "Ci�ncias Exatas e da Terra",
                              "CIENCIAS_SOCIAIS_APLICADAS" = "Ci�ncias Sociais Aplicadas",
                              "CIENCIAS_AGRARIAS" = "Ci�ncias Agr�rias",
                              "OUTROS" = "Outros",
                              "ENGENHARIAS" = "Engenharias",
                              "LINGUISTICA_LETRAS_E_ARTES" = "Lingu�stica, Letras e Artes"))

profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$area)) %>% 
  unlist() %>% table() %>% sort() %>% as.data.frame() %>% filter(!. == "") %>% tail(20) %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "green4",alpha=0.8,width=0.8) + coord_flip() +
  labs(title = "N�mero de Pessoas por �rea Atua��o",x="�rea de atua��o", y="Quantidade") +
  geom_text(aes(label=Freq),hjust=-0.2,vjust=0.3,size=3.5) +
  scale_y_continuous(limits=c(0,800))+theme_bw()

profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$sub_area)) %>% 
  unlist() %>% table() %>% sort() %>% as.data.frame() %>% filter(!. == "") %>% tail(30) %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "green4",alpha=0.8,width=0.8) + coord_flip() + 
  geom_text(aes(label=Freq),hjust=-0.2,vjust=0.3,size=3.5) +
  labs(title = "N�mero de Pessoas por Sub �rea atua��o",x="Sub �rea",y="Quantidade") +
  scale_y_continuous(limits=c(0,300))+theme_bw()

profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$especialidade)) %>% 
  unlist() %>% table() %>% sort() %>% as.data.frame() %>% filter(!. == "") %>% tail(29) %>%  
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "green4",alpha=0.8,width=0.8) + coord_flip() +
  labs(title = "N�mero de Pessoas por Especialidade",x="Especialidade",y="Quantidade")+
  geom_text(aes(label=Freq),hjust=-0.2,vjust=0.3,size=3.0)+
  theme_bw()+
  scale_y_continuous(limits=c(0,150))

# N�mero de áreas de atua��o por pessoa (grande área, area, sub_area, especialidade)
profile %>% 
  sapply(function(x) length(unique(x$areas_de_atuacao$grande_area))) %>% 
  unlist() %>% table() %>% sort() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "orange",alpha=0.9) + 
  geom_text(aes(label=Freq),size=3.5,vjust=-1) +
  labs(title = "N�mero de Pessoas por Quantitativo de Grande �reas", 
       y = "N�mero de Pessoas", x = "Quantitativo de Grande �reas") + theme_bw()

profile %>% 
  sapply(function(x) length(unique(x$areas_de_atuacao$area))) %>% 
  unlist() %>% table() %>% sort() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "orange",alpha=0.9) + 
  geom_text(aes(label=Freq),size=3.5,vjust=-1) +
  labs(title = "N�mero de Pessoas por Quantitativo de �reas", 
       y = "N�mero de Pessoas", x = "Quantitativo de �reas") + theme_bw()

profile %>% 
  sapply(function(x) length(unique(x$areas_de_atuacao$sub_area))) %>% 
  unlist() %>% table() %>% sort() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "orange",alpha=0.9) + 
  geom_text(aes(label=Freq),size=3.5,vjust=-1) +
  labs(title = "N�mero de Pessoas por Quantitativo de Sub �reas", 
       y = "N�mero de Pessoas", x = "Quantitativo de Sub �reas") + theme_bw()

profile %>% 
  sapply(function(x) length(unique(x$areas_de_atuacao$especialidade))) %>% 
  unlist() %>% table() %>% sort() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "orange") + 
  geom_text(aes(label=Freq),size=3.5,vjust=-1) +
  labs(title = "N�mero de Pessoas por Quantitativo de Especialidades", 
       y = "N�mero de Pessoas", x = "Quantitativo de Especialidades") + theme_bw()

# N�mero de pessoas por quantitativo de produ��es por pessoa
profile %>% 
  sapply(function(x) length(x$producao_bibiografica$PERIODICO$ano)) %>% 
  unlist() %>% table()   %>% rev() %>% as.data.frame() %>%
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "green4", width = 0.6) +
  labs(title = "N�mero de Artigos Publicados por Quantitativo de pessoas", y = "Pessoas", x = "Publica��es")+
  theme_bw()+coord_flip()

profile %>% 
  sapply(function(x) length(x$producao_bibiografica$CAPITULO_DE_LIVRO$ano)) %>% 
  unlist() %>% table() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "green4", width = 0.8) + coord_flip() + 
  labs(title = "N�mero de Cap�tulos de Livros por Quantitativo de pessoas", y = "Pessoas", x = "Publica��es")+
  scale_x_discrete()+theme_bw()+geom_text(aes(label=Freq),hjust=-0.3,vjust=0.3,size=3.1)+scale_y_continuous(limits=c(0,500))

profile %>% 
  sapply(function(x) length(x$producao_bibiografica$LIVRO$ano)) %>% 
  unlist() %>% table() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "green4", width = 0.5) + coord_flip() + 
  labs(title = "N�mero de Livros por Quantitativo de pessoas", y = "Pessoas", x = "Publica��es")+
  theme_bw()+geom_text(aes(label=Freq),hjust=-0.3,vjust=0.3,size=3.1)+scale_y_continuous(limits=c(0,400))

# N�mero de pessoas por quantitativo de Orienta��es por pessoa
profile %>% 
  sapply(function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$ano)) %>% 
  unlist() %>% table() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "blue4", width = 0.5) + coord_flip() + 
  labs(title = "N�mero de Orienta��es de Mestrado por Quantitativo de pessoas", 
       y = "Pessoas", x = "Orienta��es") + scale_y_continuous(limits = c(0, 400))+theme_bw()+
  geom_text(aes(label=Freq),hjust=-0.3,vjust=0.3,size=3.1)

profile %>% 
  sapply(function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano)) %>% 
  unlist() %>% table() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "blue", width = 0.5) + coord_flip() + 
  labs(title = "N�mero de Orienta��es de Doutorado por Quantitativo de pessoas", 
       y = "Pessoas", x = "Orienta��es") + scale_y_continuous(limits = c(0, 300))+theme_bw()+
  geom_text(aes(label=Freq),hjust=-0.3,vjust=0.3,size=3.1)


profile %>% 
  sapply(function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_POS_DOUTORADO$ano)) %>% 
  unlist() %>% table() %>% rev() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_col(fill = "blue", width = 0.5) + coord_flip() + 
  labs(title = "N�mero de Orienta��es de P�s-Doutorado por Quantitativo de pessoas", 
       y = "Pessoas", x = "Orienta��es") + scale_y_continuous(limits = c(0, 200))+theme_bw()+
  geom_text(aes(label=Freq),hjust=-0.3,vjust=0.3,size=3.1)