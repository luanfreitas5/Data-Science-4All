#Leitura de arquivos do publication.json
#Resumo de Scripts para leitura do arquivo

library(tidyverse)
library(jsonlite)
library(listviewer)
library(scales)
library(dplyr)
library(readxl)
library(readr)
library(ggplot2)

#Arquivo com funcionalidades que transformam o arquivo formato list em DataFrames
source("elattes.ls2df.R")

#Definir o local onde est√£o os arquivos json
#setwd("~/Documents/eLattes/Pacote e-Lattes/UnBPosGeral")

#Ler o arquivo publication
file.info("Geotecnia/publication.json")
publication <- fromJSON("Geotecnia/publication.json")

#Visualizar o arquivo no formato list
listviewer::jsonedit(publication)

table(unlist(sapply(profile, function(x) (x$producao_bibiografica$ARTIGO_ACEITO$ano))))

#Dados Descritivos Gerais ####
  # Tipos de objetos armazenados
names(publication)

  # Periodo analisado
names(publication[[1]])

names(publication$PERIODICO$`2012`)

head(sort(table(publication$PERIODICO$`2017`$periodico), decreasing = TRUE), 10)

head(sort(table(publication$LIVRO$`2015`$nome_da_editora), decreasing = TRUE), 10)

# Visualizar a estrutura do arquivo de Publicacao

#Criando um data-frame com todos os anos
publication.df <- data.frame()
for (i in 1:length(publication[[1]]))
  publication.df <- rbind(publication.df, publication$PERIODICO[[i]])
glimpse(publication.df)

# Limpando o data-frame de listas
publication.df$autores <- gsub("\",\"|\", \"", "; ", publication.df$autores)
publication.df$autores <- gsub("\"|c\\(|\\)", "", publication.df$autores)
publication.df$`autores-endogeno` <- gsub(",", ";", publication.df$`autores-endogeno`)
publication.df$`autores-endogeno` <- gsub("\"|c\\(|\\)", "", publication.df$`autores-endogeno`)
glimpse(publication.df)


  # N√∫mero de resultados (publica√ß√µes e outros) por Tipo e Ano
    # Por Tipo Fixo (PERIODICO)
publication$PERIODICO %>% 
  sapply(function(x) length(x$autores))

    # Por Ano Fixo (2010)
publication %>% 
  sapply(function(x) length(x$"2010"$autores))

    # Por Tipo e Ano Geral
publication %>% 
  sapply(function(x) 
    sapply(x, function(x) 
      length(x$autores)))

#Periodicos ####  
  #Periodicos e o seu total geral e por ano
    # Nome dos (100 primeiros) periodicos em ordem alfab√©tica
publication$PERIODICO %>% 
  sapply(function(x)
    (x$periodico)) %>% 
  unlist() %>% unique() %>% 
  sort() %>% head(100)

    # N√∫mero total de peri√≥dicos geral
publication$PERIODICO %>% 
  sapply(function(x)
    (x$periodico)) %>% 
  unlist() %>% unique() %>% length()

    # N√∫mero total de peri√≥dicos por ano
publication$PERIODICO %>% 
  sapply(function(x)
    length(unique(unlist(x$periodico))))

  #Periodicos mais frequentes (10) por ano
for (i in 1:length(publication$PERIODICO)){
  print(names(publication$PERIODICO[i]))
  publication$PERIODICO[[i]]$periodico %>% 
    table() %>% sort(decreasing = TRUE) %>% 
    head(10) %>% print()
  }

  #Periodicos mais frequentes (20) geral
publication$PERIODICO %>% 
  sapply(function(x) (x$periodico)) %>% 
  unlist() %>% table() %>% sort(decreasing = TRUE) %>% 
  head(20)

  #Lista de revistas
head(sort(table(publication$PERIODICO$`2011`$periodico), decreasing = TRUE), 20)

  #An√°lise de um periodico espec√?fico (Plos One)
publication$PERIODICO %>% 
  sapply(function(x)
    (x$periodico)) %>% unlist() %in% "Plos One" %>% sum()

#Livros e Cap√?tulos ####
  # N√∫mero total de livros por ano e tipo
publication$LIVRO %>% 
  sapply(function(x)
    table(x$tipo))

  # N√∫mero total de livros por ano e natureza
publication$LIVRO %>% 
  sapply(function(x)
    table(x$natureza))

  # Livros publicationados por Pais por ano
publication$LIVRO %>% 
  sapply(function(x)
      sort(table(x$pais_de_publicacao), decreasing = TRUE))

  # Livros publicados por autor geral
publication$LIVRO %>% 
  sapply(function(x)
    (x$"autores-endogeno")) %>% 
  unlist() %>% table() %>% 
  sort(decreasing = TRUE) %>% head(40)

  # Editora de publica√ß√£o
publication$LIVRO %>% 
  sapply(function(x)
    (x$nome_da_editora)) %>% 
  unlist() %>% table() %>% 
  sort(decreasing = TRUE) %>% head(20)

#Evento ####
  # Participa√ß√£o em Evento por Pais por ano
publication$EVENTO %>% 
  sapply(function(x)
    sort(table(x$pais_do_evento), decreasing = TRUE))

  # Participa√ß√£o em Evento por Cidade (40) Geral
publication$EVENTO %>% 
  sapply(function(x)
    (x$cidade_do_evento)) %>% 
  unlist() %>% table() %>% 
  sort(decreasing = TRUE) %>% head()

  # Nome de evento em cidade espec√?fica (Gramado)
(publication$EVENTO %>% 
  sapply(function(x)
    (x$nome_do_evento)) %>% 
  unlist())[publication$EVENTO %>% 
             sapply(function(x)
               (x$cidade_do_evento)) %>% 
             unlist() %in% "Gramado"]

# Formato Data Frame  ####
  # An√°lise dos dados no formato DF
    #Periodico = 1; Livro = 2; Cap√?tulo = 3; Jornais = 4; 
    #Evento = 5; Artigo aceto = 6; Demais produ√ß√µes = 7
publication.periodico.df <- pub.ls2df(publication, 1) #artigos
publication.livros.df <- pub.ls2df(publication, 2) #livros
publication.eventos.df <- pub.ls2df(publication, 5) #eventos
#PublicaÁıes por ano
publication.periodico.df %>% 
  ggplot(aes(x = ano)) + geom_bar(fill = "green4") + 
  geom_text(stat = "count", aes(label=formatC(..count.., big.mark=",")),vjust=-0.4) + 
  theme_bw()+labs(x="Ano",y="Quantidade")+
  scale_y_continuous(labels = comma)
#PublicaÁıes Zika por ano
#publication.periodico.df %>% filter(grepl("zika|zkv|zikv", titulo, ignore.case = TRUE)) %>%
#  ggplot(aes(x = ano)) + geom_bar(fill = "green4") + 
#  geom_text(stat = "count", aes(label=formatC(..count.., big.mark=",")),vjust=-0.4) + 
#  theme_bw()+labs(x="Ano",y="Quantidade")+
#  scale_y_continuous(labels = comma)
#PublicaÁıes Dengue por ano
publication.periodico.df %>% filter(grepl("dengue", titulo, ignore.case = TRUE)) %>%
#  ggplot(aes(x = ano)) + geom_bar(fill = "green4") + 
#  geom_text(stat = "count", aes(label=formatC(..count.., big.mark=",")),vjust=-0.4) + 
#  theme_bw()+labs(x="Ano",y="Quantidade")+
#  scale_y_continuous(labels = comma)
#PublicaÁıes Chikungunya por ano
publication.periodico.df %>% filter(grepl("chikungunya", titulo, ignore.case = TRUE)) %>%
#  ggplot(aes(x = ano)) + geom_bar(fill = "green4") + 
#  geom_text(stat = "count", aes(label=formatC(..count.., big.mark=",")),vjust=-0.4) + 
#  theme_bw()+labs(x="Ano",y="Quantidade")+
#  scale_y_continuous(labels = comma)

#20 revistas mais publicadas
#Mesma vis√£o que anterior mas agora trabalhando no DataFrame
publication.periodico.df %>% select(periodico) %>% table() %>% as.data.frame() %>% arrange(desc(Freq)) %>% 
  head(20) %>% ggplot(aes(x = reorder(., (Freq)), y = Freq)) + geom_col(fill = "red4") + coord_flip() + 
  labs(title = "Os 20 PeriÛdicos com Maior PublicaÁıes", 
       y = "N˙mero de PublicaÁıes", x = "Revistas") + geom_text(aes(label=comma(Freq)),hjust=-0.2,vjust=0.3,size=3.5)+theme_bw()+
  scale_y_continuous(limits=c(0,400))

#publica√ß√£o de livros fora do Brasil
publication.livros.df %>%
  group_by(pais_de_publicacao) %>%
  summarise(Quantidade = n()) %>%
  filter(pais_de_publicacao != "Brasil") %>% 
  ggplot(aes(x = reorder(pais_de_publicacao, (Quantidade)), y = Quantidade)) +
  geom_col(fill = "coral")+ geom_text(aes(label=comma(Quantidade)),hjust=-0.2,vjust=0.3,size=3.5) + coord_flip() +
  labs(title = "PublicaÁıes de Livros em PaÌses Estrangeiros", x = "PaÌses", y = "Quantidade de Livros")+
  theme_bw()

publication.livros.df %>%
  filter(pais_de_publicacao %in% c("Brasil", "Estados Unidos", "Holanda",
                                   "Gr„-Bretanha", "Alemanha", "SuiÁa")) %>%
  group_by(ano,pais_de_publicacao) %>%
  ggplot(aes(x=ano,y=pais_de_publicacao, color= pais_de_publicacao)) +
  xlab("Ano") + ylab("PaÌs") + geom_point() + geom_jitter()+
  labs(color="PaÌs de PublicaÁıes")

#Eventos
publication.eventos.df %>%
  group_by(pais_do_evento) %>%
  summarise(Quantidade = n()) %>%
  filter(pais_do_evento != "Brasil") %>% 
  ggplot(aes(x = reorder(pais_do_evento, (Quantidade)), y = Quantidade)) +
  geom_col(fill = "coral3")+ geom_text(aes(label=comma(Quantidade)),hjust=-0.2,vjust=0.3,size=2.5)+ coord_flip() +
  labs(title = "ParticipaÁ„o de Eventos em PaÌses Estrangeiros", x = "PaÌses", y = "Quantidade de Livros")+theme_bw()

#publication.eventos.zika.df %>%
#  filter(pais_do_evento %in% 
#           c(names(head(sort(table(publication.eventos.df$pais_do_evento)
#                             , decreasing = TRUE), 10)))) %>%
  #  group_by(ano_do_trabalho,pais_do_evento) %>%
#  ggplot(aes(x=ano_do_trabalho,y=pais_do_evento, color= pais_do_evento)) +
#  ggtitle("Participation on Events by Country and Year") +
#  xlab("Year") + ylab("Country") + geom_point() + geom_jitter()+
#  labs(color="Country of The Event")