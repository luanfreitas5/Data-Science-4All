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

#Arquivo com funcionalidades que transformam o arquivo formato list em DataFrames
source("elattes.ls2df.R")

#Definir o local onde estão os arquivos json
#setwd("~/Documents/eLattes/Pacote e-Lattes/UnBPosGeral")

#Ler o arquivo advise
file.info("Geotecnia/advise.json")
advise <- fromJSON("Geotecnia/advise.json")

#Visualizar o arquivo no formato list
listviewer::jsonedit(advise)

#Dados Descritivos Gerais ####
# Tipos de objetos armazenados
names(advise)
names(advise$ORIENTACAO_CONCLUIDA_DOUTORADO)

length(advise$ORIENTACAO_CONCLUIDA_DOUTORADO$`2016`$natureza)

head(sort(table(advise$ORIENTACAO_CONCLUIDA_DOUTORADO$`2017`$curso), decreasing = TRUE), 10)

head(sort(table(advise$ORIENTACAO_CONCLUIDA_MESTRADO$`2017`$curso), decreasing = TRUE), 10)

# Periodo analisado
names(advise[[1]])

# Número de resultados (orientações) por Tipo e Ano
# Por Tipo Fixo (ORIENTACAO_CONCLUIDA_DOUTORADO)
advise$ORIENTACAO_CONCLUIDA_DOUTORADO %>% 
  sapply(function(x) length(x$natureza))

# Por Ano Fixo (2010)
advise %>% 
  sapply(function(x) length(x$"2010"$natureza))

# Por Tipo e Ano Geral
advise %>% 
  sapply(function(x) 
    sapply(x, function(x) 
      length(x$natureza)))

# Ano - Todas orientações por tipo, por ano e para todos os anos
for (i in 1:length(advise)){
  print(names(advise[i]))
  print(advise[[i]] %>% 
          sapply(function(x)
            length(x$ano)) %>% sum())
}

for (i in 1:length(advise[[1]])){
  print(names(advise[[1]][i]))
  print(advise %>% 
          sapply(function(x)
            length(x[[i]]$ano)) %>% sum())
}

advise %>% 
  sapply(function(x) 
    sapply(x, function(x) 
      length(x$natureza))) %>% sum()

  # Detalhe em OUTRAS_ORIENTACOES_CONCLUIDAS
advise$OUTRAS_ORIENTACOES_CONCLUIDAS %>% 
  sapply(function(x) 
    table(x$natureza))

#Oreintação ####  
  #Avaliação do segundo nível hierárquico
    # Titulo - Busca por palavra no título (Zika) ####
    # Resultado Título
(advise %>% 
  sapply(function(x) 
    sapply(x, function(x)
      (x$titulo))) %>% 
  unlist())[advise %>% 
             sapply(function(x) 
               sapply(x, function(x) 
                 (x$titulo))) %>%
             unlist() %>% grepl(pattern = "Zika")]
      # Resultado Autor
(advise %>% 
    sapply(function(x) 
      sapply(x, function(x)
        (x$nome_aluno))) %>% 
    unlist())[advise %>% 
                sapply(function(x) 
                  sapply(x, function(x) 
                    (x$titulo))) %>%
                unlist() %>% grepl(pattern = "Zika")]
        # Resultado Tipo de orientação / trabalho
(advise %>% 
    sapply(function(x) 
      sapply(x, function(x)
        (x$natureza))) %>% 
    unlist())[advise %>% 
                sapply(function(x) 
                  sapply(x, function(x) 
                    (x$titulo))) %>%
                unlist() %>% grepl(pattern = "Zika")] %>% 
  table()

        # Resultado - Análise em Tipo e período específico
advise$OUTRAS_ORIENTACOES_CONCLUIDAS$`2017`$titulo %>% 
  grepl(pattern = "Zika")

    # Por instituição ####
      #Instituição onde os pesquisadores / professores orientam
advise %>% 
  sapply(function(x) 
    sapply(x, function(x) 
      (x$instituicao))) %>% 
  unlist() %>% table() %>% 
  sort(decreasing = TRUE) %>% head(20)

    # Por Curso ####
      #Cursos mais orientados dos pesquisadores / professores
advise %>% 
  sapply(function(x) 
    sapply(x, function(x) 
      (x$curso))) %>% 
  unlist() %>% table() %>% 
  sort(decreasing = TRUE) %>% head(40)

    # Por Agencia Financiadora ####
      #Agencias que mais financiaram
advise %>% 
  sapply(function(x) 
    sapply(x, function(x) 
      (x$agencia_financiadora))) %>% 
  unlist() %>% table() %>% 
  sort(decreasing = TRUE) %>% head(30)

#Orienta��o
#Visualizar a estrutura do json no painel Viewer
#jsonedit(advise)
#Reunir todos os anos e orienta��es concluidas em um mesmo data-frame
advise.tipo.df <- data.frame(); advise.df <- data.frame()
for (i in 1:length(advise[[1]]))
  advise.tipo.df <- rbind(advise.tipo.df, advise$ORIENTACAO_CONCLUIDA_POS_DOUTORADO[[i]])
advise.df <- rbind(advise.df, advise.tipo.df); advise.tipo.df <- data.frame()
for (i in 1:length(advise[[1]]))
  advise.tipo.df <- rbind(advise.tipo.df, advise$ORIENTACAO_CONCLUIDA_DOUTORADO[[i]])
advise.df <- rbind(advise.df, advise.tipo.df); advise.tipo.df <- data.frame()
for (i in 1:length(advise[[1]]))
  advise.tipo.df <- rbind(advise.tipo.df, advise$ORIENTACAO_CONCLUIDA_MESTRADO[[i]])
advise.df <- rbind(advise.df, advise.tipo.df)
glimpse(advise.df)

#Transformar as colunas de listas em caracteres eliminando c("")
advise.df$nome_orientadores <- gsub("\"|c\\(|\\)", "", advise.df$nome_orientadores)
advise.df$id_lattes_orientadores <- gsub("\"|c\\(|\\)", "", advise.df$id_lattes_orientadores)

#Separar as colunas com dois orientadores
advise.df <- separate(advise.df, nome_orientadores, into = c("ori1", "ori2"), sep = ",")

advise.df <- separate(advise.df, id_lattes_orientadores, into = c("idLattes1", "idLattes2"), sep = ",")

#Numero de orientacoes por ano
table(advise.df$ano)

#Tabela com nome de professor e numero de orientacoes
head(sort(table(rbind(advise.df$ori1, advise.df$ori2)), decreasing = TRUE), 20)


# Formato Data Frame  ####
# Análise dos dados no formato DF
#ORIENTACAO_EM_ANDAMENTO_DE_POS_DOUTORADO = 1; ORIENTACAO_EM_ANDAMENTO_DOUTORADO = 2; 
#ORIENTACAO_EM_ANDAMENTO_MESTRADO = 3; ORIENTACAO_EM_ANDAMENTO_GRADUACAO = 4; 
#ORIENTACAO_EM_ANDAMENTO_INICIACAO_CIENTIFICA = 5; ORIENTACAO_CONCLUIDA_POS_DOUTORADO = 6; 
#ORIENTACAO_CONCLUIDA_DOUTORADO = 7; ORIENTACAO_CONCLUIDA_MESTRADO = 8; 
#OUTRAS_ORIENTACOES_CONCLUIDAS = 9
orient.posdoutorado.df <- ori.ls2df(advise, 6) #pos-Doutorado concluído
orient.doutorado.df <- ori.ls2df(advise, 7) #Doutorado concluído
orient.mestrado.df <- ori.ls2df(advise, 8) #Mestrado concluído

orient.posdoutorado_p.df <- ori.ls2df(advise, 1) #pos-Doutorado concluído
orient.doutorado_p.df <- ori.ls2df(advise, 2) #Doutorado concluído
orient.mestrado_p.df <- ori.ls2df(advise, 3) #Mestrado concluído

orient.df <- rbind(rbind(orient.posdoutorado.df, orient.doutorado.df), orient.mestrado.df)
orient_p.df <- rbind(rbind(orient.posdoutorado_p.df, orient.doutorado_p.df), orient.mestrado_p.df)

orient_sum <- group_by(orient.df, ano, natureza) %>%
                summarise(n = n())
orient_p_sum <- group_by(orient_p.df, ano, natureza) %>%
  summarise(n = n())

ggplot(orient.df,aes(ano, fill=factor(natureza))) +
  geom_bar(stat = "count", position = "dodge") +
  ggtitle("Natureza das Orienta��es Completas Por Ano") +
  theme(legend.position="right",legend.text=element_text(size=7)) +
  guides(fill=guide_legend(nrow=5, byrow=TRUE, title.position = "top")) +
  labs(x="Ano",y="Quantidade")+labs(fill="Natureza")+theme_bw()+
  geom_text(hjust=0.6,vjust=-0.4,size=3,color='black',position = position_dodge(width=0.9),stat = "count", aes(group=factor(natureza),label=formatC(..count.., big.mark=",")),check_overlap = TRUE)

  
ggplot(orient_sum, aes(x = ano, y = n, group = natureza, color = natureza)) +
  geom_line(alpha = 0.6) +
  geom_point(alpha = 0.6)

ggplot(orient_p_sum, aes(x = ano, y = n, group = natureza, color = natureza)) +
    geom_line(aes(alpha = 0.3)) +
    geom_point(aes(alpha = 0.3))