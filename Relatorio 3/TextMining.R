#An√°lise inicial dos arquivos gerados pelo e-Lattes

#Pacotes para serem ativados
library(tidyverse)
library(tm)
library(jsonlite)
library(listviewer)

#upload de arquivo com fun√ß√µes para transformar listas em Data Frames e objeto igraph
source("elattes.ls2df.R")
public <- fromJSON("gaexatas.publication.json")
listviewer::jsonedit(public)

# Visualizando alguns dados ####
#1. Qual o nome dos autores e titulo dos livros publicados
public$LIVRO %>%
  sapply(function(x)
    unlist(x$autores))
public$LIVRO %>%
  sapply(function(x)
    (x$titulo))

#2. Quais as cidades que tiveram participa√ß√£o de pesquisadores em 10 eventos
(
  public$EVENTO %>%
    sapply(function(x)
      (x$cidade_do_evento)) %>%
    unlist() %>% table() %>% sort(decreasing = TRUE) == 10
) %>%
  which() %>% names()

#3. Qual o n√∫mero de publica√ß√µes em artigos (periodicos) e eventos por ano
public$PERIODICO %>%
  sapply(function(x)
    length(x$titulo))

public$EVENTO %>%
  sapply(function(x)
    length(x$titulo))

#Tratamento dos Dados ####
#Buscando e transofmrando os dados em um Corpus
tit_pub <- public$PERIODICO %>%
  sapply(function(x)
    (x$titulo)) %>% unlist()
print(tit_pub)

tit_source <- VectorSource(tit_pub)
#Pode ser feito o mesmo com um DataFrame com DataframeSource()
tit_corpus <- VCorpus(tit_source)
print(tit_corpus)
tit_corpus[[11]]
tit_corpus[[11]]$content

#Limpando os dados
# tolower (): coloque todos os caracteres em min˙sculas
# removePunctuation (): remova todos os sinais de pontuaÁ„o
# removeNumbers (): remover n˙meros
# stripWhitespace (): remova o excesso de espaÁo em branco

tolower(tit_corpus[[11]]$content)
removePunctuation(tit_corpus[[11]]$content)
removeNumbers(tit_corpus[[11]]$content)

#StopWords
stopwords("en")
stopwords("pt")
removeWords(tit_corpus[[11]]$content, stopwords("en")) %>% stripWhitespace()
new_stops <- c("graph", stopwords("en"))
removeWords(tit_corpus[[11]]$content, new_stops) %>% stripWhitespace()

frase <- "A Cross_Ref, base de @publicaÁıes, contÈm milhÙes de registros"

fraseLimpa <- tolower(frase) %>%
removeWords(stopwords("pt")) %>% removePunctuation() %>% stripWhitespace()

fraseCorpus <- VectorSource(fraseLimpa) %>% VCorpus()

library(RWeka)

trigrama <- DocumentTermMatrix(fraseCorpus, 
                                 control = list(tokenize = tokenizer))
  
trigrama$dimnames

tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 3, max = 3))
}

#Stemming
stem_doc <-
  stemDocument(c("computational", "computers", "computation"))
comp_dict <- c("computer")
stemCompletion(stem_doc, comp_dict)

#limpando todo o Corpus
tit_clean <-
  tm_map(tit_corpus, removeWords, words = c(stopwords("en")))

#Term Document Matrix (TDM) ou Document Term Matrix (DTM)
tit_dtm <- DocumentTermMatrix(tit_clean)
tit_dtm
tit_m1 <- as.matrix(tit_dtm)
dim(tit_m1)
tit_m1[1:10, 1000:1010]

tit_tdm <- TermDocumentMatrix(tit_clean)
tit_tdm
tit_m2 <- as.matrix(tit_tdm)
dim(tit_m2)
tit_m2[1000:1010, 2000:2010]


#Visualiza√ß√£o de Dados ####
term_frequency <- rowSums(tit_m2)
term_frequency <- sort(term_frequency, decreasing = TRUE)
head(term_frequency, 10)

library(wordcloud)
library(viridisLite)

# Create word_freqs
term <- names(term_frequency)
num <- term_frequency
word_freqs <- data.frame(term, num)

# Create a wordcloud for the values in word_freqs
wordcloud(
  word_freqs$term,
  word_freqs$num,
  max.words = 100,
  colors = 'red'
)
