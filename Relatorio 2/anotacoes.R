ggplot(cic_orient.df,aes(ano, fill=factor(natureza))) +
  geom_bar(stat = "count",position='dodge') +
  ggtitle("Natureza das Orienta��es") +
  theme(legend.position="right",legend.text=element_text(size=7)) +
  guides(fill=guide_legend(nrow=5, byrow=TRUE, title.position = "top")) +
  labs(x="Ano",y="Quantidade")+labs(fill="Natureza")+theme_bw(base_size = 10)+
  geom_text(hjust=0.6,
            vjust=-0.4,
            size=3,color='black',
            position = position_dodge(width=0.9),
            stat = "count", aes(group=factor(natureza),
                                label=formatC(..count.., big.mark=",")),
            check_overlap = TRUE)

###############################################################################

areas_atuacao_cic <- cic_profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$area)) %>% 
  unlist() %>% table() %>% sort(decreasing = TRUE) %>% 
  as.data.frame() %>% filter(!. == "")

quantidade <- sum(areas_atuacao_cic$Freq)
areas_atuacao_cic <- mutate(areas_atuacao_cic, percent = round(areas_atuacao_cic$Freq/quantidade * 100, 0))

colnames(areas_atuacao_cic) <- c("Areas", "Quantidade", "Porcentagem")

ggplot(areas_atuacao_cic, aes(x="", y=Quantidade, fill=Areas))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0, direction = -1) +
  geom_text(data = areas_atuacao_cic, 
            aes(x ="", y=Quantidade, label = paste(Porcentagem, "%")),
            position = position_stack(vjust = 0.5), size=3)

###############################################################################

subarea <- cic_profile %>% 
  sapply(function(x) (x$areas_de_atuacao$sub_area)) %>% 
  unlist() %>% table() %>% 
  sort(decreasing = TRUE) %>%   as.data.frame() %>% 
  filter(!. == "") %>% head(6)

quantidade <- sum(subarea$Freq)

subarea <- mutate(subarea, percent = round(subarea$Freq/quantidade * 100, 0))

colnames(subarea) <- c("Subarea", "Quantidade", "Porcentagem")

ggplot(subarea, aes(x="", y=Quantidade, fill=Subarea))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0, direction = -1) +
  geom_text(data = subarea, 
            aes(x ="", y=Quantidade, label = paste(Porcentagem, "%")),
            position = position_stack(vjust = 0.5))

###############################################################################

especialidades_frequentes <- cic_profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$especialidade)) %>% 
  unlist() %>% table() %>% sort(decreasing = TRUE) %>% 
  as.data.frame() %>% filter(!. == "") %>% head(10) 

quantidade <- sum(especialidades_frequentes$Freq)

especialidades_frequentes <- mutate(especialidades_frequentes, percent = round(especialidades_frequentes$Freq/quantidade * 100, 0))

colnames(especialidades_frequentes) <- c("Especialidade", "Quantidade", "Porcentagem")

ggplot(especialidades_frequentes, aes(x="", y=Quantidade, fill=Especialidade))+
  geom_bar(width = 1, stat = "identity") + 
  geom_text(data = especialidades_frequentes, 
            aes(x ="", y=Quantidade, label = Quantidade),
            position = position_stack(vjust = 0.5))

###############################################################################

cic_public.periodico.df %>% 
  ggplot(aes(x = ano)) + geom_bar(aes(fill=ano)) + 
  geom_text(stat = "count", aes(label=formatC(..count.., big.mark=",")),vjust=-0.1) + 
  theme_bw()+labs(x="Ano",y="Quantidade de Peri�dicos") +
  scale_y_continuous(labels = comma)
```

\pagebreak
#### Publica��es de livros fora do Brasil
```{r echo=FALSE}
cic_public.livros.df %>%
  group_by(pais_de_publicacao) %>%
  summarise(Quantidade = n()) %>%
  filter(pais_de_publicacao != "Brasil") %>% 
  ggplot(aes(x = reorder(pais_de_publicacao, (Quantidade)), y = Quantidade)) +
  geom_col(fill = "coral")+ 
  geom_text(aes(label=comma(Quantidade)),
            hjust=-0.2,
            vjust=0.3,
            size=3.5) + coord_flip() +
  labs(title = "Publica��o de Livros em Pa�ses Estrangeiros", x = "Pa�ses", y = "Quantidade de Livros")+
  theme_bw()

```

\pagebreak
#### Eventos por Pa�s

```{r echo=FALSE}
cic_public.eventos.df %>%
  group_by(pais_do_evento) %>%
  summarise(Quantidade = n()) %>%
  filter(pais_do_evento != "Brasil") %>% 
  ggplot(aes(x = reorder(pais_do_evento, (Quantidade)), y = Quantidade)) +
  geom_col(fill = "coral3")+ 
  geom_text(aes(label=comma(Quantidade)),
            hjust=-0.2,
            vjust=0.3,
            size=2.5)+ coord_flip() +
  labs(title = "Eventos", x = "Pa�ses", y = "Quantidade de Eventos") + theme_bw()

```{r warning = FALSE, echo=FALSE}
comp_public.periodico.df <- pub.ls2df(comp_publication, 1) #artigos
comp_public.livros.df <- pub.ls2df(comp_publication, 2) #livros
comp_public.textojornais.df <- pub.ls2df(comp_publication, 4) #textos em jornais
comp_public.eventos.df <- pub.ls2df(comp_publication, 5) #eventos
```


```{r warning = FALSE, echo=FALSE}
comp_orient.posdoutorado.df <- ori.ls2df(comp_advise, 6) #pos-Doutorado conclu�do
comp_orient.doutorado.df <- ori.ls2df(comp_advise, 7) #Doutorado conclu�do
comp_orient.mestrado.df <- ori.ls2df(comp_advise, 8) #Mestrado conclu�do

comp_orient.df <- rbind(rbind(comp_orient.posdoutorado.df, comp_orient.doutorado.df), comp_orient.mestrado.df)
```

```{r echo=FALSE}
ggplot(comp_orient.df,aes(ano, fill=factor(natureza))) +
  geom_bar(stat = "count",position='dodge') +
  ggtitle("Natureza das Orienta��es") +
  theme(legend.position="right",legend.text=element_text(size=7)) +
  guides(fill=guide_legend(nrow=5, byrow=TRUE, title.position = "top")) +
  labs(x="Ano",y="Quantidade")+labs(fill="Natureza")+theme_bw(base_size = 10)+
  geom_text(hjust=0.6,
            vjust=-0.4,
            size=3,
            color='black',
            position = position_dodge(width=0.9),
            stat = "count", 
            aes(group=factor(natureza),
                label=formatC(..count.., big.mark=",")),
            check_overlap = TRUE)

areas_atuacao_comp <- comp_profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$area)) %>% 
  unlist() %>% table() %>% sort(decreasing = TRUE) %>% 
  as.data.frame() %>% filter(!. == "")

quantidade <- sum(areas_atuacao_comp$Freq)
areas_atuacao_comp <- mutate(areas_atuacao_comp, percent = round(areas_atuacao_comp$Freq/quantidade * 100, 0))

colnames(areas_atuacao_comp) <- c("Areas", "Quantidade", "Porcentagem")

ggplot(areas_atuacao_comp, aes(x="", y=Quantidade, fill=Areas))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0, direction = -1) +
  geom_text(data = areas_atuacao_comp, 
            aes(x ="", y=Quantidade, label = paste(Porcentagem, "%")),
            position = position_stack(vjust = 0.5), size=3)
```

Percebe-se que a �rea de atua��o com maior recorr�ncia � a *Ci�ncia da Computa��o* o que faz todo sentido j� que se trata de um programa de Computa��o.
Percebe-se tamb�m uma grande porcentagem em rela��o �s demais �reas existe uma distribui��o bem uniforme, demonstrando que o ponto forte desse programa de fato s�o assuntos relacionados � Computa��o e Ci�ncia da Computa��o.

\pagebreak
#### Distribui��o de sub�reas de atua��o mais frequentes dos pesquisadores

```{r echo=FALSE}
subarea <- comp_profile %>% 
  sapply(function(x) (x$areas_de_atuacao$sub_area)) %>% 
  unlist() %>% table() %>% 
  sort(decreasing = TRUE) %>%   as.data.frame() %>% 
  filter(!. == "") %>% head(6)

quantidade <- sum(subarea$Freq)

subarea <- mutate(subarea, percent = round(subarea$Freq/quantidade * 100, 0))

colnames(subarea) <- c("Subarea", "Quantidade", "Porcentagem")

ggplot(subarea, aes(x="", y=Quantidade, fill=Subarea))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0, direction = -1) +
  geom_text(data = subarea, 
            aes(x ="", y=Quantidade, label = paste(Porcentagem, "%")),
            position = position_stack(vjust = 0.5))


```

Atrav�s da an�lise acima, pode-se ver que a �rea com maior n�mero de docentes atuantes � em *Sistemas de Computa��o*, seguida de perto por *Metodologia e T�cnicas de Computa��o* e *Teoria da Computa��o*.

\pagebreak
#### Distribui��o de especialidades mais frequentes dos pesquisadores
```{r echo=FALSE}
especialidades_frequentes <- comp_profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$especialidade)) %>% 
  unlist() %>% table() %>% sort(decreasing = TRUE) %>% 
  as.data.frame() %>% filter(!. == "") %>% head(10) 

quantidade <- sum(especialidades_frequentes$Freq)

especialidades_frequentes <- mutate(especialidades_frequentes, percent = round(especialidades_frequentes$Freq/quantidade * 100, 0))

colnames(especialidades_frequentes) <- c("Especialidade", "Quantidade", "Porcentagem")

ggplot(especialidades_frequentes, aes(x="", y=Quantidade, fill=Especialidade))+
  geom_bar(width = 1, stat = "identity") + 
  geom_text(data = especialidades_frequentes, 
            aes(x ="", y=Quantidade, label = Quantidade),
            position = position_stack(vjust = 0.5))

```
Aqui v�-se as 10 especialidades mais recorrentes e percebe-se uma boa distribui��o da quantidade entre elas.

### Publica��es

#### Quantidade de Publica��es por tipo
```{r echo=FALSE}
for (i in 1:length(comp_publication)){
  print(names(comp_publication[i]))
  print(comp_publication[[i]] %>% 
          sapply(function(x)
            length(x$ano)) %>% sum())
}
```

Percebe-se que o tipo mais recorrente de publica��es � do tipo Evento, possui bem mais ocorr�ncias do que o segundo colocado, Peri�dico.

#### Quantidade de publica��es por tipo e por ano 
```{r echo=FALSE}
publication_tipo_comp <- comp_publication %>% 
  sapply(function(x) 
    sapply(x, function(x)
      length(x$autores)))
publication_tipo_comp
```

Destaca-se que nos ano de 2016 houveram mais publica��es de Peri�dicos e Eventos. Outro ponto interessante � a aus�ncia de peri�dicos do tipo Artigo Aceito e Texto em Jornais em todos os anos.

#### Participa��o em eventos por pa�s

```{r echo=FALSE}
comp_publication$EVENTO %>% 
  sapply(function(x)
    (x$pais_do_evento)) %>% 
  unlist() %>% table() %>% 
  sort(decreasing = TRUE)
```

Este programa possui um bom n�mero de participa��es em eventos em outros pa�ses. 

\pagebreak
#### Eventos por pa�s

```{r echo=FALSE, out.width='100%'}
comp_public.eventos.df %>%
  group_by(pais_do_evento) %>%
  summarise(Quantidade = n()) %>%
  filter(pais_do_evento != "Brasil") %>% 
  ggplot(aes(x = reorder(pais_do_evento, (Quantidade)), y = Quantidade)) +
  geom_col(fill = "coral3")+ 
  geom_text(aes(label=comma(Quantidade)),
            hjust=-0.2,
            vjust=0.3,
            size=2.5)+ coord_flip() +
  labs(title = "Eventos", x = "Pa�ses", y = "Quantidade de Eventos") + theme_bw()
```


## P�s-Gradua��o em Matem�tica

### Orienta��es

#### N�meros de orienta��es completas por ano

```{r echo=FALSE}
for (i in 6:9){
  print(names(mat_advise[i]))
  print(mat_advise[[i]] %>% 
          sapply(function(x)
            length(x$ano)))
}
```

No geral percebe-se que existem mais orienta��es conclu�das do tipo Mestrado e Outras Orienta��es, havendo bem poucas orienta��es de P�s Doutorado. Pode-se supor que isto aconte�a devido ao n�vel de exig�ncia das orienta��es de P�s Doutorado.

#### Disposi��o de orienta��es por situa��o e ano

```{r echo=FALSE}
for (i in 1:length(mat_advise)){
  print(names(mat_advise[i]))
  print(mat_advise[[i]] %>% 
          sapply(function(x)
            length(x$ano)) )
}
```

\pagebreak
#### Natureza das orienta��es

```{r warning = FALSE, echo=FALSE}
mat_public.periodico.df <- pub.ls2df(mat_publication, 1) #artigos
mat_public.livros.df <- pub.ls2df(mat_publication, 2) #livros
mat_public.textojornais.df <- pub.ls2df(mat_publication, 4) #textos em jornais
mat_public.eventos.df <- pub.ls2df(mat_publication, 5) #eventos
```


```{r warning = FALSE, echo=FALSE}
mat_orient.posdoutorado.df <- ori.ls2df(mat_advise, 6) #pos-Doutorado conclu�do
mat_orient.doutorado.df <- ori.ls2df(mat_advise, 7) #Doutorado conclu�do
mat_orient.mestrado.df <- ori.ls2df(mat_advise, 8) #Mestrado conclu�do

mat_orient.df <- rbind(rbind(mat_orient.posdoutorado.df, mat_orient.doutorado.df), mat_orient.mestrado.df)
```

```{r echo=FALSE}
ggplot(mat_orient.df,aes(ano, fill=factor(natureza))) +
  geom_bar(stat = "count",position='dodge') +
  ggtitle("Natureza das Orienta��es") +
  theme(legend.position="right",legend.text=element_text(size=7)) +
  guides(fill=guide_legend(nrow=5, byrow=TRUE, title.position = "top")) +
  labs(x="Ano",y="Quantidade")+labs(fill="Natureza")+theme_bw(base_size = 10)+
  geom_text(hjust=0.6,
            vjust=-0.4,
            size=3,color='black',
            position = position_dodge(width=0.9),
            stat = "count", 
            aes(group=factor(natureza),
                label=formatC(..count.., big.mark=",")),
            check_overlap = TRUE)
```

\pagebreak
### Perfil

#### Distribui��o de �reas de atua��o dos pesquisadores

```{r echo=FALSE}
areas_atuacao_mat <- mat_profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$area)) %>% 
  unlist() %>% table() %>% sort(decreasing = TRUE) %>% 
  as.data.frame() %>% filter(!. == "")

quantidade <- sum(areas_atuacao_mat$Freq)
areas_atuacao_mat <- mutate(areas_atuacao_mat, percent = round(areas_atuacao_mat$Freq/quantidade * 100, 0))

colnames(areas_atuacao_mat) <- c("Areas", "Quantidade", "Porcentagem")

ggplot(areas_atuacao_mat, aes(x="", y=Quantidade, fill=Areas))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0, direction = -1) +
  geom_text(data = areas_atuacao_mat, 
            aes(x ="", y=Quantidade, label = paste(Porcentagem, "%")),
            position = position_stack(vjust = 0.5), size=3)
```

Percebe-se que a �rea de atua��o com maior recorr�ncia � a *Ci�ncia da Computa��o* e apenas uma parcela reduzida equivale a �rea *Matem�tica*, o que se analisado isoladamente pode causar estranheza, pois seria de se esperar que no programa de Matem�tica a �rea *Matem�tica* tivesse uma maior porcentagem.

\pagebreak
#### Distribui��o de sub�reas de atua��o mais frequentes dos pesquisadores

```{r echo=FALSE}
subarea <- mat_profile %>% 
  sapply(function(x) (x$areas_de_atuacao$sub_area)) %>% 
  unlist() %>% table() %>% 
  sort(decreasing = TRUE) %>%   as.data.frame() %>% 
  filter(!. == "") %>% head(6)

quantidade <- sum(subarea$Freq)

subarea <- mutate(subarea, percent = round(subarea$Freq/quantidade * 100, 0))

colnames(subarea) <- c("Subarea", "Quantidade", "Porcentagem")

ggplot(subarea, aes(x="", y=Quantidade, fill=Subarea))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0, direction = -1) +
  geom_text(data = subarea, 
            aes(x ="", y=Quantidade, label = paste(Porcentagem, "%")),
            position = position_stack(vjust = 0.5))


```

Atrav�s da an�lise acima, pode-se ver que a �rea com maior n�mero de docentes atuantes � em *Geometria e Topologia*, seguida por *An�lise*, *Matem�tica Aplicada* e *Teoria da Computa��o*.

\pagebreak
#### Distribui��o de especialidades mais frequentes dos pesquisadores
```{r echo=FALSE}
especialidades_frequentes <- comp_profile %>% 
  sapply(function(x) unique(x$areas_de_atuacao$especialidade)) %>% 
  unlist() %>% table() %>% sort(decreasing = TRUE) %>% 
  as.data.frame() %>% filter(!. == "") %>% head(10) 

quantidade <- sum(especialidades_frequentes$Freq)

especialidades_frequentes <- mutate(especialidades_frequentes, percent = round(especialidades_frequentes$Freq/quantidade * 100, 0))

colnames(especialidades_frequentes) <- c("Especialidade", "Quantidade", "Porcentagem")

ggplot(especialidades_frequentes, aes(x="", y=Quantidade, fill=Especialidade))+
  geom_bar(width = 1, stat = "identity") + 
  geom_text(data = especialidades_frequentes, 
            aes(x ="", y=Quantidade, label = Quantidade),
            position = position_stack(vjust = 0.5))

```
Aqui v�-se as 10 especialidades mais recorrentes e percebe-se uma boa distribui��o da quantidade entre elas.

### Publica��es

#### Quantidade de Publica��es por tipo
```{r echo=FALSE}
for (i in 1:length(mat_publication)){
  print(names(mat_publication[i]))
  print(mat_publication[[i]] %>% 
          sapply(function(x)
            length(x$ano)) %>% sum())
}
```
Percebe-se que o tipo mais recorrente de publica��es � do tipo Peri�dico.

#### Quantidade de publica��es por tipo e por ano 
```{r echo=FALSE}
publication_tipo_mat <- mat_publication %>% 
  sapply(function(x) 
    sapply(x, function(x)
      length(x$autores)))
publication_tipo_mat
```

Destaca-se que nos anos de 2016 e 2017 houveram mais publica��es de Peri�dicos e em 2018 houve uma queda, mas no ano de 2018 houveram mais publica��es do tipo Artigo Aceito e Demais Tipos de Produ��o Bibliografica. Pode-se imaginar que o programa focou mais nesses outros tipos de publica��o no ano de 2018 em rela��o a Peri�dicos.

#### Participa��o em eventos por pa�s

```{r echo=FALSE}
mat_publication$EVENTO %>% 
  sapply(function(x)
    (x$pais_do_evento)) %>% 
  unlist() %>% table() %>% 
  sort(decreasing = TRUE)
```

� interessante que o pa�s tenha participado de mais eventos na Fran�a do que Chile que � um pa�s mais pr�ximo do Brasil. 

\pagebreak
#### Publica��es por ano

```{r echo=FALSE, out.width='100%'}
mat_public.periodico.df %>% 
  ggplot(aes(x = ano)) + geom_bar(aes(fill=ano)) + 
  geom_text(stat = "count", aes(label=formatC(..count.., big.mark=",")),vjust=-0.1) + 
  theme_bw()+labs(x="Ano",y="Quantidade de Peri�dicos") +
  scale_y_continuous(labels = comma)
```

\pagebreak
#### Publica��es de livros fora do Brasil
```{r echo=FALSE}
mat_public.livros.df %>%
  group_by(pais_de_publicacao) %>%
  summarise(Quantidade = n()) %>%
  filter(pais_de_publicacao != "Brasil") %>% 
  ggplot(aes(x = reorder(pais_de_publicacao, (Quantidade)), y = Quantidade)) +
  geom_col(fill = "coral")+ 
  geom_text(aes(label=comma(Quantidade)),
            hjust=-0.2,vjust=0.3,size=3.5) + coord_flip() +
  labs(title = "Publica��o de Livros em Pa�ses Estrangeiros", 
       x = "Pa�ses", 
       y = "Quantidade de Livros")+
  theme_bw()

```

\pagebreak
#### Eventos por pa�s

```{r echo=FALSE, out.width='100%'}
mat_public.eventos.df %>%
  group_by(pais_do_evento) %>%
  summarise(Quantidade = n()) %>%
  filter(pais_do_evento != "Brasil") %>% 
  ggplot(aes(x = reorder(pais_do_evento, (Quantidade)), y = Quantidade)) +
  geom_col(fill = "coral3")+ 
  geom_text(aes(label=comma(Quantidade)),
            hjust=-0.2,vjust=0.3,size=2.5)+ coord_flip() +
  labs(title = "Eventos", x = "Pa�ses", y = "Quantidade de Eventos") + theme_bw()
```

# Modelos de An�lise

## An�lise de Redes
Uma rede (ou um grafo de redes) representa uma conex�o entre indiv�duos. Na representa��o por redes, a presen�a de uma aresta entre um ou mais n�s representa a exist�ncia de uma rela��o entre os indiv�duos conectados, formando assim a rede. H� v�rios tipos de padr�es e relacionamentos que podem ser modelos e representados como uma rede dessa maneira.

A partir dos arquivos graph.json podemos criar modelos de relacionamento entre os pesquisadores de diversos departamentos. Para tal ser� utilizada a biblioteca **igraph**, que permite a visualiza��o gr�fica desses relacionamentos na forma de uma estrutura de dados conhecida como *grafo*.

Come�amos nossa an�lise carregando a biblioteca **igraph**:
  
  ```{r error=FALSE, message=FALSE}
library(igraph)
```

A an�lise de redes detalhada ser� feita para cada um dos programas de p�s-gradua��o a seguir:
  
  ### Ci�ncia da Computa��o
  Come�amos carregando o arquivo json para a mem�ria e criando vari�veis para representar os n�s e os relacionamentos entre os diversos autores do programa de P�s-Gradua��o em Ci�ncia da Computa��o.
```{r}
cic_graph <- jsonlite::fromJSON("cic_graph.json")
cic_nodes <- cic_graph$nodes
cic_nodes$properties <- NULL
cic_relations <- cic_graph$links
```

Podemos observar as primeiras linhas dos n�s (ou seja, os pesquisadores em si):
  
  ```{r echo=FALSE}
head(cic_nodes)
```

O mesmo pode ser feita para ser observar os relacionamentos (as arestas no grafo):
  
  ```{r echo=FALSE}
head(cic_relations)
```

A biblioteca **igraph** facilita bastante o processo de gera��o das redes a partir dos dados fornecidos.

```{r echo=FALSE}
graph_cic <- graph_from_data_frame(cic_relations, directed=TRUE, vertices=cic_nodes)
plot(graph_cic)
```

Pode-se observar facilmente que h� dois *clusters* distintos de pesquisadores que j� trabalharam em conjunto, al�m de tr�s outros pesquisadores que trabalham apenas sozinhos.

### Computa��o
Fazendo-se a an�lise anterior para a P�s-Gradua��o em Computa��o, obtemos os seguintes resultados:
  
  ```{r warning = FALSE, echo=FALSE}
comp_graph <- jsonlite::fromJSON("comp_graph.json")
comp_nodes <- comp_graph$nodes
comp_nodes$properties <- NULL
comp_relations <- comp_graph$links
```

Observando-se os primeiros dados dos conjuntos de n�s e das arestas:
  
  ```{r echo=FALSE}
head(comp_nodes)
```


```{r echo=FALSE}
head(comp_relations)
```

```{r echo=FALSE}
graph_comp <- graph_from_data_frame(comp_relations, directed=TRUE, vertices=comp_nodes)
plot(graph_comp)
```

Na an�lise para a P�s-Gradua��o em Computa��o fica bem claro que h� um n�mero menor de pesquisadores, e diferentemente da P�s-Gradua��o em Ci�ncia da Computa��o, aqui fica bem claro que n�o h� uma separa��o t�o grandes entre diferentes *clusters* de pesquisa.

### Matem�tica
Por �ltimo, a an�lise de redes ser� feita para a P�s-Gradua��o em Matem�tica.

```{r warning = FALSE, echo=FALSE}
mat_graph <- jsonlite::fromJSON("mat_graph.json")
mat_nodes <- mat_graph$nodes
mat_nodes$properties <- NULL
mat_relations <- mat_graph$links
```

Observando-se os dados:
  
  ```{r echo=FALSE}
head(mat_nodes)
```

```{r echo=FALSE}
head(mat_relations)
```

```{r warning = FALSE, echo=FALSE}
graph_mat <- graph_from_data_frame(mat_relations, directed=TRUE, vertices=mat_nodes)
plot(graph_mat)
```
