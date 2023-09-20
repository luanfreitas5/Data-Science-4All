ggplot(cic_orient.df,aes(ano, fill=factor(natureza))) +
  geom_bar(stat = "count",position='dodge') +
  ggtitle("Natureza das Orientações") +
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
  theme_bw()+labs(x="Ano",y="Quantidade de Periódicos") +
  scale_y_continuous(labels = comma)
```

\pagebreak
#### Publicações de livros fora do Brasil
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
  labs(title = "Publicação de Livros em Países Estrangeiros", x = "Países", y = "Quantidade de Livros")+
  theme_bw()

```

\pagebreak
#### Eventos por País

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
  labs(title = "Eventos", x = "Países", y = "Quantidade de Eventos") + theme_bw()

```{r warning = FALSE, echo=FALSE}
comp_public.periodico.df <- pub.ls2df(comp_publication, 1) #artigos
comp_public.livros.df <- pub.ls2df(comp_publication, 2) #livros
comp_public.textojornais.df <- pub.ls2df(comp_publication, 4) #textos em jornais
comp_public.eventos.df <- pub.ls2df(comp_publication, 5) #eventos
```


```{r warning = FALSE, echo=FALSE}
comp_orient.posdoutorado.df <- ori.ls2df(comp_advise, 6) #pos-Doutorado concluído
comp_orient.doutorado.df <- ori.ls2df(comp_advise, 7) #Doutorado concluído
comp_orient.mestrado.df <- ori.ls2df(comp_advise, 8) #Mestrado concluído

comp_orient.df <- rbind(rbind(comp_orient.posdoutorado.df, comp_orient.doutorado.df), comp_orient.mestrado.df)
```

```{r echo=FALSE}
ggplot(comp_orient.df,aes(ano, fill=factor(natureza))) +
  geom_bar(stat = "count",position='dodge') +
  ggtitle("Natureza das Orientações") +
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

Percebe-se que a área de atuação com maior recorrência é a *Ciência da Computação* o que faz todo sentido já que se trata de um programa de Computação.
Percebe-se também uma grande porcentagem em relação às demais áreas existe uma distribuição bem uniforme, demonstrando que o ponto forte desse programa de fato são assuntos relacionados à Computação e Ciência da Computação.

\pagebreak
#### Distribuição de subáreas de atuação mais frequentes dos pesquisadores

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

Através da análise acima, pode-se ver que a área com maior número de docentes atuantes é em *Sistemas de Computação*, seguida de perto por *Metodologia e Técnicas de Computação* e *Teoria da Computação*.

\pagebreak
#### Distribuição de especialidades mais frequentes dos pesquisadores
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
Aqui vê-se as 10 especialidades mais recorrentes e percebe-se uma boa distribuição da quantidade entre elas.

### Publicações

#### Quantidade de Publicações por tipo
```{r echo=FALSE}
for (i in 1:length(comp_publication)){
  print(names(comp_publication[i]))
  print(comp_publication[[i]] %>% 
          sapply(function(x)
            length(x$ano)) %>% sum())
}
```

Percebe-se que o tipo mais recorrente de publicações é do tipo Evento, possui bem mais ocorrências do que o segundo colocado, Periódico.

#### Quantidade de publicações por tipo e por ano 
```{r echo=FALSE}
publication_tipo_comp <- comp_publication %>% 
  sapply(function(x) 
    sapply(x, function(x)
      length(x$autores)))
publication_tipo_comp
```

Destaca-se que nos ano de 2016 houveram mais publicações de Periódicos e Eventos. Outro ponto interessante é a ausência de periódicos do tipo Artigo Aceito e Texto em Jornais em todos os anos.

#### Participação em eventos por país

```{r echo=FALSE}
comp_publication$EVENTO %>% 
  sapply(function(x)
    (x$pais_do_evento)) %>% 
  unlist() %>% table() %>% 
  sort(decreasing = TRUE)
```

Este programa possui um bom número de participações em eventos em outros países. 

\pagebreak
#### Eventos por país

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
  labs(title = "Eventos", x = "Países", y = "Quantidade de Eventos") + theme_bw()
```


## Pós-Graduação em Matemática

### Orientações

#### Números de orientações completas por ano

```{r echo=FALSE}
for (i in 6:9){
  print(names(mat_advise[i]))
  print(mat_advise[[i]] %>% 
          sapply(function(x)
            length(x$ano)))
}
```

No geral percebe-se que existem mais orientações concluídas do tipo Mestrado e Outras Orientações, havendo bem poucas orientações de Pós Doutorado. Pode-se supor que isto aconteça devido ao nível de exigência das orientações de Pós Doutorado.

#### Disposição de orientações por situação e ano

```{r echo=FALSE}
for (i in 1:length(mat_advise)){
  print(names(mat_advise[i]))
  print(mat_advise[[i]] %>% 
          sapply(function(x)
            length(x$ano)) )
}
```

\pagebreak
#### Natureza das orientações

```{r warning = FALSE, echo=FALSE}
mat_public.periodico.df <- pub.ls2df(mat_publication, 1) #artigos
mat_public.livros.df <- pub.ls2df(mat_publication, 2) #livros
mat_public.textojornais.df <- pub.ls2df(mat_publication, 4) #textos em jornais
mat_public.eventos.df <- pub.ls2df(mat_publication, 5) #eventos
```


```{r warning = FALSE, echo=FALSE}
mat_orient.posdoutorado.df <- ori.ls2df(mat_advise, 6) #pos-Doutorado concluído
mat_orient.doutorado.df <- ori.ls2df(mat_advise, 7) #Doutorado concluído
mat_orient.mestrado.df <- ori.ls2df(mat_advise, 8) #Mestrado concluído

mat_orient.df <- rbind(rbind(mat_orient.posdoutorado.df, mat_orient.doutorado.df), mat_orient.mestrado.df)
```

```{r echo=FALSE}
ggplot(mat_orient.df,aes(ano, fill=factor(natureza))) +
  geom_bar(stat = "count",position='dodge') +
  ggtitle("Natureza das Orientações") +
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

#### Distribuição de áreas de atuação dos pesquisadores

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

Percebe-se que a área de atuação com maior recorrência é a *Ciência da Computação* e apenas uma parcela reduzida equivale a área *Matemática*, o que se analisado isoladamente pode causar estranheza, pois seria de se esperar que no programa de Matemática a área *Matemática* tivesse uma maior porcentagem.

\pagebreak
#### Distribuição de subáreas de atuação mais frequentes dos pesquisadores

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

Através da análise acima, pode-se ver que a área com maior número de docentes atuantes é em *Geometria e Topologia*, seguida por *Análise*, *Matemática Aplicada* e *Teoria da Computação*.

\pagebreak
#### Distribuição de especialidades mais frequentes dos pesquisadores
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
Aqui vê-se as 10 especialidades mais recorrentes e percebe-se uma boa distribuição da quantidade entre elas.

### Publicações

#### Quantidade de Publicações por tipo
```{r echo=FALSE}
for (i in 1:length(mat_publication)){
  print(names(mat_publication[i]))
  print(mat_publication[[i]] %>% 
          sapply(function(x)
            length(x$ano)) %>% sum())
}
```
Percebe-se que o tipo mais recorrente de publicações é do tipo Periódico.

#### Quantidade de publicações por tipo e por ano 
```{r echo=FALSE}
publication_tipo_mat <- mat_publication %>% 
  sapply(function(x) 
    sapply(x, function(x)
      length(x$autores)))
publication_tipo_mat
```

Destaca-se que nos anos de 2016 e 2017 houveram mais publicações de Periódicos e em 2018 houve uma queda, mas no ano de 2018 houveram mais publicações do tipo Artigo Aceito e Demais Tipos de Produção Bibliografica. Pode-se imaginar que o programa focou mais nesses outros tipos de publicação no ano de 2018 em relação a Periódicos.

#### Participação em eventos por país

```{r echo=FALSE}
mat_publication$EVENTO %>% 
  sapply(function(x)
    (x$pais_do_evento)) %>% 
  unlist() %>% table() %>% 
  sort(decreasing = TRUE)
```

É interessante que o país tenha participado de mais eventos na França do que Chile que é um país mais próximo do Brasil. 

\pagebreak
#### Publicações por ano

```{r echo=FALSE, out.width='100%'}
mat_public.periodico.df %>% 
  ggplot(aes(x = ano)) + geom_bar(aes(fill=ano)) + 
  geom_text(stat = "count", aes(label=formatC(..count.., big.mark=",")),vjust=-0.1) + 
  theme_bw()+labs(x="Ano",y="Quantidade de Periódicos") +
  scale_y_continuous(labels = comma)
```

\pagebreak
#### Publicações de livros fora do Brasil
```{r echo=FALSE}
mat_public.livros.df %>%
  group_by(pais_de_publicacao) %>%
  summarise(Quantidade = n()) %>%
  filter(pais_de_publicacao != "Brasil") %>% 
  ggplot(aes(x = reorder(pais_de_publicacao, (Quantidade)), y = Quantidade)) +
  geom_col(fill = "coral")+ 
  geom_text(aes(label=comma(Quantidade)),
            hjust=-0.2,vjust=0.3,size=3.5) + coord_flip() +
  labs(title = "Publicação de Livros em Países Estrangeiros", 
       x = "Países", 
       y = "Quantidade de Livros")+
  theme_bw()

```

\pagebreak
#### Eventos por país

```{r echo=FALSE, out.width='100%'}
mat_public.eventos.df %>%
  group_by(pais_do_evento) %>%
  summarise(Quantidade = n()) %>%
  filter(pais_do_evento != "Brasil") %>% 
  ggplot(aes(x = reorder(pais_do_evento, (Quantidade)), y = Quantidade)) +
  geom_col(fill = "coral3")+ 
  geom_text(aes(label=comma(Quantidade)),
            hjust=-0.2,vjust=0.3,size=2.5)+ coord_flip() +
  labs(title = "Eventos", x = "Países", y = "Quantidade de Eventos") + theme_bw()
```

# Modelos de Análise

## Análise de Redes
Uma rede (ou um grafo de redes) representa uma conexão entre indivíduos. Na representação por redes, a presença de uma aresta entre um ou mais nós representa a existência de uma relação entre os indivíduos conectados, formando assim a rede. Há vários tipos de padrões e relacionamentos que podem ser modelos e representados como uma rede dessa maneira.

A partir dos arquivos graph.json podemos criar modelos de relacionamento entre os pesquisadores de diversos departamentos. Para tal será utilizada a biblioteca **igraph**, que permite a visualização gráfica desses relacionamentos na forma de uma estrutura de dados conhecida como *grafo*.

Começamos nossa análise carregando a biblioteca **igraph**:
  
  ```{r error=FALSE, message=FALSE}
library(igraph)
```

A análise de redes detalhada será feita para cada um dos programas de pós-graduação a seguir:
  
  ### Ciência da Computação
  Começamos carregando o arquivo json para a memória e criando variáveis para representar os nós e os relacionamentos entre os diversos autores do programa de Pós-Graduação em Ciência da Computação.
```{r}
cic_graph <- jsonlite::fromJSON("cic_graph.json")
cic_nodes <- cic_graph$nodes
cic_nodes$properties <- NULL
cic_relations <- cic_graph$links
```

Podemos observar as primeiras linhas dos nós (ou seja, os pesquisadores em si):
  
  ```{r echo=FALSE}
head(cic_nodes)
```

O mesmo pode ser feita para ser observar os relacionamentos (as arestas no grafo):
  
  ```{r echo=FALSE}
head(cic_relations)
```

A biblioteca **igraph** facilita bastante o processo de geração das redes a partir dos dados fornecidos.

```{r echo=FALSE}
graph_cic <- graph_from_data_frame(cic_relations, directed=TRUE, vertices=cic_nodes)
plot(graph_cic)
```

Pode-se observar facilmente que há dois *clusters* distintos de pesquisadores que já trabalharam em conjunto, além de três outros pesquisadores que trabalham apenas sozinhos.

### Computação
Fazendo-se a análise anterior para a Pós-Graduação em Computação, obtemos os seguintes resultados:
  
  ```{r warning = FALSE, echo=FALSE}
comp_graph <- jsonlite::fromJSON("comp_graph.json")
comp_nodes <- comp_graph$nodes
comp_nodes$properties <- NULL
comp_relations <- comp_graph$links
```

Observando-se os primeiros dados dos conjuntos de nós e das arestas:
  
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

Na análise para a Pós-Graduação em Computação fica bem claro que há um número menor de pesquisadores, e diferentemente da Pós-Graduação em Ciência da Computação, aqui fica bem claro que não há uma separação tão grandes entre diferentes *clusters* de pesquisa.

### Matemática
Por último, a análise de redes será feita para a Pós-Graduação em Matemática.

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
