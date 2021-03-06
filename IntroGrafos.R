## Notebook Grafos Aula 01

### Pacotes
install.packages("dplyr")
install.packages("plyr")
install.packages("igraph")
install.packages("timeordered")

### Bibliotecas
library(dplyr)
library(plyr)
library(igraph)
library(timeordered)

### Consciência situacional
setwd('/Users/sidneibarbieri/Documents/Grafos/IntroGrafos/')
getwd()
g1 = read_graph("./G1.net", format = c("pajek"))
g1

g2 <- graph( edges = c(1,2, 
                       1,3, 
                       2,3, 
                       3,4, 
                       4,5), n=5, directed=F)
plot(g2)
plot(g2, vertex.size=40, vertex.color="orange", edge.color="darkblue", edge.width=2, vertex.label.font=5)


### Plotar o Grafo G1
plot(g1)
plot(g1, vertex.size=40, vertex.color="orange", edge.color="darkblue", edge.width=2, vertex.label.font=5)

names = V(g1)$id
names

plot(g1, vertex.label=names, 
     vertex.size=40, 
     vertex.color="yellow", 
     edge.color="darkblue", 
     edge.width=2, 
     vertex.label.font=5)

### Listar Vértices
V(g1)
V(g2)

###Nomes dos Vérices
V(g1)$name
vertex_attr(g1)

V(g2)$name
V(g2)$name <- c("1", "2", "3", "4", "5")
vertex_attr(g2)


### Listar Arestas
E(g1)

#### Peso das Arestas
E(g1)$weight

edge_attr(g2)
E(g2)$weight <- 1
edge_attr(g2)


### Matriz de Incidências
g1[]


### Conexões de um Vértice
#### Vértice 1
g1[1,]

#### Vértice 5
g1[5,]


### Distribuição de Graus
d1 = degree.distribution(g1, cumulative=FALSE)
len = length(d1)

png(filename="G1_graus.png", height=500, width=500, bg="white")
barplot(d1, main="Distribuição de Graus", xlab="Graus", ylab="PDF", names.arg=c(0:(len-1)))

dev.off()

### Proximidade dos Vértices
cl = closeness(g1)
cl


### Vértice com maior grau
cl_max= max(cl)
cl_max

vindex_max = which(closeness(g1) == cl_max)
vindex_max

vmax=V(g1)[vindex_max]$id
vmax


### Vértice com menor grau
cl_min= min(cl)
cl_min

vindex_min = which(closeness(g1) == cl_min)
vindex_min

vmin=V(g1)[vindex_min]$id
vmin


### Máxima distância mínima entre Vértices
b = betweenness(g1)
bmax=max(b)
bmax

bindex_max = which(betweenness(g1) == bmax)
bindex_max

bmax=V(g1)[bindex_max]$id
bmax

bmin= min(b)
bmin

bindex_min = which(betweenness(g1) == bmin)
bindex_min

bmin=V(g1)[bindex_min]$id
bmin


### Arestas
vcount(g1)
ecount(g1)
E(g1)
eb = edge.betweenness(g1)
eb


### Caminho mais curto
distances(g1)

coreness(g1, mode="all")

g2 <- simplify(g2, remove.multiple = F, remove.loops = T)


### Lista de Arestas
as_edgelist(g2, names=T)


### Matriz de Adjacências 
madj <- as_adjacency_matrix(g2, attr="weight")


### Matriz de Caminhos Mínimos
distances(g1)


### Grau dos Vértices
degree(g2)
grau <- degree(g2)
as.data.frame(grau)

V(g2)$grau <- 0
k1 <- sum(madj[,1])
k2 <- sum(madj[,2])
k3 <- sum(madj[,3])
k4 <- sum(madj[,4])
k5 <- sum(madj[,5])

V(g2)$grau <- c(k1, k2, k3, k4, k5)
V(g2)$grau <- as.numeric(V(g2)$grau)


### Vizinhos dos Vertices
n1 <- neighbors(g2, 1)
n2 <- neighbors(g2, 2)
n3 <- neighbors(g2, 3)
n4 <- neighbors(g2, 4)
n5 <- neighbors(g2, 5)

### Número de arestas entre os vizinhos
E1 <- length(E(induced_subgraph(g2, n1)))
E2 <- length(E(induced_subgraph(g2, n2)))
E3 <- length(E(induced_subgraph(g2, n3)))
E4 <- length(E(induced_subgraph(g2, n4)))
E5 <- length(E(induced_subgraph(g2, n5)))


### Coeficiente de aglomeração
V(g2)$coef <- c(0, 0, 0, 0, 0)
V(g2)$coef <- as.double(V(g2)$coef)

c1 = 2*E1/k1*(k1-1)
c2 = 2*E2/k2*(k2-1)
c3 = 2*E3/k3*(k3-1)
c4 = 2*E4/k4*(k4-1)
c5 = 2*E5/k5*(k5-1)

V(g2)$coef <- as.double(c(c1, c2, c3, c4, c5))

V(g2)$coef

### Mínimo caminho médio
mcm <- distances(g1)
mcm <- as.data.frame(mcm)

l1 <- sum(mcm[1,])/(5-1)
l2 <- sum(mcm[2,])/(5-1)
l3 <- sum(mcm[3,])/(5-1)
l4 <- sum(mcm[4,])/(5-1)
l5 <- sum(mcm[5,])/(5-1)

average.path.length(g2, directed = FALSE)

### Diâmetro
diameter(g2)

