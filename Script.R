
rm(list=ls(all=TRUE))



setwd("D:\\- PUC_MINAS_VIRTUAL\\dataset")

# Dados from
# http://cbsusrv04.tc.cornell.edu/users/panzea/download.aspx?filegroupid=9


### *************************
## Carrendo conjunto de dados e removendo dados perdidos (NA's)
## **************************

#install.packages( "readxl" )
library("readxl")

dados <- read_excel("Peiffer2014Genetics_blupPhenos20150325.xlsx" )
dados <- data.frame(dados)

head(dados)

dados <- dados[ ,c(1 , 2 , 3 , 10, 12, 13 )]
dados$PH <- as.numeric( dados$PH  )
dados$EH <- as.numeric( dados$EH  )
str(dados)

summary(dados)
dim(dados)

dados <- na.omit(dados)
summary(dados)
dim(dados)



## **************************************
## Investigacao conjunto de dados
## **************************************

# Variaveis 
# maize plant height (PHT) - "PH" em cm
# ear height (EHT) -  "EH"	em cm
# flowering time (days to anthesis, DTA) - "DTA" em dias

table(dados$Panel)
unique(dados$Panel)

table( dados$Family_Inbred_Name )
unique( dados$Family_Inbred_Name )
length( dados$Family_Inbred_Name )




## **************************************
## Estratificacao de populacoes
## **************************************

d1 <- dados
d1 <- d1[ d1$Panel == "AMES", ]

d2 <- dados
d2 <- d2[ d2$Panel == "NAM" , ]

d3 <- dados
d3 <- d3[ d3$Panel == "ASSO", ]



## **************************************
## Estudo das populaï¿½ï¿½es
## **************************************

summary(d1[,4:6]) # Pop. AMES
summary(d2[,4:6]) # Pop. NAM
summary(d3[,4:6]) # Pop. ASSO

#pdf("Boxplot.pdf" , 10 , 7)
boxplot( c( d1[,4:6] , d2[,4:6] ,d3[,4:6] )  ,
	col = c("orange" , "orange", "orange", "cyan","cyan","cyan", "yellow","yellow","yellow"))
#dev.off()



### ***************************
# 	INVESTIGACAO DA Analise de 
# 	diversidade genEtica dentro
# 	de cada populacao
### ***************************

## ***********************
# ConstruCAo das matrizes e dendrogramas
## ***********************

matriz_d1 <- dist(d1[,-c(1,2,3)], method = "euclidean")
dendrograma_d1 <- hclust(matriz_d1, method ="average")
#dendrograma_d1$labels
#dendrograma_d1$labels <- rep("ï¿½" , 2380)

#pdf("dendro-pop d1.pdf", 14, 7)
plot(dendrograma_d1 , hang = -1, cex=0.8, ylab="Distï¿½ncia Euclidiana",
	xlab = "Individuos", labels = FALSE  )
k = 1.25
PCorte =mean(dendrograma_d1$height)+k*sd(dendrograma_d1$height)
abline(h=PCorte,v=NULL,col=4,lty=2)
#dev.off()



matriz_d2 <- dist(d2[,-c(1,2,3)], method = "euclidean")
dendrograma_d2 <- hclust(matriz_d2, method ="average")
#dendrograma_d2$labels
#dendrograma_d2$labels <- rep("*" , 4892)

#pdf("dendro-pop d2.pdf", 14, 7)
plot(dendrograma_d2 , hang = -1, cex=0.8, ylab="Distï¿½ncia Euclidiana",
	xlab = "Individuos", labels = FALSE  )
k = 1.25
PCorte =mean(dendrograma_d2$height)+k*sd(dendrograma_d2$height)
abline(h=PCorte,v=NULL,col=4,lty=2)
#dev.off()



matriz_d3 <- dist(d3[,-c(1,2,3)], method = "euclidean")
dendrograma_d3 <- hclust(matriz_d3, method ="average")
#dendrograma_d3$labels
#dendrograma_d3$labels <- rep("*" , 282)

#pdf("dendro-pop d3.pdf", 14, 7)
plot(dendrograma_d3 , hang = -1, cex=0.8, ylab="Distancia Euclidiana",
	xlab = "Individuos", labels = FALSE  )
k = 1.25
PCorte =mean(dendrograma_d3$height)+k*sd(dendrograma_d3$height)
abline(h=PCorte,v=NULL,col=4,lty=2)
#dev.off()







## Todas as populações juntas

matriz_Comp <- dist(dados[,-c(1,2,3)], method = "euclidean")
dendrograma_Comp <- hclust(matriz_Comp, method ="average")


plot(dendrograma_Comp, hang = -1, cex=0.8, ylab="Distancia Euclidiana",
	xlab = "Individuos", labels = FALSE )


plot(dendrograma_Comp , hang = -1, cex=0.8, ylab="Distï¿½ncia Euclidiana",
	xlab = "Indivï¿½duos", labels = FALSE  )
k = 1.25









# **************************
## Implementação de mapa auto-organizável de Kohonem
# para investigação da diversidade
# **************************

library(SOMbrero)
library(kohonen)


# Status de Gerenciamento de memória e Garbage Colection
gc(reset = TRUE) 
gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)

head(dados)


Z <- scale( dados[, 4:6] , center = T, scale = T)

# determina uma semente aleatória para análise.
set.seed(100)


# Constrói um gride de dimensão 8 neurônios
# por 8 neurônios
mapa <- som(Z, grid = somgrid(8,8 , "hexagonal"), rlen = 100000)
print(mapa$grid)
	
print(summary(mapa))

#pdf("Mapa.pdf")
plot(mapa)
plot (mapa , type = "changes")
counts <- plot(mapa, type="counts", shape = "straight")
#dev.off()
names(mapa)



## *********************
## Investigação da alocação dos indivíduos nos neurônios
## **********************

# Apresenta em qual neurônio o respectivo indivíduo está alocado
print(mapa$unit.classif)

# investiga as posição dos indiduos nos neurônios
posicao <- print(mapa$unit.classif)


# mostra quantos indivíduos tem em cada neurônio
nb <- table(mapa$unit.classif)
nb		




### ***************************
# Clustering of Nodes
### ***************************


#distance matrix between the cells 
dc <- dist(mapa$codes[[1]])

#hac – the option “members” is crucial 
cah <- hclust(dc,method="ward.D2",members=nb)
plot(cah,hang=-1)
plot(cah,hang=-1, labels = F)



#pdf("Dendro - SOM.pdf", 10 , 7)
#hac – the option “members” is crucial 
cah <- hclust(dc,method="average",members=nb)
plot(cah,hang=-1 , ylab = "Distancia Euclidiana")
plot(cah,hang=-1, ylab = "Distancia Euclidiana", labels = F)
#dev.off()

 	

## Insvestigação de quantos indivíduos estao alocados em cada neuronio

## Pop AMES
table( mapa$unit.classif[1:2356] )

## Pop NAM
table( mapa$unit.classif[2357:7248] )

## Pop ASSO
table( mapa$unit.classif[7249:7529] )

## ***********************

# Optimal number of clusters for k-means
library("factoextra")
fviz_nbclust(dados[, 4:6],  kmeans, method = "gap_stat")


## https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/
# Clusters por kmeans , método silhouette

par(mfrow = c(1,2))

#pdf("número ótimo de clusters.pdf", 7, 5)
fviz_nbclust(dados[, 4:6],  kmeans, method = "silhouette")
fviz_nbclust(dados[, 4:6],  kmeans, method = "wss")
#dev.off()





# Visualizando Clusters no dendrograma 


#pdf("Dendro - SOM.pdf", 10 , 7)
#hac – the option “members” is crucial 

cah <- hclust(dc,method="average",members=nb)
plot(cah,hang=-1 , ylab = "Distancia Euclidiana")
rect.hclust(cah,k=5)

#dev.off()



# Visualizacao dos clusters no SOM

#pdf("mapa + Divisoes.pdf",14, 7) 
groupes = cutree(cah, k = 5)
par(mfrow = c(1,2))

plot(mapa)
add.cluster.boundaries(mapa,clustering=groupes)

plot(mapa, type = "mapping", 
	bgcol = c("steelblue1", "sienna1", "yellowgreen", "yellow", "white")[groupes])
add.cluster.boundaries(mapa,clustering=groupes)


par(mfrow = c(1,2))

plot(mapa,
	bgcol = c("steelblue1", "sienna1", "yellowgreen", "yellow", "white")[groupes])
add.cluster.boundaries(mapa,clustering=groupes)

plot(mapa, type = "mapping", 
	bgcol = c("steelblue1", "sienna1", "yellowgreen", "yellow", "white")[groupes])
add.cluster.boundaries(mapa,clustering=groupes)

#dev.off()



