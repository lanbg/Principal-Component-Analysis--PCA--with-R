# Script para rodar an?lise de componentes principais

# Instalacao e carregamento dos pacotes
lista.de.pacotes <- c("tidyverse","ggplot2")
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in% installed.packages()[,"Package"])]
if(length(novos.pacotes)) install.packages(novos.pacotes)
lapply(lista.de.pacotes, require, character.only = TRUE)

#Este conjunto de dados contém estatísticas, em prisões por 100.000 residentes 
#por agressão, assassinato e estupro em cada um dos 50 estados dos EUA em 1973.
#Também é fornecida a porcentagem da população que vive em áreas urbanas.

#load data
data("USArrests")

dados <- USArrests

#view first six rows of data
head(USArrests)

#calculate principal components / scale = TRUE faz padronização das variáveis colocando todas
#na mesma escala
results <- prcomp(USArrests, scale = TRUE)

#display principal components
#valor dos meus escores(autovetores) das variáveis em cada componente principal
results$rotation

#display the first six scores
head(results$x)
#scale = 0 para centralizar o "0" do PCA que as vezes não fica certo
biplot(results, scale = 0)

#display states with highest murder rates in original dataset
head(USArrests[order(-USArrests$Murder),])

#calculate total variance explained by each principal component
#Percentual de explicação dos PC
results$sdev^2 / sum(results$sdev^2)

#calculate total variance explained by each principal component
var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:4), var_explained) +
  geom_line() +
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)
