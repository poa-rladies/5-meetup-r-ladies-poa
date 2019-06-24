#Primeiros passos
setwd("C:/Users/SouthSystem/Desktop/DIA+1/DIA 1")
getwd()
rm(list=ls())

#Criando um vetor
altura <- c(1.82,1.67,1.75)
altura

#Criando uma matriz
matriz <- data.frame(c(1.82,1.67,1.75),c(28,32,43))
matriz

#Mudando o nome das colunas
colnames(matriz) <- c("Altura","Idade")
matriz

#Mudando o nome das linhas
rownames(matriz) <- c("Pedro","Ana","Camila")
matriz

#Retornando um valor
matriz[2,1]
matriz$Idade[1]

#Input automático de dados
data <- read.csv("HR_comma_sep_PT.csv")

#Análise dos dados
head(data)
str(data)
summary(data)

#Ajustando o dados
is.na(data$satisfacao)
head(is.na(data$satisfacao))
which(is.na(data$satisfacao))
data$satisfacao[2] <- 0.8
head(data)
colnames(data) == "comercial"
which(colnames(data) == "comercial")
colnames(data)[9] <- "area"
str(data)

#Salvando os dados
save(data, file = "saved_dt.RData")
