#Primeiros passos
setwd("C:/Users/SouthSystem/Desktop/DIA+1/DIA 1")
rm(list=ls())

#Carregando os pacotes
library(corrplot)
library(psych)

#Lendo os dados
data <- read.csv("Aval_Lider.csv")
head(data)

#Corrigindo os dados
colnames(data)[1] <- "NOME"
summary(data)

#Histogramas
hist(data$GERAL, main = "Avaliação geral", xlab = "Nota", ylab = "Frequência")
multi.hist(data[,-1])

#Correlações
Mcor <- cor(data[,-1])
corrplot(Mcor, method = "square", type = "lower", tl.col = "black", tl.srt = 45,
         addCoef.col = "white", tl.cex = 0.7, number.cex = 0.7)

#Investigando
plot(x = data$TECNICO, y = data$GERAL, main = "Relação entre GERAL e TECNICO",
     xlab = "TECNICO", ylab = "GERAL")

#Regressão linear
fit <- lm(data$GERAL ~ data$TECNICO)
summary(fit)
plot(x = data$TECNICO, y = data$GERAL, main = "Relação entre GERAL e TECNICO",
     xlab = "TECNICO", ylab = "GERAL")
abline(fit, col = "blue")