#Iniciando
setwd("C:/Users/SouthSystem/Desktop/DIA 3")
rm(list=ls())
load("saved_dt.RData")

#Bibliotecas
library(rpart)
library(caret)

#Treinamento e teste
smpsize <- floor(0.75*nrow(data))
set.seed(76)
seq <- seq_len(nrow(data))
train_ind <- sample(seq, size = smpsize)
train <- data[train_ind,]
test <- data[-train_ind,]

#Modelo
rpartmodel<- rpart(saiu ~ ., data = train, method = "class")
rpart.plot::rpart.plot(rpartmodel, sub = "Árvore de decisão", min.inter.height = 10)

#Avaliando o modelo
predictions <- predict(rpartmodel, test, type = "class")
hr_model_tree <- cbind(test, predictions)
confusionMatrix <- confusionMatrix(hr_model_tree$predictions,factor(hr_model_tree$saiu))
confusionMatrix

#Segundo modelo
rpartmodel2 <- rpart(saiu ~ ., data = train, method = "class", 
                     control = rpart.control(minsplit=2, cp=0))
rpart.plot::rpart.plot(rpartmodel2, sub = "Árvore de decisão")

#Avalaindo o modelo 2
predictions2 <- predict(rpartmodel2, test, type = "class")
hr_model_tree2 <- cbind(test, predictions2)
confusionMatrix2 <- confusionMatrix(hr_model_tree2$predictions2, factor(hr_model_tree2$saiu))
confusionMatrix2

#Salvando o modelo
save(rpartmodel, file = "modelo.RData")

#Aplicando o modelo
load("modelo.RData")
data_mod <- read.csv("Pred.csv")
previsoes <- predict(rpartmodel, data_mod, type = "class")
data_mod <- cbind(data_mod,previsoes)
write.csv(data_mod, "Previsões.csv")
