#Primeiros passos
setwd("C:/Users/SouthSystem/Desktop/DIA 2")
rm(list=ls())

#Dados
load("saved_dt.RData")
str(data)

#Correla��o
library(corrplot)
Mcor <- cor(data[,1:8])
corrplot(Mcor, method = "square", type = "lower", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", tl.cex = 0.7, number.cex = 0.7)

#Atribuindo nomes
data$acidente_trabalho <- factor(data$acidente_trabalho, levels = c(0,1),
                                 labels = c("Não teve acidente","Teve acidente"))
data$saiu <- factor(data$saiu, levels = c(0,1), labels = c("Ativo","Saiu"))
data$promocao_ultimos_5_anos <- factor(data$promocao_ultimos_5_anos, levels = c(0,1),
                                       labels = c("Não teve promoção","Teve promoção"))

#ggplot
library(ggplot2)
ggplot(data, aes(satisfacao))
ggplot(data, aes(satisfacao)) + geom_histogram(bins = 15)
ggplot(data, aes(satisfacao)) + geom_histogram(bins = 15) + theme_light()
ggplot(data, aes(satisfacao)) + geom_histogram(aes(fill = saiu), bins = 15) + theme_light()
ggplot(data, aes(satisfacao)) + theme_light() + labs(title = "Histograma sobreposto") +
  geom_histogram(aes(fill = saiu), bins = 15, position = "identity", alpha = 0.5)

#Densidade de probabilidade
ggplot(data, aes(satisfacao)) + labs(title = "Densidade de probabilidade") +
  theme_light() + geom_density(aes(fill = saiu, color = saiu), position = "identity",
                               alpha = 0.5)

#Criando uma fun��o customizada para an�lise univariada
analiseUnivariada <- function(dataset, variavel, split){
  library(ggplot2)
  library(cowplot)
  g1 <- ggplot(dataset, aes_string(variavel)) +
    geom_histogram(aes_string(fill=split, colour=split), position = "stack", bins=10,
                   alpha=.5, show.legend = FALSE) +
    theme_light() + labs(title = "Histograma empilhado")
  g2 <- ggplot(dataset, aes_string(variavel)) +
    geom_histogram(aes_string(fill=split, colour=split), position = "identity", bins=10,
                   alpha=.5, show.legend = FALSE) +
    theme_light() + labs(title = "Histograma sobreposto")
  g3 <- ggplot(dataset, aes_string(variavel)) +
    geom_density(aes_string(colour=split), position = "identity", alpha=.5) +
    theme_light() + labs(title = "Densidade de probabilidade")
  plot_grid(g1,g2,g3, nrow=1)
}

#Analisando as variáveis contínuas
analiseUnivariada(data, "satisfacao", "saiu")
analiseUnivariada(data, "ultima_avaliacao", "saiu")
analiseUnivariada(data, "volume_projetos", "saiu")
analiseUnivariada(data, "media_horas_mensais", "saiu")
analiseUnivariada(data, "tempo_cia", "saiu")

#Criando um gráfico de barras
ggplot(data, aes(salario)) + theme_light() + labs(title = "Gr?fico de barras") +
  geom_bar(aes(fill = saiu, x = salario, y = (..count../sum(..count..))*100)) +
  labs(y = "percentual (%)") +
  geom_text(stat = "count", aes(x = salario, y=..count../sum(..count..)*80,
                                label = round(..count../sum(..count..)*100, 1),
                                group = saiu), position = "stack", vjust = 1)

#Criando uma função customizada para gráfico de barras
graficoBarras <- function(dataset, variavel, split) {
  library(ggplot2)
  library(cowplot)
  g1 <- ggplot(dataset) +
    geom_bar(aes_string(x=variavel, y="(..count../sum(..count..))*100", fill=split)) +
    geom_text(stat="count", aes_string(x=variavel,
                                       y="..count../sum(..count..)*80",
                                       label = "round(..count../sum(..count..)*100, 1)",
                                       group=split),
              position = "stack",
              vjust = 1) +
    labs(y = "percent (%)") +
    theme_light() +
    theme(axis.text.x=element_text(angle = 90, hjust = 1))
  g2 <- ggplot(dataset) +
    geom_bar(aes_string(x=variavel, y="..count..", fill=split), position = "dodge") +
    labs(y = "Qtde") +
    theme_light() +
    theme(axis.text.x=element_text(angle = 90, hjust = 1))
  plot_grid(g1,g2, nrow=1)
}

#Analisando as variáveis categóricas
graficoBarras(data, "promocao_ultimos_5_anos", "saiu")
graficoBarras(data, "area", "saiu")
graficoBarras(data, "salario", "saiu")
graficoBarras(data, "acidente_trabalho", "saiu")
graficoBarras(data, "tempo_cia", "saiu")
graficoBarras(data, "volume_projetos", "promocao_ultimos_5_anos")

#Salvando os dados
save(data, file = "saved_dt1.RData")