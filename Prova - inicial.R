###### Prova 2 
setwd("C:/Users/natyo/Documents/10 semestre/Processos Estocásticos - VERÃO 2024.0/Prova")

# Pacotes utilizados
pacman::p_load(tidyverse)

# 1 ####

dados <- read.delim("dados.txt", sep =";")[,-7]
dados$Precipitacao <- round(dados$Precipitacao)

# > a ####

precipitacao <- round(as.vector(dados$Precipitacao[!is.na(dados$Precipitacao)]))

par(mfrow = c(1,2))
plot(dados$Precipitacao, type = "l")
plot(precipitacao, type = "l")

(x <- table(precipitacao))

set.seed(21)
(elementos <- sample(sort(unique(precipitacao)), 5)) # elementos contando todos os possíveis resultados
(elementos2 <- sample(sort(unique(precipitacao))[1:50], 5)) # elementos contando os 50 resultados com mais ocorrência

proc1 <- dados[dados$Precipitacao %in% elementos,]
plot(1:length(proc1$Precipitacao),proc1$Precipitacao)

proc2 <- dados[dados$Precipitacao %in% elementos2,]
plot(1:length(proc2$Precipitacao), proc2$Precipitacao)

plot(1:length(proc1$Precipitacao),proc1$Precipitacao, type = "l")
plot(1:length(proc2$Precipitacao), proc2$Precipitacao, type = "l")

# > b ####

proc1$Data <- as.Date(proc1$Data, format = "%d/%m/%Y")
unique(proc1$Data)
proc1 <- proc1[c(1:695),]

transicoes <- matrix(0, nrow = 5, ncol = 5)


