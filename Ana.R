# Pacotes utilizados
pacman::p_load(tidyverse, markovchain)

# 1 

dados <- read.delim("dados.txt", sep =";")[,-7]
#dados$Precipitacao <- round(dados$Precipitacao)

dados <- dados %>%
  select(Estacao, Data, Hora, Precipitacao) %>%
  na.omit()


# Extrair a variável de precipitação
precipitacao <- dados$Precipitacao

# (a) Criar um processo com espaço de estados contendo 5 elementos

# Criando os estados a partir da variável "Precipitacao"
estados <- cut(dados$Precipitacao, breaks = 5, labels = c("Estado 1", "Estado 2", "Estado 3", "Estado 4", "Estado 5"))

# (b) Estimar as probabilidades de transição
# Criar a cadeia de Markov

estados <- head(estados, n = 18470)
P1 <- createSequenceMatrix(estados)
P1

Fit = markovchainFit(data = estados,confidencelevel = 0.95)
Fit

mc = Fit$estimate
summary(mc)

plot(mc)
steadyStates(mc)


#c)
tail(estados, n=10)
predict(mc,newdata = "Estado 1",n.ahead = 3)
