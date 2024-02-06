###### Prova 2 
setwd("C:/Users/natyo/Documents/10 semestre/Processos Estocásticos - VERÃO 2024.0/Prova/Estocasticos")

# Pacotes utilizados
pacman::p_load(tidyverse, MASS, quantmod)

# 1 ####

dados <- read.delim("dados.txt", sep =";")[,-c(5,6,7)]
dados$Precipitacao <- round(dados$Precipitacao)
dados <- dados %>% na.omit()

# > a ####

precipitacao <- round(as.vector(dados$Precipitacao[!is.na(dados$Precipitacao)]))

par(mfrow = c(1,2))
plot(dados$Precipitacao, type = "l")
plot(precipitacao, type = "l")

elementos <- cut(dados$Precipitacao, breaks = 5, labels  = c("1","2","3","4","5"), levels = labels)
# ele me dá as seguintes classes: 
# estado 1 = (-0.131,26.2]
# estado 2 = (26.2, 52.4]
# estado 3 = (52.4,78.6]
# estado 4 = (78.6,105]
# estado 5 = (105,131]

plot(as.numeric(elementos))

# > b ####

# proc1$Data <- as.Date(proc1$Data, format = "%d/%m/%Y")
# unique(proc1$Data)
# proc1 <- proc1[c(1:695),]
# 
# transicoes <- matrix(0, nrow = 5, ncol = 5)


estados <- head(elementos, n = 18470)
P1 <- createSequenceMatrix(estados)
P1

Fit = markovchainFit(data = estados,confidencelevel = 0.95)
Fit

mc = Fit$estimate
summary(mc)

plot(mc)
steadyStates(mc)


# > c ####
real <- as.vector(tail(estados, n=10))
(pred <- predict(mc,newdata = c("1","1"),n.ahead = 10))

table(real, pred)

# 2 ####

BBAS3 <- quantmod::getSymbols("BBAS3.SA", src = "yahoo", auto.assign = FALSE,
                              from = '2023-01-01', 
                              to = '2023-12-31', return.class = 'xts')
BB=BBAS3$BBAS3.SA.Close %>% as.vector()

plot(BB, type="l")

###################################################

#processo de Poisson

#uniforme
n=100
t_max=1
T_de_chegada=sort(runif(n,min=0, max = t_max))

tempo=0.25
sum(T_de_chegada<tempo)
N_tempo=sum(T_de_chegada<tempo)


## processo
plot(T_de_chegada, 1:n, type="l")


#exp
taxa=1/10
(T_entre_chegadas=rexp(n,taxa))
(T_de_chegadas=cumsum(T_entre_chegadas))


## Poisson
incremento=1/100
tempos=seq(0,10,by=incremento)
n=length(tempos)
taxa=1

N=c(0,cumsum(rpois(n-1, lambda=taxa*incremento)))
length(N)

plot(tempos, N, type="l")



# > a ####

# movimento browniano
simulatebrown <-  function(n,h) {
  times = (0:n)*h
  incrementos = (rnorm(n, 0, sd= sqrt(h)))
  B= c(0, cumsum(incrementos))
  return(list(mb = B, tempos= times))
  
}

a <- simulatebrown(n=100, h=0.1)

plot(a$tempos, a$mb, type="l")

num <- 100
results <- lapply(1:num, 
                  function(i) simulatebrown(n=100, h=0.1))

plot(results[[1]]$tempos, results[[1]]$mb, type= "l", col= 1, xlab= "Tempo", ylab= "Valor acumulado", ylim = c(-10,10), 
     main= "Simulações do Movimento Browniano acumulado")

for (i in 1:num) {
  lines(results[[i]]$tempos, results[[i]]$mb, col=i)
}


# > b ####

pdisc <- function(tempo, historico, taxa, processo){
  soma <- 0
  for (i in 1:length(historico)) {
    soma = sum(historico[i] * (tempo[i] - tempo[i-1]))
  }
  xtk = historico[length(historico)] - taxa * soma + processo
  return(xtk)
}

pdisc(tempo=seq(0,1,0.1), historico=c(1,0.8,0.7,.6), taxa=0.5, processo=rnorm(1))
