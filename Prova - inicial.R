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
(real <- as.vector(tail(estados, n=10)))
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

a <- simulatebrown(n=248, h=0.1)

plot(a$tempos, a$mb, type="l")

# processo de poisson

incremento=1/248
tempos=seq(0,1,by=incremento)
n=length(tempos)
taxa=1

N=c(0,cumsum(rpois(n-1, lambda=taxa*incremento)))
length(N)

plot(tempos, N, type="l")

# > b ####

pdisc <- function(tempo, historico, taxa, processo){
  soma = sum(historico * (1/248)) 
  xtk = historico[length(historico)] - taxa * soma + processo
  return(xtk)
}

pdisc(tempo = seq(0,1,.002), historico = BB, taxa = 0.5, processo = rpois(248,1))

# > c ####

# Browniano
theoretical_process_brownian <- function(theta, observed_data) {
  n <- length(observed_data)
  time_points <- seq(0, 1, length.out = n)
  result <- numeric(n)
  for (k in 2:n) {
    result[k] <- result[k - 1] - theta * sum(observed_data[1:(k - 1)] * diff(time_points[1:(k - 1)])) + rnorm(1)
  }
  return(result)
}

# Função para calcular a soma dos quadrados dos resíduos para o Movimento Browniano
sum_of_squares_brownian <- function(theta, observed_data) {
  model_data <- theoretical_process_brownian(theta, observed_data)
  residuals <- observed_data - model_data
  return(sum(residuals^2))
}

# Estimação do parâmetro theta para o Movimento Browniano
set.seed(1)
theta_hat_MB <- optimize(f = sum_of_squares_brownian, interval = c(0, 5), observed_data = as.numeric(BB), tol = .01)$minimum
theta_hat_MB

# Poisson
# theoretical_process_poisson <- function(lambda, observed_data) {
#   n <- length(observed_data)
#   result <- numeric(n)
#   for (k in 1:n) {
#     result[k] <- exp(-lambda) * lambda * k + rpois(1, lambda = lambda)
#   }
#   
#   return(result)
# }

# Função para calcular a soma dos quadrados dos resíduos para o Processo de Poisson
sum_of_squares_poisson <- function(lambda, observed_data) {
  model_data <- theoretical_process_poisson(lambda, observed_data)
  residuals <- observed_data - model_data
  return(sum(residuals^2))
}

# Estimação do parâmetro lambda para o Processo de Poisson
lambda_hat_PP <- optimize(f = sum_of_squares_poisson, interval = c(0, 1), observed_data = as.numeric(BB), tol = .01)$minimum
lambda_hat_PP

# > d ####


