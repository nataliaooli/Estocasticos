###### Prova 2 
setwd("C:/Users/natyo/Documents/10 semestre/Processos Estocásticos - VERÃO 2024.0/Prova/Estocasticos")

# Pacotes utilizados
pacman::p_load(tidyverse, MASS, quantmod)

# 1 ####

dados <- read.delim("dados.txt", sep =";")[,-c(5,6,7)]
dados$Precipitacao <- as.numeric(dados$Precipitacao)
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

(elementos <- cut(dados$Precipitacao, breaks = c(0,4,26,56,86,131), labels = c("1","2","3","4","5"), levels = labels))
(elementos <- cut(dados$Precipitacao, breaks = c(0.0,4.0,24.0,54.0,80.0,131.0), 
                  labels = c("1","2","3","4","5"), levels = labels,
                  include.lowest = T))

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
Fit$estimate

mc = Fit$estimate
summary(mc)

plot(mc)
steadyStates(mc)


# > c ####
(real <- as.vector(tail(estados, n=10)))
(pred <- predict(mc,newdata = c("1","1"),n.ahead = 1000))

table(real, pred)

# 2 ####

BBAS3 <- as.data.frame(quantmod::getSymbols("BBAS3.SA", src = "yahoo", auto.assign = FALSE,
                                            from = '2023-01-01', 
                                            to = '2023-12-31', return.class = 'xts')) %>% 
  mutate(t = seq(1,248,1))

BB=BBAS3$BBAS3.SA.Close %>% as.vector()

plot(BBAS3$t, BB, type="l", xlim = c(1,248))

# > a ####

# movimento browniano
simulatebrown <-  function(n,h) {
  times = (0:n)*h
  incrementos = (rnorm(n, 0, sd= sqrt(h)))
  B= c(0, cumsum(incrementos))
  return(list(mb = B, tempos= times))
}

a <- simulatebrown(n=248, h=1/248)

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

pdisc(tempo = seq(0,1,1/248), historico = BB, taxa = 0.5, processo = rpois(248,1))

# > c ####

# Browniano e poisson, tem que ser definido no parâmetro tipo qual processo será usado
# Caso seja utilizado o processo de Poisson, é necessário declarar o lambda

processo_simulado <- function(theta, dado_observado, tipo = "Browniano", lambda=1) {
  n <- length(dado_observado)
  h <- 1/n
  tempo <- seq(0, 1, length.out = n)
  result <- numeric(n)
  if(tipo == "Browniano"){
    ruido = rnorm(1)
  }
  if(tipo == "Poisson"){
    ruido = rpois(1,lambda = lambda*(n*h))
  }
  for (k in 2:n) {
    result[k] <- result[k - 1] - theta * sum(dado_observado[1:(k - 1)] * h) + ruido
  }
  return(result)
}

# Função para calcular a soma dos quadrados dos resíduos
soma_de_quadrados <- function(theta, dado_observado, tipo = "Browniano", lambda=1) {
  modelo_simulado <- processo_simulado(theta, dado_observado, tipo, lambda=1)
  residuals <- dado_observado - modelo_simulado
  return(sum(residuals^2))
}

# Estimação do parâmetro theta para o Movimento Browniano
set.seed(1)
theta_chapeu_MB <- optimize(f = soma_de_quadrados, interval = c(0, 5), 
                         dado_observado = as.numeric(BB),
                         tol = .01)$minimum
theta_chapeu_MB

set.seed(1)
theta_chapeu_PP <- optimize(f = soma_de_quadrados, interval = c(0, 5), 
                            dado_observado = as.numeric(BB), 
                            tipo = "Poisson", lambda=1,
                            tol = .01)$minimum
theta_chapeu_PP

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
# 
# # Função para calcular a soma dos quadrados dos resíduos para o Processo de Poisson
# sum_of_squares_poisson <- function(lambda, observed_data) {
#   model_data <- theoretical_process_poisson(lambda, observed_data)
#   residuals <- observed_data - model_data
#   return(sum(residuals^2))
# }
# 
# # Estimação do parâmetro lambda para o Processo de Poisson
# lambda_hat_PP <- optimize(f = sum_of_squares_poisson, interval = c(0, 1), observed_data = as.numeric(BB), tol = .01)$minimum
# lambda_hat_PP

# > d ####


