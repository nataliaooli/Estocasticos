# Pacotes utilizados
pacman::p_load(tidyverse, markovchain, quantmod)

# QUESTAO 1 

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

###################################################################################################

# QUESTAO 2

##IMPORTANDO AS BASES
BBAS3 <- quantmod::getSymbols("BBAS3.SA", src = "yahoo", auto.assign = FALSE,
                              from = '2023-01-01', 
                              to = '2023-12-31', return.class = 'xts')
BB=BBAS3$BBAS3.SA.Close %>% as.vector()

plot(BB, type="l")



## a)

# Movimento Browniano

# Simulando uma trajetória do movimento Browniano
set.seed(123)  # Define uma semente para reproduzibilidade
n_steps <- length(BBAS3) - 1
delta_t <- 1 / n_steps

# Geração de amostras da distribuição normal padrão
brownian_motion <- cumsum(sqrt(delta_t) * rnorm(n_steps))

# Adicionando o valor inicial para obter a trajetória completa
brownian_motion <- c(0, brownian_motion)

# Plotando a trajetória do movimento Browniano
plot(brownian_motion, type = "l", col = "blue", xlab = "Tempo", ylab = "X(t)", main = "Movimento Browniano")


# Processo de Poisson

#uniforme
rppoisson=function(n, t_max){}
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


#b) Naty fez

#c) 

# Função para calcular o valor do processo teórico para o Movimento Browniano
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
theta_hat_MB <- optimize(f = sum_of_squares_brownian, interval = c(0, 1), observed_data = as.numeric(BBAS3), tol = 1e-6)$minimum

# Imprime o resultado
cat("Estimativa de theta para o Movimento Browniano:", theta_hat_MB, "\n")

# Função para calcular o valor do processo teórico para o Processo de Poisson
theoretical_process_poisson <- function(lambda, observed_data) {
  n <- length(observed_data)
  result <- numeric(n)
  
  for (k in 1:n) {
    result[k] <- exp(-lambda) * lambda * k + rpois(1, lambda = lambda)
  }
  
  return(result)
}

# Função para calcular a soma dos quadrados dos resíduos para o Processo de Poisson
sum_of_squares_poisson <- function(lambda, observed_data) {
  model_data <- theoretical_process_poisson(lambda, observed_data)
  residuals <- observed_data - model_data
  return(sum(residuals^2))
}

# Estimação do parâmetro lambda para o Processo de Poisson
lambda_hat_PP <- optimize(f = sum_of_squares_poisson, interval = c(0, 5), observed_data = as.numeric(BBAS3), tol = 1e-6)$minimum

# Imprime o resultado
cat("Estimativa de lambda para o Processo de Poisson:", lambda_hat_PP, "\n")

#-----------------------------------