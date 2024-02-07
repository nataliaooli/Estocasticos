###### Prova 2 
setwd("C:/Users/natyo/Documents/10 semestre/Processos Estocásticos - VERÃO 2024.0/Prova/Estocasticos")

# Pacotes utilizados
pacman::p_load(tidyverse, MASS, quantmod, ggplot2, ggpubr, markovchain)

# 1 ####

dados <- read.delim("dados.txt", sep =";")[,-c(5,6,7)]
dados$Precipitacao <- as.numeric(dados$Precipitacao)
dados <- dados %>% 
  na.omit() %>% 
  mutate(Tempo = seq(1, 18480, 1))
 

# > a ####

precipitacao <- round(as.vector(dados$Precipitacao[!is.na(dados$Precipitacao)]))

par(mfrow = c(1,2))
plot(dados$Precipitacao, type = "l")
plot(precipitacao, type = "l")

ggplot(dados, aes(x= Tempo, y = Precipitacao, group = 1))+
  geom_line()+
  labs(title = "Precipitação ao longo dos dias (de 01/01/1964 a 31/12/2014)",
       y = "Precipitação",
       x = "Tempo")+
  scale_y_continuous(limits = c(0,131), breaks = seq(0,131,131/5))+
  scale_x_continuous(limits = c(0,18480), breaks = seq(0,18480,2000))+
  theme_light()+
  theme(axis.title.y=element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.text = element_text(colour = "black", size=10),
        title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank())

# separando em classes a variável Precipitação e cada classe será um estado
elementos <- cut(dados$Precipitacao, breaks = 5, labels  = c("1","2","3","4","5"), levels = labels)
# ele me dá as seguintes classes: 
# estado 1 = (-0.131,26.2]
# estado 2 = (26.2, 52.4]
# estado 3 = (52.4,78.6]
# estado 4 = (78.6,105]
# estado 5 = (105,131]

ggplot(data.frame(x = c(1:18480), y = elementos), aes(x=x, y=y)) +
  geom_point()+
  labs(title = "Cadeia de Markov - Precipitação",
       y = "Estados",
       x = "Tempo")+
  scale_x_continuous(limits = c(0,18480), breaks = seq(0,18480,2000))+
  theme_bw()+
  theme(axis.title.y=element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.text = element_text(colour = "black", size=10),
        title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank())


# > b ####

# com a função cut definindo automaticamente as classes, a probabilidade  de transição
# do estado 1 ficava muito grande, então pensando já no item c da questão 1,
# decidimos testar classes que englobassem classes diferentes das geradas inicialmente.
# De forma que pudéssemos ter os valores preditos mais próximos dos valores reais.


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
(pred <- predict(mc,newdata = c("1","1"),n.ahead = 10))

table(real, pred)

# primeiro teste para classes diferentes
elementos <- cut(dados$Precipitacao, breaks = c(0,4,26,56,86,131), labels = c("1","2","3","4","5"), 
                 levels = labels, include.lowest = T)
cm2 <- ggplot(data.frame(x = c(1:18480), y = elementos), aes(x=x, y=y)) +
  geom_point()+
  labs(y = "Estados",
       x = "")+
  scale_x_continuous(limits = c(0,18480), breaks = seq(0,18480,2000))+
  theme_bw()+
  theme(axis.title.y=element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.text = element_blank(),
        panel.grid.major.x = element_blank())

# segundo teste para classes diferentes
elementos <- cut(dados$Precipitacao, breaks = c(0,4,24,54,80,131), 
                 labels = c("1","2","3","4","5"), levels = labels,
                 include.lowest = T)
cm3 <- ggplot(data.frame(x = c(1:18480), y = elementos), aes(x=x, y=y)) +
  geom_point()+
  labs(y = "Estados",
       x = "")+
  scale_x_continuous(limits = c(0,18480), breaks = seq(0,18480,2000))+
  theme_bw()+
  theme(axis.title.y=element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.text = element_blank(),
        panel.grid.major.x = element_blank())

# quarto e último teste para classes diferentes
elementos <- cut(dados$Precipitacao, breaks = c(0,3,23,43,73,131), 
                 labels = c("1","2","3","4","5"), levels = labels,
                 include.lowest = T)

cm4 <- ggplot(data.frame(x = c(1:18480), y = elementos), aes(x=x, y=y)) +
  geom_point()+
  labs(y = "Estados",
       x = "Tempo")+
  scale_x_continuous(limits = c(0,18480), breaks = seq(0,18480,2000))+
  theme_bw()+
  theme(axis.title.y=element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.text = element_text(colour = "black", size=10),
        panel.grid.major.x = element_blank())

ggarrange(cm2, cm3, cm4, nol = 1, nrow =3)

# repetir os vetores "real" e "pred" para cada vez que rodar um novo vetor de elementos

# 2 ####

BBAS3 <- as.data.frame(quantmod::getSymbols("BBAS3.SA", src = "yahoo", auto.assign = FALSE,
                                            from = '2023-01-01', 
                                            to = '2023-12-31', return.class = 'xts')) %>% 
  mutate(t = seq(0,1, length.out = 248))

BB=BBAS3$BBAS3.SA.Close %>% as.vector()

#plot(BBAS3$t, BB, type="l", xlim = c(0,1))

ggplot(BBAS3, aes(x = t, y = BBAS3.SA.Close))+
  geom_line()+
  labs(title = "Preços de Fechamento dos valores das ações do BBAS3 no ano de 2023",
       y = "Preços",
       x = "Tempo")+
  scale_x_continuous(limits = c(0,1))+
  theme_bw()+
  theme(axis.title.y=element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.text = element_text(colour = "black", size=10),
        title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank())

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

pdisc <- function(h, historico, taxa, processo){
  soma = sum(historico * h) 
  xtk = historico[length(historico)] - taxa * soma + processo
  return(xtk)
}

plot(pdisc(h=1/248, historico = BB, taxa = 0.5, processo = rnorm(248)), type="l")

# > c ####

# Browniano e poisson, tem que ser definido no parâmetro tipo qual processo será usado
# Caso seja utilizado o processo de Poisson, é necessário declarar o lambda

processo_simulado <- function(theta, dado_observado, tipo = "Browniano", lambda=1) {
  n <- length(dado_observado)
  h <- 1/n
  tempo <- seq(0, 1, length.out = n)
  result <- numeric(n)
  if(tipo == "Browniano"){
    ruido = rnorm(248)
  }
  if(tipo == "Poisson"){
    ruido = rpois(248, lambda = lambda*(n*h))
  }
  for (k in 2:n) {
    result[k] <- result[k - 1] - theta * sum(dado_observado[1:(k - 1)] * h) + ruido[k]
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
theta_chapeu_MB <- optimize(f = soma_de_quadrados, interval = c(0, 1), 
                         dado_observado = as.numeric(BB),
                         tol = .9)$minimum

# Estimação do parâmetro theta para o Processo Poisson
set.seed(1)
theta_chapeu_PP <- optimize(f = soma_de_quadrados, interval = c(0, 1), 
                            dado_observado = as.numeric(BB), 
                            tipo = "Poisson", lambda=1,
                            tol = .9)$minimum

theta_chapeu_MB
theta_chapeu_PP

# Poisson - ERRADO !!!!!!!!!!!!
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

set.seed(1)
x_simulado_mb <- pdisc(h=1/248, historico = BB, taxa = theta_chapeu_MB, processo = rnorm(248))
residuos_mb <- BB - x_simulado_mb
qqnorm(residuos_mb)
qqline(residuos_mb)
shapiro.test(residuos_mb)

set.seed(1)
x_simulado_pp <- pdisc(h=1/248, historico = BB, taxa = theta_chapeu_PP, processo = rpois(248,1))
residuos_pp <- BB - x_simulado_pp
qqnorm(residuos_pp)
qqline(residuos_pp)
shapiro.test(residuos_pp)

# > e ####

plot(x_simulado_mb, type = "l")
plot(x_simulado_pp, type = "l")

plot(BB, type = "l")

processos <- data.frame(indice = seq(0, 1, length.out= (248)),
                        acoes = BB,
                        MB = x_simulado_mb,
                        PP = x_simulado_pp)

processos <- processos %>% pivot_longer(!indice, names_to = "Modelo", values_to = "Processos")
ppoisson <- processos %>% filter(Modelo != "MB")
pbrown <- processos %>% filter(Modelo != "PP")

(g1 <- ggplot(data = pbrown, aes(x = indice, y= Processos, color = Modelo, group = Modelo)) +
    geom_line(size = 1)+
    scale_color_manual(values = c("acoes" = "red", "MB" = "#4682B4"), 
                     labels = c("acoes" = "Observado", "MB" = "Movimento Browniano"))+
    labs(title = "Real X Browniano",
         y = "Valores dos modelos",
         x = "Tempo")+
    ylim(30, 60)+
    theme_light()+
    theme(axis.title.y=element_text(size=12),
          axis.title.x = element_text(size=12),
          axis.text = element_text(colour = "black", size=10),
          title = element_text(size = 14),
          plot.title = element_text(hjust = 0.5),
          panel.border = element_blank(),
          legend.position = "bottom",
          legend.background = element_rect(colour = "black"),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10))
)

(g2 <- ggplot(data = ppoisson, aes(x = indice, y= Processos, color = Modelo, group = Modelo)) +
    geom_line(size = 1)+
    scale_color_manual(values = c("acoes" = "red", "PP" = "#2E8B57"), 
                       labels = c("acoes" = "Observado", "PP" = "Processo de Poisson"))+
    labs(title = "Real X Poisson",
         y = "",
         x = "Tempo")+
    ylim(30, 60)+
    theme_light()+
    theme(axis.title.y=element_text(size=12),
          axis.title.x = element_text(size=12),
          axis.text = element_text(colour = "black", size=10),
          title = element_text(size = 14),
          plot.title = element_text(hjust = 0.5),
          panel.border = element_blank(),
          legend.position = "bottom",
          legend.background = element_rect(colour = "black"),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10))
)

ggarrange(g1, g2, ncol = 2, nrow= 1)
# # teste que não deu certo
# pdisc <- function(h, historico, taxa, processo){
#   xtk <- numeric(length(historico))
#   xtk[1] <- historico[1]
#   for (k in 2:(n-1)) {
#     xtk[k] <- xtk[k - 1] - taxa * h * sum(xtk[1:(k - 1)]) + processo[k]
#   }
#   return(xtk)
# }
# 
# pdisc <- function(h, historico, taxa, processo){
#   for (k in 2:n){
#   soma = sum(historico * h) 
#   xtk = historico[length(historico)] - taxa * soma + processo
#   return(xtk)
#   }
# }
# 
# # atual 
# pdisc <- function(h, historico, taxa, processo){
#   soma = sum(historico * h) 
#   xtk = historico[length(historico)] - taxa * soma + processo
#   return(xtk)
# }

# > f #### 
#teórica

# > g ####

BB24 <- as.data.frame(quantmod::getSymbols("BBAS3.SA", src = "yahoo", auto.assign = FALSE,
                                            from = '2024-01-01', 
                                            to = '2024-01-15', return.class = 'xts'))
fechamento <- BB24$BBAS3.SA.Close %>% as.vector()
