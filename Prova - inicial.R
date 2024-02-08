###### Prova 2 
setwd("C:/Users/natyo/Documents/10 semestre/Processos Estocásticos - VERÃO 2024.0/Prova/Estocasticos")

# Pacotes utilizados
pacman::p_load(tidyverse, MASS, quantmod, ggplot2, ggpubr, markovchain)

# 1 ####

# leitura dos dados da questão 1
dados <- read.delim("dados.txt", sep =";")[,-c(5,6,7)]
dados$Precipitacao <- as.numeric(dados$Precipitacao)
dados <- dados %>% 
  na.omit() %>% 
  mutate(Tempo = seq(1, 18480, 1))
 

# > a ####

precipitacao <- round(as.vector(dados$Precipitacao[!is.na(dados$Precipitacao)]))

# par(mfrow = c(1,2))
# plot(dados$Precipitacao, type = "l")
# plot(precipitacao, type = "l")

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

estados <- head(elementos, n = 18470) #selecionando todos os dados, exceto os dos 10 últimos dias
P1 <- createSequenceMatrix(estados) # criando uma matriz de transições
P1 # utilizando o método de máxima verossimilhança

Fit = markovchainFit(data = estados,confidencelevel = 0.95) # Gerando uma cadeia de markov a partir dos dados
Fit$estimate # matriz das probabilidades de transições

mc = Fit$estimate
summary(mc) # classificando os estados

plot(mc) # grafo 


# > c ####
estados[c(18469,18470)] # vendo quais foram os estados dos últimos 2 dias
(real <- as.vector(tail(estados, n=10))) # estados observados dos 10 últimos dias
(pred <- predict(mc,newdata = c("1","1"),n.ahead = 10)) # estados preditos dos 10 últimos dias do banco

table(real, pred) # tentativa de matriz de confusão

# com a função cut definindo automaticamente as classes, a probabilidade  de transição
# do estado 1 ficava muito grande, decidimos testar classes que 
# englobassem intervalos diferentes das geradas inicialmente.
# De forma que pudéssemos ter os valores preditos mais próximos dos valores reais.

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

ggarrange(cm2, cm3, cm4, nol = 1, nrow =3) # analisando os gráficos lado a lado

# repetir os vetores "real" e "pred" para cada vez que rodar um novo vetor de elementos

# 2 ####

# leitura do banco de dados da questão 2
BBAS3 <- as.data.frame(quantmod::getSymbols("BBAS3.SA", src = "yahoo", auto.assign = FALSE,
                                            from = '2023-01-01', 
                                            to = '2023-12-31', return.class = 'xts')) %>% 
  mutate(t = seq(0,1, length.out = 248))

BB=BBAS3$BBAS3.SA.Close %>% as.vector() # vetor com os valores da variável de interesse

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

set.seed(1)
a <- simulatebrown(n=248, h=1/248)

ggplot(data.frame(x = a$tempos, y = a$mb), aes(x=x, y=y)) +
  geom_line()+
  labs(y = "Estados",
       x = "Tempo",
       title = "Movimento Browniano")+
  scale_x_continuous(limits = c(0,1))+
  theme_bw()+
  theme(axis.title.y=element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.text = element_text(colour = "black", size=10),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank())

# processo de poisson
incremento=1/248
tempos=seq(0,1,by=incremento)
n=length(tempos)
taxa=1

set.seed(24)
N=c(0,cumsum(rpois(n-1, lambda=taxa*incremento)))

ggplot(data.frame(x = tempos, y = N), aes(x=x, y=y)) +
  geom_line()+
  labs(y = "Estados",
       x = "Tempo", 
       title = "Processo de Poisson")+
  scale_x_continuous(limits = c(0,1))+
  theme_bw()+
  theme(axis.title.y=element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.text = element_text(colour = "black", size=10),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank())

# > b ####

pdisc <- function(h, historico, taxa, processo){
  xtk <- numeric(length(historico)) #gerando um vetor de 0 do tamanho da amostra
  xtk[1] <- historico[1] # recebe o primeiro valor do historico do processo
  for (k in 2:(n-1)) {
    xtk[k] <- xtk[k - 1] - taxa * h * sum(xtk[1:(k - 1)]) + processo[k] # equação dada no item
  } # usando o h, pois a diferença dos entre os tempo é a mesma para todas eles sendo assim a distancia entre elas será h = 1/n
  return(xtk)
}

head(pdisc(h= 1/248, historico = BB, taxa = 0.5, processo = rnorm(248)), 5) # testando se a função deu certo para o movimento browniano

head(pdisc(h = 1/248, historico = BB, taxa = 0.5, processo = rpois(248,1)),5) # testando se a função deu certo para o processo de poisson
# > c ####

# Browniano e poisson, tem que ser definido qual processo será utilizado no parâmetro "tipo" 
# Caso seja utilizado o processo de Poisson, é necessário declarar o lambda, se for diferente de 1

processo_simulado <- function(theta, dado_observado, tipo = "Browniano", lambda=1) {
  n <- length(dado_observado)
  h <- 1/n
  tempo <- seq(0, 1, length.out = n)
  result <- numeric(n)
  if(tipo == "Browniano"){
    ruido = rnorm(248)
  }
  if(tipo == "Poisson"){
    ruido = rpois(248,lambda = lambda*(n*h))
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
# utilizando a função optimize para achar o valor mínimo para theta, i.e., utilizamos
# o método dos mínimos quadrados para estimação dos thetas
set.seed(1)
theta_chapeu_MB <- optimize(f = soma_de_quadrados, 
                            interval = c(0, 1e6), 
                            dado_observado = as.numeric(BB),
                            tol = .01)$minimum 

# Estimação do parâmetro theta para o Processo Poisson
set.seed(1)
theta_chapeu_PP <- optimize(f = soma_de_quadrados, 
                            interval = c(0, 1e6), 
                            dado_observado = as.numeric(BB), 
                            tipo = "Poisson", lambda=1,
                            tol = .01)$minimum

theta_chapeu_MB # movimento browniano
theta_chapeu_PP # processo poisson

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
x_simulado_mb <- pdisc(h=1/248, historico = BB, taxa = theta_chapeu_MB, processo = rnorm(248)) # modelo ajustado - movimento browniano
residuos_mb <- BB - x_simulado_mb
hist(residuos_mb)
qqnorm(residuos_mb)
qqline(residuos_mb)
shapiro.test(residuos_mb) #teste de Shapiro-Wil para normalidade

set.seed(1)
x_simulado_pp <- pdisc(h=1/248, historico = BB, taxa = theta_chapeu_PP, processo = rpois(248,1)) # modelo ajustado - poisson
residuos_pp <- BB - x_simulado_pp
hist(residuos_pp)
qqnorm(residuos_pp)
qqline(residuos_pp)
shapiro.test(residuos_pp) 

# plot(x_simulado_mb, type = "l")
# plot(x_simulado_pp, type = "l")

# teste atual
# pdisc <- function(h, historico, taxa, processo){
#   xtk <- numeric(length(historico))
#   xtk[1] <- historico[1]
#   for (k in 2:(n-1)) {
#     xtk[k] <- xtk[k - 1] - taxa * h * sum(xtk[1:(k - 1)]) + processo[k]
#   }
#   return(xtk)
# }


# # estávamos usando esse, só que os valores estavam extremamente, 
# # muito provavelmente porque ele começa o processo de onde a série terminou
# # e ele não faz a soma dos valores que o processo está teno e sim da série
# 
# pdisc <- function(h, historico, taxa, processo){
#   soma = h * sum(historico)
#   xtk = historico[length(historico)] - taxa * soma + processo
#   return(xtk)
# }

# > e ####

processos <- data.frame(indice = seq(0, 1, length.out= (248)),
                        acoes = BB,
                        MB = x_simulado_mb,
                        PP = x_simulado_pp) # juntando os valores reais, gerados pela poisson e pelo browniano, com os tempos em um data.frame

processos <- processos %>% 
  pivot_longer(!indice, names_to = "Modelo", values_to = "Processos") # colocando os valores dos modelos todos em uma coluna, para facilitar a plotagem
ppoisson <- processos %>% 
  filter(Modelo != "MB")
pbrown <- processos %>% 
  filter(Modelo != "PP")

(g1 <- ggplot(data = pbrown, aes(x = indice, y= Processos, color = Modelo, group = Modelo)) +
    geom_line(size = 1)+
    scale_color_manual(values = c("acoes" = "red", "MB" = "#4682B4"), 
                     labels = c("acoes" = "Observado", "MB" = "Movimento Browniano"))+
    labs(title = "Real X Browniano",
         y = "Valores dos modelos",
         x = "Tempo")+
    #ylim(30, 60)+
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
    #ylim(30, 60)+
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

# > f #### 

# Calculando o erro quadrático médio (EQM) para cada modelo
eqm_mb <- mean(residuos_mb^2)
eqm_pp <- mean(residuos_pp^2)

eqm_mb
eqm_pp
eqm_mb < eqm_pp # vendo qual é menor

# Teorica -> respondida no markdown

# > g ####
#repetimos o que fizemos nos itens anteriores

BB24 <- as.data.frame(quantmod::getSymbols("BBAS3.SA", src = "yahoo", auto.assign = FALSE,
                                           from = '2024-01-01', 
                                           to = '2024-01-15', return.class = 'xts')) %>% 
  mutate(dias = c(2,3,4,5,8,9,10,11,12))

fechamento <- BB24$BBAS3.SA.Close %>% as.vector()

ggplot(BB24, aes(x= dias, y = BBAS3.SA.Close))+
  geom_line()+
  geom_point()+
  labs(title = "Preços de Fechamento dos valores das ações do BBAS3",
       y = "Preços",
       x = "Tempo")+
  scale_x_continuous(limits = c(2,12), breaks = seq(2,12,2))+
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

set.seed(1)
mb2024 <- (pdisc(h=1/9, historico = fechamento, 
                 taxa = theta_chapeu_MB, 
                 processo = rnorm(9)))[1:9]
residuos_2024 <- fechamento - mb2024
hist(residuos_2024)
qqnorm(residuos_2024)
qqline(residuos_2024)
shapiro.test(residuos_2024)

acoes24 <- data.frame(indice = c(2,3,4,5,8,9,10,11,12),
                      acoes = fechamento,
                      MB = mb2024)

acoes24 <- acoes24 %>% pivot_longer(!indice, names_to = "Modelo", values_to = "Processos")

ggplot(data = acoes24, aes(x = indice, y= Processos, color = Modelo, group = Modelo)) +
  geom_line(size = 1)+
  geom_point(size = 1.5)+
  scale_color_manual(values = c("acoes" = "red", "MB" = "#4682B4"), 
                     labels = c("acoes" = "Modelo observado", "MB" = "Modelo ajustado"))+
  labs(title = "Comparação entre \n Modelo observado e o Modelo ajustado",
       y = "Preços das ações",
       x = "Tempo")+
  scale_x_continuous(limits = c(2,12), breaks=seq(2,12,2))+
  scale_y_continuous(limits = c(53,57), breaks = c(53:57))+
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
