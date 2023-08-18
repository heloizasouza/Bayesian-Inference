rm(list = ls())
library(gumbel) # pacote da distribuição gumbel
library(mvtnorm) # pacote para distribuição normal multivariada

# normal multivariada -----------------------------------------------------





sigma <- matrix(c(4,2,2,2,3,2,2,2,5), ncol=3)
x <- rmvnorm(n=500, mean=c(1,2,3), sigma=sigma)
colMeans(x)
var(x)
dS <- dmvnorm(x, sigma = sigma)
?dmvnorm

rmvnorm() # gera valores aleatórios da distribuição normal multivariada
dmvnorm() # gera a densidade da normal multivariada



# Metropolis-Hastings -----------------------------------------------------


# densidade a posteriori
posteriori <- function(mu, sigmai, ksi){
    # ai é estimada pela mediana de Lavras
    # bi é estimada pelo quantil 9% de Lavras
    # qi é gerado da gumbel com ai e bi como parâmetros
    # x é a amostra
    verossimil <- (1/sigmai^n) * prod( (1+ ksi*((x-mu)/sigmai))^((1+ksi)/ksi) ) * exp( sum(- (1+ ksi* ((x-mu)/sigmai))^(-1/ksi) ))
    pmu <- dnorm(mu, 0, 10)
    psig <- dgamma(sigmai, shape = 2, scale = 1/2)
    pksi <- dnorm(ksi, 0, 10)
    h <- verossimil * pmu * psig * pksi
    return(h)
}

# razão do critério de escolha 
razao <- function(y, x){
    # y e x matriz de parâmetros ??
    num <- posteriori(y[i], y[i+1], y[i+2]) * dmvnorm(x, y)
    den <- posteriori(x[i], x[i+1], x[i+2]) * dmvnorm(y, x)
}


# amostra da população de estudo TESTE (aqui é pra ser os dados de precipitação)
n <- 30
x <- rgumbel(n, 4)

# inicializando a cadeia
tamanho <- 20
xt <- matrix(nrow = tamanho, ncol = 3)
xt[1,] <- c(1,2,1)

# algoritmo M-H
for (t in 2:tamanho) {
    y <- rmvnorm(1)