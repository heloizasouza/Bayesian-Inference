rm(list = ls())


# normal multivariada -----------------------------------------------------


library(mvtnorm) # pacote para distribuição normal multivariada


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
    
    # jacobiano <- (sigmai/ksi^2) * det()
    nucleo_gumbel <- prod( (1/bi) * exp( -((qi-ai)/bi) - exp(- ((qi-ai)/bi))))
    n <- length(x) # tamanho da amostra
    verossimil <- (1/sigmai^n) * prod( (1+ ksi*((x-mu)/sigmai))^((1+ksi)/ksi) ) * exp( sum(- (1+ ksi* ((x-mu)/sigmai))^-1/ksi ))
}


# razão do critério de escolha 
razao <- function(y, x){
    # y e x matriz de parâmetros ??
    num <- posteriori(y[i], y[i+1], y[i+2]) * dmvnorm(x, y)
    den <- posteriori(x[i], x[i+1], x[i+2]) * dmvnorm(y, x)
}




# a amostra observada são as precipitações né?

