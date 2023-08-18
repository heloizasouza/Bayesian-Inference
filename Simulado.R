# Simulado exercício prático prova 3

rm(list = ls())
library(coda)
set.seed(2023)



# densidade a posteriori --------------------------------------------------

posteriori <- function(alpha, sigma){
    ( (alpha/sigma)^n ) * ( prod((x/sigma)^(alpha-1)) ) * exp( -sum( (x/sigma)^alpha ) ) * dgamma(alpha,a,scale = 1/b) * dgamma(sigma,c,scale = 1/d)
}


# razao do critério de escolha --------------------------------------------

razao <- function(y,xt) {
    num <- posteriori(y[1], y[2])
    den <- posteriori(xt[1], xt[2])
    return(num/den)
}


# amostra da população em estudo ------------------------------------------

n <- 10
x <- rweibull(n, shape = 5, scale = 2)
summary(x)


# inicializando a cadeia --------------------------------------------------

a = b = c = d = 0.01 # hiperparâmetros
tamanho <- 20 # tamanho da cadeia
theta <- matrix(nrow = tamanho, ncol = 2) # matriz dos parâmetros
theta[1,] <- c(1,1) # chute inicial


# algoritmo Metropolis ----------------------------------------------------

for (t in 2:tamanho) {
    u <- runif(1)
    y <- c(theta[t-1,1] + rgamma(1,0,0.08), theta[t-1,2] + rnorm(1,0,0.08)) # simula por passeio aleatório os dois parâmetros ao mesmo tempo
    cat("\n", "t=",t,"y=",y," razao=", razao(y[t], theta[t-1,]))
    aceita <- razao(y, theta[t-1,])
    if(u <= aceita) {
        theta[t,] <- y
    } else {
        theta[t,] <- theta[t-1,]
    }
}

# taxa de rejeição
1 - length(unique(theta))/tamanho


# TESTES DOS ERROS --------------------------------------------------------


# posteriori aplicada nos valores de y
((y[1]/y[2])^n ) * ( prod( (x/y[2]) )^(y[1]-1) ) * exp( -sum( (x)^y[1] )/y[2]^y[1] ) * dgamma(y[1],a,scale = 1/b) * dgamma(y[2],c,scale = 1/d)

# POSTERIORI DO CADUUUU
y[1]^(n+a-1)*y[2]^(c-1-n*y[1])*exp((-sum(x^y[1])/(y[2]^y[1]))-b*y[1]-d*y[2])*prod(x^(y[1]-1))


# convergência da cadeia --------------------------------------------------

mcmc_ <- mcmc(theta)
geweke.plot(mcmc_[,1]) # convergência de alpha
geweke.plot(mcmc_[,2]) # convergência de sigma


# amostra aleatória -------------------------------------------------------

# cadeia do parâmetro alpha
alpha <- theta[seq(1001,tamanho,by=2),1]
# cadeia do parâmetro sigma
sigma <- theta[seq(1001,tamanho,by=2),2]

# função de perda quadrática
mean(alpha)
mean(sigma)

# função de perda absoluta
median(alpha)
median(sigma)


# Densidade por Kernel ----------------------------------------------------

par(mfrow=c(1,2))
hist(x = alpha, probability = T, main = 'Alpha')
lines(density(alpha), col='blue')
hist(sigma, probability = T, main = 'Sigma')
lines(density(sigma), col='red')


# Intervalos MDP ----------------------------------------------------------

HPDinterval(alpha) 
HPDinterval(sigma)
