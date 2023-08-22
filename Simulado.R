# Simulado exercício prático prova 3

rm(list = ls())
library(coda)
set.seed(2023)



# densidade a posteriori --------------------------------------------------

# minha posteriori
posteriori <- function(alpha, sigma){
    ( (alpha/sigma)^n ) * ( prod((x/sigma)^(alpha-1)) ) * exp( -sum( (x/sigma)^alpha ) ) * dgamma(alpha,a,scale = 1/b) * dgamma(sigma,c,scale = 1/d)
}

# posteriori do Cadu
# posteriori <- function(alpha, sigma){
#     alpha^(n+a-1)*sigma^(c-1-n*alpha)*exp((-sum(x^alpha)/(sigma^alpha))-b*alpha-d*sigma)*prod(x^(alpha-1))
# }


# razao do critério de escolha --------------------------------------------

razao <- function(y,xt) {
    num <- posteriori(y[1], y[2])
    den <- posteriori(xt[1], xt[2])
    return(num/den)
}


# amostra da população em estudo ------------------------------------------

n <- 100
x <- rweibull(n, shape = 5, scale = 2)
summary(x)


# inicializando a cadeia --------------------------------------------------

a = b = c = d = 0.01 # hiperparâmetros das prioris
# sigmas do passeio aleatório
dp1 = 2
dp2 = 0.1
tamanho <- 20000 # tamanho da cadeia
theta <- matrix(nrow = tamanho, ncol = 2) # matriz dos parâmetros
theta[1,] <- c(2,1) # chute inicial


# algoritmo Metropolis ----------------------------------------------------

for (t in 2:tamanho) {
    u <- runif(1)
    y <- c(theta[t-1,1] + rnorm(1,0,dp1), theta[t-1,2] + rnorm(1,0,dp2)) # simula por passeio aleatório os dois parâmetros ao mesmo tempo
    #cat("\n", "t=",t,"y=",y," razao=", razao(y, theta[t-1,]))
    aceita <- razao(y, theta[t-1,])
    if(u <= aceita) {
        theta[t,] <- y
    } else {
        theta[t,] <- theta[t-1,]
    }
}

# taxa de rejeição
1 - length(unique(theta[,1]))/tamanho

# visualização das cadeias geradas
par(mfrow=c(1,2))
plot(theta[,1], type = 'l')
plot(theta[,2], type='l')


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


# convergência da cadeia --------------------------------------------------

geweke.plot(as.mcmc(alpha)) # convergência de alpha
geweke.plot(as.mcmc(sigma)) # convergência de sigma


geweke.diag(as.mcmc(alpha), frac1 = 0.4)

# Densidade por Kernel ----------------------------------------------------

par(mfrow=c(1,2))
hist(x = alpha, probability = T, main = 'Alpha')
lines(density(alpha), col='blue')
hist(sigma, probability = T, main = 'Sigma')
lines(density(sigma), col='red')


# Intervalos MDP ----------------------------------------------------------

HPDinterval(as.mcmc(alpha)) 
HPDinterval(as.mcmc(sigma))
