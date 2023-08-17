# Amostrador de Gibbs


# Exemplo 1 ---------------------------------------------------------------

rm(list = ls())
set.seed(2023)

# amostra da população de estudo
n <- 100
x <- rnorm(n, 9, 2)

# tamanho da cadeia
tamanho <- 4000
theta <- matrix(nrow = tamanho, ncol = 2) # col 1-->mu e col 2-->tau
theta[1,] <- c(0,1) # chute inicial mu=0 e tau=1

# parâmetros conhecidos das prioris
a = 8
b = 1
sigma0 = 10

# algoritmo de Gibbs
for (t in 2:tamanho) {
    c <- (n*theta[t-1,2] + 1/sigma0^2)^(-1)
    m <- n*theta[t-1,2]*mean(x)*c
    mu <- rnorm(1, m, sqrt(c)) # gera mu
    
    alpha <- (n/2) + a
    beta <- 0.5*sum((x - mu)^2) + b
    tau <- rgamma(1, shape = alpha, scale = 1/beta) # gera o tau
    
    theta[t,] <- c(mu,tau) # matriz de parâmtros estimados
}

# plot dos valores gerados
par(mfrow=c(1,2))
plot(theta[,1], type = 'l')
plot(theta[,2], type = 'l')

# burn-in e saltos
amostra <- theta[seq(1001,tamanho,3),]

# plot da amostra independente gerada
par(mfrow=c(1,2))
plot(amostra[,1], type = 'l')
plot(amostra[,2], type = 'l')
