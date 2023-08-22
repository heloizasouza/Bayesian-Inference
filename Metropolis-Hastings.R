# Amostrador Metropolis-Hastings


# Exemplo 1 ---------------------------------------------------------------


set.seed(2023)

# amostra da população
z <- rbinom(n = 200, size = 1, prob = 0.3)

# distribuição candidata unif(0,1)


# probabilidade a posteriori
posteriori <- function(theta,amostra){
    n <- length(amostra)
    h <- theta^(1+sum(amostra))*(1-theta)^(1+n- sum(amostra))
    return(h)
}

# razão de escolha do candidato
razao <- function(amostra,y,x){
    num <- posteriori(y,amostra)
    den <- posteriori(x,amostra)
    return(num/den)
}


# tamanho da cadeia de Markov
n <- 11000 
x <- numeric(n)

# x_0 da distribuição candidata
x[1] <- runif(1)


# algoritmo Metropolis-Hastings
for (i in 2:n) {
    y <- runif(1)
    u <- runif(1)
    x[i] <- ifelse(u <= razao(z, y, x[i-1]), y, x[i-1])
}

# resumo da distribuição desejada
summary(x)

# plot dos valores gerados pelo MCMC
plot(seq(1:n), x, type = "l")

# quantidade de rejeições
1 - length(unique(x))/n

# correlação na cadeia
acf(x)

# amostra com burnin e saltos
amostra <- x[seq(1001,n,by=10)]

# correlação na amostra final
acf(amostra)

# plot da amostra independente
plot(seq_along(amostra), amostra, type = "l")


# comparação duma amostra teórica com a obtida por mcmc
hist(amostra, freq = F)
lines(density(amostra), col="red") # amostra mcmc
theta_aux <- seq(0.01,0.99,0.01)
lines(theta_aux, dbeta(theta_aux,35,69), col="blue") # amostra teórica



# Exercício 1 -------------------------------------------------------------


rm(list = ls())
set.seed(2023)

# amostra da população
z <- rpois(n = 30, lambda = 4)

# tamanho da cadeia de Markov
n <- 11000
x <- numeric(n)

# distribuição a posteriori
posteriori <- function(amostra, theta){
    m <- length(amostra)
    h <- theta^(1+sum(amostra))*exp(-theta*(m+2))
    return(h)
}

# distribuição candidata gamma

# razão de escolha do candidato
razao <- function(amostra, y, x){
    num <- posteriori(amostra, y)*dgamma(x, shape = y)
    den <- posteriori(amostra, x)*dgamma(y, shape = x)
    return(num/den)
}

# gerando x_0 de chute inicial
x[1] <- rgamma(1,3)

# algoritmo M-H
for (i in 2:n) {
    y <- rgamma(1, shape = x[i-1])
    u <- runif(1)
    x[i] <- ifelse(u <= razao(z, y, x[i-1]), y, x[i-1])
}

# plot dos valores gerados pelo amostrador
plot(x, type = 'l')

# quantidade de rejeições
1 - length(unique(x))/n

# correlação na cadeia
acf(x)

# amostra com burnin e jumps
amostra <- x[seq(1001,n,by=10)]

# plot da amostra final
plot(amostra, type = 'l')

# resumo da amostra
summary(amostra)

# comparação da amostra teórica com a gerada
hist(amostra, freq = F)
lines(density(amostra), col='red')
theta_aux <- seq(1,8,by=0.01)
lines(theta_aux, dgamma(theta_aux, shape = sum(z)+2, rate = length(z)+2), col='blue')


# Exercício 2 -------------------------------------------------------------

rm(list = ls())
set.seed(2023)


# tamanho da cadeia de Markov
n <- 21000
x <- numeric(n)

# distribuição a posteriori
posteriori <- function(amostra, mu){
    h <- exp(-0.5*sum(amostra-mu)^2) * (1+mu^2/4)^(-5/2)
    return(h)
}

# razao de escolha do candidato
razao <- function(amostra,y,x){
    num <- posteriori(amostra,y)*dnorm(x,mean = y,0.05)
    den <- posteriori(amostra,x)*dnorm(y,mean = x,0.05)
    return(num/den)
}

# amostra da população
z <- rnorm(n = 90)

# chute inicial x_0
x[1] <- 0.3

# algoritmo Metropolis-Hastings
for (i in 2:n) {
    y <- rnorm(1,x[i-1],0.05)
    u <- runif(1)
    x[i] <- ifelse(u <= razao(z,y,x[i-1]), y, x[i-1])
}

# plot dos valores gerados
plot(x, type = 'l')

# número de rejeições
1 - length(unique(x))/n

# amostra com burnin e jump
amostra <- x[seq(1001,n,by=10)]

# resumo da amostra independente
summary(amostra)
acf(amostra)
plot(amostra, type='l')

# estimação da densidade a posteriori
hist(amostra, freq =F)
lines(density(amostra),col='red')