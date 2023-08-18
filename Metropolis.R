# Amostrador Metropolis



# Exemplo -----------------------------------------------------------------

rm(list = ls())

# probabilidade a posteriori
posteriori <- function(mu){
    (1+ mu^2/v)^{-(v+1)/2} * exp(-0.5* sum(x-mu)^2)
}

# razão do critério de escolha
razao <- function(y,x){
    num <- posteriori(y)
    den <- posteriori(x)
    return(num/den)
}

# amostra da população de estudo
set.seed(2023)
x <- rnorm(50)

# variâncias para comparação das cadeias
sigma0 <- c(0.2,0.1,0.05,0.001)

# algotirmo Metropolis
n <- 4000
v <- 4
rejeicao <- numeric()
mu <- matrix(NA,ncol = 4,nrow = n)
mu[1,] <- 0.3

for (i in 1:4) {
    u <- runif(n)
    k <- 0
    for (t in 2:n) {
        y <- mu[t-1, i] + rnorm(1,0, sigma0[i]) # gerando passeio aleatório ~N(0,sigma0)
        aceita <- razao(y, mu[t-1, i])
        if(u[t] <= aceita) {
            mu[t, i] <- y
        } else {
            mu[t, i] <- mu[t-1, i]
            k <- k+1
        }
    }
    rejeicao[i] <- k/n
}

# plot dos valores gerados
cor <- c('red', 'blue','green', 'yellow')

par(mfrow=c(2,2))
for (i in 1:4) {
    plot(mu[,i], type='l', col=cor[i], 
         main = paste0('Sigma2=', sigma0[i], ", rejeição=", rejeicao[i]))    
}



# Exercício 1 -------------------------------------------------------------
rm(list = ls())

# log_posteriori <- function(theta){
#     -(1/400) * (-2*theta*n*(x1-x2) + n*theta^2) + dnorm(theta, mean = 10, sd = sqrt(50), log = T)
# }
# 
# razão do critério de escolha
# razao <- function(y, x) {
#     num <- log_posteriori(y)
#     den <- log_posteriori(x)
#     return(exp(num-den))
# }


# densidade a posteriori
posteriori <- function(theta){
     exp( -(1/400)*(-2*theta*n*(x1-x2) + n*theta^2) )*dnorm(theta, mean=10, sd=sqrt(50))
 }

# razão do critério de escolha
 razao <- function(y, x) {
     num <- posteriori(y)
     den <- posteriori(x)
     return(num/den)
 }

# amostras da população de estudo
set.seed(2023)
n <- 25
x1 <- mean(rnorm(n,80,10)) # xbarra população 1
x2 <- mean(rnorm(n,60,10)) # xbarra população 2
#sigma0 <- c(0.1,1,5,10)

# algotirmo Metropolis
tamanho <- 4000
#mu <- matrix(nrow = tamanho, ncol = length(sigma0))
mu <- numeric(tamanho)
mu[1] <- 3

for (t in 2:tamanho) {
    y <- mu[t-1] + rnorm(1, 0, 5) # gerando passeio aleatório ~N(0,sigma)
    u <- runif(1)
    mu[t] <- ifelse(u <= razao(y, mu[t-1]), y, mu[t-1])
}

# plot dos valores gerados
plot(mu, type = 'l')

# quantidade de rejeições
1 - length(unique(mu))/tamanho

# correlação da cadeia gerada
acf(mu)
