rm(list = ls())

# observações -------------------------------------------------------------

# prioris: mu~N(mu0,sigma0); sigma~gamma(alpha0,beta0); ksi~N(mu1,sigma1)
# dist candidata 3 normais independentes

posteriori <- function(mu, sigmai, ksi){
    verossimil <- (1/sigmai^n) * prod( (1+ ksi*((x-mu)/sigmai))^(-(1+ksi)/ksi) ) * exp( sum(- (1+ ksi* ((x-mu)/sigmai))^(-1/ksi) ))
    pmu <- dnorm(mu, 0, 10) # priori pra mu
    psig <- dgamma(sigmai, shape = 2) # priori pra sigma
    pksi <- dnorm(ksi, 0, 10) # priori pra ksi
    h <- verossimil * pmu * psig * pksi
    return(h)
}



# Metropolis-Hastings -----------------------------------------------------


# densidade a posteriori
posteriori <- function(mu, sigmai, ksi){
    verossimil <- (1/sigmai^n) * prod( (1+ ksi*((x-mu)/sigmai))^((1+ksi)/ksi) ) * exp( sum(- (1+ ksi* ((x-mu)/sigmai))^(-1/ksi) ))
    pmu <- dnorm(mu, 0, 10) # priori pra mu
    psig <- dgamma(sigmai, shape = 2) # priori pra sigma
    pksi <- dnorm(ksi, 0, 10) # priori pra ksi
    h <- verossimil * pmu * psig * pksi
    return(h)
}

# razão do critério de escolha 
razao <- function(y, xt){
    num <- posteriori(y[1], y[2], y[3]) * dnorm(xt[1], mean = y[1])*dnorm(xt[2], mean = y[2])*dnorm(xt[3], mean = y[3])
    den <- posteriori(xt[1], xt[2], xt[3]) * dnorm(y[1], mean = xt[1])*dnorm(y[2], mean = xt[2])*dnorm(y[3], mean = xt[3])
    return(num/den)
}


# amostra da pressipitação de Jaboticabal
x <- readxl::read_xlsx('precipitacao_jaboticabal.xlsx')
x <- x$MAX.ANO
n <- length(x)

# inicializando a cadeia
tamanho <- 100
theta <- matrix(nrow = tamanho, ncol = 3)
theta[1,] <- c(16,36,10)

# algoritmo M-H
for (t in 2:tamanho) {
    u <- runif(1)
    y <- c(rnorm(1, mean = theta[t-1,1]), rnorm(1, mean = theta[t-1,2]), rnorm(1, mean = theta[t-1,3]))
    cat("\n", "t=",t,"y=",y," razao=", razao(y, theta[t-1,]))
    aceita <- razao(y, theta[t-1,])
    if(u <= aceita) {
        theta[t,] <- y
    } else {
        theta[t,] <- theta[t-1,]
    }
    
}

1 - length(unique(theta[,2]))/tamanho

# visualização das cadeias geradas
par(mfrow=c(1,3))
plot(theta[,1], type = 'l')
plot(theta[,2], type='l')
plot(theta[,3], type='l')


dgamma(xt[2], shape = y[2])
dgamma(y[2], shape = xt[2])
rgamma(1, shape = theta[t-1,2])

# algoritmo Metropolis ---------------------------------------------------

rm(list = ls())

posteriori <- function(mu, sigmai, ksi){
    verossimil <- (1/sigmai^n) * prod( (1+ ksi*((x-mu)/sigmai))^((1+ksi)/ksi) ) * exp( sum(- (1+ ksi* ((x-mu)/sigmai))^(-1/ksi) ))
    pmu <- dnorm(mu, 0, 10) # priori pra mu
    psig <- dgamma(sigmai, shape = 2, scale = 1/0.01) # priori pra sigma
    pksi <- dnorm(ksi, 0, 10) # priori pra ksi
    h <- verossimil * pmu * psig * pksi
    return(h)
}

razao <- function(y, xt){
    num <- posteriori(y[1], y[2], y[3])
    den <- posteriori(xt[1], xt[2], xt[3])
    return(num/den)
}


x <- readxl::read_xlsx('precipitacao_jaboticabal.xlsx')
x <- x$MAX.ANO
n <- length(x)

tamanho <- 50
theta <- matrix(nrow = tamanho, ncol = 3)
theta[1,] <- c(16,36,10)


for (t in 2:tamanho) {
    u <- runif(1)
    y <- c(theta[t-1,1] + rnorm(1), theta[t-1,2] + rnorm(1, 0, 6), theta[t-1,3] + rnorm(1))
    cat("\n", "t=",t,"y=",y," razao=", razao(y, theta[t-1,]))
    aceita <- razao(y, theta[t-1,])
    if(u <= aceita) {
        theta[t,] <- y
    } else {
        theta[t,] <- theta[t-1,]
    }

}

1 - length(unique(theta[,2]))/tamanho

par(mfrow=c(1,3))
plot(theta[,1], type = 'l')
plot(theta[,2], type='l')
plot(theta[,3], type='l')


# Deu certo uma vez -------------------------------------------------------


# rm(list = ls())
# 
# posteriori <- function(mu, sigmai, ksi){
#          verossimil <- (1/sigmai^n) * prod( (1+ ksi*((x-mu)/sigmai))^((1+ksi)/ksi) ) * exp( sum(- (1+ ksi* ((x-mu)/sigmai))^(-1/ksi) ))
#          pmu <- dnorm(mu, 0, 10) # priori pra mu
#          psig <- dgamma(sigmai, shape = 2, scale = 1/2) # priori pra sigma
#          pksi <- dnorm(ksi, 0, 10) # priori pra ksi
#          h <- verossimil * pmu * psig * pksi
#          return(h)
# }
# 
# razao <- function(y, xt){
#      num <- posteriori(y[1], y[2], y[3]) * dnorm(xt[1])*dnorm(xt[3])
#      den <- posteriori(xt[1], xt[2], xt[3]) * dnorm(y[1])*dnorm(y[2])*dnorm(y[3])
#      return(num/den)
# }
# 
# x <- readxl::read_xlsx('precipitacao_jaboticabal.xlsx')
# x <- x[,2]
# n <- length(x)
# 
# tamanho <- 20
# theta <- matrix(nrow = tamanho, ncol = 3)
# theta[1,] <- c(16,36,12)
# 
# for (t in 2:tamanho) {
#      u <- runif(1)
#      y <- c(rnorm(1, mean = theta[t-1,1]), rgamma(1, shape = theta[t-1,2]), rnorm(1, mean = theta[t-1,3]))
#      cat("\n", "t=",t,"y=",y," razao=", razao(y, theta[t-1,]))
#      aceita <- razao(y, theta[t-1,])
#      if(u <= aceita) {
#          theta[t,] <- y
#      } else {
#          theta[t,] <- theta[t-1,]
#      }
# }
