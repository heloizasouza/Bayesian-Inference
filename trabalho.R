rm(list = ls())

# observações -------------------------------------------------------------

# prioris: mu~N(mu0,sigma0); sigma~gamma(alpha0,beta0); ksi~N(mu1,sigma1)
# dist candidata 3 normais independentes




# Metropolis-Hastings -----------------------------------------------------


# densidade a posteriori
posteriori <- function(mu, sigmai, ksi){
    verossimil <- (1/sigmai^n) * prod( (1+ ksi*((x-mu)/sigmai))^((1+ksi)/ksi) ) * exp( sum(- (1+ ksi* ((x-mu)/sigmai))^(-1/ksi) ))
    pmu <- dnorm(mu, 0, 10) # priori pra mu
    psig <- dgamma(sigmai, shape = 2, scale = 1/2) # priori pra sigma
    pksi <- dnorm(ksi, 0, 10) # priori pra ksi
    h <- verossimil * pmu * psig * pksi
    return(h)
}

# razão do critério de escolha 
razao <- function(y, xt){
    num <- posteriori(y[1], y[2], y[3]) * dnorm(x, mean = y)
    den <- posteriori(xt[1], xt[2], xt[3]) * dnorm(y, mean = x)
    return(num/den)
}


# amostra da pressipitação de Jaboticabal
x <- readxl::read_xlsx('precipitacao_jaboticabal.xlsx')
x <- x[,-1]
n <- length(x)

# inicializando a cadeia
tamanho <- 20
theta <- matrix(nrow = tamanho, ncol = 3)
theta[1,] <- c(0.5,2,0.33)

# algoritmo M-H
for (t in 2:tamanho) {
    u <- runif(1)
    y <- c(rnorm(1, mean = theta[t-1,1]), rgamma(1, shape = theta[t-1,2]), rnorm(1, mean = theta[t-1,3]))
    cat("\n", "t=",t,"y=",y," razao=", razao(y, theta[t-1,]))
    aceita <- razao(y, theta[t-1,])
    if(u <= aceita) {
        theta[t,] <- y
    } else {
        theta[t,] <- theta[t-1,]
    }
    
}
