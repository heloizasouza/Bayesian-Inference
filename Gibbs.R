# Amostrador de Gibbs


# Exemplo 1 ---------------------------------------------------------------

rm(list = ls())
set.seed(2023)

# amostra da população de estudo
n <- 100
x <- rnorm(n, 9, 2)

# parâmetros conhecidos das prioris
a = 8
b = 1
sigma0 = 10

# inicializando a cadeia
tamanho <- 4000
theta <- matrix(nrow = tamanho, ncol = 2) # col 1-->mu e col 2-->tau
theta[1,] <- c(0,1) # chute inicial mu=0 e tau=1


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


# Exercício 1 -------------------------------------------------------------

rm(list = ls())
set.seed(2023)

# amostra da população de estudo com dois parâmetros
x <- c(rpois(10,3), rpois(20,10))
n <- length(x)

# hiperparâmetros (são parâmetros conhecidos usados pra controlar o processo de aprendizado do modelo)
a= b = c = d = 0.1

# inicializando a cadeia
tamanho <- 4000 # tamanho da cadeia
theta <- matrix(nrow = tamanho, ncol = 3) # col1--> lambda, col2--> phi, col3--> m
theta[1,] <- c(1,1,n/2)

# algoritmo de Gibbs
for (t in 2:tamanho) {
    s1 <- sum(x[1:theta[t-1,3]]) # soma de x até o ponto de mudança m
    s2 <- sum(x) - s1 # soma de x após o ponto de mudança m
    
    alp <- s1 + a
    bet <- theta[t-1,3] + b
    lambda <- rgamma(1, alp, bet)
    
    alp <- s2 + c
    bet <- n - theta[t-1,3] + d
    phi <- rgamma(1, alp, bet)
    
    prob <- numeric()
    for (j in 1:n) {
        t1 <- sum(x[1:j])
        t2 <- sum(x)-t1
        aux <- (lambda^t1) * exp(-j*lambda) * (phi^t2) * exp(-(n-j)*phi)
        prob <- c(prob,aux)
    }
    soma <- sum(prob)
    probb <- prob/soma
    m <- sample(x = 1:n, size = 1, prob = probb)
    
    theta[t,] <- c(lambda, phi, m)

}

# plot dos valores gerados
par(mfrow=c(1,3))
plot(theta[,1], type = 'l')
plot(theta[,2], type = 'l')
plot(theta[,3], type = 'l')

# amostra independente
amostra <- theta[seq(1001,tamanho,by=3),]

# densidade por kernel
hist(amostra[,1], probability = T,main = 'Lambda')
lines(density(amostra[,1]),col='red')

# Convergência e intervalo MDP --------------------------------------------


# análise de convergência da cadeia
library(coda)
mcmc_ <- mcmc(amostra) # objeto do tipo mcmc
codamenu() # para análise e diagnóstico da cadeia

# Gráfico de Geweke para convergência
geweke.plot(mcmc_[,1], nbins = 20) # convergência do lambda
geweke.plot(mcmc_[,2], nbins = 20) # convergência do phi
geweke.plot(mcmc_[,3], nbins = 20) # convergência do m

# intervalo de credibilidade
HPDinterval(mcmc_[,1], prob = 0.95) # intervalo do lambda
HPDinterval(mcmc_[,2], prob = 0.95) # intervalo do phi
HPDinterval(mcmc_[,3], prob = 0.95) # intervalo do m



# Intervalo de Máxima Densidade a posteriori - MDP calculado

# parâmetro lambda
lambda <- theta[seq(1001,tamanho, by=3), 1]
acf(lambda)
plot(lambda, type = 'l')

# função para obter o intervalo de credibilidade
lambda_Ord <- sort(lambda)
quant <- length(lambda_Ord)
IC_L <- function(alpha){
    LI = LS = numeric()
    for (i in 1:(quant - (1-alpha)*quant)) {
        LI[i] <- lambda_Ord[i]
        LS[i] <- lambda_Ord[i + (1-alpha)*quant]
    }
    cbind(LI, LS, LS-LI)
}

intervalos <- IC_L(0.05)
j <- which.min(intervalos[,3]) # índice do intervalo de menor comprimento
cat("O intervalo de credibilidade de 95% é: (",round(intervalos[j,1],2),",",round(intervalos[j,2],2),")")
