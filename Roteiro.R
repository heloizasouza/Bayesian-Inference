# Estrutura algoritmo Metropolis-Hastings


# posteriori --------------------------------------------------------------



# razão -------------------------------------------------------------------



# amostra da população de estudo ------------------------------------------

# atribuir os hiperparâmetros


# inicialização da cadeia -------------------------------------------------

# chute inicial da distribuição candidata


# algoritmo M-H -----------------------------------------------------------

# y é gerado da distribuição candidata
# para ajustar a taxa de rejeição altero o valor de sigma


# convergência da cadeia --------------------------------------------------

# posso usar os plots do Geweke -- package coda -- comando geweke.plot


# amostra independente e função perda -------------------------------------

# pra ter a amostra independente faço o burn-in e os saltos
# a estimativa pela função perda quadrática é a média e a perda absoluta é a mediana
# cada parâmetro deve ter sua amostra final


# estimação da densidade --------------------------------------------------

# as densidades podem ser plotadas da amostra final com histograma
# e podem ser estimadas pelo comando density()


# intervalo MDP -----------------------------------------------------------

# os intervalos MDP podem ser obtidos pelo comando HPDinterval()
# cada parâmetro deve ter seu intervalo




# METROPOLIS --------------------------------------------------------------

# O que muda do M-H para o metropolis é apenas que:
# a candidata não é usada mais e o y é estimado por
# y = xt[t-1] + rnorm(1,mu,sigma) com passeio aleatório




# GIBBS -------------------------------------------------------------------

# O que muda do M-H para o Gibbs é que:
# não preciso criar função a posteriori e razão, todos candidatos são aceitos;
# os parâmetros usam a si mesmos para serem gerados;
# preciso das distribuições individuais condicionadas de cada parâmetro
