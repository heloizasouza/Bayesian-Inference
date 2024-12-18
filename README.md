# Análise Bayesiana no Estudo do Tempo de Retorno das Precipitações Pluviais Máximas em Jaboticabal (SP)

Este repositório contém a implementação de uma análise bayesiana para estimar o tempo de retorno das precipitações pluviais máximas na cidade de Jaboticabal, São Paulo. O objetivo principal é aplicar a Inferência Bayesiana em conjunto com distribuições a priori baseadas em quantis extremos para incorporar o conhecimento de especialistas e melhorar a qualidade das estimativas.

## Descrição do Projeto

O estudo utiliza dados históricos de precipitação máxima para prever chuvas extremas, essenciais para a elaboração de projetos agrícolas e de engenharia hidráulica. A distribuição generalizada de valores extremos (GEV) é comumente utilizada nesses estudos, porém apresenta desafios na obtenção de estimativas confiáveis. Este trabalho visa aprimorar essas estimativas através de uma abordagem bayesiana.

A análise foca nas precipitações máximas anuais e mensais para os períodos da estação chuvosa, utilizando dados de 40 anos (1956-1995) fornecidos pelo Departamento de Ciências Exatas da Faculdade de Ciências Agrárias e Veterinária da UNESP - campus Jaboticabal, estado de São Paulo.

## Metodologia

1. **Inferência Bayesiana**: A análise adota uma distribuição a priori baseada em quantis extremos, permitindo incorporar o conhecimento de especialistas.
2. **Algoritmo de Metropolis**: O amostrador MCMC (Cadeias de Markov por Monte Carlo) foi utilizado para realizar a inferência a posteriori dos parâmetros da distribuição.
3. **Distribuição Generalizada de Valores Extremos (GEV)**: A distribuição GEV foi utilizada para modelar as precipitações extremas, com foco em tempos de retorno de 10 e 20 anos.

## Dependências

O código utiliza as seguintes bibliotecas R:

- `readxl` — Faz a leitura de arquivos Excel (.xls e .xlsx) no R.
- `ggplot2` — Faz a criação de gráficos e visualizações de dados de forma eficiente e flexível.
- `coda` — Utilizado para análise de cadeias de Markov e diagnóstico de modelos bayesianos.
- `mvtnorm` — Fornece funções para trabalhar com distribuições normais multivariadas.

## Contribuições

Contribuições são bem-vindas! Se você tiver sugestões ou melhorias para este projeto, sinta-se à vontade para fazer um fork e enviar pull requests.

