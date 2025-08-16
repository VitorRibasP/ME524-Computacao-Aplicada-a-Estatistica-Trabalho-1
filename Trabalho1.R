library(tidyverse)

#Dados Coletados
n <- c(2,4,2,4,4,2,3,4,2,3,5,10,2,5)
m <- c(0,2,4,4,2,3,2,2,3,5,6,2,5)
r <- sum(n) - sum(m)

#Funcao que Executa o Amostrador de Gibbs no Primeiro caso em Questao
amostradorGibbs <- function(k) {
  
  #Matriz em que serão Guardados Resultados das Iteracoes
  matriz <- matrix(NA, nrow = k, ncol = 15)
  
  #Amostras Iniciais Baseadas na Priori
  matriz[1, 1] <- rpois(1, 30)
  matriz[1,2:15] <- runif(14, 0, 1)
  
  for (i in 2:k) {
    matriz[i, 1] <- rpois(1, 30*prod(1 - matriz[i-1, 2:15])) + 12
    matriz[i, 2:15] <- sapply(n, function(ni) rbeta(1, ni + 1, matriz[i,1] - ni + 1)) }
  return(matriz) }

#Implementando uma Amostra
set.seed(524)
amostra <- amostradorGibbs(1000)
colnames(amostra) <- c("N", paste0("p_", 1:14))
as.data.frame(amostra) %>% mutate("indice" = 1:1000) %>% 
  pivot_longer(1:15, names_to = "Var") %>% 
  mutate(Var = factor(Var, levels = c("N", paste0("p_", 1:14)))) %>% 
  ggplot() + geom_line(aes(x = indice, y = value)) +
  facet_wrap(~Var, scales = "free_y", ncol = 3) +
  theme_bw() + labs(x = "", y = "")

#Realizando 500 amostras para analisar
amostras <- matrix(NA, nrow = 500, ncol = 15)
set.seed(524)
for (i in 1:500) amostras[i,] <- amostradorGibbs(1000)[1000,]

colnames(amostras) <- c("N", paste0("p_", 1:14))
as.data.frame(amostras) %>% pivot_longer(1:15, names_to = "Var") %>% 
  mutate(Var = factor(Var, levels = c("N", paste0("p_", 1:14)))) %>% 
  ggplot() + geom_histogram(aes(x = value), fill = "black") +
  facet_wrap(~Var, scales = "free", ncol = 3) + 
  theme_bw() + labs(x = "", y = "")
  
#Amostrador de Gibbs no segundo caso em Questao
amostradorGibbs2 <- function(k) {
  
  #Matriz em que serão Guardados Resultados das Iterações
  matriz <- matrix(NA, nrow = k, ncol = 15)
  
  #Amostras Iniciais Baseados na Priori
  matriz[1, 1] <- rpois(1, 30)
  matriz[1,2:15] <- rbeta(14, 2, 5)
  
  for (i in 2:k) {
    matriz[i, 1] <- rpois(1, 30*prod(1 - matriz[i-1, 2:15])) + 12
    matriz[i, 2:15] <- sapply(n, function(ni) rbeta(1, ni + 2, matriz[i,1] - ni + 5)) }
  return(matriz) }

#Implementando uma amostra
set.seed(524)
amostra2 <- amostradorGibbs2(1000)
colnames(amostra2) <- c("N", paste0("p_", 1:14))

as.data.frame(amostra2) %>% mutate("indice" = 1:1000) %>% 
  pivot_longer(1:15, names_to = "Var") %>% 
  mutate(Var = factor(Var, levels = c("N", paste0("p_", 1:14)))) %>% 
  ggplot() + geom_line(aes(x = indice, y = value)) +
  facet_wrap(~Var, scales = "free_y", ncol = 3) +
  theme_bw() + labs(x = "", y = "") +
  

#Realizando 500 amostras para analisar
set.seed(524)
amostras2 <- matrix(NA, nrow = 500, ncol = 15)
for (i in 1:500) amostras2[i,] <- amostradorGibbs2(1000)[1000,]

colnames(amostras2) <- c("N", paste0("p_", 1:14))
as.data.frame(amostras2) %>% pivot_longer(1:15, names_to = "Var") %>% 
  mutate(Var = factor(Var, levels = c("N", paste0("p_", 1:14)))) %>% 
  ggplot() +
  geom_histogram(aes(x = value), fill = "black") +
  facet_wrap(~Var, scales = "free", ncol = 3) +
  theme_bw() + labs(x = "", y = "")
