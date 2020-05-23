library(mvtnorm)
library(RColorBrewer)
library(plyr)
library(ggplot2)
library(gridExtra)
library(ggpubr) 

# função que faz o gráfico dos limites de decisão:
PlotGrid <- function(pred, dados, Grid, cores) {
  surf <- (ggplot(data = dados, aes(x = x1, y = x2), color = gp) +
             geom_tile(data = cbind(Grid, classes = pred), aes(fill = classes)) +
             scale_fill_manual(name = 'classes', values = cores) +
             ggtitle("Decision region") + theme(legend.text = element_text(size = 10)) +
             scale_colour_manual(name = 'classes', values = cores)) 
  pts <- (ggplot(data = dados, aes(x = x1, y = x2, color = gp)) +
            geom_contour(data = cbind(Grid, classes = pred), aes(z = as.numeric(classes)), 
                         color = "red") +
            geom_point(size = 4, alpha = .5) + 
            ggtitle("Decision boundary") +
            theme(legend.text = element_text(size = 10)) +
            scale_colour_manual(name = 'classes', values = cores)) 
  grid.arrange(surf, pts, ncol = 2)
}

#-------------------------------------------------------
# gerando dados para o exemplo:
#-------------------------------------------------------
# Vamos criar 3 níveis para Y (3 grupos).
# Para cada grupo vamos gerar (X1, X2) independentes,
# com distribuição normal. 
# No i-ésimo grupo X1 e X2 terão média igual a 4.5*i, i = 1,...,3
# Em todos os grupos a variância de X1 e X2 será igual a 1.
#
# OBS: A função mvtnorm gera variáveis com distribuição 
# normal multivariada, com matriz de variância e convariânica "sigma".
# Como estamos definindo sigma = diag(2), ou seja,  sigma = identidade,
# segue que as variáveis geradas são independentes.
# Nesse caso, seria equivalent usar rnorm() para gerar cada Xi
# de maneira separada.
#
#  A variável Y indica o grupo ao qual os X's pertencem
#  Nesse exemplo, a diferença entre os grupos se dá devido à média
#  das variáveis X's 
#

# Nesse exemplo os três grupos terão o mesmo tamanho, mas
# poderíamos gerar grupos com tamanhos distintos.

#  tamanho dos grupos:
ns = c(100,100,100)

set.seed(1234)
x = NULL
Y = NULL
for(i in 1:3){
  # gerando os dados do i-ésimo grupo:
  tp = rmvnorm(ns[i], mean = rep(4.5*i, 2), sigma = diag(2))
  
  # empilhando os valores de x
  x = rbind(x, tp)
  
  # A codificação abaixo não muda o resultado da classificação
  # No i-ésimo grupo Y vai assumir valor "i", i = 1,2,3.
  Y = c(Y, rep(i, ns[i]))
}

# salvando os valores gerados em um data.frame
#  - y será salva com sua categorização numérica
#  - a variável gp (que é igual a y, mas transformada em fator) servirá 
#     para os casos em que precisamos que a resposta seja categorica
dados = data.frame(x1 = x[,1], x2 = x[,2], y = Y, gp = as.factor(Y))



######################################
#
#   Analise dos dados
#
######################################
# lendo os dados...
# dados =  bla bla bla
head(dados, n = 3)

# número total de observações
n = nrow(dados)

# níveis de Y
ys = unique(dados$y)

# número de grupos:
K = length(ys)

# tamanho de cada grupo
ns = table(dados$y)


####################################################
#
# LDA  - Classificação baseada em probabilidade
#
####################################################

#-----------------------------------------------
#   Cálculo de Médias 
#-----------------------------------------------
# médias para cada Xi por grupo:
# a primeira coluna do data.frame resultante contém a categoria do y
Medias = ddply(dados, ~y, summarise, mx1 = mean(x1), mx2 = mean(x2))
Medias

# transpondo para aparecer os grupos nas colunas
# esse formato pode ser mais eficiente em alguns casos...
# eliminamos a primeira coluna que apenas contém
# a categoria à qual a média se refere
m = t(Medias[,-1])
colnames(m) = Medias[,1]; m

#-----------------------------------------------
#   Cáculo da matriz de Variância/covariâncias
#-----------------------------------------------
# Vamos salvar as variâncias de cada grupo em uma lista
# Vamos usar essas variâncias para calcular a variância combinada
# que é a soma ponderada das variâncias dos grupos

Sigma_gp = list()
Sigma_comb = 0
for(i in 1:K){
  # selecionando os indices correspondentes à i-ésima categoria:
  w = with(dados, which(y == ys[i]))
  
  # calculando a matriz de variância/covariância para os dados do 
  # i-ésimo grupo:  "cov" e "var" fazem a mesma coisa
  Sigma_gp[[i]] = with(dados[w,], cov(cbind(x1, x2)))
  
  # somatória das dispersões
  Sigma_comb = Sigma_comb + (length(w)-1)*Sigma_gp[[i]]
}
# soma das dispersões dividida por (n-K)
Sigma_comb = 1/(n - K)*Sigma_comb  # K = 3 grupos

# enfeitando um pouco...
names(Sigma_gp) = paste("Y=", ys, sep = "")
Sigma_gp


# estimativas para P(Y = y_k) = pi_k
prior = ns/n

#---------------------------------------------------
#   Função discriminante, k = 1,2,3 (grupos) 
#---------------------------------------------------
#  delta_k(x) = x'S_k^{-1}*m_k - 1/2*m_k'S_k^{-1}m_k + log(pi_k)
#  onde
#   m_k = vetor das médias do grupo k  
#   S_k = matriz de variância e covariância. Se 
#         estamos assumindo que as variâncias são iguais, usar 
#         Sigma_comb. Caso contrário usar Sigmas_gp[[k]]
#   pi_k = estimativa de P(Y = y_k)
#
#  fazendo:          
#    b_k0 = - 1/2*m'S^{-1}m + log(pi_k)  (constante)
#    (b_k1, b_k2)' =  S^{-1}*m   (matriz vezes vetor = vetor)
#  podemos escrever
#   delat_k(x) = (1, x')b_k, onde b_k = (b_k0, b_k1, b_k2)'
# 


# calculando os b_k's. 
# # b_k = (b_k0, b_k1, b_k2)', k = 1,2,3 
# A k-ésima coluna do B vai conter o b_k
#
# Vamos aproveitar o exemplo e fazer os dois casos 
#   Sigmas iguais e Sigmas diferentes.
# Como as variâncias são de fato iguais, deveria ficar muuuuito parecido.
#
B_Sigual = matrix(0, ncol = 3, nrow = 3)
B_Sdiferente = matrix(0, ncol = 3, nrow = 3)
for(k in 1:3){
  # constante que não depende de "x"  = b_k0
  B_Sigual[1,k] = -1/2*t(m[,k])%*%solve(Sigma_comb)%*%m[,k] + log(prior[k])
  B_Sdiferente[1,k] = -1/2*t(m[,k])%*%solve(Sigma_gp[[k]])%*%m[,k] + log(prior[k])
  # S^{-1}*mu[k] = (b_k1, b_k2)'
  B_Sigual[2:3,k] = solve(Sigma_comb)%*%m[,k]
  B_Sdiferente[2:3,k] = solve(Sigma_gp[[k]])%*%m[,k]
}
B_Sigual
B_Sdiferente

# construindo (1, x'): 
X = cbind(1, dados$x1, dados$x2)

# construindo as funções discriminate delta_k(x)
D_Sigual = matrix(0,ncol = 3, nrow = nrow(dados))
D_Sdiferente = matrix(0,ncol = 3, nrow = nrow(dados))
D_Sigual = X%*%B_Sigual
D_Sdiferente = X%*%B_Sdiferente

# enfeitando
colnames(D_Sigual) = colnames(D_Sdiferente) = paste("D", ys, sep = "")
head(D_Sigual, n = 3)
head(D_Sdiferente, n = 3)

#  Calculando a previsão com base nos valores 
# das funções discriminante
#
# yhat = y[j], y = argmax(delta_j(x))
Yhat_Sigual = apply(D_Sigual, 1, function(d) ys[which.max(d)])
Yhat_Sdiferente = apply(D_Sdiferente, 1, function(d) ys[which.max(d)])

# olhando para a matriz de confusão
# note que os resultados não são os mesmos...
table(dados$y, Yhat_Sigual)
table(dados$y, Yhat_Sdiferente)
table(Yhat_Sigual, Yhat_Sdiferente)

# vamos fazer um gráfico para visualizar o plano 
# e os limites de decisão
# 1) criando uma grade de x1 e x2 e fazendo a previsão com 
#    as funções discriminante para os valores nessa grade
npt = 200   
x1 <- seq(min(dados$x1), max(dados$x1), length.out=npt)    
x2 <- seq(min(dados$x2), max(dados$x2), length.out=npt)    
Grid = expand.grid(x1 = x1, x2 = x2,KEEP.OUT.ATTRS = FALSE) 
Dout_Sigual = cbind(1, as.matrix(Grid))%*%B_Sigual
Dout_Sdiferente = cbind(1, as.matrix(Grid))%*%B_Sdiferente
Yhat_Sigual = apply(Dout_Sigual, 1, function(d) ys[which.max(d)])
Yhat_Sdiferente = apply(Dout_Sdiferente, 1, function(d) ys[which.max(d)])

# transformando em categórica para usar nos gráficos:
pred_Sigual = as.character(Yhat_Sigual)
pred_Sdiferente = as.character(Yhat_Sdiferente)

# observamos diferença em vários pontos:
table(pred_Sigual, pred_Sdiferente)

# configurações para o gráfico
cores3 <- brewer.pal(3,'Set1')[3:1]
names(cores3) <- paste(ys)
PlotGrid(pred_Sigual, dados, Grid, cores3)
PlotGrid(pred_Sdiferente, dados, Grid, cores3)



#########################################################
#
# LDA  - Classificação usando ideias combinadas 
#   de distância + Probabilidade
#
#########################################################
library(MASS)

lda.fit = lda(y ~ x1+x2, data = dados, method = "moment")
lda.fit

# Abaixo são apresentados os coeficientes de combinações lineares 
# das variáveis X1,..., Xp. O intercepto w0 não aparece pois as 
# variáveis foram centradas na média dos grupos.
#
# L = w0 + w1*X1  + ... + wk*Xp
#
# se 
#   - X tem rank "r"
#   - Y tem "K" categorias
# existem  min(r, K-1) funções L (combinações lineares). 
# 
# As colunas do objeto abaixo contém os coeficientes wk's dessas combinações lineares.
#
# Estão ordenadas em ordem decrescente de poder discriminante.
# 
# Se quisermos uma projeção em um subespaço de dimensão 1, basta considerar
# a combinação L correspondente aos pesos LD1
# Para uma projeção em um subespaço de tamanho 2, considera-se LD1 e LD2,...
lda.fit$scaling

# Previsão: retorna uma lista com a classe predita, a probabilidade posteriori
# associada e o valor de "x" no eixo formado por LD1 e LD2
lda.pred = predict(lda.fit)
names(lda.pred)

# matriz de confusão
table(dados$y, lda.pred$class)

# (x1, x2) no sistema de eixos original e no novo sistema de eixos
lda.data <- cbind(dados, lda.pred$x)
g1 <- ggplot(lda.data, aes(x1, x2)) + 
  geom_point(aes(color = gp), size = 3) + theme_minimal()

g2 <- ggplot(lda.data, aes(LD1, LD2)) + 
  geom_point(aes(color = gp), size = 3) + theme_minimal()

ggarrange(g1, g2, ncol = 1, nrow = 2,  align = "hv", 
          widths = c(1,1), heights = c(1,1),
          common.legend = TRUE)


# limites de decisão
lda.fit.out = predict(lda.fit, newdata = Grid)
PlotGrid(lda.fit.out$class, dados, Grid, cores3)


#####
##  QDA: não usa ideias de distância
#####

# ajuste
qda.fit = qda(y ~ x1+x2, data = dados, method = "moment")
qda.fit

# Previsão:
qda.pred = predict(qda.fit)

# matriz de confusão
table(dados$y, qda.pred$class)

# limites de decisão
qda.fit.out = predict(qda.fit, newdata = Grid)
PlotGrid(qda.fit.out$class, dados, Grid, cores3)



##########################################################
#
# Classificação usando regressão logística
#
###########################################################
library(nnet)
# Training the multinomial model
multinom.fit <- multinom(y ~ x1+x2, data = dados)

# Checking the model
summary(multinom.fit)

## extracting coefficients from the model and exponentiate
exp(coef(multinom.fit))

#Predicting the values for train dataset
precticed <- predict(multinom.fit, newdata = dados, "class")

# Building classification table
ctable <- table(dados$y, precticed)
ctable

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)

# limites de decisão
pred.logi = predict(multinom.fit, newdata = Grid)
PlotGrid(pred.logi, dados, Grid, cores3)



##########################################################
#
#
# Para os curiosos - Passo a passo da LDA como o R faz:
#
#
###########################################################
ns = table(dados$y)

# médias por grupo:
Medias = ddply(dados, ~y, summarise, mx1 = mean(x1), mx2 = mean(x2))
m = t(Medias[,-1])

# centrando o X na média dos grupos Xi - mi
for(i in 1:K){
  w = which(dados$y == ys[i])
  dados$newX1[w] = dados$x1[w] - m[1,i]
  dados$newX2[w] = dados$x2[w] - m[2,i]
}
newX = cbind(dados$newX1, dados$newX2)

# desvio padrão dos X's centrados nas médias dos grupos
sds <- sqrt(diag(cov(newX)))
inv.sd <- diag(1/sds, ncol = 2)

#  X's centrados nas médias dos grupos e padronizados para ter desvio 1.
X01 = newX %*% inv.sd

dados$X01.1 = X01[,1]
dados$X01.2 = X01[,2]

# Calculando 1/sqrt(n-k)*Xpadronizado
# note que X'X = S_W/(n-K)
X <- sqrt(1/(n-K))*X01

# verificando que X'X  = S_W/(n-K)
cov(X01)*(n-1)/(n-K)
t(X)%*%X

# Decomposição SVD de X retorna os autovetores de S_W.
# Mais ainda S_W = V'DV.
#
# Na decomposição SVD:
# U = autovetores de XX', D = autovalores, V = autovetores de X'X
# ou seja, V são os autovetores da matriz de variância e covariância
# da variável X centrada na média dos grupos.
X.s <- svd(X, nu = 0L)
rank = length(X.s$d)

# Calculando 1/sd*VD^{-1}, onde V são os autovetores de S_W e 
# D é a matriz cuja diagonal são os autovalores
scaling <- inv.sd %*% X.s$v[, 1L:rank] %*% diag(1/X.s$d[1L:rank],ncol = rank)

# média global
xbar = colMeans(cbind(dados$x1, dados$x2))

# matriz com entradas sqrt(nk/(K-1))*(mu_i - xbar) *  1/sd*VD^{-1}
# M'M = D^{-1}V'(S_B/(K-1))VD^{-1}
M <- c(sqrt(ns/(K-1))) * scale(t(m), center = xbar, scale = FALSE) %*% scaling

# A decomposição SVD de XM vai nos dar os autovetores de 
# D^{-1}V'S_BVD^{-1} (a ,menos de uma constante)
X.s <- svd(M, nu = 0L)
w <- scaling %*% X.s$v[, 1L:rank]
dimnames(w) <- list(NULL, paste("LD", 1L:rank, sep = ""))

# comparando o output do lda.fit e as contas acima:
lda.fit$scaling
w


#####
# Previsão:
#####
ys = unique(dados$y) 
K = length(ys)
x = cbind(dados$x1, dados$x2)
prior = lda.fit$prior
# xbar usado para calcular os pesos
xbar <- colSums(prior*lda.fit$means)

# Usando as componentes LDA para prever:
# Usar LD1 e LD2 é equivalente a usar Sigma^{-1}
# compara com os resultados obtidos usando apenas a primeira
# componente principal
scaling <- lda.fit$scaling
scaling1 <- lda.fit$scaling[,1]

D.lda = matrix(0,ncol = 3, nrow = nrow(dados))
D.lda1 = matrix(0,ncol = 3, nrow = nrow(dados))

# (x - xbar) e (m_k - xbar)
x.scaled <- scale(x, center = xbar, scale = FALSE) %*% scaling
m.scaled <- scale(t(m), center = xbar, scale = FALSE)%*%scaling
x.scaled1 <- scale(x, center = xbar, scale = FALSE) %*% scaling1
m.scaled1 <- scale(t(m), center = xbar, scale = FALSE)%*%scaling1

# calculando log(P(Y = y | X = x))
for(i in 1:K){
  # constante que não depende de "x"
  p1 = c(-1/2*sum(m.scaled[i,]^2) + log(prior[i]))
  p11 = c(-1/2*sum(m.scaled1[i,]^2) + log(prior[i]))
  # parte que depende de x
  p2 = x.scaled%*%m.scaled[i,]
  p21 = x.scaled1%*%m.scaled1[i,]
  D.lda[,i] = p1 + p2
  D.lda1[,i] = p11 + p21
}
# previsão usando o máximo dos delta_k
pred.lda.d <- factor(ys[max.col(D.lda)], levels = ys)
pred.lda.d1 <- factor(ys[max.col(D.lda1)], levels = ys)

# comparando com as previsões do fit.lda
table(lda.pred$class, pred.lda.d)
table(pred.lda.d, pred.lda.d1)

# cálculo das probabilidades a posterior
# Assim é como o predict.lda faz:
#
# exp(menos a distância entre as D.lda's e o menor valor de cada linha)
# 0 < e^(-dif) <= 1, sendo 1 se, e somente se, d_k atinge o mínimo
dist = exp((D.lda - apply(D.lda, 1L, min, na.rm=TRUE)))
dist1 = exp((D.lda1 - apply(D.lda1, 1L, min, na.rm=TRUE)))
# posteriori = dist/soma(dist). Para garantir que soma 1 nas linhas
posterior <- dist / drop(dist %*% rep(1, K))
pred.lda.post <- factor(ys[max.col(posterior)], levels = ys)
posterior1 <- dist1 / drop(dist1 %*% rep(1, K))
pred.lda.post1 <- factor(ys[max.col(posterior1)], levels = ys)

# comparando
table(pred.lda.d, pred.lda.post)
table(pred.lda.post, lda.pred$class)
table(pred.lda.post, pred.lda.post1)


# usando delta do livro: delta_k = w*(x - media)
# aqui não faz sentido usar as duas componentes, apenas LD1
what = as.matrix(scaling[,1])
muhat = Medias[,2:3]
rownames(muhat) = Medias[,1]
teste = matrix(ncol = 3, nrow = nrow(dados))
for(i in 1:nrow(dados)){
  x = rbind(dados$x1[i], dados$x2[i])
  xmw = as.matrix(x - muhat)%*%what
  teste[i,] = abs(xmw)
}
pred.ldat <- factor(ys[max.col(-teste)], levels = ys)
table(pred.lda.post, pred.ldat)


