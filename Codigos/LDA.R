####################################################
#  CÓDIGO RESUMINDO:
#  LDA  - Classificação baseada em probabilidade
####################################################
# leia os dados de algum lugar:
# dados =  bla bla bla

# n, grupos, nr de grupos e tamanho dos grupos
n = nrow(dados)
ys = unique(dados$y)
K = length(ys)
ns = table(dados$y)

# medias dos grupos
Medias = ddply(dados, ~y, summarise, mx1 = mean(x1), mx2 = mean(x2))
m = t(Medias[,-1])

# variancia/covariancia dos grupos
Sigma_gp = list()
Sigma_comb = 0
for(i in 1:K){
  w = with(dados, which(y == ys[i]))
  Sigma_gp[[i]] = with(dados[w,], cov(cbind(x1, x2)))
  Sigma_comb = Sigma_comb + (length(w)-1)*Sigma_gp[[i]]
}
Sigma_comb = 1/(n - K)*Sigma_comb  

# Parâmetros da função discriminante
prior = ns/n
B_Sigual = matrix(0, ncol = 3, nrow = 3)
B_Sdiferente = matrix(0, ncol = 3, nrow = 3)
for(k in 1:3){
  B_Sigual[1,k] = -1/2*t(m[,k])%*%solve(Sigma_comb)%*%m[,k] + log(prior[k])
  B_Sdiferente[1,k] = -1/2*t(m[,k])%*%solve(Sigma_gp[[k]])%*%m[,k] + log(prior[k])
  B_Sigual[2:3,k] = solve(Sigma_comb)%*%m[,k]
  B_Sdiferente[2:3,k] = solve(Sigma_gp[[k]])%*%m[,k]
}

# Valor da função discrininante para os dados 
X = cbind(1, dados$x1, dados$x2)
D_Sigual = matrix(0,ncol = 3, nrow = nrow(dados))
D_Sdiferente = matrix(0,ncol = 3, nrow = nrow(dados))
D_Sigual = X%*%B_Sigual
D_Sdiferente = X%*%B_Sdiferente

# Classificação
Yhat_Sigual = apply(D_Sigual, 1, function(d) ys[which.max(d)])
Yhat_Sdiferente = apply(D_Sdiferente, 1, function(d) ys[which.max(d)])

# Matriz de confusão
table(dados$y, Yhat_Sigual)
table(dados$y, Yhat_Sdiferente)




#########################################################
#
# LDA  - Classificação usando ideias combinadas 
#   de distância + Probabilidade
#
#########################################################
library(MASS)
# ajuste
lda.fit = lda(y ~ x1+x2, data = dados, method = "moment")
lda.fit

# Previsão:
lda.pred = predict(lda.fit)

# matriz de confusão
table(dados$y, lda.pred$class)


############################################
#
#  QDA: assumindo que os grupos não tem a
#   mesma matiz de var/cov
#
############################################
# ajuste
qda.fit = qda(y ~ x1+x2, data = dados, method = "moment")
qda.fit

# Previsão:
qda.pred = predict(qda.fit)

# matriz de confusão
table(dados$y, qda.pred$class)


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

# Predicting the values for train dataset
precticed <- predict(multinom.fit, newdata = dados, "class")

# Building classification table
ctable <- table(dados$y, precticed)
ctable

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)