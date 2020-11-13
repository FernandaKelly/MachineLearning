################################ PACOTES UTILIZADOS #####################################################################################
library(caret)
# ID3
library(data.tree)
# CART
library(rpart)      # for computing decision tree models 
library(rpart.plot)
# Bagging
library(ipred)
# Random Forest
library(randomForest)
# Boosting
library(gbm)
##########################################################################################################################################



########################################
#
#   Bagging
#
########################################


#DADOS DO ESTUDO

attach(mtcars)
dados = mtcars
dados$country <- as.factor(c(rep("Japan", 3), 
                             rep("US",4), 
                             rep("Europe", 7),
                             rep("US",3), "Europe",
                             rep("Japan", 3), rep("US",4), rep("Europe", 3), 
                             "US", rep("Europe", 3)))

#ALTERANDO AS ALGUMAS VARIÁVEIS
dados$vs = factor(dados$vs, levels = c(0,1), labels = c("V-shaped", "straight"))
dados$am = factor(dados$am, levels = c(0,1), labels = c("automatic", "manual"))


########################################
#---------------------------------------------------------
# usando a função bagging() do pacote "ipred" diretamente
# chama a função rpart() do pacote "rpart"
#
# NOTA: o parâmetro "cp" no rpart é usado apenas 
#       para imprimir o output. O parâmetro de 
#       complexidade para a poda da árvore é 
#       escolhido por CV.
#----------------------------------------------------------
########################################

set.seed(123)

inTrain <- createDataPartition(dados$country, p = 3/4)[[1]] #Dados de treinamento

modelo_bag <- bagging(country ~., 
                      data = dados[inTrain,], 
                      nbagg = 100, coob = TRUE, #nbagg numero de amostras bootstrap 
                      #coob é o out of bagg
                      control = rpart.control(minsplit = 2, cp = 0))

modelo_bag
names(modelo_bag)
modelo_bag$mtrees[1] #guarda todas as árvores

# previsão in sample e out-of-sample
clsIn <- predict(modelo_bag, newdata=dados[inTrain,])
clsOut <- predict(modelo_bag, newdata=dados[-inTrain,])


# matriz de confusão
table(dados$country[inTrain], clsIn)
table(dados$country[-inTrain], clsOut) #fazer com o confusiomatrix

# proporção de classificações incorretas
cat("Misclass error est: ", mean(dados[inTrain, "country"] != clsIn), "")
cat("Misclass error est: ", mean(dados[-inTrain, "country"] != clsOut), "")

# proporção de classificações corretas
mean(dados[inTrain, "country"] == clsIn)
mean(dados[-inTrain, "country"] == clsOut)

#  out-of-bag erro
cat("Misclass error oob: ", modelo_bag$err, "")

confusionMatrix(dados$country[-inTrain], clsOut)

# não tem gráfico pra isso











































