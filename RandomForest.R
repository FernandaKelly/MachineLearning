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
#   Random Forest
#
########################################


#opçoes básicas:
modelo_rf = randomForest(country ~., data = dados, subset = inTrain,
                         ntree = 500, mtry = 6, importance = TRUE) #6 numero de variáveis que vai ser selecionada em cada iteração
modelo_rf
names(modelo_rf) #importance é a importancia das variáveis


# previsão in sample e out-of-sample
rfIn <- predict(modelo_rf, newdata=dados[inTrain,])
rfOut <- predict(modelo_rf, newdata=dados[-inTrain,])

# matriz de confusão
table(dados$country[inTrain], rfIn)
table(dados$country[-inTrain], rfOut)

####################################################################
# To check important variables
#  Nas três primeiras colunas observamos a importância 
#    da variável na predição de cada classe em particular.
#  
# MeanDecreaseAccuracy: quanto maior, melhor!!!
#     contains a measure of the extent to which a 
#     variable improves the accuracy of the forest in predicting the classification.
#     Higher values mean that the variable improves prediction. 
#     In a rough sense, it can be interpreted as showing the amount of increase 
#     in classification accuracy that is provided by including the variable in the model 
#
# MeanDecreaseGini: provides a more nuanced measure of importance, 
#     which factors in both the contribution that variable makes to 
#     accuracy, and the degree of misclassification 
####################################################################



importance(modelo_rf)        
varImpPlot(modelo_rf)  



# comparando a taxa de erros para diferentes valores de mtry

oob.err = double(10)
test.err = double(10)

for(mtry in 1:10){
  fit = randomForest(country ~., data = dados, subset = inTrain,
                     ntree = 500, mtry = mtry)
  oob.err[mtry] = fit$err.rate[500]
  pred = predict(fit, dados[-inTrain,])
  test.err[mtry] = with(dados[-inTrain,], mean(country != pred))
}
matplot(1:mtry, cbind(oob.err, test.err),
        pch = 23, col = c("red", "blue"), type = "b",
        ylab="Mean Squared Error")
legend("center", legend = c("OOB", "Test"), pch = 23, col = c("red", "blue"))





#--------------------------------------
# clustering com randomForest
#--------------------------------------

colnames(dados)
ncol(dados)
str(dados)

modelo_rf2 = randomForest(dados[,1:11], proximity = TRUE, mtry = 6)
modelo_rf2

prox <- modelo_rf2$proximity #matriz de proximidade é a matriz de similaridade do cluster
kk = kmeans(prox, centers = 3)
gps = as.character(kk$cluster)

ggplot(data = dados, aes(x = mpg, y= hp)) +
  geom_point() + 
  geom_text(aes(label=country, col = gps), hjust = 0, vjust = 0) +
  theme_minimal()





# como usar com hclust
prox <- as.dist(modelo_rf2$proximity)

hc = hclust(prox, method = "complete")

gp.c = as.character(cutree(hc, k = 3))

ggplot(data = dados, aes(x = mpg, y= hp)) +
  geom_point() + 
  geom_text(aes(label = country, col = gp.c), hjust = 0, vjust = 0) +
  theme_minimal()
table(gp.c, dados$country)






