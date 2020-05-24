library(caret)      # for easy machine learning workflow

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



########################################
#
#   ID3
#   https://cran.r-project.org/web/packages/data.tree/vignettes/applications.html#id3-machine-learning
#
########################################
# Função para verificar se um conjunto é puro
# a classe deve estar na última coluna!!
IsPure <- function(data){
  length(unique(data[,ncol(data)])) == 1
}

# Função para calcular a Entropia
Entropy <- function(vls) {
  res <- vls/sum(vls) * log2(vls/sum(vls))
  # se algum valor é 0 o cálculo acima retorn NA.
  # vamos corrigir pois deveria ser 0
  res[vls == 0] <- 0
  -sum(res)
}

# Função para calcular o ganho de informação:
InformationGain <- function(tble) {
  tble <- as.data.frame.matrix(tble)
  entropyBefore <- Entropy(colSums(tble))
  s <- rowSums(tble)
  entropyAfter <- sum(s/sum(s)*apply(tble, MARGIN = 1, FUN = Entropy))
  informationGain <- entropyBefore - entropyAfter
  return (informationGain)
}

# Algoritmo ID3
# assume that the classifying features are in columns 1 to n-1
TrainID3 <- function(node, data) {
  
  node$obsCount <- nrow(data)
  
  #if the data-set is pure (e.g. all toxic), then
  if (IsPure(data)) {
    #construct a leaf having the name of the pure feature (e.g. 'toxic')
    child <- node$AddChild(unique(data[,ncol(data)]))
    node$feature <- tail(names(data), 1)
    child$obsCount <- nrow(data)
    child$feature <- ''
  } else {
    #calculate the information gain
    ig <- sapply(colnames(data)[-ncol(data)], 
                 function(x) InformationGain(
                   table(data[,x], data[,ncol(data)])
                 )
    )
    #chose the feature with the highest information gain (e.g. 'color')
    #if more than one feature have the same information gain, then take
    #the first one
    feature <- names(which.max(ig))
    node$feature <- feature
    
    #take the subset of the data-set having that feature value
    
    childObs <- split(data[ ,names(data) != feature, drop = FALSE], 
                      data[ ,feature], 
                      drop = TRUE)
    
    for(i in 1:length(childObs)) {
      #construct a child having the name of that feature value (e.g. 'red')
      child <- node$AddChild(names(childObs)[i])
      
      #call the algorithm recursively on the child and the subset      
      TrainID3(child, childObs[[i]])
    }
  }
}

# predict function, which will route data through our tree 
# and make a prediction based on the leave where it ends up:
Predict <- function(tree, features) {
  if (tree$children[[1]]$isLeaf) return (tree$children[[1]]$name)
  child <- tree$children[[features[[tree$feature]]]]
  return ( Predict(child, features))
}


data(mushroom)
mushroom

# Create a data.tree Structure With Nodes
tree <- Node$new("mushroom")
tree

# Construindo a árvore
TrainID3(tree, mushroom)
tree

# imprimindo com mais detalhes
print(tree, "feature", "obsCount")

# previsão 
Predict(tree, c(color = 'red', size = 'large', points = 'yes'))

# continue explorando as opções em
# https://cran.r-project.org/web/packages/data.tree/vignettes/applications.html#id3-machine-learning



########################################
#
#   CART
#   https://cran.r-project.org/web/packages/rpart/vignettes/longintro.pdf
#   https://stats.stackexchange.com/questions/171574/meaning-of-surrogate-split
#
########################################
# conjunto de dados que está na distribuição base do R
attach(mtcars)
dados = mtcars
dados$country <- c(rep("Japan", 3), 
                   rep("US",4), rep("Europe", 7),
                   rep("US",3), "Europe", rep("Japan", 3),
                   rep("US",4), rep("Europe", 3), "US", 
                   rep("Europe", 3))

str(dados)

#  weights: pesos para as observações
#  subset: conjunto de treinamento
#  cost: pesos para as colunas. Utilizado para split
#  control: lista de opções (ver rpart.control)
# control = rpart.control(
#   minsplit       = 20,   # minimum number of observations required before split
#   minbucket      = 20/3, # minimum number of observations in any terminal node. deault = minsplit/3
#   cp             = 0.01, # complexity parameter used as the stopping rule  (alpha)
#   maxcompete     = 4,    # number of competitor splits retained in the output
#   maxsurrogate   = 5,    # number of surrogate splits retained in the output
#   usesurrogate   = 2,    # how to use surrogates in the splitting process  (2 = majority direction)
#   xval           = 10,   # number of cross-validations
#   surrogatestyle = 0,    # controls the selection of a best surrogate (0 = olha para o número total de classifcações corretas para escolher)
#   maxdepth       = 30)   # maximum depth of any node of the final tree
# 
#  OBS: 
#  surrogate:  para auxiliar quando tem NA
#  "No surrogate that does worse than “go with the majority” is printed or used"
#
#  maxdepth: Values greater than 30 rpart will give nonsense results on 32-bit machines.
rpart.control()

set.seed(123)
inTrain <- createDataPartition(dados$country, p = 3/4)[[1]]
model <- rpart(country ~., data = dados, subset = inTrain,
               control = list(minsplit = 5, minbucket = 2),
               parms = list(split = "information"))


# árvore e Previsões
rpart.plot(model, yesno = 2, tweak = 0.8)
inpred <- predict(model, dados[inTrain,], type = "class")
outpred <- predict(model, dados[-inTrain,], type = "class")
confusionMatrix(inpred, as.factor(dados$country[inTrain]))
confusionMatrix(outpred, as.factor(dados$country[-inTrain]))


# Exemplo com conjunto de treinamento:
# n <- nrow(dados)
# train <- sort(sample(1:n, floor(n/2)))
# model <- rpart(country ~., data = dados, subset = train)

print(model, digits = 2)

summary(model)
#  A rule of thumb is to choose the lowest level where
#        rel_error + xstd < xerror
# rel_error = erro relativo
# x-error = erro de cross-validation
# xstd = desvio padrão de cross-validation
model$cptable
cp = model$cptable; cp
cp[,3] + cp[,5] < cp[,4]
# para visualizar onde é o pondo ideal para poda:
plotcp(model)

# gráfico da árvore já podada
par(xpd = NA) # otherwise on some devices the text is clipped
plot(model)
text(model, digits = 3)
# ou...
rpart.plot(model, yesno = 2, tweak = 0.8)


# Previsões
inpred <- predict(model, dados[inTrain,], type = "class")
outpred <- predict(model, dados[-inTrain,], type = "class")
confusionMatrix(inpred, as.factor(dados$country[inTrain]))
confusionMatrix(outpred, as.factor(dados$country[-inTrain]))

# continue explorando as opções em
#  http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/141-cart-model-decision-tree-essentials/



########################################
#
#   Bagging
#
########################################
attach(mtcars)
dados = mtcars
dados$country <- as.factor(c(rep("Japan", 3), 
                             rep("US",4), 
                             rep("Europe", 7),
                             rep("US",3), "Europe",
                             rep("Japan", 3), rep("US",4), rep("Europe", 3), 
                             "US", rep("Europe", 3)))

# podemos ter variáveis contínuas e categóricas
dados$vs = factor(dados$vs, levels = c(0,1), labels = c("V-shaped", "straight"))
dados$am = factor(dados$am, levels = c(0,1), labels = c("automatic", "manual"))

#---------------------------------------------------------
# usando a função bagging() do pacote "ipred" diretamente
# chama a função rpart() do pacote "rpart"
#
# NOTA: o parâmetro "cp" no rpart é usado apenas 
#       para imprimir o output. O parâmetro de 
#       complexidade para a poda da árvore é 
#       escolhido por CV.
#----------------------------------------------------------
set.seed(123)
inTrain <- createDataPartition(dados$country, p = 3/4)[[1]]

modelo_bag <- bagging(country ~., 
                      data = dados[inTrain,], 
                      nbagg = 100, coob = TRUE, #nbagg numero de amostras bootstrap 
                      #coob é o out of bagg?
                      
                      control = rpart.control(minsplit = 2, cp = 0))

modelo_bag
names(modelo_bag)
modelo_bag$mtrees[1] #guarda toas as árvores

# previsão in sample e out-of-sample
clsIn <- predict(modelo_bag, newdata=dados[inTrain,])
clsOut <- predict(modelo_bag, newdata=dados[-inTrain,])

# matriz de confusão
table(dados$country[inTrain], clsIn)
table(dados$country[-inTrain], clsOut)#fazer com o confusiomatrix

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

########################################
#
#   Random Forest
#
########################################
# opçoes básicas:
modelo_rf = randomForest(country ~., data = dados, subset = inTrain,
                         ntree = 500, mtry = 6, importance = TRUE) # 6 numero de variáveis que vai ser selecionada em cada iteração
modelo_rf

names(modelo_rf) #importance é a importancia das variáveis
# previsão in sample e out-of-sample
rfIn <- predict(modelo_rf, newdata=dados[inTrain,])
rfOut <- predict(modelo_rf, newdata=dados[-inTrain,])

# matriz de confusão
table(dados$country[inTrain], rfIn)
table(dados$country[-inTrain], rfOut)

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


########################################
#
#   Boosting
#   utils::browseVignettes("gbm")
#
########################################
# shrinkage = learning rate
nt = 10
modelo_bos = gbm(country ~., data = dados[inTrain,],
                 n.trees = 10, shrinkage = 0.01,
                 n.minobsinnode = 2, #numero minimo dentro do nó
                 interaction.depth = 4) #cada árvore vai a 4 níveis
summary(modelo_bos)

predIn = predict(modelo_bos, newdata = dados[inTrain,], n.trees = 10, type = "response")
predIn.class = apply(predIn[,,1], 1, which.max)
predIn.class = factor(predIn.class, levels = 1:3, labels = levels(dados$country))
confusionMatrix(predIn.class, dados$country[inTrain])

pred = predict(modelo_bos, newdata = dados[-inTrain,], n.trees = 10, type = "response")
pred.class = apply(pred[,,1], 1, which.max)
pred.class = factor(pred.class, levels = 1:3, labels = levels(dados$country))
confusionMatrix(pred.class, dados$country[-inTrain])


# prediction
n.trees = seq(from = 1, to = nt, length.out = 10)
predmat = predict(modelo_bos, newdata = dados[-inTrain,], n.trees = n.trees, type = "response")

predmat.Class = data.frame(matrix(ncol = length(n.trees), nrow = nrow(dados[-inTrain,])))
colnames(predmat.Class) = n.trees
for(i in 1:length(n.trees)){ 
  predmat.Class[,i] = apply(predmat[,,i], 1, which.max)
  predmat.Class[,i] = factor(predmat.Class[,i], levels = 1:3, labels = levels(dados$country))
}

boost.err = apply(predmat.Class, 2, function(p) mean(dados$country[-inTrain] != p) )
plot(n.trees, boost.err, pch = 23, ylab = "Error Rate", xlab = "# Trees", main = "Boosting Test Error")



# This dataset consists of data on 32 models of car, taken from an American motoring magazine (1974 Motor Trend magazine). For each car, you have 11 features, expressed in varying units (US units), They are as follows:
#   
# mpg: Fuel consumption (Miles per (US) gallon): more powerful and heavier cars tend to consume more fuel.
# 
# cyl: Number of cylinders: more powerful cars often have more cylinders
# 
# disp: Displacement (cu.in.): the combined volume of the engine's cylinders
# 
# hp: Gross horsepower: this is a measure of the power generated by the car
# 
# drat: Rear axle ratio: this describes how a turn of the drive shaft corresponds to a turn of the wheels. Higher values will decrease fuel efficiency.
# 
# wt: Weight (1000 lbs): pretty self-explanatory!
# 
# qsec: 1/4 mile time: the cars speed and acceleration
# 
# vs: Engine block: this denotes whether the vehicle's engine is shaped like a "V", or is a more common straight shape.
# 
# am: Transmission: this denotes whether the car's transmission is automatic (0) or manual (1).
# 
# gear: Number of forward gears: sports cars tend to have more gears.
# 
# carb: Number of carburetors: associated with more powerful engines
# 
# Note that the units used vary and occupy different scales.



# Boosting - Regression
attach(mtcars)
dados = mtcars
with(dados, plot(hp, mpg))

nt = 10000
modelo_bos = gbm(mpg ~ hp, data = dados,
                 n.trees = nt, shrinkage = 0.01,
                 n.minobsinnode = 2,
                 interaction.depth = 4)

n.trees = c(1, 10, 50, 100, 1000, 10000)
predmat = predict(modelo_bos, newdata = dados, n.trees = n.trees, type = "response")


plot(dados$hp, dados$mpg, pch = 19, cex = 2)
for(i in 1:ncol(predmat)){
  points(dados$hp, predmat[,i], col = 7-i, pch = paste(i), cex = 1)
  Sys.sleep(1)
}
