############################################## PACOTES UTILIZADOS ########################################################################

library(caret)     
library(data.tree)
# CART
library(rpart)      
library(rpart.plot)

##########################################################################################################################################




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

########################################





########################################
#BASE DE DADOS DO PACOTE DATASETS

attach(mtcars)
dados = mtcars
dados$country <- c(rep("Japan", 3), 
                   rep("US",4), 
                   rep("Europe", 7),
                   rep("US",3), 
                   "Europe", 
                   rep("Japan", 3), 
                   rep("US",4), 
                   rep("Europe", 3), 
                   "US", 
                   rep("Europe", 3))

str(dados)


dim(dados)


########################################



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



########################################


#REVER AS FUNÇÕES DO R.PART.CONTROL

rpart.control()

#posso controlar os pesos das variáveis 
#cp é igual ao alpha
#xval é a vaidação cruzada



########################################


set.seed(123)  #SEMENTE PRA NÃO ALTERAR O RESULTADO
inTrain <- createDataPartition(dados$country, p = 3/4)[[1]] #SEPARANDO 3/4 DOS DADOS PARA TREINO

model <- rpart(country ~., data = dados, subset = inTrain,
               control = list(minsplit = 5, minbucket = 3), #pesos de quebra é minsplit
               parms = list(split = "information")) #infomration ou gini


# ÁRVORE E PREVISÕES
rpart.plot(model, yesno = 2, tweak = 0.8)

inpred <- predict(model, dados[inTrain,], type = "class")
outpred <- predict(model, dados[-inTrain,], type = "class")

confusionMatrix(inpred, as.factor(dados$country[inTrain]))
confusionMatrix(outpred, as.factor(dados$country[-inTrain]))


#OBS.: O MODELO NA BASE DE TESTE OBETEVE UMA ACURÁCIA DE 57% (O QUE NÃO É BOM, PODEMOS MELHORAR)



########################################

#MELHORANDO O MODELO



# Exemplo com conjunto de treinamento:
# n <- nrow(dados)
# train <- sort(sample(1:n, floor(n/2)))
# model <- rpart(country ~., data = dados, subset = train)


print(model, digits = 2) #imprimi a ávore
summary(model)


# informações do ajuste da validação cruzada
#  A rule of thumb is to choose the lowest level where
#        rel_error + xstd < xerror
# rel_error = erro relativo
# x-error = erro de cross-validation
# xstd = desvio padrão de cross-validation


model$cptable
cp = model$cptable; 
cp


cp[,3] + cp[,5] < cp[,4] # para visualizar onde é o pondo ideal para poda:
plotcp(model)



#gráfico da árvore já podada

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







