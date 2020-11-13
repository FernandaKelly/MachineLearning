############################################## PACOTES UTILIZADOS ########################################################################

library(caret)     
library(data.tree)
library(rpart)      
library(rpart.plot)

##########################################################################################################################################




########################################
# Função para verificar se um conjunto é puro
# a classe deve estar na última coluna

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




########################################################################################################################################

#ASSUMA QUE OS RECURSOS DE CLASSIFICAÇÃO SE ENCONTRAM NAS COLUNAS 1 A N-1



TrainID3 <- function(node, data) {
  
  node$obsCount <- nrow(data)
  
  
  if (IsPure(data)) {
    
    child <- node$AddChild(unique(data[,ncol(data)]))
    node$feature <- tail(names(data), 1)
    child$obsCount <- nrow(data)
    child$feature <- ''
  } else {
    
    ig <- sapply(colnames(data)[-ncol(data)], 
                 function(x) InformationGain(
                   table(data[,x], data[,ncol(data)])
                 )
    )
    
    feature <- names(which.max(ig))
    node$feature <- feature
    
    
    childObs <- split(data[ ,names(data) != feature, drop = FALSE], 
                      data[ ,feature], 
                      drop = TRUE)
    
    for(i in 1:length(childObs)) {
      
      child <- node$AddChild(names(childObs)[i])
      
      
      TrainID3(child, childObs[[i]])
    }
  }
}




Predict <- function(tree, features) {
  if (tree$children[[1]]$isLeaf) return (tree$children[[1]]$name)
  child <- tree$children[[features[[tree$feature]]]]
  return ( Predict(child, features))
}


#dados do pacote data.tree
data(mushroom)
mushroom


tree <- Node$new("mushroom")
tree


TrainID3(tree, mushroom)
tree


print(tree, "feature", "obsCount")


Predict(tree, c(color = 'red', size = 'large', points = 'yes'))
