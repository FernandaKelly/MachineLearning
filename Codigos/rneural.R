#https://medium.com/datathings/neural-networks-and-backpropagation-explained-in-a-simple-way-f540a3611f5e







library(caret)

attach(mtcars)

mtcars$country=mtcars.country <- c(rep("Japan", 3), rep("US",4), 
                                   rep("Europe", 7),rep("US",3), 
                                   "Europe", rep("Japan", 3), 
                                   rep("US",4), rep("Europe", 3), 
                                   "US", rep("Europe", 3))

set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model


inTrain <- createDataPartition(mtcars$country, p = 3/4)[[1]]
training <- mtcars[inTrain,]
testing <- mtcars[-inTrain,]

numFolds <- trainControl(method = 'cv', number = 10)
fit2 <- train(country ~ . , data = training, method = 'nnet', #nnet pra usar cross validation
              preProcess = c('center', 'scale'), trControl = numFolds, #preProcess pra normalizar
              tuneGrid=expand.grid(size=c(10), decay=c(0.1))) #SIZE numero de camadas, decay é taxa de aprendizagem


results1 <- predict(fit2, newdata=training)
conf1 <- confusionMatrix(results1, as.factor(training$country))

results2 <- predict(fit2, newdata=testing)
conf2 <- confusionMatrix(results2, as.factor(testing$country))

probs <- predict(fit2, newdata=testing, type='prob')





##### Combinação de técnicas também vale :)


dados=mtcars[,-c(8,9)]
mtcars.pca <- prcomp(dados[,-10], center = TRUE,scale. = TRUE) #pca
summary(mtcars.pca)

chunk = data.frame(mtcars.pca$x[,1:3], country=mtcars.country)#country é a resposta

ctrain=chunk[inTrain,]
ctest=chunk[-inTrain,]

fit1 <- train(country ~ . , data = ctrain, method = 'nnet', preProcess = c('center', 'scale'),
              trControl = numFolds, tuneGrid=expand.grid(size=c(10), decay=c(0.1)))

results3 <- predict(fit1, newdata=ctrain)
conf3 <- confusionMatrix(results3, as.factor(ctrain$country))

results4 <- predict(fit1, newdata=ctest)
conf4 <- confusionMatrix(results4, as.factor(ctest$country))

probs <- predict(fit1, newdata=ctest, type='prob')



### Repetindo tudo com apenas duas dimensões do PCA

chunk = data.frame(mtcars.pca$x[,1:2], country=mtcars.country)

ctrain2=chunk[inTrain,]
ctest2=chunk[-inTrain,]

fit3 <- train(country ~ . , data = ctrain2, method = 'nnet', preProcess = c('center', 'scale'), trControl = numFolds, 
              tuneGrid=expand.grid(size=c(10), decay=c(0.1)))

results5 <- predict(fit3, newdata=ctrain2)
conf5 <- confusionMatrix(results5, as.factor(ctrain2$country))

results6 <- predict(fit3, newdata=ctest2)
conf6 <- confusionMatrix(results6, as.factor(ctest2$country))

probs <- predict(fit3, newdata=ctest2, type='prob')




##################################################

