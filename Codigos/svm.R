# gerando dados para o problema de duas classes
set.seed(1)
x = matrix(rnorm(20*2), ncol =2)
y = c(rep(-1 ,10), rep(1 ,10))

# geralmente esses dados não são linearmente separáveis....
plot(x, col = (3-y))

# forçando a separação para o exemplo:
x[y == 1,] = x[y == 1,] + 2
plot(x, col = (3-y))


dat = data.frame(x2 = x[,2], x1 = x[,1],  y = as.factor( y ))
head(dat)
library (e1071)
svmfit = svm(y~., data = dat, kernel ="linear", cost = 10, scale = FALSE )
summary(svmfit)

# limites de decisão:
plot(svmfit, dat, grid = 100, svSymbol = "X", dataSymbol = 19)

# indice dos support vectors:
svmfit$index

# betas:
beta0 = svmfit$rho
betas = c(svmfit$coefs)%*%as.matrix(dat[svmfit$index,1:2])
cbind(intercept = beta0, betas)

# matrix de confusão:
table(predict(svmfit), y)


#########################
#
#   3 classes
#
#########################
# gerando dados
set.seed(1)
x = rbind(x, matrix (rnorm (50*2), ncol =2))
y = c (y, rep (0 ,50))
plot(x, col = as.factor(y))

# separando um pouco os dados
x[y == 0,2] = x[y == 0,2] + 2
plot(x, col = as.factor(y))

dat = data.frame(x2  = x[,2], x1 = x[,1],  y = as.factor(y))

# kernel linear: u'*v
svmfit.l = svm(y~. ,data = dat , kernel ="linear" , cost =10 , gamma =1)
plot(svmfit.l, dat, grid = 200)

# kernel polinomial: (gamma*u'*v + coef0)^degree
svmfit.p = svm(y~. ,data = dat , kernel ="polynomial", cost =10, coef0 = 1, gamma = 1, degree = 3)
plot(svmfit.p, dat, grid = 200)

# kernel bases radiais: exp(-gamma*|u-v|^2)
svmfit.r = svm(y~. ,data = dat , kernel = "radial", cost =10 , gamma = 1)
plot(svmfit.r, dat, grid = 200)

# kernel sigmoid: tanh(gamma*u'*v + coef0)
svmfit.s = svm(y~. ,data = dat , kernel ="sigmoid", cost =10, coef0 = 0, gamma = 1)
plot(svmfit.s, dat, grid = 200)

