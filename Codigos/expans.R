ak=function(x){1/(1+exp(-x))}

# pdf(file="plot10.pdf", width = 6, height = 6)
# curve(ak, -10,10, lwd=2, main="função sigmóide")
# abline(v=0,col="red", lty=2,lwd=2)
# dev.off()

set.seed(232)
x=sort(runif(100,-5,5))
y=6*ak(x)+rnorm(100, sd=0.7)
plot(x,y)


# Regiões
u1=-2
u2=2

# Dummies
I1 = as.integer(x < u1)
I2 = as.integer(x >= u1 & x < u2)
I3 = as.integer(x >= u2)

modelo = lm(y ~ -1+ I1 + I2 + I3)
summary(modelo)
plot(x, y)
lines(x, fitted(modelo), col = "red", lwd = 3)

#linear por partes, MQO por partes, não é continuo e atrapalha derivadas, ex. 2
I = cbind(I1, I2, I3)
XI = x*I
modelo2 = lm(y ~ -1 + I + XI)
summary(modelo2)
plot(x, y)
lines(x[which(I1==1)], fitted(modelo2)[which(I1==1)], col = "red", lwd = 3)
lines(x[which(I2==1)], fitted(modelo2)[which(I2==1)], col = "blue", lwd = 3)
lines(x[which(I3==1)], fitted(modelo2)[which(I3==1)], col = "green", lwd = 3)

#regressão quadratica por partes
X2I = x^2*I
modelo3 = lm(y ~ -1 + I + X2I)
summary(modelo3)
plot(x, y)
lines(x[which(I1==1)], fitted(modelo3)[which(I1==1)], col = "red", lwd = 3)
lines(x[which(I2==1)], fitted(modelo3)[which(I2==1)], col = "blue", lwd = 3)
lines(x[which(I3==1)], fitted(modelo3)[which(I3==1)], col = "green", lwd = 3)

#cubica por partes
X3I = x^3*I
modelo4 = lm(y ~ -1 + I + X2I+ X3I)
summary(modelo4)

lines(x[which(I1==1)], fitted(modelo4)[which(I1==1)], col = "red", lwd = 3, lty=2)
lines(x[which(I2==1)], fitted(modelo4)[which(I2==1)], col = "blue", lwd = 3, lty=2)
lines(x[which(I3==1)], fitted(modelo4)[which(I3==1)], col = "green", lwd = 3, lty=2)


### Base de splines

library(splines)

#cubico
s = bs(x, knots=c(-2,2), degree=4)
modelo5 = lm(y ~ s)
plot(x, y)
lines(x,predict(modelo5), lwd=2, col="red")

# Localização dos nós via quantis.
s1 = bs(x, degree=1, df = 8)
attr(s1,"knots")
modelo6 = lm(y ~ s1)
plot(x,predict(modelo6), lwd=2)
abline(v=attr(s1,"knots"), lty=2, col="red")
attr(s1,"knots")


########################################################
#### Previsão usando splines, inclusive fora da amostra!
########################################################


# predição dos valores da base para os pontos a serem preditos
a = predict(s1, newx = c(seq(-6,-4,length.out = 200),seq(4,6,length.out = 200))) 

# predição dos valores de y
b = modelo6$coefficients[1] + modelo6$coefficients[-1]%*%t(a)

plot(x, y, xlim=c(-6,6), ylim=c(-1.5,8.3))
lines(seq(-6,6,length.out = 12),6*ak(seq(-6,6,length.out = 12)), lwd=2,lty=2) #verdadeira
lines(x,predict(modelo6), lwd=2, col="red") #predito
lines(seq(-6,-4,length.out = 12),b[1:12], col="blue") #previsão
lines(seq(4,6,length.out = 12),b[13:24], col="blue") # previsão

##############################################################
## Será que conseguimos ganhar dinheiro na bolsa com splines?
##############################################################

cl = read.csv(file = "BTC-USD.csv", header=T)$Adj.Close

# reservando os últimos 20 valores para previsão
ycl = cl[1:346]
plot.ts(ycl)
ty = 1:346
prev.cl = cl[347:366]

sg1 = bs(ty, degree=1, df = 10)
modelo7 = lm(ycl ~ sg1)
lines(ty,predict(modelo7), lwd=2, lty=2, col="blue")



sg2 = bs(ty, degree=2, df=8)
modelo8 = lm(ycl ~ sg2)
plot.ts(ycl)
lines(ty,predict(modelo8), lwd=2, lty=2, col="red")


sg3  = bs(ty, degree=4, df=25)
modelo9 = lm(ycl ~ sg3)
plot.ts(ycl)
lines(ty,predict(modelo9), lwd=2, lty=4, col="magenta")

sg4  = bs(ty, degree=5, df=40)
modelo10 = lm(ycl ~ sg4)
plot.ts(ycl)
lines(ty,predict(modelo10), lwd=2, lty=4, col="darkblue")


a = predict(sg4, newx = 347:366)

# predição dos valores de y
b = modelo10$coefficients[1] + modelo10$coefficients[-1]%*%t(a)

plot.ts(ycl, xlim=c(0,366))
lines(347:366,cl[347:366], col="red")
abline(v=347, lty=2)
lines(ty,predict(modelo10), lwd=2, lty=2, col="blue")
lines(347:366,b, col="darkgreen")


a = predict(sg1, newx = 347:366)

# predição dos valores de y
b = modelo7$coefficients[1] + modelo7$coefficients[-1]%*%t(a)

plot.ts(ycl, xlim=c(0,366))
lines(347:366,cl[347:366], col="red")
abline(v=347, lty=2)
lines(ty,predict(modelo7), lwd=2, lty=2, col="blue")
lines(347:366,b, col="darkgreen")



###################################
# Nadaraya-Watson com Epanechnikov 
###################################

epa.ker=function(x,x0,lambda){
  tp = abs(x-x0)/lambda
  ifelse(abs(tp)<=1, 3/4*(1-tp^2),0)
}

NW.epa=function(x0, xs,ys,lambda){
  lun = numeric(length(x0))
  for(i in 1:length(x0)){
    w = epa.ker(x0[i],xs,lambda)  
    lun[i] = 1/sum(w)*sum(w*ys)
  }
  return(lun)
}
set.seed(120)
x=runif(100)
y=sin(12*(x+0.2))+rnorm(100)

# problemático para lambdas muito pequenos
grid=seq(0,1,length.out = 1000)
plot(x,y)
lines(grid,sin(12*(grid+0.2)))
lines(grid,NW.epa(grid,x,y,2), col="red")
lines(grid,NW.epa(grid,x,y,1), col="red")
lines(grid,NW.epa(grid,x,y,0.3), col="red")

plot(x,y, pch=16)
lines(grid,sin(12*(grid+0.2)), lwd=2)
lines(grid,NW.epa(grid,x,y,0.1), lwd=2, col="red")
lines(grid,NW.epa(grid,x,y,0.05), lwd=2, col="green")
lines(grid,NW.epa(grid,x,y,0.04), lwd=2, col="blue")
lines(grid,NW.epa(grid,x,y,0.03), lwd=2, col="magenta")

# problemático com lambdas muito pequenos
plot(x,y, pch=16)
lines(grid,sin(12*(grid+0.2)), lwd=2)
lines(grid,NW.epa(grid,x,y,0.02), lwd=2, col="red")


##########################
## Kernel Tri-cúbico
##########################

tricub.ker=function(x,x0,lambda){
  tp = abs(x-x0)/lambda
  ifelse(abs(tp)<=1, (1-abs(tp)^3)^3,0)
}

NW.tricub=function(x0, xs,ys,lambda){
  lun = numeric(length(x0))
  for(i in 1:length(x0)){
    w = tricub.ker(x0[i],xs,lambda)  
    lun[i] = 1/sum(w)*sum(w*ys)
  }
  return(lun)
}


plot(x,y)
lines(grid,sin(12*(grid+0.2)))
lines(grid,NW.tricub(grid,x,y,2), col="red")
lines(grid,NW.tricub(grid,x,y,1), col="red")
lines(grid,NW.tricub(grid,x,y,0.3), col="red")

plot(x,y, pch=16)
lines(grid,sin(12*(grid+0.2)), lwd=2)
lines(grid,NW.tricub(grid,x,y,0.1), lwd=2, col="red")
lines(grid,NW.tricub(grid,x,y,0.05), lwd=2, col="green")
lines(grid,NW.tricub(grid,x,y,0.04), lwd=2, col="blue")
lines(grid,NW.tricub(grid,x,y,0.03), lwd=2, col="magenta")

# problemático com lambdas muito pequenos
plot(x,y, pch=16)
lines(grid,sin(12*(grid+0.2)), lwd=2)
lines(grid,NW.tricub(grid,x,y,0.02), lwd=2, col="red")
lines(grid,NW.epa(grid,x,y,0.02), lwd=2, col="magenta")


##########################
#### Kernel Gaussiano
##########################


g.ker=function(x,x0,lambda){
  return(dnorm(abs(x-x0)/lambda))
}

NW.g=function(x0, xs,ys,lambda){
  lun = numeric(length(x0))
  for(i in 1:length(x0)){
    w = g.ker(x0[i],xs,lambda)  
    lun[i] = 1/sum(w)*sum(w*ys)
  }
  return(lun)
}


plot(x,y, pch=16)
lines(grid,sin(12*(grid+0.2)))
lines(grid,NW.g(grid,x,y,2), col="red")
lines(grid,NW.g(grid,x,y,1), col="red")
lines(grid,NW.g(grid,x,y,0.3), col="red")

plot(x,y, pch=16)
lines(grid,sin(12*(grid+0.2)), lwd=2)
lines(grid,NW.g(grid,x,y,0.1), lwd=2, col="red")
lines(grid,NW.g(grid,x,y,0.05), lwd=2, col="green")
lines(grid,NW.g(grid,x,y,0.04), lwd=2, col="blue")
lines(grid,NW.g(grid,x,y,0.03), lwd=2, col="magenta")

#  lambdas muito pequenos já não são problemas
plot(x,y, pch=16)
lines(grid,sin(12*(grid+0.2)), lwd=2)
lines(grid,NW.g(grid,x,y,0.02), lwd=2, col="red")
lines(grid,NW.g(grid,x,y,0.01), lwd=2, col="blue")
lines(grid,NW.g(grid,x,y,0.005), lwd=2, col="magenta")


#################################
# E vamos ganhar dinheiro com NW?
#################################

plot.ts(ycl)
lambda=10
lines(ty,NW.g(ty,ty,ycl,lambda), lwd=2, lty=2, col="green")
lambda=5
lines(ty,NW.g(ty,ty,ycl,lambda), lwd=2, lty=2, col="blue")
lambda=2
lines(ty,NW.g(ty,ty,ycl,lambda), lwd=2, lty=2, col="red")



lambda=10
plot.ts(ycl, xlim=c(0,366))
lines(347:366,cl[347:366], col="red")
abline(v=347, lty=2)
lines(ty,NW.g(ty,ty,ycl,lambda), lwd=2, lty=2, col="blue")
lines(347:366,NW.g(347:366,ty,ycl,lambda), col="darkgreen")



##################################
# Utilizando local regression para analisar os dados do Bitcoin
##################################

modelo11 = loess(ycl~ty, span = 0.5, degree=1)
modelo12 = loess(ycl~ty, span = 0.25, degree=1)
modelo13 = loess(ycl~ty, span = 0.15, degree=1)
modelo14 = loess(ycl~ty, span = 0.1, degree=1)
plot.ts(ycl)
lines(ty,predict(modelo11), lwd=2, lty=4, col="blue")
lines(ty,predict(modelo12), lwd=2, lty=4, col="red")
lines(ty,predict(modelo13), lwd=2, lty=4, col="darkgreen")
lines(ty,predict(modelo14), lwd=2, lty=4, col="magenta")


