# Tarefa 2
# Desenvolvedor: Mathias Giordani Titton

############################# Inicialização ################################
# Bibliotecas:
library(glmnet)
library(ggplot2)
library(ggpubr)

# Código para utilizar todos os núcleos do processador. 
library(doParallel)
nucleos = 4                                     # Informar número de núcleos virtuais (informações do sistema -> processador).
cl = makePSOCKcluster(nucleos)
registerDoParallel(cl)

# Parâmetros iniciais:
seed_ini = 1234     # Semente que será utilizada.
rep = 1000          # Número de repetições do experimento.
set.seed(seed_ini)  # Fixa semente do algoritmo.
n = 50              # Número de dados.
ncol = 100          # Colunas das variáveis independentes X1...X100.
nrow = n            # Número de linhas da matriz (Número de dados).

############################# Exercício 1 #################################

# 1) Gere uma amostra de tamanho n = 50 do modelo (1):
# Gera amostras de X1...X100 independentes com Xi ~ N(0,sqrt(i))
# Gera o modelo Y = 20+5X1+5X10+5X20+5X50+5X90+eps, com eps ~ N(0,1)
# 2) Ajuste um modelo LASSO aos dados simulados utilizando k-fold CV, para k pertencente a {5, 10}.
# 3) Repita os passos acima 1000 vezes.

#****************************** k = 5 ***********************************# 
kfold = 5
coeficientes_k5 = matrix(ncol = ncol+1, nrow = rep)
lambdaotimo_k5 = matrix(ncol = 1, nrow = rep)
var_exist_k5 = matrix(ncol = 1, nrow = rep)
var_total_k5 = matrix(ncol = 1, nrow = rep)

for(r in 1:rep){
  # Modifica a semente a cada iteração.
  set.seed(seed_ini+r-1)                                              
  
  # 1) Gera amostras de tamanho n = 50 de X1...X100 independentes com Xi ~ N(0,sqrt(i))
  X = matrix(NA, ncol = ncol, nrow = nrow)                          # Inicializa a matriz com dados "vazios".
  for(i in 1:ncol) X[,i] = rnorm(nrow, mean = 0, sd = sqrt(i))      # Gera as amostras.
  
  # 2) Gera o modelo Y = 20+5X1+5X10+5X20+5X50+5X90+eps, com eps ~ N(0,1)
  beta = rep(0,ncol)                                                # Inicializa beta com valores nulos.
  beta[c(1,10,20,50,90)] = rep(5,5)                                 # Atribui os valores de beta que multiplicam Xi.
  eps = rnorm(nrow, mean = 0, sd = 1)                               # Distribuição normal com n dados e desvio padrão igual a 1.
  Y = 20+X%*%beta+eps                                               # Modelo.
  
  validacaoCruzada = cv.glmnet(X, Y, alpha = 1, nfold = kfold)      # alpha = 0 -> Ridge, alpha = 1 -> LASSO.
  lambdaOtimo = validacaoCruzada$lambda.min                         # Adquire o lambda ótimo que minimiza o erro.
  ajuste.otimo = glmnet(X, Y, alpha=1, lambda = lambdaOtimo)        # Verifica qual o ajuste ótimo para o lambda ótimo.
  cf = coef(ajuste.otimo)                                           # Adquire os coeficientes do LASSO.
  nm = rownames(cf)[cf[,1] != 0]                                    # Nomeia os labels para os coeficientes não-nulos.
  vars = paste("V",c(1,10,20,50,90), sep = "")                      # Organiza labels.
  num_vars = sum(vars %in% nm)                                      # Número de variáveis que o modelo encontrou dentro do cj.                                        
  
  coeficientes_k5[r,] = t(cf[,1])                                   # Adquire os coeficientes transpostos.
  lambdaotimo_k5[r,] = lambdaOtimo                                  # Valor de lambda ótimo para o ajuste do LASSO.
  var_exist_k5[r,] = num_vars                                       # Número de variáveis que foram selecionadas pelo LASSO e que existem no modelo original.
  var_total_k5[r,] = length(nm)-1                                   # Desconta o intercept (beta0).
}
colnames(coeficientes_k5) <- names(cf[,1])                          # Ajusta os nomes das colunas da matriz de coeficientes.

# Variáveis auxiliares:
soma_correta_k5 = 0 
soma_presente_k5 = 0
aux = 1
aux2 = 1
ind_correto_k5 = 0
ind_incorreto_k5 = 0
for (i in 1:rep)
{
  # LASSO selecionou somente as variáveis do modelo:
  if ((var_exist_k5[i,] == 5) & (var_total_k5[i,] == 5)){
    soma_correta_k5 = soma_correta_k5+1
    ind_correto_k5[aux] = i
    aux = aux+1
  }
  else{
    ind_incorreto_k5[aux2] = i   
    aux2 = aux2+1
  }
  
  # LASSO selecionou variáveis a mais que o modelo, porém as variáveis do modelo estavam coletivamente presentes:
  if (var_exist_k5[i,] == 5){
    soma_presente_k5 = soma_presente_k5+1
  }
}
SelecaoCorreta_k5 = soma_correta_k5                                 # Contagem de quantas vezes o LASSO acertou o modelo.
SelecaoPresente_k5 = soma_presente_k5                               # Contagem de quantas vezes o modelo estava presente nos coeficientes encontrados pelo LASSO.




#****************************** k = 10 ***********************************# 
kfold = 10
coeficientes_k10 = matrix(ncol = ncol+1, nrow = rep)
lambdaotimo_k10 = matrix(ncol = 1, nrow = rep)
var_exist_k10 = matrix(ncol = 1, nrow = rep)
var_total_k10 = matrix(ncol = 1, nrow = rep)

for(r in 1:rep){
  # Modifica a semente a cada iteração.
  set.seed(seed_ini+r-1)                                              
  
  # 1) Gera amostras de tamanho n = 50 de X1...X100 independentes com Xi ~ N(0,sqrt(i))
  X = matrix(NA, ncol = ncol, nrow = nrow)                          # Inicializa a matriz com dados "vazios".
  for(i in 1:ncol) X[,i] = rnorm(nrow, mean = 0, sd = sqrt(i))      # Gera as amostras.
  
  # 2) Gera o modelo Y = 20+5X1+5X10+5X20+5X50+5X90+eps, com eps ~ N(0,1)
  beta = rep(0,ncol)                                                # Inicializa beta com valores nulos.
  beta[c(1,10,20,50,90)] = rep(5,5)                                 # Atribui os valores de beta que multiplicam Xi.
  eps = rnorm(nrow, mean = 0, sd = 1)                               # Distribuição normal com n dados e desvio padrão igual a 1.
  Y = 20+X%*%beta+eps                                               # Modelo.
  
  validacaoCruzada = cv.glmnet(X, Y, alpha = 1, nfold = kfold)      # alpha = 0 -> Ridge, alpha = 1 -> LASSO.
  lambdaOtimo = validacaoCruzada$lambda.min                         # Adquire o lambda ótimo que minimiza o erro.
  ajuste.otimo = glmnet(X, Y, alpha=1, lambda = lambdaOtimo)        # Verifica qual o ajuste ótimo para o lambda ótimo.
  cf = coef(ajuste.otimo)                                           # Adquire os coeficientes do LASSO.
  nm = rownames(cf)[cf[,1] != 0]                                    # Nomeia os labels para os coeficientes não-nulos.
  vars = paste("V",c(1,10,20,50,90), sep = "")                      # Organiza labels.
  num_vars = sum(vars %in% nm)                                      # Número de variáveis que o modelo encontrou dentro do cj.                                        
  
  coeficientes_k10[r,] = t(cf[,1])                                  # Adquire os coeficientes transpostos.
  lambdaotimo_k10[r,] = lambdaOtimo                                 # Valor de lambda ótimo para o ajuste do LASSO.
  var_exist_k10[r,] = num_vars                                      # Número de variáveis que foram selecionadas pelo LASSO e que existem no modelo original.
  var_total_k10[r,] = length(nm)-1                                  # Desconta o intercept (beta0).
}
colnames(coeficientes_k10) <- names(cf[,1])                         # Ajusta os nomes das colunas da matriz de coeficientes.

# Variáveis auxiliares:
soma_correta_k10 = 0 
soma_presente_k10 = 0
aux = 1
aux2 = 1
ind_correto_k10 = 0
ind_incorreto_k10 = 0
for (i in 1:rep)
{
  # LASSO selecionou somente as variáveis do modelo:
  if ((var_exist_k10[i,] == 5) & (var_total_k10[i,] == 5)){
    soma_correta_k10 = soma_correta_k10+1
    ind_correto_k10[aux] = i
    aux = aux+1
  }
  else{
    ind_incorreto_k10[aux2] = i   
    aux2 = aux2+1
  }
  
  # LASSO selecionou variáveis a mais que o modelo, porém as variáveis do modelo estavam coletivamente presentes:
  if (var_exist_k10[i,] == 5){
    soma_presente_k10 = soma_presente_k10+1
  }
}
SelecaoCorreta_k10 = soma_correta_k10                               # Contagem de quantas vezes o LASSO acertou o modelo.
SelecaoPresente_k10 = soma_presente_k10                             # Contagem de quantas vezes o modelo estava presente nos coeficientes encontrados pelo LASSO.


# Questão (a):
SelecaoCorreta = c(SelecaoCorreta_k5,SelecaoCorreta_k10)
vetor_k = c(5,10)
dados = data.frame(vetor_k,SelecaoCorreta)
colnames(dados) = c("kfold","Frequência")
p<- ggplot(data=dados, aes(x=kfold, y=Frequência)) +
  geom_bar(stat="identity",width=2.5,color="black",fill="steelblue", position = position_dodge(width=20))+
  geom_text(aes(label=Frequência), vjust=1.6, color="white", size=3.5)+theme_minimal()+ scale_x_discrete(limits = c(5,10))+scale_y_continuous(breaks = seq(0, rep, rep/5), lim = c(0, rep))
p

# Questão (b):
SelecaoPresente = c(SelecaoPresente_k5,SelecaoPresente_k10)
vetor_k = c(5,10)
dados = data.frame(vetor_k,SelecaoPresente)
colnames(dados) = c("kfold","Frequência")
p<- ggplot(data=dados, aes(x=kfold, y=Frequência)) +
  geom_bar(stat="identity",width=2.5,color="black",fill="steelblue", position = position_dodge(width=20))+
  geom_text(aes(label=Frequência), vjust=1.6, color="white", size=3.5)+theme_minimal()+ scale_x_discrete(limits = c(5,10))+scale_y_continuous(breaks = seq(0, rep, rep/5), lim = c(0, rep))
p

# Questão (c): Explicativa.

# Questão (d):
#coeficientes_k5[(ind_correto_k5),1]   # X0
#coeficientes_k5[(ind_correto_k5),2]   # X1
#coeficientes_k5[(ind_correto_k5),11]  # X10
#coeficientes_k5[(ind_correto_k5),21]  # X20
#coeficientes_k5[(ind_correto_k5),51]  # X50
#coeficientes_k5[(ind_correto_k5),91]  # X90
dados = data.frame(coeficientes_k5[(ind_correto_k5),1],
                   coeficientes_k5[(ind_correto_k5),2],
                   coeficientes_k5[(ind_correto_k5),11],
                   coeficientes_k5[(ind_correto_k5),21],
                   coeficientes_k5[(ind_correto_k5),51],
                   coeficientes_k5[(ind_correto_k5),91])


colnames(dados) = c("X0","X1","X10","X20","X50","X90")
p1<- ggplot(dados, aes(x=1, y=X0,fill = as.factor(1)))+
  geom_boxplot() +
  theme_bw() + 
  ylab(" ")+xlab("X0")+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_y_continuous(lim = c(19, 21))+ theme(legend.position = "none")+scale_fill_manual(values=c("#E69F00"))

p2<- ggplot(dados, aes(x=1, y=X1,fill = as.factor(1)))+
  geom_boxplot() +
  theme_bw() + 
  ylab(" ")+xlab("X1")+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_y_continuous(lim = c(3, 5.5))+ theme(legend.position = "none")+scale_fill_manual(values=c("#3266A8"))

p3<- ggplot(dados, aes(x=1, y=X10,fill = as.factor(1)))+
  geom_boxplot() +
  theme_bw() + 
  ylab(" ")+xlab("X10")+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_y_continuous(lim = c(3, 5.5))+ theme(legend.position = "none")+scale_fill_manual(values=c("#C70039"))

p4<- ggplot(dados, aes(x=1, y=X20,fill = as.factor(1)))+
  geom_boxplot() +
  theme_bw() + 
  ylab(" ")+xlab("X20")+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_y_continuous(lim = c(3, 5.5))+ theme(legend.position = "none")+scale_fill_manual(values=c("#54C358"))

p5<- ggplot(dados, aes(x=1, y=X50,fill = as.factor(1)))+
  geom_boxplot() +
  theme_bw() + 
  ylab(" ")+xlab("X50")+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_y_continuous(lim = c(3, 5.5))+ theme(legend.position = "none")+scale_fill_manual(values=c("#61D6DC"))

p6<- ggplot(dados, aes(x=1, y=X90,fill = as.factor(1)))+
  geom_boxplot() +
  theme_bw() + 
  ylab(" ")+xlab("X90")+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_y_continuous(lim = c(3, 5.5))+ theme(legend.position = "none")+scale_fill_manual(values=c("#ED8A33"))

figure <- ggarrange(p1,p2,p3,p4,p5,p6,
                    ncol = 3, nrow = 2)
figure


x = rep(1:498)
dados = data.frame(coeficientes_k5[(ind_correto_k5),1],coeficientes_k5[(ind_correto_k5),2],coeficientes_k5[(ind_correto_k5),11],coeficientes_k5[(ind_correto_k5),21],
                   coeficientes_k5[(ind_correto_k5),51],coeficientes_k5[(ind_correto_k5),91])
colnames(dados) = c("X0","X1","X10","X20","X50","X90")
p1<- ggplot(dados, aes(x=x, y=X0,fill = as.factor(1)))+
  geom_point() +
  theme_bw() + 
  ylab(" ")+xlab("X0")+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_y_continuous(lim = c(19, 21))+ theme(legend.position = "none")+scale_fill_manual(values=c("#E69F00"))

p2<- ggplot(dados, aes(x=x, y=X1,fill = as.factor(1)))+
  geom_point() +
  theme_bw() + 
  ylab(" ")+xlab("X1")+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_y_continuous(lim = c(3, 5.5))+ theme(legend.position = "none")+scale_fill_manual(values=c("#3266A8"))

p3<- ggplot(dados, aes(x=x, y=X10,fill = as.factor(1)))+
  geom_point() +
  theme_bw() + 
  ylab(" ")+xlab("X10")+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_y_continuous(lim = c(3, 5.5))+ theme(legend.position = "none")+scale_fill_manual(values=c("#C70039"))

p4<- ggplot(dados, aes(x=x, y=X20,fill = as.factor(1)))+
  geom_point() +
  theme_bw() + 
  ylab(" ")+xlab("X20")+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_y_continuous(lim = c(3, 5.5))+ theme(legend.position = "none")+scale_fill_manual(values=c("#54C358"))

p5<- ggplot(dados, aes(x=x, y=X50,fill = as.factor(1)))+
  geom_point() +
  theme_bw() + 
  ylab(" ")+xlab("X50")+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_y_continuous(lim = c(3, 5.5))+ theme(legend.position = "none")+scale_fill_manual(values=c("#61D6DC"))

p6<- ggplot(dados, aes(x=x, y=X90,fill = as.factor(1)))+
  geom_point() +
  theme_bw() + 
  ylab(" ")+xlab("X90")+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_y_continuous(lim = c(3, 5.5))+ theme(legend.position = "none")+scale_fill_manual(values=c("#ED8A33"))

figure <- ggarrange(p1,p2,p3,p4,p5,p6,
                    ncol = 3, nrow = 2)
figure

x = rep(1:498)
dados = data.frame(coeficientes_k10[(ind_correto_k10),1],
                   coeficientes_k10[(ind_correto_k10),2],
                   coeficientes_k10[(ind_correto_k10),11],
                   coeficientes_k10[(ind_correto_k10),21],
                   coeficientes_k10[(ind_correto_k10),51],
                   coeficientes_k10[(ind_correto_k10),91])
colnames(dados) = c("X0","X1","X10","X20","X50","X90")
p1<- ggplot(dados, aes(x=x, y=X0,fill = as.factor(1)))+
  geom_point() +
  theme_bw() + 
  ylab(" ")+xlab("X0")+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_y_continuous(lim = c(19, 21))+ theme(legend.position = "none")+scale_fill_manual(values=c("#E69F00"))

p2<- ggplot(dados, aes(x=x, y=X1,fill = as.factor(1)))+
  geom_point() +
  theme_bw() + 
  ylab(" ")+xlab("X1")+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_y_continuous(lim = c(3, 5.5))+ theme(legend.position = "none")+scale_fill_manual(values=c("#3266A8"))

p3<- ggplot(dados, aes(x=x, y=X10,fill = as.factor(1)))+
  geom_point() +
  theme_bw() + 
  ylab(" ")+xlab("X10")+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_y_continuous(lim = c(3, 5.5))+ theme(legend.position = "none")+scale_fill_manual(values=c("#C70039"))

p4<- ggplot(dados, aes(x=x, y=X20,fill = as.factor(1)))+
  geom_point() +
  theme_bw() + 
  ylab(" ")+xlab("X20")+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_y_continuous(lim = c(3, 5.5))+ theme(legend.position = "none")+scale_fill_manual(values=c("#54C358"))

p5<- ggplot(dados, aes(x=x, y=X50,fill = as.factor(1)))+
  geom_point() +
  theme_bw() + 
  ylab(" ")+xlab("X50")+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_y_continuous(lim = c(3, 5.5))+ theme(legend.position = "none")+scale_fill_manual(values=c("#61D6DC"))

p6<- ggplot(dados, aes(x=x, y=X90,fill = as.factor(1)))+
  geom_point() +
  theme_bw() + 
  ylab(" ")+xlab("X90")+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_y_continuous(lim = c(3, 5.5))+ theme(legend.position = "none")+scale_fill_manual(values=c("#ED8A33"))

figure <- ggarrange(p1,p2,p3,p4,p5,p6,
                    ncol = 3, nrow = 2)
figure

# Questão (e):

dados2 = data.frame(lambdaotimo_k5[(ind_correto_k5),1])
colnames(dados2) = c("Lambda")
l1<- ggplot(dados2, aes(x=1, y=Lambda,fill = as.factor(1)))+
  geom_boxplot() +
  theme_bw() + 
  ylab(" ")+xlab("Modelo Correto")+
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
  scale_y_continuous(lim = c(0, 1.5))+ theme(legend.position = "none")+
  scale_fill_manual(values=c("#FF00FF"))

dados3 = data.frame(lambdaotimo_k5[(ind_incorreto_k5),1])
colnames(dados3) = c("Lambda")
l2<- ggplot(dados3, aes(x=1, y=Lambda,fill = as.factor(1)))+
  geom_boxplot() +
  theme_bw() + 
  ylab(" ")+xlab("Modelo Incorreto")+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_y_continuous(lim = c(0, 1.5))+ theme(legend.position = "none")+scale_fill_manual(values=c("#3266A8"))

dados4 = data.frame(lambdaotimo_k5)
colnames(dados4) = c("Lambda")
l3<- ggplot(dados4, aes(x=1, y=Lambda,fill = as.factor(1)))+
  geom_boxplot() +
  theme_bw() + 
  ylab(" ")+xlab("Lambda ótimo Geral")+
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
  scale_y_continuous(lim = c(0, 1.5))+ theme(legend.position = "none")+
  scale_fill_manual(values=c("#5F04B4"))

figure <- ggarrange(l1,l2,
                    ncol = 2, nrow = 1)
figure


dados2 = data.frame(lambdaotimo_k10[(ind_correto_k10),1])
colnames(dados2) = c("Lambda")
l1<- ggplot(dados2, aes(x=1, y=Lambda,fill = as.factor(1)))+
  geom_boxplot() +
  theme_bw() + 
  ylab(" ")+xlab("Modelo Correto")+
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
  scale_y_continuous(lim = c(0, 1.5))+ theme(legend.position = "none")+
  scale_fill_manual(values=c("#FF00FF"))

dados3 = data.frame(lambdaotimo_k10[(ind_incorreto_k10),1])
colnames(dados3) = c("Lambda")
l2<- ggplot(dados3, aes(x=1, y=Lambda,fill = as.factor(1)))+
  geom_boxplot() +
  theme_bw() + 
  ylab(" ")+xlab("Modelo Incorreto")+theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_y_continuous(lim = c(0, 1.5))+ theme(legend.position = "none")+scale_fill_manual(values=c("#3266A8"))

dados4 = data.frame(lambdaotimo_k10)
colnames(dados4) = c("Lambda")
l3<- ggplot(dados4, aes(x=1, y=Lambda,fill = as.factor(1)))+
  geom_boxplot() +
  theme_bw() + 
  ylab(" ")+xlab("Lambda ótimo Geral")+
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
  scale_y_continuous(lim = c(0, 1.5))+ theme(legend.position = "none")+
  scale_fill_manual(values=c("#5F04B4"))

figure <- ggarrange(l1,l2,
                    ncol = 2, nrow = 1)
figure

# Histogramas:
# hist(lambdaotimo_k5[(ind_correto_k5),1],breaks = seq(from = 0.2, to = 1.6, by = 0.1),
#      main = " ",
#      xlab="Lambda ótimo (Modelo Correto)", 
#      ylab = "Frequência",
#      border="black", 
#      col="#E69F00",
#      xlim = c(0.2,1.6),
#      ylim = c(0,160),
#      las=1)
# grid()
# hist(lambdaotimo_k5[(ind_correto_k5),1], add = TRUE, col = '#E69F00')
# hist(lambdaotimo_k5[(ind_incorreto_k5),1],breaks = seq(from = 0.2, to = 1.6, by = 0.1),
#      main = " ",
#      xlab="Lambda ótimo (Modelo Incorreto)", 
#      ylab = "Frequência",
#      border="black", 
#      col="#3266A8",
#      xlim = c(0.2,1.6),
#      ylim = c(0,160),
#      las=1)
#     grid()
# hist(lambdaotimo_k5[(ind_incorreto_k5),1], add = TRUE, col = '#3266A8')

hist(lambdaotimo_k5[(ind_correto_k5),1],breaks = seq(from = 0.2, to = 1.6, by = 0.1),
     main = " ",
     xlab="Lambda ótimo", 
     ylab = "Frequência",
     border="black", 
     col= rgb(0.90,0.62,0,1),
     xlim = c(0.2,1.6),
     ylim = c(0,140),
     las=1)
grid()
hist(lambdaotimo_k5[(ind_correto_k5),1], add = TRUE, col = rgb(0.90,0.62,0,1))
hist(lambdaotimo_k5[(ind_incorreto_k5),1],breaks = seq(from = 0.2, to = 1.6, by = 0.1),
     main = " ",
     xlab="Lambda ótimo", 
     ylab = "Frequência",
     border="black", 
     col= rgb(0.2,0.4,0.66,0.75),
     xlim = c(0.2,1.6),
     ylim = c(0,140),
     las=1,add = TRUE)
box()
legend("topright", c("Modelo Correto", "Modelo Incorreto"), 
       lty=c(1,1), bty = "n", 
       fill=c("#E69F00", "#3266A8"))
