library(uclust)
library(RColorBrewer)
library(ggplot2)


attach(mtcars)
names(mtcars)
head(mtcars, n = 2)

nm = rownames(mtcars)
mtcars.country <- c(rep("Japan", 3), rep("US",4), rep("Europe", 7),rep("US",3), "Europe", rep("Japan", 3), rep("US",4), rep("Europe", 3), "US", rep("Europe", 3))

cores = c("red", "green","blue")
clrs <- cores[as.factor(mtcars.country)]

dados = mtcars[, c("hp", "mpg")]
ggplot(data = dados, aes(x = hp, y= mpg)) +
  geom_point() + 
  geom_text(aes(label=nm, col = mtcars.country), hjust = 0, vjust = 0) +
  theme_minimal()


# reescalando as variáveis
df =  scale(dados[, c("hp", "mpg")])

ggplot(data = data.frame(df), aes(x = hp, y= mpg)) +
  geom_point() + 
  geom_text(aes(label=nm, col =  mtcars.country), hjust = 0, vjust = 0) +
  theme_minimal()


# k-means
km2 = kmeans(df[,c("hp","mpg")], 3)
grupo.km = as.factor(km2$cluster)

cores.km <- c(cores, cores)
ggplot(data = data.frame(df)) +
  geom_point(aes(x = hp, y= mpg, color = grupo.km), size = 5) +
  scale_color_manual(values = cores.km) +
  geom_text(aes(x = hp, y= mpg, label = nm, color = mtcars.country), hjust = 0, vjust = 0) +
  theme_minimal()


# k-means na dimensão do PCA
mtcars.pca <- prcomp(df[,c("hp","mpg")])
summary(mtcars.pca)  
km.pca = kmeans(mtcars.pca$x,3)  
grupo.pca = as.factor(km.pca$cluster)

cores.pca <- c(c("green","red","blue"), cores)
ggplot(data = data.frame(df)) +
  geom_point(aes(x = hp, y= mpg, color = grupo.pca), size = 5) +
  scale_color_manual(values = cores.pca) +
  geom_text(aes(x = hp, y= mpg, label = nm, color = mtcars.country), hjust = 0, vjust = 0) +
  theme_minimal()


# Calculando a matriz de dissimilaridades
#  caso necessário, usar as.matrix(d) para transformar o objeto em uma matriz
# 
# Medidas disponíveis
#   "euclidean", "maximum", "manhattan", 
#   "canberra", "binary" or "minkowski"
d <- dist(df, method = "euclidean")


# Hierarchical clustering 
# Métodos de ligação disponíveis:
#   "ward.D", "ward.D2", "single", 
#   "complete", "average" (= UPGMA), 
#   "mcquitty" (= WPGMA), "median" (= WPGMC) or 
#   "centroid" (= UPGMC)

#  1) Single
hc.single <- hclust(d, method = "single" )
plot(hc.single, hang = -1)
#----------------------------------------------------
# enfeitando o dendograma
dend <- as.dendrogram(hc.single, center = T)
dend %>% set("labels_col", clrs) %>% # change color
  set("labels_cex", 1) %>% # Change size
  set("branches_k_color", k = 3) %>% 
  set("labels_colors", clrs[hc.single$order]) %>%
  plot() # plot
#----------------------------------------------------

gp.s = cutree(hc.single, k = 3)
head(gp.s, n = 4)
cores.hc.s <- c(c("red", "blue", "green"), cores)
ggplot(data = data.frame(df, gp = as.factor(gp.s))) +
  geom_point(aes(x = hp, y= mpg, color = gp), size = 5) +
  scale_color_manual(values = cores.hc.s) +
  geom_text(aes(x = hp, y= mpg, label = nm, color = mtcars.country), hjust = 0, vjust = 0) +
  theme_minimal()


#  2) Complete
hc.complete <- hclust(d, method = "complete" )

dend <- as.dendrogram(hc.complete, center = T)
dend %>% set("labels_col", clrs) %>% # change color
  set("labels_cex", 1) %>% # Change size
  set("branches_k_color", k = 3) %>% 
  set("labels_colors", clrs[hc.complete$order]) %>%
  plot() # plot

gp.c = cutree(hc.complete, k = 3)
cores.hc.c <- c(c("blue", "green","red"), cores)
ggplot(data = data.frame(df, gp = as.factor(gp.c))) +
  geom_point(aes(x = hp, y= mpg, color = gp), size = 5) +
  scale_color_manual(values = cores.hc.c) +
  geom_text(aes(x = hp, y= mpg, label = nm, color = mtcars.country), hjust = 0, vjust = 0) +
  theme_minimal()



#  3) average
hc.average <- hclust(d, method = "average" )

dend <- as.dendrogram(hc.average, center = T)
dend %>% set("labels_col", clrs) %>% # change color
  set("labels_cex", 1) %>% # Change size
  set("branches_k_color", k = 3) %>% 
  set("labels_colors", clrs[hc.average$order]) %>%
  plot() # plot

gp.a = cutree(hc.average, k = 3)
cores.hc.a <- c(c("blue", "green","red"), cores)
ggplot(data = data.frame(df, gp = as.factor(gp.a))) +
  geom_point(aes(x = hp, y= mpg, color = gp), size = 5) +
  scale_color_manual(values = cores.hc.a) +
  geom_text(aes(x = hp, y= mpg, label = nm, color = mtcars.country), hjust = 0, vjust = 0) +
  theme_minimal()



#----------------------------------------------
# Usando o pacote uclust (Cybis e Valk)
#---------------------------------------------
# usando a mesma matriz de antes
res = uhclust(md = as.matrix(d))

# default: usando distância euclidiana ao quadrado
res2 = uhclust(data = df)


gp.u = res$groups
cores.hc.a <- c(1:length(unique(gp.u)), cores)
ggplot(data = data.frame(df, gp = as.factor(gp.u))) +
  geom_point(aes(x = hp, y= mpg, color = gp), size = 5) +
  scale_color_manual(values = cores.hc.a) +
  geom_text(aes(x = hp, y= mpg, label = nm, color = mtcars.country), hjust = 0, vjust = 0) +
  theme_minimal()
