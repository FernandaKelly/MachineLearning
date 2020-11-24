##################################################################################
##  Exemplos do livro R FOR DATA SCIENCE
##################################################################################

##################################################################################
#Visualização de Dados com GGPLT2
##################################################################################

gplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5))


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 3)


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

#facet_grid(. ~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

# left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# right
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))


ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) +
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)


ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))

ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )

?stat_bin


ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")


?position_dodge
?position_fill
?position_identity
?position_jitter
?position_stack

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()







##################################################################################
# MANIPULAÇÃO DE DADOS
##################################################################################



##################################################################################
#dplyr básico
##################################################################################

library(nycflights13)
library(tidyverse)
library(ggplot2)
library(dplyr)

##################################################################################

#stats::filter()
#stats::lag()

nycflights13::flights #QUADRO DE DADOS QUE VAMOS UTILIZAR
str(flights)

#int significa números inteiros.
#dbl significa duplas ou números reais.
#chr significa vetores de caracteres ou strings.
#dttm significa data-hora (uma data + uma hora).
#lgl significa lógico, vetores que contêm apenas TRUEou FALSE.
#fctr significa fatores, que R usa para representar variáveis categóricas com valores possíveis fixos.
#date significa datas.

dim(flights)

?flights #DOCUMENTAÇÃO DO QUADRO DE DADOS

dplyr::filter(flights, month == 1, day == 1) %>%
  dim()  #SÃO 842 OBSERVAÇÕES COM ESSAS CARACTERÍSTICAS SEM MODIFICAR O QUADRO DE DADOS REAL

jan1 <- dplyr::filter(flights, month == 1, day == 1) #R IMPRIME OS RESULTADOS E SALVA EM UMA VARIÁVEL

sqrt(2)**2 == 2
near(sqrt(2)**2,2)

dplyr::filter(flights, flights$month == 11 | flights$month == 12) #ENCONTRAR TODOS OS VOOS QUE PARTIRAM EM NOVEMBRO OU DEZEMBRO

#x %in% y

nov_dec <- dplyr::filter(flights, month %in% c(11,12)) #SELECIONA TODAS AS LINHAS QUE ESTÃO EM X EM Y

dplyr::filter(flights, !(flights$arr_delay > 120 | dep_delay > 120)) #OS DOIS FILTROS RETORNAM A MESMA FILTRAGEM DE DADOS
dplyr::filter(flights, flights$arr_delay <=20, dep_delay <= 120)

#Obs.: A FUNÇÃO FILTER INCLUI APENAS LINHAS ONDE A CONDIÇÃO DA FUNÇÃO IS.NA() É TRUE; ELE EXCLUI OS VALORES FALSE E NA.
#QUANDO FOR DE INTERESSE TRABALHAR COM NA DEVEMOS SOLICITA-LOS EXPLICITAMENTE


#ORGANIZAR LINHAS COM A FUNÇÃO ARRANGE

dplyr::arrange(flights, year, month, day)

dplyr::arrange(flights, desc(dep_delay)) #OS VALORES AUSENTES SÃO SEMPRE CLASSIFICADOS NO FINAL

#SELECIONAR VARIÁVEIS COM A FUNÇÃO SELECT

dplyr::select(flights, year, month, day) #É APENAS UMA SELEÇÃO
dplyr::select(flights, year:day)
dplyr::select(flights, -(year:day)) #TODAS AS VARIÁVEIS MENOS DE YEAR Á DAY

#FUNÇÕES AUXILIARES

#starts_with("abc")
#ends_with("xyz")
#contains("ijk")
#matches("(.)\\1")
#num_range("x", 1:3)


#FUNÇÃO RENAME

dplyr::rename(flights, tail_num = tailnum)

dplyr::select(flights, time_hour, air_time, everything())


#FUNÇÃO MUTATE

flights_sml <- dplyr::select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time)


dplyr::mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60)


dplyr::mutate(flights_sml,
       gain = dep_delay - arr_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours)

#FUNÇÃO TRANSMUTE

dplyr::transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours)

dplyr::transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100)

(x <- 1:10)
lag(x)
lead(x)

cumsum(x)
cummean(x)

y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
min_rank(desc(y))

#FUNÇÃO SUMMARISE

summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))

delay <- filter(delay, count > 20, dest != "HNL")


ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)



#OUTRA FORMA DE FAZER O QUE ESTÁ ACIMA UTILIZANDO O OPERADOR PIPE
delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")
















