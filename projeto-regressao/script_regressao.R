
### Leitura, Limpeza e Organizacao base

library(tidyverse)

base = read.csv(file = "PRSA_data_2010.1.1-2014.12.31.csv")
base

glimpse(base)

base$No = base$No |> as.character()

glimpse(base)

### sera criada uma var dia de semana a partir da data
base = base |> mutate(
  diadasemana = weekdays.Date(ymd(paste0(base$year,"-",base$month,"-",base$day)))
)

glimpse(base)

base$year = base$year |> as.factor()
base$month = base$month |> as.factor()
base$day = base$day |> as.factor()
base$hour = base$hour |> as.factor()
base$cbwd = base$cbwd |> as.factor()
base$diadasemana = base$diadasemana |> as.factor()

summary(base)

#para procurar e excluir NA
library(naniar)
base |> gg_miss_var()
base |> vis_miss()
base = base |> filter(!is.na(base$pm2.5))
base |> gg_miss_var()
base |> vis_miss()


#### Separacao em treino e teste

N = dim(base)[1]
n = (N*.75)%/%1
base_treino = base[1:n,]
base_teste  = base[(n+1):N,]
dim(base_treino)
dim(base_teste)

glimpse(base_treino)
glimpse(base_teste)



#### Breve análise descritiva na base de treino

### Correlação entre variáveis independentes quantitativas
cor(base_treino |> select(DEWP,TEMP,PRES,Iws,Is,Ir))
## DEWP, TEMP e PRES muito correlacionadas -> escolher uma delas

### Relação entre variaveis independentes quantitativas e a var alvo
plot(base_treino$DEWP,base_treino$pm2.5)
cor(base_treino$DEWP,base_treino$pm2.5)

plot(base_treino$TEMP,base_treino$pm2.5)
cor(base_treino$TEMP,base_treino$pm2.5)

plot(base_treino$PRES,base_treino$pm2.5)
cor(base_treino$PRES,base_treino$pm2.5)

plot(base_treino$Iws,base_treino$pm2.5)
plot(base_treino$Is,base_treino$pm2.5)
plot(base_treino$Ir,base_treino$pm2.5)

### Relação entre variaveis independentes quantitativas e a var alvo
boxplot(base_treino$pm2.5 ~ base_treino$year) #pouco relevante
boxplot(base_treino$pm2.5 ~ base_treino$month) #relevante
boxplot(base_treino$pm2.5 ~ base_treino$day)
boxplot(base_treino$pm2.5 ~ base_treino$hour)
boxplot(base_treino$pm2.5 ~ base_treino$diadasemana)
boxplot(base_treino$pm2.5 ~ base_treino$cbwd)


##Preparacao da base de treino

### Padronizacao das var independentes quantitativas

X_quanti = base_treino |> select(DEWP,Iws,Is,Ir) |> scale()
class(X_quanti)
dim(X_quanti)
colnames(X_quanti)



### Tratamento nas variaveis qualitativas
base_treino = base_treino |> mutate(
  estacao = 
    ifelse(
      base_treino$month %in% c("10","11","12","1","2","3"),"outonoinverno","primaveraverao")
)
base_treino$estacao = factor(base_treino$estacao)
base_treino$estacao


X_quali = model.matrix(~ . ,data = base_treino |> select(estacao,diadasemana,cbwd))
class(X_quali)
dim(X_quali)
colnames(X_quali)
#retirar acentos e caracteres especiais dos nomes das variaveis
colnames(X_quali) = c( 
  "(Intercept)","estacaoprimaveraverao",
  "diadasemanaquarta","diadasemanaquinta","diadasemanasabado","diadasemanasegunda","diadasemanasexta","diadasemanaterca",
  "cbwdNE","cbwdNW","cbwdSE")


### Padronizar a variavel resposta
Y = base_treino |> select(pm2.5) |> scale()


### Treinamento de uma RNA
library(neuralnet)
help("neuralnet")




set.seed(123456789)
h1 = Sys.time()
h1
RNA_1_ = neuralnet(pm2.5 ~ 
                     estacaoprimaveraverao + 
                     diadasemanaquarta + diadasemanaquinta + diadasemanasabado + diadasemanasegunda + diadasemanasexta + diadasemanaterca + 
                     cbwdNE + cbwdNW + cbwdSE + 
                     DEWP + Iws + Is + Ir,
                   data = cbind(X_quali,X_quanti,Y),
                   hidden = 1)
h2 = Sys.time()
h2-h1
#saveRDS(object = RNA_1_,file = "RNA_1_.RDS")
#RNA_1_ = readRDS(file = "RNA_1_.RDS")
#Time difference of 22.83098 secs
plot(RNA_1_)
prev_treino = predict(RNA_1_,newdata = cbind(X_quali[,-1],X_quanti))
MSE_treino_1_ = mean((Y - prev_treino)^2)
R2_treino_1_ = 1 - sum((prev_treino - Y)^2)/(sum((mean(Y) - Y)^2))



set.seed(123456789)
h1 = Sys.time()
h1
RNA_2_ = neuralnet(pm2.5 ~ 
                     estacaoprimaveraverao + 
                     diadasemanaquarta + diadasemanaquinta + diadasemanasabado + diadasemanasegunda + diadasemanasexta + diadasemanaterca + 
                     cbwdNE + cbwdNW + cbwdSE + 
                     DEWP + Iws + Is + Ir,
                   data = cbind(X_quali,X_quanti,Y),
                   hidden = 2)
h2 = Sys.time()
h2-h1
#saveRDS(object = RNA_2_,file = "RNA_2_.RDS")
#RNA_2_ = readRDS(file = "RNA_2_.RDS")
#Time difference of 28.88668 secs
plot(RNA_2_)
prev_treino = predict(RNA_2_,newdata = cbind(X_quali[,-1],X_quanti))
MSE_treino_2_ = mean((Y - prev_treino)^2)
R2_treino_2_ = 1 - sum((prev_treino - Y)^2)/(sum((mean(Y) - Y)^2))


set.seed(123456789)
h1 = Sys.time()
h1
RNA_3_ = neuralnet(pm2.5 ~ 
                     estacaoprimaveraverao + 
                     diadasemanaquarta + diadasemanaquinta + diadasemanasabado + diadasemanasegunda + diadasemanasexta + diadasemanaterca + 
                     cbwdNE + cbwdNW + cbwdSE + 
                     DEWP + Iws + Is + Ir,
                   data = cbind(X_quali,X_quanti,Y),
                   hidden = 3,
                   stepmax =  1e+06)
h2 = Sys.time()
h2-h1
#Time difference of 20.33879 mins
#saveRDS(object = RNA_3_,file = "RNA_3_.RDS")
#RNA_3_ = readRDS(file = "RNA_3_.RDS")
plot(RNA_3_)
prev_treino = predict(RNA_3_,newdata = cbind(X_quali[,-1],X_quanti))
MSE_treino_3_ = mean((Y - prev_treino)^2)
R2_treino_3_ = 1 - sum((prev_treino - Y)^2)/(sum((mean(Y) - Y)^2))



set.seed(123456789)
h1 = Sys.time()
h1
RNA_4_ = neuralnet(pm2.5 ~ 
                     estacaoprimaveraverao + 
                     diadasemanaquarta + diadasemanaquinta + diadasemanasabado + diadasemanasegunda + diadasemanasexta + diadasemanaterca + 
                     cbwdNE + cbwdNW + cbwdSE + 
                     DEWP + Iws + Is + Ir,
                   data = cbind(X_quali,X_quanti,Y),
                   hidden = 4,
                   stepmax =  1e+06)
h2 = Sys.time()
h2-h1
#saveRDS(object = RNA_4_,file = "RNA_4_.RDS")
#Time difference of 35.49424 mins
#RNA_4_ = readRDS(file = "RNA_4_.RDS")
plot(RNA_4_)
prev_treino = predict(RNA_4_,newdata = cbind(X_quali[,-1],X_quanti))
MSE_treino_4_ = mean((Y - prev_treino)^2)
R2_treino_4_ = 1 - sum((prev_treino - Y)^2)/(sum((mean(Y) - Y)^2))


set.seed(123456789)
h1 = Sys.time()
h1
RNA_2_2_ = neuralnet(pm2.5 ~ 
                       estacaoprimaveraverao + 
                       diadasemanaquarta + diadasemanaquinta + diadasemanasabado + diadasemanasegunda + diadasemanasexta + diadasemanaterca + 
                       cbwdNE + cbwdNW + cbwdSE + 
                       DEWP + Iws + Is + Ir,
                     data = cbind(X_quali,X_quanti,Y),
                     hidden = c(2,2),
                     stepmax =  1e+06)
h2 = Sys.time()
h2-h1
#saveRDS(object = RNA_2_2_,file = "RNA_2_2_.RDS")
#RNA_2_2_ = readRDS(file = "RNA_2_2_.RDS")
plot(RNA_2_2_)



set.seed(123456789)
h1 = Sys.time()
h1
RNA_2_3_ = neuralnet(pm2.5 ~ 
                       estacaoprimaveraverao + 
                       diadasemanaquarta + diadasemanaquinta + diadasemanasabado + diadasemanasegunda + diadasemanasexta + diadasemanaterca + 
                       cbwdNE + cbwdNW + cbwdSE + 
                       DEWP + Iws + Is + Ir,
                     data = cbind(X_quali,X_quanti,Y),
                     hidden = c(2,3),
                     stepmax = 1e+06)
h2 = Sys.time()
h2-h1
#saveRDS(object = RNA_2_3_,file = "RNA_2_3_.RDS")
#RNA_2_3_ = readRDS(file = "RNA_2_3_.RDS")
plot(RNA_2_3_)


set.seed(123456789)
h1 = Sys.time()
h1
RNA_3_2_ = neuralnet(pm2.5 ~ 
                       estacaoprimaveraverao + 
                       diadasemanaquarta + diadasemanaquinta + diadasemanasabado + diadasemanasegunda + diadasemanasexta + diadasemanaterca + 
                       cbwdNE + cbwdNW + cbwdSE + 
                       DEWP + Iws + Is + Ir,
                     data = cbind(X_quali,X_quanti,Y),
                     hidden = c(3,2),
                     stepmax = 1e+06)
h2 = Sys.time()
h2-h1
#Time difference of 25.83185 mins
#saveRDS(object = RNA_3_2_,file = "RNA_3_2_.RDS")
#RNA_3_2_ = readRDS(file = "RNA_3_2_.RDS")
plot(RNA_3_2_)
prev_treino = predict(RNA_3_2_,newdata = cbind(X_quali[,-1],X_quanti))
MSE_treino_3_2_ = mean((Y - prev_treino)^2)
R2_treino_3_2_ = 1 - sum((prev_treino - Y)^2)/(sum((mean(Y) - Y)^2))



set.seed(123456789)
h1 = Sys.time()
h1
RNA_3_3_ = neuralnet(pm2.5 ~ 
                       estacaoprimaveraverao + 
                       diadasemanaquarta + diadasemanaquinta + diadasemanasabado + diadasemanasegunda + diadasemanasexta + diadasemanaterca + 
                       cbwdNE + cbwdNW + cbwdSE + 
                       DEWP + Iws + Is + Ir,
                     data = cbind(X_quali,X_quanti,Y),
                     hidden = c(3,3),
                     stepmax =  1e+06)
# Warning message:
#   Algorithm did not converge in 1 of 1 repetition(s) within the stepmax. 
h2 = Sys.time()
h2-h1
#Time difference of 5.148957 hours
#saveRDS(object = RNA_3_3_,file = "RNA_3_3_.RDS")
#RNA_3_3_ = readRDS(file = "RNA_3_3_.RDS")
plot(RNA_3_3_)





set.seed(123456789)
h1 = Sys.time()
h1
RNA_2_2_2_ = neuralnet(pm2.5 ~ 
                         estacaoprimaveraverao + 
                         diadasemanaquarta + diadasemanaquinta + diadasemanasabado + diadasemanasegunda + diadasemanasexta + diadasemanaterca + 
                         cbwdNE + cbwdNW + cbwdSE + 
                         DEWP + Iws + Is + Ir,
                       data = cbind(X_quali,X_quanti,Y),
                       hidden = c(2,2,2),
                       stepmax =  1e+06)
# Warning message:
#   Algorithm did not converge in 1 of 1 repetition(s) within the stepmax
h2 = Sys.time()
h2-h1
#Time difference of 5.008457 hours
#saveRDS(object = RNA_2_2_2_,file = "RNA_2_2_2_.RDS")
#RNA_2_2_2_ = readRDS(,file = "RNA_2_2_2_.RDS")
plot(RNA_2_2_2_)


barplot(
  height = c(R2_treino_1_,R2_treino_2_,R2_treino_3_,R2_treino_4_,R2_treino_3_2_),
  names.arg = c("RNA_1_","RNA_2_","RNA_3_","RNA_4_","RNA_3_2_"),
  main = "R2 para os dados de treino")






### Previsao na base de teste

### Preparar a base de teste



### Padronizacao das var independentes quantitativas

X_quanti

X_quanti_teste = base_teste |> mutate(
  DEWP = (DEWP - 2.40080467)/14.411632 ,
  Iws = (Iws - 24.32770572)/48.766166  ,
  Is = (Is - 0.06431012   )/0.818277,
  Ir = (Ir - 0.22300987 )/1.543185 ) |> select(DEWP,Iws,Is,Ir)

class(X_quanti_teste)
dim(X_quanti_teste)
colnames(X_quanti_teste)


### Tratamento nas variaveis qualitativas
base_teste = base_teste |> mutate(
  estacao = 
    ifelse(
      base_teste$month %in% c("10","11","12","1","2","3"),"outonoinverno","primaveraverao")
)
base_teste$estacao = factor(base_teste$estacao)
base_teste$estacao


X_quali_teste = model.matrix(~ . ,data = base_teste |> select(estacao,diadasemana,cbwd))
class(X_quali_teste)
dim(X_quali_teste)
colnames(X_quali_teste)
#retirar acentos e caracteres especiais dos nomes das variaveis
colnames(X_quali_teste) = c( 
  "(Intercept)","estacaoprimaveraverao",
  "diadasemanaquarta","diadasemanaquinta","diadasemanasabado","diadasemanasegunda","diadasemanasexta","diadasemanaterca",
  "cbwdNE","cbwdNW","cbwdSE")


### Padronizar a variavel resposta
Y
Y_teste = (base_teste$pm2.5 - 98.85644 )/91.1152 



#### Previsao base teste
prev_teste = predict(RNA_1_,newdata = cbind(X_quali_teste[,-1],X_quanti_teste))
MSE_teste_1_ = mean((Y_teste - prev_teste)^2)
R2_teste_1_ = 1 - sum((prev_teste - Y_teste)^2)/(sum((mean(Y_teste) - Y_teste)^2))

prev_teste = predict(RNA_2_,newdata = cbind(X_quali_teste[,-1],X_quanti_teste))
MSE_teste_2_ = mean((Y_teste - prev_teste)^2)
R2_teste_2_ = 1 - sum((prev_teste - Y_teste)^2)/(sum((mean(Y_teste) - Y_teste)^2))

prev_teste = predict(RNA_3_,newdata = cbind(X_quali_teste[,-1],X_quanti_teste))
MSE_teste_3_ = mean((Y_teste - prev_teste)^2)
R2_teste_3_ = 1 - sum((prev_teste - Y_teste)^2)/(sum((mean(Y_teste) - Y_teste)^2))

prev_teste = predict(RNA_4_,newdata = cbind(X_quali_teste[,-1],X_quanti_teste))
MSE_teste_4_ = mean((Y_teste - prev_teste)^2)
R2_teste_4_ = 1 - sum((prev_teste - Y_teste)^2)/(sum((mean(Y_teste) - Y_teste)^2))

prev_teste = predict(RNA_3_2_,newdata = cbind(X_quali_teste[,-1],X_quanti_teste))
MSE_teste_3_2_ = mean((Y_teste - prev_teste)^2)
R2_teste_3_2_ = 1 - sum((prev_teste - Y_teste)^2)/(sum((mean(Y_teste) - Y_teste)^2))



R2_treino = c(R2_treino_1_,R2_treino_2_,R2_treino_3_,R2_treino_4_,R2_treino_3_2_)
R2_teste = c(R2_teste_1_,R2_teste_2_,R2_teste_3_,R2_teste_4_,R2_teste_3_2_)
barplot(
  height = matrix(c(R2_treino,R2_teste),nrow = 2,byrow = T),
  beside = T,
  names.arg = c("RNA_1_","RNA_2_","RNA_3_","RNA_4_","RNA_3_2_"),
  main = "R2 para os dados de treino e teste",legend.text = c("treino","teste") )





