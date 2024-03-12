


### Leitura, Limpeza e Organizacao base

library(tidyverse)

base = read.csv(file = "PRSA_data_2010.1.1-2014.12.31.csv")
base

glimpse(base)

base$No = base$No |> as.character()

glimpse(base)

### a var hour sera categorizada para intervalo de hora
base = base |> mutate(
  hora = ifelse(base$hour < 6,"MADRUGADA",
                ifelse(base$hour < 12, "MANHA",
                       ifelse(base$hour < 18,"TARDE","NOITE"))))

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
base$hora = base$hora |> as.factor()
base$diadasemana = base$diadasemana |> as.factor()

summary(base)

#para procurar e excluir NA
library(naniar)
base |> gg_miss_var()
base |> vis_miss()
is.na(base$pm2.5)
base = base |> filter(!is.na(base$pm2.5))
base |> gg_miss_var()
base |> vis_miss()


#### Separacao em treino e teste

base$year
base$day
base$hour

N = dim(base)[1]
n = (N*.75)%/%1
base_treino = base[1:n,]
base_teste  = base[(n+1):N,]
dim(base_treino)
dim(base_teste)

glimpse(base_treino)
glimpse(base_teste)

### Padronizacao das var independentes quantitativas

X_quanti = base_treino |> select(DEWP,TEMP,PRES,Iws,Is,Ir) |> scale()
class(X_quanti)
dim(X_quanti)
colnames(X_quanti)

### Tratamento nas variaveis qualitativas
### obs2: as vars year, day e hour nao serao consideradas
X_quali = model.matrix(~ . ,data = base_treino |> select(month,hora,diadasemana,cbwd))
class(X_quali)
dim(X_quali)
colnames(X_quali)
#retirar acentos e caracteres especiais dos nomes das variaveis
colnames(X_quali) = c( 
  "(Intercept)",
  "month2","month3","month4","month5","month6","month7","month8","month9","month10","month11","month12",
  "horaMANHA","horaNOITE","horaTARDE",
  "diadasemanaquarta",
  "diadasemanaquinta",
  "diadasemanasabado",
  "diadasemanasegunda",
  "diadasemanasexta",
  "diadasemanaterca","cbwdNE","cbwdNW","cbwdSE")



### Padronizar a variavel resposta
Y = base_treino |> select(pm2.5) |> scale()


#### Breve análise descritiva

### Correlação entre variáveis independentes quantitativas
cor(X_quanti)
## DEWP, TEMP e PRES muito correlacionadas -> escolher uma delas

### Relação entre variaveis independentes quantitativas e a var alvo
j = 1
plot(X_quanti[,j],Y,xlab = colnames(X_quanti)[j])
j = j + 1
plot(X_quanti[,j],Y,xlab = colnames(X_quanti)[j])


### Relação entre variaveis independentes quantitativas e a var alvo
boxplot(base_treino$pm2.5 ~ base_treino$year) #pouco relevante
boxplot(base_treino$pm2.5 ~ base_treino$month) #relevante
boxplot(base_treino$pm2.5 ~ base_treino$day)
boxplot(base_treino$pm2.5 ~ base_treino$hour)
boxplot(base_treino$pm2.5 ~ base_treino$hora)
boxplot(base_treino$pm2.5 ~ base_treino$diadasemana)
boxplot(base_treino$pm2.5 ~ base_treino$cbwd)



### Treinamento de uma RNA
library(neuralnet)
help("neuralnet")
RNA_1 = neuralnet(pm2.5 ~ TEMP + Iws + Is + Ir + 
                  horaMANHA + horaNOITE + horaTARDE + 
                  cbwdNE + cbwdNW + cbwdSE, 
                  data = cbind(X_quali,X_quanti,Y),
                  hidden = 1)
plot(RNA_1)
#saveRDS(object = RNA_1,file = "RNA_1.RDS")
#RNA_1 = readRDS(file = "RNA_1.RDS")
prev_treino_1 = RNA_1$net.result[[1]][,1]
mse_treino_1 =  mean((prev_treino_1 - Y)^2)
R2_treino_1 = 1 - sum((prev_treino_1 - Y)^2)/(sum((mean(Y) - Y)^2))



RNA_2 = neuralnet(pm2.5 ~ TEMP + Iws + Is + Ir + 
                    horaMANHA + horaNOITE + horaTARDE + 
                    cbwdNE + cbwdNW + cbwdSE, 
                  data = cbind(X_quali,X_quanti,Y),
                  hidden = 2)
plot(RNA_2)
#saveRDS(object = RNA_2,file = "RNA_2.RDS")
#RNA_2 = readRDS(file = "RNA_2.RDS")
prev_treino_2 = RNA_2$net.result[[1]][,1]
mse_treino_2 =  mean((prev_treino_2 - Y)^2)
R2_treino_2 = 1 - sum((prev_treino_2 - Y)^2)/(sum((mean(Y) - Y)^2))



RNA_3 = neuralnet(pm2.5 ~ TEMP + Iws + Is + Ir + 
                    horaMANHA + horaNOITE + horaTARDE + 
                    cbwdNE + cbwdNW + cbwdSE + 
                    month2 + month3 + month4 + month5 + month6 + 
                    month7 + month8 + month9 + month10 + month11 + month12, 
                  data = cbind(X_quali,X_quanti,Y),
                  hidden = 2)
plot(RNA_3)
#saveRDS(object = RNA_3,file = "RNA_3.RDS")
#RNA_3 = readRDS(file = "RNA_3.RDS")
prev_treino_3 = RNA_3$net.result[[1]][,1]
mse_treino_3 =  mean((prev_treino_3 - Y)^2)
R2_treino_3 = 1 - sum((prev_treino_3 - Y)^2)/(sum((mean(Y) - Y)^2))


RNA_4 = neuralnet(pm2.5 ~ TEMP + Iws + Is + Ir + 
                    horaMANHA + horaNOITE + horaTARDE + 
                    cbwdNE + cbwdNW + cbwdSE + 
                    month2 + month3 + month4 + month5 + month6 + 
                    month7 + month8 + month9 + month10 + month11 + month12 + 
                    diadasemanaquarta + diadasemanaquinta + diadasemanasabado + 
                    diadasemanasegunda + diadasemanasexta + diadasemanaterca, 
                  data = cbind(X_quali,X_quanti,Y),
                  hidden = 2)
#saveRDS(object = RNA_4,file = "RNA_4.RDS")
#RNA_4 = readRDS(file = "RNA_4.RDS")
plot(RNA_4)
prev_treino_4 = RNA_4$net.result[[1]][,1]
mse_treino_4 =  mean((prev_treino_4 - Y)^2)
R2_treino_4 = 1 - sum((prev_treino_4 - Y)^2)/(sum((mean(Y) - Y)^2))


barplot(
  height = c(R2_treino_1,R2_treino_2,R2_treino_3,R2_treino_4),
  names.arg = c("RNA_1","RNA_2","RNA_3","RNA_4"),
  main = "R2 para os dados de treino")


RNA_5 = neuralnet(pm2.5 ~ TEMP + Iws + Is + Ir + 
                    horaMANHA + horaNOITE + horaTARDE + 
                    cbwdNE + cbwdNW + cbwdSE + 
                    month2 + month3 + month4 + month5 + month6 + 
                    month7 + month8 + month9 + month10 + month11 + month12, 
                  data = cbind(X_quali,X_quanti,Y),
                  hidden = c(2,2),
                  stepmax = 1e+06)
#saveRDS(object = RNA_5,file = "RNA_5.RDS")
#RNA_5 = readRDS(file = "RNA_5.RDS")
plot(RNA_5)
prev_treino_5 = RNA_5$net.result[[1]][,1]
mse_treino_5 =  mean((prev_treino_5 - Y)^2)
R2_treino_5 = 1 - sum((prev_treino_5 - Y)^2)/(sum((mean(Y) - Y)^2))

barplot(
  height = c(R2_treino_1,R2_treino_2,R2_treino_3,R2_treino_4,
             R2_treino_5),
  names.arg = c("RNA_1","RNA_2","RNA_3","RNA_4","RNA_5"),
  main = "R2 para os dados de treino")



RNA_6 = neuralnet(pm2.5 ~ TEMP + Iws + Is + Ir + 
                    horaMANHA + horaNOITE + horaTARDE + 
                    cbwdNE + cbwdNW + cbwdSE + 
                    month2 + month3 + month4 + month5 + month6 + 
                    month7 + month8 + month9 + month10 + month11 + month12, 
                  data = cbind(X_quali,X_quanti,Y),
                  hidden = 3,
                  stepmax = 1e+06)
#saveRDS(object = RNA_6,file = "RNA_6.RDS")
#RNA_6 = readRDS(file = "RNA_6.RDS")
plot(RNA_6)
prev_treino_6 = RNA_6$net.result[[1]][,1]
mse_treino_6 =  mean((prev_treino_6 - Y)^2)
R2_treino_6 = 1 - sum((prev_treino_6 - Y)^2)/(sum((mean(Y) - Y)^2))


barplot(
  height = c(R2_treino_1,R2_treino_2,R2_treino_3,R2_treino_4,
             R2_treino_5,R2_treino_6),
  names.arg = c("RNA_1","RNA_2","RNA_3","RNA_4","RNA_5","RNA_6"),
  main = "R2 para os dados de treino")

RNA_7 = neuralnet(pm2.5 ~ TEMP + Iws + Is + Ir + 
                    horaMANHA + horaNOITE + horaTARDE + 
                    cbwdNE + cbwdNW + cbwdSE + 
                    month2 + month3 + month4 + month5 + month6 + 
                    month7 + month8 + month9 + month10 + month11 + month12, 
                  data = cbind(X_quali,X_quanti,Y),
                  hidden = c(3,2),
                  stepmax = 1e+06)
#saveRDS(object = RNA_7,file = "RNA_7.RDS")
#RNA_7 = readRDS(file = "RNA_7.RDS")
plot(RNA_7)
prev_treino_7 = RNA_7$net.result[[1]][,1]
mse_treino_7 =  mean((prev_treino_7 - Y)^2)
R2_treino_7 = 1 - sum((prev_treino_7 - Y)^2)/(sum((mean(Y) - Y)^2))


barplot(
  height = c(R2_treino_1,R2_treino_2,R2_treino_3,R2_treino_4,
             R2_treino_5,R2_treino_6,R2_treino_7),
  names.arg = c("RNA_1","RNA_2","RNA_3","RNA_4","RNA_5","RNA_6","RNA_7"),
  main = "R2 para os dados de treino")





Sys.time()
RNA_8 = neuralnet(pm2.5 ~ TEMP + Iws + Is + Ir + 
                    horaMANHA + horaNOITE + horaTARDE + 
                    cbwdNE + cbwdNW + cbwdSE + 
                    month2 + month3 + month4 + month5 + month6 + 
                    month7 + month8 + month9 + month10 + month11 + month12, 
                  data = cbind(X_quali,X_quanti,Y),
                  hidden = c(3,3),
                  stepmax = 1e+06)
Sys.time() 
#2h e 21min
#saveRDS(object = RNA_8,file = "RNA_8.RDS")
#RNA_8 = readRDS(file = "RNA_8.RDS")
plot(RNA_8)
prev_treino_8 = RNA_8$net.result[[1]][,1]
mse_treino_8 =  mean((prev_treino_8 - Y)^2)
R2_treino_8 = 1 - sum((prev_treino_8 - Y)^2)/(sum((mean(Y) - Y)^2))


barplot(
  height = c(R2_treino_1,R2_treino_2,R2_treino_3,R2_treino_4,
             R2_treino_5,R2_treino_6,R2_treino_7,R2_treino_8),
  names.arg = c("RNA_1","RNA_2","RNA_3","RNA_4","RNA_5","RNA_6","RNA_7","RNA_8"),
  main = "R2 para os dados de treino")



h1 = Sys.time()
h1
RNA_9 = neuralnet(pm2.5 ~ TEMP + Iws + Is + Ir + 
                    cbwdNE + cbwdNW + cbwdSE + 
                    month2 + month3 + month4 + month5 + month6 + 
                    month7 + month8 + month9 + month10 + month11 + month12, 
                  data = cbind(X_quali,X_quanti,Y),
                  hidden = c(3,3),
                  stepmax = 1e+07)
h2 = Sys.time() 
h2
h2-h1
#Time difference of 6.01276 hours
saveRDS(object = RNA_9,file = "RNA_9.RDS")
#RNA_9 = readRDS(file = "RNA_8.RDS")
plot(RNA_9)
prev_treino_9 = RNA_9$net.result[[1]][,1]
mse_treino_9 =  mean((prev_treino_9 - Y)^2)
R2_treino_9 = 1 - sum((prev_treino_9 - Y)^2)/(sum((mean(Y) - Y)^2))





h1 = Sys.time()
h1
RNA_10 = neuralnet(pm2.5 ~ TEMP + Iws + Is + Ir + 
                    cbwdNE + cbwdNW + cbwdSE + 
                    month2 + month3 + month4 + month5 + month6 + 
                    month7 + month8 + month9 + month10 + month11 + month12, 
                  data = cbind(X_quali,X_quanti,Y),
                  hidden = c(2,2,2),
                  stepmax = 1e+07)
h2 = Sys.time() 
h2
h2-h1
#
saveRDS(object = RNA_10,file = "RNA_10.RDS")


h1 = Sys.time()
h1
RNA_11 = neuralnet(pm2.5 ~ TEMP + Iws + Is + Ir + 
                     cbwdNE + cbwdNW + cbwdSE + 
                     month2 + month3 + month4 + month5 + month6 + 
                     month7 + month8 + month9 + month10 + month11 + month12, 
                   data = cbind(X_quali,X_quanti,Y),
                   hidden = c(3,3,2),
                   stepmax = 1e+07)
h2 = Sys.time() 
h2
h2-h1
#
saveRDS(object = RNA_11,file = "RNA_11.RDS")


h1 = Sys.time()
h1
RNA_12 = neuralnet(pm2.5 ~ TEMP + Iws + Is + Ir + 
                     cbwdNE + cbwdNW + cbwdSE + 
                     month2 + month3 + month4 + month5 + month6 + 
                     month7 + month8 + month9 + month10 + month11 + month12, 
                   data = cbind(X_quali,X_quanti,Y),
                   hidden = c(4,3),
                   stepmax = 1e+07)
h2 = Sys.time() 
h2
h2-h1
#
saveRDS(object = RNA_12,file = "RNA_12.RDS")












colnames(mse_treino) = c("ModComp","M2","M3","M4","M5","MR1","MR2")
row.names(mse_treino) = paste("it",1:10)
summary(mse_treino)

colnames(mse_teste) = c("ModComp","M2","M3","M4","M5","MR1","MR2")
row.names(mse_teste) = paste("it",1:10)
summary(mse_treino)


### opte pelo completo

var = base_analise$atemp
escala = scale(var)
media_atemp = attr(escala,"scaled:center")  
sd_atemp = attr(escala,"scaled:scale")  
atemp_ = escala[,1]

var = base_analise$hum
escala = scale(var)
media_hum = attr(escala,"scaled:center")  
sd_hum = attr(escala,"scaled:scale")  
hum_ = escala[,1]

var = base_analise$windspeed
escala = scale(var)
media_windspeed = attr(escala,"scaled:center")  
sd_windspeed = attr(escala,"scaled:scale")  
windspeed_ = escala[,1]

var = base_analise$cnt
escala = scale(var)
media_cnt = attr(escala,"scaled:center")  
sd_cnt = attr(escala,"scaled:scale")  
cnt_ = escala[,1]

base_analise_ = base_analise |> 
  select(season,holiday,weekday,workingday,weathersit,faixa_hr,faixa_day) |>
  mutate(atemp_ = atemp_,
         hum_ = hum_,
         windspeed_ = windspeed_,
         cnt_ = cnt_)


matriz_ = model.matrix(~ season + holiday + weekday + workingday + 
                                weathersit + faixa_hr + faixa_day + 
                         atemp_ + hum_ + windspeed_ + cnt_,
                       data = base_analise_)

modelo_final = 
  neuralnet(cnt_ ~ seasonverão + seasonoutono + seasoninverno + 
              holidaysim + weekdaysegunda + weekdayterça + weekdayquarta + 
              weekdayquinta + weekdaysexta + weekdaysábado + workingdaysim + 
              weathersitmedio + weathersitruim + faixa_hrmanhã + faixa_hrnoite + 
              faixa_hrtarde + faixa_daysemana2 + faixa_daysemana3 + 
              faixa_daysemana4 + atemp_ + hum_ + windspeed_,
            data = matriz_,
            hidden = 0,
            linear.output = TRUE)

  
base_competicao = read_delim(
  file = "base_competicao.csv",
  delim = ";",
  locale = locale(decimal_mark = ","),
  col_types = cols(season = col_character(),
                   holiday = col_character(),
                   weekday = col_character(),
                   workingday = col_character(),
                   weathersit = col_character()))

base_competicao$season = factor(x = base_competicao$season,
                     labels = c("primavera","verão","outono","inverno"))
base_competicao$holiday = factor(x = base_competicao$holiday,
                      labels = c("não","sim"))
base_competicao$weekday = factor(x = base_competicao$weekday,
                      labels = c("domingo","segunda","terça","quarta","quinta","sexta","sábado"))
base_competicao$workingday = factor(x = base_competicao$workingday,
                         labels = c("não","sim"))
base_competicao$weathersit = factor(x = base_competicao$weathersit,
                         labels = c("bom","medio","ruim"))
base_competicao = base_competicao |> 
  mutate(faixa_hr = ifelse(hr %in% c(0:5),"madrugada",
                           ifelse(hr %in% c(6:11),"manhã",
                                  ifelse(hr %in% c(12:16),"tarde","noite"))))

base_competicao = base_competicao |> mutate(faixa_day = ifelse(day %in% c(1:7),"semana1",
                                         ifelse(hr %in% c(8:14),"semana2",
                                                ifelse(hr %in% c(15:21),"semana3","semana4"))))
base_competicao = base_competicao |> select(season,
                                            holiday,
                                            weekday,
                                            workingday,
                                            weathersit,
                                            atemp,
                                            hum,
                                            windspeed,
                                            faixa_hr,
                                            faixa_day)


base_competicao = base_competicao |> mutate(
  atemp_ = (base_competicao$atemp - media_atemp)/sd_atemp,
  hum_ = (base_competicao$hum - media_hum)/sd_hum,
  windspeed_ = (base_competicao$windspeed - media_windspeed)/sd_windspeed)


matriz_comp_ = model.matrix(~ season + holiday + weekday + workingday + 
                               weathersit + faixa_hr + faixa_day + atemp_ + 
                               hum_ + windspeed_ ,data = base_competicao)

prev_teste = (modelo_final |> compute(matriz_comp_))$net.result[,1]
prev_teste = prev_teste*sd_cnt + media_cnt

summary(prev_teste)

resposta = ifelse(prev_teste<0,0,prev_teste)

summary(resposta)
head(resposta)

write_csv2(data.frame(resposta),file="prev_reg.csv")
head(resposta)

