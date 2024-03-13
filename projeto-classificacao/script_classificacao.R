
### Leitura, Limpeza e Organizacao base

library(tidyverse)

base = read.csv("winequality_class.csv")
glimpse(base)

base$X = base$X |> as.character()
base$tipo = base$tipo |> as.factor()
base$classe = base$classe |> as.factor()
glimpse(base)

summary(base)


#### Separacao em treino e teste

N = dim(base)[1]
n = (N*.75)%/%1
indices_treino = sample(1:N,size = n)
base_treino = base[indices_treino,]
base_teste  = base[-indices_treino,]
dim(base_treino)[1]/(dim(base_teste)[1]+dim(base_treino)[1])

glimpse(base_treino)
glimpse(base_teste)



#### Breve análise descritiva na base de treino

### Correlação entre variáveis independentes quantitativas
cor(base_treino |> select(-c(X,tipo,classe)))

### Relação entre variaveis independentes quantitativas e a var alvo quantitativa
boxplot(base_treino$acidez_fixa ~ base_treino$classe,horizontal = T)
boxplot(base_treino$acidez_volatil ~ base_treino$classe,horizontal = T)
boxplot(base_treino$acido_citrico ~ base_treino$classe,horizontal = T)
boxplot(base_treino$acucar_residual ~ base_treino$classe,horizontal = T)
boxplot(base_treino$cloretos ~ base_treino$classe,horizontal = T)
boxplot(base_treino$dioxido_de_enxofre_livre ~ base_treino$classe,horizontal = T)
boxplot(base_treino$total.sulfur.total ~ base_treino$classe,horizontal = T)
boxplot(base_treino$pH ~ base_treino$classe,horizontal = T)
boxplot(base_treino$sulfatos ~ base_treino$classe,horizontal = T)
boxplot(base_treino$alcool ~ base_treino$classe,horizontal = T)

#relação entre duas variaveis quantitativas
tabela_tipo_classe = table(base_treino$classe,base_treino$tipo)
barplot(tabela_tipo_classe,legend.text = rownames(tabela_tipo_classe))
tabela_tipo_classe[,1] = tabela_tipo_classe[,1]/sum(tabela_tipo_classe[,1])
tabela_tipo_classe[,2] = tabela_tipo_classe[,2]/sum(tabela_tipo_classe[,2])
barplot(tabela_tipo_classe,legend.text = rownames(tabela_tipo_classe))

### Padronizacao das var independentes quantitativas
X_quanti = base_treino |> select(acidez_volatil,
                                 acido_citrico,
                                 acucar_residual,
                                 cloretos,
                                 dioxido_de_enxofre_livre,
                                 pH,
                                 sulfatos,
                                 alcool) |> scale()
class(X_quanti)
dim(X_quanti)
colnames(X_quanti)



### Tratamento nas variaveis qualitativas, inclusive a var resposta
X_quali = model.matrix(~ . ,data = base_treino |> select(tipo,classe))
class(X_quali)
dim(X_quali)
colnames(X_quali)
classe1 = !X_quali[,"classe2"] & !X_quali[,"classe3"]
X_quali = cbind(X_quali,classe1)
head(X_quali)

### Treinamento de uma RNA
library(neuralnet)



set.seed(123456789)
h1 = Sys.time()
h1

RNA_1_ = neuralnet(classe1 + classe2 + classe3 ~ 
                     acidez_volatil + 
                     acido_citrico + 
                     acucar_residual + 
                     cloretos + 
                     dioxido_de_enxofre_livre + 
                     pH + 
                     sulfatos + 
                     alcool + 
                     tipowhite,
                   linear.output = F, # Classification
                   data = cbind(X_quali,X_quanti),
                   hidden = 1)
h2 = Sys.time()
h2-h1
saveRDS(object = RNA_1_,file = "RNA_1_.RDS")

plot(RNA_1_)
pred_treino_1_ = predict(RNA_1_,newdata=cbind(X_quali,X_quanti))

pred_classe_treino_1 = apply(pred_treino_1_,MARGIN = 1,FUN = "which.max")
classe_real_treino = base_treino$classe

library(caret)
confusionMatrix(data=as.factor(pred_classe_treino_1),reference=classe_real_treino)


set.seed(123456789)
h1 = Sys.time()
h1
RNA_2_ = neuralnet(classe1 + classe2 + classe3 ~ 
                     acidez_volatil + 
                     acido_citrico + 
                     acucar_residual + 
                     cloretos + 
                     dioxido_de_enxofre_livre + 
                     pH + 
                     sulfatos + 
                     alcool + tipowhite,
                   linear.output = F, # Classification
                   data = cbind(X_quali,X_quanti),
                   hidden = 2)
h2 = Sys.time()
h2-h1
saveRDS(object = RNA_2_,file = "RNA_2_.RDS")
#



set.seed(123456789)
h1 = Sys.time()
h1
RNA_3_ = neuralnet(classe1 + classe2 + classe3 ~  
                     acidez_volatil + 
                     acido_citrico + 
                     acucar_residual + 
                     cloretos + 
                     dioxido_de_enxofre_livre + 
                     pH + 
                     sulfatos + 
                     alcool + tipowhite,
                   linear.output = F, # Classification
                   data = cbind(X_quali,X_quanti),
                   hidden = 3)
h2 = Sys.time()
h2-h1
saveRDS(object = RNA_3_,file = "RNA_3_.RDS")
#


set.seed(123456789)
h1 = Sys.time()
h1
RNA_4_ = neuralnet(classe1 + classe2 + classe3 ~  
                     acidez_volatil + 
                     acido_citrico + 
                     acucar_residual + 
                     cloretos + 
                     dioxido_de_enxofre_livre + 
                     pH + 
                     sulfatos + 
                     alcool + tipowhite,
                   linear.output = F, # Classification
                   data = cbind(X_quali,X_quanti),
                   hidden = 4)
h2 = Sys.time()
h2-h1
saveRDS(object = RNA_4_,file = "RNA_4_.RDS")
#


set.seed(123456789)
h1 = Sys.time()
h1
RNA_2_1_ = neuralnet(classe1 + classe2 + classe3 ~
                       acidez_volatil + 
                       acido_citrico + 
                       acucar_residual + 
                     cloretos + 
                     dioxido_de_enxofre_livre + 
                     pH + 
                     sulfatos + 
                     alcool + tipowhite,
                   linear.output = F, # Classification
                   data = cbind(X_quali,X_quanti),
                   hidden = c(2,1))
h2 = Sys.time()
h2-h1
saveRDS(object = RNA_2_1_,file = "RNA_2_1_.RDS")
#



set.seed(123456789)
h1 = Sys.time()
h1
RNA_2_2_ = neuralnet(classe1 + classe2 + classe3 ~ 
                       acidez_volatil + 
                       acido_citrico + 
                       acucar_residual + 
                       cloretos + 
                       dioxido_de_enxofre_livre + 
                       pH + 
                       sulfatos + 
                       alcool + tipowhite,
                     linear.output = F, # Classification
                     data = cbind(X_quali,X_quanti),
                     hidden = c(2,2))
h2 = Sys.time()
h2-h1
saveRDS(object = RNA_2_2_,file = "RNA_2_2_.RDS")
#




set.seed(123456789)
h1 = Sys.time()
h1
RNA_2_3_ = neuralnet(classe1 + classe2 + classe3 ~ 
                       acidez_volatil + 
                       acido_citrico + 
                       acucar_residual + 
                       cloretos + 
                       dioxido_de_enxofre_livre + 
                       pH + 
                       sulfatos + 
                       alcool + tipowhite,
                     linear.output = F, # Classification
                     data = cbind(X_quali,X_quanti),
                     hidden = c(2,3),
                     stepmax =  1e+06)
h2 = Sys.time()
h2-h1
saveRDS(object = RNA_2_3_,file = "RNA_2_3_.RDS")
#


set.seed(123456789)
h1 = Sys.time()
h1
RNA_3_3_ = neuralnet(classe1 + classe2 + classe3 ~ 
                       acidez_volatil + 
                       acido_citrico + 
                       acucar_residual + 
                       cloretos + 
                       dioxido_de_enxofre_livre + 
                       pH + 
                       sulfatos + 
                       alcool + tipowhite,
                     linear.output = F, # Classification
                     data = cbind(X_quali,X_quanti),
                     hidden = c(3,3),
                     stepmax =  1e+06)
h2 = Sys.time()
h2-h1
saveRDS(object = RNA_3_3_,file = "RNA_3_3_.RDS")
#