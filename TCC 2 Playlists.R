##### install ####
# install.packages("dplyr")
# install.packages("car")
# install.packages("lme4")
# install.packages("readr")
# install.packages('data.table')
# install.packages("tidyverse")
# install.packages("plotly")
#install.packages("foreign")
#install.packages("nnet")
#install.packages("ggplot2")
#install.packages("reshape2")
#install.packages("caret")
#install.packages("rlang")
#install.packages("InformationValue")
#install.packages("tibble")
#install.packages("Ecdat")
#install.packages("randomForest") preciso atualizar o R
#install.packages("logistf")
#install.packages("xtable")

#urlPackage <- 'https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-14.tar.gz'
#install.packages(urlPackage, repos=NULL, type="source") 
#####
######libraries

library(randomForest)
library(InformationValue)
library(reshape2)
library(dplyr)
library(readr)
library(data.table)
library(plotly)
library(ggplot2)
library(MASS)
library(tibble)
library(caret)
library(Ecdat)
library(boot)
library(logistf)
library(xtable)
#library(nnet)
#library(foreign)




options(scipen=7)

## Playlists escolhidas beast Mode VS Piano Relaxante
## Corrigir boxplots. AGORA SERÃO TODOS BOXPLOTS "DUPLOS". AGRUPAR VARIÁVEIS 
## "SEMELHANTES" TIPO ENERGY, DANCEABILITY, VALENCE E 
## ACOUSTIC, LIVENESS, SPEECHINESS, ETC

## CONFERIR DESCRIÇÃO DE VARIÁVEIS NO TCC1

## ESCOLHER SÓ 2 PLAYLISTS
## TESTES DE MÉDIA
## REG LOG SIMPLES
## RANDOM FORREST
## REDES NEURAIS

## 29/07
## buscar 2 playlists um pouco mais parecidas. Acousticness separa perfeitamente os dados
## seguir com random forrest com essas duas mesmas playlists e redes neurais. Deve ter a repetição do problema. Retirar as variáveis tipo "acousticness"
## depois fazer tudo de novo com outras playlists
## comparar metodologias, desempenho, técnicas.
## verificar se alguma combinação de playlist nos permite não remover 


## olhar a biblioteca caret para separar dados entre treino e teste
## biblioteca kfold
## leave one out

## livro de Rafael data splitting
## capítulo 1 e 2, ideia geral de problemas. Capitulo 7 e 8 métodos de classificação


## Começar a interpretar as variáveis do modelo.

## problemas de identificação na convergência da verossimilhança

###################################################################################################################################################################
###################################################################################################################################################################
##################################### Beast Mode vs Piano Relaxante
###################################################################################################################################################################
###################################################################################################################################################################


###################################################################################################################################################################
##################################### Importação dos dados
###################################################################################################################################################################

# dbbeast <- fread('C:/Users/Camila.cmsb/Downloads/beast_mode.csv', header = T)
# dbpiano <- fread('C:/Users/Camila.cmsb/Downloads/piano_relaxante.csv', header = T)

dbbeast <- fread('C:/Users/55619/Documents/Estatística/TCC/Base de dados/beast_mode.csv', header = T)
dbpiano <- fread('C:/Users/55619/Documents/Estatística/TCC/Base de dados/piano_relaxante.csv', header = T)

dbbeast <- mutate(dbbeast, 'Playlist'='Beast Mode')
dbpiano <- mutate(dbpiano, 'Playlist'='Piano Relaxante')

dbbeast1 <- dplyr::select(dbbeast,"Track URI", "Track Name", "Artist Name(s)", "Track Duration (ms)","Explicit", "Popularity", "Artist Genres":"Time Signature", "Playlist")
dbpiano1 <- dplyr::select(dbpiano, "Track URI", "Track Name", "Artist Name(s)", "Track Duration (ms)","Explicit", "Popularity", "Artist Genres":"Time Signature","Playlist")

dbbeast1 <- rename(dbbeast1 , "Duration"=`Track Duration (ms)`)
dbpiano1 <- rename(dbpiano1 , "Duration"=`Track Duration (ms)`)


###################################################################################################################################################################
##################################### Manipulando os dados
###################################################################################################################################################################

g1beast <- data.frame('Speechiness'=dbbeast1$Speechiness, 'Liveness'=dbbeast1$Liveness, 
                      'Instrumentalness'=dbbeast1$Instrumentalness, 'Acousticness'=dbbeast1$Acousticness, 
                      'Playlist'=dbbeast1$Playlist, 'ID'=dbbeast1$`Track URI`)

g2beast <- data.frame('Valence'=dbbeast1$Valence, 'Energy'=dbbeast1$Energy, 
                      'Danceability'=dbbeast1$Danceability, 
                      'Playlist'=dbbeast1$Playlist, 'ID'=dbbeast1$`Track URI`)

g3beast<- data.frame('BPM'=dbbeast1$Tempo, 'Playlist'=dbbeast1$Playlist, 'ID'=dbbeast1$`Track URI`)
g4beast<- data.frame('Duration'=dbbeast1$Duration, 'Playlist'=dbbeast1$Playlist, 'ID'=dbbeast1$`Track URI`)
g5beast<- data.frame('Loudness'=dbbeast1$Loudness, 'Playlist'=dbbeast1$Playlist, 'ID'=dbbeast1$`Track URI`)

g1piano <- data.frame('Speechiness'=dbpiano1$Speechiness, 'Liveness'=dbpiano1$Liveness, 
                      'Instrumentalness'=dbpiano1$Instrumentalness, 'Acousticness'=dbpiano1$Acousticness, 
                      'Playlist'=dbpiano1$Playlist, 'ID'=dbpiano1$`Track URI`)

g2piano <- data.frame('Valence'=dbpiano1$Valence, 'Energy'=dbpiano1$Energy, 
                      'Danceability'=dbpiano1$Danceability, 
                      'Playlist'=dbpiano1$Playlist, 'ID'=dbpiano1$`Track URI`)
g3piano<- data.frame('BPM'=dbpiano1$Tempo, 'Playlist'=dbpiano1$Playlist, 'ID'=dbpiano1$`Track URI`)
g4piano<- data.frame('Duration'=dbpiano1$Duration, 'Playlist'=dbpiano1$Playlist, 'ID'=dbpiano1$`Track URI`)
g5piano<- data.frame('Loudness'=dbpiano1$Loudness, 'Playlist'=dbpiano1$Playlist, 'ID'=dbpiano1$`Track URI`)

g1<-data.frame(rbind(g1beast, g1piano))
g2<-data.frame(rbind(g2beast, g2piano))
g3<-data.frame(rbind(g3beast, g3piano))
g4<-data.frame(rbind(g4beast, g4piano))
g5<-data.frame(rbind(g5beast, g5piano))

g1reshaped <-reshape2::melt(g1)
g2reshaped <-reshape2::melt(g2)
g3reshaped <-reshape2::melt(g3)
g4reshaped <-reshape2::melt(g4)
g5reshaped <-reshape2::melt(g5)


###################################################################################################################################################################
##################################### Boxplots
###################################################################################################################################################################


## Decidir qual estilo de boxplot utilizar
## 1 legenda no topo. Mais espaço horizontal pro gráfico
ggplot(g1reshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + coord_flip() + theme(legend.position = "top")
## 2 legenda na direta. Mais "clássico"
ggplot(g1reshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + coord_flip() + theme_classic()

ggplot(g2reshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + coord_flip() + theme_classic()

ggplot(g3reshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + theme_classic()

ggplot(g4reshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + theme_classic()

ggplot(g5reshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + theme_classic()

## Através dos boxplots já podemos identificar um possível problema para nossos métodos de classificação.
## As variáveis Acousticness, Energy e Loudness separam nossos dados completamente. 
## Com certeza nosso modelo de regressão logística não vai convergir se usarmos qualquer uma das 3 variáveis


###################################################################################################################################################################
##################################### União e partição do banco
###################################################################################################################################################################

db1<-data.frame(rbind(dbbeast1, dbpiano1))
dbfac<-db1

dbfac$Playlist <-as.factor(dbfac$Playlist)
dbfac$Playlist <-relevel(dbfac$Playlist, ref="Beast Mode")

set.seed(12)

amostra <- sample(c(TRUE, FALSE), nrow(dbfac), replace=TRUE, prob=c(0.7,0.3))
train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]

table(amostra)

###################################################################################################################################################################
##################################### Regressão Logística - Modelos
###################################################################################################################################################################

## Beast Mode VS Piano Relaxante

##################################### Modelo 1
###################################################################################################################################################################
## Todas as variáveis
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
modelrg1<- glm(Playlist ~ Acousticness + Danceability + Duration + Energy
               + Instrumentalness + Key + Liveness + Loudness + Mode
               + Speechiness + Tempo + Valence
               ,data=train, family='binomial')
summary(modelrg1)


xtable(summary(modelrg1), auto = F)
#xtable(summary(modelrg1), auto = T)

#modelrg1<- glm(Playlist ~ Acousticness + Danceability + Duration + Energy
#               + Instrumentalness + Key + Liveness + Loudness + Mode
#               + Speechiness + Tempo + Valence
#               ,data=train, family='binomial', maxit=1)
#summary(modelrg1)

rg1.treino <- ifelse(modelrg1$fitted.values > 0.5, "Piano Relaxante", "Beast Mode")

table(rg1.treino, train$Playlist)

xtable(table(rg1.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 1", label = "par 1 treino mod 1 reg log")

## Como previsto anteriormente, nosso modelo de regressão logística retornou valores estranhos de erro e z 
## graças à não convergência do nosso algoritmo.
## Com base nos boxplots, as variáveis Acousticness. Energy e Loudness serão removidas do modelo 
## por separarem completamente nossos dados.
## Independente disso, o modelo obteve um resultado perfeito no seu treino


## AIC = 26

# Para ver a probabilidade de cada observação dentro do modelo
#y_pred = predict(modelrg1, dbfac, type="response")
#df <- data.frame(dbfac$Playlist,y_pred)

##################################### Modelo 2
###################################################################################################################################################################
## Removendo as variáveis Acousticness, Energy, Instrumentalness e Loudness
## Danceability, Duration, Key, Liveness, Mode, Speechiness, Tempo, Valence
modelrg2<- glm(Playlist ~ Danceability + Duration
               + Key + Liveness + Mode 
               + Speechiness + Tempo + Valence
               ,data=train, family='binomial')
summary(modelrg2)

xtable(summary(modelrg2), auto = F)

rg2.treino <- ifelse(modelrg2$fitted.values > 0.5, "Piano Relaxante", "Beast Mode")

table(rg2.treino, train$Playlist)

xtable(table(rg2.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 2", label = "par 1 treino mod 2 reg log")

## Modelo converge
## Removendo Instrumentalness nosso modelo já passa a convergir
## e indica Danceability com p-valor inferior a 0,001 enquanto que Duration e Speechiness possuem p-valor inferior a 0,01
## Além de Liveness e Tempo com p-valor inferior a 0,05
## O modelo resultado muito bom no seu treino. Erro de 1,54%.

## AIC = 52,953

##################################### Modelo 3
###################################################################################################################################################################
## Apenas variáveis significantes para o modelo que converge.
## Danceability, Duration, Liveness, Speechiness, Tempo
modelrg3<- glm(Playlist ~ Danceability + Duration
               + Liveness
               + Speechiness + Tempo
               ,data=train, family='binomial')
summary(modelrg3)

xtable(summary(modelrg3), auto = F)

rg3.treino <- ifelse(modelrg3$fitted.values > 0.5, "Piano Relaxante", "Beast Mode")

table(rg3.treino, train$Playlist)

xtable(table(rg3.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 3", label = "par 1 treino mod 3 reg log")

## Modelo converge, não tem probabilidades iguais a 0 (zero) ou 1 (um)
## O treino do modelo teve resultado muito bom. Erro de 1,8%

## AIC = 52,437


###################################################################################################################################################################
##################################### Regressão Logística - Curiosidade - Probabilidade de cada obs no modelo
###################################################################################################################################################################
# Para ver a probabilidade de cada observação dentro do modelo
#y_pred = predict(modelrg4, dbfac, type="response")
#df <- data.frame(dbfac$Playlist,y_pred)


###################################################################################################################################################################
##################################### Regressão Logística - Calculando Previsões
###################################################################################################################################################################

# Predição do banco teste utilizando cada modelo

##################################### Modelo 1
###################################################################################################################################################################
rg1.prob <- predict(modelrg1, test, type = "response")
rg1.pred <- ifelse(rg1.prob > 0.5, "Piano Relaxante", "Beast Mode")

##################################### Modelo 2
###################################################################################################################################################################
rg2.prob <- predict(modelrg2, test, type = "response")
rg2.pred <- ifelse(rg2.prob > 0.5, "Piano Relaxante", "Beast Mode")

##################################### Modelo 3
###################################################################################################################################################################
rg3.prob <- predict(modelrg3, test, type = "response")
rg3.pred <- ifelse(rg3.prob > 0.5, "Piano Relaxante", "Beast Mode")


###################################################################################################################################################################
##################################### Regressão Logística - Comparando Previsões
###################################################################################################################################################################

## Criando uma variável auxiliar para conseguir demonstrar os resultados de tabelas de confusão e curva ROC próximos
#################################################################################################################################
test<-mutate(test, Playlist1=Playlist)
train<-mutate(train, Playlist1=Playlist)

test$Playlist1 <-ifelse(test$Playlist=="Piano Relaxante", 1, 0)
train$Playlist1 <-ifelse(train$Playlist=="Piano Relaxante", 1, 0)
#################################################################################################################################

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

# Previsão do banco teste
table(rg1.pred, test$Playlist)

xtable(table(rg1.pred, test$Playlist), caption = "Matriz de confusão do teste do modelo 1", label = "par 1 teste mod 1 reg log")

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg1.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg1.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg1.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg1.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg1.prob)
# Curva ROC
plotROC(test$Playlist1, rg1.prob)

# Acerto da previsão
mean((rg1.pred ==test$Playlist))
# Erro da previsão
1 - mean((rg1.pred ==test$Playlist))


## A previsão do primeiro modelo é perfeita


##################################### Modelo 2
###################################################################################################################################################################
## Danceability, Duration, Key, Liveness, Mode, Speechiness, Tempo, Valence

# Previsão do banco teste
table(rg2.pred, test$Playlist)

xtable(table(rg2.pred, test$Playlist), caption = "Matriz de confusão do teste do modelo 2", label = "par 1 teste mod 2 reg log")

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg2.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg2.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg2.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg2.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg2.prob)
# Curva ROC
plotROC(test$Playlist1, rg2.prob)

# Acerto da previsão
mean((rg2.pred ==test$Playlist))
# Erro da previsão
1 - mean((rg2.pred ==test$Playlist))

## Nosso primeiro modelo a convergir possui previsão quase perfeita. Erro de 1,9%


##################################### Modelo 3
###################################################################################################################################################################
## Danceability, Duration, Liveness, Speechiness, Tempo

# Previsão do banco teste
table(rg3.pred, test$Playlist)

xtable(table(rg3.pred, test$Playlist), caption = "Matriz de confusão do teste do modelo 3", label = "par 1 teste mod 3 reg log")

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg3.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg3.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg3.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg3.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg3.prob)
# Curva ROC
plotROC(test$Playlist1, rg3.prob)

# Acerto da previsão
mean((rg3.pred ==test$Playlist))
# Erro da previsão
1 - mean((rg3.pred ==test$Playlist))

## Nosso modelo utilizando apenas as variáveis significativas do primeiro modelo a convergir tem previsão quase perfeita. Erro de 1,9%

## Acredito que Especificidade e Sensibilidade estão invertidos. Setando o fator como valor de referência ele vira o evento 0,
## e as estatísticas são calculadas em relação ao evento 1


###################################################################################################################################################################
##################################### Resetando os bancos de treino e teste
###################################################################################################################################################################

train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]


###################################################################################################################################################################
##################################### Random Forest - Modelos
###################################################################################################################################################################

## https://www.r-bloggers.com/2018/01/how-to-implement-random-forests-in-r/

## Replicarei os mesmos modelos do método de Regressão Logística agora no método de Random Forest

## Beast Mode VS Piano Relaxante


##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
set.seed(12)
modelrf1 <- randomForest(Playlist ~ Acousticness + Danceability + Duration + Energy 
                         + Instrumentalness + Key + Liveness + Loudness + Mode 
                         + Speechiness + Tempo + Valence
                         ,data = train, importance = TRUE)
modelrf1

xtable(modelrf1$confusion, caption = "Matriz de confusão random forest do treino do modelo 1", label = "par 1 treino mod 1 ran for")
xtable(t(modelrf1$confusion), caption = "Matriz de confusão random forest do treino do modelo 1", label = "par 1 treino mod 1 ran for")

par(mar = c(3,3,2,10), xpd=T)
plot(modelrf1, main = NA)
legend(x="right",inset = c(-0.585,0),
       legend= c("Média", "Beast Mode", "Piano Relaxante"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

## Novamente o modelo com todas as variáveis é perfeito.
## Vamos agora testar a remoção das mesmas variáveis de antes, na mesma ordem

## Verificando as variáveis importantes para o modelo de Random Forest

modelrf1$importance
importance(modelrf1)

varImpPlot(modelrf1, main = NA)

## Novamente as variáveis Energy, Acousticness e Loudness são as mais importantes para a previsão
## Vamos removê-las do modelo e testar os resultados

##################################### Modelo 2
###################################################################################################################################################################
## Danceability, Duration, Key, Liveness, Mode, Speechiness, Tempo, Valence
set.seed(12)
modelrf2 <- randomForest(Playlist ~ Danceability + Duration 
                         + Key + Liveness + Mode 
                         + Speechiness + Tempo + Valence
                         ,data = train, importance = TRUE)
modelrf2

xtable(modelrf2$confusion, caption = "Matriz de confusão random forest do treino do modelo 2", label = "par 1 treino mod 2 ran for")
xtable(t(modelrf2$confusion), caption = "Matriz de confusão random forest do treino do modelo 2", label = "par 1 treino mod 2 ran for")

par(mar = c(4,4,2,10), xpd=T)
plot(modelrf2, main = NA)
legend(x="right",inset = c(-0.62,0),
       legend= c("Média", "Beast Mode", "Piano Relaxante"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

## A previsão deixa de ser perfeita como no método de regressão logística, mas continua muito boa

##
importance(modelrf2)
varImpPlot(modelrf2, main = NA)

## Removeremos agora a variável Danceability

##################################### Modelo 3
###################################################################################################################################################################
## Danceability, Duration, Liveness, Speechiness, Tempo
set.seed(12)
modelrf3 <- randomForest(Playlist ~ Danceability + Duration
                         + Liveness
                         + Speechiness + Tempo
                         ,data = train, importance = TRUE)
modelrf3

xtable(modelrf3$confusion, caption = "Matriz de confusão random forest do treino do modelo 3", label = "par 1 treino mod 3 ran for")
xtable(t(modelrf3$confusion), caption = "Matriz de confusão random forest do treino do modelo 3", label = "par 1 mod 3 treino ran for")


par(mar = c(4,4,2,10), xpd=T)
plot(modelrf3, main = NA)
legend(x="right",inset = c(-0.62,0),
       legend= c("Média", "Beast Mode", "Piano Relaxante"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

## Semelhante ao modelo de regressão logística, nosso modelo de RF ainda se comporta muito bem

##
importance(modelrf3)
varImpPlot(modelrf3, main = NA)
## Importante lembrar que o método de Random Forest tem o fator de aleatoriedade entao os resultados não são sempre iguais para os modelos


###################################################################################################################################################################
##################################### Random Forest - Previsões
###################################################################################################################################################################


##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

# Resultado do modelo com o banco de treino
modelrf1$confusion

t(modelrf1$confusion)

# Previsão perfeita

# Previsão do modelo no banco de teste
predValid1 <- predict(modelrf1, test, type = "class")
table(predValid1, test$Playlist)
xtable(table(predValid1, test$Playlist), caption = "Matriz de confusão random forest da previsão do modelo 1", label = "par 1 teste mod 1 ran for")

# Acerto da previsão
mean((predValid1 ==test$Playlist))
# Erro da previsão
1 - mean((predValid1 ==test$Playlist))


# Resultado de previsão perfeito, assim como no REG LOG

##################################### Modelo 2
###################################################################################################################################################################
## Danceability, Duration, Key, Liveness, Mode, Speechiness, Tempo, Valence

# Resultado do modelo com o banco de treino
modelrf2$confusion

t(modelrf2$confusion)

# Resultado de previsão quase perfeito. Diferente do resultado deste mesmo modelo na regressão logística

# Previsão do modelo no banco de teste
predValid2 <- predict(modelrf2, test, type = "class")
table(predValid2, test$Playlist)
xtable(table(predValid2, test$Playlist), caption = "Matriz de confusão random forest da previsão do modelo 2.", label = "par 1 teste mod 2 ran for")

# Acerto da previsão
mean((predValid2 ==test$Playlist))
# Erro da previsão
1-mean((predValid2 ==test$Playlist))

# Resultado de previsão quase perfeito. Erro de 1,9%

##################################### Modelo 3
###################################################################################################################################################################
## Danceability, Duration, Liveness, Speechiness, Tempo

# Resultado do modelo com o banco de treino
modelrf3$confusion

t(modelrf3$confusion)

# Resultado de previsão muito bom

# Previsão do modelo no banco de teste
predValid3 <- predict(modelrf3, test, type = "class")
table(predValid3, test$Playlist)
xtable(table(predValid3, test$Playlist), caption = "Matriz de confusão random forest da previsão do modelo 3.", label = "par 1 teste mod 3 ran for")

# Acerto da previsão
mean((predValid3 ==test$Playlist))
# Erro da previsão
1-mean((predValid3 ==test$Playlist))

# Resultado de previsão quase perfeito. Erro de 1,9%


###################################################################################################################################################################
##################################### Redes Neurais
###################################################################################################################################################################

## Beast Mode VS Piano Relaxante


# sad boys 
# :(

###################################################################################################################################################################
##################################### Validação Cruzada
###################################################################################################################################################################

## Beast Mode VS Piano Relaxante


###################################################################################################################################################################
##################################### K-Folds
###################################################################################################################################################################
## https://www.statology.org/k-fold-cross-validation-in-r/
## https://rforhr.com/kfold.html

## Criando a função kf que define o tipo de validação cruzada utilizada. Neste momento, estamos usando o método k-folds
## Vamos testar a diferença entre repetições no k-folds e sem repetições.
kf <- trainControl(method="cv", number=10, savePredictions="all", classProbs=TRUE)
#kf10 <- trainControl(method="repeatedcv", number=10, savePredictions="all", classProbs=TRUE, repeats = 10)


###################################################################################################################################################################
##################################### Resetando bases e manipulando dados
###################################################################################################################################################################

## o método de validação cruzada, por algum motivo exige que os fatores tenham apenas 1 palavra

## usar a validação cruzada no banco completo ou só na amostra de treino 
## e fazer uma nova previsão do banco de teste?

dbfac1<-dbfac
dbfac1$Playlist <-ifelse(dbfac$Playlist=="Piano Relaxante", "Piano", "Beast")
dbfac1$Playlist <-as.factor(dbfac1$Playlist)


###################################################################################################################################################################
##################################### K-Folds - Regressão Logística
###################################################################################################################################################################


##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
kmodelrg1<- train(Playlist ~ Acousticness + Danceability + Duration 
                + Energy + Instrumentalness + Key + Liveness + Loudness + Mode 
                + Speechiness + Tempo + Valence
                ,data=dbfac1, method="glm", family='binomial', trControl=kf)

summary(kmodelrg1)
xtable(summary(kmodelrg1))
## AIC = 26

kmodelrg1$resampledCM
kfrg1<-kmodelrg1$resampledCM[,-5:-6]

rownames(kfrg1)<-c("Fold 1","Fold 2","Fold 3","Fold 4","Fold 5","Fold 6","Fold 7","Fold 8","Fold 9","Fold 10")
colnames(kfrg1)<-c("Acerto","Erro","Erro","Acerto")
kfrg1
xtable(kfrg1, caption = "Resultados dos 10-folds do modelo 1", label = "par 1 kfolds mod 1 reg log")

table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])
xtable(table(kmodelrg1$pred[,1], kmodelrg1$pred[,2]), caption = "Resultados dos 10-folds do modelo 1", label = "par 1 kfolds mod 1 reg log")


table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


kmodelrg1$results[,-1]
xtable(kmodelrg1$results[,-1])
1-kmodelrg1$results[2]

## Accuracy de 100% de acerto na previsão média de todas as instâncias
## Kappa de 100%. 100% - perfeita

## Interpretação de Kappa
##       100% Kappa perfeita distinção
##  99% - 81% Kappa quase perfeita
##  80% - 61% Kappa substancial
##  60% - 41% Kappa moderada
##  40% - 21% Kappa leve
##  20% -  1% Kappa muito leve
##         0% Kappa sem distinção

##################################### Modelo 2
###################################################################################################################################################################
## Danceability, Duration, Key, Liveness, Mode, Speechiness, Tempo, Valence
kmodelrg2<- train(Playlist ~ Danceability + Duration 
                + Key + Liveness + Mode 
                + Speechiness + Tempo + Valence
                ,data=dbfac1, method="glm", family='binomial', trControl=kf)

summary(kmodelrg2)
xtable(summary(kmodelrg2))
## AIC = 74,967


kmodelrg2$resampledCM
kfrg2<-kmodelrg2$resampledCM[,-5:-6]

rownames(kfrg2)<-c("Fold 1","Fold 2","Fold 3","Fold 4","Fold 5","Fold 6","Fold 7","Fold 8","Fold 9","Fold 10")
colnames(kfrg2)<-c("Acerto","Erro","Erro","Acerto")
kfrg2
xtable(kfrg2, caption = "Resultados dos 10-folds do modelo 2", label = "par 1 kfolds mod 2 reg log")

table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])
xtable(table(kmodelrg2$pred[,1], kmodelrg2$pred[,2]), caption = "Resultados dos 10-folds do modelo 2", label = "par 1 kfolds mod 2 reg log")


table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


kmodelrg2$results[,-1]
xtable(kmodelrg2$results[,-1])
1-kmodelrg2$results[2]
## Accuracy de 97,27% de acerto na previsão média de todas as instâncias
## Kappa de 94,13%%. Entre 81% e 99% - quase perfeita
## Erro de 2,73%

##################################### Modelo 3
###################################################################################################################################################################
## Danceability, Duration, Liveness, Speechiness, Tempo
kmodelrg3<- train(Playlist ~ Danceability + Duration 
                + Liveness
                + Speechiness + Tempo
                ,data=dbfac1, method="glm", family='binomial', trControl=kf)

summary(kmodelrg3)
xtable(summary(kmodelrg3))
## AIC = 76,715

kmodelrg3$resampledCM
kfrg3<-kmodelrg3$resampledCM[,-5:-6]

rownames(kfrg3)<-c("Fold 1","Fold 2","Fold 3","Fold 4","Fold 5","Fold 6","Fold 7","Fold 8","Fold 9","Fold 10")
colnames(kfrg3)<-c("Acerto","Erro","Erro","Acerto")
kfrg3
xtable(kfrg3, caption = "Resultados dos 10-folds do modelo 3", label = "par 1 kfolds mod 3 reg log")

table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])
xtable(table(kmodelrg3$pred[,1], kmodelrg3$pred[,2]), caption = "Resultados dos 10-folds do modelo 3", label = "par 1 kfolds mod 3 reg log")


table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


kmodelrg3$results[-1]
xtable(kmodelrg3$results[,-1])
1-kmodelrg3$results[2]
## Accuracy de 98,18% de acerto na previsão média de todas as instâncias
## Kappa de 96,07%%. Entre 81% e 99% - quase perfeita
## Erro de 1,82%

###################################################################################################################################################################
##################################### K-Folds - Random Forest
###################################################################################################################################################################

## https://rdrr.io/github/marccamb/optiranger/man/rf.kfold.html

## https://rpubs.com/jvaldeleon/forest_repeat_cv

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
set.seed(12)
kmodelrf1<- train(Playlist ~ Acousticness + Danceability + Duration + Energy
                + Instrumentalness + Key + Liveness + Loudness + Mode
                + Speechiness + Tempo + Valence
                ,data=dbfac1, method="rf", family='binomial', trControl=kf)

kmodelrf1$finalModel$confusion
xtable(t(kmodelrf1$finalModel$confusion))

varImp(kmodelrf1)
plot(varImp(kmodelrf1), main = NA)

plot((kmodelrf1$finalModel), main = NA)
legend(x="top",
       legend= c("Média", "Beast Mode", "Piano Relaxante"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

kmodelrf1$resampledCM

kfrf1<-kmodelrf1$resampledCM[order(kmodelrf1$resampledCM$mtry),]

kfrf1<-kfrf1[1:10,-6]


rownames(kfrf1)<-c()
colnames(kfrf1)<-c()
rownames(kfrf1)<-c("Fold 1","Fold 2","Fold 3","Fold 4","Fold 5","Fold 6","Fold 7","Fold 8","Fold 9","Fold 10")
colnames(kfrf1)<-c("Acerto","Erro","Erro","Acerto", "mtry")
kfrf1
xtable(kfrf1, caption = "Resultados dos 10-folds do modelo 1", label = "par 1 kfolds mod 1 ran for")

table(kmodelrf1$pred[,1], kmodelrf1$pred[,2])
xtable(table(kmodelrf1$pred[,1], kmodelrf1$pred[,2]), caption = "Resultados dos 10-folds do modelo 1", label = "par 1 kfolds mod 1 ran for")

kmodelrf1

ab<-list()
ab<-kmodelrf1$pred
ab<-ab[order(ab$mtry),]

kmodelrf1mtry2<-list()
kmodelrf1mtry2<-ab[1:547, ]

## Melhor previsão de regressão linear foi feita pelo primeiro modelo

table(kmodelrf1mtry2[,1], kmodelrf1mtry2[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf1mtry2[,1], kmodelrf1mtry2[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf1mtry2[,1], kmodelrf1mtry2[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf1mtry2[,1], kmodelrf1mtry2[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


kmodelrf1$results
xtable(kmodelrf1$results)
1-kmodelrf1$results[2]
## Accuracy de 100% de acerto na previsão média de todas as instâncias
## Kappa de 100%. 100% - perfeita
## Erro de 0


##################################### Modelo 2
###################################################################################################################################################################
## Danceability, Duration, Key, Liveness, Mode, Speechiness, Tempo, Valence
set.seed(12)
kmodelrf2<- train(Playlist ~ Danceability + Duration 
                + Key + Liveness + Mode 
                + Speechiness + Tempo + Valence
                ,data=dbfac1, method="rf", family='binomial', trControl=kf)

kmodelrf2

kmodelrf2$finalModel$confusion

xtable(t(kmodelrf2$finalModel$confusion))

varImp(kmodelrf2)
plot(varImp(kmodelrf2), main = NA)

plot((kmodelrf2$finalModel), main = NA)
legend(x="top",
       legend= c("Média", "Beast Mode", "Piano Relaxante"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

kmodelrf2$resampledCM

kfrf2<-kmodelrf2$resampledCM[order(kmodelrf2$resampledCM$mtry),]

kfrf2<-kfrf2[11:20,-6]


rownames(kfrf2)<-c()
colnames(kfrf2)<-c()
rownames(kfrf2)<-c("Fold 1","Fold 2","Fold 3","Fold 4","Fold 5","Fold 6","Fold 7","Fold 8","Fold 9","Fold 10")
colnames(kfrf2)<-c("Acerto","Erro","Erro","Acerto", "mtry")
kfrf2
xtable(kfrf2, caption = "Resultados dos 10-folds do modelo 2", label = "par 1 kfolds mod 2 ran for")

table(kmodelrf2$pred[,1], kmodelrf2$pred[,2])
xtable(table(kmodelrf2$pred[,1], kmodelrf2$pred[,2]), caption = "Resultados dos 10-folds do modelo 2", label = "par 1 kfolds mod 2 ran for")

kmodelrf2

ab<-list()
ab<-kmodelrf2$pred
ab<-ab[order(ab$mtry),]

kmodelrf2mtry2<-list()
kmodelrf2mtry2<-ab[1:547, ]


## Melhor previsão de regressão linear foi feita pelo primeiro modelo

table(kmodelrf2mtry2[,1], kmodelrf2mtry2[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf2mtry2[,1], kmodelrf2mtry2[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf2mtry2[,1], kmodelrf2mtry2[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf2mtry2[,1], kmodelrf2mtry2[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10

kmodelrf2$results
xtable(kmodelrf2$results)
1-kmodelrf2$results[2]
## Accuracy de 97,79% de acerto na previsão média de todas as instâncias
## Kappa de 95,24%. Entre 81% e 99% - quase perfeita
## Erro de 2,21%


##################################### Modelo 3
###################################################################################################################################################################
## Danceability, Duration, Liveness, Speechiness, Tempo
set.seed(12)
kmodelrf3<- train(Playlist ~ Danceability + Duration 
                + Liveness 
                + Speechiness + Tempo
                ,data=dbfac1, method="rf", family='binomial', trControl=kf)

kmodelrf3

kmodelrf3$finalModel$confusion

xtable(t(kmodelrf3$finalModel$confusion))

varImp(kmodelrf3)
plot(varImp(kmodelrf3), main = NA)

plot((kmodelrf3$finalModel), main = NA)
legend(x="top",
       legend= c("Média", "Beast Mode", "Piano Relaxante"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)


kmodelrf3$resampledCM

kfrf3<-kmodelrf3$resampledCM[order(kmodelrf3$resampledCM$mtry),]

kfrf3<-kfrf3[1:10,-6]


rownames(kfrf3)<-c()
colnames(kfrf3)<-c()
rownames(kfrf3)<-c("Fold 1","Fold 2","Fold 3","Fold 4","Fold 5","Fold 6","Fold 7","Fold 8","Fold 9","Fold 10")
colnames(kfrf3)<-c("Acerto","Erro","Erro","Acerto", "mtry")
kfrf3
xtable(kfrf3, caption = "Resultados dos 10-folds do modelo 3", label = "par 1 kfolds mod 3 ran for")

table(kmodelrf3$pred[,1], kmodelrf3$pred[,2])
xtable(table(kmodelrf3$pred[,1], kmodelrf3$pred[,2]), caption = "Resultados dos 10-folds do modelo 2", label = "par 1 kfolds mod 2 ran for")


kmodelrf3

ab<-list()
ab<-kmodelrf3$pred
ab<-ab[order(ab$mtry),]

kmodelrf3mtry2<-list()
kmodelrf3mtry2<-ab[1:547, ]


## Melhor previsão de regressão linear foi feita pelo primeiro modelo

table(kmodelrf3mtry2[,1], kmodelrf3mtry2[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf3mtry2[,1], kmodelrf3mtry2[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf3mtry2[,1], kmodelrf3mtry2[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf3mtry2[,1], kmodelrf3mtry2[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


kmodelrf3$results
xtable(kmodelrf3$results)
1-kmodelrf3$results[2]
## Accuracy de 97,99% de acerto na previsão média de todas as instâncias
## Kappa de 95,67%. Entre 81% e 99% - quase perfeita
## Erro de 2,01%

###################################################################################################################################################################
##################################### K-Folds - Redes Neurais
###################################################################################################################################################################



###################################################################################################################################################################
##################################### Leave One Out
###################################################################################################################################################################

## LOO manual (possivelmente terei de usar isso nas redes neurais)
#First initialize the output vector as an empty object outside the loop.
#fitted_value <- NULL
#for(i in 1:53){
#  #you did this part right
#  validation<-ironslag[i,]
#  training<-ironslag[-i,]
#  model1<-lm(magnetic ~ chemical, data = training)
#  #when you fit the model, use the newdata argument to predict on a new row
#  #also, fitted_value needs the index [i], so the each loop doesn't overwrite the previous
#  fitted_value[i] <- predict(model1, newdata = validation)}
#
## https://www.geeksforgeeks.org/loocvleave-one-out-cross-validation-in-r-programming/#:~:text=LOOCV(Leave%20One%20Out%20Cross%2DValidation)%20is%20a%20type,considered%20as%20the%20training%20set.
## https://www.statology.org/leave-one-out-cross-validation-in-r/

loo <- trainControl(method="LOOCV", savePredictions= "all", classProbs = TRUE)
#loo <- trainControl(method="LOOCV")

###################################################################################################################################################################
##################################### Leave One Out - Regressão Logística
###################################################################################################################################################################

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

loomodelrg1<- train(Playlist ~ Acousticness + Danceability + Duration 
                  + Energy + Instrumentalness + Key + Liveness + Loudness + Mode 
                  + Speechiness + Tempo + Valence
                  ,data=dbfac1, method="glm", family='binomial', trControl=loo)

summary(loomodelrg1)
xtable(summary(loomodelrg1))
## AIC = 26


table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])
xtable(table(loomodelrg1$pred[,1], loomodelrg1$pred[,2]))


table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10

## Comparar com k-folds e reg log normal


loomodelrg1$results
xtable(loomodelrg1$results[,-1])
1-loomodelrg1$results[2]
## Accuracy de 100% de acerto na previsão média de todas as instâncias
## Kappa de 100%. 100% - perfeita

## Interpretação de Kappa
##       100% Kappa perfeita distinção
##  99% - 81% Kappa quase perfeita
##  80% - 61% Kappa substancial
##  60% - 41% Kappa moderada
##  40% - 21% Kappa leve
##  20% -  1% Kappa muito leve
##         0% Kappa sem distinção

##################################### Modelo 2
###################################################################################################################################################################
## Danceability, Duration, Key, Liveness, Mode, Speechiness, Tempo, Valence
loomodelrg2<- train(Playlist ~ Danceability + Duration 
                  + Key + Liveness + Mode 
                  + Speechiness + Tempo + Valence
                  ,data=dbfac1, method="glm", family='binomial', trControl=loo)

summary(loomodelrg2)
xtable(summary(loomodelrg2))

## AIC = 74,967

table(loomodelrg2$pred[,1], loomodelrg2$pred[,2])
xtable(table(loomodelrg2$pred[,1], loomodelrg2$pred[,2]))

table(loomodelrg2$pred[,1], loomodelrg2$pred[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg2$pred[,1], loomodelrg2$pred[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg2$pred[,1], loomodelrg2$pred[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg2$pred[,1], loomodelrg2$pred[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10

## Comparar com k-folds e reg log normal


loomodelrg2$results
xtable(loomodelrg2$results[,-1])
1-loomodelrg2$results[2]
## Accuracy de 97,07% de acerto na previsão média de todas as instâncias
## Kappa de 93,70%. Entre 81% e 100% - quase perfeita
## Erro de 2,93%

##################################### Modelo 3
###################################################################################################################################################################
## Danceability, Duration, Liveness, Speechiness, Tempo
loomodelrg3<- train(Playlist ~ Danceability + Duration 
                  + Liveness 
                  + Speechiness + Tempo
                  ,data=dbfac1, method="glm", family='binomial', trControl=loo)

summary(loomodelrg3)
xtable(summary(loomodelrg3))

## AIC = 76,715

table(loomodelrg3$pred[,1], loomodelrg3$pred[,2])
xtable(table(loomodelrg3$pred[,1], loomodelrg3$pred[,2]))

table(loomodelrg3$pred[,1], loomodelrg3$pred[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg3$pred[,1], loomodelrg3$pred[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg3$pred[,1], loomodelrg3$pred[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg3$pred[,1], loomodelrg3$pred[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10

## Comparar com k-folds e reg log normal


loomodelrg3$results[,-1]
xtable(loomodelrg3$results[,-1])
1-loomodelrg3$results[2]
## Accuracy de 97,99% de acerto na previsão média de todas as instâncias
## Kappa de 95,66%. Entre 81% e 99% - muito leve
## Erro de 2,01%

###################################################################################################################################################################
##################################### Leave One Out - Random Forest
###################################################################################################################################################################

## https://rdrr.io/github/marccamb/optiranger/man/rf.kfold.html

## https://rpubs.com/jvaldeleon/forest_repeat_cv

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
loomodelrf1<- train(Playlist ~ Acousticness + Danceability + Duration
                  + Energy + Instrumentalness + Key + Liveness + Loudness + Mode
                  + Speechiness + Tempo + Valence
                  ,data=dbfac1, method="rf", family='binomial', trControl=loo)

## demorou 3 minutos pra rodar

loomodelrf1$finalModel

loomodelrf1$finalModel$confusion
xtable(loomodelrf1$finalModel$confusion)

loomodelrf1$finalModel$importance
loomodelrf1

varImp(loomodelrf1)
plot(varImp(loomodelrf1), main = NA)

plot((loomodelrf1$finalModel), main = NA)
legend(x="top",
       legend= c("Média", "Beast Mode", "Piano Relaxante"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)


loomodelrf1

ab<-list()
ab<-loomodelrf1$pred
ab<-ab[order(ab$mtry),]
loomodelrf1mtry2<-list()
loomodelrf1mtry2<-ab[1:547,]


table(loomodelrf1mtry2[,1], loomodelrf1mtry2[,2])
table(loomodelrf1mtry2[,1], loomodelrf1mtry2[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf1mtry2[,1], loomodelrf1mtry2[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf1mtry2[,1], loomodelrf1mtry2[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf1mtry2[,1], loomodelrf1mtry2[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


loomodelrf1$results
xtable(loomodelrf1$results)
1-loomodelrf1$results[2]
## Accuracy de 100% de acerto na previsão média de todas as instâncias
## Kappa de 100%. 100% - perfeita

## Interpretação de Kappa
##       100% Kappa perfeita distinção
##  99% - 81% Kappa quase perfeita
##  80% - 61% Kappa substancial
##  60% - 41% Kappa moderada
##  40% - 21% Kappa leve
##  20% -  1% Kappa muito leve
##         0% Kappa sem distinção


##################################### Modelo 2
###################################################################################################################################################################
## Danceability, Duration, Key, Liveness, Mode, Speechiness, Tempo, Valence
loomodelrf2<- train(Playlist ~ Danceability + Duration 
                  + Key + Liveness + Mode 
                  + Speechiness + Tempo + Valence
                  ,data=dbfac1, method="rf", family='binomial', trControl=loo)

loomodelrf2$finalModel

loomodelrf2$finalModel$confusion
xtable(loomodelrf2$finalModel$confusion)

loomodelrf2$finalModel$importance
loomodelrf2

varImp(loomodelrf2)
plot(varImp(loomodelrf2), main = NA)

plot(loomodelrf2$finalModel, main = NA)
legend(x="top",
       legend= c("Média", "Beast Mode", "Piano Relaxante"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)


loomodelrf2

ab<-list()
ab<-loomodelrf2$pred
ab<-ab[order(ab$mtry),]
loomodelrf2mtry2<-list()
loomodelrf2mtry2<-ab[1:547,]


table(loomodelrf2mtry2[,1], loomodelrf2mtry2[,2])
table(loomodelrf2mtry2[,1], loomodelrf2mtry2[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf2mtry2[,1], loomodelrf2mtry2[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf2mtry2[,1], loomodelrf2mtry2[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf2mtry2[,1], loomodelrf2mtry2[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


loomodelrf2$results
xtable(loomodelrf2$results)
1-loomodelrf2$results[2]
## Accuracy de 100% de acerto na previsão média de todas as instâncias
## Kappa de 100%. 100% - perfeita
## Erro de 2,38%

##################################### Modelo 3
###################################################################################################################################################################
## Danceability, Duration, Liveness, Speechiness, Tempo
loomodelrf3<- train(Playlist ~ Danceability + Duration 
                  + Liveness 
                  + Speechiness + Tempo
                  ,data=dbfac1, method="rf", family='binomial', trControl=loo)

loomodelrf3$finalModel

loomodelrf3$finalModel$confusion
xtable(loomodelrf3$finalModel$confusion)

loomodelrf3$finalModel$importance

varImp(loomodelrf3)
plot(varImp(loomodelrf3), main = NA)

plot(loomodelrf3$finalModel, main = NA)
legend(x="top",
       legend= c("Média", "Beast Mode", "Piano Relaxante"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

loomodelrf3

ab<-list()
ab<-loomodelrf3$pred
ab<-ab[order(ab$mtry),]
loomodelrf3mtry2<-list()
loomodelrf3mtry2<-ab[1:547,]


table(loomodelrf3mtry2[,1], loomodelrf3mtry2[,2])
table(loomodelrf3mtry2[,1], loomodelrf3mtry2[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf3mtry2[,1], loomodelrf3mtry2[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf3mtry2[,1], loomodelrf3mtry2[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf3mtry2[,1], loomodelrf3mtry2[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


loomodelrf3$results
xtable(loomodelrf3$results)
1-loomodelrf3$results[2]
## Accuracy de 100% de acerto na previsão média de todas as instâncias
## Kappa de 100%. 100% - perfeita
## Erro de 2,19%

## testar com leave one out e ver se os métodos kfolds e leave one out
## concordam entre os modelos testados (ex: ambos concordam que model 1 é melhor)


###################################################################################################################################################################
##################################### Leave One Out - Redes Neurais
###################################################################################################################################################################


###################################################################################################################################################################
###################################################################################################################################################################
##################################### Alone Again vs Life Sucks
###################################################################################################################################################################
###################################################################################################################################################################


###################################################################################################################################################################
##################################### Importação dos dados
###################################################################################################################################################################

# dbalone <- fread('C:/Users/Camila.cmsb/Downloads/alone_again.csv', header = T)
# dbsucks <- fread('C:/Users/Camila.cmsb/Downloads/life_sucks.csv', header = T)

dbalone <- fread('C:/Users/55619/Documents/Estatística/TCC/Base de dados/alone_again.csv', header = T)
dbsucks <- fread('C:/Users/55619/Documents/Estatística/TCC/Base de dados/life_sucks.csv', header = T)

dbalone <- mutate(dbalone, 'Playlist'='Alone Again')
dbsucks <- mutate(dbsucks, 'Playlist'='Life Sucks')

dbalone1 <- dplyr::select(dbalone,"Track URI", "Track Name", "Artist Name(s)", "Track Duration (ms)","Explicit", "Popularity", "Artist Genres":"Time Signature", "Playlist")
dbsucks1 <- dplyr::select(dbsucks, "Track URI", "Track Name", "Artist Name(s)", "Track Duration (ms)","Explicit", "Popularity", "Artist Genres":"Time Signature","Playlist")

dbalone1 <- rename(dbalone1 , "Duration"=`Track Duration (ms)`)
dbsucks1 <- rename(dbsucks1 , "Duration"=`Track Duration (ms)`)

###################################################################################################################################################################
##################################### Manipulando os dados
###################################################################################################################################################################

g1alone <- data.frame('Speechiness'=dbalone1$Speechiness, 'Liveness'=dbalone1$Liveness, 
                      'Instrumentalness'=dbalone1$Instrumentalness, 'Acousticness'=dbalone1$Acousticness, 
                      'Playlist'=dbalone1$Playlist, 'ID'=dbalone1$`Track URI`)
g2alone <- data.frame('Valence'=dbalone1$Valence, 
                      'Energy'=dbalone1$Energy, 'Danceability'=dbalone1$Danceability, 
                      'Playlist'=dbalone1$Playlist, 'ID'=dbalone1$`Track URI`)
g3alone<- data.frame('BPM'=dbalone1$Tempo, 'Playlist'=dbalone1$Playlist, 'ID'=dbalone1$`Track URI`)
g4alone<- data.frame('Duration'=dbalone1$Duration, 'Playlist'=dbalone1$Playlist, 'ID'=dbalone1$`Track URI`)
g5alone<- data.frame('Loudness'=dbalone1$Loudness, 'Playlist'=dbalone1$Playlist, 'ID'=dbalone1$`Track URI`)

g1sucks <- data.frame('Speechiness'=dbsucks1$Speechiness, 'Liveness'=dbsucks1$Liveness, 
                      'Instrumentalness'=dbsucks1$Instrumentalness, 'Acousticness'=dbsucks1$Acousticness, 
                      'Playlist'=dbsucks1$Playlist, 'ID'=dbsucks1$`Track URI`)
g2sucks <- data.frame('Valence'=dbsucks1$Valence, 
                      'Energy'=dbsucks1$Energy, 'Danceability'=dbsucks1$Danceability, 
                      'Playlist'=dbsucks1$Playlist, 'ID'=dbsucks1$`Track URI`)
g3sucks<- data.frame('BPM'=dbsucks1$Tempo, 'Playlist'=dbsucks1$Playlist, 'ID'=dbsucks1$`Track URI`)
g4sucks<- data.frame('Duration'=dbsucks1$Duration, 'Playlist'=dbsucks1$Playlist, 'ID'=dbsucks1$`Track URI`)
g5sucks<- data.frame('Loudness'=dbsucks1$Loudness, 'Playlist'=dbsucks1$Playlist, 'ID'=dbsucks1$`Track URI`)

g1<-data.frame(rbind(g1alone, g1sucks))
g2<-data.frame(rbind(g2alone, g2sucks))
g3<-data.frame(rbind(g3alone, g3sucks))
g4<-data.frame(rbind(g4alone, g4sucks))
g5<-data.frame(rbind(g5alone, g5sucks))

g1reshaped <-reshape2::melt(g1)
g2reshaped <-reshape2::melt(g2)
g3reshaped <-reshape2::melt(g3)
g4reshaped <-reshape2::melt(g4)
g5reshaped <-reshape2::melt(g5)


###################################################################################################################################################################
##################################### Boxplots
###################################################################################################################################################################


ggplot(g1reshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + coord_flip() + theme_classic()

ggplot(g2reshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + coord_flip() + theme_classic()

ggplot(g3reshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + theme_classic()

ggplot(g4reshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + theme_classic()

ggplot(g5reshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + theme_classic()

## Através dos boxplots vemos um caso bem diferente do anterior. 
## As playlists em questão são muito mais parecidas do que o par anterior
## nossos modelo terão muito mais trabalho para diferenciá-las dessa vez.


###################################################################################################################################################################
##################################### União e partição do banco
###################################################################################################################################################################

db1 <-data.frame(rbind(dbalone1, dbsucks1))
dbfac <-db1

dbfac$Playlist <-as.factor(dbfac$Playlist)
dbfac$Playlist <-relevel(dbfac$Playlist, ref="Alone Again")

set.seed(12)

amostra <- sample(c(TRUE, FALSE), nrow(dbfac), replace=TRUE, prob=c(0.7,0.3))
train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]
table(amostra)


###################################################################################################################################################################
##################################### Regressão Logística - Modelos
###################################################################################################################################################################

## Alone Again VS Life Sucks

##################################### Modelo 1
###################################################################################################################################################################
## Todas as variáveis
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
modelrg1<- glm(Playlist ~ Acousticness + Danceability + Duration + Energy 
               + Instrumentalness + Key + Liveness + Loudness + Mode 
               + Speechiness + Tempo + Valence
               ,data=train, family='binomial')
summary(modelrg1)
xtable(summary(modelrg1), auto = F)

rg1.treino <- ifelse(modelrg1$fitted.values > 0.5, "Life Sucks", "Alone Again")
table(rg1.treino, train$Playlist)
xtable(table(rg1.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 1 regressão logística", label = "par 2 treino mod 1 reg log")

#AIC = 336,01

## Apenas Energy possui um p-valor menor que 0,05
## Loudness possui um p-valor inferior a 0,1.
## Erro de 33,88%

##################################### Modelo 2
###################################################################################################################################################################
## Apenas as variáveis de p-valor inferior a 0,1
## Energy, Loudness
modelrg2<- glm(Playlist ~ Energy 
               + Loudness
               ,data=train, family='binomial')
summary(modelrg2)
xtable(summary(modelrg2), auto = F)

rg2.treino <- ifelse(modelrg2$fitted.values > 0.5, "Life Sucks", "Alone Again")
table(rg2.treino, train$Playlist)
xtable(table(rg2.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 2 regressão logística", label = "par 2 treino mod 2 reg log")

# AIC = 327,55

## Erro de 37,56%

##################################### Modelo 3
###################################################################################################################################################################
## Modelo após stepwise do modelo 1
## Acousticness, Energy, Instrumentalness, Loudness
modelrg3<-stepAIC(modelrg1)

summary(modelrg3)
xtable(summary(modelrg3), auto = F)

rg3.treino <- ifelse(modelrg3$fitted.values > 0.5, "Life Sucks", "Alone Again")
table(rg3.treino, train$Playlist)
xtable(table(rg3.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 3 regressão logística", label = "par 2 treino mod 3 reg log")
# AIC = 326,3

## Erro de 34,69%

###################################################################################################################################################################
##################################### Regressão Logística - Calculando Previsões
###################################################################################################################################################################

# Predição do banco teste utilizando cada modelo

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
rg1.prob <- predict(modelrg1, test, type = "response")
rg1.pred <- ifelse(rg1.prob > 0.5, "Life Sucks", "Alone Again")

##################################### Modelo 2
###################################################################################################################################################################
## Energy, Loudness
rg2.prob <- predict(modelrg2, test, type = "response")
rg2.pred <- ifelse(rg2.prob > 0.5, "Life Sucks", "Alone Again")

##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Energy, Instrumentalness, Loudness
rg3.prob <- predict(modelrg3, test, type = "response")
rg3.pred <- ifelse(rg3.prob > 0.5, "Life Sucks", "Alone Again")


###################################################################################################################################################################
##################################### Regressão Logística - Comparando Previsões
###################################################################################################################################################################

## Criando uma variável auxiliar para conseguir demonstrar os resultados de tabelas de confusão e curva ROC próximos
#################################################################################################################################
test<-mutate(test, Playlist1=Playlist)
train<-mutate(train, Playlist1=Playlist)

test$Playlist1 <-ifelse(test$Playlist=="Life Sucks", 1, 0)
train$Playlist1 <-ifelse(train$Playlist=="Life Sucks", 1, 0)
#################################################################################################################################

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

# Previsão do banco teste
table(rg1.pred, test$Playlist)
xtable(table(rg1.pred, test$Playlist), caption = "Matriz de confusão do teste do modelo 1 regressão logística", label = "par 2 teste mod 1 reg log")

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg1.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg1.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg1.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg1.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg1.prob)
# Curva ROC
plotROC(test$Playlist1, rg1.prob)

# Acerto da previsão
mean((rg1.pred ==test$Playlist))
# Erro da previsão
1 - mean((rg1.pred ==test$Playlist))



## O modelo tem muita dificuldade de separar as playlists. 
## Erro de 51,43%

##################################### Modelo 2
###################################################################################################################################################################
## Energy, Loudness

# Previsão do banco teste
table(rg2.pred, test$Playlist)
xtable(table(rg2.pred, test$Playlist), caption = "Matriz de confusão do teste do modelo 2 regressão logística", label = "par 2 teste mod 2 reg log")

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg2.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg2.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg2.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg2.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg2.prob)
# Curva ROC
plotROC(test$Playlist1, rg2.prob)

# Acerto da previsão
mean((rg2.pred ==test$Playlist))
# Erro da previsão
1 - mean((rg2.pred ==test$Playlist))

## Erro de 53,33 %

##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Energy, Instrumentalness, Loudness

# Previsão do banco teste
table(rg3.pred, test$Playlist)
xtable(table(rg3.pred, test$Playlist), caption = "Matriz de confusão do teste do modelo 3 regressão logística", label = "par 2 teste mod 3 reg log")

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg3.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg3.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg3.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg3.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg3.prob)
# Curva ROC
plotROC(test$Playlist1, rg3.prob)

# Acerto da previsão
mean((rg3.pred ==test$Playlist))
# Erro da previsão
1 - mean((rg3.pred ==test$Playlist))

## Muita dúvida sobre porque a ROC do terceiro modelo foi menor que o primeiro. 
## A matriz de confusão do terceiro modelo é bem melhor que o primeiro. 

## Nenhum dos modelo performou bem com as variáveis utilizadas 
## com o par de playlists escolhido

## Erro de 43,81%

###################################################################################################################################################################
##################################### Resetando os bancos de treino e teste
###################################################################################################################################################################

train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]


###################################################################################################################################################################
##################################### Random Forest - Modelos
###################################################################################################################################################################

## Alone Again VS Life Sucks

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
set.seed(12)
modelrf1 <- randomForest(Playlist ~ Acousticness + Danceability + Duration + Energy 
                         + Instrumentalness + Key + Liveness + Loudness + Mode 
                         + Speechiness + Tempo + Valence
                         ,data = train, importance = TRUE)
modelrf1

xtable(modelrf1$confusion, caption = "Matriz de confusão random forest do treino do modelo 1.", label = "par 2 treino mod 1 ran for")
xtable(t(modelrf1$confusion), caption = "Matriz de confusão random forest do treino do modelo 1.", label = "par 2 mod 1 treino ran for")


par(mar = c(4,4,2,10), xpd=T)
plot(modelrf1, main = NA)
legend(x="right",inset = c(-0.58,0),
       legend= c("Média", "Alone Again", "Life Sucks"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

## Verificando as variáveis importantes para o modelo de Random Forest
importance(modelrf1)
varImpPlot(modelrf1, main = NA)

## Erro de 49,39%


##################################### Modelo 2
###################################################################################################################################################################
## Energy, Loudness
set.seed(12)
modelrf2 <- randomForest(Playlist ~ Energy
                         + Loudness
                         , data = train, importance = TRUE)
modelrf2

xtable(modelrf2$confusion, caption = "Matriz de confusão random forest do treino do modelo 2.", label = "par 2 treino mod 2 ran for")
xtable(t(modelrf2$confusion), caption = "Matriz de confusão random forest do treino do modelo 2.", label = "par 2 mod 2 treino ran for")


par(mar = c(4,4,2,10), xpd=T)
plot(modelrf2, main = NA)
legend(x="right",inset = c(-0.58,0),
       legend= c("Média", "Alone Again", "Life Sucks"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

importance(modelrf2)
varImpPlot(modelrf2, main = NA)

## Erro de 53,06%


##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Energy, Instrumentalness, Loudness
set.seed(12)
modelrf3 <- randomForest(Playlist ~ Acousticness + Energy
                         + Instrumentalness + Loudness
                         , data = train, importance = TRUE)
modelrf3

xtable(modelrf3$confusion, caption = "Matriz de confusão random forest do treino do modelo 3.", label = "par 3 treino mod 3 ran for")
xtable(t(modelrf3$confusion), caption = "Matriz de confusão random forest do treino do modelo 3.", label = "par 3 mod 3 treino ran for")

par(mar = c(4,4,2,10), xpd=T)
plot(modelrf3, main = NA)
legend(x="right",inset = c(-0.58,0),
       legend= c("Média", "Alone Again", "Life Sucks"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

importance(modelrf3)
varImpPlot(modelrf3, main = NA)

## Erro de 48,57%


###################################################################################################################################################################
##################################### Random Forest - Previsões
###################################################################################################################################################################


##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

# Resultado do modelo com o banco de treino
modelrf1$confusion
t(modelrf1$confusion)
# Previsão perfeita

# Previsão do modelo no banco de teste
predValid1 <- predict(modelrf1, test, type = "class")
table(predValid1, test$Playlist)
xtable(table(predValid1, test$Playlist), caption = "Matriz de confusão random forest da previsão do modelo 1.", label = "par 2 mod 1 prev ran for")

# Acerto da previsão
mean((predValid1 ==test$Playlist))
# Erro da previsão
1 - mean((predValid1 ==test$Playlist))

## Erro de 58,1%


##################################### Modelo 2
###################################################################################################################################################################
## Energy, Loudness

# Resultado do modelo com o banco de treino
modelrf2$confusion
# Previsão perfeita

# Previsão do modelo no banco de teste
predValid2 <- predict(modelrf2, test, type = "class")
table(predValid2, test$Playlist)
xtable(table(predValid2, test$Playlist), caption = "Matriz de confusão random forest da previsão do modelo 2.", label = "par 2 mod 2 prev ran for")

# Acerto da previsão
mean((predValid2 ==test$Playlist))
# Erro da previsão
1 - mean((predValid2 ==test$Playlist))

## Erro de 65,71%


##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Energy, Instrumentalness, Loudness

# Resultado do modelo com o banco de treino
modelrf3$confusion
# Previsão perfeita

# Previsão do modelo no banco de teste
predValid3 <- predict(modelrf3, test, type = "class")
table(predValid3, test$Playlist)
xtable(table(predValid3, test$Playlist), caption = "Matriz de confusão random forest da previsão do modelo 3.", label = "par 2 mod 3 prev ran for")

# Acerto da previsão
mean((predValid3 ==test$Playlist))
# Erro da previsão
1 - mean((predValid3 ==test$Playlist))

## Modelo Random Forest me pareceu um pouco, mas muito pouco melhor

## Erro de 57,14%


## https://www.r-bloggers.com/2021/04/random-forest-in-r/
## https://www.geeksforgeeks.org/random-forest-approach-in-r-programming/
## https://towardsdatascience.com/random-forest-in-r-f66adf80ec9


###################################################################################################################################################################
##################################### Redes Neurais
###################################################################################################################################################################

## Alone Again VS Life Sucks

# sad boys 
# :(

###################################################################################################################################################################
##################################### Validação Cruzada
###################################################################################################################################################################

## Alone Again VS Life Sucks

###################################################################################################################################################################
##################################### K-Folds
###################################################################################################################################################################

## Alone Again VS Life Sucks

###################################################################################################################################################################
##################################### Resetando bases e manipulando dados
###################################################################################################################################################################

## o método de validação cruzada, por algum motivo exige que os fatores tenham apenas 1 palavra

## usar a validação cruzada no banco completo ou só na amostra de treino 
## e fazer uma nova previsão do banco de teste?

dbfac1<-dbfac
dbfac1$Playlist <-ifelse(dbfac$Playlist=="Alone Again", "Alone", "Sucks")
dbfac1$Playlist <-as.factor(dbfac1$Playlist)


###################################################################################################################################################################
##################################### K-Folds - Regressão Logística
###################################################################################################################################################################

## Alone Again VS Life Sucks

## https://www.statology.org/k-fold-cross-validation-in-r/
## https://rforhr.com/kfold.html

## Criando a função kf que define o tipo de validação cruzada utilizada. Neste momento, estamos usando o método k-folds
## Vamos testar a diferença entre repetições no k-folds e sem repetições.
kf <- trainControl(method="cv", number=10, savePredictions="all", classProbs=TRUE)
#kf10 <- trainControl(method="repeatedcv", number=10, savePredictions="all", classProbs=TRUE, repeats = 10)

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

kmodelrg1<- train(Playlist ~ Acousticness + Danceability + Duration 
                  + Energy + Instrumentalness + Key + Liveness + Loudness + Mode 
                  + Speechiness + Tempo + Valence
                  ,data=dbfac1, method="glm", family='binomial', trControl=kf)

summary(kmodelrg1)
xtable(summary(kmodelrg1))
## AIC = 481,15

kmodelrg1$resampledCM
kfrg1<-kmodelrg1$resampledCM[,-5:-6]

rownames(kfrg1)<-c("Fold 1","Fold 2","Fold 3","Fold 4","Fold 5","Fold 6","Fold 7","Fold 8","Fold 9","Fold 10")
colnames(kfrg1)<-c("Acerto","Erro","Erro","Acerto")
kfrg1
xtable(kfrg1, caption = "Resultados de cada fold do modelo 1 para regressão logística", label = "par 2 kfolds mod 1 reg log")


table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])
xtable(table(kmodelrg1$pred[,1], kmodelrg1$pred[,2]), caption = "Resultados dos 10-folds do modelo 1", label = "par 2 kfolds mod 1 reg log")


table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[1]*105/350 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[2]*105/350 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[3]*105/350 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[4]*105/350 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


kmodelrg1$results[,-1]
xtable(kmodelrg1$results[,-1])
1-kmodelrg1$results[2]
## Accuracy de 56,57% de acerto na previsão média de todas as instâncias
## Kappa de 7,8%. Entre 1% e 20% - muito leve

## Erro de 43,43%


## Interpretação de Kappa
##       100% Kappa perfeita distinção
##  99% - 81% Kappa quase perfeita
##  80% - 61% Kappa substancial
##  60% - 41% Kappa moderada
##  40% - 21% Kappa leve
##  20% -  1% Kappa muito leve
##         0% Kappa sem distinção

##################################### Modelo 2
###################################################################################################################################################################
## Energy, Loudness
kmodelrg2<- train(Playlist ~ Energy
                  + Loudness
                  ,data=dbfac1, method="glm", family='binomial', trControl=kf)

summary(kmodelrg2)
xtable(summary(kmodelrg2))
## AIC = 476,06

kmodelrg2$resampledCM
kfrg2<-kmodelrg2$resampledCM[,-5:-6]

rownames(kfrg2)<-c("Fold 1","Fold 2","Fold 3","Fold 4","Fold 5","Fold 6","Fold 7","Fold 8","Fold 9","Fold 10")
colnames(kfrg2)<-c("Acerto","Erro","Erro","Acerto")
kfrg2
xtable(kfrg2, caption = "Resultados de cada fold do modelo 2 para regressão logística", label = "par 2 kfolds mod 2 reg log")

table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])
xtable(table(kmodelrg2$pred[,1], kmodelrg2$pred[,2]), caption = "Resultados dos 10-folds do modelo 2", label = "par 2 kfolds mod 2 reg log")


table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])[1]*105/350 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])[2]*105/350 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])[3]*105/350 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])[4]*105/350 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


kmodelrg2$results[,-1]
xtable(kmodelrg2$results[,-1])
1-kmodelrg2$results[2]
## Accuracy de 56,57% de acerto na previsão média de todas as instâncias
## Kappa de 4,45%. Entre 1% e 20% - muito leve

## Erro de 43,43%

##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Energy, Instrumentalness, Loudness

kmodelrg3<- train(Playlist ~ Acousticness + Energy 
                  + Instrumentalness + Loudness
                  ,data=dbfac1, method="glm", family='binomial', trControl=kf)

summary(kmodelrg3)
xtable(summary(kmodelrg3))
## AIC = 474,62

kmodelrg3$resampledCM
kfrg3<-kmodelrg3$resampledCM[,-5:-6]

rownames(kfrg3)<-c("Fold 1","Fold 2","Fold 3","Fold 4","Fold 5","Fold 6","Fold 7","Fold 8","Fold 9","Fold 10")
colnames(kfrg3)<-c("Acerto","Erro","Erro","Acerto")
kfrg3
xtable(kfrg3, caption = "Resultado de cada fold do modelo 3 para regressão logística", label = "par 2 kfolds mod 3 reg log")

table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])
xtable(table(kmodelrg3$pred[,1], kmodelrg3$pred[,2]), caption = "Resultado dos 10-folds do modelo 3", label = "par 2 kfolds mod 3 reg log")


table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


kmodelrg3$results[-1]
xtable(kmodelrg3$results[,-1])
1-kmodelrg3$results[2]
## Accuracy de 100% de acerto na previsão média de todas as instâncias
## Kappa de 100%. 100% - perfeita
## Erro de 39,43%

###################################################################################################################################################################
##################################### K-Folds - Random Forest
###################################################################################################################################################################

## Alone Again VS Life Sucks

## https://rdrr.io/github/marccamb/optiranger/man/rf.kfold.html

## https://rpubs.com/jvaldeleon/forest_repeat_cv

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

kmodelrf1<- train(Playlist ~ Acousticness + Danceability + Duration
                  + Energy + Instrumentalness + Key + Liveness + Loudness + Mode
                  + Speechiness + Tempo + Valence
                  ,data=dbfac1, method="rf", family='binomial', trControl=kf)

kmodelrf1$finalModel$confusion
xtable(kmodelrf1$finalModel$confusion)

varImp(kmodelrf1)
plot(varImp(kmodelrf1), main = NA)

plot((kmodelrf1$finalModel), main = NA)
legend(x="top",
       legend= c("Média", "Alone Again", "Life Sucks"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

kmodelrf1$resampledCM

kfrf1<-kmodelrf1$resampledCM[order(kmodelrf1$resampledCM$mtry),]

kfrf1<-kfrf1[1:10,-6]


rownames(kfrf1)<-c()
colnames(kfrf1)<-c()
rownames(kfrf1)<-c("Fold 1","Fold 2","Fold 3","Fold 4","Fold 5","Fold 6","Fold 7","Fold 8","Fold 9","Fold 10")
colnames(kfrf1)<-c("Acerto","Erro","Erro","Acerto", "mtry")
kfrf1
xtable(kfrf1, caption = "Resultado de cada fold do modelo 1 para random forest", label = "par 2 kfolds mod 1 ran for")

table(kmodelrf1$pred[,1], kmodelrf1$pred[,2])
xtable(table(kmodelrf1$pred[,1], kmodelrf1$pred[,2]), caption = "Resultado dos 10-folds do modelo 1", label = "par 2 kfolds mod 1 ran for")

kmodelrf1

ab<-list()
ab<-kmodelrf1$pred
ab<-ab[order(ab$mtry),]

kmodelrf1mtry12<-list()
kmodelrf1mtry12<-ab[701:1050, ]


## Melhor previsão de regressão linear foi feita pelo primeiro modelo

table(kmodelrf1mtry12[,1], kmodelrf1mtry12[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf1mtry12[,1], kmodelrf1mtry12[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf1mtry12[,1], kmodelrf1mtry12[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf1mtry12[,1], kmodelrf1mtry12[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


kmodelrf1$results
xtable(kmodelrf1$results)
1-kmodelrf1$results[2]
## Accuracy de 49,42% de acerto na previsão média de todas as instâncias
## Kappa de 5,47%. Entre 1% e 20% - muito leve.

## Erro de 52,86%

## Interpretação de Kappa
##       100% Kappa perfeita distinção
##  99% - 81% Kappa quase perfeita
##  80% - 61% Kappa substancial
##  60% - 41% Kappa moderada
##  40% - 21% Kappa leve
##  20% -  1% Kappa muito leve
##         0% Kappa sem distinção


##################################### Modelo 2
###################################################################################################################################################################
## Energy, Loudness
set.seed(12)
kmodelrf2<- train(Playlist ~ Danceability + Duration 
                  + Instrumentalness + Key + Liveness + Mode 
                  + Speechiness + Tempo + Valence
                  ,data=dbfac1, method="rf", family='binomial', trControl=kf)

kmodelrf2
kmodelrf2$finalModel$confusion

xtable(t(kmodelrf2$finalModel$confusion))

varImp(kmodelrf2)
plot(varImp(kmodelrf2), main = NA)

plot((kmodelrf2$finalModel), main = NA)
legend(x="top",
       legend= c("Média", "Alone Again", "Life Sucks"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

kmodelrf2$resampledCM

kfrf2<-kmodelrf2$resampledCM[order(kmodelrf2$resampledCM$mtry),]

kfrf2<-kfrf2[21:30,-6]


rownames(kfrf2)<-c()
colnames(kfrf2)<-c()
rownames(kfrf2)<-c("Fold 1","Fold 2","Fold 3","Fold 4","Fold 5","Fold 6","Fold 7","Fold 8","Fold 9","Fold 10")
colnames(kfrf2)<-c("Acerto","Erro","Erro","Acerto", "mtry")
kfrf2
xtable(kfrf2, caption = "Resultado de cada fold do modelo 2 para random forest", label = "par 2 kfolds mod 2 ran for")

table(kmodelrf2$pred[,1], kmodelrf2$pred[,2])
xtable(table(kmodelrf2$pred[,1], kmodelrf2$pred[,2]), caption = "Resultado dos 10-folds do modelo 2", label = "par 2 kfolds mod 2 ran for")

kmodelrf2

ab<-list()
ab<-kmodelrf2$pred
ab<-ab[order(ab$mtry),]

kmodelrf2mtry2<-list()
kmodelrf2mtry2<-ab[1:350, ]


## Melhor previsão de regressão linear foi feita pelo primeiro modelo

table(kmodelrf2mtry2[,1], kmodelrf2mtry2[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf2mtry2[,1], kmodelrf2mtry2[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf2mtry2[,1], kmodelrf2mtry2[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf2mtry2[,1], kmodelrf2mtry2[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


kmodelrf2$results
xtable(kmodelrf2$results)
1-kmodelrf2$results[2]
## Accuracy de 49,71% de acerto na previsão média de todas as instâncias
## Kappa de 4,66%. Entre 1% e 20% - muito leve.

## Erro de 50,28%


##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Energy, Instrumentalness, Loudness

kmodelrf3<- train(Playlist ~ Danceability + Duration 
                  + Key + Liveness + Mode 
                  + Speechiness + Tempo + Valence
                  ,data=dbfac1, method="rf", family='binomial', trControl=kf)

kmodelrf3$finalModel$confusion

xtable(kmodelrf3$finalModel$confusion)

varImp(kmodelrf3)
plot(varImp(kmodelrf3), main = NA)

plot((kmodelrf3$finalModel), main = NA)
legend(x="top",
       legend= c("Média", "Alone Again", "Life Sucks"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

kmodelrf3$resampledCM

kfrf3<-kmodelrf3$resampledCM[order(kmodelrf3$resampledCM$mtry),]

kfrf3<-kfrf3[1:10,-6]


rownames(kfrf3)<-c()
colnames(kfrf3)<-c()
rownames(kfrf3)<-c("Fold 1","Fold 2","Fold 3","Fold 4","Fold 5","Fold 6","Fold 7","Fold 8","Fold 9","Fold 10")
colnames(kfrf3)<-c("Acerto","Erro","Erro","Acerto", "mtry")
kfrf3
xtable(kfrf3, caption = "Resultado de cada fold do modelo 3  para random forest", label = "par 2 kfolds mod 3 ran for")

table(kmodelrf3$pred[,1], kmodelrf3$pred[,2])
xtable(table(kmodelrf3$pred[,1], kmodelrf3$pred[,2]), caption = "Resultado dos 10-folds do modelo 3", label = "par 2 kfolds mod 3 ran for")


kmodelrf3

ab<-list()
ab<-kmodelrf3$pred
ab<-ab[order(ab$mtry),]

kmodelrf3mtry8<-list()
kmodelrf3mtry8<-ab[701:1050, ]


## Melhor previsão de regressão linear foi feita pelo primeiro modelo

table(kmodelrf3mtry8[,1], kmodelrf3mtry8[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf3mtry8[,1], kmodelrf3mtry8[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf3mtry8[,1], kmodelrf3mtry8[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf3mtry8[,1], kmodelrf3mtry8[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


kmodelrf3$results
xtable(kmodelrf3$results)
1-kmodelrf3$results[2]
## Accuracy de 46,86% de acerto na previsão média de todas as instâncias
## Kappa de 10,89%. Entre 1% e 20% - muito leve.

## Erro de 53,43%


###################################################################################################################################################################
##################################### K-Folds - Redes Neurais
###################################################################################################################################################################

## Alone Again VS Life Sucks

###################################################################################################################################################################
##################################### Leave One Out
###################################################################################################################################################################

## Alone Again VS Life Sucks

## LOO manual (possivelmente terei de usar isso nas redes neurais)
#First initialize the output vector as an empty object outside the loop.
fitted_value <- NULL
for(i in 1:53){
  #you did this part right
  validation<-ironslag[i,]
  training<-ironslag[-i,]
  model1<-lm(magnetic ~ chemical, data = training)
  #when you fit the model, use the newdata argument to predict on a new row
  #also, fitted_value needs the index [i], so the each loop doesn't overwrite the previous
  fitted_value[i] <- predict(model1, newdata = validation)}

## https://www.geeksforgeeks.org/loocvleave-one-out-cross-validation-in-r-programming/#:~:text=LOOCV(Leave%20One%20Out%20Cross%2DValidation)%20is%20a%20type,considered%20as%20the%20training%20set.
## https://www.statology.org/leave-one-out-cross-validation-in-r/

loo <- trainControl(method="LOOCV", savePredictions= "all", classProbs = TRUE)
#loo <- trainControl(method="LOOCV")

###################################################################################################################################################################
##################################### Leave One Out - Regressão Logística
###################################################################################################################################################################

## Alone Again VS Life Sucks

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

loomodelrg1<- train(Playlist ~ Acousticness + Danceability + Duration 
                    + Energy + Instrumentalness + Key + Liveness + Loudness + Mode 
                    + Speechiness + Tempo + Valence
                    ,data=dbfac1, method="glm", family='binomial', trControl=loo)

summary(loomodelrg1)
xtable(summary(loomodelrg1))
## AIC = 481,15

table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])
xtable(table(loomodelrg1$pred[,1], loomodelrg1$pred[,2]))

table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10

## Comparar com k-folds e reg log normal


loomodelrg1$results
xtable(loomodelrg1$results[,-1])
1-loomodelrg1$results[2]
## Accuracy de 55,71% de acerto na previsão média de todas as instâncias
## Kappa de 6,54%. Entre 1% e 20% - muito leve

## Erro de 44,29%

## Interpretação de Kappa
##       100% Kappa perfeita distinção
##  99% - 81% Kappa quase perfeita
##  80% - 61% Kappa substancial
##  60% - 41% Kappa moderada
##  40% - 21% Kappa leve
##  20% -  1% Kappa muito leve
##         0% Kappa sem distinção


##################################### Modelo 2
###################################################################################################################################################################
## Energy, Loudness

loomodelrg2<- train(Playlist ~ Energy 
                    + Loudness
                    ,data=dbfac1, method="glm", family='binomial', trControl=loo)

summary(loomodelrg2)
xtable(summary(loomodelrg2))

## AIC = 476,06

table(loomodelrg2$pred[,1], loomodelrg2$pred[,2])
xtable(table(loomodelrg2$pred[,1], loomodelrg2$pred[,2]))

table(loomodelrg2$pred[,1], loomodelrg2$pred[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg2$pred[,1], loomodelrg2$pred[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg2$pred[,1], loomodelrg2$pred[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg2$pred[,1], loomodelrg2$pred[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10

## Comparar com k-folds e reg log normal


loomodelrg2$results
xtable(loomodelrg2$results[,-1])
1-loomodelrg2$results[2]
## Accuracy de 56,57% de acerto na previsão média de todas as instâncias
## Kappa de 4,49%. Entre 1% e 20% - muito leve

## Erro de 43,43%


##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Energy, Instrumentalness, Loudness

loomodelrg3<- train(Playlist ~ Acousticness + Energy 
                    + Instrumentalness + Loudness
                    ,data=dbfac1, method="glm", family='binomial', trControl=loo)

summary(loomodelrg3)
xtable(summary(loomodelrg3))

## AIC = 474,62

table(loomodelrg3$pred[,1], loomodelrg3$pred[,2])
xtable(table(loomodelrg3$pred[,1], loomodelrg3$pred[,2]))

table(loomodelrg3$pred[,1], loomodelrg3$pred[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg3$pred[,1], loomodelrg3$pred[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg3$pred[,1], loomodelrg3$pred[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg3$pred[,1], loomodelrg3$pred[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10

## Comparar com k-folds e reg log normal


loomodelrg3$results[,-1]
xtable(loomodelrg3$results[,-1])
1-loomodelrg3$results[2]
## Accuracy de 58,86% de acerto na previsão média de todas as instâncias
## Kappa de 10,48%. Entre 1% e 20% - muito leve

## Erro de 41,14%


###################################################################################################################################################################
##################################### Leave One Out - Random Forest
###################################################################################################################################################################

## Alone Again VS Life Sucks

## https://rdrr.io/github/marccamb/optiranger/man/rf.kfold.html

## https://rpubs.com/jvaldeleon/forest_repeat_cv

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

loomodelrf1<- train(Playlist ~ Acousticness + Danceability + Duration
                    + Energy + Instrumentalness + Key + Liveness + Loudness + Mode
                    + Speechiness + Tempo + Valence
                    ,data=dbfac1, method="rf", family='binomial', trControl=loo)

## demorou 3 minutos pra rodar

loomodelrf1$finalModel

loomodelrf1$finalModel$confusion
xtable(loomodelrf1$finalModel$confusion)

loomodelrf1$finalModel$importance
loomodelrf1

varImp(loomodelrf1)
plot(varImp(loomodelrf1), main = NA)

plot((loomodelrf1$finalModel), main = NA)
legend(x="top",
       legend= c("Média", "Alone Again", "Life Sucks"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

loomodelrf1

ab<-list()
ab<-loomodelrf1$pred
ab<-ab[order(ab$mtry),]
loomodelrf1mtry2<-list()
loomodelrf1mtry2<-ab[1:350,]


table(loomodelrf1mtry2[,1], loomodelrf1mtry2[,2])
table(loomodelrf1mtry2[,1], loomodelrf1mtry2[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf1mtry2[,1], loomodelrf1mtry2[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf1mtry2[,1], loomodelrf1mtry2[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf1mtry2[,1], loomodelrf1mtry2[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


loomodelrf1$results
xtable(loomodelrf1$results)
1-loomodelrf1$results[2]
## Accuracy de 46,86% de acerto na previsão média de todas as instâncias
## Kappa de 11,66%. Entre 1% e 20% - muito leve

## Erro de 53,14%

## Interpretação de Kappa
##       100% Kappa perfeita distinção
##  99% - 81% Kappa quase perfeita
##  80% - 61% Kappa substancial
##  60% - 41% Kappa moderada
##  40% - 21% Kappa leve
##  20% -  1% Kappa muito leve
##         0% Kappa sem distinção

##################################### Modelo 2
###################################################################################################################################################################
## Energy, Loudness

loomodelrf2<- train(Playlist ~ Energy
                    + Loudness
                    ,data=dbfac1, method="rf", family='binomial', trControl=loo)

loomodelrf2$finalModel

loomodelrf2$finalModel$confusion
xtable(loomodelrf2$finalModel$confusion)

loomodelrf2$finalModel$importance
loomodelrf2

varImp(loomodelrf2)
plot(varImp(loomodelrf2), main = NA)

plot(loomodelrf2$finalModel, main = NA)
legend(x="top",
       legend= c("Média", "Alone Again", "Life Sucks"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

loomodelrf2

ab<-list()
ab<-loomodelrf2$pred
ab<-ab[order(ab$mtry),]
loomodelrf2mtry2<-list()
loomodelrf2mtry2<-ab[1:350,]


table(loomodelrf2mtry2[,1], loomodelrf2mtry2[,2])
table(loomodelrf2mtry2[,1], loomodelrf2mtry2[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf2mtry2[,1], loomodelrf2mtry2[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf2mtry2[,1], loomodelrf2mtry2[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf2mtry2[,1], loomodelrf2mtry2[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


loomodelrf2$results
xtable(loomodelrf2$results)
1-loomodelrf2$results[2]
## Accuracy de 44,57% de acerto na previsão média de todas as instâncias
## Kappa de 14,89%. Entre 1% e 20% - muito leve

## Erro de 55,43%


##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Energy, Instrumentalness, Loudness

loomodelrf3<- train(Playlist ~ Acousticness + Energy
                    + Instrumentalness + Loudness
                    ,data=dbfac1, method="rf", family='binomial', trControl=loo)

loomodelrf3$finalModel

loomodelrf3$finalModel$confusion
xtable(loomodelrf3$finalModel$confusion)

loomodelrf3$finalModel$importance
loomodelrf3

varImp(loomodelrf3)
plot(varImp(loomodelrf3), main = NA)

plot(loomodelrf3$finalModel, main = NA)
legend(x="top",
       legend= c("Média", "Alone Again", "Life Sucks"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

loomodelrf3

ab<-list()
ab<-loomodelrf3$pred
ab<-ab[order(ab$mtry),]
loomodelrf3mtry3<-list()
loomodelrf3mtry3<-ab[351:700,]


table(loomodelrf3mtry3[,1], loomodelrf3mtry3[,2])
table(loomodelrf3mtry3[,1], loomodelrf3mtry3[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf3mtry3[,1], loomodelrf3mtry3[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf3mtry3[,1], loomodelrf3mtry3[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf3mtry3[,1], loomodelrf3mtry3[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


loomodelrf3$results
xtable(loomodelrf3$results)
1-loomodelrf3$results[2]
## Accuracy de 100% de acerto na previsão média de todas as instâncias
## Kappa de 100%. 100% - Entre 1% e 20% - muito leve

## Erro de 53,71%


###################################################################################################################################################################
##################################### Leave One Out - Redes Neurais
###################################################################################################################################################################

## Alone Again VS Life Sucks

###################################################################################################################################################################
###################################################################################################################################################################
##################################### Spooning vs Power Hour
###################################################################################################################################################################
###################################################################################################################################################################


###################################################################################################################################################################
##################################### Importação dos dados
###################################################################################################################################################################

# dbspoon <- fread('C:/Users/Camila.cmsb/Downloads/spooning.csv', header = T)
# dbpower <- fread('C:/Users/Camila.cmsb/Downloads/power_hour.csv', header = T)

dbspoon <- fread('C:/Users/55619/Documents/Estatística/TCC/Base de dados/spooning.csv', header = T)
dbpower <- fread('C:/Users/55619/Documents/Estatística/TCC/Base de dados/power_hour.csv', header = T)

dbspoon <- mutate(dbspoon, 'Playlist'='Spooning')
dbpower <- mutate(dbpower, 'Playlist'='Power Hour')

dbspoon1 <- dplyr::select(dbspoon,"Track URI", "Track Name", "Artist Name(s)", "Track Duration (ms)","Explicit", "Popularity", "Artist Genres":"Time Signature", "Playlist")
dbpower1 <- dplyr::select(dbpower, "Track URI", "Track Name", "Artist Name(s)", "Track Duration (ms)","Explicit", "Popularity", "Artist Genres":"Time Signature","Playlist")

dbspoon1 <- rename(dbspoon1 , "Duration"=`Track Duration (ms)`)
dbpower1 <- rename(dbpower1 , "Duration"=`Track Duration (ms)`)

###################################################################################################################################################################
##################################### Manipulando os dados
###################################################################################################################################################################

g1spoon <- data.frame('Speechiness'=dbspoon1$Speechiness, 'Liveness'=dbspoon1$Liveness, 
                      'Instrumentalness'=dbspoon1$Instrumentalness, 'Acousticness'=dbspoon1$Acousticness, 
                      'Playlist'=dbspoon1$Playlist, 'ID'=dbspoon1$`Track URI`)
g2spoon <- data.frame('Valence'=dbspoon1$Valence, 'Energy'=dbspoon1$Energy,
                      'Danceability'=dbspoon1$Danceability, 
                      'Playlist'=dbspoon1$Playlist, 'ID'=dbspoon1$`Track URI`)
g3spoon<- data.frame('BPM'=dbspoon1$Tempo, 'Playlist'=dbspoon1$Playlist, 'ID'=dbspoon1$`Track URI`)
g4spoon<- data.frame('Duration'=dbspoon1$Duration, 'Playlist'=dbspoon1$Playlist, 'ID'=dbspoon1$`Track URI`)
g5spoon<- data.frame('Loudness'=dbspoon1$Loudness, 'Playlist'=dbspoon1$Playlist, 'ID'=dbspoon1$`Track URI`)

g1power <- data.frame('Speechiness'=dbpower1$Speechiness, 'Liveness'=dbpower1$Liveness, 
                      'Instrumentalness'=dbpower1$Instrumentalness, 'Acousticness'=dbpower1$Acousticness, 
                      'Playlist'=dbpower1$Playlist, 'ID'=dbpower1$`Track URI`)
g2power <- data.frame('Valence'=dbpower1$Valence, 'Energy'=dbpower1$Energy,
                      'Danceability'=dbpower1$Danceability, 
                      'Playlist'=dbpower1$Playlist, 'ID'=dbpower1$`Track URI`)
g3power<- data.frame('BPM'=dbpower1$Tempo, 'Playlist'=dbpower1$Playlist, 'ID'=dbpower1$`Track URI`)
g4power<- data.frame('Duration'=dbpower1$Duration, 'Playlist'=dbpower1$Playlist, 'ID'=dbpower1$`Track URI`)
g5power<- data.frame('Loudness'=dbpower1$Loudness, 'Playlist'=dbpower1$Playlist, 'ID'=dbpower1$`Track URI`)

g1<-data.frame(rbind(g1spoon, g1power))
g2<-data.frame(rbind(g2spoon, g2power))
g3<-data.frame(rbind(g3spoon, g3power))
g4<-data.frame(rbind(g4spoon, g4power))
g5<-data.frame(rbind(g5spoon, g5power))

g1reshaped <-reshape2::melt(g1)
g2reshaped <-reshape2::melt(g2)
g3reshaped <-reshape2::melt(g3)
g4reshaped <-reshape2::melt(g4)
g5reshaped <-reshape2::melt(g5)


###################################################################################################################################################################
##################################### Boxplots
###################################################################################################################################################################

ggplot(g1reshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + coord_flip() + theme_classic()

ggplot(g2reshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + coord_flip() + theme_classic()

ggplot(g3reshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + theme_classic()

ggplot(g4reshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + theme_classic()

ggplot(g5reshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + theme_classic()

## Através dos boxplots podemos esperar algum possível problema para nossos métodos de classificação.
## As variáveis Acousticness, Instrumentalness, Energy, Loudness e Tempo se misturam pouco entre as playlists 
## Veremos se nosso modelo convergirá.


###################################################################################################################################################################
##################################### União e partição do banco
###################################################################################################################################################################

db1 <-data.frame(rbind(dbspoon1, dbpower1))
dbfac <-db1

dbfac$Playlist <-as.factor(dbfac$Playlist)
dbfac$Playlist <-relevel(dbfac$Playlist, ref="Power Hour")

set.seed(12)

amostra <- sample(c(TRUE, FALSE), nrow(dbfac), replace=TRUE, prob=c(0.7,0.3))
train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]


###################################################################################################################################################################
##################################### Regressão Logística - Modelos
###################################################################################################################################################################

## Spooning VS Power Hour

##################################### Modelo 1
###################################################################################################################################################################
## Todas as variáveis
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
modelrg1<- glm(Playlist ~ Acousticness + Danceability + Duration + Energy
               + Instrumentalness + Key + Liveness + Loudness + Mode
               + Speechiness + Tempo + Valence
               ,data=train, family='binomial')
summary(modelrg1)

xtable(summary(modelrg1), auto = F)

## Modelo não converge.
## AIC = 26

rg1.treino <- ifelse(modelrg1$fitted.values > 0.5, "Spooning", "Power Hour")
table(rg1.treino, train$Playlist)
xtable(table(rg1.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 1 regressão logística", label = "par 3 treino mod 1 reg log")

#modelrg1<- logistf(Playlist ~ Acousticness + Danceability + Duration + Energy
#                   + Instrumentalness + Key + Liveness + Loudness + Mode
#                   + Speechiness + Tempo + Valence
#                   ,data=train, firth=TRUE)
#summary(modelrg1)

#modelrg1<- glm(Playlist ~ Acousticness + Danceability + Duration + Energy
#               + Instrumentalness + Key + Liveness + Loudness + Mode
#               + Speechiness + Tempo + Valence
#               ,data=train, family='binomial', maxit= 9)
#summary(modelrg1)

##################################### Modelo 2
###################################################################################################################################################################
## Removendo Energy
## Acousticness, Danceability, Duration, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
modelrg2<- glm(Playlist ~ Acousticness + Danceability + Duration 
             + Instrumentalness + Key + Liveness + Loudness + Mode
             + Speechiness + Tempo + Valence
             ,data=train, family='binomial')
summary(modelrg2)
xtable(summary(modelrg2), auto = F)

rg2.treino <- ifelse(modelrg2$fitted.values > 0.5, "Spooning", "Power Hour")
table(rg2.treino, train$Playlist)
xtable(table(rg2.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 2 regressão logística", label = "par 3 treino mod 2 reg log")

## Modelo converge e p-valores fazem mais sentido
## AIC = 48,385


##################################### Modelo 3
###################################################################################################################################################################
## Variáveis significativas
## Acousticness, Duration, Instrumentalness, Liveness, Loudness
modelrg3<- glm(Playlist ~ Acousticness + Duration
               + Instrumentalness + Liveness + Loudness
               ,data=train, family='binomial')
summary(modelrg3)
xtable(summary(modelrg3), auto = F)

rg3.treino <- ifelse(modelrg3$fitted.values > 0.5, "Spooning", "Power Hour")
table(rg3.treino, train$Playlist)
xtable(table(rg3.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 3 regressão logística", label = "par 3 treino mod 3 reg log")

## Modelo converge
## AIC = 41,786


###################################################################################################################################################################
##################################### Regressão Logística - Calculando Previsões
###################################################################################################################################################################

# Predição do banco teste utilizando cada modelo

##################################### Modelo 1
###################################################################################################################################################################
rg1.prob <- predict(modelrg1, test, type = "response")
rg1.pred <- ifelse(rg1.prob > 0.5, "Spooning", "Power Hour")

##################################### Modelo 2
###################################################################################################################################################################
rg2.prob <- predict(modelrg2, test, type = "response")
rg2.pred <- ifelse(rg2.prob > 0.5, "Spooning", "Power Hour")

##################################### Modelo 3
###################################################################################################################################################################
rg3.prob <- predict(modelrg3, test, type = "response")
rg3.pred <- ifelse(rg3.prob > 0.5, "Spooning", "Power Hour")


###################################################################################################################################################################
##################################### Regressão Logística - Comparando Previsões
###################################################################################################################################################################

## Criando uma variável auxiliar para conseguir demonstrar os resultados de tabelas de confusão e curva ROC próximos
#################################################################################################################################
test<-mutate(test, Playlist1=Playlist)
train<-mutate(train, Playlist1=Playlist)

test$Playlist1 <-ifelse(test$Playlist=="Spooning", 1, 0)
train$Playlist1 <-ifelse(train$Playlist=="Spooning", 1, 0)
#################################################################################################################################

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

# Previsão do banco teste
table(rg1.pred, test$Playlist)
xtable(table(rg1.pred, test$Playlist), caption = "Matriz de confusão do teste do modelo 1 regressão logística", label = "par 3 teste mod 1 reg log")

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg1.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg1.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg1.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg1.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg1.prob)
# Curva ROC do modelo completo
plotROC(test$Playlist1, rg1.prob)

# Acerto da previsão
mean((rg1.pred ==test$Playlist))
# Erro da previsão
1 - mean((rg1.pred ==test$Playlist))


##################################### Modelo 2
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

# Previsão do banco teste
table(rg2.pred, test$Playlist)
xtable(table(rg2.pred, test$Playlist), caption = "Matriz de confusão do teste do modelo 2 regressão logística", label = "par 2 teste mod 2 reg log")

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg2.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg2.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg2.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg2.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg2.prob)
# Curva ROC do modelo completo
plotROC(test$Playlist1, rg2.prob)

# Acerto da previsão
mean((rg2.pred ==test$Playlist))
# Erro da previsão
1 - mean((rg2.pred ==test$Playlist))


##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Duration, Instrumentalness, Liveness, Loudness

# Previsão do banco teste
table(rg3.pred, test$Playlist)
xtable(table(rg3.pred, test$Playlist), caption = "Matriz de confusão do teste do modelo 3", label = "par 3 teste mod 3 reg log")

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg3.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg3.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg3.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg3.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg3.prob)
# Curva ROC do modelo completo
plotROC(test$Playlist1, rg3.prob)

# Acerto da previsão
mean((rg3.pred ==test$Playlist))
# Erro da previsão
1 - mean((rg3.pred ==test$Playlist))


###################################################################################################################################################################
##################################### Resetando os bancos de treino e teste
###################################################################################################################################################################

train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]


###################################################################################################################################################################
##################################### Random Forest - Modelos
###################################################################################################################################################################

## Spooning VS Power Hour

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
set.seed(12)
modelrf1 <- randomForest(Playlist ~ Acousticness + Danceability + Duration + Energy 
                         + Instrumentalness + Key + Liveness + Loudness + Mode 
                         + Speechiness + Tempo + Valence
                         ,data = train, importance = TRUE)
modelrf1

xtable(modelrf1$confusion, caption = "Matriz de confusão random forest do treino do modelo 1.", label = "par 1 treino mod 1 ran for")
xtable(t(modelrf1$confusion), caption = "Matriz de confusão random forest do treino do modelo 1.", label = "par 1 mod 1 treino ran for")

par(mar = c(4,4,2,10), xpd=T)
plot(modelrf1, main = NA)
legend(x="right",inset = c(-0.58,0),
       legend= c("Média", "Power Hour", "Spooning"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

## Verificando as variáveis importantes para o modelo de Random Forest
importance(modelrf1)
varImpPlot(modelrf1, main = NA)


##################################### Modelo 2
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
set.seed(12)
modelrf2 <- randomForest(Playlist ~ Acousticness + Danceability + Duration 
                         + Instrumentalness + Key + Liveness + Loudness + Mode 
                         + Speechiness + Tempo + Valence
                         ,data = train, importance = TRUE)
modelrf2

xtable(modelrf2$confusion, caption = "Matriz de confusão random forest do treino do modelo 2", label = "par 3 treino mod 2 ran for")
xtable(t(modelrf2$confusion), caption = "Matriz de confusão random forest do treino do modelo 2.", label = "par 3 mod 2 treino ran for")


par(mar = c(4,4,2,10), xpd=T)
plot(modelrf2, main = NA)
legend(x="right",inset = c(-0.58,0),
       legend= c("Média", "Power Hour", "Spooning"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

## Verificando as variáveis importantes para o modelo de Random Forest
importance(modelrf2)
varImpPlot(modelrf2, main = NA)


##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Duration, Instrumentalness, Liveness, Loudness
set.seed(12)
modelrf3 <- randomForest(Playlist ~ Acousticness + Duration 
                         + Instrumentalness + Liveness + Loudness 
                         ,data = train, importance = TRUE)
modelrf3

xtable(modelrf3$confusion, caption = "Matriz de confusão random forest do treino do modelo 3", label = "par 3 treino mod 3 ran for")
xtable(t(modelrf3$confusion), caption = "Matriz de confusão random forest do treino do modelo 3.", label = "par 3 mod 3 treino ran for")

par(mar = c(4,4,2,10), xpd=T)
plot(modelrf3, main = NA)
legend(x="right",inset = c(-0.58,0),
       legend= c("Média", "Power Hour", "Spooning"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

## Verificando as variáveis importantes para o modelo de Random Forest
importance(modelrf3)
varImpPlot(modelrf3, main = NA)


###################################################################################################################################################################
##################################### Random Forest - Previsões
###################################################################################################################################################################


##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

# Resultado do modelo com o banco de treino
modelrf1$confusion

# Previsão do modelo no banco de teste
predValid1 <- predict(modelrf1, test, type = "class")
table(predValid1, test$Playlist)
xtable(table(predValid1, test$Playlist), caption = "Matriz de confusão random forest da previsão do modelo 1.", label = "par 3 mod 1 prev ran for")

# Acerto da previsão
mean((predValid1 ==test$Playlist))
# Erro da previsão
1 - mean((predValid1 ==test$Playlist))

# Previsão muito boa, para as duas playlists. Erro geral de 1,43%

##################################### Modelo 2
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

# Resultado do modelo com o banco de treino
modelrf2$confusion

# Previsão do modelo no banco de teste
predValid2 <- predict(modelrf2, test, type = "class")
table(predValid2, test$Playlist)
xtable(table(predValid2, test$Playlist), caption = "Matriz de confusão random forest da previsão do modelo 2.", label = "par 3 mod 2 prev ran for")

# Acerto da previsão
mean((predValid2 ==test$Playlist))
# Erro da previsão
1 - mean((predValid2 ==test$Playlist))

# Previsão muito boa também para ambas as playlists. Erro geral de 4,29%

##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Duration, Instrumentalness, Liveness, Loudness

# Resultado do modelo com o banco de treino
modelrf3$confusion

# Previsão do modelo no banco de teste
predValid3 <- predict(modelrf3, test, type = "class")
table(predValid3, test$Playlist)
xtable(table(predValid3, test$Playlist), caption = "Matriz de confusão random forest da previsão do modelo 3.", label = "par 3 mod 3 teste ran for")

# Acerto da previsão
mean((predValid3 ==test$Playlist))
# Erro da previsão
1 - mean((predValid3 ==test$Playlist))

# Previsão muito boa para ambas as playlists. Erro geral de 7,14%


###################################################################################################################################################################
##################################### Redes Neurais
###################################################################################################################################################################

## Spooning VS Power Hour

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

##################################### Modelo 2
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Duration, Instrumentalness, Liveness, Loudness


################################################################################
######## Método 1
################################################################################
# Generating random number
# Using set.seed()
set.seed(500)

# Import required library
library(MASS)

# Import data set
data <- dbfac
head(dbfac)


# Split the dataset to
# Test and train set
index <- sample(1:nrow(data),
                round(0.75 * nrow(data)))
train <- data[index, ]
test <- data[-index, ]

# Fit the model
lm.fit <- glm(medv~., data = train)
summary(lm.fit)
pr.lm <- predict(lm.fit, test)
MSE.lm <- sum((pr.lm - test$medv)^2) / nrow(test)

# Fit the neural network
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data,
                              center = mins,
                              scale = maxs - mins))

train_ <- scaled[index, ]
test_ <- scaled[-index, ]

# Import neuralnet library
#install.packages("neuralnet")
library(neuralnet)

# Work on parameters
# of hidden layers of NN
n <- names(train_)
f <- as.formula(paste("medv ~",
                      paste(n[! n %in% "medv"],
                            collapse = " + ")))
nn <- neuralnet(f,
                data = train_,
                hidden = c(5, 3),
                linear.output = T)



# Predicting the medv
pr.nn <- compute(nn, test_[, 1:13])

pr.nn_ <- pr.nn$net.result * (max(data$medv)
                              - min(data$medv)) + min(data$medv)

test.r <- (test_$medv) * (max(data$medv)
                          - min(data$medv)) + min(data$medv)

MSE.nn <- sum((test.r - pr.nn_)^2) / nrow(test_)


# Plotting the final graph
plot(test$medv, pr.nn_,
     col = 'green',
     main = 'Real vs predicted NN',
     pch = 18, cex = 0.7)
points(test$medv, pr.lm,
       col = 'blue',
       pch = 18, cex = 0.7)
abline(0, 1, lwd = 2)
legend('bottomright',
       legend = c('Real', 'Predicted'),
       pch = 18,
       col = c('green', 'blue'))

################################################################################
######## Método 2 -  esse é o que vai funcionar
################################################################################
## https://www.geeksforgeeks.org/how-neural-networks-are-used-for-classification-in-r-programming/

library(neuralnet)
library(caret)

data("iris")
str(iris)

set.seed(123)

indexes = createDataPartition(iris$Species, p=.85, list = F)
train = iris[indexes,]
test = iris[-indexes,]

xtest = test[, -5]
ytest = test[,  5]

nnet=neuralnet(Species ~ ., train, hidden = c(10,8,4,3), linear.output = FALSE)

plot(nnet)

ypred = neuralnet::compute(nnet, xtest)

yhat = ypred$net.result
print(yhat)

yhat = data.frame("yhat"=ifelse(max.col(yhat[,1:3])==1, "setosa",
                         ifelse(max.col(yhat[,1:3])==2, "versicolor","virginica")))

yhat$yhat<-as.factor(yhat$yhat)
cm = confusionMatrix(as.factor(ytest), yhat$yhat)

cm$table

print (cm)



# https://www.datatechnotes.com/2017/10/neural-networks-example-in-r.html#:~:text=The%20neural%20network%20models%20are,method%20of%20biological%20neural%20networks.

# http://uc-r.github.io/ann_classification
# https://www.r-bloggers.com/2013/10/classification-using-neural-net-in-r/

# https://datascienceplus.com/neuralnet-train-and-test-neural-networks-using-r/


###################################################################################################################################################################
##################################### Validação Cruzada
###################################################################################################################################################################

## Spooning vs Power Hour


###################################################################################################################################################################
##################################### K-Folds
###################################################################################################################################################################

## Spooning vs Power Hour

###################################################################################################################################################################
##################################### Resetando bases e manipulando dados
###################################################################################################################################################################

## o método de validação cruzada, por algum motivo exige que os fatores tenham apenas 1 palavra

## usar a validação cruzada no banco completo ou só na amostra de treino 
## e fazer uma nova previsão do banco de teste?

dbfac1<-dbfac
dbfac1$Playlist <-ifelse(dbfac$Playlist=="Power Hour", "Power", "Spoon")
dbfac1$Playlist <-as.factor(dbfac1$Playlist)


###################################################################################################################################################################
##################################### K-Folds - Regressão Logística
###################################################################################################################################################################

## https://www.statology.org/k-fold-cross-validation-in-r/
## https://rforhr.com/kfold.html

## Criando a função kf que define o tipo de validação cruzada utilizada. Neste momento, estamos usando o método k-folds
## Vamos testar a diferença entre repetições no k-folds e sem repetições.
kf <- trainControl(method="cv", number=10, savePredictions="all", classProbs=TRUE)
#kf <- trainControl(method="repeatedcv", number=10, savePredictions="all", classProbs=TRUE, repeats = 10)

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
kmodelrg1<- train(Playlist ~ Acousticness + Danceability + Duration + Energy 
                  + Instrumentalness + Key + Liveness + Loudness + Mode 
                  + Speechiness + Tempo + Valence
                  ,data=dbfac1, method="glm", family='binomial', trControl=kf)

summary(kmodelrg1)
xtable(summary(kmodelrg1))
## AIC = 26

kmodelrg1$resampledCM
kfrg1<-kmodelrg1$resampledCM[,-5:-6]

rownames(kfrg1)<-c("Fold 1","Fold 2","Fold 3","Fold 4","Fold 5","Fold 6","Fold 7","Fold 8","Fold 9","Fold 10")
colnames(kfrg1)<-c("Acerto","Erro","Erro","Acerto")
kfrg1
xtable(kfrg1, caption = "Resultado de cada fold do modelo 1 para regressão logística", label = "par 1 kfolds mod 1 reg log")

table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])
xtable(table(kmodelrg1$pred[,1], kmodelrg1$pred[,2]), caption = "Resultado dos 10-folds do modelo 1", label = "par 1 kfolds mod 1 reg log")


table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


kmodelrg1$results[,-1]
xtable(kmodelrg1$results[,-1])
1-kmodelrg1$results[2]
## Accuracy de 56,57% de acerto na previsão média de todas as instâncias
## Kappa de 7,8%. Entre 1% e 20% - muito leve

## Interpretação de Kappa
##       100% Kappa perfeita distinção
##  99% - 81% Kappa quase perfeita
##  80% - 61% Kappa substancial
##  60% - 41% Kappa moderada
##  40% - 21% Kappa leve
##  20% -  1% Kappa muito leve
##         0% Kappa sem distinção

##################################### Modelo 2
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
kmodelrg2<- train(Playlist ~ Acousticness + Danceability + Duration
                  + Instrumentalness + Key + Liveness + Loudness + Mode 
                  + Speechiness + Tempo + Valence
                  ,data=dbfac1, method="glm", family='binomial', trControl=kf)

summary(kmodelrg2)
xtable(summary(kmodelrg2))
## AIC = 70,512

kmodelrg2$resampledCM
kfrg2<-kmodelrg2$resampledCM[,-5:-6]

rownames(kfrg2)<-c("Fold 1","Fold 2","Fold 3","Fold 4","Fold 5","Fold 6","Fold 7","Fold 8","Fold 9","Fold 10")
colnames(kfrg2)<-c("Acerto","Erro","Erro","Acerto")
kfrg2
xtable(kfrg2, caption = "Resultado de cada fold do modelo 2 para regressão logística", label = "par 1 kfolds mod 2 reg log")

table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])
xtable(table(kmodelrg2$pred[,1], kmodelrg2$pred[,2]), caption = "Resultado dos 10-folds do modelo 2", label = "par 1 kfolds mod 2 reg log")

table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


kmodelrg2$results[,-1]
xtable(kmodelrg2$results[,-1])
1-kmodelrg2$results[2]
## Accuracy de 56,57% de acerto na previsão média de todas as instâncias
## Kappa de 7,8%. Entre 1% e 20% - muito leve


##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Duration, Instrumentalness, Liveness, Loudness
kmodelrg3<- train(Playlist ~ Acousticness + Duration
                  + Instrumentalness + Liveness + Loudness
                  ,data=dbfac1, method="glm", family='binomial', trControl=kf)

summary(kmodelrg3)
xtable(summary(kmodelrg3))
## AIC = 65,76

kmodelrg3$resampledCM
kfrg3<-kmodelrg3$resampledCM[,-5:-6]

rownames(kfrg3)<-c("Fold 1","Fold 2","Fold 3","Fold 4","Fold 5","Fold 6","Fold 7","Fold 8","Fold 9","Fold 10")
colnames(kfrg3)<-c("Acerto","Erro","Erro","Acerto")
kfrg3
xtable(kfrg3, caption = "Resultado de cada fold do modelo 3 para regressão logística", label = "par 1 kfolds mod 3 reg log")

table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])
xtable(table(kmodelrg3$pred[,1], kmodelrg3$pred[,2]), caption = "Resultado dos 10-folds do modelo 3", label = "par 1 kfolds mod 3 reg log")

table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


kmodelrg3$results[-1]
xtable(kmodelrg3$results[,-1])
1-kmodelrg3$results[2]
## Accuracy de 56,57% de acerto na previsão média de todas as instâncias
## Kappa de 7,8%. Entre 1% e 20% - muito leve


###################################################################################################################################################################
##################################### K-Folds - Random Forest
###################################################################################################################################################################

## https://rdrr.io/github/marccamb/optiranger/man/rf.kfold.html

## https://rpubs.com/jvaldeleon/forest_repeat_cv

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
kmodelrf1<- train(Playlist ~ Acousticness + Danceability + Duration + Energy 
                  + Instrumentalness + Key + Liveness + Loudness + Mode
                  + Speechiness + Tempo + Valence
                  ,data=dbfac1, method="rf", family='binomial', trControl=kf)

kmodelrf1$finalModel
kmodelrf1$finalModel$confusion
xtable(kmodelrf1$finalModel$confusion)

varImp(kmodelrf1)
plot(varImp(kmodelrf1), main = NA)

plot((kmodelrf1$finalModel), main = NA)
legend(x="top",
       legend= c("Média", "Power Hour", "Spooning"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

kmodelrf1$resampledCM

kfrf1<-kmodelrf1$resampledCM[order(kmodelrf1$resampledCM$mtry),]

kfrf1<-kfrf1[1:10,-6]


rownames(kfrf1)<-c()
colnames(kfrf1)<-c()
rownames(kfrf1)<-c("Fold 1","Fold 2","Fold 3","Fold 4","Fold 5","Fold 6","Fold 7","Fold 8","Fold 9","Fold 10")
colnames(kfrf1)<-c("Acerto","Erro","Erro","Acerto", "mtry")
kfrf1
xtable(kfrf1, caption = "Resultado de cada fold do modelo 1 para random forest", label = "par 2 kfolds mod 1 ran for")

table(kmodelrf1$pred[,1], kmodelrf1$pred[,2])
xtable(table(kmodelrf1$pred[,1], kmodelrf1$pred[,2]), caption = "Resultado dos 10-folds do modelo 1", label = "par 2 kfolds mod 1 ran for")

kmodelrf1

ab<-list()
ab<-kmodelrf1$pred
ab<-ab[order(ab$mtry),]
kmodelrf1mtry2<-list()
kmodelrf1mtry2<-ab[1:250, ]

table(amostra)
table(kmodelrf1mtry2[,1], kmodelrf1mtry2[,2])
table(kmodelrf1mtry2[,1], kmodelrf1mtry2[,2])[1]*70/250 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf1mtry2[,1], kmodelrf1mtry2[,2])[2]*70/250 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf1mtry2[,1], kmodelrf1mtry2[,2])[3]*70/250 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf1mtry2[,1], kmodelrf1mtry2[,2])[4]*70/250 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


kmodelrf1$results
xtable(kmodelrf1$results)
1-kmodelrf1$results[2]
## Accuracy de 97,6% de acerto na previsão média de todas as instâncias
## Kappa de 94,99%. Entre 81% e 99% - quase perfeita

## Interpretação de Kappa
##       100% Kappa perfeita distinção
##  99% - 81% Kappa quase perfeita
##  80% - 61% Kappa substancial
##  60% - 41% Kappa moderada
##  40% - 21% Kappa leve
##  20% -  1% Kappa muito leve
##         0% Kappa sem distinção

##################################### Modelo 2
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
kmodelrf2<- train(Playlist ~ Acousticness + Danceability + Duration
                  + Instrumentalness + Key + Liveness + Loudness + Mode 
                  + Speechiness + Tempo + Valence
                  ,data=dbfac1, method="rf", family='binomial', trControl=kf)

kmodelrf2$finalModel

kmodelrf2$finalModel$confusion

xtable(kmodelrf2$finalModel$confusion)

varImp(kmodelrf2)
plot(varImp(kmodelrf2), main = NA)

plot((kmodelrf2$finalModel), main = NA)
legend(x="top",
       legend= c("Média", "Power Hour", "Spooning"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

kmodelrf2$resampledCM

kfrf2<-kmodelrf2$resampledCM[order(kmodelrf2$resampledCM$mtry),]

kfrf2<-kfrf2[1:10,-6]


rownames(kfrf2)<-c()
colnames(kfrf2)<-c()
rownames(kfrf2)<-c("Fold 1","Fold 2","Fold 3","Fold 4","Fold 5","Fold 6","Fold 7","Fold 8","Fold 9","Fold 10")
colnames(kfrf2)<-c("Acerto","Erro","Erro","Acerto", "mtry")
kfrf2
xtable(kfrf2, caption = "Resultado de cada fold do modelo 2 para random forest", label = "par 1 kfolds mod 2 ran for")

table(kmodelrf2$pred[,1], kmodelrf2$pred[,2])
xtable(table(kmodelrf2$pred[,1], kmodelrf2$pred[,2]), caption = "Resultado dos 10-folds do modelo 2", label = "par 1 kfolds mod 2 ran for")

kmodelrf2

ab<-list()
ab<-kmodelrf2$pred
ab<-ab[order(ab$mtry),]
kmodelrf2mtry2<-list()
kmodelrf2mtry2<-ab[1:250, ]


table(kmodelrf2mtry2[,1], kmodelrf2mtry2[,2])[1]*70/250 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf2mtry2[,1], kmodelrf2mtry2[,2])[2]*70/250 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf2mtry2[,1], kmodelrf2mtry2[,2])[3]*70/250 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf2mtry2[,1], kmodelrf2mtry2[,2])[4]*70/250 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


kmodelrf2$results
xtable(kmodelrf2$results)
1-kmodelrf2$results[2]
## Accuracy de 96% de acerto na previsão média de todas as instâncias
## Kappa de 91,63%. Entre 81% e 99% - quase perfeita


##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Duration, Instrumentalness, Liveness, Loudness
kmodelrf3<- train(Playlist ~ Acousticness + Duration 
                  + Instrumentalness + Liveness + Loudness 
                  ,data=dbfac1, method="rf", family='binomial', trControl=kf)

kmodelrf3$finalModel
kmodelrf3$finalModel$confusion

xtable(kmodelrf3$finalModel$confusion)

varImp(kmodelrf3)
plot(varImp(kmodelrf3), main = NA)

plot((kmodelrf3$finalModel), main = NA)
legend(x="top",
       legend= c("Média", "Power Hour", "Spooning"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

kmodelrf3$resampledCM

kfrf3<-kmodelrf3$resampledCM[order(kmodelrf3$resampledCM$mtry),]

kfrf3<-kfrf3[1:10,-6]


rownames(kfrf3)<-c()
colnames(kfrf3)<-c()
rownames(kfrf3)<-c("Fold 1","Fold 2","Fold 3","Fold 4","Fold 5","Fold 6","Fold 7","Fold 8","Fold 9","Fold 10")
colnames(kfrf3)<-c("Acerto","Erro","Erro","Acerto", "mtry")
kfrf3
xtable(kfrf3, caption = "Resultado de cada fold do modelo 3  para random forest", label = "par 1 kfolds mod 3 ran for")

table(kmodelrf3$pred[,1], kmodelrf3$pred[,2])
xtable(table(kmodelrf3$pred[,1], kmodelrf3$pred[,2]), caption = "Resultado dos 10-folds do modelo 3", label = "par 1 kfolds mod 3 ran for")

kmodelrf3

ab<-list()
ab<-kmodelrf3$pred
ab<-ab[order(ab$mtry),]
kmodelrf3mtry2<-list()
kmodelrf3mtry2<-ab[1:250, ]


## Melhor previsão de regressão linear foi feita pelo primeiro modelo

table(kmodelrf3mtry2[,1], kmodelrf3mtry2[,2])[1]*70/250 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf3mtry2[,1], kmodelrf3mtry2[,2])[2]*70/250 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf3mtry2[,1], kmodelrf3mtry2[,2])[3]*70/250 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf3mtry2[,1], kmodelrf3mtry2[,2])[4]*70/250 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


kmodelrf3$results
xtable(kmodelrf3$results)
1-kmodelrf3$results[2]
## Accuracy de 49,42% de acerto na previsão média de todas as instâncias
## Kappa de 5,47%. Entre 1% e 20% - muito leve.


###################################################################################################################################################################
##################################### K-Folds - Redes Neurais
###################################################################################################################################################################


##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

##################################### Modelo 2
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Duration, Instrumentalness, Liveness, Loudness


###################################################################################################################################################################
##################################### Leave One Out
###################################################################################################################################################################

## Spooning vs Power Hour

## LOO manual (possivelmente terei de usar isso nas redes neurais)
#First initialize the output vector as an empty object outside the loop.
#fitted_value <- NULL
#for(i in 1:53){
#  #you did this part right
#  validation<-ironslag[i,]
#  training<-ironslag[-i,]
#  model1<-lm(magnetic ~ chemical, data = training)
#  #when you fit the model, use the newdata argument to predict on a new row
#  #also, fitted_value needs the index [i], so the each loop doesn't overwrite the previous
#  fitted_value[i] <- predict(model1, newdata = validation)}
#
## https://www.geeksforgeeks.org/loocvleave-one-out-cross-validation-in-r-programming/#:~:text=LOOCV(Leave%20One%20Out%20Cross%2DValidation)%20is%20a%20type,considered%20as%20the%20training%20set.
## https://www.statology.org/leave-one-out-cross-validation-in-r/

loo <- trainControl(method="LOOCV", savePredictions= "all", classProbs = TRUE)
#loo <- trainControl(method="LOOCV")

###################################################################################################################################################################
##################################### Leave One Out - Regressão Logística
###################################################################################################################################################################

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
loomodelrg1<- train(Playlist ~ Acousticness + Danceability + Duration + Energy 
                    + Instrumentalness + Key + Liveness + Loudness + Mode 
                    + Speechiness + Tempo + Valence
                    ,data=dbfac1, method="glm", family='binomial', trControl=loo)

summary(loomodelrg1)
xtable(summary(loomodelrg1))
## AIC = 26

table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])
xtable(table(loomodelrg1$pred[,1], loomodelrg1$pred[,2]))


table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[1]*70/250 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[2]*70/250 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[3]*70/250 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[4]*70/250 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10

## Comparar com k-folds e reg log normal


loomodelrg1$results
xtable(loomodelrg1$results[,-1])
1-loomodelrg1$results[2]
## Accuracy de 55,71% de acerto na previsão média de todas as instâncias
## Kappa de 6,54%. Entre 1% e 20% - muito leve

## Interpretação de Kappa
##       100% Kappa perfeita distinção
##  99% - 81% Kappa quase perfeita
##  80% - 61% Kappa substancial
##  60% - 41% Kappa moderada
##  40% - 21% Kappa leve
##  20% -  1% Kappa muito leve
##         0% Kappa sem distinção

##################################### Modelo 2
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
loomodelrg2<- train(Playlist ~ Acousticness + Danceability + Duration
                    + Instrumentalness + Key + Liveness + Loudness + Mode 
                    + Speechiness + Tempo + Valence
                    ,data=dbfac1, method="glm", family='binomial', trControl=loo)

summary(loomodelrg2)
xtable(summary(loomodelrg2))

## AIC = 41,45

table(loomodelrg2$pred[,1], loomodelrg2$pred[,2])
xtable(table(loomodelrg2$pred[,1], loomodelrg2$pred[,2]))

table(loomodelrg2$pred[,1], loomodelrg2$pred[,2])[1]*70/250 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg2$pred[,1], loomodelrg2$pred[,2])[2]*70/250 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg2$pred[,1], loomodelrg2$pred[,2])[3]*70/250 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg2$pred[,1], loomodelrg2$pred[,2])[4]*70/250 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10

## Comparar com k-folds e reg log normal


loomodelrg2$results
xtable(loomodelrg2$results[,-1])
1-loomodelrg2$results[2]
## Accuracy de 95,2% de acerto na previsão média de todas as instâncias
## Kappa de 90,03%. Entre 1% e 20% - muito leve


##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Duration, Instrumentalness, Liveness, Loudness
loomodelrg3<- train(Playlist ~ Acousticness + Duration 
                    + Instrumentalness + Liveness + Loudness
                    ,data=dbfac1, method="glm", family='binomial', trControl=loo)

summary(loomodelrg3)
xtable(summary(loomodelrg3))

## AIC = 70,512

table(loomodelrg3$pred[,1], loomodelrg3$pred[,2])
xtable(table(loomodelrg3$pred[,1], loomodelrg3$pred[,2]))

table(loomodelrg3$pred[,1], loomodelrg3$pred[,2])[1]*70/250 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg3$pred[,1], loomodelrg3$pred[,2])[2]*70/250 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg3$pred[,1], loomodelrg3$pred[,2])[3]*70/250 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg3$pred[,1], loomodelrg3$pred[,2])[4]*70/250 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10

## Comparar com k-folds e reg log normal


loomodelrg3$results[,-1]
xtable(loomodelrg3$results[,-1])
1-loomodelrg3$results[2]
## Accuracy de 93,6% de acerto na previsão média de todas as instâncias
## Kappa de 86,76%. Entre 1% e 20% - muito leve


###################################################################################################################################################################
##################################### Leave One Out - Random Forest
###################################################################################################################################################################

## https://rdrr.io/github/marccamb/optiranger/man/rf.kfold.html

## https://rpubs.com/jvaldeleon/forest_repeat_cv

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
loomodelrf1<- train(Playlist ~ Acousticness + Danceability + Duration + Energy 
                    + Instrumentalness + Key + Liveness + Loudness + Mode 
                    + Speechiness + Tempo + Valence
                    ,data=dbfac1, method="rf", family='binomial', trControl=loo)

## demorou 3 minutos pra rodar

loomodelrf1$finalModel

loomodelrf1$finalModel$confusion
xtable(loomodelrf1$finalModel$confusion)

loomodelrf1$finalModel$importance
loomodelrf1

varImp(loomodelrf1)
plot(varImp(loomodelrf1), main = NA)

plot(loomodelrf1$finalModel, main = NA)
legend(x="top",
       legend= c("Média", "Power Hour", "Spooning"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)


loomodelrf1

ab<-list()
ab<-loomodelrf1$pred
ab<-ab[order(ab$mtry),]
loomodelrf1mtry2<-list()
loomodelrf1mtry2<-ab[1:250,]


table(loomodelrf1mtry2[,1], loomodelrf1mtry2[,2])
table(loomodelrf1mtry2[,1], loomodelrf1mtry2[,2])[1]*70/250 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf1mtry2[,1], loomodelrf1mtry2[,2])[2]*70/250 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf1mtry2[,1], loomodelrf1mtry2[,2])[3]*70/250 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf1mtry2[,1], loomodelrf1mtry2[,2])[4]*70/250 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


loomodelrf1$results
xtable(loomodelrf1$results)
1-loomodelrf1$results[2]
## Accuracy de 98,4% de acerto na previsão média de todas as instâncias
## Kappa de 96,68%. Entre 81% e 100% - quase perfeita

## Interpretação de Kappa
##       100% Kappa perfeita distinção
##  99% - 81% Kappa quase perfeita
##  80% - 61% Kappa substancial
##  60% - 41% Kappa moderada
##  40% - 21% Kappa leve
##  20% -  1% Kappa muito leve
##         0% Kappa sem distinção


##################################### Modelo 2
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
loomodelrf2<- train(Playlist ~ Acousticness + Danceability + Duration
                    + Instrumentalness + Key + Liveness + Loudness + Mode 
                    + Speechiness + Tempo + Valence
                    ,data=dbfac1, method="rf", family='binomial', trControl=loo)

loomodelrf2$finalModel

loomodelrf2$finalModel$confusion
xtable(loomodelrf2$finalModel$confusion)

loomodelrf2$finalModel$importance
loomodelrf2

varImp(loomodelrf2)
plot(varImp(loomodelrf2), main = NA)

plot(loomodelrf2$finalModel, main = NA)
legend(x="top",
       legend= c("Média", "Power Hour", "Spooning"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)


loomodelrf2

ab<-list()
ab<-loomodelrf2$pred
ab<-ab[order(ab$mtry),]
loomodelrf2mtry2<-list()
loomodelrf2mtry2<-ab[1:250,]


table(loomodelrf2mtry2[,1], loomodelrf2mtry2[,2])
table(loomodelrf2mtry2[,1], loomodelrf2mtry2[,2])[1]*70/250 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf2mtry2[,1], loomodelrf2mtry2[,2])[2]*70/250 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf2mtry2[,1], loomodelrf2mtry2[,2])[3]*70/250 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf2mtry2[,1], loomodelrf2mtry2[,2])[4]*70/250 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


loomodelrf2$results
xtable(loomodelrf2$results)
1-loomodelrf2$results[2]
## Accuracy de 46,86% de acerto na previsão média de todas as instâncias
## Kappa de 11,66%. Entre 1% e 20% - muito leve


##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Duration, Instrumentalness, Liveness, Loudness
loomodelrf3<- train(Playlist ~ Acousticness + Duration 
                    + Instrumentalness + Liveness + Loudness
                    ,data=dbfac1, method="rf", family='binomial', trControl=loo)

loomodelrf3$finalModel

loomodelrf3$finalModel$confusion
xtable(loomodelrf3$finalModel$confusion)

loomodelrf3$finalModel$importance
loomodelrf3

varImp(loomodelrf3)
plot(varImp(loomodelrf3), main = NA)

plot(loomodelrf3$finalModel, main = NA)
legend(x="top",
       legend= c("Média", "Power Hour", "Spooning"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)


loomodelrf3

ab<-list()
ab<-loomodelrf3$pred
ab<-ab[order(ab$mtry),]
loomodelrf3mtry3<-list()
loomodelrf3mtry3<-ab[251:500,]


table(loomodelrf3mtry3[,1], loomodelrf3mtry3[,2])
table(loomodelrf3mtry3[,1], loomodelrf3mtry3[,2])[1]*70/250 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf3mtry3[,1], loomodelrf3mtry3[,2])[2]*70/250 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf3mtry3[,1], loomodelrf3mtry3[,2])[3]*70/250 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf3mtry3[,1], loomodelrf3mtry3[,2])[4]*70/250 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


loomodelrf3$results
xtable(loomodelrf3$results)
1-loomodelrf3$results[2]
## Accuracy de 46,86% de acerto na previsão média de todas as instâncias
## Kappa de 11,66%. Entre 1% e 20% - muito leve

## testar com leave one out e ver se os métodos kfolds e leave one out
## concordam entre os modelos testados (ex: ambos concordam que model 1 é melhor)


###################################################################################################################################################################
##################################### Leave One Out - Redes Neurais
###################################################################################################################################################################

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

##################################### Modelo 2
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Mode, Speechiness, Tempo, Valence

##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Duration, Instrumentalness, Liveness, Loudness


###################################################################################################################################################################
###################################################################################################################################################################
##################################### Todas as Playlists
###################################################################################################################################################################
###################################################################################################################################################################


###################################################################################################################################################################
##################################### União e partição do banco
###################################################################################################################################################################
set.seed(12)
db1<-rbind(dbbeast1,dbpiano1, dbpower1, dbsucks1, dbalone1, dbspoon1)
dbfac<-db1

dbfac$Playlist<-as.factor(dbfac$Playlist)


###################################################################################################################################################################
##################################### Manipulando os dados
###################################################################################################################################################################
g1dbf <- data.frame('Speechiness'=db1$Speechiness, 'Liveness'=db1$Liveness, 'Instrumentalness'=db1$Instrumentalness, 
                    'Acousticness'=db1$Acousticness, 'Playlist'=db1$Playlist, 'ID'=db1$`Track URI`)

g2dbf <- data.frame('Valence'=db1$Valence, 'Energy'=db1$Energy, 'Danceability'=db1$Danceability,
                    'Playlist'=db1$Playlist, 'ID'=db1$`Track URI`)

g3dbf<- data.frame('BPM'=db1$Tempo, 'Playlist'=db1$Playlist, 'ID'=db1$`Track URI`)

g4dbf<- data.frame('Duration'=db1$Duration, 'Playlist'=db1$Playlist, 'ID'=db1$`Track URI`)

g5dbf<- data.frame('Loudness'=db1$Loudness, 'Playlist'=db1$Playlist, 'ID'=db1$`Track URI`)


g1dbfreshaped <-reshape2::melt(g1dbf)
g2dbfreshaped <-reshape2::melt(g2dbf)
g3dbfreshaped <-reshape2::melt(g3dbf)
g4dbfreshaped <-reshape2::melt(g4dbf)
g5dbfreshaped <-reshape2::melt(g5dbf)


###################################################################################################################################################################
##################################### Boxplots
###################################################################################################################################################################

## Decidir qual estilo de boxplot utilizar

## 1 legenda no topo. Mais espaço horizontal pro gráfico
ggplot(g1dbfreshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + coord_flip() + theme(legend.position = "top")
## 2 legenda na direta. Mais "clássico"
ggplot(g1dbfreshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + coord_flip() + theme_classic()

ggplot(g2dbfreshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + coord_flip() + theme_classic()

ggplot(g3dbfreshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + theme_classic()

ggplot(g4dbfreshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + theme_classic()

ggplot(g5dbfreshaped, aes(variable, value)) + 
  geom_boxplot(aes(fill=Playlist)) + theme_classic()

## Através dos boxplots eu diria que não teremos mais problemas de "não-convergência" nos modelos de regressão logística
## Além disso, diria que todos os métodos terão dificuldade para diferenciar as playlists Alone Again, Life Sucks e Spooning entre si
## E as playlists Beast Mode e Power Hour entre si.
## Piano Relaxante deve ser bem prevista com facilidade.

###################################################################################################################################################################
##################################### Regressão Logística
###################################################################################################################################################################

###################################################################################################################################################################
##################################### Referência - Alone Again
###################################################################################################################################################################

dbfac$Playlist <-relevel(dbfac$Playlist, ref="Alone Again")
set.seed(12)

amostra <- sample(c(TRUE, FALSE), nrow(dbfac), replace=TRUE, prob=c(0.7,0.3))
train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]
table(amostra)

##################################### Modelo 1
###################################################################################################################################################################
## Todas as variáveis
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
modelrg1<- glm(Playlist ~ Acousticness + Danceability + Duration + Energy 
             + Instrumentalness + Key + Liveness + Loudness + Mode 
             + Speechiness + Tempo + Valence
             ,data=train, family='binomial')

summary(modelrg1)
xtable(summary(modelrg1), auto = F, caption = "Tabela de regressão do treino do modelo 1 para Alone Again contra as playlists restantes.", label = "alone mod 1 tabela reg log")

## AIC = 451,71


rg1.treino <- ifelse(modelrg1$fitted.values > 0.5, "Outras", "Alone Again")

table(rg1.treino, train$Playlist)

xtable(table(rg1.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 1 para Alone Again contra as playlists restantes.", label = "alone mod 1 treino reg log")

##################################### Modelo 2
###################################################################################################################################################################
## Apenas variáveis significantes para o modelo
## Danceability, Energy, Instrumentalness, Loudness, Mode, Speechiness, Tempo
modelrg2<- glm(Playlist ~ Danceability + Energy + 
               + Instrumentalness + Loudness + Mode
               + Speechiness + Tempo
               ,data=train, family='binomial')

summary(modelrg2)
xtable(summary(modelrg2), auto = F, caption = "Tabela de regressão do treino do modelo 2 para Alone Again contra as playlists restantes.", label = "alone mod 2 tabela reg log")
## AIC = 445,01

rg2.treino <- ifelse(modelrg2$fitted.values > 0.5, "Outras", "Alone Again")

table(rg2.treino, train$Playlist)

xtable(table(rg2.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 2 para Alone Again contra as playlists restantes.", label = "alone mod 2 treino reg log")

##################################### Modelo 3
###################################################################################################################################################################
## Apenas variávis significantes a alfa=0,05
## Danceability, Energy, Instrumentalness, Mode, Tempo
modelrg3<- stepAIC(modelrg1)
## igual ao modelo 2

summary(modelrg3)

## AIC = 449,37


###################################################################################################################################################################
##################################### Regressão Logística - Calculando Previsões
###################################################################################################################################################################

# Predição do banco teste utilizando cada modelo

##################################### Modelo 1
###################################################################################################################################################################
rg1.prob <- predict(modelrg1, test, type = "response")
rg1.pred <- ifelse(rg1.prob > 0.5, "Outras", "Alone Again")
rg1.pred1 <- ifelse(rg1.prob > 0.5, 1, 0)

##################################### Modelo 2
###################################################################################################################################################################
rg2.prob <- predict(modelrg2, test, type = "response")
rg2.pred <- ifelse(rg2.prob > 0.5, "Outras", "Alone Again")
rg2.pred1 <- ifelse(rg2.prob > 0.5, 1, 0)

##################################### Modelo 3
###################################################################################################################################################################
rg3.prob <- predict(modelrg3, test, type = "response")
rg3.pred <- ifelse(rg3.prob > 0.5, "Outras", "Alone Again")
rg3.pred1 <- ifelse(rg3.prob > 0.5, 1, 0)

###################################################################################################################################################################
##################################### Regressão Logística - Comparando Previsões
###################################################################################################################################################################

## Criando uma variável auxiliar para conseguir demonstrar os resultados de tabelas de confusão e curva ROC próximos
#################################################################################################################################
test<-mutate(test, Playlist1=Playlist)
train<-mutate(train, Playlist1=Playlist)

test$Playlist1 <-ifelse(test$Playlist=="Alone Again", 0, 1)
train$Playlist1 <-ifelse(train$Playlist=="Alone Again", 0, 1)
#################################################################################################################################

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

# Previsão do banco teste
table(rg1.pred, test$Playlist)
xtable(table(rg1.pred, test$Playlist), caption = "Matriz de confusão da previsão do modelo 1 para Alone Again contra as playlists restantes.", label = "alone mod 1 prev reg log")
table(rg1.pred, test$Playlist1)
xtable(table(rg1.pred, test$Playlist1), caption = "Matriz de confusão da previsão do modelo 1 simplificada para Alone Again contra as playlists restantes.", label = "alone mod 1 prev simp reg log")

alonerg<-list()
alonerg1<-list()
alonerg[[1]]<-table(rg1.pred, test$Playlist)
alonerg1[[1]]<-table(rg1.pred, test$Playlist1)


# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg1.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg1.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg1.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg1.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg1.prob)
# Curva ROC do modelo completo
plotROC(test$Playlist1, rg1.prob)


# Acerto da previsão
mean((rg1.pred1 ==test$Playlist1))
# Erro da previsão
1 - mean((rg1.pred1 ==test$Playlist1))


## Como estamos apenas testando a capacidade do modelo de diferenciar a playlist Alone Again de todas as outras, 
## precisamos tomar muito cuidado com as medidas de acerto e erro gerais
## O desbalanço entre a quantidade de observações dos 2 grupos acaba enganando tais medidas nos dando números bons, 
## afinal tivemos apenas 15% de erro
## Porém, se olharmos a medida de especificidade(verdadeiro negativo)\sensibilidade(verdadeiro positivo) 
## (lembrando que para o programa nosso evento de interesse é 0), veremos uma taxa de verdadeiro positivo de apenas 0,2
## Ou seja, das 45 observações da playlist Alone Again, o nosso modelo só acerto 9 (20%)
## E das 23 que ele previu como Alone Again, só acertou 39%

## Importante notar que as observações malclassificadas como Alone Again pertencem a Life Sucks e Spooning.

## Por estarmos trabalhando com um modelo binomial de regressão logística, acredito que,
## para todas as playlists os modelos se saíram "bem" no geral, mas mal nas medidas do evento de interesse


##################################### Modelo 2
###################################################################################################################################################################
## Acousticness, Danceability, Key, Loudness, Speechiness, Tempo

# Previsão do banco teste
table(rg2.pred, test$Playlist)
xtable(table(rg2.pred, test$Playlist), caption = "Matriz de confusão da previsão do modelo 2 para Alone Again contra as playlists restantes.", label = "alone mod 2 prev reg log")
table(rg2.pred, test$Playlist1)
xtable(table(rg2.pred, test$Playlist1), caption = "Matriz de confusão da previsão do modelo 2 simplificadanpara Alone Again contra as playlists restantes.", label = "alone mod 2 prev simp reg log")

alonerg[[2]]<-table(rg2.pred, test$Playlist)
alonerg1[[2]]<-table(rg2.pred, test$Playlist1)

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg2.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg2.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg2.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg2.prob)
# Percentual de Erro de classificação
misClassError(test$Playlist1, rg2.prob)
# Curva ROC
plotROC(test$Playlist1, rg2.prob)

# Acerto da previsão
mean((rg2.pred1 ==test$Playlist1))
# Erro da previsão
1 - mean((rg2.pred1 ==test$Playlist1))

## Resultado muito parecido com o anterior, mas dessa vez com uma melhora substancial na precisão, 
## porque o modelo classificou menos observações como evento de interesse
## Então foi uma melhora por ter diminuído a quantidade geral de previsões do evento de interesse

##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Danceability, Key, Speechiness

# Previsão do banco teste
table(rg3.pred, test$Playlist)
table(rg3.pred1, test$Playlist1)

alonerg[[3]]<-table(rg3.pred, test$Playlist)
alonerg1[[3]]<-table(rg3.pred, test$Playlist1)


# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg3.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg3.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg3.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg3.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg3.prob)
# Curva ROC
plotROC(test$Playlist1, rg3.prob)


# Acerto da previsão
mean((rg3.pred1 ==test$Playlist1))
# Erro da previsão
1 - mean((rg3.pred1 ==test$Playlist1))

## Resultado pior que os dois anteriores. Ainda menos previsões de evento de interesse, mas dessa vez a diminuição se deu nos acerto.

prevreglog<-list()
prevreglog[[1]]<-table(rg1.pred, test$Playlist)
prevreglog[[2]]<-table(rg2.pred, test$Playlist)
prevreglog[[3]]<-table(rg3.pred, test$Playlist)


###################################################################################################################################################################
##################################### Referência - Beast Mode
###################################################################################################################################################################

dbfac$Playlist <-relevel(dbfac$Playlist, ref="Beast Mode")
set.seed(12)

amostra <- sample(c(TRUE, FALSE), nrow(dbfac), replace=TRUE, prob=c(0.7,0.3))
train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]


##################################### Modelo 1
###################################################################################################################################################################
## Todas as variáveis
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
modelrg1<- glm(Playlist ~ Acousticness + Danceability + Duration + Energy 
               + Instrumentalness + Key + Liveness + Loudness + Mode 
               + Speechiness + Tempo + Valence
               ,data=train, family='binomial')

summary(modelrg1)
xtable(summary(modelrg1), auto = F, caption = "Tabela de regressão do treino do modelo 1 para Beast Mode contra as playlists restantes.", label = "beast mod 1 tabela reg log")
## AIC = 342.21

rg1.treino <- ifelse(modelrg1$fitted.values > 0.5, "Outras", "Beast Mode")

table(rg1.treino, train$Playlist)

xtable(table(rg1.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 1 para Beast Mode contra as playlists restantes.", label = "beast mod 1 treino reg log")

##################################### Modelo 2
###################################################################################################################################################################
## Apenas variáveis significantes para o modelo
## Acousticness, Danceability, Key, Loudness, Speechiness, Tempo
modelrg2<- glm(Playlist ~ Acousticness + Danceability
               + Key + Loudness
               + Speechiness + Tempo
               ,data=train, family='binomial')

summary(modelrg2)
xtable(summary(modelrg2), auto = F, caption = "Tabela de regressão do treino do modelo 2 para Beast Mode contra as playlists restantes.", label = "beast mod 2 tabela reg log")

## AIC = 333,21
rg2.treino <- ifelse(modelrg2$fitted.values > 0.5, "Outras", "Beast Mode")

table(rg2.treino, train$Playlist)

xtable(table(rg2.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 2 para Beast Mode contra as playlists restantes.", label = "beast mod 2 treino reg log")

##################################### Modelo 3
###################################################################################################################################################################
## Apenas variávis significativas a alfa=0,05
## Acousticness, Danceability, Speechiness
modelrg3<-stepAIC(modelrg1)
modelrg3<- glm(Playlist ~ Acousticness + Danceability
               + Speechiness
               ,data=train, family='binomial')

summary(modelrg3)
## acho que não vou usar esses putos

## AIC = 342.53


###################################################################################################################################################################
##################################### Regressão Logística - Calculando Previsões
###################################################################################################################################################################

# Predição do banco teste utilizando cada modelo

##################################### Modelo 1
###################################################################################################################################################################
rg1.prob <- predict(modelrg1, test, type = "response")
rg1.pred <- ifelse(rg1.prob > 0.5, "Outras", "Beast Mode")
rg1.pred1 <- ifelse(rg1.prob > 0.5, 1, 0)

##################################### Modelo 2
###################################################################################################################################################################
rg2.prob <- predict(modelrg2, test, type = "response")
rg2.pred <- ifelse(rg2.prob > 0.5, "Outras", "Beast Mode")
rg2.pred1 <- ifelse(rg2.prob > 0.5, 1, 0)

##################################### Modelo 3
###################################################################################################################################################################
rg3.prob <- predict(modelrg3, test, type = "response")
rg3.pred <- ifelse(rg3.prob > 0.5, "Outras", "Beast Mode")
rg3.pred1 <- ifelse(rg3.prob > 0.5, 1, 0)

###################################################################################################################################################################
##################################### Regressão Logística - Comparando Previsões
###################################################################################################################################################################

## Criando uma variável auxiliar para conseguir demonstrar os resultados de tabelas de confusão e curva ROC próximos
#################################################################################################################################
test<-mutate(test, Playlist1=Playlist)
train<-mutate(train, Playlist1=Playlist)

test$Playlist1 <-ifelse(test$Playlist=="Beast Mode", 0, 1)
train$Playlist1 <-ifelse(train$Playlist=="Beast Mode", 0, 1)
#################################################################################################################################

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

# Previsão do banco teste
table(rg1.pred, test$Playlist)
xtable(table(rg1.pred, test$Playlist), caption = "Matriz de confusão da previsão do modelo 1 para Beast Mode contra as playlists restantes.", label = "beast mod 1 prev reg log")
table(rg1.pred1, test$Playlist1)
xtable(table(rg1.pred, test$Playlist1), caption = "Matriz de confusão da previsão do modelo 1 simplificada para Beast Mode contra as playlists restantes.", label = "beast mod 1 prev simp reg log")

beastrg<-list()
beastrg1<-list()
beastrg[[1]]<-table(rg1.pred, test$Playlist)
beastrg1[[1]]<-table(rg1.pred, test$Playlist1)

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg1.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg1.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg1.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg1.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg1.prob)
# Curva ROC do modelo completo
plotROC(test$Playlist1, rg1.prob)


# Acerto da previsão
mean((rg1.pred1 ==test$Playlist1))
# Erro da previsão
1 - mean((rg1.pred1 ==test$Playlist1))


## Os resultados são bem melhores que a playlist anterior, contrariando minha opinião.
## Erro geral de apenas 10%, mas apesar do grande desbalanço entre os grupos 
## a taxa de verdadeiro positivo é 72,8%. E a taxa de preditivo positivo é 71,6%


##################################### Modelo 2
###################################################################################################################################################################
## Acousticness, Danceability, Key, Loudness, Speechiness, Tempo

# Previsão do banco teste
table(rg2.pred, test$Playlist)
xtable(table(rg2.pred, test$Playlist), caption = "Matriz de confusão da previsão do modelo 2 para Beast Mode contra as playlists restantes.", label = "beast mod 2 prev reg log")
table(rg2.pred1, test$Playlist1)
xtable(table(rg2.pred, test$Playlist1), caption = "Matriz de confusão da previsão do modelo 2 simplificada para Beast Mode contra as playlists restantes.", label = "beast mod 2 prev simp reg log")

beastrg[[2]]<-table(rg2.pred, test$Playlist)
beastrg1[[2]]<-table(rg2.pred, test$Playlist1)

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg2.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg2.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg2.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg2.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg2.prob)
# Curva ROC
plotROC(test$Playlist1, rg2.prob)

# Acerto da previsão
mean((rg2.pred1 ==test$Playlist1))
# Erro da previsão
1 - mean((rg2.pred1 ==test$Playlist1))

## Resultado muito parecido com o anterior, e semelhante ao ocorrido no segundo modelo da playlist anterior.
## O modelo classificou menos observações como evento de interesse.


##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Danceability, Key, Speechiness

# Previsão do banco teste
table(rg3.pred, test$Playlist)
table(rg3.pred1, test$Playlist1)

beastrg[[3]]<-table(rg3.pred, test$Playlist)
beastrg1[[3]]<-table(rg3.pred, test$Playlist1)

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg3.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg3.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg3.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg3.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg3.prob)
# Curva ROC
plotROC(test$Playlist1, rg3.prob)


# Acerto da previsão
mean((rg3.pred1 ==test$Playlist1))
# Erro da previsão
1 - mean((rg3.pred1 ==test$Playlist1))

## Resultado muito parecido com o primeiro modelo. Ocorreu apenas um acerto a mais de verdadeiro negativo e verdadeiro positivo


prevreglog[[4]]<-table(rg1.pred, test$Playlist)
prevreglog[[5]]<-table(rg2.pred, test$Playlist)
prevreglog[[6]]<-table(rg3.pred, test$Playlist)


###################################################################################################################################################################
##################################### Referência - Life Sucks
###################################################################################################################################################################
dbfac$Playlist <-relevel(dbfac$Playlist, ref="Alone Again")
dbfac$Playlist <-relevel(dbfac$Playlist, ref="Life Sucks")
set.seed(12)

amostra <- sample(c(TRUE, FALSE), nrow(dbfac), replace=TRUE, prob=c(0.7,0.3))
train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]


##################################### Modelo 1
###################################################################################################################################################################
## Todas as variáveis
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
modelrg1<- glm(Playlist ~ Acousticness + Danceability + Duration + Energy 
               + Instrumentalness + Key + Liveness + Loudness + Mode 
               + Speechiness + Tempo + Valence
               ,data=train, family='binomial')

summary(modelrg1)
xtable(summary(modelrg1), auto = F, caption = "Tabela de regressão do treino do modelo 1 para Life Sucks contra as playlists restantes.", label = "sucks mod 1 tabela reg log")

## AIC = 570,71

rg1.treino <- ifelse(modelrg1$fitted.values > 0.5, "Outras", "Life Sucks")

table(rg1.treino, train$Playlist)

xtable(table(rg1.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 1 para Life Sucks contra as playlists restantes.", label = "sucks mod 1 treino reg log")

##################################### Modelo 2
###################################################################################################################################################################
## Apenas variáveis significantes para o modelo
## Danceability, Energy, Instrumentalness, Loudness, Speechiness
modelrg2<- glm(Playlist ~ Danceability + Energy + 
               + Instrumentalness + Loudness
               + Speechiness
               ,data=train, family='binomial')

summary(modelrg2)
xtable(summary(modelrg2), auto = F, caption = "Tabela de regressão do treino do modelo 2 para Life Sucks contra as playlists restantes.", label = "sucks mod 2 tabela reg log")
## AIC = 562,9

rg2.treino <- ifelse(modelrg2$fitted.values > 0.5, "Outras", "Life Sucks")

table(rg2.treino, train$Playlist)

xtable(table(rg2.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 2 para Life Sucks contra as playlists restantes.", label = "sucks mod 2 treino reg log")

##################################### Modelo 3
###################################################################################################################################################################
## Apenas variávis significantes a alfa=0,05
## obs: 
## Energy, Instrumentalness
modelrg3<-stepAIC(modelrg1)
modelrg3<- glm(Playlist ~ Energy
               + Instrumentalness
               ,data=train, family='binomial')

summary(modelrg3)
xtable(summary(modelrg3), auto = F, caption = "Tabela de regressão do treino do modelo 3 para Life Sucks contra as playlists restantes.", label = "sucks mod 3 tabela reg log")
## STEPAIC = 562,26
## AIC = 568,91

rg3.treino <- ifelse(modelrg3$fitted.values > 0.5, "Outras", "Life Sucks")

table(rg3.treino, train$Playlist)

xtable(table(rg3.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 3 para Life Sucks contra as playlists restantes.", label = "sucks mod 3 treino reg log")

###################################################################################################################################################################
##################################### Regressão Logística - Calculando Previsões
###################################################################################################################################################################

# Predição do banco teste utilizando cada modelo

##################################### Modelo 1
###################################################################################################################################################################
rg1.prob <- predict(modelrg1, test, type = "response")
rg1.pred <- ifelse(rg1.prob > 0.5, "Outras", "Life Sucks")
rg1.pred1 <- ifelse(rg1.prob > 0.5, 1, 0)

##################################### Modelo 2
###################################################################################################################################################################
rg2.prob <- predict(modelrg2, test, type = "response")
rg2.pred <- ifelse(rg2.prob > 0.5, "Outras", "Life Sucks")
rg2.pred1 <- ifelse(rg2.prob > 0.5, 1, 0)

##################################### Modelo 3
###################################################################################################################################################################
rg3.prob <- predict(modelrg3, test, type = "response")
rg3.pred <- ifelse(rg3.prob > 0.5, "Outras", "Life Sucks")
rg3.pred1 <- ifelse(rg3.prob > 0.5, 1, 0)

###################################################################################################################################################################
##################################### Regressão Logística - Comparando Previsões
###################################################################################################################################################################

## Criando uma variável auxiliar para conseguir demonstrar os resultados de tabelas de confusão e curva ROC próximos
#################################################################################################################################
test<-mutate(test, Playlist1=Playlist)
train<-mutate(train, Playlist1=Playlist)

test$Playlist1 <-ifelse(test$Playlist=="Life Sucks", 0, 1)
train$Playlist1 <-ifelse(train$Playlist=="Life Sucks", 0, 1)
#################################################################################################################################

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

# Previsão do banco teste
table(rg1.pred, test$Playlist)
xtable(table(rg1.pred, test$Playlist), caption = "Matriz de confusão da previsão do modelo 1 para Life Sucks contra as playlists restantes.", label = "sucks mod 1 prev reg log")
table(rg1.pred1, test$Playlist1)
xtable(table(rg1.pred, test$Playlist1), caption = "Matriz de confusão da previsão do modelo 1 simplificada para Life Sucks contra as playlists restantes.", label = "sucks mod 1 prev simp reg log")

sucksrg<-list()
sucksrg1<-list()
sucksrg[[1]]<-table(rg1.pred, test$Playlist)
sucksrg1[[1]]<-table(rg1.pred, test$Playlist1)

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg1.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg1.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg1.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg1.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg1.prob)
# Curva ROC do modelo completo
plotROC(test$Playlist1, rg1.prob)


# Acerto da previsão
mean((rg1.pred1 ==test$Playlist1))
# Erro da previsão
1 - mean((rg1.pred1 ==test$Playlist1))

## Alone Again, Life Sucks e Spooning são as 3 playlists que pelos boxplots me pareciam ser as difíceis de classificar entre elas
## Acredito que isso é que esteja dificultando a classificação dessas 2 playlists
## O modelo se comporta mal para classificar as observações como Life Sucks, mas, curiosamente, se sai melhor do que o modelo
## de Alone Again para classificar as músicas como Alone Again

## Além disso, comentário muito parecidos com o primeiro modelo de Alone Again


##################################### Modelo 2
###################################################################################################################################################################
## Acousticness, Danceability, Key, Loudness, Speechiness, Tempo

# Previsão do banco teste
table(rg2.pred, test$Playlist)
xtable(table(rg2.pred, test$Playlist), caption = "Matriz de confusão da previsão do modelo 2 para Life Sucks contra as playlists restantes.", label = "sucks mod 2 prev reg log")
table(rg2.pred1, test$Playlist1)
xtable(table(rg2.pred, test$Playlist1), caption = "Matriz de confusão da previsão do modelo 2 simplificada para Life Sucks contra as playlists restantes.", label = "sucks mod 2 prev simp reg log")

sucksrg[[2]]<-table(rg2.pred, test$Playlist)
sucksrg1[[2]]<-table(rg2.pred, test$Playlist1)

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg2.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg2.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg2.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg2.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg2.prob)
# Curva ROC
plotROC(test$Playlist1, rg2.prob)

# Acerto da previsão
mean((rg2.pred1 ==test$Playlist1))
# Erro da previsão
1 - mean((rg2.pred1 ==test$Playlist1))

## Resultado muito parecido com o segundo modelo de Alone Again. Menos observações foram classificadas como Life Sucks.
## Piora na previsão do evento e melhora na ausência de interesse


##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Danceability, Key, Speechiness

# Previsão do banco teste
table(rg3.pred, test$Playlist)
xtable(table(rg3.pred, test$Playlist), caption = "Matriz de confusão da previsão do modelo 3 para Life Sucks contra as playlists restantes.", label = "sucks mod 3 prev reg log")
table(rg3.pred1, test$Playlist1)
xtable(table(rg3.pred, test$Playlist1), caption = "Matriz de confusão da previsão do modelo 3 simplificada para Life Sucks contra as playlists restantes.", label = "sucks mod 3 prev simp reg log")

sucksrg[[3]]<-table(rg3.pred, test$Playlist)
sucksrg1[[3]]<-table(rg3.pred, test$Playlist1)

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg3.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg3.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg3.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg3.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg3.prob)
# Curva ROC
plotROC(test$Playlist1, rg3.prob)


# Acerto da previsão
mean((rg3.pred1 ==test$Playlist1))
# Erro da previsão
1 - mean((rg3.pred1 ==test$Playlist1))

## Mais uma vez resultado similar ao modelo de Alone Again. Menos observações foram classificadas como Life Sucks, 
## mas dessa vez a precisão chegou a 50%.
## Talvez esse seja o melhor dos 3 modelos para esta playlist


prevreglog[[7]]<-table(rg1.pred, test$Playlist)
prevreglog[[8]]<-table(rg2.pred, test$Playlist)
prevreglog[[9]]<-table(rg3.pred, test$Playlist)


###################################################################################################################################################################
##################################### Referência - Piano Relaxante
###################################################################################################################################################################
dbfac$Playlist <-relevel(dbfac$Playlist, ref="Beast Mode")
amostra <- sample(c(TRUE, FALSE), nrow(dbfac), replace=TRUE, prob=c(0.7,0.3))
train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]

dbfac$Playlist <-relevel(dbfac$Playlist, ref="Alone Again")
amostra <- sample(c(TRUE, FALSE), nrow(dbfac), replace=TRUE, prob=c(0.7,0.3))
train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]

dbfac$Playlist <-relevel(dbfac$Playlist, ref="Piano Relaxante")
set.seed(12)

amostra <- sample(c(TRUE, FALSE), nrow(dbfac), replace=TRUE, prob=c(0.7,0.3))
train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]


##################################### Modelo 1
###################################################################################################################################################################
## Todas as variáveis
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
modelrg1<- glm(Playlist ~ Acousticness + Danceability + Duration + Energy 
               + Instrumentalness + Key + Liveness + Loudness + Mode 
               + Speechiness + Tempo + Valence
               ,data=train, family='binomial')
summary(modelrg1)
xtable(summary(modelrg1), auto = F, caption = "Tabela de regressão do treino do modelo 1 para Piano Relaxante contra as playlists restantes.", label = "piano mod 1 tabela reg log")
## AIC = 26

rg1.treino <- ifelse(modelrg1$fitted.values > 0.5, "Outras", "Piano Relaxante")

rg1.treino <- relevel(as.factor(rg1.treino), ref = "Piano Relaxante")

table(rg1.treino, train$Playlist)

xtable(table(rg1.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 1 para Piano Relaxante contra as playlists restantes.", label = "piano mod 1 treino reg log")

## Pela primeira vez o modelo não convergiu
## É necessária a remoção de Instrumentalness e Loudness para a convergência do modelo

##################################### Modelo 2
###################################################################################################################################################################
## Remoção de Instrumentalness e Loudness para que haja convergência no modelo
## Acousticness, Danceability, Duration, Energy, Key, Liveness, Mode, Speechiness, Tempo, Valence
modelrg2<- glm(Playlist ~ Acousticness + Danceability + Duration + Energy 
               + Key + Liveness + Mode 
               + Speechiness + Tempo + Valence
               ,data=train, family='binomial')
summary(modelrg2)
xtable(summary(modelrg2), auto = F, caption = "Tabela de regressão do treino do modelo 2 para Piano Relaxante contra as playlists restantes.", label = "piano mod 2 tabela reg log")
## AIC = 42,746

rg2.treino <- ifelse(modelrg2$fitted.values > 0.5, "Outras", "Piano Relaxante")

rg2.treino <- relevel(as.factor(rg2.treino), ref = "Piano Relaxante")

table(rg2.treino, train$Playlist)

xtable(table(rg2.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 2 para Piano Relaxante contra as playlists restantes.", label = "piano mod 2 treino reg log")

##################################### Modelo 3
###################################################################################################################################################################
## Apenas variávis significantes a alfa=0,05
## Acousticness, Duration, Energy
modelrg3<-stepAIC(modelrg2)
modelrg3<- glm(Playlist ~ Acousticness + Duration + Energy
               ,data=train, family='binomial')
## temos esses 2 modelos pra decidir. stepaic ou só significantes?
#modelrg3<- glm(Playlist ~ Acousticness + Danceability + Duration + Energy
#               + Key
#               ,data=train, family='binomial')

summary(modelrg3)
xtable(summary(modelrg3), auto = F, caption = "Tabela de regressão do treino do modelo 3 para Piano Relaxante contra as playlists restantes.", label = "piano mod 3 tabela reg log")
## AIC = 40,073

rg3.treino <- ifelse(modelrg3$fitted.values > 0.5, "Outras", "Piano Relaxante")

rg3.treino <- relevel(as.factor(rg3.treino), ref = "Piano Relaxante")

table(rg3.treino, train$Playlist)

xtable(table(rg3.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 3 para Piano Relaxante contra as playlists restantes.", label = "piano mod 3 treino reg log")


###################################################################################################################################################################
##################################### Regressão Logística - Calculando Previsões
###################################################################################################################################################################

# Predição do banco teste utilizando cada modelo

##################################### Modelo 1
###################################################################################################################################################################
rg1.prob <- predict(modelrg1, test, type = "response")
rg1.pred <- ifelse(rg1.prob > 0.5, "Outras", "Piano Relaxante")
rg1.pred1 <- ifelse(rg1.prob > 0.5, 1, 0)
rg1.pred<-relevel(as.factor(rg1.pred), ref="Piano Relaxante")

##################################### Modelo 2
###################################################################################################################################################################
rg2.prob <- predict(modelrg2, test, type = "response")
rg2.pred <- ifelse(rg2.prob > 0.5, "Outras", "Piano Relaxante")
rg2.pred1 <- ifelse(rg2.prob > 0.5, 1, 0)
rg2.pred<-relevel(as.factor(rg2.pred), ref="Piano Relaxante")

##################################### Modelo 3
###################################################################################################################################################################
rg3.prob <- predict(modelrg3, test, type = "response")
rg3.pred <- ifelse(rg3.prob > 0.5, "Outras", "Piano Relaxante")
rg3.pred1 <- ifelse(rg3.prob > 0.5, 1, 0)
rg3.pred<-relevel(as.factor(rg3.pred), ref="Piano Relaxante")

###################################################################################################################################################################
##################################### Regressão Logística - Comparando Previsões
###################################################################################################################################################################

## Criando uma variável auxiliar para conseguir demonstrar os resultados de tabelas de confusão e curva ROC próximos
#################################################################################################################################
test<-mutate(test, Playlist1=Playlist)
train<-mutate(train, Playlist1=Playlist)

test$Playlist1 <-ifelse(test$Playlist=="Piano Relaxante", 0, 1)
train$Playlist1 <-ifelse(train$Playlist=="Piano Relaxante", 0, 1)
#################################################################################################################################

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

# Previsão do banco teste
table(rg1.pred, test$Playlist)
xtable(table(rg1.pred, test$Playlist), caption = "Matriz de confusão da previsão do modelo 1 para Piano Relaxante contra as playlists restantes.", label = "piano mod 1 prev reg log")
table(rg1.pred1, test$Playlist1)
xtable(table(rg1.pred, test$Playlist1), caption = "Matriz de confusão da previsão do modelo 1 simplificada para Piano Relaxante contra as playlists restantes.", label = "piano mod 1 prev simp reg log")

pianorg<-list()
pianorg1<-list()
pianorg[[1]]<-table(rg1.pred, test$Playlist)
pianorg1[[1]]<-table(rg1.pred, test$Playlist1)

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg1.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg1.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg1.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg1.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg1.prob)
# Curva ROC do modelo completo
plotROC(test$Playlist1, rg1.prob)


# Acerto da previsão
mean((rg1.pred1 ==test$Playlist1))
# Erro da previsão
1 - mean((rg1.pred1 ==test$Playlist1))


## Assim como na comparação apenas entre Beast Mode e Piano Relaxante, 
## as observações de Piano Relaxante possuem dados muito diferente das outras playlists, 
## assim o modelo com todas as variáveis não converge.
## A previsão do modelo acaba sendo perfeita.

##################################### Modelo 2
###################################################################################################################################################################
## Acousticness, Danceability, Key, Loudness, Speechiness, Tempo

# Previsão do banco teste
table(rg2.pred, test$Playlist)
xtable(table(rg2.pred, test$Playlist), caption = "Matriz de confusão da previsão do modelo 2 para Life Sucks contra as playlists restantes.", label = "sucks mod 2 prev reg log")
table(rg2.pred1, test$Playlist1)
xtable(table(rg2.pred, test$Playlist1), caption = "Matriz de confusão da previsão do modelo 2 simplificada para Life Sucks contra as playlists restantes.", label = "sucks mod 2 prev simp reg log")

pianorg[[2]]<-table(rg2.pred, test$Playlist)
pianorg1[[2]]<-table(rg2.pred, test$Playlist1)

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg2.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg2.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg2.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg2.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg2.prob)
# Curva ROC
plotROC(test$Playlist1, rg2.prob)

# Acerto da previsão
mean((rg2.pred1 ==test$Playlist1))
# Erro da previsão
1 - mean((rg2.pred1 ==test$Playlist1))

## Mesmo após removermos 2 varíáveis do modelo para que ela convergisse, as observações dessa playlist ainda são muito diferente.
## O modelo performa muito bem, o melhor de todos até aqui, e acredito que será o melhor de todos na regressão logística.
## Apenas 2 erros de classificação.

##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Danceability, Key, Speechiness

# Previsão do banco teste
table(rg3.pred, test$Playlist)
xtable(table(rg3.pred, test$Playlist), caption = "Matriz de confusão da previsão do modelo 3 para Piano Relaxante contra as playlists restantes.", label = "piano mod 3 prev reg log")
table(rg3.pred1, test$Playlist1)
xtable(table(rg3.pred, test$Playlist1), caption = "Matriz de confusão da previsão do modelo 3 simplificada para Piano Relaxante contra as playlists restantes.", label = "piano mod 3 prev simp reg log")

pianorg[[3]]<-table(rg3.pred, test$Playlist)
pianorg1[[3]]<-table(rg3.pred, test$Playlist1)

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg3.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg3.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg3.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg3.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg3.prob)
# Curva ROC
plotROC(test$Playlist1, rg3.prob)


# Acerto da previsão
mean((rg3.pred1 ==test$Playlist1))
# Erro da previsão
1 - mean((rg3.pred1 ==test$Playlist1))

## Utilizando apenas 3 variáveis o modelo ainda performa muito bem com apenas 4 erros de classificação.


prevreglog[[10]]<-table(rg1.pred, test$Playlist)
prevreglog[[11]]<-table(rg2.pred, test$Playlist)
prevreglog[[12]]<-table(rg3.pred, test$Playlist)


###################################################################################################################################################################
##################################### Referência - Power Hour
###################################################################################################################################################################
dbfac$Playlist <-relevel(dbfac$Playlist, ref="Life Sucks")
amostra <- sample(c(TRUE, FALSE), nrow(dbfac), replace=TRUE, prob=c(0.7,0.3))
train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]

dbfac$Playlist <-relevel(dbfac$Playlist, ref="Beast Mode")
amostra <- sample(c(TRUE, FALSE), nrow(dbfac), replace=TRUE, prob=c(0.7,0.3))
train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]

dbfac$Playlist <-relevel(dbfac$Playlist, ref="Alone Again")
amostra <- sample(c(TRUE, FALSE), nrow(dbfac), replace=TRUE, prob=c(0.7,0.3))
train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]

dbfac$Playlist <-relevel(dbfac$Playlist, ref="Power Hour")
set.seed(12)

amostra <- sample(c(TRUE, FALSE), nrow(dbfac), replace=TRUE, prob=c(0.7,0.3))
train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]


##################################### Modelo 1
###################################################################################################################################################################
## Todas as variáveis
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
modelrg1<- glm(Playlist ~ Acousticness + Danceability + Duration + Energy 
               + Instrumentalness + Key + Liveness + Loudness + Mode 
               + Speechiness + Tempo + Valence
               ,data=train, family='binomial')

summary(modelrg1)
xtable(summary(modelrg1), auto = F, caption = "Tabela de regressão do treino do modelo 1 para Power Hour contra as playlists restantes.", label = "power mod 1 tabela reg log")
## AIC = 259,82

rg1.treino <- ifelse(modelrg1$fitted.values > 0.5, "Outras", "Power Hour")

rg1.treino <- relevel(as.factor(rg1.treino), ref = "Power Hour")

table(rg1.treino, train$Playlist)

xtable(table(rg1.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 1 para Power Hour contra as playlists restantes.", label = "power mod 1 treino reg log")

##################################### Modelo 2
###################################################################################################################################################################
## Apenas variáveis significantes para o modelo
## Acousticness, Energy, Key, Loudness, Mode
modelrg2<- glm(Playlist ~ Acousticness + Energy
               + Key + Loudness + Mode
               ,data=train, family='binomial')

summary(modelrg2)
xtable(summary(modelrg2), auto = F, caption = "Tabela de regressão do treino do modelo 2 para Power Hour contra as playlists restantes.", label = "power mod 2 tabela reg log")
## AIC = 249,49

rg2.treino <- ifelse(modelrg2$fitted.values > 0.5, "Outras", "Power Hour")

rg2.treino <- relevel(as.factor(rg2.treino), ref = "Power Hour")

table(rg2.treino, train$Playlist)

xtable(table(rg2.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 2 para Power Hour contra as playlists restantes.", label = "power mod 2 treino reg log")

##################################### Modelo 3
###################################################################################################################################################################
## Apenas variávis significantes a alfa=0,05
## obs: 
## Energy, Mode
modelrg3<- stepAIC(modelrg1)
modelrg3<- glm(Playlist ~ Energy
               + Mode
               ,data=train, family='binomial')

summary(modelrg3)
xtable(summary(modelrg3), auto = F, caption = "Tabela de regressão do treino do modelo 3 para Power Hour contra as playlists restantes.", label = "power mod 3 tabela reg log")

## stepAIC = 248,9
## AIC = 258,25

rg3.treino <- ifelse(modelrg3$fitted.values > 0.5, "Outras", "Power Hour")

rg3.treino <- relevel(as.factor(rg3.treino), ref = "Power Hour")

table(rg3.treino, train$Playlist)

xtable(table(rg3.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 3 para Power Hour contra as playlists restantes.", label = "power mod 3 treino reg log")

###################################################################################################################################################################
##################################### Regressão Logística - Calculando Previsões
###################################################################################################################################################################

# Predição do banco teste utilizando cada modelo

##################################### Modelo 1
###################################################################################################################################################################
rg1.prob <- predict(modelrg1, test, type = "response")
rg1.pred <- ifelse(rg1.prob > 0.5, "Outras", "Power Hour")
rg1.pred1 <- ifelse(rg1.prob > 0.5, 1, 0)
rg1.pred<-relevel(as.factor(rg1.pred), ref="Power Hour")

##################################### Modelo 2
###################################################################################################################################################################
rg2.prob <- predict(modelrg2, test, type = "response")
rg2.pred <- ifelse(rg2.prob > 0.5, "Outras", "Power Hour")
rg2.pred1 <- ifelse(rg2.prob > 0.5, 1, 0)
rg2.pred<-relevel(as.factor(rg2.pred), ref="Power Hour")

##################################### Modelo 3
###################################################################################################################################################################
rg3.prob <- predict(modelrg3, test, type = "response")
rg3.pred <- ifelse(rg3.prob > 0.5, "Outras", "Power Hour")
rg3.pred1 <- ifelse(rg3.prob > 0.5, 1, 0)
rg3.pred<-relevel(as.factor(rg3.pred), ref="Power Hour")

###################################################################################################################################################################
##################################### Regressão Logística - Comparando Previsões
###################################################################################################################################################################

## Criando uma variável auxiliar para conseguir demonstrar os resultados de tabelas de confusão e curva ROC próximos
#################################################################################################################################
test<-mutate(test, Playlist1=Playlist)
train<-mutate(train, Playlist1=Playlist)

test$Playlist1 <-ifelse(test$Playlist=="Power Hour", 0, 1)
train$Playlist1 <-ifelse(train$Playlist=="Power Hour", 0, 1)
#################################################################################################################################

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

# Previsão do banco teste
table(rg1.pred, test$Playlist)
xtable(table(rg1.pred, test$Playlist), caption = "Matriz de confusão da previsão do modelo 1 para Power Hour contra as playlists restantes.", label = "power mod 1 prev reg log")
table(rg1.pred1, test$Playlist1)
xtable(table(rg1.pred, test$Playlist1), caption = "Matriz de confusão da previsão do modelo 1 simplificada para Power Hour contra as playlists restantes.", label = "power mod 1 prev simp reg log")

powerrg<-list()
powerrg1<-list()
powerrg[[1]]<-table(rg1.pred, test$Playlist)
powerrg1[[1]]<-table(rg1.pred, test$Playlist1)

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg1.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg1.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg1.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg1.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg1.prob)
# Curva ROC do modelo completo
plotROC(test$Playlist1, rg1.prob)


# Acerto da previsão
mean((rg1.pred1 ==test$Playlist1))
# Erro da previsão
1 - mean((rg1.pred1 ==test$Playlist1))


## Assim como previsto pela análise dos boxplots, Power Hour se confunde com Beast Mode, 
## sendo a única playlist cujas observações são classificadas como Power Hour
## O modelo tem certo grau de qualidade para classificação. Não performa tão bem quanto
## quando a playlist de interesse era Beast Mode, mas ainda vai bem com taxa de verdadeiro positivo de 50%
## e precisão de 63%


##################################### Modelo 2
###################################################################################################################################################################
## Acousticness, Danceability, Key, Loudness, Speechiness, Tempo

# Previsão do banco teste
table(rg2.pred, test$Playlist)
xtable(table(rg2.pred, test$Playlist), caption = "Matriz de confusão da previsão do modelo 2 para Power Hour contra as playlists restantes.", label = "power mod 2 prev reg log")
table(rg2.pred1, test$Playlist1)
xtable(table(rg2.pred, test$Playlist1), caption = "Matriz de confusão da previsão do modelo 2 simplificada para Power Hour contra as playlists restantes.", label = "power mod 2 prev simp reg log")

powerrg[[2]]<-table(rg2.pred, test$Playlist)
powerrg1[[2]]<-table(rg2.pred, test$Playlist1)

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg2.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg2.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg2.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg2.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg2.prob)
# Curva ROC
plotROC(test$Playlist1, rg2.prob)

# Acerto da previsão
mean((rg2.pred1 ==test$Playlist1))
# Erro da previsão
1 - mean((rg2.pred1 ==test$Playlist1))

## Resultado muito parecido com o anterior, mas com uma pequena melhora na taxa de verdadeiro positivo, 
## agora 53% e uma leve piora na precisão, agora de 62,5%

##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Danceability, Key, Speechiness

# Previsão do banco teste
table(rg3.pred, test$Playlist)
xtable(table(rg3.pred, test$Playlist), caption = "Matriz de confusão da previsão do modelo 3 para Power Hour contra as playlists restantes.", label = "power mod 3 prev reg log")
table(rg3.pred1, test$Playlist1)
xtable(table(rg3.pred, test$Playlist1), caption = "Matriz de confusão da previsão do modelo 3 simplificada para Power Hour contra as playlists restantes.", label = "power mod 3 prev simp reg log")

powerrg[[3]]<-table(rg3.pred, test$Playlist)
powerrg1[[3]]<-table(rg3.pred, test$Playlist1)

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg3.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg3.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg3.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg3.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg3.prob)
# Curva ROC
plotROC(test$Playlist1, rg3.prob)


# Acerto da previsão
mean((rg3.pred1 ==test$Playlist1))
# Erro da previsão
1 - mean((rg3.pred1 ==test$Playlist1))

## Resultado pior que os dois anteriores. Menos observações classificadas, 
## melhora na precisão (52,17%) e piora na taxa de verdadeiros positivos (42,85%)


prevreglog[[13]]<-table(rg1.pred, test$Playlist)
prevreglog[[14]]<-table(rg2.pred, test$Playlist)
prevreglog[[15]]<-table(rg3.pred, test$Playlist)


###################################################################################################################################################################
##################################### Referência - Spooning
###################################################################################################################################################################
dbfac$Playlist <-relevel(dbfac$Playlist, ref="Piano Relaxante")
amostra <- sample(c(TRUE, FALSE), nrow(dbfac), replace=TRUE, prob=c(0.7,0.3))
train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]

dbfac$Playlist <-relevel(dbfac$Playlist, ref="Life Sucks")
amostra <- sample(c(TRUE, FALSE), nrow(dbfac), replace=TRUE, prob=c(0.7,0.3))
train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]

dbfac$Playlist <-relevel(dbfac$Playlist, ref="Beast Mode")
amostra <- sample(c(TRUE, FALSE), nrow(dbfac), replace=TRUE, prob=c(0.7,0.3))
train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]

dbfac$Playlist <-relevel(dbfac$Playlist, ref="Alone Again")
amostra <- sample(c(TRUE, FALSE), nrow(dbfac), replace=TRUE, prob=c(0.7,0.3))
train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]


dbfac$Playlist <-relevel(dbfac$Playlist, ref="Spooning")
set.seed(12)

amostra <- sample(c(TRUE, FALSE), nrow(dbfac), replace=TRUE, prob=c(0.7,0.3))
train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]


##################################### Modelo 1
###################################################################################################################################################################
## Todas as variáveis
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
modelrg1<- glm(Playlist ~ Acousticness + Danceability + Duration + Energy 
               + Instrumentalness + Key + Liveness + Loudness + Mode 
               + Speechiness + Tempo + Valence
               ,data=train, family='binomial')

summary(modelrg1)
xtable(summary(modelrg1), auto = F, caption = "Tabela de regressão do treino do modelo 1 para Spooning contra as playlists restantes.", label = "spoon mod 1 tabela reg log")
## AIC = 537,28

rg1.treino <- ifelse(modelrg1$fitted.values > 0.5, "Outras", "Spooning")

rg1.treino <- relevel(as.factor(rg1.treino), ref = "Spooning")

table(rg1.treino, train$Playlist)

xtable(table(rg1.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 1 para Spooning contra as playlists restantes.", label = "spoon mod 1 treino reg log")

##################################### Modelo 2
###################################################################################################################################################################
## Apenas variáveis significantes para o modelo
## Danceability, Duration, Instrumentalness, Liveness, Tempo
modelrg2<- glm(Playlist ~ Danceability + Duration +
               + Instrumentalness + Liveness
               + Tempo
               ,data=train, family='binomial')

summary(modelrg2)
xtable(summary(modelrg2), auto = F, caption = "Tabela de regressão do treino do modelo 2 para Spooning contra as playlists restantes.", label = "spoon mod 2 tabela reg log")
## AIC = 537,35

rg2.treino <- ifelse(modelrg2$fitted.values > 0.5, "Outras", "Spooning")

rg2.treino <- relevel(as.factor(rg2.treino), ref = "Spooning")

table(rg2.treino, train$Playlist)

xtable(table(rg2.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 2 para Spooning contra as playlists restantes.", label = "spoon mod 2 treino reg log")

##################################### Modelo 3
###################################################################################################################################################################
## Apenas variávis significantes a alfa=0,05
## Instrumentalness, Liveness, Tempo
modelrg3<- stepAIC(modelrg1)
modelrg3<- glm(Playlist ~ Instrumentalness + Liveness
               + Tempo
               ,data=train, family='binomial')

summary(modelrg3)
xtable(summary(modelrg3), auto = F, caption = "Tabela de regressão do treino do modelo 3 para Spooning contra as playlists restantes.", label = "spoon mod 3 tabela reg log")
## stepAIC = 527.8
## AIC = 539.93

rg3.treino <- ifelse(modelrg3$fitted.values > 0.5, "Outras", "Spooning")

rg3.treino <- relevel(as.factor(rg3.treino), ref = "Spooning")

table(rg3.treino, train$Playlist)

xtable(table(rg3.treino, train$Playlist), caption = "Matriz de confusão do treino do modelo 3 para Spooning contra as playlists restantes.", label = "spoon mod 3 treino reg log")

###################################################################################################################################################################
##################################### Regressão Logística - Calculando Previsões
###################################################################################################################################################################

# Predição do banco teste utilizando cada modelo

##################################### Modelo 1
###################################################################################################################################################################
rg1.prob <- predict(modelrg1, test, type = "response")
rg1.pred <- ifelse(rg1.prob > 0.5, "Outras", "Spooning")
rg1.pred1 <- ifelse(rg1.prob > 0.5, 1, 0)
rg1.pred<-relevel(as.factor(rg1.pred), ref="Spooning")

##################################### Modelo 2
###################################################################################################################################################################
rg2.prob <- predict(modelrg2, test, type = "response")
rg2.pred <- ifelse(rg2.prob > 0.5, "Outras", "Spooning")
rg2.pred1 <- ifelse(rg2.prob > 0.5, 1, 0)
rg2.pred<-relevel(as.factor(rg2.pred), ref="Spooning")

##################################### Modelo 3
###################################################################################################################################################################
rg3.prob <- predict(modelrg3, test, type = "response")
rg3.pred <- ifelse(rg3.prob > 0.5, "Outras", "Spooning")
rg3.pred1 <- ifelse(rg3.prob > 0.5, 1, 0)
rg3.pred<-relevel(as.factor(rg3.pred), ref="Spooning")

###################################################################################################################################################################
##################################### Regressão Logística - Comparando Previsões
###################################################################################################################################################################

## Criando uma variável auxiliar para conseguir demonstrar os resultados de tabelas de confusão e curva ROC próximos
#################################################################################################################################
test<-mutate(test, Playlist1=Playlist)
train<-mutate(train, Playlist1=Playlist)

test$Playlist1 <-ifelse(test$Playlist=="Spooning", 0, 1)
train$Playlist1 <-ifelse(train$Playlist=="Spooning", 0, 1)
#################################################################################################################################

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

# Previsão do banco teste
table(rg1.pred, test$Playlist)
xtable(table(rg1.pred, test$Playlist), caption = "Matriz de confusão da previsão do modelo 1 para Spooning contra as playlists restantes.", label = "spoon mod 1 prev reg log")
table(rg1.pred1, test$Playlist1)
xtable(table(rg1.pred, test$Playlist1), caption = "Matriz de confusão da previsão do modelo 1 simplificada para Spooning contra as playlists restantes.", label = "spoon mod 1 prev simp reg log")

spoonrg<-list()
spoonrg1<-list()
spoonrg[[1]]<-table(rg1.pred, test$Playlist)
spoonrg1[[1]]<-table(rg1.pred, test$Playlist1)

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg1.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg1.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg1.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg1.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg1.prob)
# Curva ROC do modelo completo
plotROC(test$Playlist1, rg1.prob)


# Acerto da previsão
mean((rg1.pred1 ==test$Playlist1))
# Erro da previsão
1 - mean((rg1.pred1 ==test$Playlist1))


## Pela primeira vez o modelo não classificou qualquer música como a playlist de interessante. 
## Apesar disso, o modelo teve um erro de apenas 11%, graças ao desbalanço da quantidade de observações entre os possíveis eventos do estudo


##################################### Modelo 2
###################################################################################################################################################################
## Acousticness, Danceability, Key, Loudness, Speechiness, Tempo

# Previsão do banco teste
table(rg2.pred, test$Playlist)
xtable(table(rg2.pred, test$Playlist), caption = "Matriz de confusão da previsão do modelo 2 para Spooning contra as playlists restantes.", label = "spoon mod 2 prev reg log")
table(rg2.pred1, test$Playlist1)
xtable(table(rg2.pred, test$Playlist1), caption = "Matriz de confusão da previsão do modelo 2 simplificada para Spooning contra as playlists restantes.", label = "spoon mod 2 prev simp reg log")

spoonrg[[2]]<-table(rg2.pred, test$Playlist)
spoonrg1[[2]]<-table(rg2.pred, test$Playlist1)

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg2.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg2.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg2.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg2.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg2.prob)
# Curva ROC
plotROC(test$Playlist1, rg2.prob)

# Acerto da previsão
mean((rg2.pred1 ==test$Playlist1))
# Erro da previsão
1 - mean((rg2.pred1 ==test$Playlist1))

## Todos os modelos tiveram o mesmo resultado

##################################### Modelo 3
###################################################################################################################################################################
## Acousticness, Danceability, Key, Speechiness

# Previsão do banco teste
table(rg3.pred, test$Playlist)
xtable(table(rg3.pred, test$Playlist), caption = "Matriz de confusão da previsão do modelo 3 para Spooning contra as playlists restantes.", label = "spoon mod 3 prev reg log")
table(rg3.pred1, test$Playlist1)
xtable(table(rg3.pred, test$Playlist1), caption = "Matriz de confusão da previsão do modelo 3 simplificada para Spooning contra as playlists restantes.", label = "spoon mod 3 prev simp reg log")

spoonrg[[3]]<-table(rg3.pred, test$Playlist)
spoonrg1[[3]]<-table(rg3.pred, test$Playlist1)

# Sensibilidade (taxa de verdadeiro positivo)
InformationValue::sensitivity(test$Playlist1, rg3.prob)
# Especificidade (taxa de verdadeiro negativo)
InformationValue::specificity(test$Playlist1, rg3.prob)
# Precisão (valor preditivo positivo)
InformationValue::precision(test$Playlist1, rg3.prob)
# Valor preditivo negativo
InformationValue::npv(test$Playlist1, rg3.prob)

# Percentual de Erro de classificação
misClassError(test$Playlist1, rg3.prob)
# Curva ROC
plotROC(test$Playlist1, rg3.prob)


# Acerto da previsão
mean((rg3.pred1 ==test$Playlist1))
# Erro da previsão
1 - mean((rg3.pred1 ==test$Playlist1))

## Todos os modelos tiveram o mesmo resultado


prevreglog[[16]]<-table(rg1.pred, test$Playlist)
prevreglog[[17]]<-table(rg2.pred, test$Playlist)
prevreglog[[18]]<-table(rg3.pred, test$Playlist)
prevreglog


###################################################################################################################################################################
##################################### Resetando os bancos de treino e teste
###################################################################################################################################################################
dbfac<-db1
dbfac$Playlist<-as.factor(dbfac$Playlist)

dbfac$Playlist <-relevel(dbfac$Playlist, ref="Spooning")
dbfac$Playlist <-relevel(dbfac$Playlist, ref="Power Hour")
dbfac$Playlist <-relevel(dbfac$Playlist, ref="Piano Relaxante")
dbfac$Playlist <-relevel(dbfac$Playlist, ref="Life Sucks")
dbfac$Playlist <-relevel(dbfac$Playlist, ref="Beast Mode")
dbfac$Playlist <-relevel(dbfac$Playlist, ref="Alone Again")

train <- dbfac[amostra, ]
test <- dbfac[!amostra, ]

###################################################################################################################################################################
##################################### Random Forest - Modelos
###################################################################################################################################################################


##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
set.seed(12)
modelrf1 <- randomForest(Playlist ~ Acousticness + Danceability + Duration + Energy
                         + Instrumentalness + Key + Liveness + Loudness + Mode 
                         + Speechiness + Tempo + Valence
                         ,data = train, importance = TRUE)
modelrf1

xtable(t(modelrf1$confusion), caption = "Matriz de confusão $random forest$ do treino do modelo 1", label = "todas mod 1 treino ran for")


par(mar = c(4,4,2,10), xpd=T)
plot(modelrf1, main = NA)
legend(x="right", inset = c(-0.62,0),
       legend= c("Média", "Alone Again", "Beast Mode", "Life Sucks", "Piano Relaxante", "Power Hour", "Spooning"),
       lty = c(1,2,3,4,5,6,2),
       col = c(1,2,3,4,5,6,1),
       lwd = 2)

## Adicionar legendas pra mostrar quais playlists são que linhas. 
## A preta é a média, próxima de 0,4. Acredito que a vermelha é Alone Again, azul mais escuro é Life Sucks, 
## rosa é Power Hour, preto é Spooning, verde é Beast Mode e azul claro é Piano Relaxante

## Interessante que é basicamente a mesma ordem de erro da reg log

## Verificando as variáveis importantes para o modelo de Random Forest
importance(modelrf1)
par(mar = c(4,4,2,10), xpd=F)
inset = c(-0.58,0),

varImpPlot(modelrf1, main = NA)

## Aqui vemos como as variáveis têm importância diferente para cada playlist


###################################################################################################################################################################
##################################### Random Forest - Previsões
###################################################################################################################################################################


##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence

# Resultado do modelo com o banco de treino
modelrf1$confusion # matriz está transposta. À esquerda estão os valores reais, e em cima o valores de previsão do modelo

# Previsão do modelo no banco de teste
predValid1 <- predict(modelrf1, test, type = "class")
tabelarf<-table(test$Playlist, predValid1)
tabelarf
xtable(table(predValid1, test$Playlist), caption = "Matriz de confusão random forest da previsão do modelo 1", label = "todas mod 1 prev ran for")
# Acerto da previsão
mean((predValid1 ==test$Playlist))
# Erro da previsão
1 - mean((predValid1 ==test$Playlist))


## Importante falar da diferença entre os métodos. No método de regressão logística classificamos apenas 1 playlist contra todas as outras,
## então cada playlist possui seus modelos, suas variáveis significativas e suas características específicas.
## Aqui no modelo de random forest, a classificação será feita para todas as playlists ao mesmo tempo por um único modelo

## Alone Again teve o pior desempenho. Pouco acerto e o modelo parece estar mais refinado para acertar a classificação de Life Sucks
## Beast mode tem um desempenho bom e só tem confusão substancial com Power Hour
## Life Sucks tem um desempenho mediano para fraco, mas se confunde muito com Alone Again e em grau menor com Spooning
## Piano Relaxante tem desempenho perfeito graças a ser muito diferente das outras playlists
## Power Hour tem um desempenho mediano para fraco também pois se confunde muito com Beast Mode
## Spooning tem um desempenho mediano para fraco também. Se confunde muito com Life Sucks e em menor grau com Alone Again e Beast Mode

## Mais uma vez vemos que a maior dificuldade do método está em separar Beast Mode de Power Hour e Alone Again de Life Sucks e de Spooning
## Piano Relaxante se separa muito bem de todas as outras


##################################################################################################################################################################
##################################### Redes Neurais
###################################################################################################################################################################


##################################################################################################################################################################
##################################### Comparando resultados
###################################################################################################################################################################


tabelarg<-list()
tabelarg[1]<-prevreglog[2]
tabelarg[2]<-prevreglog[4]
tabelarg[3]<-prevreglog[7] # ou talvez 8
tabelarg[4]<-prevreglog[10]
tabelarg[5]<-prevreglog[14] # ou talvez 13
tabelarg[6]<-prevreglog[16]

tabelarg

t(tabelarf) # precise transpor a tabela para comparar com os resultados de regressão linear com maior facilidade


## Alone again tem um comportamente similar em ambos os métodos, se confundindo com Life Sucks e Spooning, porém o método de regressão linear faz menos classificações incorretas
## Beast mode também tem comportamente similar em ambos os métodos, se confunde com Power Hour. Assim como em Alone Again, o método de regressão linear faz menos classificações incorretas
## Life Sucks tem comportamenteo semelhante nos dois métodos porém em graus distintos. Em ambos a classificação se confunde com Alone Again e Spooning. Temos muito mais classificações pelo método de random forest. São mais classificações corretas e incorretas, entretanto, entretanto a precisão do método é a melhor das duas.
## Piano Relaxante possui previsão perfeita pelos 2 métodos.
## Power Hour também tem comportamente semelhante em ambos os métodos, se confunde com Beast Mode. Porém dessa vez é a regressão logística que faz mais classificações.
## Spooning é o caso mais discrepante da nossa análise. O método de regressão linear não faz sequer uma classificação. O método de randomforest mostra que novamente há confusão entre Alone Again, Life Sucks e Spooning

## Interessante notarmos um desempenho muito próximo, no geral, entre os 2 métodos. Entre as diferença o que mais salta aos olhos é a incapacidade de classificação de qualquer observação na playlist Spooning pelo método de regressão logística
## Em geral o método de random forest tende a "ousar" mais fazendo mais classificações que os modelos ideias para cada playlist na regressão logística. Apesar de não ser o melhor em todas as classificações, eu diria que pelo fato desse método conseguir fazer alguma classificação da playlist Spooning, é mais interessante trabalhar com ele do que com regressão logística binomial.
##

###################################################################################################################################################################
##################################### Validação Cruzada
###################################################################################################################################################################

## Junção de Playlists


###################################################################################################################################################################
##################################### K-Folds
###################################################################################################################################################################

## Junção de Playlists

## https://www.statology.org/k-fold-cross-validation-in-r/
## https://rforhr.com/kfold.html

#cfm <- function(data, lev = NULL, model = NULL) {
#  cm <- confusionMatrix(table(data$pred, data$obs))
#  print(cm)
#  cm$byClass
#}

## Criando a função kf que define o tipo de validação cruzada utilizada. Neste momento, estamos usando o método k-folds
set.seed(12)
kf <- trainControl(method="cv", number=10, savePredictions="all", classProbs=TRUE)
#kf <- trainControl(method="cv", number=10, savePredictions="all", classProbs=TRUE, summaryFunction = cfm) # mostra todas as iterações do kfolds
#kf <- trainControl(method="repeatedcv", number=10, savePredictions="all", classProbs=TRUE, repeats = 10, summaryFunction = cfm) # k-folds com repetição


###################################################################################################################################################################
##################################### Resetando bases e manipulando dados
###################################################################################################################################################################

## o método de validação cruzada, por algum motivo exige que os fatores tenham apenas 1 palavra

## usar a validação cruzada no banco completo ou só na amostra de treino 
## e fazer uma nova previsão do banco de teste?

dbfac1=dbfac
dbfac1$Playlist <-ifelse(dbfac$Playlist=="Alone Again", "Alone", "Outros")
dbfac1$Playlist <-as.factor(dbfac1$Playlist)
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Alone")


###################################################################################################################################################################
##################################### K-Folds - Regressão Logística - Alone Again
###################################################################################################################################################################


##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
set.seed(12)
kmodelrg1<- train(Playlist ~ Acousticness + Danceability + Duration + Energy
                  + Instrumentalness + Key + Liveness + Loudness + Mode 
                  + Speechiness + Tempo + Valence
                  ,data=dbfac1, method="glm", family='binomial', trControl=kf)

summary(kmodelrg1)
xtable(summary(kmodelrg1))
## AIC = 628,77

kmodelrg1$resampledCM
kfrg1<-kmodelrg1$resampledCM[,-5:-6]

rownames(kfrg1)<-c("Fold 1","Fold 2","Fold 3","Fold 4","Fold 5","Fold 6","Fold 7","Fold 8","Fold 9","Fold 10")
colnames(kfrg1)<-c("Acerto","Erro","Erro","Acerto")
kfrg1
xtable(kfrg1, caption = "Resultados dos 10-folds do modelo 1", label = "alone mod 1 kfolds reg log")

table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])
xtable(table(kmodelrg1$pred[,1], kmodelrg1$pred[,2]), caption = "Resultados dos 10-folds do modelo 1", label = "alone mod 1 kfolds reg log")

## Melhor previsão foi feita pelo segundo modelo de regressão linear
alonerg1[2]

table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[1]*0.2868 ## alone alone ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[2]*0.2868 ## previu como outro e na verdade é alone ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[3]*0.2868 ## previu como alone e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[4]*0.2868 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


## Ao comparar com os resultados das previsões anteriores é importante notar que o "recorte" é diferente de antes.
## Antes tínhamos o triplo de observações quando prevíamos no banco "test" (0,3 do banco total) via regressão logística
## E aqui estamos trabalhando com 10-folds, (10 recortes) de tamanho igual do banco de dados (0,1 do banco total)
## Ao fazermos um simples regra de três, vemos que o método de regressão se beneficiou pelo recorte da amostra utilizada
## e se saiu melhor que a previsão média. Diria que a previsão original não é muito representativa do poder do modelo.


kmodelrg1

kmodelrg1$results[,-1]
xtable(kmodelrg1$results[,-1])
1-kmodelrg1$results[2]
## Accuracy de 86% de acerto na previsão média de todas as instâncias
## Kappa acima de 17%. Entre 1% e 20% - muito leve

## Interpretação de Kappa
##       100% Kappa perfeita distinção
##  99% - 81% Kappa quase perfeita
##  80% - 61% Kappa substancial
##  60% - 41% Kappa moderada
##  40% - 21% Kappa leve
##  20% -  1% Kappa muito leve
##         0% Kappa sem distinção


##################################### Modelo 2
###################################################################################################################################################################
## Apenas variáveis significantes para o modelo
## Danceability, Energy, Instrumentalness, Loudness, Mode, Speechiness, Tempo
set.seed(12)
kmodelrg2<- train(Playlist ~ Danceability + Energy + 
                 + Instrumentalness + Loudness + Mode
               + Speechiness + Tempo
               ,data=dbfac1, method="glm", family='binomial', trControl=kf)

summary(kmodelrg2)
xtable(summary(kmodelrg2))
## AIC = 623,26

kmodelrg2
kmodelrg2$resampledCM
kfrg2<-kmodelrg2$resampledCM[,-5:-6]

rownames(kfrg2)<-c("Fold 1","Fold 2","Fold 3","Fold 4","Fold 5","Fold 6","Fold 7","Fold 8","Fold 9","Fold 10")
colnames(kfrg2)<-c("Acerto","Erro","Erro","Acerto")
kfrg2
xtable(kfrg2, caption = "Resultados dos 10-folds do modelo 2", label = "alone mod 2 kfolds reg log")

table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])
xtable(table(kmodelrg2$pred[,1], kmodelrg2$pred[,2]), caption = "Resultados dos 10-folds do modelo 2", label = "alone mod 2 kfolds reg log")


table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


kmodelrg2$results[,-1]
xtable(kmodelrg2$results[,-1])
1-kmodelrg2$results[2]
## Accuracy de % de acerto na previsão média de todas as instâncias
## Kappa de %%. Entre % e % - 
## Erro de %


##################################### Modelo 3
###################################################################################################################################################################
## Apenas variávis significantes a alfa=0,05
## Danceability, Energy, Instrumentalness, Mode, Tempo
set.seed(12)
kmodelrg3<- stepAIC(kmodelrg1)
kmodelrg3<- train(Playlist ~ Danceability + Energy + 
                 + Instrumentalness + Loudness + Mode
                 + Speechiness + Tempo
                 ,data=dbfac1, method="glm", family='binomial', trControl=kf)


summary(kmodelrg3)
xtable(summary(kmodelrg3))
## AIC = 623,26

kmodelrg3$resampledCM
kfrg3<-kmodelrg3$resampledCM[,-5:-6]

rownames(kfrg3)<-c("Fold 1","Fold 2","Fold 3","Fold 4","Fold 5","Fold 6","Fold 7","Fold 8","Fold 9","Fold 10")
colnames(kfrg3)<-c("Acerto","Erro","Erro","Acerto")
kfrg3
xtable(kfrg3, caption = "Resultados dos 10-folds do modelo 3", label = "alone mod 3 kfolds reg log")

table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])
xtable(table(kmodelrg3$pred[,1], kmodelrg3$pred[,2]), caption = "Resultados dos 10-folds do modelo 3", label = "alone mod 3 kfolds reg log")


table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])[1]*158/547 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])[2]*158/547 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])[3]*158/547 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])[4]*158/547 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


kmodelrg3$results[-1]
xtable(kmodelrg3$results[,-1])
1-kmodelrg3$results[2]
## Accuracy de % de acerto na previsão média de todas as instâncias
## Kappa de %%. Entre % e % - 
## Erro de %

###################################################################################################################################################################
##################################### Resetando bases e manipulando dados
###################################################################################################################################################################

## o método de validação cruzada, por algum motivo exige que os fatores tenham apenas 1 palavra

dbfac<-db1

dbfac1$Playlist <-ifelse(dbfac$Playlist=="Beast Mode", "Beast", "Outros")
dbfac1$Playlist <-as.factor(dbfac1$Playlist)
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Beast")


###################################################################################################################################################################
##################################### K-Folds - Regressão Logística - Beast Mode
###################################################################################################################################################################

## https://www.statology.org/k-fold-cross-validation-in-r/
## https://rforhr.com/kfold.html

## Criando a função kf que define o tipo de validação cruzada utilizada. Neste momento, estamos usando o método k-folds
## Vamos testar a diferença entre repetições no k-folds e sem repetições.
kf <- trainControl(method="cv", number=10, savePredictions="all", classProbs=TRUE)
#kf <- trainControl(method="repeatedcv", number=10, savePredictions="all", classProbs=TRUE, repeats = 10)

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
set.seed(12)
kmodelrg1<- train(Playlist ~ Acousticness + Danceability + Duration + Energy
                  + Instrumentalness + Key + Liveness + Loudness + Mode
                  + Speechiness + Tempo + Valence
                  ,data=dbfac1, method="glm", family='binomial', trControl=kf)

summary(kmodelrg1)

## AIC = 474,59


kmodelrg1$resampledCM


table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])
xtable(table(kmodelrg1$pred[,1], kmodelrg1$pred[,2]), caption = "Resultados dos 10-folds do modelo 1", label = "beast 1 mod 1 kfolds reg log")

## Melhor previsão de regressão linear foi feita pelo primeiro modelo
beastrg1[1]

table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[1]*0.2868 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[2]*0.2868 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[3]*0.2868 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[4]*0.2868 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10

## Ao comparar os resultados, existe uma pequena diferença entre a média do k-folds e a melhor previsão da regressão logística
## A previsão realizada sem validação cruzada se beneficiou pouco da amostra utilizada. Diria que é bem representativa do poder do modelo

kmodelrg1$results[,-1]
xtable(kmodelrg1$results[,-1])
1-kmodelrg1$results[2]
## Accuracy de 89% de acerto na previsão média de todas as instâncias
## Kappa de 63%. Entre 61% e 80% - substancial


##################################### Modelo 2
###################################################################################################################################################################
## Apenas variáveis significantes para o modelo
## Acousticness, Danceability, Key, Loudness, Speechiness, Tempo
set.seed(12)
kmodelrg2<- train(Playlist ~ Acousticness + Danceability
               + Key + Loudness
               + Speechiness + Tempo
               ,data=dbfac1, method="glm", family='binomial', trControl=kf)
summary(kmodelrg2)

## AIC = 472,6

table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])
xtable(table(kmodelrg2$pred[,1], kmodelrg2$pred[,2]), caption = "Resultados dos 10-folds do modelo 2", label = "beast mod 2 kfolds reg log")

kmodelrg2$results[,-1]
xtable(kmodelrg2$results[,-1])
1-kmodelrg2$results[2]


##################################### Modelo 3
###################################################################################################################################################################
## Apenas variávis significativas a alfa=0,05
## Acousticness, Danceability, Speechiness
set.seed(12)
kmodelrg3<-stepAIC(kmodelrg1)
kmodelrg3<- train(Playlist ~ Acousticness + Danceability
                  + Key + Loudness
                  + Speechiness + Tempo
                  ,data=dbfac1, method="glm", family='binomial', trControl=kf)

summary(kmodelrg3)
## AIC = 472,6

table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])
xtable(table(kmodelrg3$pred[,1], kmodelrg3$pred[,2]), caption = "Resultados dos 10-folds do modelo 3", label = "par 1 kfolds mod 3 reg log")


kmodelrg3$results[-1]
xtable(kmodelrg3$results[,-1])
1-kmodelrg3$results[2]

###################################################################################################################################################################
##################################### Resetando bases e manipulando dados
###################################################################################################################################################################

## o método de validação cruzada, por algum motivo exige que os fatores tenham apenas 1 palavra

dbfac<-db1

dbfac1$Playlist <-ifelse(dbfac$Playlist=="Life Sucks", "Sucks", "Outros")
dbfac1$Playlist <-as.factor(dbfac1$Playlist)
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Sucks")


###################################################################################################################################################################
##################################### K-Folds - Regressão Logística - Life Sucks
###################################################################################################################################################################

## https://www.statology.org/k-fold-cross-validation-in-r/
## https://rforhr.com/kfold.html

## Criando a função kf que define o tipo de validação cruzada utilizada. Neste momento, estamos usando o método k-folds
## Vamos testar a diferença entre repetições no k-folds e sem repetições.
kf <- trainControl(method="cv", number=10, savePredictions="all", classProbs=TRUE)
#kf <- trainControl(method="repeatedcv", number=10, savePredictions="all", classProbs=TRUE, repeats = 10)

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
set.seed(12)
kmodelrg1<- train(Playlist ~ Acousticness + Danceability + Duration + Energy
                  + Instrumentalness + Key + Liveness + Loudness + Mode
                  + Speechiness + Tempo + Valence
                  ,data=dbfac1, method="glm", family='binomial', trControl=kf)

summary(kmodelrg1)

## AIC = 808,86


kmodelrg1$resampledCM


table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])
xtable(table(kmodelrg1$pred[,1], kmodelrg1$pred[,2]), caption = "Resultados dos 10-folds do modelo 1", label = "sucks mod 1 kfolds reg log")


## Melhor previsão de regressão linear foi feita pelo primeiro modelo
#sucksrg1[1]# classifica mais obs corretamente mas tem a precisão mais baixa
sucksrg1[2]# acho que é o melhor pela precisão


table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[1]*0.2868 ## sucks sucks ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[2]*0.2868 ## previu como outro e na verdade é sucks ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[3]*0.2868 ## previu como sucks e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[4]*0.2868 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10

## Ao comparar os resultados, existe uma pequena diferença entre a média do k-folds e a melhor previsão da regressão logística
## A previsão realizada com validação cruzada foi um pouco melhor. Mesmo assim eu diria que a regressão sem validação cruzada é representativa do poder do modelo


kmodelrg1
kmodelrg1$results[,-1]
xtable(kmodelrg1$results[,-1])
1-kmodelrg1$results[2]

## Accuracy de 80,4% de acerto na previsão média de todas as instâncias
## Kappa de 11,7%. Entre 1% e 20% - muito leve


##################################### Modelo 2
###################################################################################################################################################################
## Apenas variáveis significantes para o modelo
## Danceability, Energy, Instrumentalness, Loudness, Speechiness
set.seed(12)
kmodelrg2<- train(Playlist ~ Danceability + Energy + 
                 + Instrumentalness + Loudness
               + Speechiness
               ,data=dbfac1, method="glm", family='binomial', trControl=kf)
summary(kmodelrg2)

## AIC = 798,98

table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])
xtable(table(kmodelrg2$pred[,1], kmodelrg2$pred[,2]), caption = "Resultados dos 10-folds do modelo 2", label = "sucks mod 2 kfolds reg log")

kmodelrg2$results[,-1]
xtable(kmodelrg2$results[,-1])
1-kmodelrg2$results[2]


##################################### Modelo 3
###################################################################################################################################################################
## Apenas variávis significantes a alfa=0,05
## obs: 
## Energy, Instrumentalness
set.seed(12)
kmodelrg3<-stepAIC(kmodelrg1)
set.seed(12)
kmodelrg3<- train(Playlist ~ Acousticness + Danceability + Energy + 
                    + Instrumentalness + Loudness
                  + Speechiness
                  ,data=dbfac1, method="glm", family='binomial', trControl=kf)
summary(kmodelrg3)

## AIC = 799,01

table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])
xtable(table(kmodelrg3$pred[,1], kmodelrg3$pred[,2]), caption = "Resultados dos 10-folds do modelo 3", label = "par 1 kfolds mod 3 reg log")

kmodelrg3$results[-1]
xtable(kmodelrg3$results[,-1])
1-kmodelrg3$results[2]


###################################################################################################################################################################
##################################### Resetando bases e manipulando dados
###################################################################################################################################################################

## o método de validação cruzada, por algum motivo exige que os fatores tenham apenas 1 palavra

dbfac<-db1

dbfac1$Playlist <-ifelse(dbfac$Playlist=="Piano Relaxante", "Piano", "Outros")
dbfac1$Playlist <-as.factor(dbfac1$Playlist)
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Piano")


###################################################################################################################################################################
##################################### K-Folds - Regressão Logística - Piano Relaxante
###################################################################################################################################################################

## https://www.statology.org/k-fold-cross-validation-in-r/
## https://rforhr.com/kfold.html

## Criando a função kf que define o tipo de validação cruzada utilizada. Neste momento, estamos usando o método k-folds
kf <- trainControl(method="cv", number=10, savePredictions="all", classProbs=TRUE)
#kf <- trainControl(method="repeatedcv", number=10, savePredictions="all", classProbs=TRUE, repeats = 10)

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
set.seed(12)
kmodelrg1<- train(Playlist ~ Acousticness + Danceability + Duration + Energy
                  + Instrumentalness + Key + Liveness + Loudness + Mode
                  + Speechiness + Tempo + Valence
                  ,data=dbfac1, method="glm", family='binomial', trControl=kf)

summary(kmodelrg1)

## AIC = 26


kmodelrg1$resampledCM


table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])
xtable(table(kmodelrg1$pred[,1], kmodelrg1$pred[,2]), caption = "Resultados dos 10-folds do modelo 1", label = "par 1 kfolds mod 1 reg log")

## Melhor previsão de regressão linear foi feita pelo primeiro modelo
pianorg1[1]

table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[1]*0.2868 ## piano piano ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[2]*0.2868 ## previu como outro e na verdade é piano ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[3]*0.2868 ## previu como piano e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[4]*0.2868 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10

## Tanto a regressão simples quanto a validação cruzada tiveram previsões perfeitas

kmodelrg1$results[,-1]
xtable(kmodelrg1$results[,-1])
1-kmodelrg1$results[2]
## Accuracy de 100% de acerto na previsão média de todas as instâncias
## Kappa de 100%. 100% - perfeito


##################################### Modelo 2
###################################################################################################################################################################
## Remoção de Instrumentalness e Loudness para que haja convergência no modelo
## Acousticness, Danceability, Duration, Energy, Key, Liveness, Mode, Speechiness, Tempo, Valence
set.seed(12)
kmodelrg2<- train(Playlist ~ Acousticness + Danceability + Duration + Energy 
               + Key + Liveness + Mode 
               + Speechiness + Tempo + Valence
               ,data=dbfac1, method="glm", family='binomial', trControl=kf)
summary(kmodelrg2)

## AIC = 62,082

table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])
xtable(table(kmodelrg2$pred[,1], kmodelrg2$pred[,2]), caption = "Resultados dos 10-folds do modelo 2", label = "piano mod 2 kfolds reg log")

kmodelrg2$results[,-1]
xtable(kmodelrg2$results[,-1])
1-kmodelrg2$results[2]

##################################### Modelo 3
###################################################################################################################################################################
## Apenas variávis significantes a alfa=0,05
## Acousticness, Duration, Energy
set.seed(12)
#kmodelrg3<-stepAIC(kmodelrg2)
kmodelrg3<- train(Playlist ~ Acousticness + Danceability + Duration + Energy
                  + Key
                  ,data=dbfac1, method="glm", family='binomial', trControl=kf)
summary(kmodelrg3)

## AIC = 55,017

#set.seed(12)
#kmodelrg3<- train(Playlist ~ Acousticness + Duration + Energy
#               ,data=dbfac1, method="glm", family='binomial', trControl=kf)

table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])
xtable(table(kmodelrg3$pred[,1], kmodelrg3$pred[,2]), caption = "Resultados dos 10-folds do modelo 3", label = "par 1 kfolds mod 3 reg log")

kmodelrg3$results[-1]
xtable(kmodelrg3$results[,-1])
1-kmodelrg3$results[2]


###################################################################################################################################################################
##################################### Resetando bases e manipulando dados
###################################################################################################################################################################

## o método de validação cruzada, por algum motivo exige que os fatores tenham apenas 1 palavra

dbfac<-db1

dbfac1$Playlist <-ifelse(dbfac$Playlist=="Power Hour", "Power", "Outros")
dbfac1$Playlist <-as.factor(dbfac1$Playlist)
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Power")


###################################################################################################################################################################
##################################### K-Folds - Regressão Logística -  Power Hour
###################################################################################################################################################################

## https://www.statology.org/k-fold-cross-validation-in-r/
## https://rforhr.com/kfold.html

## Criando a função kf que define o tipo de validação cruzada utilizada. Neste momento, estamos usando o método k-folds
kf <- trainControl(method="cv", number=10, savePredictions="all", classProbs=TRUE)
#kf <- trainControl(method="repeatedcv", number=10, savePredictions="all", classProbs=TRUE, repeats = 10)

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
set.seed(12)
kmodelrg1<- train(Playlist ~ Acousticness + Danceability + Duration + Energy
                  + Instrumentalness + Key + Liveness + Loudness + Mode
                  + Speechiness + Tempo + Valence
                  ,data=dbfac1, method="glm", family='binomial', trControl=kf)

summary(kmodelrg1)

## AIC = 357,87


kmodelrg1$resampledCM


table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])
xtable(table(kmodelrg1$pred[,1], kmodelrg1$pred[,2]), caption = "Resultados dos 10-folds do modelo 1", label = "par 1 kfolds mod 1 reg log")


## Melhor previsão de regressão linear foi feita pelo primeiro modelo
powerrg1[1] # melhor precisão
powerrg1[2]

table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[1]*0.2868 ## power power ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[2]*0.2868 ## previu como outro e na verdade é power ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[3]*0.2868 ## previu como power e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[4]*0.2868 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10

## Ao comparar os resultados, existe uma pequena diferença entre a média do k-folds e as melhores previsões da regressão logística
## A previsão realizada sem validação cruzada se beneficiou um pouco da amostra utilizada. Diria que é bem representativa do poder do modelo

kmodelrg1$results[,-1]
xtable(kmodelrg1$results[,-1])
1-kmodelrg1$results[2]
## Accuracy de 92% de acerto na previsão média de todas as instâncias
## Kappa de 46%. Entre 41% e 60% - moderada


##################################### Modelo 2
###################################################################################################################################################################
## Apenas variáveis significantes para o modelo
## Acousticness, Energy, Key, Loudness, Mode
set.seed(12)
kmodelrg2<- train(Playlist ~ Acousticness + Energy
               + Key + Loudness + Mode
               ,data=dbfac1, method="glm", family='binomial', trControl=kf)
summary(kmodelrg2)

## AIC = 347,8

table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])
xtable(table(kmodelrg2$pred[,1], kmodelrg2$pred[,2]), caption = "Resultados dos 10-folds do modelo 2", label = "par 1 kfolds mod 2 reg log")

kmodelrg2$results[,-1]
xtable(kmodelrg2$results[,-1])
1-kmodelrg2$results[2]


##################################### Modelo 3
###################################################################################################################################################################
## Apenas variávis significantes a alfa=0,05
## obs: 
## Energy, Mode
set.seed(12)
kmodelrg3<- stepAIC(kmodelrg1)
kmodelrg3<- train(Playlist ~ Acousticness + Energy
                  + Key + Loudness + Mode
                  + Valence
                  ,data=dbfac1, method="glm", family='binomial', trControl=kf)
summary(kmodelrg3)

## AIC = 348,27

#set.seed(12)
#kmodelrg3<- train(Playlist ~ Energy
#               + Mode
#               ,data=dbfac1, method="glm", family='binomial', trControl=kf)

table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])
xtable(table(kmodelrg3$pred[,1], kmodelrg3$pred[,2]), caption = "Resultados dos 10-folds do modelo 3", label = "par 1 kfolds mod 3 reg log")

kmodelrg3$results[-1]
xtable(kmodelrg3$results[,-1])
1-kmodelrg3$results[2]


###################################################################################################################################################################
##################################### Resetando bases e manipulando dados
###################################################################################################################################################################

## o método de validação cruzada, por algum motivo exige que os fatores tenham apenas 1 palavra

dbfac<-db1

dbfac1$Playlist <-ifelse(dbfac$Playlist=="Spooning", "Spoon", "Outros")
dbfac1$Playlist <-as.factor(dbfac1$Playlist)
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Spoon")


###################################################################################################################################################################
##################################### K-Folds - Regressão Logística - Spooning
###################################################################################################################################################################

## https://www.statology.org/k-fold-cross-validation-in-r/
## https://rforhr.com/kfold.html

## Criando a função kf que define o tipo de validação cruzada utilizada. Neste momento, estamos usando o método k-folds
## Vamos testar a diferença entre repetições no k-folds e sem repetições.
kf <- trainControl(method="cv", number=10, savePredictions="all", classProbs=TRUE)
#kf <- trainControl(method="repeatedcv", number=10, savePredictions="all", classProbs=TRUE, repeats = 10)

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
set.seed(12)
kmodelrg1<- train(Playlist ~ Acousticness + Danceability + Duration + Energy
                  + Instrumentalness + Key + Liveness + Loudness + Mode
                  + Speechiness + Tempo + Valence
                  ,data=dbfac1, method="glm", family='binomial', trControl=kf)

summary(kmodelrg1)

## AIC = 748,36


kmodelrg1$resampledCM


table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])
xtable(table(kmodelrg1$pred[,1], kmodelrg1$pred[,2]), caption = "Resultados dos 10-folds do modelo 1", label = "par 1 kfolds mod 1 reg log")

## Melhor previsão de regressão linear foi feita pelo primeiro modelo
spoonrg1[1]

table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[1]*0.2868 ## spoon spoon ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[2]*0.2868 ## previu como outro e na verdade é spoon ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[3]*0.2868 ## previu como spoon e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrg1$pred[,1], kmodelrg1$pred[,2])[4]*0.2868 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10

## Ao comparar os resultados, também existe uma pequena diferença entre a média do k-folds e a melhor previsão da regressão logística
## Porém, curiosamente, o modelo de regressão não consegui classicar uma observação sequer na playlist de interesse.
## Dessa maneira o modelo utilizando k-folds se torna muito superior por conseguir classificar mais de 0 observações corretamente

kmodelrg1$results[,-1]
xtable(kmodelrg1$results[,-1])
1-kmodelrg1$results[2]
## Accuracy de 86% de acerto na previsão média de todas as instâncias
## Kappa de 2%. Entre 1% e 20% - muito leve

##################################### Modelo 2
###################################################################################################################################################################
## Apenas variáveis significantes para o modelo
## Danceability, Duration, Instrumentalness, Liveness, Tempo
set.seed(12)
kmodelrg2<- train(Playlist ~ Danceability + Duration +
                 + Instrumentalness + Liveness
               + Tempo
               ,data=dbfac1, method="glm", family='binomial', trControl=kf)
summary(kmodelrg2)

## AIC = 751,01

table(kmodelrg2$pred[,1], kmodelrg2$pred[,2])
xtable(table(kmodelrg2$pred[,1], kmodelrg2$pred[,2]), caption = "Resultados dos 10-folds do modelo 2", label = "par 1 kfolds mod 2 reg log")

kmodelrg2$results[,-1]
xtable(kmodelrg2$results[,-1])
1-kmodelrg2$results[2]


##################################### Modelo 3
###################################################################################################################################################################
## Apenas variávis significantes a alfa=0,05
## Instrumentalness, Liveness, Tempo
set.seed(12)
kmodelrg3<- stepAIC(kmodelrg1)
kmodelrg3<- train(Playlist ~ Danceability + Duration + Energy
                  + Instrumentalness + Liveness
                  + Tempo
                  ,data=dbfac1, method="glm", family='binomial', trControl=kf)
summary(kmodelrg3)

## AIC = 739,84

#set.seed(12)
#kmodelrg3<- train(Playlist ~ Instrumentalness + Liveness
#               + Tempo
#               ,data=dbfac1, method="glm", family='binomial', trControl=kf)

table(kmodelrg3$pred[,1], kmodelrg3$pred[,2])
xtable(table(kmodelrg3$pred[,1], kmodelrg3$pred[,2]), caption = "Resultados dos 10-folds do modelo 3", label = "par 1 kfolds mod 3 reg log")

kmodelrg3$results[-1]
xtable(kmodelrg3$results[,-1])
1-kmodelrg3$results[2]


###################################################################################################################################################################
##################################### Resetando bases e manipulando dados
###################################################################################################################################################################

## o método de validação cruzada, por algum motivo exige que os fatores tenham apenas 1 palavra

dbfac<-db1
dbfac1<-dbfac

dbfac1$Playlist<-(ifelse(dbfac1$Playlist=="Alone Again", "Alone", 
       ifelse(dbfac1$Playlist=="Beast Mode", "Beast", 
              ifelse(dbfac1$Playlist=="Life Sucks", "Sucks",
                     ifelse(dbfac1$Playlist=="Piano Relaxante", "Piano", 
                            ifelse(dbfac1$Playlist=="Power Hour", "Power","Spoon")
                     )
              )
       )
))

table(dbfac$Playlist, dbfac1$Playlist)

dbfac1$Playlist <-as.factor(dbfac1$Playlist)

dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Spoon")
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Power")
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Piano")
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Sucks")
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Beast")
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Alone")


###################################################################################################################################################################
##################################### K-Folds - Random Forest
###################################################################################################################################################################

## https://rdrr.io/github/marccamb/optiranger/man/rf.kfold.html

## https://rpubs.com/jvaldeleon/forest_repeat_cv

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
set.seed(12)
kmodelrf1<- train(Playlist ~ Acousticness + Danceability + Duration
                  + Energy + Instrumentalness + Key + Liveness + Loudness + Mode
                  + Speechiness + Tempo + Valence
                  ,data=dbfac1, method="rf", family='binomial', trControl=kf)

kmodelrf1$finalModel

xtable(t(kmodelrf1$finalModel$confusion))

varImp(kmodelrf1)
plot(varImp(kmodelrf1), main = NA)

plot((kmodelrf1$finalModel), main = NA)
legend(x="top",
       legend= c("Média", "Beast Mode", "Piano Relaxante"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

kmodelrf1$resampledCM

table(kmodelrf1$pred[,1], kmodelrf1$pred[,2])
xtable(table(kmodelrf1$pred[,1], kmodelrf1$pred[,2]), caption = "Resultados dos 10-folds do modelo 1", label = "par 1 kfolds mod 1 ran for")

tabelarf
kmodelrf1mtry2<-list()
kmodelrf1mtry2<-kmodelrf1$pred[1:1147, ]
kmodelrf1mtry2


## Melhor previsão de regressão linear foi feita pelo primeiro modelo

table(kmodelrf1mtry2[,1], kmodelrf1mtry2[,2])[1]*0.2868 ## spoon spoon ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf1mtry2[,1], kmodelrf1mtry2[,2])[8]*0.2868 ## previu como outro e na verdade é spoon ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf1mtry2[,1], kmodelrf1mtry2[,2])[15]*0.2868 ## previu como spoon e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf1mtry2[,1], kmodelrf1mtry2[,2])[22]*0.2868 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf1mtry2[,1], kmodelrf1mtry2[,2])[29]*0.2868 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10
table(kmodelrf1mtry2[,1], kmodelrf1mtry2[,2])[36]*0.2868 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10


## Ao comparar os resultados, também existe uma pequena diferença entre a média do k-folds e a melhor previsão da regressão logística
## Porém, curiosamente, o modelo de regressão não consegui classicar uma observação sequer na playlist de interesse.
## Dessa maneira o modelo utilizando k-folds se torna muito superior por conseguir classificar mais de 0 observações corretamente

kmodelrf1$results
xtable(kmodelrf1$results)
1-kmodelrf1$results[2]
## Accuracy de 61,99% de acerto na previsão média de todas as instâncias
## Kappa de 52%. Entre 41% e 60% - moderada


###################################################################################################################################################################
##################################### K-Folds - Redes Neurais
###################################################################################################################################################################



###################################################################################################################################################################
##################################### Leave One Out
###################################################################################################################################################################

## Spooning vs Power Hour

## LOO manual (possivelmente terei de usar isso nas redes neurais)
#First initialize the output vector as an empty object outside the loop.
#fitted_value <- NULL
#for(i in 1:53){
#  #you did this part right
#  validation<-ironslag[i,]
#  training<-ironslag[-i,]
#  model1<-lm(magnetic ~ chemical, data = training)
#  #when you fit the model, use the newdata argument to predict on a new row
#  #also, fitted_value needs the index [i], so the each loop doesn't overwrite the previous
#  fitted_value[i] <- predict(model1, newdata = validation)}
#
## https://www.geeksforgeeks.org/loocvleave-one-out-cross-validation-in-r-programming/#:~:text=LOOCV(Leave%20One%20Out%20Cross%2DValidation)%20is%20a%20type,considered%20as%20the%20training%20set.
## https://www.statology.org/leave-one-out-cross-validation-in-r/

loo <- trainControl(method="LOOCV", savePredictions= "all", classProbs = TRUE)
#loo <- trainControl(method="LOOCV")

###################################################################################################################################################################
##################################### Resetando bases e manipulando dados
###################################################################################################################################################################

## o método de validação cruzada, por algum motivo exige que os fatores tenham apenas 1 palavra

## usar a validação cruzada no banco completo ou só na amostra de treino 
## e fazer uma nova previsão do banco de teste?

dbfac1=dbfac
dbfac1$Playlist <-ifelse(dbfac$Playlist=="Alone Again", "Alone", "Outros")
dbfac1$Playlist <-as.factor(dbfac1$Playlist)
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Alone")


###################################################################################################################################################################
##################################### Leave One Out - Regressão Logística - Alone Again
###################################################################################################################################################################

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
set.seed(12)
loomodelrg1<- train(Playlist ~ Acousticness + Danceability + Duration + Energy
                    + Instrumentalness + Key + Liveness + Loudness + Mode
                    + Speechiness + Tempo + Valence
                    ,data=dbfac1, method="glm", family='binomial', trControl=loo)

summary(loomodelrg1)

## AIC = 628,77

table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])
xtable(table(loomodelrg1$pred[,1], loomodelrg1$pred[,2]))

## Melhor previsão de regressão linear foi feita pelo primeiro modelo
alonerg1[1]

table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[1]*0.2868 ## alone alone ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[2]*0.2868 ## previu como outro e na verdade é alone ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[3]*0.2868 ## previu como alone e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[4]*0.2868 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10

## Comparar com k-folds e reg log normal


loomodelrg1$results
xtable(loomodelrg1$results[,-1])
1-loomodelrg1$results[2]
## Accuracy de 86% de acerto na previsão média de todas as instâncias
## Kappa de 16%. Entre 1% e 20% - muito leve

## Interpretação de Kappa
##       100% Kappa perfeita distinção
##  99% - 81% Kappa quase perfeita
##  80% - 61% Kappa substancial
##  60% - 41% Kappa moderada
##  40% - 21% Kappa leve
##  20% -  1% Kappa muito leve
##         0% Kappa sem distinção


##################################### Modelo 2
###################################################################################################################################################################
## Apenas variáveis significantes para o modelo
## Danceability, Energy, Instrumentalness, Loudness, Mode, Speechiness, Tempo
set.seed(12)
loomodelrg2<- train(Playlist ~ Danceability + Energy + 
                    + Instrumentalness + Loudness + Mode
                    + Speechiness + Tempo
                    ,data=dbfac1, method="glm", family='binomial', trControl=loo)
summary(loomodelrg2)

## AIC = 623,26

table(loomodelrg2$pred[,1], loomodelrg2$pred[,2])
xtable(table(loomodelrg2$pred[,1], loomodelrg2$pred[,2]))

loomodelrg2$results
xtable(loomodelrg2$results[,-1])
1-loomodelrg2$results[2]


##################################### Modelo 3
###################################################################################################################################################################
## Apenas variávis significantes a alfa=0,05
## Danceability, Energy, Instrumentalness, Mode, Tempo
set.seed(12)
loomodelrg3<- stepAIC(loomodelrg1)
loomodelrg3<- train(Playlist ~ Danceability + Energy + 
                      + Instrumentalness + Loudness + Mode
                    + Speechiness + Tempo
                    ,data=dbfac1, method="glm", family='binomial', trControl=loo)
summary(loomodelrg3)

## AIC = 623,26
  
## igual ao modelo 2

table(loomodelrg3$pred[,1], loomodelrg3$pred[,2])
xtable(table(loomodelrg3$pred[,1], loomodelrg3$pred[,2]))

loomodelrg3$results[,-1]
xtable(loomodelrg3$results[,-1])
1-loomodelrg3$results[2]


###################################################################################################################################################################
##################################### Resetando bases e manipulando dados
###################################################################################################################################################################

## o método de validação cruzada, por algum motivo exige que os fatores tenham apenas 1 palavra

## usar a validação cruzada no banco completo ou só na amostra de treino 
## e fazer uma nova previsão do banco de teste?

dbfac1=dbfac
dbfac1$Playlist <-ifelse(dbfac$Playlist=="Beast Mode", "Beast", "Outros")
dbfac1$Playlist <-as.factor(dbfac1$Playlist)
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Beast")


###################################################################################################################################################################
##################################### Leave One Out - Regressão Logística - Beast Mode
###################################################################################################################################################################

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
set.seed(12)
loomodelrg1<- train(Playlist ~ Acousticness + Danceability + Duration + Energy
                    + Instrumentalness + Key + Liveness + Loudness + Mode
                    + Speechiness + Tempo + Valence
                    ,data=dbfac1, method="glm", family='binomial', trControl=loo)

summary(loomodelrg1)

## AIC = 474,59

table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])
xtable(table(loomodelrg1$pred[,1], loomodelrg1$pred[,2]))

## Melhor previsão de regressão linear foi feita pelo primeiro modelo
beastrg1[1]

table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[1]*0.2868 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[2]*0.2868 ## previu como outro e na verdade é beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[3]*0.2868 ## previu como beast e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[4]*0.2868 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10

## Comparar com k-folds e reg log normal


loomodelrg1$results
xtable(loomodelrg1$results[,-1])
1-loomodelrg1$results[2]
## Accuracy de 89% de acerto na previsão média de todas as instâncias
## Kappa de 62%. Entre 41% e 60% - moderada


##################################### Modelo 2
###################################################################################################################################################################
## Apenas variáveis significantes para o modelo
## Acousticness, Danceability, Key, Loudness, Speechiness, Tempo
set.seed(12)
loomodelrg2<- train(Playlist ~ Acousticness + Danceability
               + Key + Loudness
               + Speechiness + Tempo
               ,data=dbfac1, method="glm", family='binomial', trControl=loo)
summary(loomodelrg2)

## AIC = 472,6

table(loomodelrg2$pred[,1], loomodelrg2$pred[,2])
xtable(table(loomodelrg2$pred[,1], loomodelrg2$pred[,2]))

loomodelrg2$results
xtable(loomodelrg2$results[,-1])
1-loomodelrg2$results[2]


##################################### Modelo 3
###################################################################################################################################################################
## Apenas variávis significativas a alfa=0,05
## Acousticness, Danceability, Speechiness
set.seed(12)
loomodelrg3<-stepAIC(loomodelrg1)
loomodelrg3<- train(Playlist ~ Acousticness + Danceability
                    + Key + Loudness
                    + Speechiness + Tempo
                    ,data=dbfac1, method="glm", family='binomial', trControl=loo)
summary(loomodelrg3)

## AIC = 472,6

#set.seed(12)
#loomodelrg3<- train(Playlist ~ Acousticness + Danceability
#               + Speechiness
#               ,data=dbfac1, method="glm", family='binomial', trControl=loo)

table(loomodelrg3$pred[,1], loomodelrg3$pred[,2])
xtable(table(loomodelrg3$pred[,1], loomodelrg3$pred[,2]))

loomodelrg3$results[,-1]
xtable(loomodelrg3$results[,-1])
1-loomodelrg3$results[2]


###################################################################################################################################################################
##################################### Resetando bases e manipulando dados
###################################################################################################################################################################

## o método de validação cruzada, por algum motivo exige que os fatores tenham apenas 1 palavra

## usar a validação cruzada no banco completo ou só na amostra de treino 
## e fazer uma nova previsão do banco de teste?

dbfac1=dbfac
dbfac1$Playlist <-ifelse(dbfac$Playlist=="Life Sucks", "Sucks", "Outros")
dbfac1$Playlist <-as.factor(dbfac1$Playlist)
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Sucks")


###################################################################################################################################################################
##################################### Leave One Out - Regressão Logística - Life Sucks
###################################################################################################################################################################

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
set.seed(12)
loomodelrg1<- train(Playlist ~ Acousticness + Danceability + Duration + Energy
                    + Instrumentalness + Key + Liveness + Loudness + Mode
                    + Speechiness + Tempo + Valence
                    ,data=dbfac1, method="glm", family='binomial', trControl=loo)
summary(loomodelrg1)

## AIC = 808,86

table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])
xtable(table(loomodelrg1$pred[,1], loomodelrg1$pred[,2]))

## Melhor previsão de regressão linear foi feita pelo primeiro modelo
sucksrg1[2]

table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[1]*0.2868 ## sucks sucks ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[2]*0.2868 ## previu como outro e na verdade é sucks ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[3]*0.2868 ## previu como sucks e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[4]*0.2868 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10

## Comparar com k-folds e reg log normal


loomodelrg1$results
xtable(loomodelrg1$results[,-1])
1-loomodelrg1$results[2]
## Accuracy de 80% de acerto na previsão média de todas as instâncias
## Kappa de 12%. Entre 1% e 20% - muito leve


##################################### Modelo 2
###################################################################################################################################################################
## Apenas variáveis significantes para o modelo
## Danceability, Energy, Instrumentalness, Loudness, Speechiness
set.seed(12)
loomodelrg2<- train(Playlist ~ Danceability + Energy + 
                 + Instrumentalness + Loudness
               + Speechiness
               ,data=dbfac1, method="glm", family='binomial', trControl=loo)
summary(loomodelrg2)

## AIC = 798,98

table(loomodelrg2$pred[,1], loomodelrg2$pred[,2])
xtable(table(loomodelrg2$pred[,1], loomodelrg2$pred[,2]))

loomodelrg2$results
xtable(loomodelrg2$results[,-1])
1-loomodelrg2$results[2]


##################################### Modelo 3
###################################################################################################################################################################
## Apenas variávis significantes a alfa=0,05
## obs: 
## Energy, Instrumentalness
set.seed(12)
loomodelrg3<-stepAIC(loomodelrg1)
loomodelrg3<- train(Playlist ~ Acousticness + Danceability + Energy + 
                    + Instrumentalness + Loudness
                  + Speechiness
                  ,data=dbfac1, method="glm", family='binomial', trControl=loo)
summary(loomodelrg3)

## AIC = 799,01

#set.seed(12)
#loomodelrg3<- train(Playlist ~ Energy
#               + Instrumentalness
#               ,data=dbfac1, method="glm", family='binomial', trControl=loo)
table(loomodelrg3$pred[,1], loomodelrg3$pred[,2])
xtable(table(loomodelrg3$pred[,1], loomodelrg3$pred[,2]))

loomodelrg3$results[,-1]
xtable(loomodelrg3$results[,-1])
1-loomodelrg3$results[2]


###################################################################################################################################################################
##################################### Resetando bases e manipulando dados
###################################################################################################################################################################

## o método de validação cruzada, por algum motivo exige que os fatores tenham apenas 1 palavra

## usar a validação cruzada no banco completo ou só na amostra de treino 
## e fazer uma nova previsão do banco de teste?

dbfac1=dbfac
dbfac1$Playlist <-ifelse(dbfac$Playlist=="Piano Relaxante", "Piano", "Outros")
dbfac1$Playlist <-as.factor(dbfac1$Playlist)
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Piano")


###################################################################################################################################################################
##################################### Leave One Out - Regressão Logística - Piano Relaxante
###################################################################################################################################################################

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
set.seed(12)
loomodelrg1<- train(Playlist ~ Acousticness + Danceability + Duration + Energy
                    + Instrumentalness + Key + Liveness + Loudness + Mode
                    + Speechiness + Tempo + Valence
                    ,data=dbfac1, method="glm", family='binomial', trControl=loo)

summary(loomodelrg1)

## AIC = 26

table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])
xtable(table(loomodelrg1$pred[,1], loomodelrg1$pred[,2]))

## Melhor previsão de regressão linear foi feita pelo primeiro modelo
pianorg1[1]

table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[1]*0.2868 ## piano piano ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[2]*0.2868 ## previu como outro e na verdade é piano ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[3]*0.2868 ## previu como piano e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[4]*0.2868 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10

## Comparar com k-folds e reg log normal


loomodelrg1$results
xtable(loomodelrg1$results[,-1])
1-loomodelrg1$results[2]
## Accuracy de 100% de acerto na previsão média de todas as instâncias
## Kappa de 100%. Entre 100% - perfeita


##################################### Modelo 2
###################################################################################################################################################################
## Remoção de Instrumentalness e Loudness para que haja convergência no modelo
## Acousticness, Danceability, Duration, Energy, Key, Liveness, Mode, Speechiness, Tempo, Valence
set.seed(12)
loomodelrg2<- train(Playlist ~ Acousticness + Danceability + Duration + Energy 
               + Key + Liveness + Mode 
               + Speechiness + Tempo + Valence
               ,data=dbfac1, method="glm", family='binomial', trControl=loo)
summary(loomodelrg2)

## AIC = 62,082

table(loomodelrg2$pred[,1], loomodelrg2$pred[,2])
xtable(table(loomodelrg2$pred[,1], loomodelrg2$pred[,2]))

loomodelrg2$results
xtable(loomodelrg2$results[,-1])
1-loomodelrg2$results[2]


##################################### Modelo 3
###################################################################################################################################################################
## Apenas variávis significantes a alfa=0,05
## Acousticness, Duration, Energy
set.seed(12)
loomodelrg3<-stepAIC(loomodelrg2)
loomodelrg3<- train(Playlist ~ Acousticness + Danceability + Duration + Energy
                  + Key
                  ,data=dbfac1, method="glm", family='binomial', trControl=loo)
summary(loomodelrg3)

## AIC = 55,017

#set.seed(12)
#loomodelrg3<- train(Playlist ~ Acousticness + Duration + Energy
#               ,data=dbfac1, method="glm", family='binomial', trControl=loo)
table(loomodelrg3$pred[,1], loomodelrg3$pred[,2])
xtable(table(loomodelrg3$pred[,1], loomodelrg3$pred[,2]))

loomodelrg3$results[,-1]
xtable(loomodelrg3$results[,-1])
1-loomodelrg3$results[2]


###################################################################################################################################################################
##################################### Resetando bases e manipulando dados
###################################################################################################################################################################

## o método de validação cruzada, por algum motivo exige que os fatores tenham apenas 1 palavra

## usar a validação cruzada no banco completo ou só na amostra de treino 
## e fazer uma nova previsão do banco de teste?

dbfac1=dbfac
dbfac1$Playlist <-ifelse(dbfac$Playlist=="Power Hour", "Power", "Outros")
dbfac1$Playlist <-as.factor(dbfac1$Playlist)
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Power")


###################################################################################################################################################################
##################################### Leave One Out - Regressão Logística - Power Hour
###################################################################################################################################################################

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
set.seed(12)
loomodelrg1<- train(Playlist ~ Acousticness + Danceability + Duration + Energy
                    + Instrumentalness + Key + Liveness + Loudness + Mode
                    + Speechiness + Tempo + Valence
                    ,data=dbfac1, method="glm", family='binomial', trControl=loo)

summary(loomodelrg1)

## AIC = 357,87

table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])
xtable(table(loomodelrg1$pred[,1], loomodelrg1$pred[,2]))

## Melhor previsão de regressão linear foi feita pelo primeiro modelo
powerrg1[2]

table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[1]*0.2868 ## power power ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[2]*0.2868 ## previu como outro e na verdade é power ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[3]*0.2868 ## previu como power e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[4]*0.2868 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10

## Comparar com k-folds e reg log normal


loomodelrg1$results
xtable(loomodelrg1$results[,-1])
1-loomodelrg1$results[2]
## Accuracy de 92% de acerto na previsão média de todas as instâncias
## Kappa de 47%. Entre 41% e 60% - moderada


##################################### Modelo 2
###################################################################################################################################################################
## Apenas variáveis significantes para o modelo
## Acousticness, Energy, Key, Loudness, Mode
set.seed(12)
loomodelrg2<- train(Playlist ~ Acousticness + Energy
               + Key + Loudness + Mode
               ,data=dbfac1, method="glm", family='binomial', trControl=loo)
summary(loomodelrg2)

## AIC = 347,8

table(loomodelrg2$pred[,1], loomodelrg2$pred[,2])
xtable(table(loomodelrg2$pred[,1], loomodelrg2$pred[,2]))

loomodelrg2$results
xtable(loomodelrg2$results[,-1])
1-loomodelrg2$results[2]


##################################### Modelo 3
###################################################################################################################################################################
## Apenas variávis significantes a alfa=0,05
## obs: 
## Energy, Mode
set.seed(12)
loomodelrg3<- stepAIC(loomodelrg1)
loomodelrg3<- train(Playlist ~ Acousticness + Energy
                  + Key + Loudness + Mode
                  + Valence
                  ,data=dbfac1, method="glm", family='binomial', trControl=loo)
summary(loomodelrg3)

## AIC = 348,27

#set.seed(12)
#loomodelrg3<- train(Playlist ~ Energy
#               + Mode
#               ,data=dbfac1, method="glm", family='binomial', trControl=loo)
table(loomodelrg3$pred[,1], loomodelrg3$pred[,2])
xtable(table(loomodelrg3$pred[,1], loomodelrg3$pred[,2]))


loomodelrg3$results[,-1]
xtable(loomodelrg3$results[,-1])
1-loomodelrg3$results[2]

###################################################################################################################################################################
##################################### Resetando bases e manipulando dados
###################################################################################################################################################################

## o método de validação cruzada, por algum motivo exige que os fatores tenham apenas 1 palavra

## usar a validação cruzada no banco completo ou só na amostra de treino 
## e fazer uma nova previsão do banco de teste?

dbfac1=dbfac
dbfac1$Playlist <-ifelse(dbfac$Playlist=="Spooning", "Spoon", "Outros")
dbfac1$Playlist <-as.factor(dbfac1$Playlist)
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Spoon")


###################################################################################################################################################################
##################################### Leave One Out - Regressão Logística - Spooning
###################################################################################################################################################################

##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
set.seed(12)
loomodelrg1<- train(Playlist ~ Acousticness + Danceability + Duration + Energy
                    + Instrumentalness + Key + Liveness + Loudness + Mode
                    + Speechiness + Tempo + Valence
                    ,data=dbfac1, method="glm", family='binomial', trControl=loo)

summary(loomodelrg1)

## AIC = 748,36

table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])
xtable(table(loomodelrg1$pred[,1], loomodelrg1$pred[,2]))

## Melhor previsão de regressão linear foi feita pelo primeiro modelo
spoonrg1[1]

table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[1]*0.2868 ## spoon spoon ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[2]*0.2868 ## previu como outro e na verdade é spoon ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[3]*0.2868 ## previu como spoon e na verdade é outro ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrg1$pred[,1], loomodelrg1$pred[,2])[4]*0.2868 ## outro outro ## regra de 3 = vezes 2868 e dividido por 10

## Comparar com k-folds e reg log normal


loomodelrg1$results
xtable(loomodelrg1$results[,-1])
1-loomodelrg1$results[2]
## Accuracy de 86% de acerto na previsão média de todas as instâncias
## Kappa de 1,2%. Entre 1% e 20% - muito leve


##################################### Modelo 2
###################################################################################################################################################################
## Apenas variáveis significantes para o modelo
## Danceability, Duration, Instrumentalness, Liveness, Tempo
set.seed(12)
loomodelrg2<- train(Playlist ~ Danceability + Duration +
                 + Instrumentalness + Liveness
               + Tempo
               ,data=dbfac1, method="glm", family='binomial', trControl=loo)
summary(loomodelrg2)

## AIC = 751,01

table(loomodelrg2$pred[,1], loomodelrg2$pred[,2])
xtable(table(loomodelrg2$pred[,1], loomodelrg2$pred[,2]))

loomodelrg2$results
xtable(loomodelrg2$results[,-1])
1-loomodelrg2$results[2]


##################################### Modelo 3
###################################################################################################################################################################
## Apenas variávis significantes a alfa=0,05
## Instrumentalness, Liveness, Tempo
set.seed(12)
loomodelrg3<- stepAIC(loomodelrg1)
loomodelrg3<- train(Playlist ~ Danceability + Duration + Energy
                  + Instrumentalness + Liveness
                  + Tempo
                  ,data=dbfac1, method="glm", family='binomial', trControl=loo)
summary(loomodelrg3)

## AIC = 739,84

#set.seed(12)
#loomodelrg3<- train(Playlist ~ Instrumentalness + Liveness
#               + Tempo
#               ,data=dbfac1, method="glm", family='binomial', trControl=loo)
table(loomodelrg3$pred[,1], loomodelrg3$pred[,2])
xtable(table(loomodelrg3$pred[,1], loomodelrg3$pred[,2]))

loomodelrg3$results[,-1]
xtable(loomodelrg3$results[,-1])
1-loomodelrg3$results[2]


###################################################################################################################################################################
##################################### Leave One Out - Random Forest
###################################################################################################################################################################

## https://rdrr.io/github/marccamb/optiranger/man/rf.kfold.html

## https://rpubs.com/jvaldeleon/forest_repeat_cv

###################################################################################################################################################################
##################################### Resetando bases e manipulando dados
###################################################################################################################################################################

## o método de validação cruzada, por algum motivo exige que os fatores tenham apenas 1 palavra

dbfac<-db1
dbfac1<-dbfac

dbfac1$Playlist<-(ifelse(dbfac1$Playlist=="Alone Again", "Alone", 
                         ifelse(dbfac1$Playlist=="Beast Mode", "Beast", 
                                ifelse(dbfac1$Playlist=="Life Sucks", "Sucks",
                                       ifelse(dbfac1$Playlist=="Piano Relaxante", "Piano", 
                                              ifelse(dbfac1$Playlist=="Power Hour", "Power","Spoon")
                                       )
                                )
                         )
))


dbfac1$Playlist <-as.factor(dbfac1$Playlist)

dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Spoon")
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Power")
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Piano")
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Sucks")
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Beast")
dbfac1$Playlist <-relevel(dbfac1$Playlist, ref="Alone")


##################################### Modelo 1
###################################################################################################################################################################
## Acousticness, Danceability, Duration, Energy, Instrumentalness, Key, Liveness, Loudness, Mode, Speechiness, Tempo, Valence
set.seed(12)
loomodelrf1<- train(Playlist ~ Acousticness + Danceability + Duration + Energy
                    + Instrumentalness + Key + Liveness + Loudness + Mode
                    + Speechiness + Tempo + Valence
                    ,data=dbfac1, method="rf", family='binomial', trControl=loo)


## demorou uns 30 minutos pra rodar


loomodelrf1$finalModel

loomodelrf1

loomodelrf1$finalModel$confusion
xtable(loomodelrf1$finalModel$confusion)
varImp(loomodelrf1)

plot(varImp(loomodelrf1), main = NA)

plot(loomodelrf1$finalModel, main = NA)
legend(x="top",
       legend= c("Média", "Power Hour", "Spooning"),
       lty = c(1,2,3),
       col = c(1,2,3),
       lwd = 2)

tabelarf

ab<-loomodelrf1$pred
ab<-ab[order(ab$mtry),]
loomodelrf1mtry12<-list()
loomodelrf1mtry12<-ab[1148:2295,]


table(loomodelrf1mtry12[,1], loomodelrf1mtry12[,2])
table(loomodelrf1mtry12[,1], loomodelrf1mtry12[,2])[1]*0.2868 ## alone alone ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf1mtry12[,1], loomodelrf1mtry12[,2])[8]*0.2868 ## beast beast ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf1mtry12[,1], loomodelrf1mtry12[,2])[15]*0.2868 ## sucks sucks ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf1mtry12[,1], loomodelrf1mtry12[,2])[22]*0.2868 ## piano piano ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf1mtry12[,1], loomodelrf1mtry12[,2])[29]*0.2868 ## power power ## regra de 3 = vezes 2868 e dividido por 10
table(loomodelrf1mtry12[,1], loomodelrf1mtry12[,2])[36]*0.2868 ## spoon spoon ## regra de 3 = vezes 2868 e dividido por 10


## Ao comparar os resultados, também existe uma pequena diferença entre a média do k-folds e a melhor previsão da regressão logística
## Porém, curiosamente, o modelo de regressão não consegui classicar uma observação sequer na playlist de interesse.
## Dessa maneira o modelo utilizando k-folds se torna muito superior por conseguir classificar mais de 0 observações corretamente

loomodelrf1$results
xtable(loomodelrf1$results)
1-loomodelrf1$results[2]
## Accuracy de 61,98% de acerto na previsão média de todas as instâncias
## Kappa de 52,68%. Entre 41% e 60% - moderada

## testar com leave one out e ver se os métodos kfolds e leave one out
## concordam entre os modelos testados (ex: ambos concordam que model 1 é melhor)


###################################################################################################################################################################
##################################### Leave One Out - Redes Neurais
###################################################################################################################################################################


###########################################################################################################


## tratar dados de gênero aqui no R mesmo, plottar os gêneros todos e ver quem pode ser aglutinado
## 

## matriz de correlação entre as variáveis para testar multicolinearidade (não convergência)

## análise da importância das variáveis dentro das playlists. "O que é instrumentalness e por que ele é tão importante nessa playlist"

## Entender os resultados de kfolds (o que diabos é aquele sample??)
## após validações cruzadas, começar a classificação de 1 playlist dentro das 6 já usadas no projeto.

## Entender relevel dos fatores pra glm. Acho que o fator de referência é o mais perto de 0. Por exemplo, ref="Power Hour", > 0.5 "Spoon", "Power
## Sample em Kfolds e LOOCV é de 389 por causa da partição dos dados (0.7 * 547)

## 17/08
## Redes neurais pesquisar sobre camadas, quantidade, etc. Ler Rafael, ver vídeos aula
## justificar a escolha, caso arbitrário usar os exemplos de Rafael como justificativa de valores das camadas
##

## playlists de gÊneros bem definidos tipo samba, ou forró (bem marcados) vs progressivo ou jazz, avant-garde (bem aleatório)

## tipos de playlist (com base no usuario, com base nos ouvintes dos artistas e com base exclusivamente nas características musicais - nosso caso)
## começar a escrever os casos 1 vs 1 
## descrever resultados e anotar pontos que não estão tão elaborados mas só para anotar as ideias mesmo



