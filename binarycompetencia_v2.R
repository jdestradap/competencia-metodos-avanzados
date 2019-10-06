library(dplyr)
library(tidyr)
library(glmnet)
library(Metrics)
library(tibble)
library(reshape2)
library(BMA)
library(BMS)
library(pROC)

# Function to extract a prepare the data


#cargamos datos

gettingTheData <- function(filePath) {
  data <- na.omit(read.table(filePath, header = TRUE, sep = ","))
}

df <- gettingTheData("databinarystudents.csv")

# exploracion de datos con todas las variables -----------------------------------------

seleccion<-c(1:32)

hist(df[,2])

# analisis de correlaciones

M <- cor(df[,c(2,seleccion+2)])
col<- colorRampPalette(c("Blue"))
corrplot::corrplot(M, method = "color", order="hclust", col=RColorBrewer::brewer.pal(n=8, name="Blues"))

corrplot::corrplot.mixed(M, lower.col = "black", number.cex = .7)


cormat <- round(cor(df[,c(2,seleccion+2)]),2)
cormat[lower.tri(cormat,diag=TRUE)] <- NA
head(cormat)
melted_cormat <- melt(cormat)
melted_cormat <- melted_cormat %>% drop_na()
head(melted_cormat)
melted_cormat <- melted_cormat %>% mutate(signo=ifelse(value<0,'-','+'))
melted_cormat <- melted_cormat %>% mutate(value=abs(value))
melted_cormat <- melted_cormat[order(-melted_cormat[,3]),]
melted_cormat %>% filter(value>=0.4)

# Particion del dataset

ind_train <- sample(1:nrow(df), size = round(0.8*nrow(df)))

# 20 % validacion x y y sin estandarizar
validation <- df[-ind_train,]

# 80 % data original x y y sin estandarizar
sample <- df[ind_train,]

y.sample <- sample[,2]
x.sample <- sample[,seleccion+2]

# medias
x.sample.means <- apply(x.sample, 2, mean)

# varianzas
x.sample.variances <- apply(x.sample, 2, sd)

# scale 
x.sample.scale <- scale(x.sample)

# corremos modelos con todas las variables y con particion de train -----------------------------

auc.result<-matrix(0,6,1) 
roc.result<-matrix(0,6,1)

# Lasso - GLM 

cv.lasso <- cv.glmnet(x.sample.scale, y.sample, family = "binomial", nfold = 5, type.measure = "auc", parallel = TRUE, alpha = 1,standardize = FALSE)
lasso.model <- glmnet(x.sample.scale, y.sample, family = "binomial", lambda = cv.lasso$lambda.min, alpha = 1,standardize = FALSE)

plot(cv.lasso)

# Lasso - accuracy

y.predict.lasso <- as.numeric(predict(lasso.model, x.sample.scale, type = "class"))

auc.lasso <- accuracy(y.sample,y.predict.lasso)

auc.result[1]<-auc.lasso

roc.lasso <- roc(y.sample, y.predict.lasso, quiet=TRUE)

roc.result[1]<-roc.lasso$auc

# Ridge - GLM 

cv.ridge <- cv.glmnet(x.sample.scale, y.sample, family = "binomial", nfold = 5, type.measure = "auc", parallel = TRUE, alpha = 0,standardize = FALSE)
ridge.model <- glmnet(x.sample.scale, y.sample, family = "binomial", lambda = cv.ridge$lambda.min, alpha = 0,standardize = FALSE)

plot(cv.ridge)

# Ridge - Accuracy

y.predict.ridge <- as.numeric(predict(ridge.model, x.sample.scale, type = "class"))

auc.ridge <- accuracy(y.sample,y.predict.ridge)

auc.result[2]<-auc.ridge

roc.ridge <- roc(y.sample, y.predict.ridge, quiet=TRUE)

roc.result[2]<-roc.ridge$auc

# Elasticnet - GLM

a <- seq(0.1, 0.9, 0.05)
search <- foreach(i = a, .combine = rbind) %dopar% {
  cv <- cv.glmnet(x.sample.scale, y.sample, family = "binomial", nfold = 5, type.measure = "auc", parallel = TRUE, alpha = i,standardize = FALSE)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.min], lambda.min = cv$lambda.min, alpha = i)
}

cv.optimo <- search[search$cvm == min(search$cvm), ]

elasticnet.model <- glmnet(x.sample.scale, y.sample, family = "binomial", lambda = cv.optimo$lambda.min, alpha = cv.optimo$alpha, standardize = FALSE)

# Elastic - Accuracy

y.predict.elasticnet <- as.numeric(predict(elasticnet.model, x.sample.scale, type = "class"))

auc.elasticnet <- accuracy(y.sample, y.predict.elasticnet)

auc.result[3]<-auc.elasticnet

roc.elasticnet <- roc(y.sample, y.predict.elasticnet, quiet=TRUE)

roc.result[3]<-roc.elasticnet$auc

# BMA bic - GLM - En construcción

bicbma.model<-bic.glm(x.sample.scale, y.sample, glm.family = binomial(link = "logit"),maxCol=33,
                      strict = FALSE, OR = 20, OR.fix = 2, nbest = 10, dispersion = NULL, 
                      factor.type = TRUE, factor.prior.adjust = FALSE, 
                      occam.window = TRUE, call = NULL)

summary(bicbma.model)
imageplot.bma(bicbma.model)
plot(bicbma.model)
plot(bicbma.model, e = 1e-04, mfrow = NULL, 
     include = 1:length(bicbma.model$names))

#bicbma.model

y.predict.bicbma <-  1/(1+(exp(-(cbind(matrix(1,dim(x.sample.scale)[1],1),x.sample.scale) %*% bicbma.model$postmean)))) # %*% porcentaje matricial: Y estimado - parametros esperados. Coger matriz x por su respectivo betas y sumelo y pongale 1.
y.predict.bicbma <- unlist(lapply(y.predict.bicbma,round))

y.predict.bicbma2 <- 1/(1+(exp(-(cbind(matrix(1,dim(x.sample.scale)[1],1),x.sample.scale) %*% bicbma.model$condpostmean)))) # lo que ajusta es logaritmo natual de Y. Por la familia, px / 1-px
y.predict.bicbma2 <- unlist(lapply(y.predict.bicbma2,round))

auc.bicbma <- accuracy(y.sample, y.predict.bicbma)
auc.result[4]<-auc.bicbma
auc.bicbma2 <- accuracy(y.sample, y.predict.bicbma2)
auc.result[5]<-auc.bicbma2

# bicbma - ROC

roc.bicbma <- roc(y.sample, y.predict.bicbma,quiet=TRUE)
roc.result[4]<-roc.bicbma$auc

roc.bicbma2 <- roc(y.sample, y.predict.bicbma2,quiet=TRUE)
roc.result[5]<-roc.bicbma2$auc

# Juntando el resultado de los coeficientes

lasso.coef <- coef(lasso.model)
ridge.coef <- coef(ridge.model)
elasticnet.coef <- coef(elasticnet.model)
bicbma.coef <- bicbma.model$postmean
bicbma.coef2<- bicbma.model$condpostmean

lasso.coef@x
lasso.coef@i

nombres<-c("Lasso","Ridge","Elasticnet","BMA_post","BMA_condpost","PIP_BMA")
models<-matrix(0,6,length(seleccion)+1)
namesmodels<-c("intercept",names(x.sample))
colnames(models)<-namesmodels
models<-data.frame(models)
models[1,lasso.coef@i+1]<-lasso.coef@x
models[2,ridge.coef@i+1]<-ridge.coef@x
models[3,elasticnet.coef@i+1]<-elasticnet.coef@x
models[4,]<-bicbma.model$postmean
models[5,]<-bicbma.model$condpostmean
models[6,]<-c(100,bicbma.model$probne0)
models<-add_column(models, modelos=nombres, .before = 1)
models<- models %>% mutate(AUC=auc.result,ROC=roc.result)


x<-cbind(lasso.coef,ridge.coef,elasticnet.coef,bicbma.coef,bicbma.coef2)

# corremos validacion de los modelos en los datos separados para el test ---------------------------

auc.validation<-matrix(0,5,1)
roc.validation<-matrix(0,5,1)

validation <- df[-ind_train,]

y.validation <- validation[,2]
x.validation <- validation[,seleccion+2]
# medias
x.sample.means
# varianzas
x.sample.variances
# scale 
x.validation.scale <- sweep(x.validation, 2, x.sample.means, FUN="-")
x.validation.scale<-sweep(x.validation.scale, 2, x.sample.variances, FUN="/")
x.validation.scale<-data.matrix(x.validation.scale)
#lasso validation
y.predict.lasso.validation <- as.numeric(predict(lasso.model, x.validation.scale, type = "class"))
auc.validation[1] <- accuracy(y.validation, y.predict.lasso.validation)
roc.lasso.validation <- roc(y.validation, y.predict.lasso.validation, quiet=TRUE)
roc.validation[1]<-roc.lasso.validation$auc

# Ridge validation
y.predict.ridge.validation <- as.numeric(predict(ridge.model, x.validation.scale, type = "class"))
auc.validation[2] <- accuracy(y.validation, y.predict.ridge.validation)
roc.ridge.validation <- roc(y.validation, y.predict.ridge.validation, quiet=TRUE)
roc.validation[2]<-roc.ridge.validation$auc

# Elasticnet validation
y.predict.elasticnet.validation <- as.numeric(predict(elasticnet.model, x.validation.scale, type = "class"))
auc.validation[3] <- accuracy(y.validation, y.predict.elasticnet.validation)
roc.elasticnet.validation <- roc(y.validation, y.predict.elasticnet.validation, quiet=TRUE)
roc.validation[3]<-roc.elasticnet.validation$auc

# BMA validation
y.predict.bicbma.validation <- 1/(1+(exp(-(cbind(matrix(1,dim(x.validation.scale)[1],1),x.validation.scale) %*% bicbma.model$postmean))))
y.predict.bicbma.validation <- unlist(lapply(y.predict.bicbma.validation,round))
y.predict.bicbma2.validation <- 1/(1+(exp(-(cbind(matrix(1,dim(x.validation.scale)[1],1),x.validation.scale) %*% bicbma.model$condpostmean))))
y.predict.bicbma2.validation <- unlist(lapply(y.predict.bicbma2.validation,round))

auc.validation[4] <- accuracy(y.validation, y.predict.bicbma.validation)
auc.validation[5] <- accuracy(y.validation,y.predict.bicbma2.validation)

roc.bicbma.validation <- roc(y.validation, y.predict.bicbma.validation,quiet=TRUE)
roc.validation[4]<-roc.bicbma.validation$auc

roc.bicbma2.validation <- roc(y.validation, y.predict.bicbma2.validation,quiet=TRUE)
roc.validation[5]<-roc.bicbma2.validation$auc


nombres.validation<-c("Lasso","Ridge","Elasticnet","BMA_post","BMA_condpost")
models.validation<- data.frame(cbind(nombres.validation,auc.validation,roc.validation))
colnames(models.validation)<-c('modelos','AUC','ROC')

models
models.validation

# ante diferentes corridas los modelos son muy diferentes e igualmente los resultados.

# seleccion de variables -------------------------------------------

# particionamos la muestra para buscar consistencia en las variables con PIP > 0.5 con bic.glm para el proceso de seleccion de variables


pip<-matrix(0,32,5)
for(i in 1:5) {
  ind_train <- sample(1:nrow(df), size = round(0.8*nrow(df)))
  sample <- df[ind_train,]
  y.sample <- sample[,2]
  x.sample <- sample[,seleccion+2]
  x.sample.scale <- scale(x.sample)
  
  bicbma.model<-bic.glm(x.sample.scale, y.sample, glm.family = binomial(link = "logit"),maxCol=33,
                        strict = FALSE, OR = 20, OR.fix = 2, nbest = 10, dispersion = NULL, 
                        factor.type = TRUE, factor.prior.adjust = FALSE, 
                        occam.window = TRUE, call = NULL)
  
  pip[,i]<-bicbma.model$probne0
}

count<-apply(matrix(sapply(pip,function(x){if (x >= 50) {1} else {0}}),32,5),1,sum)
pip<- cbind(pip,count)
colnames(pip)<-c("PIP1","PIP2","PIP3","PIP4","PIP5","conteo")
rownames(pip)<-names(x.sample)
pip

# variables seleccionadas

seleccion<-c(1,14,17,18,23,24)

M <- cor(df[,c(2,seleccion+2)])
col<- colorRampPalette(c("Blue"))
corrplot::corrplot(M, method = "color", order="hclust", col=RColorBrewer::brewer.pal(n=8, name="Blues"))

corrplot::corrplot.mixed(M, lower.col = "black", number.cex = .7)

cormat <- round(cor(df[,c(2,seleccion+2)]),2)
cormat[lower.tri(cormat,diag=TRUE)] <- NA
head(cormat)
melted_cormat <- melt(cormat)
melted_cormat <- melted_cormat %>% drop_na()
head(melted_cormat)
melted_cormat <- melted_cormat %>% mutate(signo=ifelse(value<0,'-','+'))
melted_cormat <- melted_cormat %>% mutate(value=abs(value))
melted_cormat <- melted_cormat[order(-melted_cormat[,3]),]
melted_cormat %>% filter(value>=0.4)

y.all <- df[,2]
x.all <- scale(df[,seleccion+2])

bicbma.model.pre<-bic.glm(x.all, y.all, glm.family = binomial(link = "logit"),maxCol=33,
                          strict = FALSE, OR = 20, OR.fix = 2, nbest = 10, dispersion = NULL, 
                          factor.type = TRUE, factor.prior.adjust = FALSE, 
                          occam.window = TRUE, call = NULL)

summary(bicbma.model.pre)

#  variables definitivas PIP > 90  ------------------------------------------

seleccion<-c(17,18,23)

#### Cross Validation modelos definitivos


fold<-5
folding <- modelr::crossv_kfold(df[,c(2,seleccion+2)],k=fold)

y.all <- df[,2]
x.all <- scale(df[,seleccion+2])

# lasso test
auc.test.lasso<-matrix(0,fold,1)
roc.test.lasso<-matrix(0,fold,1)

cv.lasso2 <- cv.glmnet(x.all, y.all, family = "binomial", nfold = fold, type.measure = "auc", parallel = TRUE, alpha = 1,standardize = FALSE)
lasso.model2 <- glmnet(x.all, y.all, family = "binomial", lambda = cv.lasso2$lambda.min, alpha = 1,standardize = FALSE)

y.predict.lasso2 <- as.numeric(predict(lasso.model2, x.all, type = "class"))
auclasso <- accuracy(y.all,y.predict.lasso2)
roc.lasso2 <- roc(y.all, y.predict.lasso2, quiet=TRUE)
roclasso<-roc.lasso2$auc

for(i in 1:fold) {
  idx.train<-folding$train[[i]]$idx
  x.train<-df[idx.train,seleccion+2]
  x.test<-df[-idx.train,seleccion+2]
  
  y.train<-df[idx.train,2]
  y.test<-df[-idx.train,2]
  
  x.train.mean <- apply(x.train, 2, mean)
  x.train.variances <- apply(x.train, 2, sd)
  
  x.train<-sweep(x.train, 2, x.train.mean, FUN="-")
  x.train<-sweep(x.train, 2, x.train.variances, FUN="/")
  x.train<-data.matrix(x.train)
  
  x.test<-sweep(x.test, 2, x.train.mean, FUN="-")
  x.test<-sweep(x.test, 2, x.train.variances, FUN="/")
  x.test<-data.matrix(x.test)
  
  
  lasso.model.train <- glmnet(x.train, y.train, family = "binomial", lambda = cv.lasso2$lambda.min, alpha = 1,standardize = FALSE)
  
  y.predict.lasso.test <- as.numeric(predict(lasso.model.train, x.test, type = "class"))
  auc.test.lasso[i] <- accuracy(y.test,y.predict.lasso.test)
  roc.lasso.test <- roc(y.test,y.predict.lasso.test, quiet=TRUE)
  roc.test.lasso[i]<-roc.lasso.test$auc
}

# ridge test
auc.test.ridge<-matrix(0,fold,1)
roc.test.ridge<-matrix(0,fold,1)

cv.ridge2 <- cv.glmnet(x.all, y.all, family = "binomial", nfold = fold, type.measure = "auc", parallel = TRUE, alpha = 0,standardize = FALSE)
ridge.model2 <- glmnet(x.all, y.all, family = "binomial", lambda = cv.ridge2$lambda.min, alpha = 1,standardize = FALSE)

y.predict.ridge2 <- as.numeric(predict(ridge.model2, x.all, type = "class"))
aucridge <- accuracy(y.all,y.predict.ridge2)
roc.ridge2 <- roc(y.all, y.predict.ridge2, quiet=TRUE)
rocridge<-roc.ridge2$auc

for(i in 1:fold) {
  idx.train<-folding$train[[i]]$idx
  x.train<-df[idx.train,seleccion+2]
  x.test<-df[-idx.train,seleccion+2]
  
  y.train<-df[idx.train,2]
  y.test<-df[-idx.train,2]
  
  x.train.mean <- apply(x.train, 2, mean)
  x.train.variances <- apply(x.train, 2, sd)
  
  x.train<-sweep(x.train, 2, x.train.mean, FUN="-")
  x.train<-sweep(x.train, 2, x.train.variances, FUN="/")
  x.train<-data.matrix(x.train)
  
  x.test<-sweep(x.test, 2, x.train.mean, FUN="-")
  x.test<-sweep(x.test, 2, x.train.variances, FUN="/")
  x.test<-data.matrix(x.test)
  
  
  ridge.model.train <- glmnet(x.train, y.train, family = "binomial", lambda = cv.ridge2$lambda.min, alpha = 0,standardize = FALSE)
  
  y.predict.ridge.test <- as.numeric(predict(ridge.model.train, x.test, type = "class"))
  auc.test.ridge[i] <- accuracy(y.test,y.predict.ridge.test)
  roc.ridge.test <- roc(y.test,y.predict.ridge.test, quiet=TRUE)
  roc.test.ridge[i]<-roc.ridge.test$auc
}

# elasticnet test
auc.test.elasticnet<-matrix(0,fold,1)
roc.test.elasticnet<-matrix(0,fold,1)

a <- seq(0.1, 0.9, 0.05)
search2 <- foreach(j = a, .combine = rbind) %dopar% {
  cv2 <- cv.glmnet(x.all, y.all, family = "binomial", nfold = fold, type.measure = "auc", parallel = TRUE, alpha = j,standardize = FALSE)
  data.frame(cvm = cv2$cvm[cv2$lambda == cv2$lambda.min], lambda.min = cv2$lambda.min, alpha = j)
}
cv.optimo2 <- search2[search2$cvm == min(search2$cvm), ]
elasticnet.model2 <- glmnet(x.all, y.all, family ="binomial", lambda = cv.optimo2$lambda.min, alpha = cv.optimo2$alpha, standardize = FALSE)

y.predict.elasticnet2 <- as.numeric(predict(elasticnet.model2, x.all, type = "class"))
aucelasticnet <- accuracy(y.all,y.predict.elasticnet2)
roc.elasticnet2 <- roc(y.all, y.predict.elasticnet2, quiet=TRUE)
rocelasticnet<-roc.elasticnet2$auc

for(i in 1:fold) {
  idx.train<-folding$train[[i]]$idx
  x.train<-df[idx.train,seleccion+2]
  x.test<-df[-idx.train,seleccion+2]
  
  y.train<-df[idx.train,2]
  y.test<-df[-idx.train,2]
  
  x.train.mean <- apply(x.train, 2, mean)
  x.train.variances <- apply(x.train, 2, sd)
  
  x.train<-sweep(x.train, 2, x.train.mean, FUN="-")
  x.train<-sweep(x.train, 2, x.train.variances, FUN="/")
  x.train<-data.matrix(x.train)
  
  x.test<-sweep(x.test, 2, x.train.mean, FUN="-")
  x.test<-sweep(x.test, 2, x.train.variances, FUN="/")
  x.test<-data.matrix(x.test)
  
  
  elasticnet.model.train <- glmnet(x.train, y.train, family = "binomial", lambda = cv.optimo2$lambda.min, alpha = cv.optimo2$alpha, standardize = FALSE)
  
  y.predict.elasticnet.test <- as.numeric(predict(elasticnet.model.train, x.test, type = "class"))
  auc.test.elasticnet[i] <- accuracy(y.test,y.predict.elasticnet.test)
  roc.elasticnet.test <- roc(y.test,y.predict.elasticnet.test, quiet=TRUE)
  roc.test.elasticnet[i]<-roc.elasticnet.test$auc
}

# BMA bic - GLM - test

auc.test.bicbma<-matrix(0,fold,1)
auc.test.bicbma2<-matrix(0,fold,1)
roc.test.bicbma<-matrix(0,fold,1)
roc.test.bicbma2<-matrix(0,fold,1)

bicbma.model2<-bic.glm(x.all, y.all, glm.family = binomial(link = "logit"),maxCol=33,
                       strict = FALSE, OR = 20, OR.fix = 2, nbest = 10, dispersion = NULL, 
                       factor.type = TRUE, factor.prior.adjust = FALSE, 
                       occam.window = TRUE, call = NULL)

y.predict.bicbma2 <-  1/(1+(exp(-(cbind(matrix(1,dim(x.all)[1],1),x.all) %*% bicbma.model2$postmean)))) # %*% porcentaje matricial: Y estimado - parametros esperados. Coger matriz x por su respectivo betas y sumelo y pongale 1.
y.predict.bicbma2 <- unlist(lapply(y.predict.bicbma2,round))
aucbicbma <- accuracy(y.all, y.predict.bicbma2)
roc.bicbma2 <- roc(y.all, y.predict.bicbma2,quiet=TRUE)
rocbicbma<-roc.bicbma2$auc

y.predict.bicbma22 <- 1/(1+(exp(-(cbind(matrix(1,dim(x.all)[1],1),x.all) %*% bicbma.model2$condpostmean)))) # lo que ajusta es logaritmo natual de Y. Por la familia, px / 1-px
y.predict.bicbma22 <- unlist(lapply(y.predict.bicbma22,round))
aucbicbma2 <- accuracy(y.all, y.predict.bicbma22)
roc.bicbma22 <- roc(y.all, y.predict.bicbma22,quiet=TRUE)
rocbicbma2<-roc.bicbma22$auc

for(i in 1:fold) {
  idx.train<-folding$train[[i]]$idx
  x.train<-df[idx.train,seleccion+2]
  x.test<-df[-idx.train,seleccion+2]
  
  y.train<-df[idx.train,2]
  y.test<-df[-idx.train,2]
  
  x.train.mean <- apply(x.train, 2, mean)
  x.train.variances <- apply(x.train, 2, sd)
  
  x.train<-sweep(x.train, 2, x.train.mean, FUN="-")
  x.train<-sweep(x.train, 2, x.train.variances, FUN="/")
  x.train<-data.matrix(x.train)
  
  x.test<-sweep(x.test, 2, x.train.mean, FUN="-")
  x.test<-sweep(x.test, 2, x.train.variances, FUN="/")
  x.test<-data.matrix(x.test)
  
  
  bicbma.model.train<-bic.glm(x.train, y.train, glm.family = binomial(link = "logit"),maxCol=33,
                              strict = FALSE, OR = 20, OR.fix = 2, nbest = 10, dispersion = NULL, 
                              factor.type = TRUE, factor.prior.adjust = FALSE, 
                              occam.window = TRUE, call = NULL)
  
  y.predict.bicbma.test <-  1/(1+(exp(-(cbind(matrix(1,dim(x.test)[1],1),x.test) %*% bicbma.model.train$postmean)))) # %*% porcentaje matricial: Y estimado - parametros esperados. Coger matriz x por su respectivo betas y sumelo y pongale 1.
  y.predict.bicbma.test <- unlist(lapply(y.predict.bicbma.test,round))
  auc.test.bicbma[i] <- accuracy(y.test, y.predict.bicbma.test)
  roc.bicbma.test <- roc(y.test, y.predict.bicbma.test,quiet=TRUE)
  roc.test.bicbma[i]<-roc.bicbma.test$auc
  
  y.predict.bicbma2.test <- 1/(1+(exp(-(cbind(matrix(1,dim(x.test)[1],1),x.test) %*% bicbma.model.train$condpostmean)))) # lo que ajusta es logaritmo natual de Y. Por la familia, px / 1-px
  y.predict.bicbma2.test <- unlist(lapply(y.predict.bicbma2.test,round))
  auc.test.bicbma2[i] <- accuracy(y.test, y.predict.bicbma2.test)
  roc.bicbma2.test <- roc(y.test, y.predict.bicbma2.test,quiet=TRUE)
  roc.test.bicbma2[i]<-roc.bicbma2.test$auc
}

# resultados tabla final


modelfinal<-c(auclasso,aucridge,aucelasticnet,aucbicbma,aucbicbma2,roclasso,rocridge,rocelasticnet,rocbicbma,rocbicbma2)
modelscv<-cbind(auc.test.lasso, auc.test.ridge, auc.test.elasticnet, auc.test.bicbma, auc.test.bicbma2,roc.test.lasso, roc.test.ridge, roc.test.elasticnet, roc.test.bicbma, roc.test.bicbma2)
meanmodelscv<-apply(modelscv, 2, mean)
sdmodelscv<-apply(modelscv, 2, sd)
tablafinal<-rbind(modelfinal,modelscv,meanmodelscv,sdmodelscv)
rownames(tablafinal)<-c("Model_all","CV1","CV2","CV3","CV4","CV5","meanCV","sdCV")
colnames(tablafinal)<-c("aucL","aucR","aucE","aucB","aucB2","rocL","rocR","rocE","rocB","rocB2")

#resultados finales
tablafinal

#modelos finales ------------------------------------------------------------
lasso.coef <- coef(lasso.model2)
ridge.coef <- coef(ridge.model2)
elasticnet.coef <- coef(elasticnet.model2)

nombres<-c("Lasso","Ridge","Elasticnet","BMA_post","BMA_condpost","PIP_BMA")
modelsf<-matrix(0,6,length(seleccion)+1)
namesmodels<-c("intercept",names(df[,seleccion+2]))
colnames(modelsf)<-namesmodels
modelsf<-data.frame(modelsf)
modelsf[1,lasso.coef@i+1]<-lasso.coef@x
modelsf[2,ridge.coef@i+1]<-ridge.coef@x
modelsf[3,elasticnet.coef@i+1]<-elasticnet.coef@x
modelsf[4,]<-bicbma.model2$postmean
modelsf[5,]<-bicbma.model2$condpostmean
modelsf[6,]<-c(100,bicbma.model2$probne0)
modelsf<-add_column(modelsf, modelos=nombres, .before = 1)

summary(bicbma.model2)

# medias x.all
x.all.mean <- apply(df[,seleccion+2], 2, mean)

# varianzas x.all
x.all.sd <- apply(df[,seleccion+2], 2, sd)

