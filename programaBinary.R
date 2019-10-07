library(dplyr)
library(tidyr)
library(glmnet)
library(Metrics)
library(tibble)
library(reshape2)
library(BMA)
library(BMS)
library(pROC)


###########################################################################
# Creando modelo con variables seleccionadas segun metodología del equipo #
###########################################################################

gettingTheData <- function(filePath) {
  data <- na.omit(read.table(filePath, header = TRUE, sep = ","))
}

df <- gettingTheData("databinarystudents.csv")

seleccion<-c(17,18)

y.all <- df[,2]
x.all <- scale(df[,seleccion+2])

# medias x.all
x.all.mean <- apply(df[,seleccion+2], 2, mean)

# varianzas x.all
x.all.sd <- apply(df[,seleccion+2], 2, sd)

bicbma.model2<-bic.glm(x.all, y.all, glm.family = binomial(link = "logit"),maxCol=33,
                       strict = FALSE, OR = 20, OR.fix = 2, nbest = 10, dispersion = NULL, 
                       factor.type = TRUE, factor.prior.adjust = FALSE, 
                       occam.window = TRUE, call = NULL)

y.predict.bicbma22 <- 1/(1+(exp(-(cbind(matrix(1,dim(x.all)[1],1),x.all) %*% bicbma.model2$condpostmean)))) # lo que ajusta es logaritmo natual de Y. Por la familia, px / 1-px
y.predict.bicbma22 <- unlist(lapply(y.predict.bicbma22,round))
accbicbma2 <- accuracy(y.all, y.predict.bicbma22)
roc.bicbma22 <- roc(y.all, y.predict.bicbma22,quiet=TRUE)
rocbicbma2<-roc.bicbma22$auc

#### Generando el modelo

modelo.final <-bicbma.model2$condpostmean

###################################################################### 
# Corriendo modelo seleccionado con datos ingresados para validación #
######################################################################

df.modelo.final <- gettingTheData("databinarystudents_eval.csv")

seleccion.modelo.final <-c(17,18)

y.modelo.all <- df[,2]
x.modelo.final <- df.modelo.final[,seleccion.modelo.final+2]

# Se estandariza los datos con las medias y varianzas dadas

x.modelo.final<-sweep(x.modelo.final, 2, x.all.mean, FUN="-")
x.modelo.final<-sweep(x.modelo.final, 2, x.all.sd, FUN="/")
scaled.x.modelo.final<-data.matrix(x.modelo.final)

# Modelo

y.predict.modelo.final <- 1/(1+(exp(-(cbind(matrix(1,dim(scaled.x.modelo.final)[1],1),scaled.x.modelo.final) %*% modelo.final)))) # lo que ajusta es logaritmo natual de Y. Por la familia, px / 1-px
y.predict.modelo.final <- unlist(lapply(y.predict.modelo.final,round))
acc.modelo.final <- accuracy(y.modelo.all, y.predict.modelo.final)
roc.modelo.final <- roc(y.modelo.all, y.predict.modelo.final, quiet=TRUE)
roc.modelo.final.auc <- roc.modelo.final$auc

####################
# Tabla resultados #
####################

resultados.data <- rbind(c(nrow(df),accbicbma2,rocbicbma2), c(nrow(df.modelo.final),acc.modelo.final,roc.modelo.final.auc))

colnames(resultados.data) <- c("Obseravaciones","Accuracy","AUC")

rownames(resultados.data) <- c("train", "validation")

resultados.data
