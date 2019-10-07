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

counts_transformation <- function(x) {
  if (x == 0) {0} else {1}
}

df <- gettingTheData("datacountstudents.csv")

seleccion<-c(3,25)

y.all <- df[,2]
x.all <- scale(df[,seleccion+2])

# medias x.all
x.all.mean <- apply(df[,seleccion+2], 2, mean)

# varianzas x.all
x.all.sd <- apply(df[,seleccion+2], 2, sd)


bicbma.model2<-bic.glm(x.all, y.all, glm.family = poisson(link = "log"),maxCol=33,
                       strict = FALSE, OR = 20, OR.fix = 2, nbest = 10, dispersion = NULL, 
                       factor.type = TRUE, factor.prior.adjust = FALSE, 
                       occam.window = TRUE, call = NULL)

y.predict.bicbma22 <- exp(cbind(matrix(1,dim(x.all)[1],1),x.all) %*% bicbma.model2$condpostmean)
y.predict.bicbma22 <- unlist(lapply(y.predict.bicbma22,round))

msebicbma2 <- mse(y.predict.bicbma22, y.all)

y.transf.predict.bicbma22 <- unlist(lapply(as.vector(y.predict.bicbma22), function(x) counts_transformation(x) ), use.names = FALSE)
y.transf.sample.bicbma2 <- unlist(lapply(y.all, function(x) counts_transformation(x) ), use.names = FALSE)
accbicbma2 <- accuracy(y.transf.sample.bicbma2, y.transf.predict.bicbma22)

#### Generando el modelo

modelo.final <-bicbma.model2$condpostmean

###################################################################### 
# Corriendo modelo seleccionado con datos ingresados para validación #
######################################################################

df.modelo.final <- gettingTheData("datacountstudents_eval.csv")

seleccion.modelo.final <-c(3,25)

y.modelo.all <- df[,2]
x.modelo.final <- df.modelo.final[,seleccion.modelo.final+2]

# Se estandariza los datos con las medias y varianzas dadas

x.modelo.final<-sweep(x.modelo.final, 2, x.all.mean, FUN="-")
x.modelo.final<-sweep(x.modelo.final, 2, x.all.sd, FUN="/")
scaled.x.modelo.final<-data.matrix(x.modelo.final)

# Modelo
y.predict.modelo.final <- exp(cbind(matrix(1,dim(scaled.x.modelo.final)[1],1),scaled.x.modelo.final) %*% modelo.final)

print("betas")
print(modelo.final)
y.predict.modelo.final <- unlist(lapply(y.predict.modelo.final,round))

mse.modelo.final <- mse(y.predict.modelo.final, y.modelo.all)

y.transf.predict.modelo.final <- unlist(lapply(as.vector(y.predict.modelo.final), function(x) counts_transformation(x) ), use.names = FALSE)
y.transf.sample.modelo.final <- unlist(lapply(y.modelo.all, function(x) counts_transformation(x) ), use.names = FALSE)
acc.modelo.final <- accuracy(y.transf.sample.modelo.final, y.transf.predict.modelo.final)

####################
# Tabla resultados #
####################

resultados.data <- rbind(c(nrow(df),msebicbma2,accbicbma2), c(nrow(df.modelo.final),mse.modelo.final,acc.modelo.final))

colnames(resultados.data) <- c("Obseravaciones", "MSE", "Accuracy")

rownames(resultados.data) <- c("train", "validation")

resultados.data
