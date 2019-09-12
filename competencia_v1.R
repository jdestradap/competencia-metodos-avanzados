# MSE para continuo

## Accuracy 
## MSE conteo
library(BMA)
attach(mtcars)
library(reshape2)
library(glmnet)
library(Metrics)
library(MetricsWeighted)
gettingTheData <- function(filePath) {
  data <- na.omit(read.table(filePath, header = TRUE, sep = ","))
}

df <- gettingTheData("datacontinuousstudents.csv")

# Analizando

M <- cor(df[,2:34])
col<- colorRampPalette(c("Blue"))
corrplot::corrplot(M, method = "color", order="hclust", col=RColorBrewer::brewer.pal(n=8, name="Blues"))

corrplot::corrplot.mixed(M, lower.col = "black", number.cex = .7)

# organizar este resultado, eliminando los duplicados, y poniendo al lado los signos.
cormat <- round(cor(df[,2:34]),2)
head(cormat)
melted_cormat <- melt(cormat)
melted_cormat <- melted_cormat[order(-melted_cormat[,3]),]
head(melted_cormat)
melted_cormat <- melted_cormat[melted_cormat$value!=1,]
melted_cormat

ind_train <- sample(1:nrow(df), size = round(0.8*nrow(df)))

# 80 %
sample <- df[ind_train,]

# 20 % validacion
validation <- df[-ind_train,]

y.sample <- sample[,2]

## x sin estandarizar
x.sample <- sample[,3:34]

# varianzas
x.sample.variances <- apply(x.sample, 2, sd)

# medias
x.sample.means <- apply(x.sample, 2, mean)

# scale 
x.sample.scale <- scale(x.sample)

#Check
(x.sample.scale[1,1] * x.sample.variances[1]) + x.sample.means[1]

# Lasso - GLM 

cv.lasso <- cv.glmnet(x.sample.scale, y.sample, family = "gaussian", nfold = 5, type.measure = "mse", parallel = TRUE, alpha = 1)
lasso.model <- glmnet(x.sample.scale, y.sample, family = "gaussian", lambda = cv.lasso$lambda.min, alpha = 1)


plot(cv.lasso)

y.predict = predict(lasso.model, x.sample.scale)

mse(y.predict, y.sample)

continuous_transformation <- function(x) {
  if (x <= -1) {0} else {1}
}

y.transf.predict <- unlist(lapply(as.vector(y.predict), function(x) continuous_transformation(x) ), use.names = FALSE)
y.transf.sample <- unlist(lapply(y.sample, function(x) continuous_transformation(x) ), use.names = FALSE)

# Lasso - Accuracy

lasso.accuracy <- accuracy(y.transf.sample, y.transf.predict)

# Ridge - GLM 

cv.ridge <- cv.glmnet(x.sample.scale, y.sample, family = "gaussian", nfold = 5, type.measure = "mse", parallel = TRUE, alpha = 0)
ridge.model <- glmnet(x.sample.scale, y.sample, family = "gaussian", lambda = cv.lasso$lambda.min, alpha = 0)

plot(cv.ridge)

y.predict.ridge = predict(ridge.model, x.sample.scale)

mse(y.predict.ridge, y.sample)

continuous_transformation <- function(x) {
  if (x <= -1) {0} else {1}
}

y.transf.predict.ridge <- unlist(lapply(as.vector(y.predict.ridge), function(x) continuous_transformation(x) ), use.names = FALSE)
y.transf.sample.ridge <- unlist(lapply(y.sample, function(x) continuous_transformation(x) ), use.names = FALSE)

# Ridge - Accuracy

ridge.accuracy <- accuracy(y.transf.sample.ridge, y.transf.predict.ridge)

ridge.accuracy


# Juntando el resultado de los coeficientes

lasso.coef <- coef(lasso.model)
ridge.coef <- coef(ridge.model)

rbind(lasso.coef,ridge.coef)

# 


# Set training control
train_control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 5,
                              search = "random",
                              verboseIter = TRUE)

# Train the model
y.sample.factor <- as.factor(y.sample)

elastic_net_model <- train(x.sample.scale,y.sample.factor,
                           method = "glmnet",
                           preProcess = NULL,
                           tuneLength = 50, # Amount of granularity in the tuning parameter grid
                           trControl = train_control,
                           family="gaussian")

md3 <- glmnet(x.train.scale, y.train, family = 'binomial', lambda = 0.240651637, alpha = 0.12921161)
coef(md3)


