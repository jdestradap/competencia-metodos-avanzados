library(caret)
library(glmnet)
gettingTheData <- function(filePath) {
  data <- na.omit(read.table(filePath, header = TRUE, sep = ","))
}

df <- gettingTheData("databinarystudents.csv")

ind_train <- sample(1:nrow(df), size = round(1*nrow(df)))

train <- df[ind_train,]

test <- df[-ind_train,]

y.train <- train[,2]

x.train.scale <- scale(train[,3:34])

y.test <- test[,2]

# Set training control
train_control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 5,
                              search = "random",
                              verboseIter = TRUE)

# Train the model
y.train.factor <- as.factor(y.train)

elastic_net_model <- train(x.train.scale,y.train.factor,
                           method = "glmnet",
                           preProcess = NULL,
                           tuneLength = 25, # Amount of granularity in the tuning parameter grid
                           trControl = train_control,
                           family="binomial")

md3 <- glmnet(x.train.scale, y.train, family = 'binomial', lambda = 0.240651637, alpha = 0.12921161)
coef(md3)
