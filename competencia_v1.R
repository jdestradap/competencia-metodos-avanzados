# Function to extract a prepare the data

gettingTheData <- function(filePath) {
  data <- na.omit(read.table(filePath, header = TRUE, sep = ","))
}

df <- gettingTheData("databinarystudents.csv")

ind_train <- sample(1:nrow(df), size = round(0.8*nrow(df)))

train <- df[ind_train,]

test <- df[-ind_train,]

y.train <- train[,2]

x.train.scale <- scale(train[,3:34])

y.test <- test[,2]


# 

