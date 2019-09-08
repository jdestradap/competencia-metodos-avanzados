# Function to extract a prepare the data

gettingTheData <- function(filePath) {
  data <- na.omit(read.table(filePath, header = TRUE, sep = ","))
}


df <- gettingTheData("databinarystudents.csv")

ind_train <- sample(1:nrow(df), size = round(0.7*nrow(df)))

train_y <- data_train$yL <- df[ind_train,]

train_x <- data_train

data_test = df[-ind_train,]

scale.dat <- scale(data_train)