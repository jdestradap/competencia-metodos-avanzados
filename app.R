library(shiny)
library(dplyr)
library(tidyr)
library(glmnet)
library(Metrics)
library(tibble)
library(reshape2)
library(BMA)
library(BMS)
library(pROC)

# Define UI for data upload app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Trabajo 1 - Métodos Avanzados en Estadística"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Seleccionar archivo conteo",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            ####
            tags$hr(),
            # Input: Select a file ----
            fileInput("file2", "Seleccionar archivo continuo",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header2", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep2", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            
            ####
            tags$hr(),
            # Input: Select a file ----
            fileInput("file3", "Seleccionar archivo binario",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header3", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep3", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ",")
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
            tableOutput("conteo"),
            tableOutput("continuo"),
            tableOutput("binario")
            
        )
    )
)

# Define server logic to read selected file ----
server <- function(input, output) {
    output$conteo <- renderTable(rownames = TRUE, colnames = TRUE,{
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                # La base de datos debe tener los 32 regresores antes de subir el archivo. Y corresponden a C1, C2, C3
                df.training.tmp <- read.csv("datacountstudents.csv",
                                   header = input$header,
                                   sep = input$sep)
                
                df.training = as.data.frame(lapply(df.training.tmp, na.omit))
                
                df.tmp <- read.csv(input$file1$datapath,
                                   header = input$header,
                                   sep = input$sep)
                
                df = as.data.frame(lapply(df.tmp, na.omit))
                
                # medias x.all
                seleccion<-c(3, 24, 25)
                
                ## Se deben cargar los dos archivos, se saca el mean y estandar
                x.all.mean <- apply(df.training[,seleccion+2], 2, mean)
                
                # varianzas x.all
                x.all.sd <- apply(df.training[,seleccion+2], 2, sd)

                resultado.modelo <- modelo.conteo(df.training, seleccion)
                
                resultados.eval <- evaluando.modelo.conteo(resultado.modelo, df, x.all.mean, x.all.sd, seleccion)
            
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        colnames(resultados.eval) <- c("datos de validación - conteo")
        rownames(resultados.eval) <- c("Observaciones", "MSE", "Accuracy")
        resultados.eval
    })
    
    output$continuo <- renderTable(rownames = TRUE, colnames = TRUE,{
        req(input$file2)
        
        tryCatch(
            {
                df.training.tmp <- read.csv("datacontinuousstudents.csv",
                                            header = input$header2,
                                            sep = input$sep2)
                
                df.training = as.data.frame(lapply(df.training.tmp, na.omit))
                
                
                df.tmp <- read.csv(input$file2$datapath,
                                   header = input$header2,
                                   sep = input$sep2)
                
                df = as.data.frame(lapply(df.tmp, na.omit))
                # medias x.all
                seleccion<-c(23,25,31)
                x.all.mean <- apply(df.training[,seleccion+2], 2, mean)
                
                # varianzas x.all
                x.all.sd <- apply(df.training[,seleccion+2], 2, sd)
                
                
                resultado.modelo <- modelo.continuo(df, seleccion)
                
                resultados.eval <- evaluando.modelo.continuo(resultado.modelo, df, x.all.mean, x.all.sd, seleccion)
                
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        colnames(resultados.eval) <- c("datos de validación - continuo")
        rownames(resultados.eval) <- c("Observaciones", "MSE", "Accuracy")
        resultados.eval
        
    })
    
    output$binario <- renderTable(rownames = TRUE, colnames = TRUE,{
        req(input$file3)
        
        tryCatch(
            {
                df.training.tmp <- read.csv("databinarystudents.csv",
                                            header = input$header3,
                                            sep = input$sep3)
                
                df.training = as.data.frame(lapply(df.training.tmp, na.omit))
                
                df.tmp <- read.csv(input$file3$datapath,
                               header = input$header3,
                               sep = input$sep3)
                
                df = as.data.frame(lapply(df.tmp, na.omit))
                
                seleccion<-c(17,18,23,24)
                x.all.mean <- apply(df.training[,seleccion+2], 2, mean)
                # varianzas x.all
                x.all.sd <- apply(df.training[,seleccion+2], 2, sd)
                resultado.modelo <- modelo.binario(df, seleccion)
                resultados.eval <- evaluando.modelo.binario(resultado.modelo, df, x.all.mean, x.all.sd, seleccion)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        colnames(resultados.eval) <- c("datos de validación - binario")
        rownames(resultados.eval) <- c("Observaciones", "Accuracy", "AUC")
        resultados.eval
    })
}

## Utils
counts_transformation <- function(x) {
    if (x == 0) {0} else {1}
}

continuous_transformation <- function(x) {
    if (x <= -1) {0} else {1}
}

##############################################
#
# resultados.data
# Obseravaciones      MSE Accuracy
# train                 150 1.353333     0.66
# validation            150 1.353333     0.66
#
##############################################

modelo.conteo <- function(data.archivo, seleccion) {
    y.all <- data.archivo[,2]
    x.all <- scale(data.archivo[,seleccion+2])
    
    bicbma.model2<-bic.glm(x.all, y.all, glm.family = poisson(link = "log"),maxCol=33,
                           strict = FALSE, OR = 20, OR.fix = 2, nbest = 10, 
                           occam.window = TRUE)
    
    y.predict.bicbma22 <- exp(cbind(matrix(1,dim(x.all)[1],1),x.all) %*% bicbma.model2$condpostmean)
    y.predict.bicbma22 <- unlist(lapply(y.predict.bicbma22,round))
    
    msebicbma2 <- mse(y.predict.bicbma22, y.all)
    
    y.transf.predict.bicbma22 <- unlist(lapply(as.vector(y.predict.bicbma22), function(x) counts_transformation(x) ), use.names = FALSE)
    y.transf.sample.bicbma2 <- unlist(lapply(y.all, function(x) counts_transformation(x) ), use.names = FALSE)
    accbicbma2 <- accuracy(y.transf.sample.bicbma2, y.transf.predict.bicbma22)
    
    bicbma.model2$condpostmean
}

evaluando.modelo.conteo <- function(data.modelo.conteo, data.archivo.evaluacion, x.all.mean, x.all.sd, seleccion) {
    print("Estos son los betas")
    print(data.modelo.conteo)
    
    print("x.all.mean")
    print(x.all.mean)
    
    print("x.all.sd")
    print(x.all.sd)
    
    y.modelo.all <- data.archivo.evaluacion[,2]
    x.modelo.final <- data.archivo.evaluacion[,seleccion+2]
    
    # Se estandariza los datos con las medias y varianzas dadas
    
    x.modelo.final<-sweep(x.modelo.final, 2, x.all.mean, FUN="-")
    x.modelo.final<-sweep(x.modelo.final, 2, x.all.sd, FUN="/")
    scaled.x.modelo.final<-data.matrix(x.modelo.final)
    
    # Modelo
    y.predict.modelo.final <- exp(cbind(matrix(1,dim(scaled.x.modelo.final)[1],1),scaled.x.modelo.final) %*% data.modelo.conteo)
    y.predict.modelo.final <- unlist(lapply(y.predict.modelo.final,round))
    
    mse.modelo.final <- mse(y.predict.modelo.final, y.modelo.all)
    
    print("mse.modelo.final")
    print(mse.modelo.final)
    
    y.transf.predict.modelo.final <- unlist(lapply(as.vector(y.predict.modelo.final), function(x) counts_transformation(x) ), use.names = FALSE)
    y.transf.sample.modelo.final <- unlist(lapply(y.modelo.all, function(x) counts_transformation(x) ), use.names = FALSE)
    acc.modelo.final <- accuracy(y.transf.sample.modelo.final, y.transf.predict.modelo.final)
    
    print("acc.modelo.final")
    print(acc.modelo.final)
    
    resultados.data <- rbind(nrow(data.archivo.evaluacion),mse.modelo.final,acc.modelo.final)
    
    print(resultados.data)
    
    resultados.data
}

##############################################
#
# Obseravaciones      MSE  Accuracy
# train                 150 3.939514 0.7733333
# validation            150 3.939514 0.7733333
#
##############################################

modelo.continuo <- function(data.archivo, seleccion) {
    y.all <- data.archivo[,2]
    x.all <- scale(data.archivo[,seleccion+2])
    
    bicbma.model2<-bic.glm(x.all, y.all, glm.family = gaussian(link = "identity"),maxCol=33,
                           strict = FALSE, OR = 20, OR.fix = 2, nbest = 10, occam.window = TRUE)
    
    y.predict.bicbma22 <- cbind(matrix(1,dim(x.all)[1],1),x.all) %*% bicbma.model2$condpostmean
    msebicbma2 <- mse(y.predict.bicbma22, y.all)

    y.transf.predict.bicbma22 <- unlist(lapply(as.vector(y.predict.bicbma22), function(x) continuous_transformation(x) ), use.names = FALSE)
    y.transf.sample.bicbma2 <- unlist(lapply(y.all, function(x) continuous_transformation(x) ), use.names = FALSE)
    accbicbma2 <- accuracy(y.transf.sample.bicbma2, y.transf.predict.bicbma22)

    bicbma.model2$condpostmean
}

evaluando.modelo.continuo <- function(data.modelo.continuo, data.archivo.evaluacion, x.all.mean, x.all.sd, seleccion) {
    print("Estos son los betas")
    print(data.modelo.continuo)
    
    print("x.all.mean")
    x.all.mean
    
    print("x.all.sd")
    x.all.sd
    
    y.modelo.all <- data.archivo.evaluacion[,2]
    x.modelo.final <- data.archivo.evaluacion[,seleccion +2]
    
    # Se estandariza los datos con las medias y varianzas dadas
    
    x.modelo.final<-sweep(x.modelo.final, 2, x.all.mean, FUN="-")
    x.modelo.final<-sweep(x.modelo.final, 2, x.all.sd, FUN="/")
    scaled.x.modelo.final<-data.matrix(x.modelo.final)
    
    # Modelo
    
    y.predict.modelo.final <- cbind(matrix(1,dim(scaled.x.modelo.final)[1],1),scaled.x.modelo.final) %*% data.modelo.continuo
    # y.predict.modelo.final <- unlist(lapply(y.predict.modelo.final,round))
    
    mse.modelo.final <- mse(y.predict.modelo.final, y.modelo.all)
    
    print("mse.modelo.final")
    print(mse.modelo.final)
    
    y.transf.predict.modelo.final <- unlist(lapply(as.vector(y.predict.modelo.final), function(x) continuous_transformation(x) ), use.names = FALSE)
    y.transf.sample.modelo.final <- unlist(lapply(y.modelo.all, function(x) continuous_transformation(x) ), use.names = FALSE)
    acc.modelo.final <- accuracy(y.transf.sample.modelo.final, y.transf.predict.modelo.final)
    
    print("acc.modelo.final")
    print(acc.modelo.final)

    resultados.data <- rbind(nrow(data.archivo.evaluacion),mse.modelo.final,acc.modelo.final)
    
    print("resultados.data")
    
    print(resultados.data)
    
    resultados.data
}

##############################################
#
# Obseravaciones  Accuracy  AUC
# train                 150 0.7333333 0.73
# validation            150 0.7333333 0.73
#
##############################################

modelo.binario <- function(data.archivo, seleccion) {
    y.all <- data.archivo[,2]
    x.all <- scale(data.archivo[,seleccion+2])
    
    bicbma.model2<-bic.glm(x.all, y.all, glm.family = binomial(link = "logit"),maxCol=33,
                           strict = FALSE, OR = 20, OR.fix = 2, nbest = 10, occam.window = TRUE)
    
    y.predict.bicbma22 <- 1/(1+(exp(-(cbind(matrix(1,dim(x.all)[1],1),x.all) %*% bicbma.model2$condpostmean)))) # lo que ajusta es logaritmo natual de Y. Por la familia, px / 1-px
    y.predict.bicbma22 <- unlist(lapply(y.predict.bicbma22,round))
    
    accbicbma2 <- accuracy(y.all, y.predict.bicbma22)
    roc.bicbma22 <- roc(y.all, y.predict.bicbma22,quiet=TRUE)
    rocbicbma2<-roc.bicbma22$auc
    
    bicbma.model2$condpostmean
}

evaluando.modelo.binario <- function(data.modelo.binario, data.archivo.evaluacion, x.all.mean, x.all.sd, seleccion) {
    print("Estos son los betas")
    print(data.modelo.binario)
    
    print("x.all.mean")
    print(x.all.mean)
    
    print("x.all.sd")
    print(x.all.sd)
    
    print("data.archivo.evaluacion")
    print(data.archivo.evaluacion)
    
    y.modelo.all <- data.archivo.evaluacion[,2]
    x.modelo.final <- data.archivo.evaluacion[,seleccion+2]
    
    # Se estandariza los datos con las medias y varianzas dadas
    
    x.modelo.final<-sweep(x.modelo.final, 2, x.all.mean, FUN="-")
    x.modelo.final<-sweep(x.modelo.final, 2, x.all.sd, FUN="/")
    scaled.x.modelo.final<-data.matrix(x.modelo.final)
    
    print("scaled.x.modelo.final")
    print(scaled.x.modelo.final)
    
    # Modelo
    
    y.predict.modelo.final <- 1/(1+(exp(-(cbind(matrix(1,dim(scaled.x.modelo.final)[1],1),scaled.x.modelo.final) %*% data.modelo.binario)))) # lo que ajusta es logaritmo natual 
    print("y.predict.modelo.final:")
    print(y.predict.modelo.final)
    
    y.predict.modelo.final <- unlist(lapply(y.predict.modelo.final,round))
    
    acc.modelo.final <- accuracy(y.modelo.all, y.predict.modelo.final)
    roc.modelo.final <- roc(y.modelo.all, y.predict.modelo.final, quiet=TRUE)
    roc.modelo.final.auc <- roc.modelo.final$auc
    
    resultados.data <- rbind(nrow(data.archivo.evaluacion),acc.modelo.final,roc.modelo.final.auc)
    resultados.data
}

# Create Shiny app ----
shinyApp(ui, server)