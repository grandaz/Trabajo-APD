heart_data <- read.csv("heart_data.csv")

# Elimina la columna index y id
heart_data <- heart_data[,-c(1,2)]

# Pasa la edad de d�as a a�os  (.25 para los a�os bisiestos)
#heart_data$age <- heart_data$age / 365.25 
#heart_data$age <- round(heart_data$age / 365.25)     # Redondea las edades
heart_data$age <- floor(heart_data$age / 365.25)     # Redondea hacia abajo
#heart_data$age <- ceiling(heart_data$age / 365.25)   # Redondea hacia arriba

# Crea una nueva columna para determinar si tiene presi�n arterial alta o baja
heart_data$high_bp <- ifelse(heart_data$ap_hi >= 140 | heart_data$ap_lo >= 90, 1, 0)


library(dplyr)

# Elimina las columnas ap_hi y ap_lo (despu�s se decidi� eliminar las variables altura y g�nero)
heart_data <- heart_data %>% select(-c("ap_hi", "ap_lo", "height", "gender"))


#------------------------------------------------------------------------------#
# An�lisis descriptivo                                                         #                                                
#------------------------------------------------------------------------------#

# 1. Resumen de los datos
summary(heart_data)


# 2. Comprobaci�n de valores faltantes
sapply(heart_data, function(x) sum(is.na(x)))


# 3. Distribuci�n de la variable dependiente
barplot(table(heart_data$cardio), 
        col = c("lightblue", "black"), 
        main = "Distribuci�n de la Variable Dependiente")


# 4. Distribuci�n de las variables independientes
hist(heart_data$age, main = "Distribuci�n de Edad", 
     xlab = "Edad", col = "lightblue", border = "black")

barplot(table(heart_data$gender), 
        col = c("lightblue", "lightyellow"), 
        main = "Distribuci�n de G�nero")

hist(heart_data$height, main = "Distribuci�n de Altura", 
     xlab = "Altura", col = "lightblue", border = "black")

hist(heart_data$weight, main = "Distribuci�n de Peso", 
     xlab = "Peso", col = "lightblue", border = "black")

hist(heart_data$cholesterol, 
     main = "Distribuci�n de nivel de colesterol", 
     xlab = "Nivel de colesterol", col = "lightblue", border = "black")

hist(heart_data$gluc, main = "Distribuci�n de nivel de glucosa", 
     xlab = "Nivel de glucosa", col = "lightblue", border = "black")

barplot(table(heart_data$smoke), 
        col = c("lightblue", "lightyellow"), 
        main = "Distribuci�n de Fumadores")

barplot(table(heart_data$alco), 
        col = c("lightblue", "lightyellow"), 
        main = "Distribuci�n de consumidores de alcohol")

barplot(table(heart_data$active), 
        col = c("lightblue", "lightyellow"), 
        main = "Distribuci�n de Actividad F�sica")

barplot(table(heart_data$high_bp), 
        col = c("lightblue", "lightyellow"), 
        main = "Distribuci�n de presi�n arterial alta")


# 5. Correlaci�n entre variables
cor(heart_data)
corr.test(heart_data)
cor.plot(cor(heart_data))


# 6. An�lisis bivariado
boxplot(age ~ cardio, data = heart_data, col = c("lightblue", "lightyellow"), 
        main = "Relaci�n entre Edad y Enfermedad cardiovascular")

barplot(table(heart_data$gender, heart_data$cardio), 
        col = c("lightblue", "lightyellow"), 
        legend = TRUE, beside = TRUE, 
        main = "Relaci�n entre G�nero y Enfermedad cardiovascular")

boxplot(height ~ cardio, data = heart_data, col = c("lightblue", "lightyellow"), 
        main = "Relaci�n entre Altura y Enfermedad cardiovascular")

boxplot(weight ~ cardio, data = heart_data, col = c("lightblue", "lightyellow"), 
        main = "Relaci�n entre Peso y Enfermedad cardiovascular")

barplot(table(heart_data$cholesterol, heart_data$cardio), 
        col = c("lightblue", "lightyellow", "lightgreen"), 
        legend = TRUE, beside = TRUE, 
        main = "Relaci�n entre Nivel de colesterol y Enfermedad cardiovascular")

barplot(table(heart_data$gluc, heart_data$cardio), 
        col = c("lightblue", "lightyellow", "lightgreen"), 
        legend = TRUE, beside = TRUE, 
        main = "Relaci�n entre Nivel de glucosa y Enfermedad cardiovascular")

barplot(table(heart_data$smoke, heart_data$cardio), 
        col = c("lightblue", "lightyellow"), 
        legend = TRUE, beside = TRUE, 
        main = "Relaci�n entre Fumadores y Enfermedad cardiovascular")

barplot(table(heart_data$alco, heart_data$cardio), 
        col = c("lightblue", "lightyellow"), 
        legend = TRUE, beside = TRUE, 
        main = "Relaci�n entre Consumidores de alcohol y Enfermedad cardiovascular")

barplot(table(heart_data$active, heart_data$cardio), 
        col = c("lightblue", "lightyellow"), 
        legend = TRUE, beside = TRUE, 
        main = "Relaci�n entre Actividad f�sica y Enfermedad cardiovascular")

barplot(table(heart_data$high_bp, heart_data$cardio), 
        col = c("lightblue", "lightyellow"), 
        legend = TRUE, beside = TRUE, 
        main = "Relaci�n entre Presi�n arterial alta y Enfermedad cardiovascular")


# 7. Manejo de outliers
boxplot(heart_data$age, main = "Boxplot de Variable Edad")
boxplot(heart_data$height, main = "Boxplot de Variable Altura")
boxplot(heart_data$weight, main = "Boxplot de Variable Peso")


# 8. Normalizaci�n/Estandarizaci�n de las variables para reducir influencia de outliers
heart_data[, c('age','weight')] <- scale(heart_data[, c('age','weight')])


# Factorizar los variables binarias y categ�ricas
# heart_data$gender <- factor(heart_data$gender)
heart_data$cholesterol <- factor(heart_data$cholesterol, ordered = TRUE)
heart_data$gluc <- factor(heart_data$gluc, ordered = TRUE)
heart_data$smoke <- factor(heart_data$smoke)
heart_data$alco <- factor(heart_data$alco)
heart_data$active <- factor(heart_data$active)
heart_data$high_bp <- factor(heart_data$high_bp)

#heart_data$cardio <- factor(heart_data$cardio)  # Variable dependiente
heart_data$cardio <- as.factor(make.names(heart_data$cardio))

#------------------------------------------------------------------------------#
# Validaci�n Cruzada (CV K-Fold)                                               #                                                
#------------------------------------------------------------------------------#

# Establecer semilla para reproducibilidad
set.seed(123)

# Divisi�n de los datos
library(caret)

# Crea una partici�n (70% entrenamiento, 15% validaci�n, 15% prueba)
indices_entrenamiento <- createDataPartition(y = heart_data$cardio, p = 0.7, list = FALSE)
train_data <- heart_data[indices_entrenamiento, ]
conjunto_temporal <- heart_data[-indices_entrenamiento, ]

# Crear partici�n en el conjunto temporal para obtener validaci�n y prueba (50% cada uno)
indices_temporales <- createDataPartition(y = conjunto_temporal$cardio, p = 0.5, list = FALSE)
validation_data <- conjunto_temporal[indices_temporales, ]
test_data <- conjunto_temporal[-indices_temporales, ]

# Definimos indicadores
# Puede ser "Accuracy",   "logLoss", "ROC",   "Kappa"
metrica <- "ROC"

# Definimos par�metros para la Validaci�n Cruzada
particiones  <- 5
repeticiones <- 10

# Definir la configuraci�n de control para la Validaci�n Cruzada
ctrl <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones,
                              #seeds = seeds,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

#------------------------------------------------------------------------------#
# Entrenamos el modelo de Regresi�n Log�stica                                  #                                                
#------------------------------------------------------------------------------#

logistic_hiperparametros <- data.frame(parameter = "none")

logistic_model <- train(cardio ~ ., data = train_data,
                         method = "glm",
                         tuneGrid = logistic_hiperparametros,
                         metric = metrica,
                         trControl = ctrl,
                         family = "binomial")
logistic_model
logistic_model$results
logistic_model$resample
summary(logistic_model$finalModel)

#------------------------------------------------------------------------------#
# Entrenamos el modelo de �rbol de Decisi�n                                    #                                                
#------------------------------------------------------------------------------#

# Define la cuadr�cula de ajuste para el par�metro cp
hiperparametros_tree <- expand.grid(cp = seq(0.01, 0.5, by = 0.01))

# Entrenar el modelo de �rbol de decisi�n
tree_model <- train(cardio ~ ., data = train_data,
                     method = "rpart",
                     tuneGrid = hiperparametros_tree,
                     metric = metrica,
                     trControl = ctrl)
tree_model
tree_model$results
tree_model$resample
summary(tree_model$finalModel)

# Visualizar el �rbol de decisi�n con rpart.plot
library(rpart.plot)
prp(tree_model$finalModel, type = 2, extra = 1, main = "�rbol de Decisi�n Binario")

library(partykit)
plot(as.party(tree_model$finalModel), tp_args = list(id = FALSE))


#------------------------------------------------------------------------------#
# Validaci�n de los modelos                                                    #                                                
#------------------------------------------------------------------------------#

library(pROC)

# Predecir las probabilidades en los datos de validaci�n
logistic_pred <- predict(logistic_model, newdata = validation_data, type = "prob")[,2]
tree_pred <- predict(tree_model, newdata = validation_data, type = "prob")[,2]

# Crear objetos roc para calcular el �rea bajo la curva ROC
logistic_roc <- roc(validation_data$cardio, logistic_pred)
tree_roc <- roc(validation_data$cardio, tree_pred)

# Comparar las �reas bajo la curva ROC
roc.test(logistic_roc, tree_roc)

# Visualizar las curvas ROC
plot(logistic_roc, col = "blue", main = "Curvas ROC de Modelos",
     lwd = 2, col.main = "black", ylim = c(0, 1))
lines(tree_roc, col = "red", lwd = 2)
legend("bottomright", legend = c("Regresi�n Log�stica", "�rbol de Decisi�n"),
       col = c("blue", "red"), lwd = 2)

# Calcular el AUC para ambos modelos
auc_logistic <- auc(logistic_roc)
auc_tree <- auc(tree_roc)

# Imprimir los resultados
cat("AUC para Regresi�n Log�stica:", auc_logistic, "\n")
cat("AUC para �rbol de Decisi�n:", auc_tree, "\n")


# Factorizar los resultados de la predicci�n seg�n el nombre
logistic_pred_binary <- factor(ifelse(logistic_pred > 0.5, "X1", "X0"), levels = levels(validation_data$cardio))
tree_pred_binary <- factor(ifelse(tree_pred > 0.5, "X1", "X0"), levels = levels(validation_data$cardio))


# Construir las matrices de confusi�n
confusion_matrix_logistic <- confusionMatrix(data = logistic_pred_binary, reference = validation_data$cardio)
confusion_matrix_tree <- confusionMatrix(data = tree_pred_binary, reference = validation_data$cardio)

# Mostrar las matrices de confusi�n
print(confusion_matrix_logistic)
print(confusion_matrix_tree)
