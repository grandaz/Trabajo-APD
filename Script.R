heart_data <- read.csv("heart_data.csv")

# Elimina la columna index y id
heart_data <- heart_data[,-c(1,2)]

# Pasa la edad de días a años  (.25 para los años bisiestos)
#heart_data$age <- heart_data$age / 365.25 
#heart_data$age <- round(heart_data$age / 365.25)     # Redondea las edades
heart_data$age <- floor(heart_data$age / 365.25)     # Redondea hacia abajo
#heart_data$age <- ceiling(heart_data$age / 365.25)   # Redondea hacia arriba

# Crea una nueva columna para determinar si tiene presión arterial alta o baja
heart_data$high_bp <- ifelse(heart_data$ap_hi >= 140 | heart_data$ap_lo >= 90, 1, 0)


library(dplyr)

# Elimina las columnas ap_hi y ap_lo (después se decidió eliminar las variables altura y género)
heart_data <- heart_data %>% select(-c("ap_hi", "ap_lo", "height", "gender"))


#------------------------------------------------------------------------------#
# Análisis descriptivo                                                         #                                                
#------------------------------------------------------------------------------#

# 1. Resumen de los datos
summary(heart_data)


# 2. Comprobación de valores faltantes
sapply(heart_data, function(x) sum(is.na(x)))


# 3. Distribución de la variable dependiente
barplot(table(heart_data$cardio), 
        col = c("lightblue", "black"), 
        main = "Distribución de la Variable Dependiente")


# 4. Distribución de las variables independientes
hist(heart_data$age, main = "Distribución de Edad", 
     xlab = "Edad", col = "lightblue", border = "black")

barplot(table(heart_data$gender), 
        col = c("lightblue", "lightyellow"), 
        main = "Distribución de Género")

hist(heart_data$height, main = "Distribución de Altura", 
     xlab = "Altura", col = "lightblue", border = "black")

hist(heart_data$weight, main = "Distribución de Peso", 
     xlab = "Peso", col = "lightblue", border = "black")

hist(heart_data$cholesterol, 
     main = "Distribución de nivel de colesterol", 
     xlab = "Nivel de colesterol", col = "lightblue", border = "black")

hist(heart_data$gluc, main = "Distribución de nivel de glucosa", 
     xlab = "Nivel de glucosa", col = "lightblue", border = "black")

barplot(table(heart_data$smoke), 
        col = c("lightblue", "lightyellow"), 
        main = "Distribución de Fumadores")

barplot(table(heart_data$alco), 
        col = c("lightblue", "lightyellow"), 
        main = "Distribución de consumidores de alcohol")

barplot(table(heart_data$active), 
        col = c("lightblue", "lightyellow"), 
        main = "Distribución de Actividad Física")

barplot(table(heart_data$high_bp), 
        col = c("lightblue", "lightyellow"), 
        main = "Distribución de presión arterial alta")


# 5. Correlación entre variables
cor(heart_data)
corr.test(heart_data)
cor.plot(cor(heart_data))


# 6. Análisis bivariado
boxplot(age ~ cardio, data = heart_data, col = c("lightblue", "lightyellow"), 
        main = "Relación entre Edad y Enfermedad cardiovascular")

barplot(table(heart_data$gender, heart_data$cardio), 
        col = c("lightblue", "lightyellow"), 
        legend = TRUE, beside = TRUE, 
        main = "Relación entre Género y Enfermedad cardiovascular")

boxplot(height ~ cardio, data = heart_data, col = c("lightblue", "lightyellow"), 
        main = "Relación entre Altura y Enfermedad cardiovascular")

boxplot(weight ~ cardio, data = heart_data, col = c("lightblue", "lightyellow"), 
        main = "Relación entre Peso y Enfermedad cardiovascular")

barplot(table(heart_data$cholesterol, heart_data$cardio), 
        col = c("lightblue", "lightyellow", "lightgreen"), 
        legend = TRUE, beside = TRUE, 
        main = "Relación entre Nivel de colesterol y Enfermedad cardiovascular")

barplot(table(heart_data$gluc, heart_data$cardio), 
        col = c("lightblue", "lightyellow", "lightgreen"), 
        legend = TRUE, beside = TRUE, 
        main = "Relación entre Nivel de glucosa y Enfermedad cardiovascular")

barplot(table(heart_data$smoke, heart_data$cardio), 
        col = c("lightblue", "lightyellow"), 
        legend = TRUE, beside = TRUE, 
        main = "Relación entre Fumadores y Enfermedad cardiovascular")

barplot(table(heart_data$alco, heart_data$cardio), 
        col = c("lightblue", "lightyellow"), 
        legend = TRUE, beside = TRUE, 
        main = "Relación entre Consumidores de alcohol y Enfermedad cardiovascular")

barplot(table(heart_data$active, heart_data$cardio), 
        col = c("lightblue", "lightyellow"), 
        legend = TRUE, beside = TRUE, 
        main = "Relación entre Actividad física y Enfermedad cardiovascular")

barplot(table(heart_data$high_bp, heart_data$cardio), 
        col = c("lightblue", "lightyellow"), 
        legend = TRUE, beside = TRUE, 
        main = "Relación entre Presión arterial alta y Enfermedad cardiovascular")


# 7. Manejo de outliers
boxplot(heart_data$age, main = "Boxplot de Variable Edad")
boxplot(heart_data$height, main = "Boxplot de Variable Altura")
boxplot(heart_data$weight, main = "Boxplot de Variable Peso")


# 8. Normalización/Estandarización de las variables para reducir influencia de outliers
heart_data[, c('age','weight')] <- scale(heart_data[, c('age','weight')])


# Factorizar los variables binarias y categóricas
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
# Validación Cruzada (CV K-Fold)                                               #                                                
#------------------------------------------------------------------------------#

# Establecer semilla para reproducibilidad
set.seed(123)

# División de los datos
library(caret)

# Crea una partición (70% entrenamiento, 15% validación, 15% prueba)
indices_entrenamiento <- createDataPartition(y = heart_data$cardio, p = 0.7, list = FALSE)
train_data <- heart_data[indices_entrenamiento, ]
conjunto_temporal <- heart_data[-indices_entrenamiento, ]

# Crear partición en el conjunto temporal para obtener validación y prueba (50% cada uno)
indices_temporales <- createDataPartition(y = conjunto_temporal$cardio, p = 0.5, list = FALSE)
validation_data <- conjunto_temporal[indices_temporales, ]
test_data <- conjunto_temporal[-indices_temporales, ]

# Definimos indicadores
# Puede ser "Accuracy",   "logLoss", "ROC",   "Kappa"
metrica <- "ROC"

# Definimos parámetros para la Validación Cruzada
particiones  <- 5
repeticiones <- 10

# Definir la configuración de control para la Validación Cruzada
ctrl <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones,
                              #seeds = seeds,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

#------------------------------------------------------------------------------#
# Entrenamos el modelo de Regresión Logística                                  #                                                
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
# Entrenamos el modelo de Árbol de Decisión                                    #                                                
#------------------------------------------------------------------------------#

# Define la cuadrícula de ajuste para el parámetro cp
hiperparametros_tree <- expand.grid(cp = seq(0.01, 0.5, by = 0.01))

# Entrenar el modelo de árbol de decisión
tree_model <- train(cardio ~ ., data = train_data,
                     method = "rpart",
                     tuneGrid = hiperparametros_tree,
                     metric = metrica,
                     trControl = ctrl)
tree_model
tree_model$results
tree_model$resample
summary(tree_model$finalModel)

# Visualizar el árbol de decisión con rpart.plot
library(rpart.plot)
prp(tree_model$finalModel, type = 2, extra = 1, main = "Árbol de Decisión Binario")

library(partykit)
plot(as.party(tree_model$finalModel), tp_args = list(id = FALSE))


#------------------------------------------------------------------------------#
# Validación de los modelos                                                    #                                                
#------------------------------------------------------------------------------#

library(pROC)

# Predecir las probabilidades en los datos de validación
logistic_pred <- predict(logistic_model, newdata = validation_data, type = "prob")[,2]
tree_pred <- predict(tree_model, newdata = validation_data, type = "prob")[,2]

# Crear objetos roc para calcular el área bajo la curva ROC
logistic_roc <- roc(validation_data$cardio, logistic_pred)
tree_roc <- roc(validation_data$cardio, tree_pred)

# Comparar las áreas bajo la curva ROC
roc.test(logistic_roc, tree_roc)

# Visualizar las curvas ROC
plot(logistic_roc, col = "blue", main = "Curvas ROC de Modelos",
     lwd = 2, col.main = "black", ylim = c(0, 1))
lines(tree_roc, col = "red", lwd = 2)
legend("bottomright", legend = c("Regresión Logística", "Árbol de Decisión"),
       col = c("blue", "red"), lwd = 2)

# Calcular el AUC para ambos modelos
auc_logistic <- auc(logistic_roc)
auc_tree <- auc(tree_roc)

# Imprimir los resultados
cat("AUC para Regresión Logística:", auc_logistic, "\n")
cat("AUC para Árbol de Decisión:", auc_tree, "\n")


# Factorizar los resultados de la predicción según el nombre
logistic_pred_binary <- factor(ifelse(logistic_pred > 0.5, "X1", "X0"), levels = levels(validation_data$cardio))
tree_pred_binary <- factor(ifelse(tree_pred > 0.5, "X1", "X0"), levels = levels(validation_data$cardio))


# Construir las matrices de confusión
confusion_matrix_logistic <- confusionMatrix(data = logistic_pred_binary, reference = validation_data$cardio)
confusion_matrix_tree <- confusionMatrix(data = tree_pred_binary, reference = validation_data$cardio)

# Mostrar las matrices de confusión
print(confusion_matrix_logistic)
print(confusion_matrix_tree)
