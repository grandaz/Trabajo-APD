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

# Elimina las columnas ap_hi y ap_lo
heart_data <- heart_data %>% select(-c("ap_hi", "ap_lo"))


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
        main = "Relación entre Edad y Enfermedad cardiovascular")

boxplot(weight ~ cardio, data = heart_data, col = c("lightblue", "lightyellow"), 
        main = "Relación entre Edad y Enfermedad cardiovascular")

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
heart_data[, c('age', 'height', 'weight')] <- scale(heart_data[, c('age', 'height', 'weight')])


#------------------------------------------------------------------------------#
# Regresión Logística                                                          #                                                
#------------------------------------------------------------------------------#

# Factorizar los variables binarias y categóricas
heart_data$gender <- factor(heart_data$gender)
heart_data$cholesterol <- factor(heart_data$cholesterol, ordered = TRUE)
heart_data$gluc <- factor(heart_data$gluc, ordered = TRUE)
heart_data$smoke <- factor(heart_data$smoke)
heart_data$alco <- factor(heart_data$alco)
heart_data$active <- factor(heart_data$active)
#heart_data$cardio <- factor(heart_data$cardio)  # Variable dependiente
heart_data$high_bp <- factor(heart_data$high_bp)








