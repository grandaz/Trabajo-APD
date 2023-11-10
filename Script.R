heart_data <- read.csv("heart_data.csv")

# Elimina la columna index y id
heart_data <- heart_data[,-c(1,2)]

# Pasa la edad de días a años
heart_data$age <- heart_data$age / 365.25 # .25 para los años bisiestos
#heart_data$age <- round(heart_data$age / 365.25)     # Redondea las edades
#heart_data$age <- floor(heart_data$age / 365.25)     # Redondea hacia abajo
#heart_data$age <- ceiling(heart_data$age / 365.25)   # Redondea hacia arriba

# Crea una nueva columna para determinar si tiene presión arterial alta o baja
heart_data$high_bp <- ifelse(heart_data$ap_hi >= 140 | heart_data$ap_lo >= 90, 1, 0)


#install.packages("dplyr")
library(dplyr)

# Elimina las columnas ap_hi y ap_lo
heart_data <- heart_data %>% select(-c("ap_hi", "ap_lo"))

# Factorizar los datos binarios
heart_data$gender <- factor(heart_data$gender)
heart_data$cholesterol <- factor(heart_data$cholesterol, ordered = TRUE)
heart_data$gluc <- factor(heart_data$gluc, ordered = TRUE)
heart_data$smoke <- factor(heart_data$smoke)
heart_data$alco <- factor(heart_data$alco)
heart_data$active <- factor(heart_data$active)
#heart_data$cardio <- factor(heart_data$cardio)
heart_data$high_bp <- factor(heart_data$high_bp)

# Estandarizar variables
heart_data[, c('age', 'height', 'weight')] <- scale(heart_data[, c('age', 'height', 'weight')])

#------------------------------------------------------------------------------#
# Análisis descriptivo                                                         #                                                
#------------------------------------------------------------------------------#
# Relación con las variables predictoras
par(mfrow=c(1,3))
plot(cardio ~ age,data =  heart_data)
plot(cardio ~ gender,data =  heart_data)
plot(cardio ~ height,data =  heart_data)
par(mfrow=c(1,1))

library(ggplot2)
library(psych)
# Matriz de Varianza Covarianza
cov(heart_data)

# Matriz de correlaciones
corr.test(heart_data)
cor.plot(cor(heart_data))


#------------------------------------------------------------------------------#
# Regresión Lineal Múltiple (RLM)                                                #                                                
#------------------------------------------------------------------------------#

# Modelo completo
full.model  <- glm(cardio ~ age + gender + height + weight + cholesterol + gluc + smoke
                  + alco + active + high_bp, data = heart_data, family = "binomial")
summary(full.model)






