# Instalar y cargar dplyr, magrittr, rpart, pROC y rpart.plot si no están instalados
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("magrittr", quietly = TRUE)) {
  install.packages("magrittr")
}
if (!requireNamespace("rpart", quietly = TRUE)) {
  install.packages("rpart")
}
if (!requireNamespace("pROC", quietly = TRUE)) {
  install.packages("pROC")
}
if (!requireNamespace("rpart.plot", quietly = TRUE)) {
  install.packages("rpart.plot")
}
if (!requireNamespace("psych", quietly = TRUE)) {
  install.packages("psych")
}
library(dplyr)
library(magrittr)
library(rpart)
library(pROC)
library(rpart.plot)
library(psych)
library(ggplot2)


# Leer datos
heart_data <- read.csv("heart_data.csv")

# Eliminar las columnas index y id
heart_data <- heart_data[,-c(1,2)]

# Convertir edad de días a años
heart_data$age <- heart_data$age / 365.25 # .25 para los años bisiestos

# Crear una nueva columna para determinar si tiene presión arterial alta o baja
heart_data$high_bp <- ifelse(heart_data$ap_hi >= 140 | heart_data$ap_lo >= 90, 1, 0)

# Eliminar las columnas ap_hi y ap_lo
heart_data <- heart_data %>% select(-c("ap_hi", "ap_lo"))

#------------------------------------------------------------------------------#
# Análisis descriptivo                                                         #                                                
#------------------------------------------------------------------------------#

# Gráfico de matriz de dispersión
pairs(heart_data[, c('age', 'height', 'weight')])

# Matriz de correlaciones
corr_matrix <- cor(heart_data)
print(corr_matrix)

# Gráfico de matriz de correlaciones
psych::cor.plot(corr_matrix)

# Diagrama de dispersión entre variables
ggplot(heart_data, aes(x = age, y = weight)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Diagrama de dispersión entre edad y peso",
       x = "Edad",
       y = "Peso")

# Factorizar los datos binarios
heart_data$gender <- factor(heart_data$gender)
heart_data$cholesterol <- factor(heart_data$cholesterol, ordered = TRUE)
heart_data$gluc <- factor(heart_data$gluc, ordered = TRUE)
heart_data$smoke <- factor(heart_data$smoke)
heart_data$alco <- factor(heart_data$alco)
heart_data$active <- factor(heart_data$active)
heart_data$high_bp <- factor(heart_data$high_bp)

# Estandarizar variables
heart_data[, c('age', 'height', 'weight')] <- scale(heart_data[, c('age', 'height', 'weight')])

#------------------------------------------------------------------------------#
# Regresión Logística                                                         #                                                
#------------------------------------------------------------------------------#

# Regresión Logística
full_model <- glm(cardio ~ age + gender + height + weight + cholesterol + gluc + smoke
                  + alco + active + high_bp, data = heart_data, family = "binomial")

# Mostrar resumen del modelo con Odds Ratios
exp(cbind(OR = coef(full_model), confint(full_model)))

# Gráfico ROC
roc_curve <- roc(heart_data$cardio, predict(full_model, type = "response"))
plot(roc_curve, main = "Curva ROC", col = "blue", lwd = 2)

#------------------------------------------------------------------------------#
# Árboles de Decisión Binarios                                                #                                                
#------------------------------------------------------------------------------#

# Crear modelo de árbol de decisión con control de profundidad
tree_model <- rpart(high_bp ~ age + gender + height + weight + cholesterol + gluc + smoke
                    + alco + active, data = heart_data, method = "class", control = rpart.control(maxdepth = 3))

# Visualizar el árbol de decisión con rpart.plot
prp(tree_model, type = 2, extra = 1, main = "Árbol de Decisión Binario")
