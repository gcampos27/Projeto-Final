
# treino_modelo.R

library(tidyverse)
library(caret)
library(randomForest)
library(MLmetrics)
library(mltools)
library(data.table)

# Ler base de dados
df <- read.csv("C:/Projeto II/PrevisãoAdaptabilidade/dados/BD_Projeto_II_modificado.csv", sep = ",")

# Preparação de dados
set.seed(123)
index <- createDataPartition(df$Adaptivity.Level, p = 0.8, list = FALSE)
train_data <- df[index, ]
test_data <- df[-index, ]

# Garantir que a variável alvo é fator
train_data$Adaptivity.Level <- as.factor(train_data$Adaptivity.Level)
test_data$Adaptivity.Level <- as.factor(test_data$Adaptivity.Level)

# Converter as colunas categóricas em fator
cat_cols <- c("Gender", "Education.Level", "Institution.Type", "IT.Student", 
              "Location", "Load_shedding", "Self.Lms", "Financial.Condition", 
              "Internet.Type", "Network.Type", "Class.Duration", "Device")

for (col in cat_cols) {
  train_data[[col]] <- as.factor(train_data[[col]])
  test_data[[col]] <- as.factor(test_data[[col]])
}

# Treinar modelo Random Forest
rf_model <- randomForest(Adaptivity.Level ~ ., 
                         data = train_data, 
                         ntree = 500, 
                         mtry = 9, 
                         importance = TRUE)

# Salvar modelo
saveRDS(rf_model, "C:/Projeto II/PrevisãoAdaptabilidade/modelo/modelo.rds")

# Salvar novamente se quiser
saveRDS(rf_model, "modelo_rf.rds")
