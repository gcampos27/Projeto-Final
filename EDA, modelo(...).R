library(tidyverse)
library(dplyr) 
library(skimr)
library(DataExplorer)
library(readxl)
library(openxlsx)
library(GGally)
library(e1071)
library(caret)
library(randomForest)
library(readxl)
library(plumber)


df <- read.xlsx("C:/Users/Utilizador/OneDrive - Ensino Lusófona/Desktop/Projeto II/BD Projeto II.xlsx")

str(df)
summary(df)

# Verificar valores faltantes
colSums(is.na(df))


# Histograma para todas as variáveis numéricas
df %>%
  select(where(is.numeric)) %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.6) +
  facet_wrap(~key, scales = "free") +
  theme_minimal() +
  labs(title = "Distribuição das Variáveis Numéricas")

# Boxplot para cada variável numérica
df %>%
  select(where(is.numeric)) %>%
  gather() %>%
  ggplot(aes(y = value)) +
  geom_boxplot(fill = "red", alpha = 0.6) +
  facet_wrap(~key, scales = "free") +
  theme_minimal() +
  labs(title = "Detecção de Outliers nas Variáveis Numéricas")

# Matriz de correlação entre as variáveis numéricas
ggcorr(df, label = TRUE, label_size = 5)

ggplot(df, aes(x = Age, y = Education.Level)) +
  geom_jitter(aes(color = factor(Adaptivity.Level)), width = 0.2, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Relação entre Idade e Tipo de Instituição",
       x = "Idade",
       y = "Tipo de Instituição",
       color = "Nível de Adaptabilidade")

ggplot(df, aes(x = Education.Level, y = Institution.Type)) +
  geom_jitter(aes(color = factor(Adaptivity.Level)), width = 0.2, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Relação entre Nível de educação e Tipo de Instituição",
       x = "Nível de educação",
       y = "Tipo de Instituição",
       color = "Nível de Adaptabilidade")

set.seed(123)  # Para garantir que os resultados sejam sempre os mesmos

# Criar índice para dividir os dados
index <- createDataPartition(df$Adaptivity.Level, p = 0.8, list = FALSE)

# Criar os conjuntos de treino e teste
train_data <- df[index, ]  # 80% dos dados para treino
test_data <- df[-index, ]  # 20% dos dados para teste

# Verificar quantas linhas tem cada conjunto
dim(train_data)
dim(test_data)

colnames(df)

str(train_data$Adaptivity.Level)

train_data$Adaptivity.Level <- as.factor(train_data$Adaptivity.Level)
test_data$Adaptivity.Level <- as.factor(test_data$Adaptivity.Level)

str(train_data$`Adaptivity.Level`)


colnames(train_data)[colnames(train_data) == "Load.shedding"] <- "Load_shedding"
colnames(test_data)[colnames(test_data) == "Load.shedding"] <- "Load_shedding"

summary(train_data$`Load-shedding`)
str(train_data$`Load-shedding`)

train_data$`Load-shedding` <- as.factor(train_data$`Load-shedding`)
test_data$`Load-shedding` <- as.factor(test_data$`Load-shedding`)


rf_model <- randomForest(Adaptivity.Level ~ . -`Load-shedding`,  
                         data = train_data,  
                         ntree = 500,  
                         mtry = 3,  
                         importance = TRUE)

'rm(rf_model)'

rf_predictions <- predict(rf_model, test_data)
confusionMatrix(rf_predictions, test_data$Adaptivity.Level)

importance(rf_model)

# Converter a importância das variáveis num data frame
var_imp <- as.data.frame(importance(rf_model))
var_imp$Variable <- rownames(var_imp)

# Ordenar por importância e criar o gráfico
library(ggplot2)
ggplot(var_imp, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "steelblue") +
  coord_flip() +  # Inverter para melhor visualização
  theme_minimal() +
  labs(title = "Importância das Variáveis no Modelo Random Forest",
       x = "Variáveis",
       y = "Mean Decrease Gini")

save(rf_model, file = "rf_model.RData")


















