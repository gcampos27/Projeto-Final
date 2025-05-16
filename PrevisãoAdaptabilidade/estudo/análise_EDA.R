library(tidyverse)
library(GGally)

df <- read.csv("C:/Projeto II/PrevisãoAdaptabilidade/dados/BD_Projeto_II_modificado.csv", sep = ",")

# Verificar estrutura
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

ggplot(df, aes(x = Gender)) +
  geom_bar(fill = "#69b3a2") +
  labs(title = "Distribuição por Género")


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

ggplot(df, aes(x = Gender, fill = as.factor(Adaptivity.Level))) +
  geom_bar(position = "dodge") +
  labs(title = "Género vs Nível de Adaptabilidade", fill = "Adaptivity Level")