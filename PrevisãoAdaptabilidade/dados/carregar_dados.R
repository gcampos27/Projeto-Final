
train_data <- read.csv("C:/Projeto II/PrevisãoAdaptabilidade/modelo/train_data.csv")

# Converter colunas categóricas para fatores
categoricas <- c("Gender", "Institution.Type", "IT.Student", "Location",
                 "Load_shedding", "Financial.Condition", "Internet.Type", "Self.Lms")

train_data[categoricas] <- lapply(train_data[categoricas], factor)
