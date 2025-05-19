library(sodium)
library(jsonlite)

caminho_users <- "C:/Projeto II/PrevisãoAdaptabilidade/Back-End/users.json"

ler_users <- function() {
  if (!file.exists(caminho_users)) return(data.frame(email = character(), hash = character(), stringsAsFactors = FALSE))
  fromJSON(caminho_users)
}

escrever_users <- function(users) {
  write(toJSON(users, pretty = TRUE, auto_unbox = TRUE), caminho_users)
}

email_valido <- function(email) {
  grepl("^\\S+@\\S+\\.\\S+$", email)
}

registar_utilizador <- function(email, pw) {
  if (!email_valido(email)) {
    return(list(sucesso = FALSE, status = 400, erro = "Email inválido"))
  }
  if (nchar(pw) < 6) {
    return(list(sucesso = FALSE, status = 400, erro = "Password deve ter ≥6 caracteres"))
  }
  
  users <- ler_users()
  if (any(users$email == email)) {
    return(list(sucesso = FALSE, status = 400, erro = "Email já registado"))
  }
  
  hash <- sodium::password_store(pw)  # já é um a string de 101 caracteres
  
  novo <- data.frame(email = email, hash = hash, stringsAsFactors = FALSE)
  users <- rbind(users, novo)
  escrever_users(users)
  
  list(sucesso = TRUE)
}

validar_login <- function(email, pw) {
  users <- ler_users()
  utilizador <- users[users$email == email, ]
  if (nrow(utilizador) == 0) return(FALSE)
  
  hash_guardado <- utilizador$hash[1]
  sodium::password_verify(pw, hash_guardado)
}

fazer_previsao <- function(dados, modelo) {
  # Converter lista para data.frame
  df <- as.data.frame(dados, stringsAsFactors = TRUE)
  
  # Garantir que os níveis das variáveis categóricas estão corretos
  colunas_modelo <- names(modelo$forest$xlevels)  # para randomForest
  for (col in colunas_modelo) {
    if (is.character(df[[col]])) {
      df[[col]] <- factor(df[[col]], levels = modelo$forest$xlevels[[col]])
    }
  }
  
  # Verificar se colunas estão completas
  if (!all(colunas_modelo %in% names(df))) {
    return(list(sucesso = FALSE, status = 400, erro = "Campos em falta ou com nomes errados"))
  }
  
  fazer_previsao <- function(dados, modelo) {
    required_vars <- c("Gender", "Age", "Education.Level", "Institution.Type",
                       "IT.Student", "Location", "Load_shedding", "Financial.Condition",
                       "Internet.Type", "Network.Type", "Class.Duration", "Self.Lms", "Device")
    
    if (!all(required_vars %in% names(dados))) {
      return(list(sucesso = FALSE, status = 400, erro = "Variáveis em falta ou com nomes errados"))
    }
    
    # Converte para data.frame e garante que os fatores estão certos
    df <- as.data.frame(dados)
    df <- df[required_vars]
    
    # Faz previsão
    pred <- predict(modelo, df)
    list(sucesso = TRUE, previsao = as.character(pred))
  }
}
