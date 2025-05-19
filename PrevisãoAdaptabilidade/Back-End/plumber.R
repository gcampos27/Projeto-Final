library(plumber)
library(jsonlite)
library(sodium)
library(jose)
library(randomForest)

modelo_rf <- readRDS("C:/Projeto II/PrevisãoAdaptabilidade/modelo/modelo.rds")
source("C:/Projeto II/PrevisãoAdaptabilidade/utils.R")

# CORS
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

# Health-check
#* @get /ping
#* @serializer json
function() {
  list(status = "ok", time = Sys.time())
}

# Signup
#* @post /signup
#* @serializer json
function(req, res) {
  body <- tryCatch(fromJSON(req$postBody), error = function(e) {
    res$status <- 400; return(list(error = "JSON inválido"))
  })
  resultado <- registar_utilizador(body$email, body$password)
  if (!resultado$sucesso) {
    res$status <- resultado$status
    return(list(error = resultado$erro))
  }
  res$status <- 201
  list(message = "Conta criada com sucesso")
}

# Login
#* @post /login
#* @serializer json
function(req, res) {
  body <- tryCatch(fromJSON(req$postBody), error = function(e) {
    res$status <- 400; return(list(error = "JSON inválido"))
  })
  if (is.null(body$email) || is.null(body$password)) {
    res$status <- 400
    return(list(error = "Faltam campos obrigatórios: email e/ou password"))
  }
  token <- paste0("token_", body$email)  # Simples para teste
  list(token = token)
}

#* @post /predict
#* @serializer json
function(req, res) {
  body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) {
    res$status <- 400
    return(list(error = "Erro ao ler JSON", detalhe = e$message))
  })
  
  required_fields <- c("Gender", "Age", "Education.Level", "Institution.Type", "IT.Student",
                       "Location", "Load_shedding", "Self.Lms", "Financial.Condition",
                       "Internet.Type", "Network.Type", "Class.Duration", "Device")
  
  missing_fields <- required_fields[!required_fields %in% names(body)]
  if (length(missing_fields) > 0) {
    res$status <- 400
    return(list(error = paste("Campos em falta:", paste(missing_fields, collapse = ", "))))
  }
  
  input <- data.frame(
    Gender              = factor(as.character(body$Gender), levels = c("Boy", "Girl")),
    Age                 = as.integer(body$Age),
    Education.Level     = factor(as.character(body$Education.Level), levels = c("1", "2", "3")),
    Institution.Type    = factor(as.character(body$Institution.Type), levels = c("0", "1")),
    IT.Student          = factor(as.character(body$IT.Student), levels = c("0", "1")),
    Location            = factor(as.character(body$Location), levels = c("0", "1")),
    Load.shedding       = factor(as.character(body$Load_shedding), levels = c("0", "1")),
    Self.Lms            = factor(as.character(body$Self.Lms), levels = c("0", "1")),
    Financial.Condition = factor(as.character(body$Financial.Condition), levels = c("1", "2", "3")),
    Internet.Type       = factor(as.character(body$Internet.Type), levels = c("0", "1")),
    Network.Type        = factor(as.character(body$Network.Type), levels = c("0", "1", "2")),
    Class.Duration      = factor(as.character(body$Class.Duration), levels = c("0", "1", "2", "4")),
    Device              = factor(as.character(body$Device), levels = c("1", "2", "3"))
  )
  
  print("=== Dados recebidos ===")
  print(input)
  str(input)
  
  tryCatch({
    pred <- predict(modelo_rf, input, type = "response")
    list(predito = as.character(pred))
  }, error = function(e) {
    res$status <- 500
    list(error = "Erro na previsão", detalhe = e$message)
  })
}
