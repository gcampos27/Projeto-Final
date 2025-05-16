# app.R

library(shiny)
library(bslib)
library(randomForest)
library(DT)

# 1) Caminhos
model_path   <- "C:/Projeto II/PrevisãoAdaptabilidade/modelo/modelo.rds"
train_path   <- "C:/Projeto II/PrevisãoAdaptabilidade/modelo/train_data.csv"
history_path <- "C:/Projeto II/PrevisãoAdaptabilidade/modelo/history.csv"

train_data <- read.csv(train_path, stringsAsFactors = FALSE)

# 1) make.names() converte “Load-shedding” → “Load.shedding”
names(train_data) <- make.names(names(train_data))
# 2) gsub transforma todos os pontos em underscore
names(train_data) <- gsub("\\.", "_", names(train_data))
# Agora tens colunas como Load_shedding, Education_Level, Class_Duration etc.


factor_cols <- c(
  "Gender", "Institution_Type", "IT_Student", "Location",
  "Load_shedding", "Financial_Condition",
  "Internet_Type", "Self_Lms"
)
factor_cols <- intersect(factor_cols, names(train_data))
train_data[factor_cols] <- lapply(train_data[factor_cols], factor)

# 4) Extrair os levels para usar no UI
gender_lv        <- levels(train_data$Gender)
inst_lv          <- levels(train_data$Institution_Type)
it_lv            <- levels(train_data$IT_Student)
loc_lv           <- levels(train_data$Location)
load_lv          <- levels(train_data$Load_shedding)
selflms_lv       <- levels(train_data$Self_Lms)
fincond_lv       <- levels(train_data$Financial_Condition)
internet_lv      <- levels(train_data$Internet_Type)


# 5) Carregar o modelo
modelo_rf <- readRDS(model_path)

# 6) UI
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("📊 Sistema de Previsão de Adaptabilidade"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Gender", "Género:", choices = list("Rapaz" = 0, "Rapariga" = 1), selected = 0),
      numericInput("Age",                "Idade:",                      18, min = 3,  max = 28),
      numericInput("Education_Level",    "Nível de Educação (1–3):",    1,  min = 1,  max = 3),
      selectInput("Institution_Type", "Tipo de Instituição:", choices = inst_lv),
      selectInput("IT_Student",       "É Estudante de Informática?", choices = it_lv),
      selectInput("Location",            "Localização:",                choices = loc_lv),
      selectInput(inputId = "Load_shedding", label   = "Cortes de eletricidade?", choices = load_lv, selected = load_lv[1]),
      selectInput("Self_Lms",         "Usa LMS próprio?",           choices = selflms_lv),
      selectInput("Financial_Condition","Condições Financeiras:",    choices = fincond_lv),
      selectInput("Internet_Type",    "Tipo de Internet:",          choices = internet_lv),
      numericInput("Network_Type",       "Tipo de Rede (0=2G–2=4G):",   1,  min = 0,  max = 2),
      numericInput("Class_Duration",     "Duração das aulas (0–4):",    2,  min = 0,  max = 4),
      numericInput("Device",             "Dispositivo (1–3):",          1,  min = 1,  max = 3),
      actionButton("predict", "📊 Prever", class = "btn btn-primary w-100")
    ),
    mainPanel(
      uiOutput("result_box"),
      hr(),
      h4("📋 Histórico de Previsões"),
      DTOutput("history")
    )
  )
)

# 7) Server
server <- function(input, output, session) {
  
  rv_pred <- reactiveVal(NULL)
  
  observeEvent(input$predict, {
    
    # ————————— 0) Mapear Age e Class_Duration segundo dicionário —————————
    age_code <- if (input$Age <= 5) 3 else if (input$Age <= 10) 8 else if (input$Age <= 15) 13 else if (input$Age <= 20) 18 else if (input$Age <= 25) 23 else 28
    cd_code  <- if (input$Class_Duration == 0) 0 else if (input$Class_Duration <= 3) 2 else 4
    # ——————————————————————————————————————————————————————————————
    
    # 1) nomes de coluna que o modelo espera
    model_names <- names(modelo_rf$forest$xlevels)
    
    # 2) valores brutos na mesma ordem
    vals <- list(
      input$Gender,           # Género (código 0/1)
      age_code,               # Idade mapeada
      input$Education_Level,  # Educação (1-3)
      input$Institution_Type, # Inst. Type (0/1)
      input$IT_Student,       # IT.Student (0/1)
      input$Location,         # Local (0/1)
      input$Load_shedding,    # Load_shedding (“0”/“1”)
      input$Financial_Condition,# Fin. Cond. (1-3)
      input$Internet_Type,    # Internet Type (0/1)
      input$Network_Type,     # Network Type (0/1/2)
      cd_code,                # Class Duration mapeada
      input$Self_Lms,         # Self.Lms (0/1)
      input$Device            # Device (1-3)
    )
    
    # 3) Montar data.frame com tipos originais
    novo_dado <- as.data.frame(setNames(vals, model_names), stringsAsFactors = FALSE)
    
    # 4) Converter *apenas* Load_shedding como factor (e deixar tudo o resto numérico)
    load_lv <- modelo_rf$forest$xlevels[["Load_shedding"]]
    novo_dado$Load_shedding <- factor(
      as.character(novo_dado$Load_shedding),
      levels = modelo_rf$forest$xlevels[["Load_shedding"]]
    )
    
    for(col in setdiff(model_names, "Load_shedding")) {
      novo_dado[[col]] <- as.numeric(novo_dado[[col]])
    }
    # todas as outras colunas em model_names são numéricas:
    num_cols <- setdiff(model_names, "Load_shedding")
    for(col in num_cols) {
      novo_dado[[col]] <- as.numeric(novo_dado[[col]])
    }
    
    # 5) Debug opcional: verificar NAs
    cat("=== NAs em novo_dado ===\n"); print(colSums(is.na(novo_dado))); cat("=======================\n")
    
    # 6) Prever
    pred <- predict(modelo_rf, newdata = novo_dado)
    
    rv_pred(predict(modelo_rf, newdata = novo_dado))
    
    cat(">>> DEBUG: pred =", pred, "\n")
    
    
    
    
    # 7) Mostrar resultado
    output$result_box <- renderUI({
      req(rv_pred())  # só mostra depois de clicar Prever
      tags$div(
        class = "alert alert-success text-center",
        tags$h4("📝 Resultado da Previsão:"),
        tags$p(paste("Nível de adaptabilidade previsto:", rv_pred()))
      )
    })
    
    # 8) Gravar no histórico
    entry <- cbind(
      novo_dado,
      Prediction = rv_pred(),
      Time       = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )
    if (!file.exists(history_path)) {
      write.table(entry, history_path, sep = ",",
                  row.names = FALSE, col.names = TRUE)
    } else {
      write.table(entry, history_path, sep = ",",
                  row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    
    output$result_box <- renderUI({
      req(rv_pred())  # só mostra depois de clicares “Prever”
      tags$div(
        class = "alert alert-success text-center",
        tags$h4("📝 Resultado da Previsão:"),
        tags$p(paste("Nível de adaptabilidade previsto:", rv_pred()))
      )
    })
    
    
    # 9) Atualizar tabela
    output$history <- renderDT({
      dat <- read.csv(history_path, stringsAsFactors = FALSE)
      datatable(dat, options = list(pageLength = 5, scrollX = TRUE))
    })
  })
  
  # Inicialização do histórico
  output$history <- renderDT({
    if (!file.exists(history_path)) {
      datatable(data.frame(Mensagem = "Ainda não há previsões"),
                options = list(dom = 't'))
    } else {
      dat <- read.csv(history_path, stringsAsFactors = FALSE)
      datatable(dat, options = list(pageLength = 5, scrollX = TRUE))
    }
  })
}

# 8) Run the app
shinyApp(ui, server)
