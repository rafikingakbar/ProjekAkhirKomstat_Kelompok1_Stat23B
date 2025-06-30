# Load library
library(shiny)
library(shinythemes)
library(shinyWidgets)

# UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  navbarPage("Aplikasi Analisis Regresi",
             
             tabPanel("Upload Data",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file1", "Upload File (.csv)", accept = ".csv"),
                          tags$hr(),
                          
                          uiOutput("var_select_ui"),
                          uiOutput("indep_vars_ui"),
                          tags$hr(),
                          
                          uiOutput("x_type_ui"),
                          
                          # Tombol untuk menjalankan regresi
                          actionButton("run_regression", "Jalankan Regresi", class = "btn-primary")
                        ),
                        
                        mainPanel(
                          h4("Preview Data"),
                          tableOutput("table"),
                          tags$hr(),
                          h4("Hasil Regresi"),
                          verbatimTextOutput("regression_output")
                        )
                      )
             ),
             
             tabPanel("Uji Asumsi Residual",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Pastikan Anda telah menjalankan regresi terlebih dahulu.")
                        ),
                        mainPanel(
                          h4("Normalitas Residual"),
                          plotOutput("hist_resid"),
                          plotOutput("qq_resid"),
                          tags$hr(),
                          
                          h4("Independensi Residual"),
                          plotOutput("resid_vs_index")
                        )
                      )
             )
             
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive: baca data upload
  dataInput <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath)
  })
  
  # Tampilkan preview data
  output$table <- renderTable({
    head(dataInput(), 10)
  })
  
  # Pilih Y
  output$var_select_ui <- renderUI({
    req(dataInput())
    cols <- names(dataInput())
    selectInput("dep_var", "Pilih Variabel Dependen (Y):",
                choices = cols,
                selected = NULL)
  })
  
  # Pilih X
  output$indep_vars_ui <- renderUI({
    req(dataInput(), input$dep_var)
    cols <- names(dataInput())
    x_choices <- setdiff(cols, input$dep_var)
    
    pickerInput("indep_vars", "Pilih Variabel Independen (X):",
                choices = x_choices,
                selected = NULL,
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `live-search` = TRUE,
                  `none-selected-text` = "Belum dipilih"
                ))
  })
  
  # Pilih tipe untuk tiap X
  output$x_type_ui <- renderUI({
    req(input$indep_vars)
    x_vars <- input$indep_vars
    
    lapply(x_vars, function(var) {
      selectInput(
        inputId = paste0("xtype_", var),
        label = paste("Tipe Variabel", var, ":"),
        choices = c("Numerik", "Kategorik (Dummy)"),
        selected = "Numerik"
      )
    })
  })
  
  # Jalankan regresi saat tombol diklik
  observeEvent(input$run_regression, {
    req(dataInput(), input$dep_var, input$indep_vars)
    
    df <- dataInput()
    dep_var <- input$dep_var
    indep_vars <- input$indep_vars
    
    # Konversi tipe variabel X berdasarkan input pengguna
    for (var in indep_vars) {
      tipe <- input[[paste0("xtype_", var)]]
      
      if (!is.null(tipe)) {
        if (tipe == "Kategorik (Dummy)") {
          df[[var]] <- as.factor(df[[var]])
        } else if (tipe == "Numerik") {
          df[[var]] <- as.numeric(df[[var]])
        }
      }
    }
    
    # Buat formula regresi
    formula_str <- paste(dep_var, "~", paste(indep_vars, collapse = " + "))
    model <- lm(as.formula(formula_str), data = df)
    
    # Tampilkan hasil
    output$regression_output <- renderPrint({
      summary(model)
    })
  })
  
  
  ## SERVER BUAT TAB DUAAAAAAA ##
  # Reactive model disimpan agar bisa dipakai di tab asumsi
  model_lm <- reactiveVal()
  
  observeEvent(input$run_regression, {
    req(dataInput(), input$dep_var, input$indep_vars)
    
    df <- dataInput()
    dep_var <- input$dep_var
    indep_vars <- input$indep_vars
    
    # Konversi tipe variabel sesuai input pengguna
    for (var in indep_vars) {
      tipe <- input[[paste0("xtype_", var)]]
      if (!is.null(tipe)) {
        if (tipe == "Kategorik (Dummy)") {
          df[[var]] <- as.factor(df[[var]])
        } else if (tipe == "Numerik") {
          df[[var]] <- as.numeric(df[[var]])
        }
      }
    }
    
    # Buat model regresi
    formula_str <- paste(dep_var, "~", paste(indep_vars, collapse = " + "))
    model <- lm(as.formula(formula_str), data = df)
    
    # Simpan model ke reactiveVal
    model_lm(model)
    
    # Output hasil summary
    output$regression_output <- renderPrint({
      summary(model)
    })
  })
  
  # Plot histogram residual
  output$hist_resid <- renderPlot({
    req(model_lm())
    hist(resid(model_lm()), main = "Histogram Residual", xlab = "Residual", col = "skyblue", border = "white")
  })
  
  # QQ Plot residual
  output$qq_resid <- renderPlot({
    req(model_lm())
    qqnorm(resid(model_lm()))
    qqline(resid(model_lm()), col = "red")
  })
  
  # Residual vs Index
  output$resid_vs_index <- renderPlot({
    req(model_lm())
    plot(resid(model_lm()), type = "b", pch = 19,
         main = "Residual vs Index Observasi",
         xlab = "Index", ylab = "Residual",
         col = "darkgreen")
    abline(h = 0, lty = 2)
  })
  
}

# Run app
shinyApp(ui = ui, server = server)
