library(shiny)
library(shinyWidgets)
library(shinydashboard)

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Analisis Regresi"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "home", icon = icon("home")),
      menuItem("Unggah Data", tabName = "upload", icon = icon("file-upload")),
      menuItem("Pemodelan", tabName = "model", icon = icon("chart-line")),
      menuItem("Uji Asumsi Klasik", tabName = "asumsi", icon = icon("check-double"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
    body, .content-wrapper {
      background-image: url('https://images.pexels.com/photos/3183151/pexels-photo-3183151.jpeg');
      background-size: cover;
      background-repeat: no-repeat;
      background-attachment: fixed;
      background-position: center;
      font-family: 'Segoe UI', sans-serif;
    }

    .home-box, .box {
      background-color: rgba(255, 255, 255, 0.92) !important;
      box-shadow: 0 4px 10px rgba(0, 0, 0, 0.1);
      border-radius: 10px;
    }

    h3, h4 {
      font-weight: 600;
      color: #2c3e50;
    }

    p, li {
      font-size: 15px;
      color: #333;
    }

    ol, ul {
      padding-left: 20px;
    }

    .table-responsive {
      overflow-x: auto;
      width: 100%;
    }
  "))
    )
    ,
    
    tabItems(
      
      # BERANDA
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            class = "home-box",
            title = tagList(icon("info-circle"), "Selamat Datang di Aplikasi Analisis Regresi"),
            
            div(style = "text-align:center",
                tags$img(
                  src = "https://cdn-icons-png.flaticon.com/512/921/921591.png",
                  height = "100px"
                )
            ),
            
            br(),
            p("Aplikasi ini dirancang untuk memudahkan pengguna khususnya pelajar, peneliti, maupun praktisi dalam melakukan analisis regresi secara cepat, interaktif, dan tetap dapat dipahami hasilnya secara mendalam."),
            p("Analisis regresi merupakan salah satu metode statistik yang digunakan untuk memahami dan memprediksi hubungan antara variabel dependen (target) dengan satu atau lebih variabel independen (prediktor). Model ini sangat berguna dalam berbagai bidang seperti ekonomi, teknik, kesehatan, dan ilmu sosial."),
            p("Aplikasi ini juga mendukung regresi dengan variabel dummy, yaitu metode untuk mengakomodasi variabel kategorik seperti jenis kelamin, musim, atau wilayah. Variabel kategorik ini akan dikonversi menjadi bentuk numerik (dummy variable) agar dapat dimasukkan dalam model regresi linear."),
            p("Dengan tampilan antarmuka yang ramah pengguna, Anda tidak perlu menulis kode untuk melakukan perhitungan, namun tetap dapat melihat hasil regresi dalam bentuk persamaan matematis, ringkasan statistik, serta pengujian asumsi-asumsi dasar regresi."),
            
            tags$hr(),
            
            h4(icon("list-ol"), "Langkah-langkah Pengujian:"),
            HTML(
              "<ol>
                <li><b>Unggah Data:</b> Upload file CSV yang akan dianalisis.</li>
                <li><b>Pemodelan:</b> Buat model regresi linear untuk memprediksi variabel target.</li>
                <li><b>Uji Asumsi Klasik:</b> Cek normalitas residual, multikolinearitas, dan independensi.</li>
              </ol>"
            ),
            
            h4(icon("info"), "Penjelasan Analisis:"),
            HTML(
              "<ul>
                <li><b>Pemodelan:</b> Memprediksi variabel Y berdasarkan X menggunakan metode regresi linear.</li>
                <li><b>Asumsi Klasik:</b> Validasi syarat seperti normalitas, multikolinearitas, dan homoskedastisitas agar hasil regresi dapat diinterpretasikan dengan benar.</li>
              </ul>"
            )
          )
        )
      ),
      
      # UNGGAH DATA
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            title = tagList(icon("file-upload"), "Unggah Data"),
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            
            fileInput("file1", "Upload File (.csv)", accept = ".csv"),
            tags$hr(),
            uiOutput("var_select_ui"),
            uiOutput("indep_vars_ui"),
            tags$hr(),
            uiOutput("x_type_ui"),
            tags$hr(),
            actionButton("run_regression", "Jalankan Regresi", class = "btn btn-success btn-block")
          ),
          
          box(
            title = tagList(icon("table"), "Preview Data"),
            width = 8,
            status = "info",
            solidHeader = TRUE,
            
            div(class = "table-responsive",
                tableOutput("table")
            ),
            conditionalPanel(
              condition = "output.model_exists == true",
              tags$div(
                tags$hr(),
                tags$p("✅ Regresi berhasil dijalankan. Silakan cek hasilnya di tab Pemodelan.",
                       style = "color: green; font-weight: bold;")
              )
            )
          )
        )
      ),
      
      # PEMODELAN
      tabItem(
        tabName = "model",
        fluidRow(
          box(
            title = tagList(icon("code"), "Formula Model"),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            verbatimTextOutput("model_formula")
          ),
          box(
            title = tagList(icon("list-alt"), "Summary Model Regresi"),
            width = 12,
            status = "info",
            solidHeader = TRUE,
            verbatimTextOutput("model_summary")
          )
        )
      ),
      
      # UJI ASUMSI
      tabItem(
        tabName = "asumsi",
        fluidRow(
          box(
            title = tagList(icon("chart-bar"), "Uji Normalitas Residual"),
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            fluidRow(
              column(12, plotOutput("hist_resid")),
              column(12, plotOutput("qq_resid"))
            ),
            tags$hr(),
            h4("Uji Shapiro-Wilk Residual"),
            verbatimTextOutput("shapiro_test")
          ),
          box(
            title = tagList(icon("wave-square"), "Residual vs Index (Independensi)"),
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            plotOutput("resid_vs_index")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  values <- reactiveValues(
    data = NULL,
    dep_var = NULL,
    indep_vars = NULL,
    x_types = list(),
    model = NULL
  )
  
  observe({
    req(input$file1)
    values$data <- read.csv(input$file1$datapath)
  })
  
  output$table <- renderTable({
    req(values$data)
    head(values$data, 10)
  })
  
  output$var_select_ui <- renderUI({
    req(values$data)
    selectInput("dep_var", "Pilih Variabel Dependen (Y):",
                choices = names(values$data), selected = NULL)
  })
  
  output$indep_vars_ui <- renderUI({
    req(values$data, input$dep_var)
    x_choices <- setdiff(names(values$data), input$dep_var)
    pickerInput("indep_vars", "Pilih Variabel Independen (X):",
                choices = x_choices, multiple = TRUE,
                options = list(`actions-box` = TRUE))
  })
  
  output$x_type_ui <- renderUI({
    req(input$indep_vars)
    lapply(input$indep_vars, function(var) {
      selectInput(
        inputId = paste0("xtype_", var),
        label = paste("Tipe Variabel", var, ":"),
        choices = c("Numerik", "Kategorik (Dummy)"),
        selected = "Numerik"
      )
    })
  })
  
  observe({
    values$dep_var <- input$dep_var
    values$indep_vars <- input$indep_vars
    if (!is.null(input$indep_vars)) {
      for (var in input$indep_vars) {
        tipe <- input[[paste0("xtype_", var)]]
        if (!is.null(tipe)) {
          values$x_types[[var]] <- tipe
        }
      }
    }
  })
  
  observeEvent(input$run_regression, {
    req(values$data, values$dep_var, values$indep_vars)
    df <- values$data
    
    for (var in values$indep_vars) {
      tipe <- values$x_types[[var]]
      if (!is.null(tipe)) {
        if (tipe == "Kategorik (Dummy)") {
          df[[var]] <- as.factor(df[[var]])
        } else {
          df[[var]] <- as.numeric(df[[var]])
        }
      }
    }
    
    formula_str <- paste(values$dep_var, "~", paste(values$indep_vars, collapse = " + "))
    model <- lm(as.formula(formula_str), data = df)
    values$model <- model
  })
  
  output$model_exists <- reactive({
    return(!is.null(values$model))
  })
  outputOptions(output, "model_exists", suspendWhenHidden = FALSE)
  
  output$model_formula <- renderPrint({
    req(values$model)
    coefs <- summary(values$model)$coefficients
    var_names <- rownames(coefs)
    est <- round(coefs[, "Estimate"], 2)
    
    formula_str <- paste0(values$dep_var, " = ", est[1])
    if (length(est) > 1) {
      for (i in 2:length(est)) {
        sign <- ifelse(est[i] >= 0, " + ", " - ")
        formula_str <- paste0(formula_str, sign, abs(est[i]), "*", var_names[i])
      }
    }
    
    cat(formula_str)
  })
  
  output$model_summary <- renderPrint({
    req(values$model)
    summary(values$model)
  })
  
  output$hist_resid <- renderPlot({
    req(values$model)
    hist(resid(values$model), col = "skyblue", main = "Histogram Residual")
  })
  
  output$qq_resid <- renderPlot({
    req(values$model)
    qqnorm(resid(values$model))
    qqline(resid(values$model), col = "red")
  })
  
  output$shapiro_test <- renderPrint({
    req(values$model)
    res <- resid(values$model)
    shapiro.test(res)
  })
  
  output$resid_vs_index <- renderPlot({
    req(values$model)
    plot(resid(values$model), type = "b", pch = 19, col = "darkgreen",
         ylab = "Residual", xlab = "Index", main = "Residual vs Index")
    abline(h = 0, lty = 2)
  })
}

shinyApp(ui, server)
