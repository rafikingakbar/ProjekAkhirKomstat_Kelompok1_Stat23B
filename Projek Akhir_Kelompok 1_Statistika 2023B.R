library(shiny)
library(shinyWidgets)
library(shinydashboard)
library("car")
library("ggplot2")
library("reshape2")
library(lmtest)

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Analisis Regresi"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "home", icon = icon("home")),
      menuItem("Unggah Data", tabName = "upload", icon = icon("file-upload")),
      menuItem("Pemodelan", tabName = "model", icon = icon("chart-line")),
      menuItem("Uji Asumsi Klasik", tabName = "asumsi", icon = icon("check-double")),
      menuItem("Simulasi Prediksi", tabName = "predict", icon = icon("calculator"))
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
      text-align: justify;
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
              <li><b>Unggah Data:</b> Upload file CSV yang akan dianalisis dan pilih variabel dependen serta independen.</li>
              <li><b>Pemodelan:</b> Buat model regresi linear berdasarkan variabel yang dipilih dan tipe variabel (numerik/kategorik dummy).</li>
              <li><b>Uji Asumsi Klasik:</b> Lakukan pengujian asumsi seperti normalitas residual, independensi residual (Durbin-Watson),homoskedastisitas (Breusch-Pagan), dan multikolinearitas.</li>
              <li><b>Simulasi Prediksi:</b> Masukkan nilai variabel X baru secara manual untuk memprediksi nilai Y.</li>
            </ol>"
            ),
            
            h4(icon("info"), "Penjelasan Analisis:"),
            HTML(
              "<ul>
              <li><b>Pemodelan:</b> Model regresi digunakan untuk memprediksi nilai variabel dependen (Y) berdasarkan kombinasi variabel independen (X), baik numerik maupun kategorik (dengan dummy).</li>
              <li><b>Asumsi Klasik:</b> Termasuk uji <b>normalitas</b> dengan histogram, QQ plot & Shapiro-Wilk; <b>independensi</b> dengan Durbin-Watson; <b>homoskedastisitas</b> secara visual dan uji Breusch-Pagan; serta <b>multikolinearitas</b> melalui korelasi Pearson dan Cramér's V.</li>
              <li><b>Simulasi Prediksi:</b> Fitur untuk menguji prediksi model terhadap kombinasi nilai input baru dari pengguna.</li>
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
            
            radioButtons("na_option", "Penanganan Nilai Hilang:",
                         choices = c("Biarkan data NA)" = "none",
                                     "Hapus baris dengan NA" = "drop",
                                     "Ganti NA dengan rata-rata (hanya numerik)" = "mean"),
                         selected = "none"),
            
            
            conditionalPanel(
              condition = "output.y_status == 'failed'",
              tags$div(
                tags$hr(),
                tags$p("\u274C Gagal: Variabel dependen (Y) harus numerik.",
                       style = "color: red; font-weight: bold;")
              )
            ),
            conditionalPanel(
              condition = "output.regression_status == 'success'",
              tags$div(
                tags$hr(),
                tags$p("\u2705 Regresi berhasil dijalankan. Silakan cek hasilnya di tab Pemodelan.",
                       style = "color: green; font-weight: bold;")
              )
            ),
            conditionalPanel(
              condition = "output.regression_status == 'failed'",
              tags$div(
                tags$hr(),
                tags$p("\u274C Regresi gagal dijalankan. Pastikan variabel independen (X) diatur ke dummy jika kategorik.",
                       style = "color: red; font-weight: bold;")
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
            title = tagList(icon("list-alt"), "Summary Model Regresi"),
            width = 12,
            status = "info",
            solidHeader = TRUE,
            verbatimTextOutput("model_summary")
          ),
          box(
            title = tagList(icon("code"), "Persamaan Regresi"),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            verbatimTextOutput("model_formula")
          ),
          box(
            title = tagList(icon("chart-line"), "Scatter Plot: Data Aktual vs Prediksi"),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("actual_vs_predicted_plot")
          )
        )
      ),
      tabItem(
        tabName = "predict",
        fluidRow(
          box(
            title = tagList(icon("calculator"), "Prediksi Nilai Y Baru"),
            width = 12,
            status = "info",
            solidHeader = TRUE,
            uiOutput("manual_input_ui"),
            actionButton("predict_manual", "Prediksi", class = "btn btn-success"),
            tags$hr(),
            verbatimTextOutput("manual_prediction")
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
            verbatimTextOutput("shapiro_test"),
            verbatimTextOutput("shapiro_interpretasi")
            
          ),
          
          
          box(
            title = tagList(icon("wave-square"), "Residual vs Index dan Uji Durbin-Watson (Independensi Residual)"),
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            # Plot residual vs index
            h4("Visualisasi Residual terhadap Index"),
            plotOutput("resid_vs_index"),
            tags$hr(),
            # Uji Durbin-Watson
            h4("Uji Durbin-Watson (Formal Test)"),
            verbatimTextOutput("dw_test"),
          ),
          
          box(
            title = tagList(icon("balance-scale"), "Uji Homoskedastisitas (Visual & Breusch-Pagan Test)"),
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            
            # Plot residual vs fitted
            plotOutput("resid_vs_fitted"),
            
            tags$hr(),
            
            # Output uji Breusch-Pagan
            h4("Uji Breusch-Pagan (Formal Test)"),
            verbatimTextOutput("bp_test"),
            
            tags$hr(),
            
            
          ),
          
          # tab multikolinearitas
          box(
            title = tagList(icon("columns"), "Uji Multikolinearitas & Korelasi"),
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            uiOutput("uji_korelasi_output")  # Output tunggal yang mencakup semua jenis korelasi
          ),
          
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
    model = NULL,
    regression_status = NULL,
    y_status = NULL
  )
  
  observe({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    
    # Tangani nilai hilang sesuai pilihan user
    if (input$na_option == "drop") {
      # Hapus baris hanya jika NA ada di kolom numerik
      numeric_cols <- sapply(df, is.numeric)
      df <- df[complete.cases(df[, numeric_cols, drop = FALSE]), ]
      
    } else if (input$na_option == "mean") {
      numeric_cols <- sapply(df, is.numeric)
      for (col in names(df)[numeric_cols]) {
        df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
      }
    }
    
    values$data <- df
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
    
    # cek Y harus numerik
    if (!is.numeric(df[[values$dep_var]])) {
      values$model <- NULL
      values$regression_status <- NULL  # tidak relevan
      values$y_status <- "failed"
      showNotification(
        paste0(
          "Variabel dependen (Y) harus numerik.\n",
          "Anda memilih variabel kategorik sebagai Y."
        ),
        type = "error",
        duration = 7
      )
      return()
    } else {
      values$y_status <- "success"
    }
    
    # proses X
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
    tryCatch({
      model <- lm(as.formula(formula_str), data = df)
      values$model <- model
      values$regression_status <- "success"
    }, error = function(e) {
      values$model <- NULL
      values$regression_status <- "failed"
      showNotification(
        paste0(
          "Terjadi kesalahan saat menjalankan regresi.\n",
          "Pastikan variabel independen (X) bertipe numerik atau diatur sebagai dummy."
        ),
        type = "error",
        duration = 7
      )
    })
  })
  
  
  output$model_exists <- reactive({
    return(!is.null(values$model))
  })
  outputOptions(output, "model_exists", suspendWhenHidden = FALSE)
  
  output$regression_status <- reactive({
    values$regression_status
  })
  outputOptions(output, "regression_status", suspendWhenHidden = FALSE)
  
  output$y_status <- reactive({
    values$y_status
  })
  outputOptions(output, "y_status", suspendWhenHidden = FALSE)
  
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
  
  output$shapiro_interpretasi <- renderPrint({
    req(values$model)
    res <- resid(values$model)
    shapiro <- shapiro.test(res)
    pval <- shapiro$p.value
    alpha <- 0.05
    cat("Hipotesis:\n")
    cat("H0 : Residual berdistribusi normal\n")
    cat("H1 : Residual tidak berdistribusi normal\n\n")
    
    cat("Signifikansi (alpha) :", alpha, "\n")
    
    if (pval < 0.0001) {
      pval_text <- "< 0.0001"
    } else {
      pval_text <- round(pval, 4)
    }
    
    cat("p-value :", pval_text, "\n\n")
    
    if (pval < alpha) {
      cat("Keputusan: Tolak H0\n")
      cat("Interpretasi:❌ Residual tidak berdistribusi normal.\n")
    } else { 
      cat("Keputusan: Gagal tolak H0\n")
      cat("Interpretasi:✅ Residual berdistribusi normal.\n")
    }
  })
  
  output$resid_vs_index <- renderPlot({
    req(values$model)
    plot(resid(values$model), type = "b", pch = 19, col = "darkgreen",
         ylab = "Residual", xlab = "Index", main = "Residual vs Index")
    abline(h = 0, lty = 2)
  })
  
  output$resid_vs_fitted <- renderPlot({
    req(values$model)
    
    res <- resid(values$model)
    fit <- fitted(values$model)
    
    plot(fit, res,
         main = "Plot Residual vs Fitted",
         xlab = "Fitted Values",
         ylab = "Residual",
         pch = 19, col = "blue")
    abline(h = 0, lty = 2, col = "red")
  })
  
  
  output$bp_test <- renderPrint({
    req(values$model)
    test <- bptest(values$model)
    
    # Cetak hasil uji
    print(test)
    
    cat("\n")
    cat("Hipotesis:\n")
    cat("H0 : Residual memiliki varian konstan (homoskedastisitas)\n")
    cat("H1 : Residual tidak memiliki varian konstan (terjadi heteroskedastisitas)\n\n")
    
    cat("Signifikansi (alpha) : 0.05\n")
    cat("p-value : ", 
        ifelse(test$p.value < 0.0001, "< 0.0001", formatC(test$p.value, format = "f", digits = 4)), "\n\n")
    
    if (test$p.value < 0.05) {
      cat("❌ Tolak H0: Terdapat indikasi heteroskedastisitas\n")
    } else {
      cat("✅ Gagal tolak H0: Tidak terdapat indikasi heteroskedastisitas\n")
    }
  })
  
  # Durbin Watson
  output$dw_test <- renderPrint({
    req(values$model)
    dw <- lmtest::dwtest(values$model)
    
    cat("Uji Durbin-Watson Residual\n\n")
    cat("Durbin-Watson Test\n\n")
    cat("data:  values$model\n")
    cat(sprintf("DW = %.5f, ", dw$statistic))
    
    # Penanganan p-value kecil
    if (dw$p.value < 0.0001) {
      cat("p-value < 0.0001\n")
    } else {
      cat(sprintf("p-value = %.4f\n", dw$p.value))
    }
    
    cat("\nHipotesis:\n")
    cat("H0 : Tidak terdapat autokorelasi (residual independen)\n")
    cat("H1 : Terdapat autokorelasi (residual tidak independen)\n\n")
    
    cat("Signifikansi (alpha) : 0.05\n")
    cat("p-value : ")
    if (dw$p.value < 0.0001) {
      cat("< 0.0001\n\n")
    } else {
      cat(sprintf("%.4f\n\n", dw$p.value))
    }
    
    if (dw$p.value < 0.05) {
      cat("Interpretasi: ❌ Residual tidak independen (terdapat autokorelasi).\n")
    } else {
      cat("Interpretasi: ✅ Residual independen (tidak terdapat autokorelasi).\n")
    }
  })
  
  #MULTIKOLINEARITAS
  output$uji_korelasi_output <- renderUI({
    req(values$data, values$indep_vars)
    
    num_vars <- values$indep_vars[sapply(values$indep_vars, function(x) is.numeric(values$data[[x]]))]
    cat_vars <- values$indep_vars[sapply(values$indep_vars, function(x) is.factor(values$data[[x]]) || is.character(values$data[[x]]))]
    
    output_list <- list()
    
    # -------- 1. Korelasi Numerik vs Numerik --------
    if (length(num_vars) >= 2) {
      corr_matrix <- cor(values$data[num_vars], use = "pairwise.complete.obs")
      melted <- reshape2::melt(corr_matrix)
      
      output$corr_plot <- renderPlot({
        ggplot(melted, aes(Var1, Var2, fill = value)) +
          geom_tile(color = "white") +
          geom_text(aes(label = round(value, 2)), size = 4) +
          scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0,
                               limit = c(-1, 1), name = "Pearson\nCorrelation") +
          theme_minimal() +
          labs(title = "Korelasi Pearson antar Variabel Numerik") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
      
      output_list <- append(output_list, list(
        tags$h4("Korelasi Numerik vs Numerik (Pearson)"),
        plotOutput("corr_plot"),
        tags$p("Interpretasi: Nilai mendekati 1 atau -1 menandakan korelasi kuat. Nilai mendekati 0 menunjukkan korelasi lemah."),
        tags$hr()
      ))
    }
    
    # -------- 2. Korelasi Numerik vs Kategorik (ANOVA) --------
    if (length(num_vars) > 0 && length(cat_vars) > 0) {
      output_list <- append(output_list, list(tags$h4("Uji Numerik vs Kategorik (ANOVA)")))
      
      for (num in num_vars) {
        for (cat in cat_vars) {
          df <- na.omit(values$data[, c(num, cat)])
          if (nrow(df) == 0) next
          df[[cat]] <- as.factor(df[[cat]])
          formula <- as.formula(paste(num, "~", cat))
          result <- tryCatch(anova(lm(formula, data = df)), error = function(e) NULL)
          
          if (!is.null(result)) {
            pval_raw <- result$`Pr(>F)`[1]
            pval_fmt <- if (pval_raw < 0.0001) formatC(pval_raw, format = "e", digits = 2) else round(pval_raw, 4)
            
            interpret <- if (pval_raw < 0.05) {
              "→ Karena p-value < 0.05, maka H₀ ditolak. Artinya, terdapat perbedaan rata-rata yang signifikan antara kelompok kategori."
            } else {
              "→ Karena p-value ≥ 0.05, maka H₀ diterima. Artinya, tidak terdapat perbedaan rata-rata yang signifikan antara kelompok kategori."
            }
            
            
            output_list <- append(output_list, list(
              tags$b(paste("ANOVA", num, "vs", cat)),
              tags$em("H0: Tidak ada perbedaan rata-rata antar kelompok."),
              tags$pre(paste(capture.output(print(result)), collapse = "\n")),
              tags$p(paste("p-value =", pval_fmt)),
              tags$p(interpret),
              tags$hr()
            ))
          }
        }
      }
    }
    
    # -------- 3. Korelasi Kategorik vs Kategorik (Cramér's V) --------
    if (length(cat_vars) >= 2) {
      output_list <- append(output_list, list(tags$h4("Korelasi Kategorik vs Kategorik (Cramér's V)")))
      
      cramers_v <- function(x, y) {
        tbl <- table(x, y)
        chi2 <- suppressWarnings(chisq.test(tbl, correct = FALSE)$statistic)
        n <- sum(tbl)
        min_dim <- min(nrow(tbl) - 1, ncol(tbl) - 1)
        sqrt(as.numeric(chi2) / (n * min_dim))
      }
      
      mat <- matrix(NA, nrow = length(cat_vars), ncol = length(cat_vars))
      rownames(mat) <- colnames(mat) <- cat_vars
      
      for (i in seq_along(cat_vars)) {
        for (j in seq_along(cat_vars)) {
          if (i == j) {
            mat[i, j] <- 1
          } else {
            mat[i, j] <- tryCatch(
              cramers_v(values$data[[cat_vars[i]]], values$data[[cat_vars[j]]]),
              error = function(e) NA
            )
          }
        }
      }
      
      melted_cramer <- reshape2::melt(mat, na.rm = TRUE)
      
      output$cramers_plot <- renderPlot({
        ggplot(melted_cramer, aes(Var1, Var2, fill = value)) +
          geom_tile(color = "white") +
          geom_text(aes(label = round(value, 2)), size = 4) +
          scale_fill_gradient2(low = "white", high = "steelblue", limit = c(0,1),
                               name = "Cramér's V") +
          theme_minimal() +
          labs(title = "Cramér's V antar Variabel Kategorik") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
      
      output_list <- append(output_list, list(
        plotOutput("cramers_plot"),
        tags$p("Interpretasi:"),
        tags$ul(
          tags$li("0–0.1: Sangat lemah"),
          tags$li("0.1–0.3: Lemah"),
          tags$li("0.3–0.5: Sedang"),
          tags$li("> 0.5: Kuat")
        ),
        tags$hr()
      ))
    }
    
    do.call(tagList, output_list)
  })
  
  #PREDICTED
  output$actual_vs_predicted_plot <- renderPlot({
    req(values$model)
    
    df <- model.frame(values$model)  # data yang benar-benar digunakan oleh model
    y_aktual <- df[[1]]              # kolom pertama = Y
    y_prediksi <- predict(values$model)
    
    plot(y_aktual, y_prediksi,
         xlab = "Actual", ylab = "Predicted",
         main = "Actual vs Predicted",
         pch = 19, col = "red",
         xlim = range(c(y_aktual, y_prediksi), na.rm = TRUE),
         ylim = range(c(y_aktual, y_prediksi), na.rm = TRUE))
    
    abline(a = 0, b = 1, col = "blue", lwd = 2)  # Garis ideal
    legend("topleft", legend = c("Data", "Ideal"),
           col = c("red", "blue"), pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2))
  })
  output$manual_input_ui <- renderUI({
    req(values$model, values$indep_vars)
    
    input_ui <- lapply(values$indep_vars, function(var) {
      tipe <- values$x_types[[var]]
      if (tipe == "Kategorik (Dummy)") {
        levels_var <- unique(as.character(values$data[[var]]))
        selectInput(
          inputId = paste0("pred_", var),
          label = paste("Masukkan nilai", var),
          choices = levels_var
        )
      } else {
        numericInput(
          inputId = paste0("pred_", var),
          label = paste("Masukkan nilai", var),
          value = 0
        )
      }
    })
    
    do.call(tagList, input_ui)
  })
  
  observeEvent(input$predict_manual, {
    req(values$model, values$indep_vars)
    
    new_data <- data.frame(matrix(ncol = length(values$indep_vars), nrow = 1))
    names(new_data) <- values$indep_vars
    
    for (var in values$indep_vars) {
      tipe <- values$x_types[[var]]
      if (tipe == "Kategorik (Dummy)") {
        new_data[[var]] <- factor(input[[paste0("pred_", var)]],
                                  levels = unique(as.character(values$data[[var]])))
      } else {
        new_data[[var]] <- as.numeric(input[[paste0("pred_", var)]])
      }
    }
    
    pred <- predict(values$model, newdata = new_data)
    
    output$manual_prediction <- renderPrint({
      cat("Prediksi nilai", values$dep_var, "adalah:", round(pred, 4))
    })
  })
}


shinyApp(ui, server)
