# =========================================================
# MSG287 Assignment 2 - R Shiny Application
# Credit Card Default Payment Analysis
# Prepared for: DR. MUHAMMAD FADHIL BIN MARSANI

rm(list = ls())

options(shiny.maxRequestSize = 100 * 1024^2)

library(shiny)
library(DT)
library(ggplot2)
library(corrplot)
library(pROC)
library(car)
library(ResourceSelection)
library(shinythemes)
library(readxl)

# =========================================================
# HELPER FUNCTIONS
# =========================================================

info_box <- function(title, text) {
  div(
    class = "info-box",
    h4(title),
    p(text)
  )
}

safe_divide <- function(x, y) {
  ifelse(y == 0, NA, x / y)
}

# =========================================================
# USER INTERFACE
# =========================================================

ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  titlePanel("Credit Card Default Payment Analysis System"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput(
        "file",
        "Upload Original Dataset",
        accept = c(".xls", ".xlsx")
      ),
      
      selectInput(
        "cor_method",
        "Select Correlation Method",
        choices = c(
          "Pearson Correlation" = "pearson",
          "Spearman Rank Correlation" = "spearman",
          "Kendall Tau" = "kendall"
        ),
        selected = "spearman"
      ),
      
      selectInput(
        "plot_type",
        "Choose Graph Type",
        choices = c(
          "Default Status",
          "Gender Distribution",
          "Education Distribution",
          "Marriage Distribution",
          "Histogram of Credit Limit",
          "Histogram of Age",
          "Credit Limit by Default",
          "Age by Default",
          "PAY_0 by Default"
        )
      ),
      
      sliderInput(
        "threshold",
        "Classification Threshold",
        min = 0.1,
        max = 0.9,
        value = 0.5,
        step = 0.05
      ),
      
      radioButtons(
        "theme_mode",
        "Display Mode",
        choices = c("Light Mode", "Dark Mode"),
        selected = "Light Mode",
        inline = TRUE
      ),
      
      hr(),
      
      selectInput(
        "download_type",
        "Choose Report File Type",
        choices = c("Text File (.txt)" = "txt",
                    "CSV File (.csv)" = "csv",
                    "PDF File (.pdf)" = "pdf"),
        selected = "txt"
      ),
      
      downloadButton(
        "download_summary",
        "Download Summary Report"
      )
    ),
    
    mainPanel(
      
      uiOutput("theme_style"),
      
      tabsetPanel(
        
        tabPanel(
          "Home",
          br(),
          info_box(
            "About This Application",
            "This application helps users analyse credit card default payment data. It is designed for new learners, so each page includes a short explanation before showing the output."
          ),
          h3("Interactive Statistical Application"),
          p("The dataset contains information about credit card clients, including credit limit, demographic details, repayment history, bill amounts, payment amounts, and default status."),
          tags$ul(
            tags$li("Upload and preview the original dataset"),
            tags$li("Check data quality such as missing values and duplicated IDs"),
            tags$li("Generate descriptive statistics and frequency tables"),
            tags$li("Create graphs to understand data patterns"),
            tags$li("Conduct hypothesis testing with H0 and H1 statements"),
            tags$li("Perform correlation analysis"),
            tags$li("Build Multiple Linear Regression and Logistic Regression models"),
            tags$li("Evaluate model performance"),
            tags$li("Choose the most suitable model")
          )
        ),
        
        tabPanel(
          "Data Preview",
          br(),
          info_box(
            "Data Preview",
            "This page shows the first 20 rows of the uploaded dataset. It helps users check whether the correct Excel file has been uploaded and whether the variable names are read correctly."
          ),
          DTOutput("preview")
        ),
        
        tabPanel(
          "Data Quality",
          br(),
          info_box(
            "Data Quality Check",
            "Before doing statistical analysis, we should check the number of rows and columns, missing values, and duplicated IDs. Data problems may affect the accuracy of the analysis."
          ),
          h4("Dataset Dimension"),
          verbatimTextOutput("data_dimension"),
          br(),
          h4("Missing Values"),
          DTOutput("missing_table"),
          br(),
          h4("Duplicated ID Check"),
          verbatimTextOutput("duplicate_check")
        ),
        
        tabPanel(
          "Summary Statistics",
          br(),
          info_box(
          "Summary Statistics",
            HTML(
              "This page shows the original summary format of the dataset. It includes <b>minimum, quartiles, median, mean, and maximum values</b> for <b>numerical variables</b>, followed by <b>frequency tables</b> for <b>categorical variables.</b>"
            )
          ),
          h4("Summary Statistics"),
          verbatimTextOutput("summary_stats"),
          br(),
          br(),
          hr(),
          br(),
          h4("Frequency Tables"),
          verbatimTextOutput("frequency_tables")
        ),
        
        tabPanel(
          "Graphs",
          br(),
          info_box(
            "Graphical Analysis",
            HTML(
            "Graphs help users understand patterns more easily. <b>Bar charts</b> show category counts, <b>histograms</b> show distributions, and <b>boxplots</b> compare numerical values between default groups.
            <br><br>
            Users may <b>choose the graph type</b> from the left-hand sidebar under <b>'Choose Graph Type'</b>. This allows new learners to explore different parts of the dataset step by step."
            )
          ),
          plotOutput("main_plot", height = "600px")
        ),
        
        tabPanel(
          "Hypothesis Testing",
          br(),
          info_box(
            "Hypothesis Testing",
            "This page provides the null hypothesis, alternative hypothesis, test output, and interpretation. Several tests are provided so users can examine relationships between default status and different variables."
          ),
          selectInput(
            "test_type",
            "Choose Hypothesis Test",
            choices = c(
              "Chi-square: Sex vs Default",
              "Chi-square: Education vs Default",
              "Chi-square: Marriage vs Default",
              "Chi-square: PAY_0 vs Default",
              "Welch t-test: Credit Limit by Default",
              "Welch t-test: Age by Default",
              "Welch t-test: Bill Amount 1 by Default",
              "Welch t-test: Payment Amount 1 by Default",
              "Correlation test: Credit Limit vs Age"
            )
          ),
          h4("Null and Alternative Hypotheses"),
          verbatimTextOutput("hypothesis_statement"),
          br(),
          h4("Hypothesis Test Output"),
          verbatimTextOutput("hypothesis_output"),
          br(),
          h4("Decision and Interpretation"),
          verbatimTextOutput("hypothesis_interpretation")
        ),
        
        tabPanel(
          "Correlation",
          br(),
          info_box(
            "Correlation Analysis",
            HTML(
              "Correlation <b>measures the strength and direction of the relationship between numerical variables.</b> Values close to 1 or -1 show stronger relationships, while values close to 0 show weaker relationships. Users may <b>select</b> Pearson Correlation, Spearman Rank Correlation, or Kendall Tau <b>correlation method from the left-hand sidebar.</b>"
            )
          ),
          
          uiOutput("kendall_note"),
          
          h4("Correlation Table"),
          DTOutput("cor_table"),
          br(),
          h4("Correlation Heatmap"),
          plotOutput("heatmap", height = "850px")
        ),
        
        tabPanel(
          "Regression Models",
          br(),
          info_box(
            "Regression Models",
            
            HTML(
              "<b>Regression analysis</b> is used to <b>study how different variables affect credit card default payment behaviour.</b> In this application, variables such as credit limit, age, repayment history, bill amount, and payment amount are used to predict whether a client is likely to default on credit card payment.
              <br><br>
              <b>Multiple Linear Regression (MLR)</b> and <b>Logistic Regression</b> are both displayed for learning and comparison purposes. <b>MLR</b> is normally used when the <b>response variable is continuous</b>, such as income or sales amount. However, in this dataset, the response variable only has two possible outcomes: Default or No Default.
              <br><br>
              Therefore, <b>Logistic Regression is more suitable</b> because it is specially designed for binary classification problems. Logistic Regression predicts the probability of a client defaulting based on the selected predictor variables."
            )
          ),
          h4("Multiple Linear Regression Summary"),
          verbatimTextOutput("mlr_summary"),
          br(),
          h4("Logistic Regression Coefficient Table"),
          DTOutput("coef_table"),
          br(),
          h4("Logistic Regression Odds Ratio Table"),
          p(
            "The Odds Ratio table helps users understand how each independent variable affects the likelihood of credit card default. Logistic Regression coefficients are difficult to interpret directly because they are expressed in log-odds form. Therefore, the coefficients are converted into Odds Ratios for easier interpretation.",
            br(), br(),
            "An Odds Ratio greater than 1 indicates that the variable increases the probability of default, while an Odds Ratio less than 1 indicates that the variable decreases the probability of default. Values close to 1 suggest little or no effect on default risk.",
            br(), br(),
            "For example, if the Odds Ratio for repayment delay (PAY_0) is greater than 1, it suggests that clients with higher repayment delays are more likely to default on credit card payment."
          ),
          br(), br(),
          DTOutput("odds_table"),
          br(),
          h4("Regression Interpretation"),
          verbatimTextOutput("regression_interpretation")
        ),
        
        tabPanel(
          "Model Performance",
          br(),
          info_box(
            "Model Performance",
            
            HTML(
              "This page evaluates how well the Logistic Regression model predicts credit card default status. After building the model, it is important to measure how accurate and reliable the predictions are before using the model for decision-making.
              <br><br>
              Several performance measures are provided in this section. The <b>Confusion Matrix</b> compares the predicted results with the actual default status to show the number of correct and incorrect predictions made by the model.
              <br><br>
              <b>Accuracy</b> measures the overall percentage of correct predictions made by the model. <b>Sensitivity</b> measures how well the model identifies actual default cases, while <b>Specificity</b> measures how well the model identifies non-default cases.
              <br><br>
              The <b>Receiver Operating Characteristic (ROC) Curve</b> and <b>Area Under the Curve (AUC)</b> are also included to evaluate the classification ability of the model. An <b>AUC value closer to 1 indicates better prediction performance</b>, while a <b>value closer to 0.5 suggests weaker classification ability.</b>
              <br><br>
              Users can also adjust the <b>Classification Threshold</b> to observe how prediction performance changes under different probability cut-off values."
            )
          ),
          div(
            style = "
    background-color:#e8f4ff;
    color:#000000 !important;
    border-left:5px solid #3498db;
    padding:12px 15px;
    border-radius:6px;
    margin-bottom:15px;
    line-height:1.7;
  ",
            
            HTML(
              "<span style='color:#000000 !important;'>
               <b>Note:</b> Changing the <b>Classification Threshold</b> affects the predicted class labels, confusion matrix, accuracy, sensitivity, and specificity.
               <br><br>
               However, the <b>Receiver Operating Characteristic (ROC) Curve</b> does not change because it already evaluates the model performance across all possible threshold values between 0 and 1.
               </span>"
            )
          ),
          
          h4("Confusion Matrix"),
          verbatimTextOutput("confusion_matrix"),
          br(),
          h4("Performance Metrics"),
          verbatimTextOutput("performance_text"),
          br(),
          plotOutput("roc_plot", height = "550px")
        ),
        
        tabPanel(
          "Diagnostics",
          br(),
          info_box(
            "Diagnostic Checks",
            
            HTML(
              "This section evaluates whether the regression models are statistically suitable and reliable for predicting credit card default behaviour. 
              <br><br>
              <b>Multiple Linear Regression (MLR)</b> and <b>Logistic Regression</b> are compared using several diagnostic measures. 
              <br><br>
              The <b>Variance Inflation Factor (VIF)</b> is used to check for multicollinearity among independent variables, while the <b>Hosmer-Lemeshow Test</b> is used to evaluate the goodness-of-fit of the Logistic Regression model.
              <br><br>
              Diagnostic plots are also provided to help users identify potential model problems and determine whether the model assumptions are reasonable."
            )
          ),
          h4("Model Fit Comparison"),
          DTOutput("model_fit_table"),
          br(),
          h4("Why MLR Is Rejected"),
          verbatimTextOutput("mlr_reject_text"),
          br(),
          h4("Variance Inflation Factor for Logistic Regression"),
          verbatimTextOutput("vif_text"),
          br(),
          h4("Hosmer-Lemeshow Goodness-of-Fit Test for Logistic Regression"),
          verbatimTextOutput("hosmer_text"),
          br(),
          h4("Diagnostic Plots for MLR"),
          plotOutput("mlr_diagnostic_plot", height = "700px"),
          br(),
          uiOutput("mlr_plot_explanation")
        ),
        
        tabPanel(
          "Best Model",
          br(),
          info_box(
            "Best Model Selection",
            "This page gives the final decision on the most suitable model. Although both MLR and Logistic Regression are displayed, the best model is selected based on the type of dependent variable and statistical suitability."
          ),
          h4("Model Comparison Table"),
          DTOutput("model_compare"),
          br(),
          h4("Best Model Identification"),
          verbatimTextOutput("best_model_text")
        ),
        
        tabPanel(
          "Conclusion",
          br(),
          info_box(
            "Conclusion",
            "This page summarises the overall findings from the analysis. It highlights the important role of repayment history and explains why Logistic Regression is the final selected model."
          ),
          h3("Overall Findings"),
          uiOutput("conclusion_text")
        )
      )
    )
  )
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output) {
  
  output$theme_style <- renderUI({
    
    if (input$theme_mode == "Dark Mode") {
      
      tags$style(HTML("
      
      .info-box {
       background-color: #1f1f1f !important;
       color: #f5f5f5 !important;
       border-left: 5px solid #66b2ff !important;
       padding: 12px 15px;
       margin-bottom: 18px;
       border-radius: 4px;
      }

      body,
      .container-fluid,
      .tab-content,
      .tab-pane {
        background-color: #121212 !important;
        color: #f5f5f5 !important;
      }

      .well {
        background-color: #1f1f1f !important;
        color: #f5f5f5 !important;
        border-color: #444444 !important;
      }

      .nav-tabs {
        background-color: #121212 !important;
        border-bottom: 1px solid #444444 !important;
      }

      .nav-tabs > li > a {
        background-color: #1f1f1f !important;
        color: #ffffff !important;
        border-color: #444444 !important;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        background-color: #333333 !important;
        color: #ffffff !important;
        border-color: #444444 !important;
      }

      pre {
        background-color: #1f1f1f !important;
        color: #f5f5f5 !important;
        border-color: #444444 !important;
      }

      h1, h2, h3, h4, h5, h6, p, label, span, li {
        color: #f5f5f5 !important;
      }

      .selectize-input,
      .form-control {
        background-color: #ffffff !important;
        color: #000000 !important;
      }

      table.dataTable {
        background-color: #ffffff !important;
        color: #000000 !important;
      }

    "))
      
    } else {
      
      tags$style(HTML("
      
      .info-box {
       background-color: #f7f9fb !important;
       color: #000000 !important;
       border-left: 5px solid #2c3e50 !important;
       padding: 12px 15px;
       margin-bottom: 18px;
       border-radius: 4px;
      }

      body,
      .container-fluid,
      .tab-content,
      .tab-pane {
        background-color: #ffffff !important;
        color: #000000 !important;
      }

      pre {
        background-color: #f5f5f5 !important;
        color: #000000 !important;
      }

    "))
    }
  })
  
  output$kendall_note <- renderUI({
    
    if (input$cor_method == "kendall") {
      p(
        style = "color: darkred; font-style: italic;",
        "Note: Due to the large dataset size, a random sample of 1000 observations is used when computing Kendall correlation to reduce computational burden and improve application performance."
      )
      
    } else {
      
      NULL
    }
  })
  
  data_input <- reactive({
    
    req(input$file)
    
    data <- read_excel(input$file$datapath, skip = 1)
    data <- as.data.frame(data)
    
    names(data) <- trimws(names(data))
    
    if ("default payment next month" %in% names(data)) {
      names(data)[names(data) == "default payment next month"] <- "DEFAULT_BINARY"
    }
    
    validate(
      need("DEFAULT_BINARY" %in% names(data), "Please upload the original credit card clients Excel dataset.")
    )
    
    numeric_columns <- c(
      "ID", "LIMIT_BAL", "SEX", "EDUCATION", "MARRIAGE", "AGE",
      "PAY_0", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6",
      "BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6",
      "PAY_AMT1", "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6",
      "DEFAULT_BINARY"
    )
    
    for (col in numeric_columns) {
      if (col %in% names(data)) {
        data[[col]] <- as.numeric(data[[col]])
      }
    }
    
    data$DEFAULT <- ifelse(data$DEFAULT_BINARY == 1, "Default", "No Default")
    data$DEFAULT <- factor(data$DEFAULT, levels = c("No Default", "Default"))
    
    data$EDUCATION_GROUP <- ifelse(
      data$EDUCATION == 1, "Graduate School",
      ifelse(
        data$EDUCATION == 2, "University",
        ifelse(data$EDUCATION == 3, "High School", "Others")
      )
    )
    
    data$EDUCATION_GROUP <- factor(
      data$EDUCATION_GROUP,
      levels = c("Graduate School", "University", "High School", "Others")
    )
    
    data
  })
  
  train_test_data <- reactive({
    
    data <- data_input()
    
    set.seed(123)
    
    index <- sample(
      1:nrow(data),
      size = 0.7 * nrow(data)
    )
    
    list(
      train = data[index, ],
      test = data[-index, ]
    )
  })
  
  selected_numeric_data <- reactive({
    
    data <- data_input()
    
    selected_columns <- c(
      "LIMIT_BAL", "AGE",
      "PAY_0", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6",
      "BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6",
      "PAY_AMT1", "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6",
      "DEFAULT_BINARY"
    )
    
    selected_columns <- selected_columns[selected_columns %in% names(data)]
    selected_data <- data[, selected_columns]
    selected_data <- as.data.frame(lapply(selected_data, as.numeric))
    
    selected_data
  })
  
  model_mlr <- reactive({
    
    data <- train_test_data()$train
    
    lm(
      DEFAULT_BINARY ~ LIMIT_BAL +
        SEX +
        EDUCATION +
        MARRIAGE +
        AGE +
        PAY_0 +
        PAY_2 +
        PAY_3 +
        BILL_AMT1 +
        BILL_AMT2 +
        PAY_AMT1 +
        PAY_AMT2,
      data = data,
      na.action = na.exclude
    )
  })
  
  model_logistic <- reactive({
    
    data <- train_test_data()$train
    
    glm(
      DEFAULT_BINARY ~ LIMIT_BAL +
        SEX +
        EDUCATION +
        MARRIAGE +
        AGE +
        PAY_0 +
        PAY_2 +
        PAY_3 +
        BILL_AMT1 +
        BILL_AMT2 +
        PAY_AMT1 +
        PAY_AMT2,
      data = data,
      family = binomial,
      na.action = na.exclude
    )
  })
  
  model_fit_data <- reactive({
    
    mlr_model <- model_mlr()
    logistic_model <- model_logistic()
    
    mlr_r2 <- summary(mlr_model)$r.squared
    mlr_adj_r2 <- summary(mlr_model)$adj.r.squared
    
    logistic_pseudo_r2 <- 1 - (
      logistic_model$deviance / logistic_model$null.deviance
    )
    
    data.frame(
      Model = c("Multiple Linear Regression", "Logistic Regression"),
      Main_Fit_Measure = c("R-squared", "McFadden Pseudo R-squared"),
      Fit_Value = c(mlr_r2, logistic_pseudo_r2),
      Additional_Measure = c("Adjusted R-squared", "AIC"),
      Additional_Value = c(mlr_adj_r2, AIC(logistic_model)),
      Suitability = c(
        "Rejected because the dependent variable is binary",
        "Accepted because the dependent variable is binary"
      )
    )
  })
  
  output$preview <- renderDT({
    
    data <- data_input()
    
    datatable(
      head(data, 20),
      options = list(scrollX = TRUE, pageLength = 10)
    )
  })
  
  output$data_dimension <- renderPrint({
    
    data <- data_input()
    
    cat("Number of Rows:", nrow(data), "\n")
    cat("Number of Columns:", ncol(data), "\n")
  })
  
  output$missing_table <- renderDT({
    
    data <- data_input()
    
    missing_df <- data.frame(
      Variable = names(data),
      Missing_Values = colSums(is.na(data))
    )
    
    datatable(missing_df)
  })
  
  output$duplicate_check <- renderPrint({
    
    data <- data_input()
    
    if ("ID" %in% names(data)) {
      cat("Number of duplicated IDs:", sum(duplicated(data$ID)))
    } else {
      cat("ID variable not found.")
    }
  })
  
  output$summary_stats <- renderPrint({
    
    data <- data_input()
    
    summary_columns <- c(
      "ID", "LIMIT_BAL", "SEX", "EDUCATION", "MARRIAGE", "AGE",
      "PAY_0", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6",
      "BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6",
      "PAY_AMT1", "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6",
      "DEFAULT_BINARY", "DEFAULT"
    )
    
    summary_columns <- summary_columns[summary_columns %in% names(data)]
    
    summary(data[, summary_columns])
  })
  
  output$frequency_tables <- renderPrint({
    
    data <- data_input()
    
    cat("SEX\n")
    cat("1 = Male, 2 = Female\n")
    print(table(data$SEX))
    
    cat("\n\n----------------------------------------\n\n")
    
    cat("EDUCATION GROUP\n")
    cat("Graduate School = 1, University = 2, High School = 3, Others = 0, 4, 5, 6\n")
    print(table(data$EDUCATION_GROUP))
    
    cat("\n\n----------------------------------------\n\n")
    
    cat("MARRIAGE\n")
    cat("1 = Married, 2 = Single, 3 = Others\n")
    print(table(data$MARRIAGE))
    
    cat("\n\n----------------------------------------\n\n")
    
    cat("DEFAULT STATUS\n")
    print(table(data$DEFAULT))
  })
  
  output$main_plot <- renderPlot({
    
    data <- data_input()
    
    if (input$plot_type == "Default Status") {
      
      ggplot(data, aes(x = DEFAULT)) +
        geom_bar(fill = "steelblue") +
        labs(x = "Default Status", y = "Count") +
        theme_minimal()
      
    } else if (input$plot_type == "Gender Distribution") {
      
      ggplot(data, aes(x = factor(SEX))) +
        geom_bar(fill = "orange") +
        labs(x = "Sex", y = "Count") +
        theme_minimal()
      
    } else if (input$plot_type == "Education Distribution") {
      
      ggplot(data, aes(x = EDUCATION_GROUP)) +
        geom_bar(fill = "darkcyan") +
        labs(x = "Education Group", y = "Count") +
        theme_minimal()
      
    } else if (input$plot_type == "Marriage Distribution") {
      
      ggplot(data, aes(x = factor(MARRIAGE))) +
        geom_bar(fill = "darkseagreen") +
        labs(x = "Marriage", y = "Count") +
        theme_minimal()
      
    } else if (input$plot_type == "Histogram of Credit Limit") {
      
      ggplot(data, aes(x = LIMIT_BAL)) +
        geom_histogram(bins = 20, fill = "darkgreen", colour = "black") +
        labs(x = "Credit Limit", y = "Frequency") +
        theme_minimal()
      
    } else if (input$plot_type == "Histogram of Age") {
      
      ggplot(data, aes(x = AGE)) +
        geom_histogram(bins = 20, fill = "purple", colour = "black") +
        labs(x = "Age", y = "Frequency") +
        theme_minimal()
      
    } else if (input$plot_type == "Credit Limit by Default") {
      
      ggplot(data, aes(x = DEFAULT, y = LIMIT_BAL)) +
        geom_boxplot(fill = "skyblue") +
        labs(x = "Default Status", y = "Credit Limit") +
        theme_minimal()
      
    } else if (input$plot_type == "Age by Default") {
      
      ggplot(data, aes(x = DEFAULT, y = AGE)) +
        geom_boxplot(fill = "pink") +
        labs(x = "Default Status", y = "Age") +
        theme_minimal()
      
    } else if (input$plot_type == "PAY_0 by Default") {
      
      ggplot(data, aes(x = DEFAULT, y = PAY_0)) +
        geom_boxplot(fill = "lightgreen") +
        labs(x = "Default Status", y = "PAY_0") +
        theme_minimal()
    }
  })
  
  # =======================================================
  # HYPOTHESIS TESTING
  # =======================================================
  
  output$hypothesis_statement <- renderPrint({
    
    if (input$test_type == "Chi-square: Sex vs Default") {
      cat("Test: Chi-square Test of Independence\n\n")
      cat("H0: There is no significant relationship between sex and default status.\n")
      cat("H1: There is a significant relationship between sex and default status.\n")
      
    } else if (input$test_type == "Chi-square: Education vs Default") {
      cat("Test: Chi-square Test of Independence\n\n")
      cat("H0: There is no significant relationship between education group and default status.\n")
      cat("H1: There is a significant relationship between education group and default status.\n\n")
      cat("Note: Education categories 0, 4, 5, and 6 are grouped as Others.")
      
    } else if (input$test_type == "Chi-square: Marriage vs Default") {
      cat("Test: Chi-square Test of Independence\n\n")
      cat("H0: There is no significant relationship between marital status and default status.\n")
      cat("H1: There is a significant relationship between marital status and default status.\n")
      
    } else if (input$test_type == "Chi-square: PAY_0 vs Default") {
      cat("Test: Chi-square Test of Independence\n\n")
      cat("H0: There is no significant relationship between most recent repayment status and default status.\n")
      cat("H1: There is a significant relationship between most recent repayment status and default status.\n")
      
    } else if (input$test_type == "Welch t-test: Credit Limit by Default") {
      cat("Test: Welch Two Sample t-test\n\n")
      cat("H0: There is no significant difference in mean credit limit between default and no-default clients.\n")
      cat("H1: There is a significant difference in mean credit limit between default and no-default clients.\n")
      
    } else if (input$test_type == "Welch t-test: Age by Default") {
      cat("Test: Welch Two Sample t-test\n\n")
      cat("H0: There is no significant difference in mean age between default and no-default clients.\n")
      cat("H1: There is a significant difference in mean age between default and no-default clients.\n")
      
    } else if (input$test_type == "Welch t-test: Bill Amount 1 by Default") {
      cat("Test: Welch Two Sample t-test\n\n")
      cat("H0: There is no significant difference in mean BILL_AMT1 between default and no-default clients.\n")
      cat("H1: There is a significant difference in mean BILL_AMT1 between default and no-default clients.\n")
      
    } else if (input$test_type == "Welch t-test: Payment Amount 1 by Default") {
      cat("Test: Welch Two Sample t-test\n\n")
      cat("H0: There is no significant difference in mean PAY_AMT1 between default and no-default clients.\n")
      cat("H1: There is a significant difference in mean PAY_AMT1 between default and no-default clients.\n")
      
    } else if (input$test_type == "Correlation test: Credit Limit vs Age") {
      cat("Test: Pearson Correlation Test\n\n")
      cat("H0: There is no significant correlation between credit limit and age.\n")
      cat("H1: There is a significant correlation between credit limit and age.\n")
    }
  })
  
  hypothesis_result <- reactive({
    
    data <- data_input()
    
    if (input$test_type == "Chi-square: Sex vs Default") {
      chisq.test(table(data$SEX, data$DEFAULT))
      
    } else if (input$test_type == "Chi-square: Education vs Default") {
      chisq.test(table(data$EDUCATION_GROUP, data$DEFAULT), simulate.p.value = TRUE, B = 10000)
      
    } else if (input$test_type == "Chi-square: Marriage vs Default") {
      chisq.test(table(data$MARRIAGE, data$DEFAULT), simulate.p.value = TRUE, B = 10000)
      
    } else if (input$test_type == "Chi-square: PAY_0 vs Default") {
      chisq.test(table(data$PAY_0, data$DEFAULT), simulate.p.value = TRUE, B = 10000)
      
    } else if (input$test_type == "Welch t-test: Credit Limit by Default") {
      t.test(LIMIT_BAL ~ DEFAULT, data = data)
      
    } else if (input$test_type == "Welch t-test: Age by Default") {
      t.test(AGE ~ DEFAULT, data = data)
      
    } else if (input$test_type == "Welch t-test: Bill Amount 1 by Default") {
      t.test(BILL_AMT1 ~ DEFAULT, data = data)
      
    } else if (input$test_type == "Welch t-test: Payment Amount 1 by Default") {
      t.test(PAY_AMT1 ~ DEFAULT, data = data)
      
    } else if (input$test_type == "Correlation test: Credit Limit vs Age") {
      cor.test(data$LIMIT_BAL, data$AGE, method = "pearson")
    }
  })
  
  output$hypothesis_output <- renderPrint({
    print(hypothesis_result())
  })
  
  output$hypothesis_interpretation <- renderPrint({
    
    result <- hypothesis_result()
    p_value <- result$p.value
    
    cat("P-value:", round(p_value, 4), "\n\n")
    cat("Decision rule:\n")
    cat("If p-value < 0.05, reject H0.\n")
    cat("If p-value >= 0.05, do not reject H0.\n\n")
    
    if (p_value < 0.05) {
      cat("Decision: Reject H0.\n\n")
      cat("Conclusion: There is sufficient evidence to indicate that ")
    } else {
      cat("Decision: Do not reject H0.\n\n")
      cat("Conclusion: There is insufficient evidence to indicate that ")
    }
    
    if (input$test_type == "Chi-square: Sex vs Default") {
      cat("there is a significant relationship between sex and default status.")
    } else if (input$test_type == "Chi-square: Education vs Default") {
      cat("there is a significant relationship between education group and default status.")
    } else if (input$test_type == "Chi-square: Marriage vs Default") {
      cat("there is a significant relationship between marital status and default status.")
    } else if (input$test_type == "Chi-square: PAY_0 vs Default") {
      cat("there is a significant relationship between most recent repayment status and default status.")
    } else if (input$test_type == "Welch t-test: Credit Limit by Default") {
      cat("there is a significant difference in mean credit limit between default and no-default clients.")
    } else if (input$test_type == "Welch t-test: Age by Default") {
      cat("there is a significant difference in mean age between default and no-default clients.")
    } else if (input$test_type == "Welch t-test: Bill Amount 1 by Default") {
      cat("there is a significant difference in mean BILL_AMT1 between default and no-default clients.")
    } else if (input$test_type == "Welch t-test: Payment Amount 1 by Default") {
      cat("there is a significant difference in mean PAY_AMT1 between default and no-default clients.")
    } else if (input$test_type == "Correlation test: Credit Limit vs Age") {
      cat("there is a significant correlation between credit limit and age.")
    }
    
    if (
      input$test_type == "Chi-square: Education vs Default" ||
      input$test_type == "Chi-square: Marriage vs Default" ||
      input$test_type == "Chi-square: PAY_0 vs Default"
    ) {
      cat("\n\nAdditional note:\n")
      cat("A simulated p-value is used because some categories may have small counts. This helps reduce the chi-square approximation warning.")
    }
  })
  
  output$cor_table <- renderDT({
    
    selected_data <- selected_numeric_data()
    
    if (input$cor_method == "kendall") {
      set.seed(123)
      selected_data <- selected_data[sample(nrow(selected_data), 1000), ]
    }
    
    cor_matrix <- cor(
      selected_data,
      use = "complete.obs",
      method = input$cor_method
    )
    
    datatable(round(cor_matrix, 3))
  })
  
  output$heatmap <- renderPlot({
    
    selected_data <- selected_numeric_data()
    
    if (input$cor_method == "kendall") {
      set.seed(123)
      selected_data <- selected_data[sample(nrow(selected_data), 1000), ]
    }
    
    cor_matrix <- cor(
      selected_data,
      use = "complete.obs",
      method = input$cor_method
    )
    
    corrplot(
      cor_matrix,
      method = "color",
      type = "upper",
      addCoef.col = "black",
      tl.cex = 0.8,
      number.cex = 0.6
    )
  })
  
  output$mlr_summary <- renderPrint({
    
    cat("Multiple Linear Regression Model Summary\n\n")
    print(summary(model_mlr()))
    
    cat("\nBeginner Explanation:\n")
    cat("R-squared shows how much variation in DEFAULT_BINARY is explained by the independent variables.\n")
    cat("However, DEFAULT_BINARY only contains 0 and 1, so MLR is not the most suitable final model.\n")
  })
  
  output$coef_table <- renderDT({
    coef_table <- summary(model_logistic())$coefficients
    datatable(round(coef_table, 4))
  })
  
  output$odds_table <- renderDT({
    
    coef_table <- summary(model_logistic())$coefficients
    
    odds_table <- data.frame(
      Variable = rownames(coef_table),
      Odds_Ratio = exp(coef_table[, 1]),
      P_Value = coef_table[, 4]
    )
    
    odds_table$Odds_Ratio <- round(odds_table$Odds_Ratio, 4)
    odds_table$P_Value <- round(odds_table$P_Value, 4)
    
    datatable(
      odds_table,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$regression_interpretation <- renderPrint({
    req(input$file)
    cat(
      "Multiple Linear Regression and Logistic Regression are both shown for comparison.\n\n",
      "MLR is normally used when the dependent variable is continuous, such as income, age, or sales amount.\n",
      "In this project, the dependent variable DEFAULT_BINARY is binary, where 0 = No Default and 1 = Default.\n\n",
      "Logistic Regression is more suitable because it predicts probability values between 0 and 1.\n",
      "Therefore, Logistic Regression is selected as the final model."
    )
  })
  
  output$confusion_matrix <- renderPrint({
    
    test_data <- train_test_data()$test
    prob <- predict(model_logistic(), newdata = test_data, type = "response")
    pred <- ifelse(prob >= input$threshold, "Default", "No Default")
    
    pred <- factor(pred, levels = c("No Default", "Default"))
    actual <- factor(test_data$DEFAULT, levels = c("No Default", "Default"))
    
    table(Predicted = pred, Actual = actual)
  })
  
  output$performance_text <- renderPrint({
    
    test_data <- train_test_data()$test
    prob <- predict(model_logistic(), newdata = test_data, type = "response")
    pred <- ifelse(prob >= input$threshold, "Default", "No Default")
    
    pred <- factor(pred, levels = c("No Default", "Default"))
    actual <- factor(test_data$DEFAULT, levels = c("No Default", "Default"))
    
    cm <- table(pred, actual)
    
    accuracy <- sum(diag(cm)) / sum(cm)
    sensitivity <- safe_divide(cm["Default", "Default"], sum(cm[, "Default"]))
    specificity <- safe_divide(cm["No Default", "No Default"], sum(cm[, "No Default"]))
    
    roc_result <- roc(test_data$DEFAULT_BINARY, prob, quiet = TRUE)
    auc_value <- auc(roc_result)
    
    cat("Accuracy:", round(accuracy, 4), "\n")
    cat("Sensitivity:", round(sensitivity, 4), "\n")
    cat("Specificity:", round(specificity, 4), "\n")
    cat("AUC:", round(auc_value, 4), "\n\n")
    
    cat("Explanation:\n")
    cat("Accuracy shows the overall correct prediction rate.\n")
    cat("Sensitivity shows how well the model detects actual default cases.\n")
    cat("Specificity shows how well the model detects actual non-default cases.\n")
    cat("AUC closer to 1 indicates better classification performance.")
  })
  
  output$roc_plot <- renderPlot({
    
    test_data <- train_test_data()$test
    prob <- predict(model_logistic(), newdata = test_data, type = "response")
    roc_result <- roc(test_data$DEFAULT_BINARY, prob, quiet = TRUE)
    
    plot(
      roc_result,
      main = "ROC Curve for Logistic Regression",
      col = "blue",
      lwd = 2
    )
  })
  
  output$model_fit_table <- renderDT({
    
    fit_table <- model_fit_data()
    fit_table$Fit_Value <- round(fit_table$Fit_Value, 4)
    fit_table$Additional_Value <- round(fit_table$Additional_Value, 4)
    
    datatable(
      fit_table,
      options = list(pageLength = 5, scrollX = TRUE)
    )
  })
  
  output$mlr_reject_text <- renderPrint({
    req(input$file)
    cat(
      "Reason for rejecting Multiple Linear Regression:\n\n",
      "1. The dependent variable DEFAULT_BINARY is binary, with only two values: 0 and 1.\n",
      "2. MLR is designed for continuous dependent variables.\n",
      "3. MLR may produce predicted values below 0 or above 1.\n",
      "4. Values below 0 or above 1 are not meaningful as probabilities.\n",
      "5. Logistic Regression is designed for binary outcomes and predicts probabilities between 0 and 1.\n\n",
      "Therefore, MLR is rejected and Logistic Regression is selected as the best model."
    )
  })
  
  output$vif_text <- renderPrint({
    print(vif(model_logistic()))
    
    cat("\nExplanation:\n")
    cat("VIF is used to check multicollinearity among independent variables.\n")
    cat("A high VIF value may indicate that some predictors are strongly related to each other.")
  })
  
  output$hosmer_text <- renderPrint({
    
    test_data <- train_test_data()$test
    prob <- predict(model_logistic(), newdata = test_data, type = "response")
    
    print(
      hoslem.test(
        test_data$DEFAULT_BINARY,
        prob,
        g = 10
      )
    )
    
    cat("\nExplanation:\n")
    cat("The Hosmer-Lemeshow test checks whether the Logistic Regression model fits the data well.\n")
    cat("Usually, a p-value greater than 0.05 suggests that the model fit is acceptable.")
  })
  
  output$mlr_diagnostic_plot <- renderPlot({
    par(mfrow = c(2, 2))
    plot(model_mlr())
    par(mfrow = c(1, 1))
  })
  output$mlr_plot_explanation <- renderUI({
    
    req(input$file)
    
    div(
      
      class = "info-box",
      style = "line-height:1.8;",
      
      HTML(
        "
        <h4><b>Interpretation of Diagnostic Plots</b></h4>
        <p>
        The diagnostic plots indicate that several assumptions of 
        <b>Multiple Linear Regression (MLR)</b> may not be fully satisfied for this dataset.
        </p>
        <ol type='I'>
        <li>
        <b>Residuals vs Fitted:</b> 
        Clear patterns can be observed instead of random scattering, suggesting possible non-linearity or unequal variance.
        </li>
        <br>
    
        <li>
        <b>Normal Q-Q:</b> 
        Many points deviate from the reference line, indicating that the residuals are not normally distributed.
        </li>
        <br>
    
        <li>
        <b>Scale-Location:</b> 
        The spread of residuals changes across fitted values, suggesting non-constant variance.
        </li>
        <br>
    
        <li>
        <b>Residuals vs Leverage:</b> 
        This plot helps identify influential observations that may strongly affect the regression model.
        </li>
    
        </ol>
  
        <p>
        Since the response variable only contains two outcomes, 
        <b>Default</b> and <b>No Default</b>, these results further support that 
        <b>Logistic Regression</b> is more statistically appropriate than MLR for this dataset.
        </p>
        "
      )
    )
  })
  
  model_comparison_data <- reactive({
    
    fit_table <- model_fit_data()
    
    data.frame(
      Model = fit_table$Model,
      Fit_Measure = fit_table$Main_Fit_Measure,
      Fit_Value = fit_table$Fit_Value,
      Decision = c(
        "Rejected",
        "Accepted as Best Model"
      ),
      Reason = c(
        "MLR is not suitable because DEFAULT_BINARY is binary",
        "Logistic Regression is suitable for binary default prediction"
      )
    )
  })
  
  output$model_compare <- renderDT({
    
    comparison <- model_comparison_data()
    comparison$Fit_Value <- round(comparison$Fit_Value, 4)
    
    datatable(
      comparison,
      options = list(pageLength = 5, scrollX = TRUE)
    )
  })
  
  output$best_model_text <- renderPrint({
    req(input$file)
    cat(
      "Best Model: Logistic Regression\n\n",
      "Final Decision:\n",
      "Multiple Linear Regression is rejected because the dependent variable DEFAULT_BINARY is binary.\n",
      "The variable only has two possible values: 0 = No Default and 1 = Default.\n\n",
      "MLR is more suitable for continuous outcomes, but this study is predicting the probability of default.\n",
      "Logistic Regression is more suitable because it is designed for binary classification problems.\n\n",
      "Therefore, the best model for this credit card default payment analysis is Logistic Regression."
    )
  })
  
  output$conclusion_text <- renderUI({
    req(input$file)
    HTML(
      "
      <div style='line-height:1.6;'>
      <h4><b>Overall Findings</b></h4>
      <p>
      This application successfully analysed <b>credit card default payment behaviour</b> using statistical analysis and predictive modelling techniques.
      </p>

      <p>
      The analysis includes:
      </p>

      <ol type='I'>
      <li>Data preview and data quality checking</li>
      <li>Summary statistics and graphical analysis</li>
      <li>Hypothesis testing and correlation analysis</li>
      <li>Regression modelling and diagnostic checking</li>
      <li>Model performance evaluation and model selection</li>
      </ol>

      <p>
      Several hypothesis tests were conducted to examine relationships and differences involving default status. The results suggest that repayment behaviour variables have strong relationships with credit card default risk.
      </p>

      <p>
      Two regression models were considered in this study:
      <b>Multiple Linear Regression (MLR)</b> and 
      <b>Logistic Regression</b>.
      </p>

      <p>
      However, MLR was rejected because the dependent variable 
      <b>DEFAULT_BINARY</b> only contains two possible outcomes:
      <b>Default</b> and <b>No Default</b>. 
      MLR is more suitable for continuous response variables and may produce invalid probability predictions below 0 or above 1.
      </p>

      <p>
      <b>Logistic Regression</b> was selected as the final model because it is specifically designed for binary classification problems and produces probability values between 0 and 1.
      </p>

      <p>
      Overall, repayment history variables such as 
      <b>PAY_0</b>, <b>PAY_2</b>, and <b>PAY_3</b> were identified as important predictors of credit card default behaviour. Clients with higher repayment delays are generally more likely to default on credit card payments.
      </p>

      <p>
      In conclusion, this application provides a beginner-friendly platform for understanding statistical analysis, predictive modelling, and credit risk evaluation using real-world credit card client data.
      </p>
      </div>
"
    )
  })
  
  output$download_summary <- downloadHandler(
    
    filename = function() {
      paste0(
        "credit_default_analysis_report_FIXED_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".",
        input$download_type
      )
    },
    
    content = function(file) {
      
      data <- data_input()
      test_data <- train_test_data()$test
      
      prob <- predict(model_logistic(), newdata = test_data, type = "response")
      pred <- ifelse(prob >= input$threshold, "Default", "No Default")
      pred <- factor(pred, levels = c("No Default", "Default"))
      actual <- factor(test_data$DEFAULT, levels = c("No Default", "Default"))
      
      cm <- table(Predicted = pred, Actual = actual)
      
      accuracy <- sum(diag(cm)) / sum(cm)
      sensitivity <- safe_divide(cm["Default", "Default"], sum(cm[, "Default"]))
      specificity <- safe_divide(cm["No Default", "No Default"], sum(cm[, "No Default"]))
      
      roc_result <- roc(test_data$DEFAULT_BINARY, prob, quiet = TRUE)
      auc_value <- auc(roc_result)
      
      section_line <- "============================================================"
      
      report_lines <- c(
        "CREDIT CARD DEFAULT PAYMENT ANALYSIS REPORT",
        section_line,
        "",
        "1. DATASET OVERVIEW",
        section_line,
        paste("Number of Rows:", nrow(data)),
        paste("Number of Columns:", ncol(data)),
        "",
        
        "2. DATA QUALITY CHECK",
        section_line,
        paste("Total Missing Values:", sum(is.na(data))),
        if ("ID" %in% names(data)) paste("Duplicated IDs:", sum(duplicated(data$ID))) else "ID variable not found.",
        "",
        
        "3. SUMMARY STATISTICS",
        section_line,
        "Summary statistics are provided in table format in the PDF report.",
        "",
        
        "4. FREQUENCY TABLES",
        section_line,
        "SEX:",
        capture.output(print(table(data$SEX))),
        "",
        "EDUCATION GROUP:",
        capture.output(print(table(data$EDUCATION_GROUP))),
        "",
        "MARRIAGE:",
        capture.output(print(table(data$MARRIAGE))),
        "",
        "DEFAULT STATUS:",
        capture.output(print(table(data$DEFAULT))),
        "",
        
        "5. GRAPHICAL ANALYSIS",
        section_line,
        "Graphs are used to visualise default status, demographic distribution, credit limit, age, and repayment status patterns.",
        "",
        
        "6. HYPOTHESIS TESTING",
        section_line,
        paste("Selected Test:", input$test_type),
        capture.output(print(hypothesis_result())),
        "",
        
        "7. CORRELATION ANALYSIS",
        section_line,
        paste("Selected Correlation Method:", input$cor_method),
        "Correlation analysis measures the strength and direction of relationships between numerical variables.",
        "",
        
        "8. REGRESSION MODELS",
        section_line,
        "Multiple Linear Regression and Logistic Regression are compared.",
        "Logistic Regression is more suitable because the response variable is binary.",
        "",
        
        "9. LOGISTIC REGRESSION SUMMARY",
        section_line,
        capture.output(print(summary(model_logistic()))),
        "",
        
        "10. MODEL PERFORMANCE",
        section_line,
        "Confusion Matrix:",
        capture.output(print(cm)),
        "",
        paste("Classification Threshold:", input$threshold),
        paste("Accuracy:", round(accuracy, 4)),
        paste("Sensitivity:", round(sensitivity, 4)),
        paste("Specificity:", round(specificity, 4)),
        paste("AUC:", round(as.numeric(auc_value), 4)),
        "",
        
        "11. DIAGNOSTIC CHECKS",
        section_line,
        "VIF is used to check multicollinearity.",
        "Hosmer-Lemeshow Test is used to check goodness-of-fit.",
        "Diagnostic plots are used to check model assumptions.",
        "",
        
        "12. BEST MODEL SELECTION",
        section_line,
        "Multiple Linear Regression is rejected because DEFAULT_BINARY is binary.",
        "Logistic Regression is selected as the best model.",
        "",
        
        "13. FINAL CONCLUSION",
        section_line,
        "The application successfully analysed credit card default payment behaviour.",
        "Repayment history variables such as PAY_0, PAY_2, and PAY_3 are important predictors of default risk.",
        "Clients with higher repayment delays are generally more likely to default.",
        "Overall, Logistic Regression is the most suitable model for this analysis."
      )
      
      if (input$download_type == "txt") {
        
        writeLines(report_lines, file)
        
      } else if (input$download_type == "csv") {
        
        report_df <- data.frame(
          Report_Content = report_lines,
          stringsAsFactors = FALSE
        )
        
        write.csv(report_df, file, row.names = FALSE)
        
      } else if (input$download_type == "pdf") {
        
        pdf(file, width = 8.5, height = 11, family = "serif")
        
        title_cex <- 1.15
        header_cex <- 0.95
        text_cex <- 0.75
        table_cex <- 0.62
        
        categorical_columns <- c(
          "SEX", "EDUCATION", "MARRIAGE",
          "PAY_0", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6",
          "DEFAULT_BINARY", "DEFAULT"
        )
        
        summary_columns <- c(
          "ID", "LIMIT_BAL", "SEX", "EDUCATION", "MARRIAGE", "AGE",
          "PAY_0", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6",
          "BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6",
          "PAY_AMT1", "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6",
          "DEFAULT_BINARY", "DEFAULT"
        )
        
        summary_columns <- summary_columns[summary_columns %in% names(data)]
        
        numeric_cols <- summary_columns[
          sapply(data[, summary_columns], is.numeric) &
            !(summary_columns %in% categorical_columns)
        ]
        
        categorical_cols <- summary_columns[
          summary_columns %in% categorical_columns
        ]
        
        numeric_summary <- data.frame(
          Variable = numeric_cols,
          Minimum = round(sapply(data[, numeric_cols], min, na.rm = TRUE), 2),
          Q1 = round(sapply(data[, numeric_cols], quantile, 0.25, na.rm = TRUE), 2),
          Median = round(sapply(data[, numeric_cols], median, na.rm = TRUE), 2),
          Mean = round(sapply(data[, numeric_cols], mean, na.rm = TRUE), 2),
          Q3 = round(sapply(data[, numeric_cols], quantile, 0.75, na.rm = TRUE), 2),
          Maximum = round(sapply(data[, numeric_cols], max, na.rm = TRUE), 2),
          row.names = NULL
        )
        
        categorical_summary <- data.frame(
          Variable = character(),
          Category = character(),
          Frequency = numeric(),
          stringsAsFactors = FALSE
        )
        
        for (col in categorical_cols) {
          
          freq <- table(data[[col]])
          
          temp_df <- data.frame(
            
            # show variable name once only
            Variable = c(col, rep("", length(freq) - 1)),
            
            # category values
            Category = names(freq),
            
            # frequencies
            Frequency = as.vector(freq),
            
            stringsAsFactors = FALSE
          )
          
          categorical_summary <- rbind(
            categorical_summary,
            temp_df
          )
        }
        
        add_text_page <- function(lines) {
          
          wrapped_lines <- unlist(strwrap(lines, width = 85))
          
          lines_per_page <- 45
          total_pages <- ceiling(length(wrapped_lines) / lines_per_page)
          
          for (page in 1:total_pages) {
            
            plot.new()
            
            start_line <- (page - 1) * lines_per_page + 1
            end_line <- min(page * lines_per_page, length(wrapped_lines))
            
            page_lines <- wrapped_lines[start_line:end_line]
            
            # FIXED spacing
            y_start <- 0.95
            line_gap <- 0.028
            
            y_pos <- y_start - (0:(length(page_lines)-1)) * line_gap
            
            for (i in seq_along(page_lines)) {
              
              is_header <- grepl("^[0-9]+\\. |CREDIT CARD", page_lines[i])
              
              text(
                x = 0.07,
                y = y_pos[i],
                labels = page_lines[i],
                adj = c(0, 1),
                cex = ifelse(is_header, header_cex, text_cex),
                font = ifelse(is_header, 2, 1)
              )
            }
          }
        }
        
        add_table_page <- function(table_data, title, rows_per_page = 22) {
          
          total_pages <- ceiling(nrow(table_data) / rows_per_page)
          col_names <- names(table_data)
          n_col <- length(col_names)
          
          x_left <- 0.05
          table_width <- 0.90
          col_w <- rep(table_width / n_col, n_col)
          col_x <- x_left + c(0, cumsum(col_w[-length(col_w)]))
          
          for (page in 1:total_pages) {
            
            plot.new()
            
            text(
              x = 0.05,
              y = 0.96,
              labels = paste(title, ifelse(total_pages > 1, paste0("(Page ", page, ")"), "")),
              adj = c(0, 1),
              cex = title_cex,
              font = 2
            )
            
            start_row <- (page - 1) * rows_per_page + 1
            end_row <- min(page * rows_per_page, nrow(table_data))
            page_table <- table_data[start_row:end_row, ]
            
            row_height <- 0.035
            y_top <- 0.90
            
            for (j in seq_along(col_names)) {
              rect(col_x[j], y_top - row_height, col_x[j] + col_w[j], y_top)
              text(col_x[j] + 0.005, y_top - row_height / 2,
                   labels = col_names[j], adj = c(0, 0.5),
                   cex = table_cex, font = 2)
            }
            
            for (i in 1:nrow(page_table)) {
              y_row <- y_top - i * row_height
              
              for (j in seq_along(col_names)) {
                
                value <- as.character(page_table[i, j])
                
                if (nchar(value) > 18) {
                  value <- paste0(substr(value, 1, 18), "...")
                }
                
                rect(col_x[j], y_row - row_height, col_x[j] + col_w[j], y_row)
                text(col_x[j] + 0.005, y_row - row_height / 2,
                     labels = value, adj = c(0, 0.5),
                     cex = table_cex)
              }
            }
          }
        }
        
        add_text_page(
          c(
            "CREDIT CARD DEFAULT PAYMENT ANALYSIS REPORT",
            "",
            "1. DATASET OVERVIEW",
            paste("Number of Rows:", nrow(data)),
            paste("Number of Columns:", ncol(data)),
            "This dataset contains credit card client information such as credit limit, demographic details, repayment status, bill amount, payment amount, and default status.",
            "",
            "2. DATA QUALITY CHECK",
            paste("Total Missing Values:", sum(is.na(data))),
            if ("ID" %in% names(data)) paste("Duplicated IDs:", sum(duplicated(data$ID))) else "ID variable not found."
          )
        )
        
        add_table_page(numeric_summary, "3. NUMERICAL SUMMARY STATISTICS TABLE", rows_per_page = 20)
        
        add_table_page(categorical_summary, "4. CATEGORICAL FREQUENCY TABLE", rows_per_page = 24)
        
        add_text_page(
          c(
            "5. GRAPHICAL ANALYSIS",
            "Graphs help users visualise patterns in default status, demographics, credit limit, age, and repayment status.",
            "",
            "6. HYPOTHESIS TESTING",
            paste("Selected Test:", input$test_type),
            capture.output(print(hypothesis_result())),
            "",
            "7. CORRELATION ANALYSIS",
            paste("Selected Correlation Method:", input$cor_method),
            "Correlation analysis measures the strength and direction of relationships between numerical variables.",
            "",
            "8. REGRESSION MODELS",
            "Multiple Linear Regression and Logistic Regression are compared.",
            "",
            "9. LOGISTIC REGRESSION SUMMARY",
            capture.output(print(summary(model_logistic()))),
            "",
            "10. MODEL PERFORMANCE",
            "Confusion Matrix:",
            capture.output(print(cm)),
            paste("Classification Threshold:", input$threshold),
            paste("Accuracy:", round(accuracy, 4)),
            paste("Sensitivity:", round(sensitivity, 4)),
            paste("Specificity:", round(specificity, 4)),
            paste("AUC:", round(as.numeric(auc_value), 4)),
            "",
            "11. DIAGNOSTIC CHECKS",
            "VIF checks multicollinearity. Hosmer-Lemeshow Test checks Logistic Regression goodness-of-fit.",
            "",
            "12. BEST MODEL SELECTION",
            "Logistic Regression is selected as the best model because the response variable is binary.",
            "",
            "13. FINAL CONCLUSION",
            "Repayment history variables such as PAY_0, PAY_2, and PAY_3 are important predictors of default risk.",
            "Clients with higher repayment delays are generally more likely to default."
          )
        )
        
        dev.off()
      }
    }
  )
}

# =========================================================
# RUN APPLICATION
# =========================================================

shinyApp(ui = ui, server = server)
