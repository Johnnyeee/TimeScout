library(shiny)
library(fredr)
library(quantmod)
library(memoise)
library(forecast)
library(xts)
library(tidyverse)
library(keras)
library(shinyalert)
library(shinythemes)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(shinyBS)
library(rintrojs)
library(shinyjs)

##################### Define the user interface
ui <- dashboardPage(
  skin = "blue",
  title = "TimeScout",
  # Header ------------------------------------------------------------
  dashboardHeader(title = span(img(src = "logo4.svg", height = 50, style="cursor:pointer;", onclick="window.location.reload();")),
                  titleWidth = 230,
                    tags$li(
                      a(
                        img(src = "github.png", height = 17), 
                        strong("GITHUB", style = "font-family: Arial; font-style: Italic; font-size:12px"),
                        height = 40,
                        href = "https://github.com/Johnnyeee/TimeScout", 
                        target = "_blank"
                      ),
                      class = "dropdown"
                    ),
                    tags$li(
                      a(
                        img(src = "LinkedIn.svg", height = 17), 
                        strong("ABOUT ME", style = "font-family: Arial; font-style: Italic; font-size:12px"),
                        height = 40,
                        href = "https://www.linkedin.com/in/yujie-ye-9b301b271/",
                        title = "",
                        target = "_blank"
                      ),
                    class = "dropdown"
                  )
                  ),
  # Sidebar -----------------------------------------------------------
  dashboardSidebar(
    #Default Display
    sidebarMenu(
      id = "tab1",
      tags$li(tags$p("DASHBOARD", style = "padding-left: 10px; font-weight: bold; color: #406ad8;
                     padding-top: 20px; padding-bottom: 0px")),
      menuItem('Example Page', tabName = "example", icon = icon("wand-magic-sparkles"))
    ),
    sidebarMenu(
      id = "tab2",
      menuItem('Implement Page', tabName = "implement", icon = icon("magnifying-glass"))
    ),
    sidebarMenu(
      id = "tab3",
      tabName = "data",
      tags$li(tags$p("DISPLAY", style = "padding-left: 10px; font-weight: bold; color: #406ad8;
                     padding-top: 20px; padding-bottom: 0px")),
      menuItem('Visualizations', tabName = "visual", icon = icon("chart-simple"),
               selectizeInput("boxVisibility","",
                           choices = c("Data Preview" = "DataPreview",
                                       "Processed Data Plot" = "ProcessedDataPlot",
                                       "ACF/PACF Plot" = "ACFPACF",
                                       "Forecast Results" = "ForecastResult",
                                       "Records" = "Record"),
                           selected = c("DataPreview", "ProcessedDataPlot", "ACFPACF", "ForecastResult", "Record"),
                           multiple = TRUE,
                           options = list('plugins' = list('remove_button', 'drag_drop'), 'maxItems' = 5))
               ),
      tags$li(tags$p("DATA", style = "padding-left: 10px; font-weight: bold; color: #406ad8;
                     padding-top: 20px; padding-bottom: 0px")),
      menuItem("Fetch", tabName = "fetch", icon = icon("magnifying-glass-chart"), id = "fetchMenu",
               uiOutput("api_key_ui"),
               textInput("outcome_variable", "Outcome Variable", value = "DGS10"),
               selectInput("frequency", "Frequency", 
                           choices = c("Daily" = "d",
                                       "Weekly" = "w",
                                       "Biweekly" = "bw",
                                       "Monthly" = "m",
                                       "Quarterly" = "q",
                                       "Semiannual" = "sa",
                                       "Annual" = "a",
                                       "Weekly, ending Monday" = "wem",
                                       "Weekly, ending Tuesday" = "wetu",
                                       "Weekly, ending Wednesday" = "wew",
                                       "Weekly, ending Thursday" = "weth",
                                       "Weekly, ending Friday" = "wef",
                                       "Weekly, ending Saturday" = "wesa",
                                       "Weekly, ending Sunday" = "wesu",
                                       "Biweekly, ending Wednesday" = "bwew",
                                       "Biweekly, ending Monday" = "bwem"),
                           selected = "m"),
               dateInput("date_from", "Date From", value = Sys.Date() - 1000),
               dateInput("date_to", "Date To", value = as.Date("2024-05-31")),
               actionButton("fetch_data", "Fetch Data", class = "btn-primary"),
               style = "color: grey;"),
      menuItem("Process",
               uiOutput("engineering_ui"), icon = icon("gear"),
               style = "color: grey;"),
      tags$li(tags$p("MODEL", style = "padding-left: 10px; font-weight: bold; color: #406ad8;
                     padding-top: 20px; padding-bottom: 0px")),
      menuItem("Parameters", tabName = "model_forecast", icon = icon("user-gear"), id = "modelForecastMenu",
               class = "toggleMenuItem",
               uiOutput("model_ui"),
               uiOutput("acf_ui"),
               div(id = "save_message", style = "color: blue; font-weight: bold"),
               style = "color: grey;"),
      menuItem("Forecast", icon = icon("chart-line"),
               uiOutput("forecast_ui"),
               hidden(actionButton("add_to_table", "Save Results", class = "btn-info")),
               style = "color: grey;"),
      tags$li(tags$p("RESULT", style = "padding-left: 10px; font-weight: bold; color: #406ad8;
                     padding-top: 20px; padding-bottom: 0px")),
      menuItem(text = "Record", tabName = "results", icon = icon("table"),
               actionButton("add_to_table", "Save Result", class = "btn-info"),
               actionButton("reset_table", "Reset Table", class = "btn-warning"),
               style = "color: grey;")
    )
  ),
  # Body --------------------------------------------------------------
  dashboardBody(
    useShinyjs(), 
    introjsUI(),
    tags$script(HTML("
      $(document).on('shiny:connected', function(event) {
        function sendWidth() {
          Shiny.setInputValue('browser_width', $(window).width());
        }
        $(window).resize(sendWidth);
        sendWidth();
      });
    ")),
    tags$div(id = "loading", class = "loader", style = "display: none;"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "AdminLTE.min.css"),
              tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
              tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css"),
              tags$link(rel = "stylesheet", type = "text/css", href = "styles2.css",
                        tags$style(HTML("
        .value-box-wide { width: 200%; } /* Approx 10 of 12 columns in Bootstrap */
        .value-box-narrow { width: 100%; } /* Approx 3 of 12 columns in Bootstrap */
        /* CSS to make the header fixed */
        .main-header {
          position: fixed;
          width: 100%;
          z-index: 999;
          box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);
        }              
        /* CSS to make the sidebar fixed */
        .main-sidebar {
          position: fixed;
          height: calc(100% - 50px); /* Adjust based on header height */
          overflow-y: auto; /* Enables scrolling inside the sidebar */
        }
        /* Adjust content wrapper margin to not overlap with the header and sidebar */
        .content-wrapper {
          margin-left: 230px; /* Match the sidebar width or adjust as needed */
          margin-top: 50px;
        }
        .small-box.bg-blue { 
          background: linear-gradient(to right, #40b6f4, #18d9e4, #40b6f4);
          color: white !important;
          }
          .small-box.bg-red { 
          background: linear-gradient(to right,#a68fd3, #DF80E2, #a68fd3);
          color: white !important;
          }
          .small-box.bg-green { 
          background: linear-gradient(to right, #2ab48f, #90c742, #2ab48f);
          color: white !important;
          }
          .small-box.bg-orange { 
          background: linear-gradient(to right, orange, #ffc207, orange);
          color: white !important;
          }
          .small-box h3 {
            font-size: 30px;
            font-weight: bold;
            margin: 0 0 10px 0;
            white-space: nowrap;
            padding: 0;
          }
          .nav-tabs > li > a {
          font-size: 14px;  /* Set the font size */
          color: #575F78 !important;  /* Set the color */
          font-weight: bold;
          letter-spacing: 0.05em;
          }
          .loader {
            position: fixed; /* Changed from absolute to fixed for viewport-relative positioning */
            left: 56%;
            top: 50%;
            z-index: 100; /* High z-index to ensure it appears above other content */
            width: 120px;
            height: 120px;
            margin: -60px 0 0 -60px; /* Half of width and height to center exactly */
            border: 16px solid #f3f3f3; /* Light grey */
            border-top: 16px solid #7c95e3; /* light purple */
            border-radius: 50%;
            width: 120px;
            height: 120px;
            animation: spin 2s linear infinite;
          }
          @keyframes spin {
            0% { transform: rotate(0deg); }
            100% { transform: rotate(360deg); }
          }
      "))),
              
    ),
    tabItems(
      tabItem(
        tabName = "example",
        fluidRow(
          column(width = 12,
                 align = "left",
                 style = "margin-bottom: 20px; 
                 color: #495057; 
                 background-color: #F8F9FA;
                 padding: 20px;
                 ",  # Customize the style as needed
                 includeHTML("intro_dashboard.html") 
          )
        ),
        fluidRow(
          uiOutput("variable_value_box_ex"),
          uiOutput("engineering_value_box_ex"),
          uiOutput("model_value_box_ex"),
          uiOutput("forecast_value_box_ex")
        ),
        fluidRow(
          tabBox(id = "DataPreview_ex", height = "510px", tabPanel("Data Preview", DTOutput("data_input_ex"))),
          tabBox(id = "ProcessedDataPlot_ex", height = "510px", tabPanel("Processed Data Plot", plotlyOutput("processed_data_plot_ex"))),
          tabBox(id = "ACFPACF_ex", height = "510px", tabPanel("ACF Plot", plotOutput("ACF_ex")),
                tabPanel("PACF Plot", plotOutput("PACF_ex"))),
          tabBox(id = "ForecastResult_ex", height = "510px", tabPanel("Forecast Results", plotlyOutput("forecast_plot_ex"),
                                                                   uiOutput("output"))),
          tabBox(width = 12, id = "Record_ex", height = "510px", tabPanel("Records", DTOutput("records_table_ex")))
        )
      ),
      
      # Data input and processing tab
      tabItem(tabName = "implement", 
              fluidRow(
                fluidRow(
                  uiOutput("variable_value_box"),
                  uiOutput("engineering_value_box"),
                  uiOutput("model_value_box"),
                  uiOutput("forecast_value_box")
                ),
                fluidRow(
                  conditionalPanel(
                    condition = "input.boxVisibility.indexOf('DataPreview') > -1",
                    tabBox(id = "DataPreview", height = "510px", tabPanel("Data Preview", DTOutput("data_input")))
                  ),
                  conditionalPanel(
                    condition = "input.boxVisibility.indexOf('ProcessedDataPlot') > -1",
                    tabBox(id = "ProcessedDataPlot", height = "510px", tabPanel("Processed Data Plot", plotlyOutput("processed_data_plot")))
                  ),
                  conditionalPanel(
                    condition = "input.boxVisibility.indexOf('ACFPACF') > -1",
                    tabBox(id = "ACFPACF", height = "510px", tabPanel("ACF Plot", plotOutput("ACF")),
                           tabPanel("PACF Plot", plotOutput("PACF")))
                  ),
                  conditionalPanel(
                    condition = "input.boxVisibility.indexOf('ForecastResult') > -1",
                    tabBox(id = "ForecastResult", height = "510px", tabPanel("Forecast Results", plotlyOutput("forecast_plot"),
                                                                             uiOutput("output")))
                  ),
                  conditionalPanel(
                    condition = "input.boxVisibility.indexOf('Record') > -1",
                    tabBox(width = 12, id = "Record", height = "510px", tabPanel("Records", DTOutput("records_table")))
                  )
                )
          )
      )
    )
  )
)

######################
######################
######################
######################
###################### Define server logic
server <- function(input, output, session) {

  ################ Sidebar collapse based on brower width
  observe({
    if (is.null(input$browser_width)) return()
    
    # Check the width and toggle sidebar
    if (input$browser_width <= 1465) {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  ################
  
  ################ Example Page
  exampleData <- read.csv("www/DGS10.csv")
  data <- na.omit(exampleData)
  exampleData$DGS10 <- as.numeric(data$DGS10)
  # data fetch
  output$data_input_ex <- renderDT({
    datatable(exampleData, extensions = 'Buttons', 
              options = list(
                dom = 'Bfrtip',
                buttons = c('csv', 'excel', 'pdf'),
                pageLength = 10,
                scrollY = '300px',
                scrollCollapse = TRUE,
                paging = FALSE,
                info = FALSE,
                columnDefs = list(list(className = 'dt-center', targets = '_all'))
              ), colnames = c("Date", "DGS10"), class = "display compact")
  }, server = FALSE)
  
  temp_result_ex1 <- diff(exampleData$DGS10, differences = 12)
  temp_result_ex2 <- diff(temp_result_ex1, differences = 1)
  acfpacf_data_ex <- tibble(OV_values = temp_result_ex2)
  processed_plot_data_ex <- data.frame(Date = as.Date(exampleData$DATE[1:length(temp_result_ex2)]), OV_values = temp_result_ex2)
  
  # data processed plot
  output$processed_data_plot_ex <- renderPlotly({
    p <- ggplot(processed_plot_data_ex, aes(x = Date, y = OV_values))  +
      geom_line(color = "#007bff") +
      geom_point(aes(text = paste("Time:", Date, "DGS10:", OV_values)), size = 0.5, color = "#DC3545")+
      labs(title = paste("Processed Data Visualization: DGS10"), x = "Time", y = "Outcome Variable Values Adjusted")+theme_classic()
    ggplotly(p, tooltip = "text")
  })

  # ACF/PACF
  output$ACF_ex <- renderPlot({
    acf(acfpacf_data_ex, lag.max = 60, main = paste("ACF: DGS10"))
  })

  output$PACF_ex <- renderPlot({
    pacf(acfpacf_data_ex, lag.max = 60, main = paste("PACF: DGS10"))
  })
  
  original_data <- matrix(exampleData[[2]], ncol=1)
  original_date <- exampleData[[1]]
  
  split_ratio <- 0.8
  split_index <- floor(nrow(original_data) * split_ratio)
  train_data <- head(original_data, split_index)
  test_data <- original_data[(split_index + 1):nrow(original_data), ]
  
  fit <- Arima(train_data, order = c(1, 1, 0))
  forecast <- forecast(fit, h = 10)
  forecast_values <- forecast$mean
  forecast_data <- c(tail(train_data,1),forecast_values)
  forecast_date <- original_date[(length(train_data)):(length(train_data) + 10)]
  
  # Forecast plot
  output$forecast_plot_ex <- renderPlotly({
    exampleData=read_csv("www/DGS10.csv")
    data <- na.omit(exampleData)
    exampleData$DGS10 <- as.numeric(data$DGS10)
    
    original_data <- matrix(exampleData[[2]], ncol=1)
    original_date <- exampleData[[1]]
    
    split_ratio <- 0.8
    split_index <- floor(nrow(original_data) * split_ratio)
    train_data <- head(original_data, split_index)
    test_data <- original_data[(split_index + 1):nrow(original_data), ]
    
    fit <- Arima(train_data, order = c(1, 1, 0))
    forecast <- forecast(fit, h = 10)
    forecast_values <- forecast$mean
    forecast_data <- c(tail(train_data,1),forecast_values)
    forecast_date <- original_date[(length(train_data)):(length(train_data) + 10)]
    
    df <- na.omit(data.frame(
      Date =  c(original_date, forecast_date),
      Values = c(original_data, forecast_data),
      Category = c(rep("Original Data", length(original_data)), rep("Forecast Data", length(forecast_data)))
    ))
    
    p <- ggplot(df, aes(x = Date, y = Values, color = Category)) +
      geom_line() +
      geom_vline(aes(xintercept = as.numeric(forecast_date[1]), color = "Train/Test Split"), linetype = "dashed", size = 1,) +
      scale_color_manual(values = c("Original Data" = "#007bff", "Forecast Data" = "#DC3545", "Train/Test Split" = "gray")) +
      labs(title = "Forecasts Plot by Direct Method (p:1, d:1, q:0)", x = "Time", y = "DGS10")+
      theme_classic()+
      theme(plot.title = element_text(size=10))
      
    
    print(ggplotly(p))
    
  })
  
  # Record
  actual_values <- head(test_data,10)
  forecast_values <- forecast$mean[1:length(actual_values)]
  
  # Calculate RMSFE
  rmsfe <- sqrt(mean((actual_values - forecast_values)^2))
  
  # Calculate AIC
  aic <- fit$aic

  record <- data.frame(
    id = 1,
    outcome_variable = "DGS10",
    frequency = "Monthly",
    data_from = "2019-06-01",
    data_to = "2024-06-01",
    engineering = "De-season&trend",
    horizon = 10,
    multistep = "Direct",
    split_ratio = 0.8,
    model = "ARIMA",
    model_parameter = "1,1,0",
    RMSFE = round(rmsfe, 4),
    AIC = round(aic, 4)
  )
  
  output$records_table_ex <- renderDT({
    datatable(record, extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = c('csv', 'excel'),
      pageLength = 5,
      scrollX = TRUE,
      info = FALSE
    ), rownames = FALSE,
    colnames = c("ID",
                 "Outcome Variable",
                 "Frequency",
                 "Data From",
                 "Data To",
                 "Engineering",
                 "Horizon",
                 "Multistep Method",
                 "Split Ratio",
                 "Model",
                 "Model Parameters",
                 "RMSFE",
                 "AIC"),class = "display compact")
  }, server = FALSE)
  
  # Summary
  renderDynamicValueBox <- function(id, value, subtitle, icon, color) {
    output[[id]] <- renderUI({
      req(input$browser_width)
      box_width <- if (input$browser_width <= 1236) 12 else 3
      valueBox(value, subtitle, width = box_width, icon = icon, color = color)
    })
  }
  
  # Call the function for each value box
  renderDynamicValueBox("variable_value_box_ex", "DGS10", "Current Variable", icon("magnifying-glass-chart"), "blue")
  renderDynamicValueBox("engineering_value_box_ex", "De-season&trend", "Current Process", icon("gears"), "green")
  renderDynamicValueBox("model_value_box_ex", "ARIMA", "Current Model", icon("user-gear"), "orange")
  renderDynamicValueBox("forecast_value_box_ex", "10", "Current Horizons", icon("chart-line"), "red")
  
  # Nav to implement
  observe({
    navValue <- input$navToTab
    if (!is.null(navValue) && grepl("implement", navValue)) {
      updateTabsetPanel(session, "tab2", selected = "implement")
    }
  })
  
  ################
  
  #show intro modal
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("intro_text.html"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  #stagehub
  reactive_stage <- reactiveValues(
    engineering_visible = FALSE,
    model_visible = FALSE,
    acf_pacf_visible = FALSE,
    arima_parameter_visible = FALSE,
    sarima_parameter_visible = FALSE,
    lstm_parameter_visible = FALSE,
    forecast_visible = FALSE,
    summary_visible = FALSE,
    data_input = NULL,
    processed_data = NULL,
    api_update = TRUE,
    forecast_plot_visible = FALSE
  )
  
  #valuehub
  values <- reactiveValues(
    fetchClicked = TRUE,
    processClicked = TRUE,
    nextModelClicked = TRUE,
    parameterClicked = TRUE,
    forecastClicked = TRUE,
    recordClicked = TRUE,
    currentOutcomeVariable = NULL,
    currentOutcomeVariable2 = NULL,
    currentOutcomeVariable3 = NULL,
    vb_engineering = FALSE,
    vb_model = FALSE,
    vb_forecast = FALSE
  )
  
  # observeEvent(input$outcome_variable,{
  #   if (values$fetchClicked) {
  #     shinyjs::click("fetch_data")
  #     values$fetchClicked <- FALSE  # Disable further clicks until reset
  #   }
  # })

  ################ Summary Current Status
  # Dynamic valueBox for the outcome variable
  output$variable_value_box <- renderUI({
    current_value <- if (is.null(values$currentOutcomeVariable)) "N/A" else values$currentOutcomeVariable
    valueBox(value = current_value, "Current Variable", width = 3, icon = icon("magnifying-glass-chart"), color = 'blue')
  })
  
  # Dynamic valueBox for the engineering
  output$engineering_value_box <- renderUI({
    current_value <- if (values$vb_engineering == FALSE) "N/A" else values$currentEngineering
    valueBox(value = current_value, "Current Process", width = 3, icon = icon("gear"), color = 'green')
  })
  
  # Dynamic valueBox for the model
  output$model_value_box <- renderUI({
    current_value <- if (values$vb_model == FALSE) "N/A" else values$currentModel
    valueBox(value = current_value, "Current Model", width = 3, icon = icon("user-gear"), color = 'orange')
  })
  
  # Dynamic valueBox for the forecast horizon
  output$forecast_value_box <- renderUI({
    current_value <- if (values$vb_forecast == FALSE) "N/A" else values$currentHorizon
    valueBox(value = current_value, "Current Horizons", width = 3, icon = icon("chart-line"), color = 'red')
  })
  
  
  renderResponsiveValueBox <- function(id, label, icon, color) {
    output[[id]] <- renderUI({
      req(input$browser_width)  # Ensure browser_width is available before proceeding
      box_width <- if (input$browser_width <= 1236) 12 else 3
      
      current_value <- switch(id,
                              "variable_value_box" = if (is.null(values$currentOutcomeVariable)) "N/A" else values$currentOutcomeVariable,
                              "engineering_value_box" = if (values$vb_engineering == FALSE) "N/A" else values$currentEngineering,
                              "model_value_box" = if (values$vb_model == FALSE) "N/A" else values$currentModel,
                              "forecast_value_box" = if (values$vb_forecast == FALSE) "N/A" else values$currentHorizon
      )
      
      valueBox(value = current_value, subtitle = label, width = box_width, icon = icon, color = color)
    })
  }
  
  # Initialize valueBoxes
  renderResponsiveValueBox("variable_value_box", "Current Variable", icon("magnifying-glass-chart"), "blue")
  renderResponsiveValueBox("engineering_value_box", "Current Process", icon("gear"), "green")
  renderResponsiveValueBox("model_value_box", "Current Model", icon("user-gear"), "orange")
  renderResponsiveValueBox("forecast_value_box", "Current Horizons", icon("chart-line"), "red")
  
  
  
  ###################### Fetch data
  observeEvent(input$fetch_data, {
    shinyjs::show("loading")
    if (reactive_stage$api_update) {
      fredr_set_key("<Add Your Own API Key>")
    } else {
      fredr_set_key(input$api_key_input)
    }
    
    data <- NULL
    
    tryCatch({
      data_test <- tryCatch({
        fredr_series_observations(series_id = "GDP")
      }, error = function(e) {
        NULL  # Return NULL if there's an error
      })
      
      if (!is.null(data_test)) { 
        data <- tryCatch({
          fredr_series_observations(
            series_id = input$outcome_variable,
            observation_start = as.Date(input$date_from),
            observation_end = as.Date(input$date_to),
            frequency = input$frequency
          )
        }, error = function(e) {
          showNotification(paste("Please enter valid fetch data parameters or enter valid data frequency. (Check details in https://fred.stlouisfed.org/)"), type = "error")
          shinyjs::reset('outcome_variable')
          shinyjs::hide("loading")
          NULL
        })
      } else {
        showNotification(paste("API key is expired. Please enter your own API key or contact author."), type = "error")
        shinyjs::reset("outcome_variable") 
        reactive_stage$api_update <- FALSE
        output$api_key_ui <- renderUI({  # Trigger UI for entering API key
          tagList(
            textInput("api_key_input", "Enter your API Key", value = "")
          )
        })
      }
    }, error = function(e) {
      NULL
    })
    
    if(is.null(data) || nrow(data) == 0) {
      NULL
    } else {
      reactive_stage$engineering_visible <- TRUE
      data <- xts(data$value, order.by=data$date)
      data <- na.omit(data[, 1, drop = FALSE])
      if(!is.numeric(data)) data <- as.numeric(data)
      
      df <- data.frame(Date = index(data), OV_values = coredata(data))
      df$Date <- switch(input$frequency,
                        "Daily" = as.Date(df$Date),
                        "Weekly" = as.Date(df$Date),
                        "Monthly" = format(as.Date(df$Date), "%Y-%m"),
                        "Quarterly" = format(as.Date(df$Date), "%Y-%m"),
                        "Biannually" = format(as.Date(df$Date), "%Y-%m"),
                        "Annually" = format(as.Date(df$Date), "%Y"),
                        as.Date(df$Date))
      df$OV_values <- coredata(data)
      
      reactive_stage$data_input <- df
      
      values$currentOutcomeVariable <- input$outcome_variable 
      
      values$vb_engineering <- FALSE
      values$vb_model <- FALSE
      values$vb_forecast <- FALSE
      reactive_stage$processed_plot_data <- NULL
      reactive_stage$acf_pacf_visible = FALSE
      reactive_stage$forecast_visible = FALSE
      shinyjs::hide("loading")
    }
  })
  
  # DTtable1
  output$data_input <- renderDT({
    datatable(reactive_stage$data_input, extensions = 'Buttons', 
              options = list(
                dom = 'Bfrtip',
                buttons = c('csv', 'excel', 'pdf'),
                pageLength = 10,
                scrollY = '300px',
                scrollCollapse = TRUE,
                paging = FALSE,
                info = FALSE,
                columnDefs = list(list(className = 'dt-center', targets = '_all'))
    ), colnames = c("Date", values$currentOutcomeVariable), class = "display compact")
  }, server = FALSE)
  
  
  ################ Engineering
  output$engineering_ui <- renderUI({
    if(reactive_stage$engineering_visible) {
      tagList(
        selectInput("engineering", "Data Engineering", 
                    choices = c("Raw", "De-seasonal", "De-trend", "De-season&trend")),
        bsTooltip(id = "engineering", 
                  title = "Raw: Original Data;<br/>De-season: Differencing at lag n;<br/>De-trend: Differencing at lag 1", 
                  placement = "top", 
                  trigger = "hover focus"),
        uiOutput("seasonal_lag_ui"),
        actionButton("process_data", "Process Data", class = "btn-primary")
      )
    }
  })
  
  # observeEvent(input$engineering, {
  #   if (values$processClicked) {
  #     shinyjs::click("process_data")
  #     values$processClicked <- FALSE
  #   }
  # })
  
  
  output$seasonal_lag_ui <- renderUI({
    # Only render the seasonal_lag input when needed
    if (input$engineering %in% c("De-seasonal", "De-season&trend")) {
      numericInput("seasonal_lag", "Seasonal Lag (n)", 12)
    }
  })
  
  observeEvent(input$process_data, {
    shinyjs::show("loading")
    req(reactive_stage$data_input)
    
    Date <- reactive_stage$data_input$Date
    temp_data <- reactive_stage$data_input$OV_values
    if (input$engineering == "Raw") {
      temp_result <- temp_data
    } else if (input$engineering == "De-seasonal") {
      temp_result <- diff(temp_data, differences = input$seasonal_lag)
    } else if (input$engineering == "De-trend") {
      temp_result <- diff(temp_data, differences = 1)
    } else if (input$engineering == "De-season&trend") {
      temp_result1 <- diff(temp_data, differences = input$seasonal_lag)
      temp_result <- diff(temp_result1, differences = 1)
    }
    
    reactive_stage$processed_plot_data_with_time <- tibble(Date = as.Date(Date[1:length(temp_result)]), OV_values = temp_result)
    reactive_stage$processed_plot_data <- tibble(OV_values = temp_result)
    values$vb_engineering = TRUE
    values$currentOutcomeVariable2 <- input$outcome_variable
    values$currentEngineering <- input$engineering
    
    values$vb_model = FALSE
    values$vb_forecast = FALSE
    reactive_stage$acf_pacf_visible = FALSE
    reactive_stage$forecast_visible = FALSE
    shinyjs::hide("loading")
  })
  
  
  output$processed_data_plot <- renderPlotly({
    req(reactive_stage$processed_plot_data_with_time, reactive_stage$processed_plot_data)
    tryCatch({
      reactive_stage$model_visible <- TRUE
      p <- ggplot(reactive_stage$processed_plot_data_with_time, aes(x = Date, y = OV_values))  +
        geom_line(color = "#007bff") +
        geom_point(aes(text = paste("Time:", Date, "Outcome Variable Value:", OV_values)), size = 0.5, color = "#DC3545")+
        labs(title = paste("Processed Data Visualization:",values$currentOutcomeVariable2), x = "Time", y = "Outcome Variable Values Adjusted")+theme_classic()
      
      # Convert ggplot object to plotly
      ggplotly(p, tooltip = "text")},
      error = function(e){
        shinyalert(title = "Error", text = "Please Check the Value of Seasonal Lag Again", type = "error")
        shinyjs::reset("engineering")  # Reset the 'engineering' input
        shinyjs::hide("add_to_table")
        reactive_stage$model_visible <- FALSE
        reactive_stage$acf_pacf_visible = FALSE
        reactive_stage$arima_parameter_visible = FALSE
        reactive_stage$sarima_parameter_visible = FALSE
        reactive_stage$lstm_parameter_visible = FALSE
        reactive_stage$forecast_visible = FALSE
        reactive_stage$summary_visible = FALSE
        NULL
      })
  })
  
  ###################### Choose model and show ACF/PACF
  output$model_ui <- renderUI({
    if(reactive_stage$model_visible) {
      tagList(
        selectInput("model", "Choose Model", choices = c("ARIMA", "SARIMA"
                                                         #, "LSTM"
                                                         )),
        actionButton("next_model", "Confirm Model", class = "btn-primary")
      )
    }
  })
  
  # observeEvent(input$model, {
  #   if (values$nextModelClicked) {
  #     shinyjs::click("next_model")
  #     values$nextModelClicked <- FALSE  # Prevent multiple programmatically triggers
  #   }
  # })

  observeEvent(input$next_model, {
    shinyjs::show("loading")
    req(input$model)
    if(input$model == "ARIMA") {
      reactive_stage$acf_pacf_visible <- TRUE
      reactive_stage$arima_parameter_visible <- TRUE
      reactive_stage$sarima_parameter_visible <- FALSE
      reactive_stage$lstm_parameter_visible <- FALSE
      reactive_stage$summary_visible = FALSE
    } else if(input$model == "SARIMA") {
      reactive_stage$acf_pacf_visible <- TRUE
      reactive_stage$arima_parameter_visible <- FALSE
      reactive_stage$sarima_parameter_visible <- TRUE
      reactive_stage$lstm_parameter_visible <- FALSE
      reactive_stage$summary_visible = FALSE
    } else if(input$model == "LSTM") {
      reactive_stage$acf_pacf_visible <- FALSE
      reactive_stage$lstm_parameter_visible <- TRUE
      reactive_stage$sarima_parameter_visible <- FALSE
      reactive_stage$arima_parameter_visible <- FALSE
      reactive_stage$summary_visible = FALSE
    }
    else {
      reactive_stage$acf_pacf_visible <- FALSE
      reactive_stage$lstm_parameter_visible <- FALSE
      reactive_stage$sarima_parameter_visible <- FALSE
      reactive_stage$arima_parameter_visible <- FALSE
      reactive_stage$summary_visible = FALSE
    }
    values$currentOutcomeVariable3 <- input$outcome_variable
    values$vb_model = TRUE
    values$vb_forecast = FALSE
    values$currentModel <- input$model
    shinyjs::hide("loading")
    
  })
  
  
  processed_data_for_acf_pacf <- eventReactive(input$next_model, {
    req(reactive_stage$processed_plot_data) # Make sure processed data is available
    reactive_stage$processed_plot_data
  }, ignoreNULL = FALSE)
  
  
  # Plotting ACF
  output$ACF <- renderPlot({
    req(processed_data_for_acf_pacf(), reactive_stage$acf_pacf_visible) # Using the event-reactive data
    acf(processed_data_for_acf_pacf(), lag.max = 60, main = paste("ACF:",values$currentOutcomeVariable3))
  })
  
  # Plotting PACF
  output$PACF <- renderPlot({
    req(processed_data_for_acf_pacf(), reactive_stage$acf_pacf_visible) # Using the event-reactive data
    pacf(processed_data_for_acf_pacf(), lag.max = 60, main = paste("PACF:",values$currentOutcomeVariable3))
  })
  
  
  
  ###################### Choose parameters of model
  output$acf_ui <- renderUI(
    if(reactive_stage$arima_parameter_visible) {
      tagList(
        numericInput("p", "ARIMA-p", value = 0, min = 0, max = 10, step = 1),
        numericInput("d", "ARIMA-d", value = 0, min = 0, max = 10, step = 1),
        numericInput("q", "ARIMA-q", value = 0, min = 0, max = 10, step = 1),
        bsTooltip(id = "p", 
                  title = "AutoRegressive", 
                  placement = "top", 
                  trigger = "hover focus"),
        bsTooltip(id = "d", 
                  title = "Differencing", 
                  placement = "top", 
                  trigger = "hover focus"),
        bsTooltip(id = "q", 
                  title = "Moving Average", 
                  placement = "top", 
                  trigger = "hover focus"),
        actionButton("next_forecast", "Confirm Parameters", class = "btn-primary")
      )
    } else if(reactive_stage$sarima_parameter_visible) {
      tagList(
        numericInput("p", "SARIMA-p", value = 0, min = 0, max = 10, step = 1),
        numericInput("d", "SARIMA-d", value = 0, min = 0, max = 10, step = 1),
        numericInput("q", "SARIMA-q", value = 0, min = 0, max = 10, step = 1),
        numericInput("P", "SARIMA-P", value = 0, min = 0, max = 10, step = 1),
        numericInput("D", "SARIMA-D", value = 0, min = 0, max = 10, step = 1),
        numericInput("Q", "SARIMA-Q", value = 0, min = 0, max = 10, step = 1),
        numericInput("frequencies", "Seasonal Period (m)", 12, min = 1, step = 1),
        bsTooltip(id = "p", 
                  title = "AutoRegressive", 
                  placement = "top", 
                  trigger = "hover focus"),
        bsTooltip(id = "d", 
                  title = "Differencing", 
                  placement = "top", 
                  trigger = "hover focus"),
        bsTooltip(id = "q", 
                  title = "Moving Average", 
                  placement = "top", 
                  trigger = "hover focus"),
        bsTooltip(id = "P", 
                  title = "Seasonal AutoRegressive", 
                  placement = "top", 
                  trigger = "hover focus"),
        bsTooltip(id = "D", 
                  title = "Seasonal Differencing", 
                  placement = "top", 
                  trigger = "hover focus"),
        bsTooltip(id = "Q", 
                  title = "Seasonal Moving Average", 
                  placement = "top", 
                  trigger = "hover focus"),
        bsTooltip(id = "frequencies", 
                  title = "The number of observations per seasonal cycle (e.g., m=12 for monthly data with an annual cycle)", 
                  placement = "top", 
                  trigger = "hover focus"),
        actionButton("next_forecast", "Confirm Parameters", class = "btn-primary")
      )
    } else if(reactive_stage$lstm_parameter_visible) {
      tagList(
        numericInput("layer", "Number of Layers", 1),
        numericInput("batch", "Batch Size", 1),
        numericInput("learning", "Learning Rate", 0.01),
        selectInput("optimizer", "Optimizer", choices = c('SGD','Adam', 'RMSprop')),
        selectInput("loss", "Loss Function",choices = c('L1','L2')),
        actionButton("next_forecast", "Confirm Parameters", class = "btn-primary")
      )
    }
  )
  
  # Observers to make forecast UI visible
  observeEvent(input$next_forecast, {
    reactive_stage$forecast_visible <- TRUE
    reactive_stage$forecast_plot_visible <- FALSE
  })
  
  observeEvent(input$next_forecast, {
    shinyjs::show("loading")
    # Function to safely convert inputs to integers, defaulting to 0 if not a number
    safe_integer <- function(x) {
      if(is.numeric(x) && x == as.integer(x)) {
        return(as.integer(x))
      } else {
        return(-1)  # Default to 0 if not a valid integer
      }
    }
    
    # Apply safe_integer to each parameter
    p <- safe_integer(input$p)
    d <- safe_integer(input$d)
    q <- safe_integer(input$q)
    P <- if(input$model == "SARIMA") safe_integer(input$P) else 0
    D <- if(input$model == "SARIMA") safe_integer(input$D) else 0
    Q <- if(input$model == "SARIMA") safe_integer(input$Q) else 0
    frequent <- if(input$model == "SARIMA") safe_integer(input$frequencies) else 1
    
    # Check if any parameter is negative, indicating an invalid input
    if (p < 0 || d < 0 || q < 0 || P < 0 || D < 0 || Q < 0 || frequent <= 0) {
      shinyalert::shinyalert(
        title = "Invalid Input",
        text = "Please ensure all parameters are non-negative integers.",
        type = "error"
      )
      shinyjs::reset("next_model")
      reactive_stage$forecast_visible <- FALSE
      reactive_stage$summary_visible <- FALSE
      shinyjs::hide("add_to_table")
    } else {
      shinyjs::html("save_message", "Parameters Updated")
      shinyjs::show(id = "save_message", anim = TRUE, time = 0.5, animType = "fade")
      shinyjs::delay(1000, shinyjs::hide(id = "save_message", anim = TRUE, time = 2.5, animType = "fade"))
    }
    shinyjs::hide("loading")
  })
  
  
  
  # observeEvent(input$p, {
  #   if (values$parameterClicked) {
  #     shinyjs::click("next_forecast")
  #     values$parameterClicked <- FALSE  # Disable further clicks until reset
  #   }
  # })
  
  
  ###################### Forecast
  output$forecast_ui <- renderUI({
    if(reactive_stage$forecast_visible) {
      tagList(
        numericInput("horizon", "Forecast Horizon (steps)", 10, min = 1, step = 1),
        bsTooltip(id = "horizon", 
                  title = "NOTICE: Maximum of Horizon = Length of Testing Data", 
                  placement = "top", 
                  trigger = "hover focus"),
        selectInput("multistep", "Multi-step Method", choices = c("Direct", "Recursive")),
        selectInput("data_split", "Data Split Ratio",
                    choices = c("50/50" = 0.5, "60/40" = 0.6, "70/30" = 0.7, "80/20" = 0.8, "90/10" = 0.9),
                    selected = "0.8"),
        actionButton("forecast_parameters", "Generate Forecasting", class = "btn-primary")
      )
    }
  })
  
  # observeEvent(input$horizon, {
  #   if (values$forecastClicked) {
  #     shinyjs::click("forecast_parameters")
  #     values$forecastClicked <- FALSE
  #   }
  # })
  
  # Initialize results as a reactive value to store model fitting and forecasting results
  results <- reactiveValues(fit = NULL, real_fit = NULL, forecast = NULL)
  # Handle model selection and forecasting
  observeEvent(input$forecast_parameters, {
    shinyjs::show("loading")
    req(input$model, reactive_stage$model_visible, reactive_stage$data_input, input$data_split,
        reactive_stage$processed_plot_data, input$multistep)
    ts_data <- reactive_stage$data_input[[2]]
    split_ratio <- as.numeric(input$data_split)
    split_index <- floor(nrow(ts_data) * split_ratio)
    train_data <- head(ts_data, split_index)
    test_data <- ts_data[(split_index + 1):nrow(ts_data), ]
    reactive_stage$forecast_plot_visible <- TRUE
    values$vb_forecast = TRUE
    values$currentHorizon <- input$horizon
    ####################################
    if(input$model == "ARIMA") {
      results$real_fit <- Arima(train_data, 
                                order = c(input$p, input$d, input$q))
      if(input$multistep == "Recursive") {
        recursive_forecast <- numeric(input$horizon)
        for (i in 1:input$horizon) {
          forecast_value <- forecast(results$real_fit, h = 1)$mean
          recursive_forecast[i] <- forecast_value
          # Append forecast to training data and refit model
          train_data <- c(train_data, forecast_value)
          results$real_fit <- auto.arima(train_data)
        }
        results$forecast <- list(mean = recursive_forecast)
      } else {
        results$forecast <- forecast(results$real_fit, h = input$horizon)
      }
      ####################################
    } else if(input$model == "SARIMA"){
      data_ts <- ts(train_data, frequency = input$frequencies)
      results$real_fit <- Arima(data_ts, 
                                order = c(input$p, input$d, input$q),
                                seasonal = c(input$P, input$D, input$Q))
      if(input$multistep == "Recursive") {
        recursive_forecast <- numeric(input$horizon)
        for (i in 1:input$horizon) {
          forecast_value <- forecast(results$real_fit, h = 1)$mean
          recursive_forecast[i] <- forecast_value
          # Append forecast to training data and refit model
          data_ts <- c(data_ts, forecast_value)
          results$real_fit <- auto.arima(data_ts)
        }
        results$forecast <- list(mean = recursive_forecast)
      } else {
        results$forecast <- forecast(results$real_fit, h = input$horizon)
      }
      ####################################
    } else if(input$model == "LSTM") {
      showNotification("Coming Soon!.", type = "error")
    }
    else {showNotification("Selected model is not yet implemented.", type = "error")}
    shinyjs::hide("loading")
  })
  
  ####################################
  output$forecast_plot <- renderPlotly({
    if (reactive_stage$forecast_plot_visible){
        req(input$forecast_parameters,reactive_stage$forecast_visible)
        isolate({
          req(results$forecast, input$multistep, reactive_stage$data_input, input$frequency,
              input$outcome_variable, input$data_split, input$horizon)
          original_data_check <- reactive_stage$data_input
          original_data <- reactive_stage$data_input[[2]]
          split_ratio <- as.numeric(input$data_split)
          split_index <- floor(nrow(original_data) * split_ratio)
          train_data <- head(original_data, split_index)
          test_data <- original_data[(split_index + 1):nrow(original_data), ]
          forecast_value <- results$forecast$mean
          forecast_data <- c(tail(train_data,1), forecast_value)
          
          
          original_date <- reactive_stage$data_input[[1]]
          forecast_date <- original_date[(length(train_data)):(length(train_data) + input$horizon)]
          
          plot_df <- na.omit(data.frame(
            Date =  c(original_date, forecast_date),
            Values = c(original_data, forecast_data),
            Category = c(rep("Original Data", length(original_data)), rep("Forecast Data", length(forecast_data)))
          ))
          
          p <- ggplot(plot_df, aes(x = Date, y = Values, color = Category)) +
            geom_line() +
            geom_vline(aes(xintercept = as.numeric(forecast_date[1]), color = "Train/Test Split"), linetype = "dashed", size = 1,) +
            scale_color_manual(values = c("Original Data" = "#007bff", "Forecast Data" = "#DC3545", "Train/Test Split" = "gray")) +
            labs(
              title = paste(
                if (input$multistep == "Recursive") "Forecasts Plot by Recursive Method" else "Forecasts Plot by Direct Method",
                "(",
                if (input$model == "ARIMA") {
                  paste("p:", input$p, "d:", input$d, "q:", input$q, ")")
                } else if (input$model == "SARIMA") {
                  paste("p:", input$p, "d:", input$d, "q:", input$q, 
                        "P:", input$P, "D:", input$D, "Q:", input$Q, ")")
                } else {
                  ""
                }
              ),
              x = "Time", y = input$outcome_variable
            ) +
            theme_classic()+
            theme(plot.title = element_text(size=10))
          
          
          # Convert ggplot object to plotly
          print(ggplotly(p))
        })
    } else
      NULL
  })
  ############################## Show all parameters and error and AIC
  Errors <- reactiveValues(rmsfe = NA, aic = NA)
  
  observeEvent(input$forecast_parameters, {
    req(results$real_fit, results$forecast, reactive_stage$data_input)  # Ensure forecast and actual data are loaded
    
    ts_data <- reactive_stage$data_input[[2]]
    split_ratio <- as.numeric(input$data_split)
    split_index <- floor(nrow(ts_data) * split_ratio)
    train_data <- head(ts_data, split_index)
    test_data <- ts_data[(split_index + 1):nrow(ts_data), ]
    actual_values <- head(test_data,input$horizon)
    forecast_values <- results$forecast$mean[1:length(actual_values)]
    
    # Calculate RMSFE
    Errors$rmsfe <- sqrt(mean((actual_values - forecast_values)^2))
    
    # Calculate AIC
    Errors$aic <- results$real_fit$aic
    
    reactive_stage$summary_visible = TRUE
  })
  
  ############### Add on
  # Show the "Save Result" button when "Confirm Forecast Parameters" is clicked
  observeEvent(input$forecast_parameters,{
    shinyjs::show("add_to_table")
  })
  
  # observeEvent(input$horizon, {
  #   if (values$recordClicked) {
  #     shinyjs::click("add_to_table")
  #     values$recordClicked <- FALSE
  #   }
  # })
  
  ######################### Record
  records <- reactiveValues(data = data.frame())
  
  
  observeEvent(input$add_to_table, {
    # Ensure the required data is present
    req(reactive_stage$forecast_visible)
    
    # Initialize an empty parameter string
    parameter_string <- ""
    
    # Construct parameter string based on the model type
    if (input$model == "ARIMA") {
      parameter_string <- paste(input$p, input$d, input$q, sep = ", ")
    } else if (input$model == "SARIMA") {
      parameter_string <- paste(input$p, input$d, input$q, input$P, input$D, input$Q, 
                                input$frequencies, sep = ", ")
    } else if (input$model == "LSTM") {
      # Suppose LSTM parameters might include layers, batch size, etc.
      parameter_string <- paste(input$layer, input$batch, input$learning, 
                                input$optimizer, input$loss, sep = ", ")
    }
    
    frequency_full <- list(d = "Daily", w = "Weekly", bw = "Biweekly", m = "Monthly", q = "Quarterly",
                       sa = "Semiannual", a = "Annual", wem = "Weekly, ending Monday",
                       wetu = "Weekly, ending Tuesday", wew = "Weekly, ending Wednesday",
                       weth = "Weekly, ending Thursday", wef = "Weekly, ending Friday",
                       wesa = "Weekly, ending Saturday", wesu = "Weekly, ending Sunday",
                       bwew = "Biweekly, ending Wednesday", bwem = "Biweekly, ending Monday")
    
    # Add a new row to the records data frame
    new_record <- data.frame(
      id = nrow(records$data) + 1,
      outcome_variable = input$outcome_variable,
      frequency = frequency_full[[input$frequency]],
      data_from = input$date_from,
      data_to = input$date_to,
      engineering = input$engineering,
      horizon = input$horizon,
      multistep = input$multistep,
      split_ratio = input$data_split,
      model = input$model,
      model_parameter = parameter_string,
      RMSFE = round(Errors$rmsfe, 4),
      AIC = round(Errors$aic, 4)
    )
    
    records$data <- rbind(records$data, new_record)
  })
  
  output$records_table <- renderDT({
    datatable(records$data, extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = c('csv', 'excel'),
      pageLength = 5,
      scrollX = TRUE,
      info = FALSE
    ), rownames = FALSE,
    colnames = c("ID", 
                 "Outcome Variable",
                 "Frequency",
                 "Data From",
                 "Data To",
                 "Engineering",
                 "Horizon",
                 "Multistep Method",
                 "Split Ratio",
                 "Model",
                 "Model Parameters",
                 "RMSFE",
                 "AIC"),class = "display compact")
  }, server = FALSE)
  
  observeEvent(input$reset_table, {
    records$data <- data.frame()  # Reset the data frame
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
