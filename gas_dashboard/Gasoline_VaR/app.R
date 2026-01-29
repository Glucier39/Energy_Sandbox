library(shiny)
library(tidyverse)
library(xgboost)
library(riem)
library(plotly)

source("~/Documents/Github/Energy_Sandbox/gas_dashboard/load_data.R")

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .well { background-color: #f8f9fa; }
      .metric-box {
        background-color: #ffffff;
        border: 1px solid #dee2e6;
        border-radius: 5px;
        padding: 15px;
        margin: 10px 0;
        text-align: center;
      }
      .metric-value {
        font-size: 24px;
        font-weight: bold;
        color: #007bff;
      }
      .metric-label {
        font-size: 14px;
        color: #6c757d;
      }
    "))
  ),
  
  titlePanel("Gasoline Price VaR - Scenario Analysis Tool"),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      h4("Simulation Settings"),
      
      sliderInput(
        "forecast_days",
        "Forecast Horizon (days):",
        min = 7,
        max = 60,
        value = 30,
        step = 7
      ),
      
      selectInput(
        "n_sims",
        "Monte Carlo Simulations:",
        choices = c("1,000" = 1000,
                    "5,000" = 5000,
                    "10,000" = 10000),
        selected = 5000
      ),
      
      hr(),
      
      h4("Stock Level Scenarios"),
      helpText("Set weekly change rates for inventory levels"),
      
      sliderInput(
        "crude_weekly_change",
        "Crude Oil Weekly Change (M bbls):",
        min = -10,
        max = 10,
        value = 0,
        step = 0.5
      ),
      
      sliderInput(
        "gas_weekly_change",
        "Gasoline Weekly Change (M bbls):",
        min = -5,
        max = 5,
        value = -1.5,
        step = 0.25
      ),
      
      sliderInput(
        "distillate_weekly_change",
        "Distillate Weekly Change (M bbls):",
        min = -5,
        max = 5,
        value = 0,
        step = 0.25
      ),
      
      hr(),
      
      h4("Weather Scenario"),
      
      selectInput(
        "weather_scenario",
        "Temperature Pattern:",
        choices = c(
          "Normal Seasonal" = "normal",
          "Heat Wave (+10°F)" = "heat_wave",
          "Cool Summer (-10°F)" = "cool",
          "Extreme Heat (+20°F)" = "extreme_heat"
        )
      ),
      
      hr(),
      
      h4("Advanced Settings"),
      
      sliderInput(
        "volatility_mult",
        "Volatility Multiplier:",
        min = 0.5,
        max = 2.0,
        value = 1.0,
        step = 0.1
      ),
      
      selectInput(
        "var_level",
        "VaR Confidence Level:",
        choices = c("95%" = 0.95,
                    "99%" = 0.99),
        selected = 0.95
      ),
      
      hr(),
      
      actionButton(
        "run_simulation",
        "Run Simulation",
        class = "btn-primary btn-lg btn-block"
      ),
      
      br(),
      
      helpText("Simulation may take 5-30 seconds depending on settings.")
      
    ),
    
    mainPanel(
      width = 9,
      
      fluidRow(
        column(12,
               div(class = "well",
                   h4("Current Market State"),
                   fluidRow(
                     column(3,
                            div(class = "metric-box",
                                div(class = "metric-label", "Current Price"),
                                div(class = "metric-value", textOutput("current_price", inline = TRUE))
                            )
                     ),
                     column(3,
                            div(class = "metric-box",
                                div(class = "metric-label", "Crude Stocks"),
                                div(class = "metric-value", textOutput("current_crude", inline = TRUE))
                            )
                     ),
                     column(3,
                            div(class = "metric-box",
                                div(class = "metric-label", "Gas Stocks"),
                                div(class = "metric-value", textOutput("current_gas", inline = TRUE))
                            )
                     ),
                     column(3,
                            div(class = "metric-box",
                                div(class = "metric-label", "Last Update"),
                                div(class = "metric-value", textOutput("last_update", inline = TRUE))
                            )
                     )
                   )
               )
        )
      ),
      
      br(),
      
      tabsetPanel(
        
        tabPanel(
          "VaR Summary",
          br(),
          
          fluidRow(
            column(4,
                   div(class = "metric-box",
                       h5("VaR (95%)"),
                       div(class = "metric-value", textOutput("var_95", inline = TRUE)),
                       p("5% chance price falls below this", class = "metric-label")
                   )
            ),
            column(4,
                   div(class = "metric-box",
                       h5("Expected Price"),
                       div(class = "metric-value", textOutput("expected_price", inline = TRUE)),
                       p("Median outcome", class = "metric-label")
                   )
            ),
            column(4,
                   div(class = "metric-box",
                       h5("95% Confidence Interval"),
                       div(class = "metric-value", textOutput("ci_95", inline = TRUE)),
                       p("Range of likely outcomes", class = "metric-label")
                   )
            )
          ),
          
          br(),
          
          h4("Price Distribution at Forecast Horizon"),
          plotlyOutput("price_distribution", height = "400px"),
          
          br(),
          
          h4("Detailed Metrics"),
          tableOutput("summary_table")
        ),
        
        tabPanel(
          "Price Paths",
          br(),
          
          h4("Simulated Price Trajectories"),
          helpText("Showing confidence bands from Monte Carlo simulations"),
          plotlyOutput("fan_chart", height = "500px"),
          
          br(),
          
          h4("Individual Simulation Paths"),
          helpText("Sample of 100 random paths from the simulation"),
          plotlyOutput("spaghetti_plot", height = "400px")
        ),
        
        tabPanel(
          "Feature Evolution",
          br(),
          
          h4("How Stock Levels Evolve in Your Scenario"),
          plotOutput("stock_evolution", height = "400px"),
          
          br(),
          
          h4("Temperature Pattern"),
          plotOutput("temp_evolution", height = "300px")
        ),
        
        tabPanel(
          "Diagnostics",
          br(),
          
          h4("Model Performance Metrics"),
          verbatimTextOutput("model_diagnostics"),
          
          br(),
          
          h4("Historical Model Accuracy"),
          helpText("Based on walk-forward cross-validation"),
          plotOutput("cv_performance", height = "300px"),
          
          br(),
          
          h4("Feature Importance"),
          plotOutput("feature_importance", height = "400px")
        ),
        
        tabPanel(
          "About",
          br(),
          
          h3("How This Tool Works"),
          
          h4("1. XGBoost Price Model"),
          p("The core model predicts gasoline prices based on:"),
          tags$ul(
            tags$li("Lagged price movements (1-day, 3-day, 7-day)"),
            tags$li("Crude oil, gasoline, and distillate inventory levels"),
            tags$li("Weather patterns (temperature)"),
            tags$li("Seasonal factors (summer driving, hurricane season)")
          ),
          
          h4("2. Monte Carlo Simulation"),
          p("The model is run iteratively for each day in the forecast horizon:"),
          tags$ul(
            tags$li("Day 1: Uses real lagged features to predict price"),
            tags$li("Day 2: Uses Day 1 predicted price as lag to predict price"),
            tags$li("This continues for all forecast days"),
            tags$li("Random noise is added based on historical model error"),
            tags$li("Process repeats 1,000-10,000 times to build distribution")
          ),
          
          h4("3. Scenario Analysis"),
          p("Unlike pure statistical models, this tool lets you stress test:"),
          tags$ul(
            tags$li("Stock drawdown/buildup rates (supply shocks)"),
            tags$li("Weather patterns (demand shocks)"),
            tags$li("Volatility regimes (market uncertainty)")
          ),
          
          h4("4. Value at Risk (VaR)"),
          p("VaR answers: 'What's the worst outcome we'd expect 95% of the time?'"),
          p("Example: VaR 95 = $2.10 means there's a 5% chance prices fall below $2.10"),
          
          hr(),
          
          h4("Limitations"),
          tags$ul(
            tags$li("Model trained for 1-day predictions; multi-day forecasts accumulate error"),
            tags$li("Stock scenarios are linear trends; real inventory can be volatile"),
            tags$li("Does not account for supply disruptions or geopolitical shocks"),
            tags$li("Historical relationships may not hold in all market regimes")
          ),
          
          hr(),
          
          p("Model last trained:", strong("January 2025")),
          p("Data sources: EIA, FRED, NOAA")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  simulation_results <- reactiveValues(
    paths = NULL,
    final_prices = NULL,
    stock_paths = NULL,
    temp_paths = NULL,
    last_run = NULL
  )
  
  latest_data <- reactive({
    latest_market_state
  })
  
  output$current_price <- renderText({
    paste0("$", sprintf("%.2f", latest_data()$price))
  })
  
  output$current_crude <- renderText({
    paste0(round(latest_data()$crude_stocks / 1000, 1), "M")
  })
  
  output$current_gas <- renderText({
    paste0(round(latest_data()$gas_stocks / 1000, 1), "M")
  })
  
  output$last_update <- renderText({
    format(latest_data()$date, "%m/%d")
  })
  
  observeEvent(input$run_simulation, {
    
    withProgress(message = 'Running Monte Carlo Simulation...', value = 0, {
      
      n_sims <- as.numeric(input$n_sims)
      n_days <- input$forecast_days
      vol_mult <- input$volatility_mult
      
      scenario <- list(
        crude_weekly_change = input$crude_weekly_change * 1000,
        gas_weekly_change = input$gas_weekly_change * 1000,
        distillate_weekly_change = input$distillate_weekly_change * 1000,
        weather = input$weather_scenario
      )
      
      all_paths <- matrix(NA, nrow = n_sims, ncol = n_days)
      stock_crude <- matrix(NA, nrow = n_sims, ncol = n_days)
      stock_gas <- matrix(NA, nrow = n_sims, ncol = n_days)
      temp_paths <- matrix(NA, nrow = n_sims, ncol = n_days)
      
      init_state <- latest_data()
      
      incProgress(0.1, detail = "Initializing...")
      
      for (sim in 1:n_sims) {
        
        current_features <- init_state
        
        for (day in 1:n_days) {
          
          weeks_forward <- day / 7
          current_features$crude_stocks <- init_state$crude_stocks + 
            (scenario$crude_weekly_change * weeks_forward) +
            rnorm(1, 0, 5000)
          
          current_features$gas_stocks <- init_state$gas_stocks + 
            (scenario$gas_weekly_change * weeks_forward) +
            rnorm(1, 0, 3000)
          
          current_features$distillate_stocks <- init_state$distillate_stocks + 
            (scenario$distillate_weekly_change * weeks_forward) +
            rnorm(1, 0, 3000)
          
          current_features$gas_lag7 <- current_features$gas_stocks
          current_features$crude_lag7 <- current_features$crude_stocks
          current_features$distillate_lag7 <- current_features$distillate_stocks
          
          stock_crude[sim, day] <- current_features$crude_stocks
          stock_gas[sim, day] <- current_features$gas_stocks
          
          base_temp <- 75 + 10 * sin(2 * pi * day / 365)
          
          temp_adjustment <- switch(
            scenario$weather,
            "normal" = 0,
            "heat_wave" = 10,
            "cool" = -10,
            "extreme_heat" = 20
          )
          
          current_features$temp_avg <- base_temp + temp_adjustment + rnorm(1, 0, 5)
          
          current_features$temp_avg_lag1 <- current_features$temp_avg
          current_features$temp_avg_lag7 <- ifelse(day >= 7,
                                                   temp_paths[sim, day - 7],
                                                   init_state$temp_avg_lag7)
          current_features$temp_max_lag1 <- current_features$temp_avg + abs(rnorm(1, 0, 5))
          current_features$temp_min_lag1 <- current_features$temp_avg - abs(rnorm(1, 0, 5))
          
          temp_paths[sim, day] <- current_features$temp_avg
          
          current_date <- init_state$date + day
          current_features$month <- lubridate::month(current_date)
          current_features$is_summer <- as.numeric(current_features$month %in% 5:8)
          current_features$is_hurricane <- as.numeric(current_features$month %in% 8:10)
          
          predicted_price <- predict_price_real(current_features)
          
          actual_price <- predicted_price + rnorm(1, 0, historical_sd * vol_mult)
          
          all_paths[sim, day] <- actual_price
          
          current_features$price_lag7 <- current_features$price_lag3
          current_features$price_lag3 <- current_features$price_lag1
          current_features$price_lag1 <- actual_price
        }
        
        if (sim %% 100 == 0) {
          incProgress(0.9 / n_sims * sim, detail = paste("Simulation", sim, "of", n_sims))
        }
      }
      
      incProgress(1, detail = "Complete!")
      
      simulation_results$paths <- all_paths
      simulation_results$final_prices <- all_paths[, n_days]
      simulation_results$stock_paths <- list(
        crude = stock_crude,
        gas = stock_gas
      )
      simulation_results$temp_paths <- temp_paths
      simulation_results$last_run <- Sys.time()
      
    })
  })
  
  output$var_95 <- renderText({
    req(simulation_results$final_prices)
    
    alpha <- 1 - as.numeric(input$var_level)
    var_value <- quantile(simulation_results$final_prices, alpha)
    
    paste0("$", sprintf("%.2f", var_value))
  })
  
  output$expected_price <- renderText({
    req(simulation_results$final_prices)
    
    median_price <- median(simulation_results$final_prices)
    paste0("$", sprintf("%.2f", median_price))
  })
  
  output$ci_95 <- renderText({
    req(simulation_results$final_prices)
    
    ci <- quantile(simulation_results$final_prices, c(0.025, 0.975))
    paste0("$", sprintf("%.2f", ci[1]), " - $", sprintf("%.2f", ci[2]))
  })
  
  output$price_distribution <- renderPlotly({
    req(simulation_results$final_prices)
    
    final_prices <- simulation_results$final_prices
    alpha <- 1 - as.numeric(input$var_level)
    var_value <- quantile(final_prices, alpha)
    
    p <- plot_ly(x = final_prices, type = "histogram", nbinsx = 50,
                 marker = list(color = 'steelblue', line = list(color = 'white', width = 1))) %>%
      layout(
        title = paste("Distribution of", input$forecast_days, "Day Price"),
        xaxis = list(title = "Price ($/gallon)"),
        yaxis = list(title = "Frequency"),
        shapes = list(
          list(
            type = "line",
            x0 = var_value, x1 = var_value,
            y0 = 0, y1 = 1,
            yref = "paper",
            line = list(color = "red", width = 3, dash = "dash")
          )
        ),
        annotations = list(
          list(
            x = var_value,
            y = 0.9,
            yref = "paper",
            text = paste0("VaR: $", sprintf("%.2f", var_value)),
            showarrow = FALSE,
            bgcolor = "red",
            font = list(color = "white")
          )
        )
      )
    
    p
  })
  
  output$fan_chart <- renderPlotly({
    req(simulation_results$paths)
    
    paths <- simulation_results$paths
    n_days <- ncol(paths)
    
    percentiles <- apply(paths, 2, function(x) {
      quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
    })
    
    days <- 1:n_days
    
    p <- plot_ly() %>%
      add_ribbons(
        x = days,
        ymin = percentiles[1, ],
        ymax = percentiles[5, ],
        fillcolor = 'rgba(70, 130, 180, 0.2)',
        line = list(color = 'transparent'),
        name = '90% CI',
        showlegend = TRUE
      ) %>%
      add_ribbons(
        x = days,
        ymin = percentiles[2, ],
        ymax = percentiles[4, ],
        fillcolor = 'rgba(70, 130, 180, 0.4)',
        line = list(color = 'transparent'),
        name = '50% CI',
        showlegend = TRUE
      ) %>%
      add_lines(
        x = days,
        y = percentiles[3, ],
        line = list(color = 'blue', width = 3),
        name = 'Median'
      ) %>%
      layout(
        title = "Price Forecast with Confidence Bands",
        xaxis = list(title = "Days Forward"),
        yaxis = list(title = "Price ($/gallon)")
      )
    
    p
  })
  
  output$spaghetti_plot <- renderPlotly({
    req(simulation_results$paths)
    
    paths <- simulation_results$paths
    sample_idx <- sample(1:nrow(paths), min(100, nrow(paths)))
    sample_paths <- paths[sample_idx, ]
    
    n_days <- ncol(sample_paths)
    days <- 1:n_days
    
    p <- plot_ly()
    
    for (i in 1:nrow(sample_paths)) {
      p <- p %>% add_lines(
        x = days,
        y = sample_paths[i, ],
        line = list(color = 'rgba(100, 100, 100, 0.1)'),
        showlegend = FALSE,
        hoverinfo = 'skip'
      )
    }
    
    p <- p %>% layout(
      title = "Sample of Simulated Price Paths",
      xaxis = list(title = "Days Forward"),
      yaxis = list(title = "Price ($/gallon)")
    )
    
    p
  })
  
  output$stock_evolution <- renderPlot({
    req(simulation_results$stock_paths)
    
    crude <- simulation_results$stock_paths$crude
    gas <- simulation_results$stock_paths$gas
    
    n_days <- ncol(crude)
    days <- 1:n_days
    
    crude_median <- apply(crude, 2, median) / 1000
    gas_median <- apply(gas, 2, median) / 1000
    
    df <- data.frame(
      Day = rep(days, 2),
      Stock_Level = c(crude_median, gas_median),
      Type = rep(c("Crude Oil", "Gasoline"), each = n_days)
    )
    
    ggplot(df, aes(x = Day, y = Stock_Level, color = Type)) +
      geom_line(size = 1.5) +
      labs(
        title = "Projected Stock Level Evolution",
        x = "Days Forward",
        y = "Stock Level (Million Barrels)",
        color = "Commodity"
      ) +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  output$temp_evolution <- renderPlot({
    req(simulation_results$temp_paths)
    
    temp <- simulation_results$temp_paths
    n_days <- ncol(temp)
    days <- 1:n_days
    
    temp_median <- apply(temp, 2, median)
    temp_lower <- apply(temp, 2, quantile, 0.25)
    temp_upper <- apply(temp, 2, quantile, 0.75)
    
    df <- data.frame(
      Day = days,
      Median = temp_median,
      Lower = temp_lower,
      Upper = temp_upper
    )
    
    ggplot(df, aes(x = Day, y = Median)) +
      geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "orange", alpha = 0.3) +
      geom_line(color = "darkorange", size = 1.5) +
      labs(
        title = paste("Temperature Pattern:", input$weather_scenario),
        x = "Days Forward",
        y = "Temperature (°F)"
      ) +
      theme_minimal()
  })
  
  output$summary_table <- renderTable({
    req(simulation_results$final_prices)
    
    final <- simulation_results$final_prices
    current <- latest_data()$price
    
    data.frame(
      Metric = c(
        "Current Price",
        "Mean Forecast",
        "Median Forecast",
        "5th Percentile (VaR 95%)",
        "1st Percentile (VaR 99%)",
        "95th Percentile",
        "Standard Deviation",
        "Expected Return",
        "Worst Case (Min)",
        "Best Case (Max)"
      ),
      Value = c(
        paste0("$", sprintf("%.2f", current)),
        paste0("$", sprintf("%.2f", mean(final))),
        paste0("$", sprintf("%.2f", median(final))),
        paste0("$", sprintf("%.2f", quantile(final, 0.05))),
        paste0("$", sprintf("%.2f", quantile(final, 0.01))),
        paste0("$", sprintf("%.2f", quantile(final, 0.95))),
        paste0("$", sprintf("%.3f", sd(final))),
        sprintf("%.1f%%", (median(final) - current) / current * 100),
        paste0("$", sprintf("%.2f", min(final))),
        paste0("$", sprintf("%.2f", max(final)))
      )
    )
  }, striped = TRUE, hover = TRUE)
  
  output$model_diagnostics <- renderPrint({
    cat("Model Performance (Walk-Forward CV):\n")
    cat("=====================================\n")
    cat("Mean R²:     ", round(model_metadata$mean_r2, 4), "\n")
    cat("Mean RMSE:   $", round(model_metadata$mean_rmse, 4), "\n")
    cat("\n")
    cat("Model Details:\n")
    cat("- Algorithm: XGBoost\n")
    cat("- Training samples:", nrow(model_metadata$cv_summary), "\n")
    cat("- Features: 12\n")
    cat("- Last trained:", as.character(model_metadata$training_date), "\n")
  })
  
  output$cv_performance <- renderPlot({
    cv_data <- model_metadata$cv_summary
    
    ggplot(cv_data, aes(x = test_start, y = r2)) +
      geom_line(color = "blue", size = 1) +
      geom_point(size = 2) +
      geom_hline(yintercept = mean(cv_data$r2), linetype = "dashed", color = "red") +
      labs(title = "Model R² Over Time (Walk-Forward CV)",
           x = "Test Period",
           y = "R²") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$feature_importance <- renderPlot({
    importance_df <- model_metadata$feature_importance
    
    ggplot(importance_df, aes(x = reorder(Feature, Gain), y = Gain)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "XGBoost Feature Importance",
           x = "Feature",
           y = "Gain") +
      theme_minimal()
  })
  
}

predict_price_real <- function(features) {
  feature_vec <- c(
    features$price_lag1,
    features$price_lag3,
    features$price_lag7,
    features$gas_lag7,
    features$crude_lag7,
    features$distillate_lag7,
    features$temp_avg_lag1,
    features$temp_avg_lag7,
    features$temp_max_lag1,
    features$temp_min_lag1,
    features$is_summer,
    features$is_hurricane
  )
  
  feature_matrix <- matrix(feature_vec, nrow = 1)
  colnames(feature_matrix) <- c(
    "price_lag1", "price_lag3", "price_lag7",
    "gas_lag7", "crude_lag7", "distillate_lag7",
    "temp_avg_lag1", "temp_avg_lag7",
    "temp_max_lag1", "temp_min_lag1",
    "is_summer", "is_hurricane"
  )
  
  predicted <- predict(xgb_model, feature_matrix)
  
  return(as.numeric(predicted))
}

shinyApp(ui = ui, server = server)
