library(shiny)
library(quantmod)
library(ggplot2)
library(dplyr)
library(moments)
library(patchwork)
library(reshape2)
library(ggrepel)
library(knitr)
library(kableExtra)
# Background info for the 4 currencies
crypto_background <- list(
  "BTC-USD" = "Bitcoin (BTC) is the first decentralized cryptocurrency, launched in 2009. It is widely regarded as digital gold.",
  "ETH-USD" = "Ethereum (ETH) is a blockchain with smart contract functionality, launched in 2015. It powers DeFi, NFTs, and more.",
  "XRP-USD" = "XRP is the native cryptocurrency of the Ripple network, designed for fast and low-cost cross-border payments.",
  "BNB-USD" = "Binance Coin (BNB) is the exchange token of Binance, used for trading fee discounts, DeFi, and payments."
)

# Define UI
ui <- fluidPage(
  
  # --- Custom CSS ---
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
        background-color: #f5f7fa;
        color: #333333;
      }
      .shiny-input-container {
        margin-bottom: 15px;
      }
      .sidebar .well {
        background-color: #ffffff;
        border: 1px solid #e1e4e8;
        box-shadow: 2px 2px 5px rgba(0,0,0,0.05);
        padding: 20px;
        border-radius: 8px;
      }
      .main-panel {
        background-color: #ffffff;
        border: 1px solid #e1e4e8;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 2px 2px 10px rgba(0,0,0,0.05);
      }
      h4 {
        color: #2c3e50;
        font-weight: 600;
        margin-top: 20px;
        margin-bottom: 10px;
      }
      table {
        border-collapse: collapse;
        width: 100%;
      }
      table, th, td {
        border: 1px solid #ddd;
      }
      th {
        background-color: #2c3e50;
        color: white;
        padding: 8px;
        text-align: center;
      }
      td {
        padding: 8px;
        text-align: center;
      }
      .tab-content {
        margin-top: 15px;
      }
      .btn {
        background-color: #2c3e50;
        color: white;
        border-radius: 6px;
        border: none;
      }
      .btn:hover {
        background-color: #34495e;
        color: #ffffff;
      }
      .slider, .numericInput {
        margin-top: 10px;
      }
    "))
  ),
  
  # --- Your existing UI ---
  titlePanel("Modelling Cryptocurrency Markets Using the Laplace Scale Mixture Distributions"),
  sidebarLayout(
    sidebarPanel(
      selectInput("crypto", "Select Cryptocurrency:",
                  choices = c("BTC-USD", "ETH-USD", "XRP-USD", "BNB-USD"),
                  selected = "BTC-USD"),
      actionButton("load_data", "Load Data"),
      checkboxGroupInput("dist_choice", "Select Distribution(s):",
                         choices = c("Standard Laplace", "BLSM", "Asymmetric Laplace", "Power Function Laplace"),
                         selected = "Standard Laplace"),
      sliderInput("n_sim", "Number of Simulated Paths:",
                  min = 10, max = 1000, value = 1000, step = 10),
      actionButton("auto_play", "Play/Pause", icon = icon("play")),
      sliderInput("speed", "Animation Speed:",
                  min = 1, max = 10, value = 5, step = 1),
      numericInput("n_days", "Simulation Days:", 30, min = 1, max = 365),
      actionButton("analyze", "Analyze")
    ),
    # In your UI, modify the Abstract tab:
    mainPanel(
      tabsetPanel(
        tabPanel("Abstract", 
                 h3("Abstract", style = "color: #2c3e50; border-bottom: 2px solid #2c3e50; padding-bottom: 10px; font-size: 30px;"),
                 div(style = "text-align: justify; line-height: 1.6; font-size:20px",
                     p("Understanding the underlying distribution of cryptocurrency returns has become increasingly important in modern finance, particularly because cryptocurrencies are often considered potential hedging tools, similar to gold. As the normal distribution tends to underperform when modeling stock price data, there is a general consensus that it is also inadequate for cryptocurrency returns. This inadequacy arises primarily from the stylized facts observed in the data: highly peaked, heavy tails, and large kurtosis, with the tail behavior often following an inverse cubic power law."),
                     
                     p("In this study, we illustrate and highlight the relevance of the Laplace scale mixture (LSM) family of distributions in modelling cryptocurrency data. The standard Laplace distribution has a fixed kurtosis of 6 and the tails do not follow the inverse cubic law, which can potentially underestimate the likelihood of extreme events. To address this limitation, we consider the scale mixture family, which introduces flexibility in the tails while retaining the general Laplace structure."),
                     
                     p("Two Laplace scale mixture distributions are considered, namely the Bernoulli-Laplace and power-function Laplace. In the estimation phase, the maximum likelihood is used to obtain parameter estimates for the underlying distributions, while fitting the proposed models to four major cryptocurrency datasets. Model performance is evaluated using the Akaike Information Criterion (AIC), Bayesian Information Criterion (BIC), and likelihood ratio tests, as well as by comparing the estimated kurtosis to the empirical kurtosis."),
                     
                     p("We further extend the study by simulating price paths using geometric Brownian motion, drawing random values from the proposed models instead of a normal distribution, based on the estimated parameters. This approach allows one to predict the Value-at-Risk (VaR) for each cryptocurrency.")
                 ),
                 br(),
                 
                 h4("What This Application Does:", style = "color: #2c3e50; margin-top: 20px;font-size:30px"),
                 div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #2c3e50;font-size:25px",
                     tags$ul(style = "margin-bottom: 0;",
                             tags$li(strong("Data Loading & Exploration:"), "Load and visualize historical price data for BTC, ETH, XRP, and BNB with interactive time series plots"),
                             tags$li(strong("Distribution Fitting:"), "Fit four different Laplace-based distributions to cryptocurrency returns using maximum likelihood estimation"),
                             tags$li(strong("Model Comparison:"), "Compare model performance using AIC, BIC, likelihood ratio tests, and kurtosis analysis"),
                             tags$li(strong("Parameter Analysis:"), "View estimated parameters and theoretical moment properties for each distribution"),
                             tags$li(strong("Visual Diagnostics:"), "Examine distribution fits through histograms, density plots, and tail behavior analysis"),
                             tags$li(strong("Risk Simulation:"), "Simulate future price paths using Geometric Brownian Motion with Laplace-distributed innovations"),
                             tags$li(strong("Value at Risk:"), "Calculate and visualize 5% Value at Risk based on simulated price distributions")
                     )
                 ),
                 
                 br(),
                 h4("How to Use This Application:", style = "color: #2c3e50; font-size:30px;"),
                 div(style="font-size:25px;",
                 tags$ol(
                   tags$li(strong("Start:"), "Select a cryptocurrency and click 'Load Data' to see historical prices and returns.Check the Time series tab and Fitted data tab"),
                   tags$li(strong("Analyze:"), "Click 'Analyze' to fit distributions and compare models in the Parameters tab"),
                   tags$li(strong("Explore:"), "Use the Fitted Data tab to visualize distribution fits and tail behavior"),
                   tags$li(strong("Simulate:"), "Go to Price Paths & VaR to run Monte Carlo simulations and calculate risk metrics"),
                   tags$li(strong("Experiment:"), "Adjust simulation parameters and distribution selections to see different results")
                 )),
                 
                 br(),
                 p(em("Note: This application demonstrates the practical implementation of Laplace Scale Mixture distributions for cryptocurrency risk modeling as described in the research paper."))
        ),
        tabPanel("Time Series",
                 textOutput("narration"),
                 plotOutput("pricePlot"),
                 plotOutput("returnPlot")),
        tabPanel("Fitted Data", uiOutput("histPlotUI")),
        tabPanel("Parameters", uiOutput("paramUI"), uiOutput("momentTable")),
        tabPanel("Price Paths & VaR", uiOutput("priceSimUI"))
      )
    )
  )
)


# Server logic
server <- function(input, output, session) {
  # Reactive value to store animation state
  auto_play <- reactiveVal(FALSE)
  
  # Observe play button
  observeEvent(input$auto_play, {
    auto_play(!auto_play())
  })
  
  # Animate the slider
  observe({
    if (auto_play()) {
      invalidateLater(1000 / input$speed)  # Adjust speed
      
      # Cycle through values
      current <- input$n_sim
      next_val <- ifelse(current >= 1000, 10, current + 10)
      updateSliderInput(session, "n_sim", value = next_val)
    }
  })
  
  
  # Reactive storage for prices + returns
  crypto_data <- eventReactive(input$load_data, {
    getSymbols(input$crypto, src = "yahoo",
               from = "2017-01-01",
               to   = "2025-01-01",
               auto.assign = FALSE) %>%
      Cl() %>%
      `colnames<-`("Price")
    
    
    
    
    # Convert actual BTC prices to data frame for plotting
   
   
  })
  
  crypto_data2 <- eventReactive(input$load_data, {
    getSymbols(input$crypto, src = "yahoo", from = '2025-01-01', to = "2025-02-01", auto.assign = TRUE)
    new_prices <- Cl(get(input$crypto))
    
    data.frame(
      Day = 0:(length(new_prices) - 1),
      Price = as.numeric(new_prices)
    )
  })
  
  returns_data <- reactive({
    prices <- crypto_data()
    returns <- diff(log(prices))
    na.omit(returns)
  })
  
  # --- Narration ---
  output$narration <- renderText({
    req(crypto_data())
    df <- data.frame(Date = index(crypto_data()), Price = as.numeric(crypto_data()))
    start_date <- min(df$Date)
    end_date   <- max(df$Date)
    last_price <- tail(df$Price, 1)
    vol        <- round(sd(as.numeric(returns_data())) * sqrt(252) * 100, 2)
    
    paste0(
      "This is the data of ", input$crypto,
      " from ", format(start_date, "%Y-%m-%d"),
      " to ", format(end_date, "%Y-%m-%d"), ". ",
      "The last closing price is $", round(last_price, 2), ". ",
      "The annualized volatility is about ", vol, "%.\n\n",
      crypto_background[[input$crypto]]
    )
  })
  
  output$simNarration <- renderText({
    sim <- priceSimData()
    
    # Compute some summary stats
    P0 <- sim$P0
    VaR <- round(sim$VaR_95, 2)
    n_paths <- sim$n_paths
    n_days <- sim$n_days
    
    paste0(
      "Simulation Overview:\n",
      "- Initial Price (P₀): $", round(P0, 2), "\n",
      "- Number of Simulated Paths: ", n_paths, "\n",
      "- Time Horizon: ", n_days, " days\n",
      "- 5% Value at Risk (VaR): $", VaR, 
      " (i.e., there is a 5% probability the final price will fall below this level)\n",
      "- The red line represents the average of all simulated paths.\n",
      "- The black dashed line shows the actual historical price."
    )
  })
  
  output$momentTable <- renderUI({
    withMathJax(
      HTML("
      <h4>Moment Properties for BLSM and PFL Distributions:</h4>
      <table border='1' style='border-collapse: collapse; width: 100%;'>
        <tr>
          <th>Moment</th>
          <th>BLSM</th>
          <th>PFL</th>
        </tr>
        <tr>
          <td>Mean $$\\mathbb{E}[X]$$</td>
          <td>$$\\mu$$</td>
          <td>$$\\mu$$</td>
        </tr>
        <tr>
          <td>Variance $$\\mathrm{Var}(X)$$</td>
          <td>$$2\\beta^2 [\\theta_1 + (1-\\theta_1)\\theta_2^2]$$</td>
          <td>For $$\\theta>2$$: $$\\mathrm{Var}(X) = \\frac{2\\beta^2\\theta}{\\theta-2}$$</td>
        </tr>
        <tr>
          <td>Kurtosis $$\\mathrm{K}(X)$$</td>
          <td>$$6 \\cdot \\frac{\\theta_1 + (1-\\theta_1)\\theta_2^4}{(\\theta_1 + (1-\\theta_1)\\theta_2^2)^2}$$</td>
          <td>For $$\\theta>4$$: $$\\kappa(X) = \\frac{6(\\theta-2)^2}{\\theta(\\theta-4)}$$</td>
        </tr>
      </table>
    ")
    )
  })
  
  
  # --- Simulation functions available everywhere ---
  rlaplace <- function(n, mu, beta) {
    u <- runif(n) - 0.5
    mu - beta * sign(u) * log(1 - 2 * abs(u))
  }
  
  rAL <- function(n, mu, beta, kappa) {
    u <- runif(n)
    ifelse(u < (kappa^2)/(1+kappa^2), mu + beta*kappa*rexp(n), mu - beta/kappa*rexp(n))
  }
  
  rlsm <- function(n, mu, beta, theta1, theta2) {
    component <- rbinom(n, 1, theta1)
    xout <- numeric(n)
    if (sum(component == 1) > 0) xout[component == 1] <- rlaplace(sum(component == 1), mu, beta)
    if (sum(component == 0) > 0) xout[component == 0] <- rlaplace(sum(component == 0), mu, theta2 * beta)
    xout
  }
  
  output$pricePlot <- renderPlot({
    req(crypto_data())
    df <- data.frame(Date = index(crypto_data()), Price = as.numeric(crypto_data()))
    
    ggplot(df, aes(x = Date, y = Price)) +
      geom_line(color = "#1f77b4", linewidth = 0.8) +
      geom_point(data = df %>% slice(which.min(Date), which.max(Date)), 
                 aes(x = Date, y = Price), color = "#1f77b4", size = 2) +
      geom_text(data = df %>% slice(which.max(Date)), 
                aes(label = paste("Current: $", round(Price, 2))), 
                nudge_x = -nrow(df)*0.2, nudge_y = max(df$Price)*0.05, 
                color = "#1f77b4", size = 3.5, fontface = "bold") +
      scale_y_continuous(
        labels = scales::dollar_format(),
        breaks = scales::pretty_breaks(n = 6)
      ) +
      scale_x_date(
        date_breaks = "1 year",
        date_labels = "%Y",
        expand = expansion(mult = c(0.02, 0.05))
      ) +
      labs(
        title = paste(input$crypto, "Price History"),
        subtitle = paste("Data from", format(min(df$Date), "%B %d, %Y"), "to", 
                         format(max(df$Date), "%B %d, %Y")),
        x = NULL,
        y = "Price (USD)",
        caption = paste("Total Return:", 
                        round((last(df$Price) - first(df$Price)) / first(df$Price) * 100, 1), "%")
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
        plot.caption = element_text(size = 10, face = "bold", color = "#1f77b4", 
                                    margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
        axis.text = element_text(color = "black", size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(20, 20, 20, 20)
      )
  })
  output$returnPlot <- renderPlot({
    req(returns_data())
    df <- data.frame(Date = index(returns_data()), LogReturn = as.numeric(returns_data()))
    
    ggplot(df, aes(x = Date, y = LogReturn)) +
      geom_line(color = "darkred", linewidth = 0.6) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.5, linetype = "solid") +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 0.1),
        breaks = scales::pretty_breaks(n = 6)
      ) +
      scale_x_date(
        date_breaks = "1 year",
        date_labels = "%Y",
        expand = expansion(mult = c(0.02, 0.02))
      ) +
      labs(
        title = paste(input$crypto, "Daily Log Returns"),
        subtitle = "Daily percentage returns over time",
        x = NULL,
        y = "Daily Log Return (%)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
        axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
        axis.text = element_text(color = "black", size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(20, 20, 20, 20)
      )
  })
  # --- Tabs show placeholder until Analyze ---
  output$paramUI <- renderUI({
    if (input$analyze == 0) {
      h4("You have to click the Analyze button before analysis can render.")
    } else {
      tagList(
        h4("Fitted Parameters:"),
        tableOutput("paramTable"),
        br(),
        
        h4("Model Selection Criteria (Log-Likelihood, AIC, BIC):"),
        tableOutput("modelCriteria"),
        br(),
        
        h4("Likelihood Ratio Test Results:"),
        tableOutput("lrtTable")
      )
    }
  })
  
  
  
  
  output$simHistUI <- renderUI({
    if (input$analyze == 0) h4("You have to click the Analyze button before analysis can render.")
    else plotOutput("simHistPlot")
  })
  
  output$priceSimUI <- renderUI({
    if (input$analyze == 0) {
      h4("You have to click the Analyze button before analysis can render.")
    } else {
      tagList(
        plotOutput("priceSimPlot"),
        br(),
        textOutput("simNarration")   # <-- narration below the plot
      )
    }
  })
  
  
  output$histPlotUI <- renderUI({
    if (input$analyze == 0) {
      tagList(
        plotOutput("histPlotInitial", height = "400px"),
        br(),
      
        p("This plot shows the initial distribution of log returns (daily percentage changes) of the selected cryptocurrency. ",
          "Log returns are calculated as the difference of the natural logarithm of consecutive closing prices. ",
          "They are preferred in financial modeling because they are additive over time and approximate continuously compounded returns. ",
          "The histogram highlights the volatility, skewness, and potential extreme movements (fat tails) in the cryptocurrency price data.")
      )
    } else {
      tagList(
        plotOutput("histCombined", height = "400px"),
        plotOutput("tailPlot", height = "400px"),
        br(),
        h5("Narration:"),
        p("After analysis, the combined histogram and tail plots show the detailed distribution of returns. ",
          "The main histogram displays overall daily return distribution, while the tail plot emphasizes extreme negative or positive returns. ",
          "This helps visualize risk, asymmetry, and potential outliers in the cryptocurrency market.")
      )
    }
  })
  
  
  output$histPlotInitial <- renderPlot({
    req(returns_data())
    df <- data.frame(LogReturn = as.numeric(returns_data()))
    
    # Calculate basic statistics
    mean_return <- mean(df$LogReturn, na.rm = TRUE)
    sd_return <- sd(df$LogReturn, na.rm = TRUE)
    
    ggplot(df, aes(x = LogReturn)) +
      geom_histogram(
        aes(y = ..density..), 
        bins = 30, 
        fill = "#1f77b4", 
        color = "white", 
        alpha = 0.8
      ) +
      
      geom_vline(xintercept = mean_return, color = "darkorange", linewidth = 1, linetype = "dashed") +
      scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(
        title = paste(input$crypto, "Distribution of Daily Log Returns"),
        subtitle = paste("Mean:", round(mean_return * 100, 3), "%, Standard Deviation:", round(sd_return * 100, 3), "%"),
        x = "Daily Log Return",
        y = "Density"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  })
  
  # --- Reactive values to store fitted parameters ---
  fitted_params <- reactiveValues(
    mu_hat = NULL,
    beta_hat = NULL,
    mu_mle = NULL,
    beta_mle = NULL,
    theta1_mle = NULL,
    theta2_mle = NULL,
    mu_alaplace = NULL,
    beta_alaplace = NULL,
    kappa_alaplace = NULL,
    mu_pfl = NULL,
    beta_pfl = NULL,
    theta_pfl = NULL
  )
  
  # --- Analyze logic ---
  observeEvent(input$analyze, {
    req(returns_data())
    x <- na.omit(as.numeric(returns_data()))
    fitted_params$x <- x 
    prices <- crypto_data()
    n <- length(x)
    
    # 1. Symmetric Laplace density function
    dlaplace <- function(x, mu, beta) {
      (1 / (2 * beta)) * exp(-abs(x - mu) / beta)
    }
    
    # 2. Asymmetric Laplace density function
    dalaplace <- function(x, mu, beta, kappa) {
      dens <- (kappa / (beta * (1 + kappa^2))) *
        ifelse(x >= mu, exp(-(x - mu) * kappa / beta),
               exp((x - mu) / (kappa * beta)))
      pmax(dens, 1e-12)
    }
    
    # 3. LSM density function
    dlsm <- function(x, mu, beta, theta1, theta2) {
      dens <- theta1 * dlaplace(x, mu, beta) + (1 - theta1) * dlaplace(x, mu, theta2 * beta)
      pmax(dens, 1e-12)
    }
    
    # 4. PF-L density function
    dpfl <- function(x, mu, beta, theta) {
      if (beta <= 0 || theta <= 0) return(0)
      
      abs_xmu <- pmax(abs(x - mu), 1e-12)
      term1 <- (theta / 2) * beta^theta / (abs_xmu^(theta + 1))
      gamma_term <- gamma(theta + 1)
      upper_gamma <- gamma_term * pgamma(abs_xmu / beta, shape = theta + 1, lower.tail = FALSE)
      
      return(term1 * pmax(gamma_term - upper_gamma, 0))
    }
    
    # 5. Negative log-likelihood functions
    neg_loglik_lsm <- function(params) {
      mu <- params[1]
      beta <- params[2]
      theta1 <- params[3]
      theta2 <- params[4]
      
      if (beta <= 0 || theta1 < 0 || theta1 > 1 || theta2 <= 1) return(Inf)
      -sum(log(dlsm(x, mu, beta, theta1, theta2)))
    }
    
    neg_loglik_alaplace <- function(params) {
      mu <- params[1]
      beta <- params[2]
      kappa <- params[3]
      
      if (beta <= 0 || kappa <= 0) return(Inf)
      dens <- dalaplace(x, mu, beta, kappa)
      -sum(log(dens + 1e-12))
    }
    
    neg_loglik_pfl <- function(par) {
      mu <- par[1]
      beta <- par[2]
      theta <- par[3]
      
      if (beta <= 1e-6 || theta <= 1e-6) return(1e12)
      dens <- dpfl(x, mu, beta, theta)
      -sum(log(pmax(dens, 1e-12)))
    }
    
    # 6. Initial parameter guesses
    start_params_lsm <- c(
      mu = median(x),
      beta = mad(x) / 1.4826,
      theta1 = 0.5,
      theta2 = 2.0
    )
    
    start_params_alaplace <- c(
      mu = median(x),
      beta = mad(x) / 1.4826,
      kappa = 1.0
    )
    
    start_params_pfl <- c(
      mu = median(x),
      beta = mad(x) / 1.4826,
      theta = 2.0
    )
    
    # 7. Optimize using L-BFGS-B
    fit_lsm <- optim(
      par = start_params_lsm,
      fn = neg_loglik_lsm,
      method = "L-BFGS-B",
      lower = c(-Inf, 1e-6, 0, 1 + 1e-6),
      upper = c(Inf, Inf, 1, Inf)
    )
    
    fit_alaplace <- optim(
      par = start_params_alaplace,
      fn = neg_loglik_alaplace,
      method = "L-BFGS-B",
      lower = c(-Inf, 1e-6, 1e-6),
      upper = c(Inf, Inf, Inf)
    )
    
    fit_pfl <- optim(
      par = start_params_pfl,
      fn = neg_loglik_pfl,
      method = "L-BFGS-B",
      lower = c(-Inf, 1e-6, 1e-6),
      upper = c(Inf, Inf, Inf)
    )
    
    # 8. Extract MLE estimates
    fitted_params$mu_mle <- fit_lsm$par[1]
    fitted_params$beta_mle <- fit_lsm$par[2]
    fitted_params$theta1_mle <- fit_lsm$par[3]
    fitted_params$theta2_mle <- fit_lsm$par[4]
    
    fitted_params$mu_alaplace <- fit_alaplace$par[1]
    fitted_params$beta_alaplace <- fit_alaplace$par[2]
    fitted_params$kappa_alaplace <- fit_alaplace$par[3]
    
    fitted_params$mu_pfl <- fit_pfl$par[1]
    fitted_params$beta_pfl <- fit_pfl$par[2]
    fitted_params$theta_pfl <- fit_pfl$par[3]
    
    # 9. Fit standard Laplace
    fitted_params$mu_hat <- median(x)
    loglike_laplace <- function(beta) {
      if (beta <= 0) return(-Inf)
      -n * log(2 * beta) - (1 / beta) * sum(abs(x - fitted_params$mu_hat))
    }
    fitted_params$beta_hat <- optimize(loglike_laplace, interval = c(1e-6, 1), maximum = TRUE)$maximum
    
    # 10. Calculate moments for the empirical data
    empirical_skewness <- skewness(x)
    empirical_kurtosis <- kurtosis(x)
    empirical_variance <- var(x)
    
    # Theoretical moments for standard Laplace distribution
    laplace_skewness <- 0
    laplace_kurtosis <- 6
    laplace_variance <- 2 * fitted_params$beta_hat^2
    
    # Theoretical moments for LSM distribution
    lsm_skewness <- 0  # Symmetric mixture
    lsm_variance <- fitted_params$theta1_mle * 2 * fitted_params$beta_mle^2 +
      (1 - fitted_params$theta1_mle) * 2 * (fitted_params$theta2_mle * fitted_params$beta_mle)^2
    
    lsm_fourth_moment <- fitted_params$theta1_mle * 24 * fitted_params$beta_mle^4 +
      (1 - fitted_params$theta1_mle) * 24 * (fitted_params$theta2_mle * fitted_params$beta_mle)^4
    
    lsm_kurtosis <- 6 * (fitted_params$theta1_mle +
                           (1 - fitted_params$theta1_mle) * fitted_params$theta2_mle^4) /
      (fitted_params$theta1_mle +
         (1 - fitted_params$theta1_mle) * fitted_params$theta2_mle^2)^2
    
    # Corrected moments for asymmetric Laplace distribution
    alaplace_mean <- fitted_params$mu_alaplace +
      fitted_params$beta_alaplace * (1 - fitted_params$kappa_alaplace^2) / fitted_params$kappa_alaplace
    
    alaplace_variance <- fitted_params$beta_alaplace^2 *
      (1 + fitted_params$kappa_alaplace^4) / fitted_params$kappa_alaplace^2
    
    alaplace_skewness <- 2 * (1 - fitted_params$kappa_alaplace^6) /
      (1 + fitted_params$kappa_alaplace^4)^(3/2)
    
    alaplace_kurtosis <- 9 - 12 / (fitted_params$kappa_alaplace^2 +
                                     1 / fitted_params$kappa_alaplace^2)^2
    
    pfl_variance <- ifelse(fitted_params$theta_pfl > 2,
                           2 * fitted_params$beta_pfl^2 * fitted_params$theta_pfl / (fitted_params$theta_pfl - 2),
                           NA)
    # 11. Create parameter + moments table
    param_df <- data.frame(
      Model = c("Empirical Data", "Standard Laplace", "BLSM", "Asymmetric Laplace", "Power Function Laplace"),
      mu = c(mean(x), fitted_params$mu_hat, fitted_params$mu_mle, fitted_params$mu_alaplace, fitted_params$mu_pfl),
      beta = c(NA, fitted_params$beta_hat, fitted_params$beta_mle, fitted_params$beta_alaplace, fitted_params$beta_pfl),
      theta1 = c(NA, NA, fitted_params$theta1_mle, NA, NA),
      theta2 = c(NA, NA, fitted_params$theta2_mle, NA, NA),
      kappa = c(NA, NA, NA, fitted_params$kappa_alaplace, NA),
      theta = c(NA, NA, NA, NA, fitted_params$theta_pfl),
      Variance = c(empirical_variance, laplace_variance, lsm_variance, alaplace_variance, pfl_variance),
      Kurtosis = c(empirical_kurtosis, laplace_kurtosis, lsm_kurtosis, alaplace_kurtosis, NA)
    )
    
   
    # --- Compute log-likelihoods ---
    loglik_lsm <- -fit_lsm$value
    loglik_alaplace <- -fit_alaplace$value
    loglik_laplace_final <- loglike_laplace(fitted_params$beta_hat)  # standard Laplace
    
    # PF-L log-likelihood
    est_pfl <- fit_pfl$par
    ll_pfl <- -neg_loglik_pfl(est_pfl)  # neg_loglik_pfl returns negative log-likelihood
    
    # --- Number of parameters ---
    k_laplace <- 2  
    k_lsm <- 4
    k_alaplace <- 3
    k_pfl <- 3  # mu, beta, theta
    
    # --- AIC and BIC ---
    aic_laplace <- 2*k_laplace - 2*loglik_laplace_final
    bic_laplace <- log(n)*k_laplace - 2*loglik_laplace_final
    
    aic_lsm <- 2*k_lsm - 2*loglik_lsm
    bic_lsm <- k_lsm*log(n) - 2*loglik_lsm
    
    aic_alaplace <- 2*k_alaplace - 2*loglik_alaplace
    bic_alaplace <- k_alaplace*log(n) - 2*loglik_alaplace
    
    aic_pfl <- 2*k_pfl - 2*ll_pfl
    bic_pfl <- k_pfl*log(n) - 2*ll_pfl
    
    # --- Likelihood Ratio Tests ---
    lrt_test <- function(loglik_simple, loglik_complex, df_diff) {
      stat <- -2 * (loglik_simple - loglik_complex)
      p_value <- 1 - pchisq(stat, df = df_diff)
      list(Statistic = stat, df = df_diff, p_value = p_value)
    }
    
    lrt_alaplace <- lrt_test(loglik_laplace_final, loglik_alaplace, df_diff = k_alaplace - k_laplace)
    lrt_lsm <- lrt_test(loglik_laplace_final, loglik_lsm, df_diff = k_lsm - k_laplace)
    lrt_pfl <- lrt_test(loglik_laplace_final, ll_pfl, df_diff = k_pfl - k_laplace)
    
    # --- Model Criteria Table ---
    criteria_df <- data.frame(
      Model = c("Standard Laplace", "Laplace Scale Mixture", "Asymmetric Laplace", "Power Function Laplace"),
      LogLikelihood = c(loglik_laplace_final, loglik_lsm, loglik_alaplace, ll_pfl),
      AIC = c(aic_laplace, aic_lsm, aic_alaplace, aic_pfl),
      BIC = c(bic_laplace, bic_lsm, bic_alaplace, bic_pfl)
    )
    
    # --- LRT Table ---
    lrt_df <- data.frame(
      Comparison = c("Standard vs Asymmetric Laplace",
                     "Standard vs LSM",
                     "Standard vs PF-L"),
      Statistic = c(lrt_alaplace$Statistic, lrt_lsm$Statistic, lrt_pfl$Statistic),
      df = c(lrt_alaplace$df, lrt_lsm$df, lrt_pfl$df),
      `p-value` = c(lrt_alaplace$p_value, lrt_lsm$p_value, lrt_pfl$p_value)
    )
    
    
    # --- Render tables in UI ---
    output$paramTable <- renderTable({
      param_df
    }, digits = 6)
    
    output$modelCriteria <- renderTable({
      criteria_df
    }, digits = 4)
    
    output$lrtTable <- renderTable({
      lrt_df
    }, digits = 4)
    
    
  
    
    # 11. Simulation functions
    rlaplace <- function(n, mu, beta) {
      u <- runif(n) - 0.5
      mu - beta * sign(u) * log(1 - 2 * abs(u))
    }
    
    rAL <- function(n, mu, beta, kappa) {
      u <- runif(n)
      ifelse(u < (kappa^2)/(1+kappa^2), mu + beta*kappa*rexp(n), mu - beta/kappa*rexp(n))
    }
    
    rlsm <- function(n, mu, beta, theta1, theta2) {
      component <- rbinom(n, 1, theta1)
      xout <- numeric(n)
      if (sum(component == 1) > 0) xout[component == 1] <- rlaplace(sum(component == 1), mu, beta)
      if (sum(component == 0) > 0) xout[component == 0] <- rlaplace(sum(component == 0), mu, theta2 * beta)
      xout
    }
    
    output$simHistPlot <- renderPlot({
      df_real <- data.frame(Return = x, Type = "Real")
      df_sim <- data.frame(Return = numeric(0), Type = character(0))
      
      if ("Standard Laplace" %in% input$dist_choice) {
        sim <- rlaplace(length(x), fitted_params$mu_hat, fitted_params$beta_hat)
        df_sim <- rbind(df_sim, data.frame(Return = sim, Type = "Simulated - Standard Laplace"))
      }
      if ("BLSM" %in% input$dist_choice) {
        sim <- rlsm(length(x), fitted_params$mu_mle, fitted_params$beta_mle, 
                    fitted_params$theta1_mle, fitted_params$theta2_mle)
        df_sim <- rbind(df_sim, data.frame(Return = sim, Type = "Simulated - LSM"))
      }
      if ("Asymmetric Laplace" %in% input$dist_choice) {
        sim <- rAL(length(x), fitted_params$mu_alaplace, fitted_params$beta_alaplace, fitted_params$kappa_alaplace)
        df_sim <- rbind(df_sim, data.frame(Return = sim, Type = "Simulated - Asymmetric Laplace"))
      }
      
      df <- rbind(df_real, df_sim)
      
      # Create a dynamic color palette based on available types
      all_types <- unique(df$Type)
      color_palette <- c(
        "Real" = "#1f77b4",  # Blue for real data
        "Simulated - Standard Laplace" = "#d62728",  # Red
        "Simulated - LSM" = "#2ca02c",  # Green
        "Simulated - Asymmetric Laplace" = "#9467bd"  # Purple
      )
      
      # Filter to only include colors for types that exist
      color_palette <- color_palette[names(color_palette) %in% all_types]
      
      ggplot(df, aes(x = Return, fill = Type)) +
        geom_histogram(
          aes(y = ..density..), 
          position = "identity", 
          bins = 40, 
          alpha = 0.6,
          color = "white",
          linewidth = 0.2
        ) +
        
        scale_x_continuous(
          labels = scales::percent_format(accuracy = 0.1),
          breaks = scales::pretty_breaks(n = 6)
        ) +
        scale_fill_manual(values = color_palette) +
        scale_color_manual(values = color_palette) +
        labs(
          title = "Real vs Simulated Log Returns Distribution",
          subtitle = paste("Comparison with", length(unique(df_sim$Type)), "simulation models"),
          x = "Daily Log Return",
          y = "Density",
          fill = "Distribution",
          color = "Distribution"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray50", 
                                       margin = margin(b = 10)),
          axis.title = element_text(face = "bold"),
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.title.y = element_text(margin = margin(r = 8)),
          axis.text = element_text(color = "black"),
          legend.position = "bottom",
          legend.title = element_text(face = "bold"),
          legend.text = element_text(size = 9),
          legend.key.size = unit(0.8, "lines"),
          panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(15, 15, 15, 15)
        )
    })
    

  
    
   
   
 
  })
  
  # --- Reactive simulation (depends on fitted params + slider + n_days) ---
  priceSimData <- reactive({
    req(fitted_params$x)   # make sure parameters are already fitted
    
    n_paths <- input$n_sim
    n_days <- input$n_days
    P0 <- as.numeric(tail(crypto_data(), 1))
    
    price_paths <- matrix(NA, nrow = n_days+1, ncol = n_paths)
    price_paths[1, ] <- P0
    
    for (t in 2:(n_days+1)) {
      returns_t <- rlsm(n_paths, fitted_params$mu_mle, fitted_params$beta_mle, 
                        fitted_params$theta1_mle, fitted_params$theta2_mle)
      price_paths[t, ] <- price_paths[t-1, ] * exp(returns_t)
    }
    
    losses <- P0 - price_paths[n_days+1, ]
    alpha <- 0.05
    VaR_95 <- quantile(losses, alpha)
    
    list(price_paths = price_paths, VaR_95 = VaR_95, P0 = P0,
         n_days = n_days, n_paths = n_paths)
  })
  
  # --- Plot (auto-updates when slider changes) ---
  output$priceSimPlot <- renderPlot({
    sim <- priceSimData()
    actual_df <- crypto_data2()  # <- use crypto_data2() directly
    
    df_paths <- data.frame(
      Day = rep(0:sim$n_days, sim$n_paths),
      Price = as.vector(sim$price_paths),
      Path = rep(1:sim$n_paths, each = sim$n_days + 1)
    )
    
    avg_path <- data.frame(
      Day = 0:sim$n_days,
      Price = rowMeans(sim$price_paths)
    )
    
    ggplot() +
      # Simulated paths
      geom_line(data = df_paths, 
                aes(x = Day, y = Price, group = Path), 
                color = "steelblue", alpha = 0.7, linewidth = 0.6) +
      
      # Actual prices
      geom_line(data = actual_df, 
                aes(x = Day, y = Price), 
                color = "black", linetype = "dashed", linewidth = 0.8) +
      
      # Average simulated path
      geom_line(data = avg_path,
                aes(x = Day, y = Price),
                color = "red", linewidth = 0.8) +
      
      # VaR horizontal line
      geom_hline(yintercept = sim$P0 + sim$VaR_95, 
                 linetype = "dotted", color = "red", linewidth = 0.8) +
      
      # Labels
      labs(
        title = paste("GBM Simulation: Price Paths with Value at Risk (5%)"),
        subtitle = paste("Cryptocurrency:", input$crypto, "|", 
                         "VaR (95% Confidence) =", round(sim$VaR_95, 2)),
        x = "Time Horizon (Days)",
        y = "Price (USD)",
        caption = paste("Initial Price (P₀):", sim$P0, "|", 
                        "Number of Paths:", length(unique(df_paths$Path)), "|",
                        "Simulation Date:", format(Sys.Date(), "%B %d, %Y"))
      ) +
      
      # Optional: annotate text for labels
      annotate("text", x = sim$n_days/3, y = tail(actual_df$Price, 1), 
               label = "Actual Path", color = "black", hjust = 1.5, size = 4) +
      annotate("text", x = sim$n_days/2, y = tail(avg_path$Price, 1), 
               label = "Average Path", color = "red", hjust = -1.9, vjust = 1.9, size = 4) +
      
      scale_y_continuous(labels = scales::dollar_format()) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 10)),
        plot.caption = element_text(size = 9, color = "gray50", margin = margin(t = 10)),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
  })
  
  
  
  
  # --- Reactive expression for histogram plot that responds to checkboxes ---
  histPlotReactive <- reactive({
    req(input$analyze > 0)
    req(returns_data())
    
    x <- na.omit(as.numeric(returns_data()))
    
    # Calculate statistics for better scaling
    x_range <- range(x)
    x_padding <- diff(x_range) * 0.05
    x_limits <- c(x_range[1] - x_padding, x_range[2] + x_padding)
    
    # Create a color palette for the distributions
    dist_colors <- c(
      "Standard Laplace" = "#2ca02c",      # Green
      "BLSM" = "#d62728",                  # Red
      "Asymmetric Laplace" = "#9467bd",    # Purple
      "Power Function Laplace" = "#ff7f0e" # Orange
    )
    
    # Get selected distributions for subtitle
    selected_dists <- input$dist_choice
    subtitle_text <- if(length(selected_dists) > 0) {
      paste("Fitted distributions:", paste(selected_dists, collapse = ", "))
    } else {
      "Empirical distribution only"
    }
    
    # Main histogram with densities overlay
    p_main <- ggplot(data.frame(Return = x), aes(x = Return)) +
      geom_histogram(
        aes(y = ..density..), 
        bins = 40, 
        fill = "#1f77b4", 
        alpha = 0.7,
        color = "white",
        linewidth = 0.2
      ) +
      
      scale_x_continuous(
        labels = scales::percent_format(accuracy = 0.1),
        breaks = scales::pretty_breaks(n = 6)
      ) +
      labs(
        title = paste(input$crypto, "Log Returns Distribution"),
        subtitle = subtitle_text,
        x = "Daily Log Return",
        y = "Density"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray50", 
                                     margin = margin(b = 10)),
        axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 8)),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.text = element_text(color = "black"),
        panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(15, 15, 15, 15)
      ) +
      coord_cartesian(xlim = x_limits)
    
    # Add selected distribution curves
    if ("Standard Laplace" %in% input$dist_choice) {
      p_main <- p_main + stat_function(
        fun = function(z) dlaplace(z, fitted_params$mu_hat, fitted_params$beta_hat),
        color = dist_colors["Standard Laplace"], linewidth = 1.2, linetype = "solid")
    }
    if ("BLSM" %in% input$dist_choice) {
      p_main <- p_main + stat_function(
        fun = function(z) dlsm(z, fitted_params$mu_mle, fitted_params$beta_mle, 
                               fitted_params$theta1_mle, fitted_params$theta2_mle),
        color = dist_colors["BLSM"], linewidth = 1.2, linetype = "solid")
    }
    if ("Asymmetric Laplace" %in% input$dist_choice) {
      p_main <- p_main + stat_function(
        fun = function(z) dalaplace(z, fitted_params$mu_alaplace, 
                                    fitted_params$beta_alaplace, fitted_params$kappa_alaplace),
        color = dist_colors["Asymmetric Laplace"], linewidth = 1.2, linetype = "solid")
    }
    if ("Power Function Laplace" %in% input$dist_choice) {
      p_main <- p_main + stat_function(
        fun = function(z) dpfl(z, fitted_params$mu_pfl, fitted_params$beta_pfl, fitted_params$theta_pfl),
        color = dist_colors["Power Function Laplace"], linewidth = 1.2, linetype = "solid")
    }
    
    p_main
  })
  
  # --- Render the histogram plot ---
  output$histCombined <- renderPlot({
    histPlotReactive()
  })
  
  # --- Reactive expression for tail plot ---
  tailPlotReactive <- reactive({
    req(input$analyze > 0)
    req(returns_data())
    
    x <- na.omit(as.numeric(returns_data()))
    
    # Empirical CCDF
    abs_returns <- sort(abs(x), decreasing = TRUE)
    empirical_ccdf <- (1:length(abs_returns)) / length(abs_returns)
    df_tail <- data.frame(abs_returns = abs_returns, ccdf = empirical_ccdf)
    
    # Helper to compute CCDF from pdf
    ccdf_from_pdf <- function(x_vals, pdf_fun) {
      sapply(x_vals, function(t) {
        left <- try(integrate(pdf_fun, lower = -Inf, upper = -t)$value, silent = TRUE)
        right <- try(integrate(pdf_fun, lower = t, upper = Inf)$value, silent = TRUE)
        v <- 0
        if (!inherits(left, "try-error")) v <- v + left
        if (!inherits(right, "try-error")) v <- v + right
        v
      })
    }
    
    # PDFs for models
    pdf_laplace <- function(z) dlaplace(z, mu = fitted_params$mu_hat, beta = fitted_params$beta_hat)
    pdf_alaplace <- function(z) dalaplace(z, fitted_params$mu_alaplace, fitted_params$beta_alaplace, fitted_params$kappa_alaplace)
    pdf_lsm <- function(z) dlsm(z, fitted_params$mu_mle, fitted_params$beta_mle, fitted_params$theta1_mle, fitted_params$theta2_mle)
    pdf_pfl <- function(z) dpfl(z, fitted_params$mu_pfl, fitted_params$beta_pfl, fitted_params$theta_pfl)
    mu_norm <- mean(x); sigma_norm <- sd(x)
    pdf_norm <- function(z) dnorm(z, mu_norm, sigma_norm)
    
    # Grid of values for CCDF
    x_vals <- seq(min(abs_returns[abs_returns > 0]), max(abs_returns), length.out = 300)
    
    # Build CCDFs depending on choice
    df_models <- data.frame(x_vals = x_vals)
    
    if ("Standard Laplace" %in% input$dist_choice) {
      df_models$Laplace <- ccdf_from_pdf(x_vals, pdf_laplace)
    }
    if ("Asymmetric Laplace" %in% input$dist_choice) {
      df_models$Asymmetric <- ccdf_from_pdf(x_vals, pdf_alaplace)
    }
    if ("BLSM" %in% input$dist_choice) {
      df_models$bLSM <- ccdf_from_pdf(x_vals, pdf_lsm)
    }
    if ("Power Function Laplace" %in% input$dist_choice) {
      df_models$pfl <- ccdf_from_pdf(x_vals, pdf_pfl)
    }
    # Optional: always include Normal as a baseline
    df_models$normal <- ccdf_from_pdf(x_vals, pdf_norm)
    
    # Long format for ggplot
    df_long <- reshape2::melt(df_models, id.vars = "x_vals", variable.name = "Model", value.name = "CCDF")
    
    # --- Tail plot ---
    ggplot() +
      geom_point(data = df_tail, aes(x = abs_returns, y = ccdf), 
                 color = "blue", alpha = 0.7, size = 1.5) +
      geom_line(data = df_long, aes(x = x_vals, y = CCDF, color = Model), 
                size = 1.2, na.rm = TRUE) +
      scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
      scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
      labs(
        x = "Absolute Return (log scale)", 
        y = "Complementary CDF (log scale)", 
        title = paste(input$crypto, "Tail Distribution"),
        subtitle = if(length(input$dist_choice) > 0) {
          paste("Models:", paste(input$dist_choice, collapse = ", "))
        } else {
          "Empirical only (with Normal baseline)"
        },
        color = "Distribution Model"
      ) +
      scale_color_manual(
        values = c(
          "normal" = "black",
          "pfl" = "#ff7f0e", 
          "Laplace" = "#2ca02c",
          "Asymmetric" = "#9467bd",
          "bLSM" = "#d62728"
        )
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 10),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
        panel.grid.minor = element_line(color = "gray95", linewidth = 0.1),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      ) +
      coord_cartesian(ylim = c(1e-06, 1), xlim = c(min(x_vals), max(x_vals)))
  })
  # --- Render the tail plot ---
  output$tailPlot <- renderPlot({
    tailPlotReactive()
  })
  
}


shinyApp(ui = ui, server = server)