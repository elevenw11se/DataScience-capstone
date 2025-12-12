


# server.R ------------------------------------------------------------
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(janitor)
library(stringr)
library(stringi)
library(readr)

# -------------------------------------------------
# 1. LOAD PREBUILT DATASET
# -------------------------------------------------
load("data/annual.RData")

# -------------------------------------------------
# 2. VARIABLE SETUP
# -------------------------------------------------
stopifnot(exists("annual"))
annual <- annual %>% arrange(Year)
numeric_cols <- names(annual)[sapply(annual, is.numeric) & names(annual) != "Year"]
var_choices  <- setNames(numeric_cols, numeric_cols)
year_min <- min(annual$Year, na.rm = TRUE)
year_max <- max(annual$Year, na.rm = TRUE)

# -------------------------------------------------
# 3. SERVER LOGIC
# -------------------------------------------------
server <- function(input, output, session) {
  
  # Update dropdowns dynamically
  for (id in c("left_var", "right_var", "x_var", "y_var",
               "gr_x_var", "gr_y_var", "vars_ts_multi")) {
    updateSelectInput(session, id, choices = var_choices)
  }
  
  # Reactive filtered dataset
  annual_filtered <- reactive({
    annual %>% filter(Year >= input$year_range[1], Year <= input$year_range[2])
  })
  
  # Main plot
  output$main_plot <- renderPlot({
    df <- annual_filtered()
    req(nrow(df) > 1)
    
    if (input$plot_type == "ts2") {
      left  <- input$left_var
      right <- input$right_var
      req(left, right, left != right)
      df2 <- df %>% filter(!is.na(.data[[left]]), !is.na(.data[[right]]))
      sf <- max(df2[[left]], na.rm = TRUE) / max(df2[[right]], na.rm = TRUE)
      
      ggplot(df2, aes(x = Year)) +
        geom_line(aes(y = .data[[left]], color = left), linewidth = 1.2) +
        geom_line(aes(y = .data[[right]] * sf, color = right),
                  linewidth = 1.2, linetype = "dashed") +
        scale_y_continuous(name = left,
                           sec.axis = sec_axis(~ . / sf, name = right)) +
        labs(title = paste(left, "vs", right),
             x = "Year", color = "Series") +
        theme_minimal(base_size = 14)
    }
    
    else if (input$plot_type == "ts_multi") {
      vars <- input$vars_ts_multi
      req(length(vars) >= 1)
      df_long <- df %>%
        select(Year, all_of(vars)) %>%
        pivot_longer(cols = -Year, names_to = "Variable", values_to = "Value") %>%
        filter(!is.na(Value))
      ggplot(df_long, aes(x = Year, y = Value, color = Variable)) +
        geom_line(linewidth = 1.1) +
        geom_point(size = 2) +
        labs(title = "Multi-series time plot", x = "Year", y = "Value") +
        theme_minimal(base_size = 14)
    }
    
    else if (input$plot_type == "scatter") {
      x_var <- input$x_var
      y_var <- input$y_var
      req(x_var, y_var, x_var != y_var)
      ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
        geom_point(size = 2.5, alpha = 0.8) +
        geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
        labs(title = paste(y_var, "vs", x_var),
             x = x_var, y = y_var) +
        theme_minimal(base_size = 14)
    }
    
    else if (input$plot_type == "granger") {
      x_var <- input$gr_x_var
      y_var <- input$gr_y_var
      req(x_var, y_var, x_var != y_var)
      df2 <- df %>% filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))
      sf <- max(df2[[y_var]], na.rm = TRUE) / max(df2[[x_var]], na.rm = TRUE)
      ggplot(df2, aes(x = Year)) +
        geom_line(aes(y = .data[[x_var]] * sf, color = x_var),
                  linewidth = 1.2, linetype = "dashed") +
        geom_line(aes(y = .data[[y_var]], color = y_var),
                  linewidth = 1.2) +
        scale_y_continuous(
          name = y_var, sec.axis = sec_axis(~ . / sf, name = x_var)
        ) +
        labs(title = paste("Series used in Granger test:", x_var, "and", y_var),
             x = "Year", color = "Series") +
        theme_minimal(base_size = 14)
    }
  })
  
  # Scatter summary
  output$scatter_summary <- renderPrint({
    req(input$plot_type == "scatter")
    df <- annual_filtered()
    x <- input$x_var; y <- input$y_var
    df_sc <- df %>% filter(!is.na(.data[[x]]), !is.na(.data[[y]]))
    r <- cor(df_sc[[x]], df_sc[[y]], use = "complete.obs")
    fit <- lm(df_sc[[y]] ~ df_sc[[x]])
    cat("Correlation:", round(r, 3), "\n\n")
    print(summary(fit))
  })
  
  # Granger summary
  output$granger_summary <- renderPrint({
    req(input$plot_type == "granger")
    if (!requireNamespace("lmtest", quietly = TRUE)) {
      cat("Install 'lmtest' to run Granger tests.\n")
      return()
    }
    df <- annual_filtered()
    x <- input$gr_x_var; y <- input$gr_y_var; L <- input$gr_lag
    df_gc <- df %>% select(Year, all_of(c(x, y))) %>% drop_na()
    if (nrow(df_gc) < (L + 5)) {
      cat("Not enough observations for Granger test.\n")
      return()
    }
    x_series <- df_gc[[x]]; y_series <- df_gc[[y]]
    cat("H0: X does NOT Granger-cause Y (X -> Y)\n")
    print(lmtest::grangertest(y_series ~ x_series, order = L))
    cat("\nH0: Y does NOT Granger-cause X (Y -> X)\n")
    print(lmtest::grangertest(x_series ~ y_series, order = L))
  })
}
