# ui.R


library(shiny)

ui <- fluidPage(
  titlePanel("Ghana Macro Dashboard – Interactive Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Plot / Analysis Type"),
      selectInput("plot_type", "Choose mode:",
                  choices = c(
                    "Time series (2 vertical axes)" = "ts2",
                    "Time series (multi-series, 1 axis)" = "ts_multi",
                    "Scatter (Y vs X)" = "scatter",
                    "Granger causality test" = "granger"
                  ), selected = "ts2"),
      sliderInput("year_range", "Year range:",
                  min = 2000, max = 2025,
                  value = c(2010, 2025), step = 1, sep = ""),
      conditionalPanel(
        condition = "input.plot_type == 'ts2'",
        selectInput("left_var", "Left axis variable:", choices = NULL),
        selectInput("right_var", "Right axis variable:", choices = NULL)
      ),
      conditionalPanel(
        condition = "input.plot_type == 'ts_multi'",
        selectizeInput("vars_ts_multi", "Variables to plot (multi-series):",
                       choices = NULL, multiple = TRUE)
      ),
      conditionalPanel(
        condition = "input.plot_type == 'scatter'",
        selectInput("x_var", "X variable:", choices = NULL),
        selectInput("y_var", "Y variable:", choices = NULL)
      ),
      conditionalPanel(
        condition = "input.plot_type == 'granger'",
        selectInput("gr_x_var", "X variable (possible cause):", choices = NULL),
        selectInput("gr_y_var", "Y variable (possible effect):", choices = NULL),
        numericInput("gr_lag", "Lag order (number of years):",
                     value = 1, min = 1, max = 5, step = 1)
      )
    ),
    
    mainPanel(
      width = 9,
      plotOutput("main_plot", height = "480px"),
      br(),
      h4("Summary / Results"),
      conditionalPanel(condition = "input.plot_type == 'ts_multi'",
                       tableOutput("summary_ts_multi")),
      conditionalPanel(condition = "input.plot_type == 'scatter'",
                       verbatimTextOutput("scatter_summary")),
      conditionalPanel(condition = "input.plot_type == 'granger'",
                       verbatimTextOutput("granger_summary"))
    )
  )
)




