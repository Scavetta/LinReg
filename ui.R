#### Shiny app LinReg 2016

dashboardPage(
  dashboardHeader(title = "Linear Models"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Linear Regression", tabName = "linreg"),
      menuItem("ANOVA", tabName = "ANOVA")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("linreg",
              fluidRow(
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Input",
                  sliderInput("samsize", "Sample size:", 
                              min=5, max=60, value=40, step=1),
                  sliderInput("sdx", "Standard deviation of X:", 
                              min=1, max=15, value=3, step=0.5),
                  sliderInput("sdu", "Standard deviation of y:", 
                              min=1, max=15, value=3, step=0.5),
                  sliderInput("rho", "r:", 
                              min=.05, max=0.95, value=0.9, step=0.05)
                  ),
                # box(
                #   width = 8, status = "info", solidHeader = TRUE,
                #   title = "Distribution",
                #   plotOutput(outputId = "dist_plot1")
                # )
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Plot",
                  plotOutput("rhoscatter")
                ),
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Selections",
                  checkboxInput("showOLS", label = "Show OLS model", value = FALSE),
                  checkboxInput("showOLSRes", label = "Show OLS model residuals", value = FALSE),
                  br(),
                  checkboxInput("showNULL", label = "Show null model", value = FALSE),
                  checkboxInput("showNULLRes", label = "Show null model residuals", value = FALSE),
                  br(),
                  checkboxInput("autoZoom", label = "Auto-zoom", value = FALSE),
                  checkboxInput("maintainRatio", label = "Maintain 1:1 aspect ratio?", value = FALSE)
                )
              ),
              fluidRow(
                box(
                  width = 12, status = "info", solidHeader = FALSE,
                  # tableOutput(outputId = "LinRegSSTplot")
                  plotOutput(outputId = "LinRegSSTplot", height = "100px")
                )
              ),
              fluidRow(
                box(
                  width = 2, status = "info", solidHeader = FALSE,
                  title = "Summary",
                  uiOutput(outputId = "statsSummaryLinReg")
                ),
                box(
                  width = 5, status = "info", solidHeader = FALSE,
                  title = "t-test",
                  tableOutput(outputId = "lmSummaryLinReg")
                ),
                box(
                  width = 5, status = "info", solidHeader = FALSE,
                  title = "F-test",
                  tableOutput(outputId = "FSummaryLinReg")
                )
              )
      ), # End Lin Reg
      tabItem("ANOVA",
              fluidRow(
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Input",
                  sliderInput("g1n", "Sample size:", 
                              min = 3, max = 60, value = 5, step= 1),
                  sliderInput("g1u", "Mean:", 
                              min = 0, max = 25, value = 2, step= 0.5),
                  sliderInput("g1s", "Standard Deviation", 
                              min = 1, max = 15, value = 2, step= 1)
                ),
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Input",
                  sliderInput("g2n", "Sample size:", 
                              min = 3, max = 60, value = 5, step= 1),
                  sliderInput("g2u", "Mean:", 
                              min = 0, max = 25, value = 4, step= 0.5),
                  sliderInput("g2s", "Standard Deviation", 
                              min = 1, max = 15, value = 1, step= 1)
                  ),
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Input",
                  sliderInput("g3n", "Sample size:", 
                              min = 3, max = 60, value = 5, step= 1),
                  sliderInput("g3u", "Mean:", 
                              min = 0, max = 25, value = 6, step= 0.5),
                  sliderInput("g3s", "Standard Deviation", 
                              min = 1, max = 15, value = 2, step= 1)
                ),
                
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Null",
                  checkboxInput("showSST", label = "Show Residuals", value = FALSE),
                  plotOutput(outputId = "main_plot")
                ),
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Model",
                  checkboxInput("showSSR", label = "Show Residuals", value = FALSE),
                  plotOutput(outputId = "var_plots")
                )
                
              ),
              fluidRow(
                box(
                  width = 12, status = "info", solidHeader = FALSE,
                  plotOutput(outputId = "SSTplot", height = "100px")
                )
              ),
              fluidRow(
                box(
                  width = 6, status = "info", solidHeader = FALSE,
                  title = "t-test",
                  tableOutput(outputId = "lmSummaryANOVA")
                ),
                box(
                  width = 6, status = "info", solidHeader = FALSE,
                  title = "F-test",
                  tableOutput(outputId = "FSummaryANOVA")
                )
              )
      ) # End ANOVA
    )
  )
)

    