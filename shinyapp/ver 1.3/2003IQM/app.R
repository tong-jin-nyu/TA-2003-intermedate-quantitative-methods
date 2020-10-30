# ----- ----- Title ----- -----
#
# Title: APSTA-GE: 2003 Intermediate Quantitative Methods
# Subtitle: Shiny Course Site
# 
# Data Created: 10/14/2020
# Data Modified: 10/27/2020
#
# ----- ----- Author ----- -----
#
# Author: Ying Lu
# Email: Ying.Lu@nyu.edu
# Affiliation: New York University
#
# Author: Tong Jin
# Email: tj1061@nyu.edu
# Affiliation: New York University
# 
# Author: Lisa Song
# Email: ls5209@nyu.edu
# Affiliation: New York University
#
# ----- ----- Copyright ----- -----
# Copyright (c) Ying Lu, 2020
# 
# ----- ----- Contents ----- -----
# 
# 1. Linear regression simulator
# 2. Sampling Distribution Generator
# 3: Hypothesis Testing Tester
# 

# Dependency -----------------------------------------------------------------
library(shiny)
library(broom)
library(DT)
library(shinythemes)
library(lubridate)
library(xtable)

# Function --------------------------------------------------------------------
onDate <- function(date) {
  # Input a short date, detect current year and output a due date in long format
  # Param: date: a string in short format (mm/dd)
  return(paste(" On:", paste(date, year(Sys.Date()), sep = "/")))
}

# Toggle -----------------------------------------------------------------------
group_solution_out <- TRUE  # Toggle to show/hide solution for group assignment

# Color palette ----------------------------------------------------------------
blue <- "#335C81"
light_blue <- "#ADD7F6"
red <- "#F42C04"

# Links ------------------------------------------------------------------------
a_ying_lu_nyu <- "mailto:ying.lu@nyu.edu"
a_tong_jin_nyu <- "mailto:tj1061@nyu.edu"
a_lisa_song_nyu <- "mailto:ls5209@nyu.edu"
a_syllabus <- "https://docs.google.com/document/d/1YSyH7IN3p-vUzAkcjw4CUFB-utiOI4DPu4KqA8YMZNQ/edit?usp=sharing"
a_zoom_reg <- "https://nyu.zoom.us/meeting/register/tJAsduCvrzwsGtCAswXfHUr1a2Wz-gYTZnZg"
a_zoom_lab2_lisa <- "https://nyu.zoom.us/j/93444830711"
a_zoom_lab3_tong <- "https://nyu.zoom.us/j/97347070628"
a_office_hours <- "https://docs.google.com/spreadsheets/d/1YY38yj8uCNIm1E7jaI9TJC494Pye2-Blq9eSK_eh6tI/edit?usp=sharing"
a_group_assignment <- "https://docs.google.com/document/d/1BkZAa453Hmbe4C4ydLiNTpp78Z6HJfA5vXdD-cEQPA4/edit?usp=sharing"
a_group_solution <- "https://drive.google.com/drive/folders/1ePk3Tu1TS71LyYz39nJzw09DrrSWpPSA?usp=sharing"

# UI ---------------------------------------------------------------

ui <- navbarPage(  # Create pages with a top level navi bar
  
  # App Title ----
  title = "NYU APSTA-GE: 2003 Intermediate Quantitative Methods",
  
  # Page 1: home ----
  tabPanel(title = "Home", class = "home",
    
    div(id = "home-title",
      h4("APSTA-GE.2003: Intermediate Quantitative Methods: General Linear Model"),
      h4(tags$b("Fall 2020"))
    ),
    hr(),  # Horizontal line
    
    # Create a navi list layout
    navlistPanel(widths = c(2, 10),
      # Tab 1: course info ----
      tabPanel(title = "Course Information", class = "home-navi-info",
        div(id = "home-navi-info-syllabus",
          p(tags$i("Subject to change. To view the latest version, please use "), 
            tags$i(a(href=a_syllabus, "this link")), tags$i(". (NYU ID required)"))
        ),
        div(style = "font-size: 20px", id = "home-navi-info-instructor",
          h3("Instructor"),
          tags$ul(
            tags$li(tags$b("Instructor: ", style = "color:purple"), a(href=a_ying_lu_nyu, "Ying Lu")),
            tags$li(tags$b("Teaching Assistant: ", style = "color:purple"), a(href=a_tong_jin_nyu, "Tong Jin")),
            tags$li(tags$b("Teaching Assistant: ", style = "color:purple"), a(href=a_lisa_song_nyu, "Lisa Song")),
          ),
          p("To send direct emails, click on names.", style = "color:black")
        ),
        div(style = "font-size: 20px", id = "home-navi-info-lecture",  # Lecture info ----
          h3("Lecture"),
          tags$ul(
            tags$li(tags$b("Time: ", style = "color:purple"), 
                    "Tuesday, 3:30 - 6:10 p.m. (Eastern U.S./Canada)"),
            tags$li(tags$b("Zoom Registration: ", style = "color:purple"), a(href=a_zoom_reg, "Click here")),
            tags$li(tags$b("Zoom Classroom: ", style = "color:purple"), tags$b("941 1971 7604")),
              tags$ul(
                tags$li(tags$b("Passcode: ", style = "color:purple"), tags$b("2003"))
              ),
            tags$li(tags$b("Office: ", style = "color:purple"), 
                    "205 Kimball Hall, 246 Greene Street, New York, NY 10003")
          )
        ),
        div(style = "font-size: 20px", id = "home-navi-info-lab2",  # Lab 002 info ----
          h3("Lab Section 002"),
          tags$ul(
            tags$li(tags$b("Time: ", style = "color:purple"), 
                    "Tuesday, 2:00 - 3:00 p.m. (Eastern U.S./Canada)"),
            tags$li(tags$b("Zoom Link: ", style = "color:purple",
                           a(href=a_zoom_lab2_lisa, "934 4483 0711"))),
              tags$ul(
                tags$li(tags$b("Passcode: ", style = "color:purple"), tags$b("2003"))
              ),
            tags$li(tags$b("Lab Instructor: ", style = "color:purple"), "Lisa Song")
          )
        ),
        div(style = "font-size: 20px", id = "home-navi-info-lab3",  # Lab 003 info ----
          h3("Lab Section 003"),
          tags$ul(
            tags$li(tags$b("Time: ", style = "color:purple"), 
                    "Tuesday, 6:25 - 7:25 p.m. (Eastern U.S./Canada)"),
            tags$li(tags$b("Zoom Link: ", style = "color:purple",
                           a(href=a_zoom_lab3_tong, "973 4707 0628"))),
            tags$ul(
              tags$li(tags$b("Passcode: ", style = "color:purple"), tags$b("2003"))
            ),
            tags$li(tags$b("Lab Instructor: ", style = "color:purple"), "Tong Jin")
          )
        ),
        div(style = "font-size: 20px", id = "home-navi-info-schedule",  # Lecture schedule ----
          h3("Lecture Schedule")
        ),

        div(DT::dataTableOutput("mytable"), style = "font-size: 100%; width: 50%")
        
      ),  # <END> home-navi-info
      
      # Tab 2: office hours ----
      tabPanel(title = "Office Hours", tags$h2("Office Hours"), class = "home-navi-office",
        div(style = "font-size: 20px", id = "home-navi-office-info",
          tags$ul(
            tags$li(tags$b("Ying Lu"), style = "color: purple"),
              tags$ul(
                tags$li(tags$b("Thursdays,"), "9:00 - 10:00 a.m."),
                tags$li(tags$b("Fridays,"), "12:00 - 1:00 p.m."),
                tags$li("or by appointment"),
                tags$li("Zoom ID for Office Hours:", tags$b("471 170 3630"))
              ),
            tags$li(tags$b("Tong Jin"), style = "color: purple"),
              tags$ul(
                tags$li(tags$b("Mondays,"), "9:00 - 10:00 a.m."),
                tags$li(tags$b("Wednesdays,"), "12:30 - 1:30 p.m."),
                tags$li("or by appointment")
              ),
            tags$li(tags$b("Lisa Song"), style = "color: purple"),
            tags$ul(
              tags$li(tags$b("Mondays,"), "5:00 - 6:00 p.m."),
              tags$li(tags$b("Wednesdays,"), "12:30 - 1:30 p.m."),
              tags$li("or by appointment")
            )
          )
        ),
        div(style = "font-size: 14px", id = "home-navi-office-link",  # Office hour sign up ----
          p(tags$b("To book additional office hours,"), "please sign-up on ", 
            a(href=a_office_hours, "this sheet"), ".")
        )
      ),  # <END> home-navi-office
      
      # Tab 3: assignment ----
      
      
      tabPanel(title = "Assignments", tags$h2("Assignment Due Dates"), class = "home-navi-assignment",
        div(style = "font-size: 18px", id = "home-navi-assignment-info",  
          tags$ul(
            tags$li(tags$h3("Pre-class Survey:", style = "color:purple"), tags$b("09/09/20")),
            tags$li(tags$h3("Assignment 1:", style = "color:purple"), tags$b("09/23/20")),
            tags$li(tags$h3("Assignment 2:", style = "color:purple"), tags$b("10/02/20")),
            tags$li(tags$h3("Assignment 3:", style = "color:purple"), tags$b("10/19/20")),
            tags$li(tags$h3("Assignment 4:", style = "color:purple"), tags$b("10/28/20")),
            tags$li(tags$h3("Assignment 5:", style = "color:purple"), tags$b("11/06/20")),
            tags$li(tags$h3("Assignment 6:", style = "color:purple"), tags$b("")),
            tags$li(tags$h3("Assignment 7:", style = "color:purple"), tags$b("")),
            tags$li(tags$h3("Assignment 8:", style = "color:purple"), tags$b("")),
            tags$li(tags$h3("Assignment 9:", style = "color:purple"), tags$b("")),
            tags$li(tags$h3("Assignment 10:", style = "color:purple"), tags$b(""))
          )
        )
      ),  # <END> home-navi-assignment
      
      # Tab 4: group assignment ----
      tabPanel(style = "font-size: 18px", title = "Group Assignment", tags$h2("Group Assignment"), class = "home-navi-assignment-group",
        div(id = "home-navi-assignment-group-info",
          p(a(href=a_group_assignment, "Questions")),
          if (group_solution_out) {  
            p(a(href=a_group_solution, "Solutions"))
          }
          
        )
      )
    )  # <END> home-navi
  ),  # <END> home
  
  
  # Page 2: Linear Regression simulator ----
  #      Fit a regression model with simulated data
  tabPanel(title="Linear Regression Simulator", class = "lr-simulator",
    withMathJax(),
    div(    id = "tab-lr-simulator-title",
      h3("Linear Regression Simulator")
    ),
    hr(), 
    # Sidebar: a slider and selection inputs ----
    sidebarLayout(
      sidebarPanel(
          helpText("Coefficients"),
          numericInput(inputId="beta0", label = "Intercept:", value = 0),
          numericInput(inputId="beta1", label = "Slope:", value = 0),
          numericInput(inputId="sigma2", label = "Error Variance:", value = 1, min=0),
          hr(),
          numericInput(inputId="sampsize", label="Sample Size:", value = 10, min = 2)
        ), # <END lm-simulator-sidebarLayout-sidebarPanel>
      
      mainPanel(
        div(    id = "tab-lm-simulator-info",
          p(tags$b("Description: "),
            "This simulator generates a simple linear regression model based on a 
            simulated sample data."),
          p("The indepenedent variable, X, is randomly generated from a normal 
            distribution: $$\\mathcal{N} \\sim (0, 1)$$"),
          p("The model equation is: 
            $$Y = \\beta_0 + \\beta_1 \\cdot X + \\varepsilon$$"),
          p("On the left sidebar, there are three parameters: Intercept, Slope, 
            and Error Variance. You can update the model by changing these 
            parameters."),
          tags$ul(
            tags$li("Intercept: the value of Y given X = 0"),
            tags$li("Slope: the multiplier of X"),
            tags$li("Error Variance: how disperse is the error")
          )
        ),
        p(strong("Instructions: "), "To create a simple linear regression model fit 
          on a randomly sampled data, click on the buttom below. 10 points will 
          be drawn by default. To change the sample size, update the input window 
          below."),
        actionButton(inputId="draw", label = "Draw Sample and Fit Model"),                 
        br(),
        plotOutput("plot0", width = "80%"),
        br(),
        textOutput('hint0'),
        br(),
        tableOutput("res0"),
        br()
      ) # <END lr-simulator-sidebarLayout-mainPanel>
    ) # <END lr-simulator-sidebarLayout>
  ), # <END lr-simulator>

  # Page 3: Sampling distribution ----
  #      Examine sampling distribution by randomly drawing multiple samples
  tabPanel(title = "Sampling Distribution", 
    sidebarLayout(
      sidebarPanel(  # Sidebar with a slider and selection inputs
        helpText("Use the following buttons to generate random samples: "),
        hr(),
        actionButton("drawone", "Draw one sample"),
        hr(),
        actionButton("drawmany", "Draw 100 samples"),
        hr(),
        # selectInput(inputId = "selection", 
        #   label = "Choose a signal-noise-ratio setup:",
        #   choices = c("high signal noise ratio" = "A", 
        #               "low signal noise ratio" = "B")
        # ),
        sliderInput(inputId = "n", 
          label = "Sample Size:", 
          min = 10,  max = 300, value = 50
        )
      ), # <END sampling-sidebarLayout-sidebarPanel>
      mainPanel(
        div(    id = "tab-sampling-info",
          p(tags$b("Description: "),
            "This application examines sampling distribution by repeatedly 
            drawing random samples from a population, known as bootstrapping."),
          p("On the left side, there are two buttons: "),
          tags$ul(
            tags$li(tags$b("DRAW ONE SAMPLE"), "to randomly draw one sample 
                    from the population and fit a regression line"),
            tags$li(tags$b("DRAW 100 SAMPLES"), "to repeatly draw 100 random 
                    samples (with replacement) from the population. For each 
                    sample, fit a regression line.")
          ),
          p("The sample size slider allows you to change the sample size of each 
            iteration. The default sample size is set at 50. You can select as 
            many as 300 sample points or as less as 10."),
          p("After clicking on both buttons, a 'population versus sample' plot will 
            be displayed below. The blue dots on the graph represent population 
            data points. The yellow dots represent the points in your random 
            sample. The blue line is the population regression line. The yellow 
            line is the estimated regression line based on the sample."),
          p("If you click on the second button, an additional histogram will be 
            displayed. It demonstrates the frequency and the distribution of 
            results (estimated beta 1) generated from 100 random samples."),
          p("A summary table will also be created each time you click on buttons.")
        ),
        hr(),
        h4("Plot(s):"),
        plotOutput("plot_sampling_1"),
        textOutput('hint1'),
        plotOutput("plot_sampling_2"),
        # tabPanel("Summary", verbatimTextOutput("summary")),
        hr(),
        h4("Summary: "),
        tableOutput("table"),
        textOutput('hint2'),
        br()
      ) # <END sampling-sidebarLayout-mainPanel>
    ) # <END sampling-sidebarLayout>
  ), # <END sampling>
  
  # Page 4: Hypothesis testing ----
  #      Examine hypothesis test based on simulated samples
  tabPanel(title = "Hypothesis Testing",
    sidebarLayout(
      sidebarPanel(
        helpText("Please specify the null hypothesis (default is 0): "),
        numericInput(inputId = "beta1_0", label = "H0: beta1 = ", value = 0),
        hr(),
        sliderInput(inputId = "n2", 
          label = "Sample Size: ", 
          min = 10, max = 300, value = 50)
      ), # <END hypothesis-sidebarLayout-sidebarPanel>
      mainPanel(
        div(    id = "tab-hypothesis-info",
          p("This application explores the logic behind hypothesis testing."),
          p("Recall that the hypothesis testing examines whether the difference 
            of the estimated values between two samples is statistically 
            significant. We first set a null hypothesis (H0) and an alternative 
            hypothesis (H1): "),
          tags$ul(
            tags$li(tags$b("H0: "), "the difference is 0 (no difference)."),
            tags$li(tags$b("H1: "), "the difference is not 0.")
          ),
          p("Then, we set a confidence interval and conduct tests to check the 
            possibility of H0, known as the p-value. If p-value falls within a 
            certain threshold, say 0.05, then we have at least 95% of confidence 
            to reject the null hypothesis. If under 95% confidence, we observed a 
            p-value that is larger than 0.05, then we faile to reject the null 
            hypothesis."),
          p("On the sidebar, there are two input widgets: H0 and sample size. 
            The H0 widget allows you to change the null hypothesis statement. 
            By default, the null hypothesis states that the estimated slope 
            (beta 1) is 0. The sample size widget allows you to change sample 
            size within the range of 10 to 300. The default sample size is 50. 
            A new result will be generated and displayed on the main panel each 
            time the widgets are updated."),
          p("On the main panel, there are two types of results: "),
          tags$ul(
            tags$li(tags$b("One Sample: "), "Draw one sample and conduct a
                    hypothesis test"),
            tags$li(tags$b("1000 Samples: "), "Draw 1000 samples, conduct a 
                    hypothesis test for each iteration, and calculate statistics"),
          )
        ),
        br(),
        h4("One Sample"),
        br(),
        tableOutput("table2"),
        textOutput('hint3'), 
        hr(),
        h4("Many Samples"), 
        br(),
        tableOutput("table3"),
        plotOutput("plot3"),
        htmlOutput('hint4'),
        br()
      ) # <END hypothesis-sidebarLayout-mainPanel>
    ) # <END hypothesis-sidebarLayout>
  ), # <END hypothesis>

  # Theme ----
  theme = shinytheme("lumen")
  
) # <END navbarPage> 
# <END ui>

# Server -----------------------------------------------------------------------
server <- function(input, output){
  
  ###
  # Lecture Schedule ---
  output$mytable = DT::renderDataTable({
    data.frame("Week" = c(1:14), "Date" = c("09/08/20","09/15/20","09/22/20","09/29/20","10/06/20","10/13/20","10/20/20","10/27/20","11/03/20","11/10/20","11/17/20","11/24/20","12/01/20","12/8/20"))
  }, rownames = FALSE, options = list(
    columnDefs = list(list(className = 'dt-center', targets = "_all"))
  ))
  
  ####
  # LR Simulator ----
  ####
  
  dat_sim <- eventReactive(
    eventExpr = input$draw, 
    valueExpr = {
      X0 <- rnorm(n = input$sampsize, mean = 0, sd = 1)
      error <- rnorm(n = input$sampsize, mean = 0, sd = input$sigma2^0.5)
      Y0 <- input$beta0 + input$beta1 * X0 + error
      return(data0 = data.frame(X = X0, Y = Y0))
    }
  )
  mod_lm <- reactive({
    data0 <- dat_sim()
    return(lm(Y ~ X, data = data0))
  })
  observeEvent(input$draw, {
    mod_out <- mod_lm()
    output$plot0 <- renderPlot(
      expr = {
        data0 <- dat_sim()
        xmin <- floor(  min(data0$X) * 1.1)
        xmax <- ceiling(  max(data0$X) * 1.1)
        ymin <- floor(  min(data0$Y) * 1.1)
        ymax <- ceiling(  max(data0$Y) * 1.1)
        r_squared <- round(summary(mod_out)$r.squared, digits = 3)
        plot(
          x = data0$X, y = data0$Y, 
          xlab = "X (I.V.)",
          ylab = "Y (D.V.)",
          main = "Simple Linear Regression Fit based on a Simulated Dateset",
          sub  = paste("R-squared=", r_squared, sep = ""),
          col = light_blue, pch = 19, 
          xlim = c(xmin, xmax),
          ylim = c(ymin, ymax)
        )
        abline(a = coef(mod_out)[1], b = coef(mod_out)[2], lwd = 2, col = blue)
        abline(a = input$beta0, b = input$beta1, 
               lty = "dotted", lwd = 2.5, col = red)
        legend("topright", 
          legend = c("Population", "Sample"), 
          lty = c(1, 2),
          col = c(red, blue), 
          title = "Regression Lines")
      }
    )
    output$hint0 <- renderText(
      "Here is a summary table of statistics on correlation coefficients. 
      This is the same as what you will get in R using the summary() function 
      on your regression model."
    )
    output$res0 <- renderTable(expr = {
        dd <- data.frame(summary(mod_out)$coefficients)
        dd$parameter <- c("beta0", "beta1")
        dd <- dd[ ,c(5, 1:4)]  # Reorder columns
        names(dd)[5] <- "p-value"
        names(dd)[3] <- "Std. Error"
        xtable(dd)
      })
  }) # <END server-lr-simulator>
  
  ####
  # Sampling distribution ----
  ####
  
  if (FALSE) { # Not run
    data <- reactive({
      if (input$selection=="A") {
        X <- rnorm(n = 10000, mean = 0, sd = 1)
        error <- rnorm(n = 10000, mean = 0, sd = 1)
        Y <- 0.5 + 1*X + error
        return(Pop = data.frame(Y = Y, X = X))
      } else if (input$selection == "B") {
        X <- rnorm(10000)
        error <- rnorm(n = 10000, mean = 0, sd = 4)
        Y <- 0.5 + 1*X + error
        return(Pop = data.frame(Y = Y, X = X))
      }
    })
  }
  
  # Generate a simulated population based on inputed coefficients
  popData <- reactive({
    X <- rnorm(n = 10000, mean = 0, sd = 1)
    error <- rnorm(n = 10000, mean = 0, sd = input$sigma2^0.5) 
    Y <- input$beta0 + input$beta1*X + error
    return(data.frame(Y = Y, X = X))
  })
  
  indices <- eventReactive(
    eventExpr = input$drawmany, 
    valueExpr =  {
      mat <- matrix(data = 0, nrow = input$n, ncol = 100)
      for (k in 1:100) {
        mat[ ,k] <- sample(x = 1:10000, size = input$n)
      }
    return(mat)
    }
  )
  
  if (FALSE) { # Not run
    SNR <- reactive({
     if (input$selection == "A") {return(1)}
     else if (input$selection == "B") {return(0.5)}
     })
  }
  
  sampleData <- eventReactive(
    eventExpr = input$drawone, 
    valueExpr = {
      data <- popData()
      return(data[sample(x = 1:10000, size = input$n), ])
    }
  )
  
  mod2 <- reactive({  # For each sample, fit a linear regression model
    return(lm(Y ~ X, data = sampleData()))
  })
  
  beta.SampleDist <- eventReactive(
    eventExpr = input$drawmany,
    valueExpr = {
      Pop <- popData()
      ID.mat <- indices()
      est.vec <- matrix(data = 0, nrow = 100, ncol = 2)
      for (k in 1:100) {
        est.vec[k,] <- coef(lm(Y ~ X, data = Pop[ID.mat[ ,k], ]))
      }
      return(est.vec)
    }
  )
  
  observeEvent(
    eventExpr = input$drawone,   # Draw one sample ----
    handlerExpr = {
      Pop <- popData()
      Sample <- sampleData()
      mod_lin_2 <- mod2()
    
      output$hint1 <- renderText({
        paste("Sample Size: ", input$n)
      })
      
      output$plot_sampling_1 <- renderPlot({
        ymin <- min(Pop$Y) - 0.25*input$sigma2^0.5
        ymax <- max(Pop$Y) + 0.25*input$sigma2^0.5
        
        plot(x = Pop$X, y = Pop$Y, 
          xlim = c(-4, 4), 
          ylim = c(ymin, ymax), 
          xlab = "X (I.V.)", ylab = "Y (D.V.)",
          main = "Population vs. One Sample",
          # sub = paste(
          #   "signal-noise-ratio=",
          #   round(input$beta1/input$sigma2^0.5, digits=2), 
          #   sep=""
          # ),
          col = light_blue, pch = 19
        )
        # text(x = -3, y = max(Pop$Y), labels = "beta1 = 1, Var(e) = 1")
        res <- lm(Y ~ X, data = Pop)
        abline(a = coef(res)[1], b = coef(res)[2], col = blue, lwd = 2)
        legend("topright", 
          legend = c("population", "sample"), 
          lty = c(1, 1),  # Solid lines
          col = c(blue, red), 
          title = ""
        )
        
        # sampleID <- sample(1:10000,input$n)
        
        # Add a sample point layer
        points(x = Sample$X, y = Sample$Y,
          col = red, pch = 19
        )
        
        # res0 <- lm(Y~X, data=Pop[sampleID,])
        
        abline(a = coef(mod_lin_2)[1], b = coef(mod_lin_2)[2], 
          col = red, lwd = 2
        )
      }) # <END drawone-plot_sampling_1>
      
      output$table <- renderTable({
        dd <- data.frame(summary(mod_lin_2)$coefficients)
        dd$parameter <- c("beta0", "beta1")
        dd <- dd[ , c(5, 1:4)]
        names(dd)[5] <- "p-value"
        names(dd)[3] <- "Std. Errow"
        xtable(dd)
      })
    }
  ) # <END drawone>
  
  observeEvent(
    eventExpr = input$drawmany,   # Draw 100 samples ----
    handlerExpr = {
      Pop <- popData()
      output$hint1 <- renderText({ paste("Sample Size: ", input$n, sep = "") })
      output$plot_sampling_1 <- renderPlot({
        ymin <- min(Pop$Y) - 0.25*input$sigma2^0.5
        ymax <- max(Pop$Y) + 0.25*input$sigma2^0.5
        
        plot(x = Pop$X, y = Pop$Y, 
          xlim = c(-4, 4), 
          ylim = c(ymin, ymax), 
          xlab = "X (I.V.)", ylab="Y (D.V.)",
          main = "Population vs. 100 Samples",
          # sub = paste(
          #   "signal-noise-ratio=",
          #   round(input$beta1/input$sigma2^0.5, digits=2), 
          #   sep=""
          # ),
          col = light_blue, pch = 19
        )
        res <- lm(Y ~ X, data = Pop)
        legend("topright", 
          legend = c("population", "sample"), 
          lty = c(1, 1),
          col = c(blue, red), 
          title = ""
        )
        est.vec <- beta.SampleDist()
        abline(a = coef(res)[1],  # Reg line for population
               b = coef(res)[2], 
               lwd = 2
        )  
        for (k in 1:100) {
          abline(a = est.vec[k, 1], 
                 b = est.vec[k, 2], 
                 col = red, lwd = 1.5
          )
        }
        
      }) # <END drawmany-plot_sampling_1>
    
      output$plot_sampling_2 <- renderPlot({ 
        est.vec <- beta.SampleDist()
        hist(x = est.vec[, 2], 
             xlab = "Estimated Beta1", 
             main = "Distribution of 100 Sample Results"
        )
        abline(v = input$beta1, 
               col = red, lwd = 2, 
               sub = paste("True Beta1 = ", input$beta1, sep="")
        )
      }) # <END drawmany-plot_sampline_2>
    
      output$hint2 <- renderText({
        paste("Try compare the standard error (SE) of beta1 in `one sample' result to 
              the S.D./S.E. of the estimated beta1 in `many samples' results.")
      })
      output$table <- renderTable({
        est.vec <- beta.SampleDist()
        tt <- data.frame(matrix(data = 0, nrow = 2, ncol = 6))
        tt[, 2] <- c(input$beta0, input$beta1)
        tt[, 3] <- round(c(input$n, input$n))
        tt[, 4] <- colMeans(est.vec)
        tt[, 5] <- apply(X = est.vec, MARGIN = 2, FUN = var)
        tt[, 6] <- tt[, 5]^0.5
        colnames(tt) <- c("Parameter", "True Value", 
                          "Sample Size", "Mean of Betahats", 
                          "Var of betahats", "SD/SE of betahats")
        tt[, 1] <- c("beta0", "beta1")
        xtable(tt)
      })
    }
  ) # <END drawmany>

  ####
  # Hypothesis Testing ----
  ####
  
  test.res <- reactive({
    Pop <- popData()
    est.vec2 <- matrix(data = 0, nrow = 1000, ncol = 2)
    for (k in 1:1000) {
      resHT <- lm(Y ~ X, data = Pop[sample(1:10000, input$n2), ])
      Tstat <- (coef(resHT)[2] - input$beta1_0) / vcov(resHT)[2, 2]^0.5
      est.vec2[k, 1] <- (1 - pt(abs(Tstat), df = input$n2 - 2))*2
      est.vec2[k, 2] <- Tstat
    }
    return(est.vec2)
  })
  
  output$hint3 <- renderText({
    HTML(paste("Point of observations: ",
      "If you choose a null hypothesis that sets beta1 differently from 0, 
      e.g. H0: beta1 = c, observe how the test results changes even given 
      the same sample data."
    ))
  })
  output$hint4 <- renderUI({
    HTML(paste("Points of observations:", 
      "1. In this example, 1000 samples were randomly drawn from the population 
      data you specified. For each of the sample, a regression line is estimated 
      and the hypothesis testing results (t values and p-values) are saved. 
      Over these 1000 replicates, we summarize how many times the null hypothesis 
      is rejected at level 5%, (i.e. p value is less than 0.05).", 
      "",
      "2. When the true beta value is the same as the null value, rejecting the 
      null represents a type I error. At level 5%, we expect about 5% of the 
      nulls are rejected by mistake. While when the true beta value is different 
      from the null value, we would like to reject the null hypothesis. 
      In which case, this percentage of H0 rejection is the power of the test.", 
      "",
      "3. you can try varying sample size and/or true beta values and etc, 
      and observe whether/how type I error rate and the power of the test changes", 
      "",
      "4. In the figure above, the distribution of the t values over 1000 tests 
      is plotted. T value = (beta.est - beta null) / SE. As you can see, this 
      distribution is always centered around 0 and follows a t distribution.",
      "Note: The density line is a little bit wiggly. This is due to relatively 
      small number of replications. If we increase the number of replicates from 
      1000 to 10000, the density curve will be smoother.", 
      sep="<br/>"))
  })
  output$table3 <- renderTable({
    est.vec2 <- test.res()
    tt <- matrix(data = 0, nrow = 1, ncol = 5)
    tt[, 1] <- input$beta1
    tt[, 2] <- input$beta1_0
    tt[, 3] <- sum(est.vec2[, 1] < 0.05) / 1000*100
    tt[, 4] <- ifelse(input$beta1 == input$beta1_0, 
                      yes = tt[, 3]/100, 
                      no = NA)
    tt[, 5] <- ifelse(input$beta1 != input$beta1_0, 
                      yes = tt[, 3]/100, 
                      no = NA)    
    colnames(tt) <- c("True value", "H0 value", 
                      "% tests rejected", "type I error", 
                      "power")
    xtable(tt)
  })
 
  output$table2 <- renderTable({
    Pop <-popData()
    resHT <- lm(Y ~ X, data = Pop[sample(x = 1:10000, size = input$n2), ])
    Tstat <- abs(coef(resHT)[2] - input$beta1_0) / vcov(resHT)[2, 2]^0.5
    
    tt <- data.frame(matrix(data = 0, nrow = 2, ncol = 6))
    tt[1, 2] <- input$beta1
    tt[1, 3] <- coef(resHT)[2]
    tt[1, 4] <- vcov(resHT)[2, 2]^0.5
    tt[1, 5] <- tt[1, 3] / tt[1, 4]
    tt[1, 6] <- (1 - pt(abs(tt[1, 5]), df = input$n2 - 2))*2
    tt[2, 2] <- input$beta1
    tt[2, 3] <- tt[1, 3]
    tt[2, 4] <- tt[1, 4]
    tt[2, 5] <- (tt[2, 3] - input$beta1_0) / tt[2, 4]
    tt[2, 6] <- (1 - pt(abs(tt[2, 5]), df = input$n2 - 2))*2
    colnames(tt) <- c("H0", "True", 
                      "Estimated", "Std. Error", 
                      "T stat", "p value")
    tt[, 1] <- c("H0: beta1 = 0", paste("H0: beta1 = ", input$beta1_0, sep=""))
    xtable(tt)
  })
    
  output$plot3 <- renderPlot({
    est.vec2 <- test.res()
    plot(density(x = est.vec2[, 2]), 
         xlab = NA, ylab=NA, 
         main = "Distribution of T statistics under Null Hypothesis",
         col = blue, lwd = 1.5
    )
    abline(v = 0, col = red, lwd = 1.5, lty = "dashed")
  })
}
# <END server>

# Generate the application -----------------------------------------------------
shinyApp(ui = ui, server = server)
