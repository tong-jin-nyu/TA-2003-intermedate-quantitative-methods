# ----- ----- Title ----- -----
#
# Title: APSTA-GE: 2003 Intermediate Quantitative Methods
# Subtitle: Shiny Course Site
# 
# Data Created: 10/14/2020
# Data Modified: 10/22/2020
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
# ----- ----- Copyright ----- -----
# Copyright (c) Ying Lu, 2020
# 
# ----- ----- Contents ----- -----
# 
# 1. Linear regression simulator
# 2. Sampling Distribution Generator
# 3: Hypothesis Testing Tester
# 

# Dependencies -----------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,
               broom,
               xtable,
               shinythemes,
               DT
)

# Functions --------------------------------------------------------------------

# Color palette ----------------------------------------------------------------
blue <- "#36558F"
light_blue <- "#81B9DB"
yellow <- "#ffb74a"

# Links ------------------------------------------------------------------------
a_ying_lu_nyu <- "mailto:ying.lu@nyu.edu"
a_tong_jin_nyu <- "mailto:tj1061@nyu.edu"
a_lisa_song_nyu <- "mailto:ls5209@nyu.edu"
a_syllabus <- "https://docs.google.com/document/d/1YSyH7IN3p-vUzAkcjw4CUFB-utiOI4DPu4KqA8YMZNQ/edit?usp=sharing"
a_zoom_reg <- "https://nyu.zoom.us/meeting/register/tJAsduCvrzwsGtCAswXfHUr1a2Wz-gYTZnZg"

# User Interface ---------------------------------------------------------------
ui <- navbarPage(  # Create pages with a top level navi bar
  
  # App title ----
  title = "NYU APSTA-GE: 2003 Intermediate Quantitative Methods",
  
  # Page 1: home ----
  tabPanel(title = "Home", class = "home",
    
    div(id = "home-title",
      h3("Welcome to IQM: General Linear Model Course Site")
    ),
    hr(), # Horizontal line
    
    # Create a navi list layout
    navlistPanel(
      # Content 1: course info ----
      tabPanel(title = "Course Information", class = "home-navi-info",
        div(id = "home-navi-info-instructor",
          h4("Instructors: "),
          tags$ul(
            tags$li("Instructor: ", a(href=a_ying_lu_nyu, "Ying Lu")),
            tags$li("Teaching Assistant: ", a(href=a_tong_jin_nyu, "Tong Jin")),
            tags$li("Teaching Assistant: ", a(href=a_lisa_song_nyu, "Lisa Song")),
          ),
                                
                                h4("Course Information:"),
                                tags$ul(
                                  tags$li("Term: Fall 2020"),
                                  tags$li(a(href=a_syllabus, "Syllabus"), "(NYU ID required)"),
                                  tags$li("Zoom Registration: ", a(href="a_zoom_reg", "Click here")),
                                  tags$li("Zoom Classroom: ", strong("941 1971 7604"), 
                                          "(Passcode available on the Syllabus)"),
                                ),
                        )
               ) # <END home-info>
      ), # <END> home-navi-info
    ), # <END> home-navi
  ), # <END> home
  
  
  # Tab: LM simulator ----
  #      Fit a regression model with simulated data
  tabPanel(title="LM Simulator", class = "lm-simulator",
    withMathJax(),
    div(    id = "tab-lm-simulator-title",
      h3("Linear Regression Simulator")
    ),
    hr(),                                                      # Horizontal line 
    # Sidebar: a slider and selection inputs ----
    sidebarLayout(
      sidebarPanel(
         numericInput(inputId="beta0", label = "Intercept", value = 0),
         numericInput(inputId="beta1", label = "Slope", value = 0),
         numericInput(inputId="sigma2", label = "Error Variance", value = 1, min=0),
         hr()
        ), # <END lm-simulator-sidebarLayout-sidebarPanel>
      
      mainPanel(
        div(    id = "tab-lm-simulator-info",
          p(strong("Description: "),
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
        numericInput(inputId="sampsize", label="Sample Size", value = 10, min = 2),
        actionButton(inputId="draw", label = "Draw Sample and Fit Model"),                 
        hr(),
        plotOutput("plot0", width = "80%"),
        textOutput('hint0'),
        dataTableOutput("res0")
      ) # <END lm-simulator-sidebarLayout-mainPanel>
    ) # <End lm-simulator-sidebarLayout>
  ), # <END lm-simulator>

# 102020, Tong: to-do ----------------------------------------------------------
  tabPanel(title="Sampling Distribution", 
          sidebarLayout(
            # Sidebar with a slider and selection inputs
            sidebarPanel(
              actionButton("drawone", "Draw one sample"),
             # h1(),
              actionButton("drawmany", "Draw many samples"),
            #  h1(),
         #     selectInput("selection", "Choose a signal-noise-ratio setup:",
          #        c("high signal noise ratio"="A", "low signal noise ratio"="B")),

              sliderInput("n", "Sample Size:",min = 10,  max = 300, value = 50)
             ), # end sidebarPanel
            mainPanel(
              tabsetPanel(type = "tabs",
                        tabPanel("Plot", textOutput('hint1'), plotOutput("plot1"),plotOutput("plot2") ),                                                                
                       # tabPanel("Summary", verbatimTextOutput("summary")),
                        tabPanel("Summary", textOutput('hint2'),tableOutput("table"))
            )
            ) # end mainPanel
          ) # end sidebarLayout
  ), # end tabPanel Sampling Variation
      
  tabPanel(title="Hypothesis Testing",
                 sidebarLayout(
                   # Sidebar with a slider and selection inputs
                   sidebarPanel(
                     helpText("Please specify the null hypothesis (default is 0)"),
                     numericInput(inputId="beta1_0", label = "H0: beta1=", value = 0),
                     sliderInput("n2", "Sample Size:",min = 10,  max = 300, value = 50)
                   ), # end sidebarPanel
                   mainPanel(
                     tabsetPanel(type = "tabs",
                                 tabPanel("One Sample", textOutput('hint3'), tableOutput("table2") ),                                                                
                                 # tabPanel("Summary", verbatimTextOutput("summary")),
                                 tabPanel("Many Samples", htmlOutput('hint4'), tableOutput("table3"),plotOutput("plot3"))
                     )
                   ) # end mainPanel
                 ) # end sidebarLayout
  ), # tablepanel Hypothesis Testing

  # Theme ----
  theme = shinytheme("lumen")
  
) # <END navbarPage> 

# Server -----------------------------------------------------------------------
server <- function(input, output){
  
  # Server: LM Simulator ----
  dat_sim <- eventReactive(
    eventExpr = input$draw, 
    valueExpr = {
      X0 <- rnorm(n = input$sampsize, mean = 0, sd = 1)
      Y0 <- input$beta0 + input$beta1 * X0 + 
            rnorm(n = input$sampsize, mean = 0, sd = input$sigma2^0.5)
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
        xmax <- ceiling(max(data0$X) * 1.1)
        ymin <- floor(  min(data0$Y) * 1.1)
        ymax <- ceiling(max(data0$Y) * 1.1)
        r_squared <- round(summary(mod_out)$r.squared, digits = 3)
        plot(
          x = data0$X, y = data0$Y, 
          xlab = "X (I.V.)",
          ylab = "Y (D.V.)",
          main = "Simple Linear Regression Fit based on a Simulated Sample",
          sub  = paste("R-squared=", r_squared, sep = ""),
          col = light_blue, pch = 19, 
          xlim = c(xmin, xmax),
          ylim = c(ymin, ymax)
        )
        abline(a = coef(mod_out)[1], b = coef(mod_out)[2], lwd = 2, col = blue)
        abline(a = input$beta0, b = input$beta1, 
               lty = "dotted", lwd = 2.5, col = yellow)
        legend("topright", 
          legend = c("Population", "Sample"), 
          lty = c(1, 2),
          col = c(yellow, blue), 
          title = "Regression Lines")
      }
    )
    output$hint0 <- renderText("Here is a summary table of statistics on 
                               correlation coefficients. This is the same 
                               as what you will get in R using the summary() 
                               function on your regression model.")
    output$res0 <- renderDataTable(
      expr = {
      dd <- data.frame(summary(mod_out)$coefficients)
      dd$parameter <- c("beta_0", "beta_1")
      dd <- dd[ ,c(5, 1:4)]  # Drop unnecessary results
      names(dd)[5] <- "p-value"
      names(dd)[3] <- "Std. Error"
      dd[, 2:5] <- lapply(dd[, 2:5], round,3)
      dd
      }, 
      options = list(
        columnDefs = list(
          list(className = "dt-center", targers = 1)
        ),
        pageLength = 2
      ))
  }) # <END server-lm-simulator>
  
  
  #############################
  ## Sampling Distribution Tab
  #############################  

  #data <- reactive({if (input$selection=="A") {
   # X <- rnorm(10000)
  #  Y <- 0.5+1*X+rnorm(10000, 0, 1) 
   # return(Pop=data.frame(Y=Y, X=X))
  #}
   # else if (input$selection=="B") {
    #  X <- rnorm(10000)
     # Y <- 0.5+1*X+rnorm(10000, 0, 4)
    #  return(Pop=data.frame(Y=Y, X=X))
    #}
    
  #  })
  
  popData <- reactive({
    X <- rnorm(10000)
    Y <- input$beta0+input$beta1*X+rnorm(10000, 0, input$sigma2^0.5) 
    return(data.frame(Y=Y, X=X))
  })

  indices <- eventReactive(input$drawmany, { 
    mat <- matrix(0, input$n,100)
  
      for (k in 1:100) {
      mat[,k]<-sample(1:10000,input$n)
    }
    return(mat)
   })
  
 # SNR <- reactive({ 
  #  if (input$selection=="A") {return(1)}
  #  else if (input$selection=="B") {return(0.5)}
  #  })
  
  sampleData <- eventReactive(input$drawone, {
    data <- popData()
   return(data[sample(1:10000, input$n),])
  })
  
  mod2 <- reactive({
    return(lm(Y~X, data=sampleData()))
  })
  
  beta.SampleDist <- eventReactive(input$drawmany,{
    Pop <- popData()
    
    ID.mat <- indices()
    est.vec<-matrix(0,100,2)
    for (k in 1:100) {
      est.vec[k,] <- coef(lm(Y~X, data=Pop[ID.mat[,k],]))
    }
    return(est.vec)
  })
  
  
  observeEvent(input$drawone, {
    
    Pop <- popData()
    Sample <- sampleData()
    rr2 <- mod2()
    
    output$hint1 <- renderText({
      
      paste("Blue dots are population data, red dots are the random 
            sample of size", input$n, "from the population. The 
            black line is the population regression line and the 
            red line is the estimated regression line based on the sample.")
      })
    
    output$plot1 <- renderPlot(
      {
        plot(Pop$X, Pop$Y, xlim=c(-4,4), ylim=c(min(Pop$Y)-0.25*input$sigma2^0.5,max(Pop$Y)+0.25*input$sigma2^0.5), xlab="X", ylab="Y",
             main = "Population Data and population regression line",
             sub =paste("signal-noise-ratio=",round(input$beta1/input$sigma2^0.5, digits=2), sep=""),
             col = "#75AADB", pch=19)
        text(-3, max(Pop$Y), "beta1=1, Var(e)=1")
        res <- lm(Pop$Y~Pop$X)
        abline(a=coef(res)[1], b=coef(res)[2], lwd=2)
        legend("topright", legend=c("population", "sample"), lty=rep(1,2),col=c("black","red"), title="Regression lines")
        
       # sampleID<-sample(1:10000,input$n)
        points(Sample$X,Sample$Y,
                col = "red", pch=19 )
        #  res0 <- lm(Y~X, data=Pop[sampleID,])
          abline(a=coef(rr2)[1], b=coef(rr2)[2], col="red", lwd=2)
      }
    ) # end renderPlot plot1
    
  output$plot2 <- renderPlot(
      {
     #   plot(1,1, type="n", col="white", xaxt="n", yaxt="n", xlab=NULL, ylab=NULL)
      }
    ) # end renderplot 2
  
    output$table <- renderTable({
      
      dd<-data.frame(summary(rr2)$coefficients)
      dd$parameter<-c("beta0", "beta1")
      dd<-dd[,c(5,1:4)]
      names(dd)[5]<-"pval"
      names(dd)[3]<-"SE"
      xtable(dd)
      
    }) # end render Table 
    
  })
  
  observeEvent(input$drawmany, {
    output$hint1 <- renderText({
      
      paste("Blue dots are population data. The 
            black line is the population regression line and the 
            red lines are the estimated regression lines based on 
            100 random samples, each of sample size", input$n)
    })
    output$plot1 <- renderPlot(
      {
        Pop <- popData()
        
        plot(Pop$X, Pop$Y, xlim=c(-4,4), ylim=c(min(Pop$Y)-0.25*input$sigma2^0.5,max(Pop$Y)+0.25*input$sigma2^0.5), xlab="X", ylab="Y",
             main = "Population versus Sample",
             sub =paste("signal-noise-ratio=", round(input$beta1/input$sigma2^0.5, digits=2), sep=""),
             col = "#75AADB", pch=19)
        text(-3, max(Pop$Y), paste("beta1=",input$beta1, "  Var(e)=", input$sigma2, sep=""))
        res <- lm(Pop$Y~Pop$X)
        legend("topright", legend=c("population", "sample"), lty=rep(1,2),col=c("black","red"), title="Regression lines")
        
        est.vec<-beta.SampleDist()
  
        for (k in 1:100) {
           abline(a=est.vec[k,1], b=est.vec[k,2], col="red", lwd=1.5)
        }
          
        abline(a=coef(res)[1], b=coef(res)[2], lwd=2)
        }) #end renderplot 1
    
    output$plot2 <- renderPlot(
      { 
        est.vec<-beta.SampleDist()
        hist(est.vec[,2], xlab="beta1hat", main="Histogram of many sample results")
        abline(v=input$beta1, col="green", lwd=2, sub=paste("true beta1=", input$beta1, sep=""))
      }
    ) # end renderplot 2
    
    output$hint2 <- renderText({
      paste("Try compare the SE of beta1 in `one sample' result with the SD/SE of betahat in `many samples' results")
    })
    output$table <- renderTable({
      est.vec<-beta.SampleDist()
      tt <- data.frame(matrix(0,2,6))
      tt[,2]<-c(input$beta0, input$beta1)
      tt[,3]<-round(c(input$n, input$n))
      tt[,4]<-colMeans(est.vec)
      tt[,5]<-apply(est.vec,2,var)
      tt[,6]<-tt[,5]^0.5
      colnames(tt)<-c("Parameter","True Value","Sample Size", "Mean of Betahats", "Var of betahats", "SD/SE of betahats")
      tt[,1]<-c("beta0", "beta1")
           xtable(tt)
      
    })
  })

  #########################
  ## Hopythesis Testing
  ########################
  test.res <- reactive({
    Pop <- popData()
    
    est.vec2<-matrix(0,1000,2)
    for (k in 1:1000) {
      resHT<-lm(Y~X, data=Pop[sample(1:10000,input$n2),])
      Tstat <- (coef(resHT)[2]-input$beta1_0)/vcov(resHT)[2,2]^0.5
      est.vec2[k,1] <- (1-pt(abs(Tstat), df=input$n2-2))*2
      est.vec2[k,2] <- Tstat
    }
    return(est.vec2)
  })
  
  output$hint3 <- renderText({"Point of observation: if you choose a null hypothesis that sets beta1 different from 0, e.g. H0: beta1=c, 
    observe how the test results changes even given the same sample data."})
  
  output$hint4 <- renderUI({HTML(paste("Points of observations:", "1. In this `many samples' example, 1000 samples were randomly drawn 
    from the population data you specified. For each of the sample, a regression line is estimated 
    and the hypothesis testing results (t values and p-values) are saved. Over these 1000 replicates,
    we summarize how many times the null hypothesis is rejected at level 5%, i.e. p value is 
    less than 0.05).","2. When the true beta value is the same as the null value, rejecting the null 
    represents a type I error. At level 5%, we expect about 5% of the nulls are rejected by mistake", 
        "While when the true beta value is different from the null value, we would like to reject the null 
    hypothesis. In which case, this percentage of H0 rejection is the power of the test. ", "3. you can try 
    varying sample size and/or true beta values and etc, and observe whether/how type I error rate and
    the power of the test changes", "4. In the figure below, the 
        distribution of the t values over 1000 tests is plotted. T val=(beta.est-beta null)/SE. As you 
        can see this distribution is always centered around 0, and follows a t distribution. Note here the 
        density line is a little bit wiggly, this is due to relatively small number of replications. If 
        we increase the number of replicates from 1000 to 10000, the density curve will be smoother.", sep="<br/>"))})
  output$table3 <- renderTable({
    est.vec2<-test.res()
    tt <- matrix(0,1,5)
    tt[,1]<-input$beta1
    tt[,2]<-input$beta1_0
    tt[,3]<-sum(est.vec2[,1]<0.05)/1000*100
    tt[,4]<-ifelse(input$beta1==input$beta1_0, tt[,3]/100, NA)
    tt[,5]<-ifelse(input$beta1!=input$beta1_0, tt[,3]/100, NA)    
    colnames(tt)<-c("True value","H0 value", "% tests rejected", "type I error","power")
    xtable(tt)
  })
 
  output$table2 <- renderTable({
    Pop <-popData()
    resHT<-lm(Y~X, data=Pop[sample(1:10000,input$n2),])
    Tstat <- abs(coef(resHT)[2]-input$beta1_0)/vcov(resHT)[2,2]^0.5
    
    tt<-data.frame(matrix(0,2,6))
    tt[1,2]<-input$beta1
    tt[1,3]<-coef(resHT)[2]
    tt[1,4]<-vcov(resHT)[2,2]^0.5
    tt[1,5]<-tt[1,3]/tt[1,4]
    tt[1,6]<-(1-pt(abs(tt[1,5]), df=input$n2-2))*2
    tt[2,2]<-input$beta1
    tt[2,3]<-tt[1,3]
    tt[2,4]<-tt[1,4]
    tt[2,5]<-(tt[2,3]-input$beta1_0)/tt[2,4]
    tt[2,6]<-(1-pt(abs(tt[2,5]), df=input$n2-2))*2
    colnames(tt)<-c("H0", "True", "Est","SE","T stat","p val")
    tt[,1]<-c("H0:beta1=0", paste("H0:beta1= ", input$beta1_0, sep=""))
    xtable(tt)
    
  })
    
  output$plot3 <- renderPlot({
    est.vec2<-test.res()
    plot(density(est.vec2[,2]), xlab=NA, ylab=NA, main="Distribution of T statistics under Null")
    abline(v=0)
  }
  )
}



# Generate the application -----------------------------------------------------
shinyApp(ui = ui, server = server)
