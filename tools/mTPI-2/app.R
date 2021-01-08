.libPaths("~")
install.packages(c("DT"))

library(shiny)
library(DT)
library(RColorBrewer)
library(colorspace)
library(crosstalk)
library(ggplot2)
library(gtable)
library(htmlwidgets)
library(labeling)
library(lazyeval)
library(dplyr)
library(matrixStats)
library(tidyr)
library(scales)



source('mtpi2_fun-v3.R')

# Define UI
ui <- fluidPage(
  titlePanel("mTPI-2"),
  tabsetPanel(
    type="tab",
    tabPanel(
      "Dose Escalation Decision Table", 
      hr(),
      fluidRow(
        column(6,plotOutput("plot1"),offset=3),
        column(4, 
               sliderInput("target",
                           h6("Target Toxicity Rate"),
                           value = 0.3,
                           min = 0,
                           max = 1),
               sliderInput("tolerance1", 
                           h6("Equivalence Radius -"), 
                           value = 0.05, 
                           min = 0, 
                           max = 0.5,
                           step = 0.01),      
               sliderInput("tolerance2", 
                           h6("Equivalence Radius +"), 
                           value = 0.05, 
                           min = 0, 
                           max = 0.5,
                           step = 0.01)),   
        
        column(4,
               sliderInput("cocap",
                           h6("Cohort Cap"),
                           min = 0,
                           max = 50,
                           value = 9),
               sliderInput("tox",
                           h6("Unacceptable Toxicity: Prob(Overdosing)"),
                           min = 0,
                           max = 1,
                           value = 0.95, 
                           step = 0.01)),
        column(4,
               numericInput("a", "Beta prior: a:", 1, min = 0, max = 100),
               numericInput("b", "Beta prior: b", 1, min = 0, max = 100),
               downloadButton('downloadData1', 'Download Plot'),
               downloadButton('downloadData2', 'Download Full Table')
        ))), # end 1st tab
    tabPanel("Operating Characteristics", 
             sidebarPanel(
               fileInput("file1", "Optional: Choose a CSV File to Replace Existing Decision Matrix",
                         accept = c(
                           "text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
               ),
               numericInput("nsim", "Number of Simulations", min = 0, max = 100000, value = 1000),
               sliderInput("nmax",
                           h6("Maximum Sample Size for the Trial",
                           tags$strong(tags$span(style = "color:red", "(Must be a multiple of Group Size)"))),
                           54,
                           min = 10,
                           max = 100,
                           step = 1),
               sliderInput("ndose",
                           h6("Number of Doses"), 
                           6, 
                           min = 2, 
                           max = 10,
                           step = 1),
               sliderInput("cosize", 
                           h6("Group Size"), 
                           3, 
                           min = 1, 
                           max = 10,
                           step = 1), 
               # change made ---------------------------------
               uiOutput("secondSelection"),
               # end of change ---------------------------------
               
               textInput('trueDLT', 'Enter a Vector of True DLT Probabilities', "0.05,0.1,0.2,0.25,0.3,0.35"),
               actionButton("Gosimu",label="Simulate")),
             mainPanel(  tabsetPanel(type = "tabs",
                                     tabPanel("Operating Characteristics",DT::dataTableOutput("Simu1")),
                                     tabPanel("Summary Statistics",DT::dataTableOutput("Simu2"))))), # end 2nd tab
    
    tabPanel("Toxicity Estimation", 
             sidebarPanel(
               # fileInput("file2", "Load an Excel File(.xlsx) for Isotonic Regression Analysis",
               #           accept = c(
               #             "text/csv",
               #             "text/comma-separated-values,text/plain",
               #             ".csv",
               #             ".xlsx")
               # ),
               # hr(),
               textInput('Neach', 'N at each dose level', "3,6,3,6,3,6"),
               textInput('DLTeach', 'Number of DLTs at each dose level', "0,1,0,0,0,2"),
               #textInput('trueDLT2', 'Enter a Vector of True DLT Probabilities', "0.05,0.10,0.20,0.25,0.30,0.35"),
               sliderInput("target2",
                           h6("Target Toxicity Rate"),
                           value = 0.3,
                           min = 0,
                           max = 1),
               # hr(),
               # actionButton("dataloaded",label="Load Data"),
               hr(),
               actionButton("Goiso",label="Get Results")
               #downloadButton('downloadiso', 'Download Results')
             ),
             # sidebarPanel(tableOutput("showdata")),
            # mainPanel( DT::dataTableOutput("ISO"))) # end 3rd tab
            mainPanel(
              DT::dataTableOutput("ISO")),
            fluidRow(
              column(4),
              column(8, downloadButton('downloadData999', 'Download Plot'))),
            fluidRow(column(4),   
            column(8, plotOutput("isoEst")))
            )
            
  ))

# Define server logic 
server <- shinyServer(function(input, output, session) {
  
  
  output$secondSelection <- renderUI({
    sliderInput("startDose", 
                h6("Starting Dose"), 
                3, 
                min = 1, 
                max = input$ndose, 
                step = 1)
    
  })
  
  
  ## update the step in the slider bar
  
  observe({
    step_var <- input$cosize
    # num_dose <- ipnut$ndose
    # cohort_cap <- input$cocap
    updateSliderInput(session, inputId = "nmax", value = input$cocap*input$ndose, 
                      min = 2*step_var, max = 100, step = step_var)
  })
  
  # Code for Output  
  output$plot1 <- renderPlot({
    DecisionPlot(input$nmax, input$cocap,input$tolerance1,
                 input$tolerance2,input$a,input$b,input$tox,input$target)
  }
  )
  
  # download decision plot
  output$downloadData1 <- downloadHandler(
    filename = function() {
      "GNGplots.png"
    },
    content = function(file) {
      png(file,width=800, height=550)
      DecisionPlot(input$nmax, input$cocap,input$tolerance1,
                   input$tolerance2,input$a,input$b,input$tox,input$target)
      dev.off()
    }
    
  )
  
  
  # Download the decision matrix as a CSV file (full matrix)
  output$downloadData2 <- downloadHandler(
    filename = function() {paste('gngtable.csv')},
    content = function(file) {
      xx =  DecisionPlot(input$nmax, input$cocap,input$tolerance1,
                         input$tolerance2,input$a,input$b,input$tox,input$target)
      write.csv(xx, file, row.names=FALSE)
    }
  )
  
  observeEvent(input$file1, {
    values <- reactiveValues(df_data = NULL)
    values$df_data <- read.csv(file=input$file1$datapath,header=T,check.names=F)
    xx <<- values$df_data
  })
  
  out <- eventReactive(input$Gosimu, {
    tdose <- as.numeric(unlist(strsplit(input$trueDLT,",")))
    Gosim(tdose,input$ndose,input$nmax,input$cosize,input$target,
          input$tolerance1,input$nsim,input$tolerance2,
          input$cocap,input$tox,input$a,input$b, input$startDose
    )
  })
  
  output$Simu1 <- DT::renderDataTable({ 
    cohs0 <- input$cosize
    mxn <- input$nmax
    ncohd0 <- input$cocap
    pt <- input$target
    pt1 <- pt-input$tolerance1
    pt2 <- pt+input$tolerance2	## target toxicity
    apr <- input$a
    bpr <- input$b					## hyper a b
    p.ud <- input$tox
    
    PmTPIDS <- out()$PerDS[,c(1,2,3,4,9,10,8,5,6,7)]
    PmTPIDS[,2]=round(PmTPIDS[,2]*100, 1)
    PmTPIDS[,3]=round(PmTPIDS[,3], 1)
    PmTPIDS[,4]=round(PmTPIDS[,4], 1)
    PmTPIDS[,5]=sprintf("%2.2f", PmTPIDS[,5])
    PmTPIDS[,6]=sprintf("%2.2f", PmTPIDS[,6])
    PmTPIDS[,7]=sprintf("%2.2f", PmTPIDS[,7])
    PmTPIDS[,8]=sprintf("%2.2f", PmTPIDS[,8])
    PmTPIDS[,9]=sprintf("%2.2f", PmTPIDS[,9])
    PmTPIDS[,10]=sprintf("%2.2f", PmTPIDS[,10])
    #print(PmTPIDS)
    
    colnames(PmTPIDS)=c("     ","       ")[c(2,2,2,2,2,1,1,1,2,1)]
    button_csv = "[{extend: 'csv',
    text: 'Save'}]"
    button_print = "[{extend: 'print',
    text: 'Print'}]"
    DT::datatable(PmTPIDS,
                  rownames = FALSE,
                  colnames = c("Doses Levels","Tox. Risk (%)","MTD Selected (%)","Dose Tested (%)",
                               "Expected N (Tested)","Expected DLTs (Tested)",
                               "DLT Rate (Tested)","Expected Isotonic Estimates",
                               "Expected N (Overall)","Expected DLTs (Overall)"),
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtip',
                    buttons =list(DT::JS(button_csv),DT::JS(button_print)) 
                  )
                  
    )
    }) 
  
  output$Simu2 <- DT::renderDataTable({ 
    cohs0 <- input$cosize
    mxn <- input$nmax
    ncohd0 <- input$cocap
    pt <- input$target
    pt1 <- pt-input$tolerance1
    pt2 <- pt+input$tolerance2	## target toxicity
    apr <- input$a
    bpr <- input$b					## hyper a b
    p.ud <- input$tox
    
    tmp1 <- out()$PerTR
    tmp1[,1]=round(tmp1[,1], 0)
    tmp1[,2]=round(tmp1[,2], 1)
    tmp1[,3]=round(tmp1[,3], 1)
    tmp1[,4]=round(tmp1[,4], 0)
    tmp1[,5]=round(tmp1[,5], 2)
    
    
    tmp1[,5]=sprintf("%2.2f", tmp1[,5])
    
    
    colnames(tmp1)=c("      ","         ")[c(1,1,2,1,2)]
    rownames(tmp1)=c("Total N","Total DLTs","N at Toxic Doses","DLTs at Toxic Doses")
    button_csv = "[{extend: 'csv',
    text: 'Save'}]"
    button_print = "[{extend: 'print',
    text: 'Print'}]"
    DT::datatable(tmp1,
                  colnames = c("","Min.","Median","Mean","Max","Std.Dev"),
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtip',
                    buttons =list(DT::JS(button_csv),DT::JS(button_print)) 
                  )
                  
    )
    
    
})  
  #  observeEvent(input$file2, {
  #    rdata <- reactiveValues(df_data = NULL)
  # #   rdata$df_data <- read.table(file=input$file2$datapath,header=T,check.names=F)
  #    rdata$df_data <- read.xlsx(input$file2$datapath,sheetIndex=1)
  #    indat <<- rdata$df_data
  #  })
  
  # dat <- eventReactive(input$dataloaded, {
  #   n <- as.integer(unlist(strsplit(input$Neach,",")))
  #   DLTs <- as.integer(unlist(strsplit(input$DLTeach,",")))
  #   if(length(n)!=length(DLTs)) stop("Number of doses and number of DLTs do not match!")
  #   Doses <- 1:length(n)
  #   indat <<- data.frame(Doses,n,DLTs)
  #   
  #   indat
  # })  
  
  res <- eventReactive(input$Goiso, {
    n <- as.integer(unlist(strsplit(input$Neach,",")))
    DLTs <- as.integer(unlist(strsplit(input$DLTeach,",")))
    #trueDLT2 <- as.numeric(unlist(strsplit(input$trueDLT2,",")))
    if(length(n)!=length(DLTs)) stop("Number of doses and number of DLTs do not match!")
    #if(length(n)!=length(trueDLT2)) stop("Number of doses and number of DLT probabilities do not match!")
    #if(length(DLTs)!=length(trueDLT2)) stop("Number of DLTs and number of DLT probabilities do not match!")
    
    #if(sum(sort(trueDLT2) == trueDLT2) != length(trueDLT2)) stop("DLT probabilites are not in ascending order")
    
    Doses <- 1:length(n)
    indat <<- data.frame(Doses,n,DLTs)
    temp <- ResultTable(indat,input$target2)
    # temp$trueDLT <- trueDLT2
    
    temp
  })  
  
  prod_figure <- eventReactive(input$Goiso, {
    
      dat1 <- res()
      names(dat1) <- c("Doses","n","DLTs","Raw.Est","Iso.Est","MTD" )#, "trueDLT")
      # print(dat1)
      dat_plot <- dat1 %>% select(Doses, Raw.Est, Iso.Est ) %>% #, trueDLT) %>% 
        gather(key = "Estimate", value = "pct", -c(1)) %>% 
        mutate(Doses = factor(Doses), 
               Estimate = ifelse(Estimate == "Raw.Est", "Emperical Estimate", "Isotonic Regression"))
      #print(dat_plot)
      ggplot(data = dat_plot, aes(x = Doses, y = pct)) + 
        geom_point(aes(shape = Estimate, color = Estimate), size = 5) + 
        geom_hline(yintercept = input$target2, color = "red", linetype = "dashed", size = 1.5) + 
        scale_y_continuous(breaks = seq(0, 0.5, by = 0.05), labels = percent) +
        labs(x = "Doses", y = "Toxicity Probability") + 
        theme(legend.position = "bottom", axis.text = element_text(size = 15),
              legend.text = element_text(size = 12), 
              axis.title = element_text(size = 18))  
    }
                          
  )
  # output$showdata <- renderTable({
  #   dat()
  # })
  
  output$ISO <- DT::renderDataTable({
    button_csv = "[{extend: 'csv',
    text: 'Save'}]"
    button_print = "[{extend: 'print',
    text: 'Print'}]"
    DT::datatable(res()[, -7],
                  colnames = c("Doses","n","DLTs","Raw.Est","Iso.Est","MTD"),# "trueDLT", "target"),
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtip',
                    buttons =list(DT::JS(button_csv),DT::JS(button_print)) 
                  ))
    })

  output$downloadData999 <-  downloadHandler(
    filename = function() {
      "Est.png"
    },
    content = function(file){
      ggsave(file, prod_figure(), width = 10, height = 7)
    }
  ) 
  
  
  output$isoEst <- renderPlot({
    prod_figure()
    }
    
    )
  

  
  # Download the isotonic regression matrix as a CSV file
  # output$downloadiso <- downloadHandler(
  #    filename = function() {paste('mtd.csv')},
  #    content = function(file) {
  #      write.csv(res(), file, row.names=FALSE)
  #    }
  #  )
  })# end server

# Run the application 
shinyApp(ui = ui, server = server)



