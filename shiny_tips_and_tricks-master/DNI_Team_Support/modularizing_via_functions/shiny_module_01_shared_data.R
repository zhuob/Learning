module_01             <- new.env()
module_01$name        <- "Shared 'data' object"
module_01$id          <- "shared_data"
module_01$icon        <- "dashboard"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Component 01
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module_01$comp_01 <- new.env()
module_01$comp_01$ui <- function(){
    box(
        width = 12,
        title = "Histogram", status = "primary", solidHeader = TRUE, collapsible = FALSE,
        plotOutput("m1c1_plot1", height = 250)
    )
}

module_01$comp_01$server <- function(input, output, session, data){
    
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Component 02
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module_01$comp_02 <- new.env()
module_01$comp_02$ui <- function(){
    verticalLayout(
        box(
            width = 12,
            title = "Controls", status = "primary", solidHeader = TRUE, collapsible = FALSE,
            sliderInput("m1c2_slider", "Number of observations:", 1, 100, 50)
        ),
        
        box(
            width = 12,
            title = "Extra 01", status = "primary", solidHeader = TRUE, collapsible = FALSE,
            tags$br(),
            h3("Extra content to make the page taller to demonstrate page scrolling"),
            tags$br()
        ),
        
        box(
            width = 12,
            title = "Extra 02", status = "primary", solidHeader = TRUE, collapsible = FALSE,
            tags$br(),
            h3("Extra content to make the page taller to demonstrate page scrolling"),
            tags$br()
        ),
        
        box(
            width = 12,
            title = "Extra 03", status = "primary", solidHeader = TRUE, collapsible = FALSE,
            tags$br(),
            h3("Extra content to make the page taller to demonstrate page scrolling"),
            tags$br()
        ),
        
        box(
            width = 12,
            title = "Extra 04", status = "primary", solidHeader = TRUE, collapsible = FALSE,
            tags$br(),
            h3("Extra content to make the page taller to demonstrate page scrolling"),
            tags$br()
        ),
        
        box(
            width = 12,
            title = "Extra 05", status = "primary", solidHeader = TRUE, collapsible = FALSE,
            tags$br(),
            h3("Extra content to make the page taller to demonstrate page scrolling"),
            tags$br()
        ),
        
        # Add some "scroll" padding at the bottom to panel looks better.
        tags$br(),
        tags$br()
    )
}

module_01$comp_02$server <- function(input, output, session, data){
    output$m1c1_plot1 <- renderPlot({
        plot_data <- data$histdata[seq_len(input$m1c2_slider)]
        hist(plot_data)
    })
}