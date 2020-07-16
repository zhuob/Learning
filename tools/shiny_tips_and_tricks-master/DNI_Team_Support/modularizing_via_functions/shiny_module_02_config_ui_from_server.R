module_02    <- new.env()
module_02$name <- "Config UI From Server"
module_02$id   <- "config_ui_from_server"
module_02$icon <- "th"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Component 01
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module_02$comp_01 <- new.env()
module_02$comp_01$ui <- function(){
    fluidRow(
        shiny::column(
            4,
            wellPanel(
                selectInput(inputId = 'm2c1_xcol',      label = 'X Variable', choices = NULL),
                selectInput(inputId = 'm2c1_ycol',      label = 'Y Variable', choices = NULL),
                numericInput(inputId = 'm2c1_clusters', label = 'Cluster count', 3, min = 1, max = 9)
            )
        ),
        shiny::column(
            8,
            box(
                width = 12,
                title = "Iris k-means clustering", status = "primary", solidHeader = TRUE, collapsible = FALSE,
                plotOutput('m2c1_plot1')
            )
        )
    )
}

module_02$comp_01$server <- function(input, output, session, data){
    # Initialize UI widgets from server data
    updateSelectInput(
        session = session,
        inputId = "m2c1_xcol",
        choices = names(data$cluster_data)
    )
    updateSelectInput(
        session = session,
        inputId = "m2c1_ycol",
        choices = names(data$cluster_data),
        selected = names(data$cluster_data)[[2]]
    )
    
    # Combine the selected variables into a new data frame
    selectedData <- reactive({
        data$cluster_data[, c(input$m2c1_xcol, input$m2c1_ycol)]
    })
    
    clusters <- reactive({
        kmeans(selectedData(), input$m2c1_clusters)
    })
    
    output$m2c1_plot1 <- renderPlot({
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             col = clusters()$cluster,
             pch = 20, cex = 3)
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
}
