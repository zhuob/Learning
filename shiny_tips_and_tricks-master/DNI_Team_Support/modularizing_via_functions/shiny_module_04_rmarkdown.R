module_04    <- new.env()
module_04$name <- "R Markdown"
module_04$id   <- "rmarkdown"
module_04$icon <- "spinner"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Component 01
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module_04$comp_01 <- new.env()
module_04$comp_01$ui <- function(){
    shiny::tags$a(id = "m4c1_download_markdown_report", class = "btn btn-default shiny-download-link text-left", style = "width: 235px; text-align: left;", href = "", target = "_blank", download = NA, icon("file-alt"), "Download HTML Report")
}

module_04$comp_01$server <- function(input, output, session, data){
    
    download_report_html <- function(file){
        shiny::withProgress(min = 0, max = 100, value = 50, message = "Rendering Markdown Report....", expr = {
            generate_html_deck(
                title   = sprintf("RMarkdown Example: %s",Sys.Date()), 
                outfile = file,
                markdown_params = list(
                    data = list(
                        cluster_data = data$cluster_data,
                        hist_data    = data$histdata,
                        
                        title_01     = paste0(strsplit(stringi::stri_rand_lipsum(1, FALSE)," ")[1:3][[1]][1:4], collapse = " "),
                        value_01     = runif(1, 100, 999),
                        
                        title_02     = paste0(strsplit(stringi::stri_rand_lipsum(1, FALSE)," ")[1:3][[1]][1:4], collapse = " "),
                        value_02     = runif(1, 100, 999),
                        
                        title_03     = paste0(strsplit(stringi::stri_rand_lipsum(1, FALSE)," ")[1:3][[1]][1:4], collapse = " "),
                        value_03     = runif(1, 100, 999),
                        
                        title_04     = paste0(strsplit(stringi::stri_rand_lipsum(1, FALSE)," ")[1:3][[1]][1:4], collapse = " "),
                        value_04     = runif(1, 100, 999)
                    )
                )
            )
        })
    }
    
    output$m4c1_download_markdown_report <- shiny::downloadHandler(
        filename = "Example_Markdown_Report.html",
        content = function(file) {
            download_report_html(file)
        }
    )
    
}
