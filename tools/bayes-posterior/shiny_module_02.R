module_02             <- new.env()
module_02$name        <- "User Manual"
module_02$id          <- "User"
module_02$icon        <- "info-circle"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Component 01
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module_02$comp_01 <- new.env()

module_02$comp_01$ui <- function(){
  
  fluidPage(
    includeHTML(("help.html")),
    hr() 
    
  )
  
}

module_02$comp_01$server <- function(input, output, session, data){
  
}