module_03             <- new.env()
module_03$name        <- "Step-by-Step Guide"
module_03$id          <- "User"
module_03$icon        <- "info-circle"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Component 01
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module_03$comp_01 <- new.env()

module_03$comp_01$ui <- function(){
  
  fluidPage(
    includeHTML(("help.html")),
    hr() 
    
  )
  
}

module_03$comp_01$server <- function(input, output, session, data){
  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Component 02
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
