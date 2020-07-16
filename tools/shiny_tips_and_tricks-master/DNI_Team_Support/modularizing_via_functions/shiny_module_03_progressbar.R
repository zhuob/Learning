module_03    <- new.env()
module_03$name <- "Progress Bar"
module_03$id   <- "progressbar"
module_03$icon <- "spinner"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Component 01
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module_03$comp_01 <- new.env()
module_03$comp_01$ui <- function(){
    actionButton(inputId = "m3c1_run_simulations", label = "Run Simulations (Progresss Update Example)")
}

module_03$comp_01$server <- function(input, output, session, data){
    
    shiny_update_progress_callback <- function(amount = 1, message = NULL, detail = NULL){
        shiny::incProgress(amount = amount, message = message, detail = detail)
    }
    
    observeEvent(input$m3c1_run_simulations,{
        
        simulation_names <- c("Sim 1", "Sim 2", "Sim 3", "Sim 4", "Sim 5")
        
        shiny::withProgress(
            min = 0, max = length(simulation_names), value = 0, message = "Running Simulations", expr = {
                run_simulations(
                    simulation_names   = simulation_names,
                    ui_progress_update = shiny_update_progress_callback
                )
            }
        )
    })
}
