#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  ui_progress_update should be a 2-arg function that
#      amount == integer arg. amount to increment progress by, default should be 1
#      message == string. optional, default null.
#      detail  == string. optional, default null.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
run_simulations <- function(simulation_names, ui_progress_update = NULL){
    
    for (sim_name in simulation_names) {
        if (!is.null(ui_progress_update))  {
            ui_progress_update( amount = 1, message = "Running Simulations", detail = sim_name)
        } 
        # Fake time to run simulation
        #Execute sim named sim_name
        Sys.sleep(1.5)
    }
    
}