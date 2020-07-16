##########  this function is used to update the basic data base information
## get what is currently in the database
c1 <- get_s3_data()
nct <- c("NCT03043872", # CASPIAN
         "NCT03043872", 
         "NCT02763579", # IMPOWER133
         "NCT02763579", 
         "NCT01866319") # MK3475006

add_info_to_s3_bucket(var_name = "NCI Number", var_value = nct)
