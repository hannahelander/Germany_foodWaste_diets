# -----------------------------------------
# Footprints
# -----------------------------------------
# Calculate Footprints along the supply chain. 

# prepare extension and define footprint

e <- E$Biomass / X                    # Footprint: Biomass
e[!is.finite(e)] <- 0

MP <- e * L


#########################################################
# Biomass footprints along the Supply Chain - Status Quo 
#########################################################

# Total footprint
FP_tot <- t(t(MP) * Y[,"DEU_Food"])           

# per capita footprint
population <- 82442336
FP_capita <- sum(FP_tot) / population             # gives ~3 tonnes





# For Plant-based products--------------------

waste <- read.csv2(file = "data/waste_data_frame.csv")

# create result matrix
# NrOfProducts <- 130
#FP__chain <- data.frame(Scenario = rep("SQ", NrOfProducts),
#                        products = rep(index$product, each = NrOfProducts),
#                        product_group = rep(index$product_group, each = NrOfProducts))


# Create a Y-matrix where all animal-products are 0 - Can we do it nicer!?
Y_plant <- Y[,"DEU_Food"] 
Y_plant[index$product_group %in% c("Livestock products", "Fish")] <- 0

FP_plant <- t(t(MP) * Y_plant)        # Total footprint of all plant-based products
FP_plant_capita <- sum(FP_plant) / population


# FP_plant <- sum(FP_plant)
# FP_plant <- sum(colSums(MP) * Y_plant) # 


# create output matrix
supply_chain_FP <- data.frame(Scenario = rep("SQ", 4),
                              chain_type = c("plant_based", "plant_based", "animal_based", "animal_based"),
                              flow = c("cont", "waste", "cont", "waste"))


# BIG CALCULATIONS--
FP_prod_waste <- sum(FP_plant * waste$harvest_production) / 100               # waste shares are given as percentage (8% = 0,08)
#FP_plant_tot<- sum(FP_plant)

supply_chain_FP$harvest_production <- c(sum(FP_plant), FP_prod_waste, NA, NA) # column has 4 rows # FAO production data reflect 'cont' flow

#FP_tmp2 <- sum(FP_tmp * waste$storage_transport) / 100               # waste shares are given as percentage (8% = 0,08)
#FP_cont <- sum(FP_tmp - FP_tmp2)

#supply_chain_FP$storage_transport <- c(FP_tmp2, FP_cont, NA, NA) # column has 4 rows



### Create function!?  ####
step.calculator <- function(waste_step, FP){                      # waste_step is column in waste, eg. waste$harvest_production?
  FP_waste <- FP * waste_step / 100
  FP_cont <- FP  - FP_waste              
  FP_step <- c(sum(FP_cont), sum(FP_waste), NA, NA)
  output <- list(FP_step, FP_cont)                                # gives one vector with 4 elements and one new footprint-matrix
  return(output)
}


Output_storage    <- step.calculator(waste$storage_transport, FP_plant)
supply_chain_FP$storage_transport  <- Output_storage[[1]]

Output_processing       <- step.calculator(waste$processing, Output_storage[[2]])
supply_chain_FP$processing         <- Output_processing[[1]]

Output_distribution     <- step.calculator(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution       <- Output_distribution[[1]]

Output_consumption  <- step.calculator(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption        <- Output_consumption[[1]]

  
### Create function!?  ####
#step.calculator <- function(waste_step, FP){                      # waste_step is column in waste, eg. waste$harvest_production?
#  FP_step <- c(sum(FP * waste_step),               
#               sum(FP - (FP * waste_step)), NA, NA)
#  FP_cont <- FP  - (FP* waste_step)                  
#  return(list(FP_step,FP_cont))
#}




# step_results <- listname[[1]]


supply_chain_FP$storage_transport  <- step.calculator(waste$harvest_production, FP_tmp)[[1]]

supply_chain_FP&processing         <- step.calculator(waste$processing)
supply_chain_FP&distribution       <- step.calculator(waste$distribution)
supply_chain_FP&final_consumption  <- step.calculator(waste$final_consumption)



# For animal based products ----------------------
Y_animal <- Y[,"DEU_Food"] 
Y_animal[index$country=="DEU" & (!index$product_group=="Livestock products" 
                                 | !index$product_group=="Milk" 
                                 | !index$product_group=="Eggs" 
                                 | !index$product_group=="Fish")] <- 0        # Set Y for all products that are not animal based


