######################################################
# ---------------------------------------------------
# Footprints
# ----------------------------------------------------
# Calculate Footprints along the supply chain. 

######## prepare extension and define footprint (if needed)
e <- E$Biomass / X                    # Footprint: Biomass
e[!is.finite(e)] <- 0
MP <- e * L                           # L needed


#### read waste data
waste <- read.csv2(file = "data/waste_data_frame.csv")


#### Function to calculate flows per step  #####
step.calculator <- function(waste_step, FP){                      # waste_step is a column in the data-frame "waste", eg. waste$harvest_production?
  FP_waste <- FP * waste_step / 100
  FP_cont <- FP  - FP_waste              
  FP_step <- c(sum(FP_cont), sum(FP_waste), NA, NA)
  output <- list(FP_step, FP_cont)                                # gives one vector with 4 elements & a new footprint-matrix (or Y-matrix)
  return(output)
}

step.calculator2 <- function(waste_step, FP){                      # waste_step is column in waste, eg. waste$harvest_production?
  FP_waste <- FP * waste_step / 100
  FP_cont <- FP  - FP_waste              
  FP_step <- c(sum(FP_cont), sum(FP_waste))
  output <- list(FP_step, FP_cont)                                # gives a vector with only 2 elements
  return(output)
}

#########################################################
# Biomass footprints along the Supply Chain - Status Quo 
#########################################################

# Total footprint
FP_tot <- t(t(MP) * Y[,"DEU_Food"])           

# per capita footprint
population <- 82442336
FP_capita <- sum(FP_tot) / population             # gives ~3 tonnes

# create output matrix
supply_chain_FP <- data.frame(Scenario = rep("SQ", 4),
                              chain_type = c("plant_based", "plant_based", "animal_based", "animal_based"),
                              flow = c("cont", "waste", "cont", "waste"))

##############################################
# For Plant-based products--------------------
###############################################

# Create a Y-matrix where all animal-products are 0
Y_plant <- Y[,"DEU_Food"] 
Y_plant[index$product_group %in% c("Livestock products", "Fish")] <- 0

FP_plant <- t(t(MP) * Y_plant)        # Total footprint of all plant-based products
#FP_plant_capita <- sum(FP_plant) / population

####################################################
### Calculate Footprints of flows and fill in Table: 

# Production
FP_prod_waste <- sum(FP_plant * waste$harvest_production) / 100               # waste shares are given as percentage (8% = 0,08)
supply_chain_FP$harvest_production <- c(sum(FP_plant), FP_prod_waste, NA, NA) # column has 4 rows # FAO production data reflect 'cont' flow

# Storage
Output_storage    <- step.calculator(waste$storage_transport, FP_plant)      # calculate
supply_chain_FP$storage_transport  <- Output_storage[[1]]                    # add values to table
rm(FP_plant)                                                                 # remove big data to save space

# Processing
Output_processing       <- step.calculator(waste$processing, Output_storage[[2]])
supply_chain_FP$processing         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)



##############################################################
# For animal based products (Livestock) ----------------------
##############################################################

# Create Y matrix that only includes animal-based products (Livestock)
Y_lvst <- Y[,"DEU_Food"] 
Y_lvst[index$product_group %in% c("Crop products", "Primary crops")] <- 0  # Set Y to 0 for all plant-based products

FP_lvst <- t(t(MP) * Y_lvst)        # Total footprint of all animal-based products
#FP_lvst_capita <- sum(FP_lvst) / population       # For control (FP per capta == FP_lvst_capita + FP_plant_capita)

####################################################
### Calculate Footprints of flows and fill in Table: 

# Production 
FP_prod_waste <- sum(FP_lvst * waste$harvest_production) / 100               # waste shares are given as percentage (8% = 0,08)
supply_chain_FP$harvest_production[3:4] <- c(sum(FP_lvst), FP_prod_waste) # column has 4 rows # FAO production data reflect 'cont' flow

# Storage and transport
Output_storage    <- step.calculator2(waste$storage_transport, FP_lvst)
supply_chain_FP$storage_transport[3:4]  <- Output_storage[[1]]
rm(FP_lvst)

# Processing
Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]])
supply_chain_FP$processing[3:4]         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution[3:4]       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption[3:4]        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)


########### Write to File #############
write.csv2(supply_chain_FP, file = "output/supply_chain_Biomass.csv")     # write to file in output-folder! 







########## notes ################

# FP_plant <- sum(colSums(MP) * Y_plant) # 

# create result matrix
# NrOfProducts <- 130
#FP__chain <- data.frame(Scenario = rep("SQ", NrOfProducts),
#                        products = rep(index$product, each = NrOfProducts),
#                        product_group = rep(index$product_group, each = NrOfProducts))


### Create function!?  ####
#step.calculator <- function(waste_step, FP){                      # waste_step is column in waste, eg. waste$harvest_production?
#  FP_step <- c(sum(FP * waste_step),               
#               sum(FP - (FP * waste_step)), NA, NA)
#  FP_cont <- FP  - (FP* waste_step)                  
#  return(list(FP_step,FP_cont))
#}

