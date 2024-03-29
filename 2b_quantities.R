######################################################
# ---------------------------------------------------
# Quantities
# ----------------------------------------------------
# needed functions: step.calculator() and step.calculator2()

#### load data (if needed):
Y     <- readRDS(paste0(path,"2013_Y.rds"))
waste <- read.csv2(file = "data/waste_data_frame.csv")
index <- read.csv2(file = "data/index_data_frame.csv")


#########################################################
# Biomass footprints along the Supply Chain - Status Quo 
#########################################################

# Total amount of food consumed in Germany
Y_tot <- Y[,"DEU_Food"]           

# per capita footprint
population <- 82442336
Y_capita <- sum(Y_tot) / population             # gives ~1 ton

# create output matrix
supply_chain_Y <- data.frame(Scenario = rep("SQ", 4),
                              chain_type = c("plant_based", "plant_based", "animal_based", "animal_based"),
                              flow = c("cont", "waste", "cont", "waste"))

##############################################
# For Plant-based products--------------------
###############################################

# Create a Y-matrix where all animal-products are 0
Y_plant <- Y[,"DEU_Food"] 
Y_plant[index$product_group %in% c("Livestock products", "Fish")] <- 0


####################################################
### Calculate Footprints of flows and fill in Table: 

# Production
Y_prod_waste <- sum(Y_plant * waste$harvest_production) / 100               # waste shares are given as percentage (8% = 0,08)
supply_chain_Y$harvest_production <- c(sum(Y_plant), Y_prod_waste, NA, NA) # column has 4 rows # FAO production data reflect 'cont' flow

# Storage
Output_storage    <- step.calculator(waste$storage_transport, Y_plant)      # calculate
supply_chain_Y$storage_transport  <- Output_storage[[1]]                    # add values to table
rm(Y_plant)                                                                 # remove big data to save space

# Processing
Output_processing       <- step.calculator(waste$processing, Output_storage[[2]])
supply_chain_Y$processing         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator(waste$distribution, Output_processing[[2]])
supply_chain_Y$distribution       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator(waste$final_consumption, Output_distribution[[2]])
supply_chain_Y$Consumption        <- Output_consumption[[1]]
rm(Output_distribution)

###### save the Y matrix that represent only "eaten food": ########
eaten_food_plants <- Output_consumption[[2]]
write.csv2(eaten_food_plants, file = "data/eaten_food_plants.csv")     # write to file in data-folder! 
rm(Output_consumption)



##############################################################
# For animal based products (Livestock) ----------------------
##############################################################

# Create Y matrix that only includes animal-based products (Livestock)
Y_lvst <- Y[,"DEU_Food"] 
Y_lvst[index$product_group %in% c("Crop products", "Primary crops")] <- 0  # Set Y to 0 for all plant-based products

sum(Y_lvst)/population

####################################################
### Calculate Footprints of flows and fill in Table: 

# Production 
Y_prod_waste <- sum(Y_lvst * waste$harvest_production) / 100               # waste shares are given as percentage (8% = 0,08)
supply_chain_Y$harvest_production[3:4] <- c(sum(Y_lvst), Y_prod_waste) # column has 4 rows # FAO production data reflect 'cont' flow

# Storage and transport
Output_storage    <- step.calculator2(waste$storage_transport, Y_lvst)
supply_chain_Y$storage_transport[3:4]  <- Output_storage[[1]]
rm(Y_lvst)

# Processing
Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]])
supply_chain_Y$processing[3:4]         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]])
supply_chain_Y$distribution[3:4]       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]])
supply_chain_Y$Consumption[3:4]        <- Output_consumption[[1]]
rm(Output_distribution)


####### save the Y matrix that represent only "eaten food":########
eaten_food_lvst <- Output_consumption[[2]]
write.csv2(eaten_food_lvst, file = "data/eaten_food_lvst.csv")     # write to file in data-folder! 
rm(Output_consumption)




########### Write to File #############
write.csv2(supply_chain_Y, file = "output/supply_chain_tonnes.csv")     # write to file in output-folder! 

