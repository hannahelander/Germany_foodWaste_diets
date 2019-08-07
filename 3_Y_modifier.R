#############################
# Create different scenarios by changing consumer behaviour in relation to Diets 
# 1) the script take the resulting diet after having removed all waste streams
# 2) it then modifies the diet according to different scenarios (eg. DGE_rec), for this part, new categorization is needed
# 3) it then adds the food wastage in each step to be able to calculate the footprint including embedded wastestreams

######## 1) remove waste from consumption to estimate "real diets"
# read Y-matrices of eaten food (Germany):
Y_eaten_plants <- read.csv2(file = "data/eaten_food_plants.csv")[,2]
Y_eaten_lvst <- read.csv2(file = "data/eaten_food_lvst.csv")[,2]
Y_eaten <- Y_eaten_plants + Y_eaten_lvst
# load index!

# POPULATION 2013
population <- 80800000 # lack detail data with trustable source


############### Functions ###############
# FUNCTION
add.percentage <- function(diet){  # take a data frame as input and return it with a sum-column and a row for percentage
  diet$sum <- rowSums(diet)
  for (i in 1:length(diet)){
    diet[2,i] <- diet[1,i]/diet$sum[1]
  }
  diet$sum[2] <- sum(diet[2,1:length(diet)-1])
  return(diet)
}


# Function to group Yvector into DGE-groups: 
DGE.grouper <- function(Yvector){
  DGE_groups <- data.frame(cereals_potatoes = c(sum(Yvector[index$DGE_group == "Cereals and potatoes"]) / population, NA, 0.3), #order of columns must stay the same)
                           excluded      = c(0, NA, 0),
                           vegetables_legumes = c(sum(Yvector[index$DGE_group == "vegetables incl. legumes"]) / population, NA, 0.26),
                           fruits        = c(sum(Yvector[index$DGE_group == "Fruits"]) / population, NA, 0.17), 
                           sugar_honey   = c(sum(Yvector[index$DGE_group == "Sugar & honey"]) / population, NA, 0),
                           veg_oils       = c(sum(Yvector[index$DGE_group == "Vegetable oils"]) / population, NA, 0.02),
                           milk          = c(sum(Yvector[index$DGE_group == "Milk"]) / population, NA, 0.18),                                                      # data from NEOMIT
                           meat_egg_fish = c(sum(Yvector[index$DGE_group == "Meat, sausages, fish, eggs"]) / population, NA, 0.07),
                           row.names = c("SQ_capita", "SQ_percentage", "DGE_rec"))
  DGE_groups <- add.percentage(DGE_groups)
  
  return(DGE_groups)
}



# Function to add the consumer waste to diets (step 3):
add.consumer.waste <- function(Yeaten){
  Yreal <- Yeaten/(100-waste$final_consumption)*100
  Yreal <- Yreal/(100-waste$distribution)*100
  Yreal <- Yreal/(100-waste$processing)*100
  Yreal <- Yreal/(100-waste$storage_transport)*100
  return(Yreal)
}

######################################
### ADDING A NEW CATEGORIZATION

# DGE Recommendations (https://www.dge.de/fileadmin/public/doc/fm/dgeinfo/DGEinfo-06-2019-Vollwertige-Ernaehrung_aheadofprint.pdf)
# 6 groups (excluding drinks), with subcategories

# redefine some commodity groups in diet-groups:
fruits <- c(2611, 2612, 2613, 2614, 2615, 2616, 2617, 2618,  2619, 2620, 2625)                    # need to be adapted
vegetables <- c(2546, 2547,  2549, 2551, 2601, 2602, 2605,  677, 2640,  2641,  2642, 2645)        # need to be adapted

# Expand index matrix and reallocate products to diet-group: 
index$diet_group <- index$com_group
levels(index$diet_group) <- c(levels(index$diet_group), "Cereals and potatoes", "Vegetables, pulses, spices", "Fruits", "Oil crops and nuts")

# Diet-group 1: Cereals and potatoes
index$diet_group[index$diet_group == "Cereals"] <- "Cereals and potatoes"
index$diet_group[index$com_group == "Roots and tubers"] <- "Cereals and potatoes"

# Diet-group 2: Vegetables, roots and legumes
index$diet_group[index$diet_group == "Vegetables, fruit, nuts, pulses, spices"] <- "Vegetables, pulses, spices"
index$diet_group[index$item_code %in% vegetables] <- "Vegetables, pulses, spices"
index$diet_group[index$item_code == "Roots and tubers"] <- "Vegetables, pulses, spices"

# Diet group 3: Fruits and nuts
index$diet_group[index$item_code %in% fruits] <- "Fruits"
index$diet_group[index$com_group == "Oil crops"] <- "Oil crops and nuts"
index$diet_group[index$product == 	"Nuts and products"] <- "Oil crops and nuts"

# Diet group: Meat and animal-based products
index$diet_group[index$com_group == "Animal fats"] <- "Meat"


#unique(index$diet_group)
#unique(products$Com.Group)
#length(unique(index$diet_group))
#################

# Create data-frame for scenarios, splitted in diet-groups (and sub-groups)
Eaten <- data.frame(cereals_potatoes = c(sum(Y_eaten[index$diet_group == "Cereals and potatoes"]) / population, NA), 
                    vegetables = c(sum(Y_eaten[index$diet_group == "Vegetables, pulses, spices"]) / population, NA),
                    fruits     = c(sum(Y_eaten[index$diet_group == "Fruits"]) / population, NA),
                    oil_crops_nuts = c(sum(Y_eaten[index$diet_group == "Oil crops and nuts"]) / population, NA),
                    veg_oils   = c(sum(Y_eaten[index$diet_group == "Vegetable oils"]) / population, NA),
                    Oil_cakes  = c(sum(Y_eaten[index$diet_group == "Oil cakes"]) / population, NA),
                    fibre_crops = c(sum(Y_eaten[index$diet_group == "Fibre crops"]) / population, NA),
                    sugar      = c(sum(Y_eaten[index$diet_group == "Sugar, sweeteners"]) / population, NA),
                    sugar_crops = c(sum(Y_eaten[index$diet_group == "Sugar crops"]) / population, NA),
                    alcohol    = c(sum(Y_eaten[index$diet_group == "Alcohol"]) / population, NA),
                    coffee_tea = c(sum(Y_eaten[index$diet_group == "Coffee, tea, cocoa"]) / population, NA),
                    meat       = c(sum(Y_eaten[index$diet_group == "Meat"]) / population, NA),
                    milk       = c(sum(Y_eaten[index$diet_group == "Milk"]) / population, NA),
                    eggs       = c(sum(Y_eaten[index$diet_group == "Eggs"]) / population, NA),
                    fish       = c(sum(Y_eaten[index$diet_group == "Fish"]) / population, NA), 
                    live_animals = c(sum(Y_eaten[index$diet_group == "Live animals"]) / population, NA), 
                    fodder = c(sum(Y_eaten[index$diet_group == "Fodder crops, grazing"]) / population, NA),
                    honey = c(sum(Y_eaten[index$diet_group == "Honey"]) / population, NA), 
                    row.names = c("SQ_capita", "SQ_percentage"))
# ignoring 'Wood', 'Hides, skines, wool', 'Tobacco, rubber', 'Ethanol'


# MODIFYING DATA TO SUIT THE PURPOSE 
Diets = Eaten[, which(!Eaten[1,] == 0)]          #removing categories == 0
Diets$milk[1] <- 0.069715                        # Replacing Milk-equivalents in tonnes of milk products (source: NEMONIT)

Diets$fodder[1] <- 0 # OBS!! There are some very small negative values for fodder, that we needt to set to 0


# DIETS EXCLUDING DRINKS:
#alcohol <- Diets$alcohol[1]
#coffee_tea <- Diets$coffee_tea[1]
#Diets_food = Diets[, which(!Diets[1,] == alcohol & !Diets[1,] == coffee_tea)]

#Diets <- add.percentage(Diets)
#Diets_food <- add.percentage(Diets_food)


######## CHECK OIL CROPS AND NUTS


########### Scenario 1 - DGE recommended Diet ##########
# ADDING ANOTHER CATEGORIZATION

# Create an additional category based on DGE's 6 groups:
index$DGE_group <- index$diet_group
levels(index$DGE_group) <- c(levels(index$DGE_group), "Meat, sausages, fish, eggs", "Sugar & honey", "vegetables incl. legumes", "excluded")
index$DGE_group[index$DGE_group == "Eggs"] <- "Meat, sausages, fish, eggs"
index$DGE_group[index$DGE_group == "Fish"] <- "Meat, sausages, fish, eggs"
index$DGE_group[index$DGE_group == "Meat"] <- "Meat, sausages, fish, eggs"
index$DGE_group[index$DGE_group == "Sugar, sweeteners"] <- "Sugar & honey"
index$DGE_group[index$DGE_group == "Honey"] <- "Sugar & honey"
index$DGE_group[index$DGE_group == "Oil crops and nuts"] <- "vegetables incl. legumes"
index$DGE_group[index$DGE_group == "Vegetables, pulses, spices"] <- "vegetables incl. legumes"
index$DGE_group[!index$DGE_group %in% c("Meat, sausages, fish, eggs", "Sugar & honey", "vegetables incl. legumes", "Cereals and potatoes", 
                                        "Vegetable oils", "Milk", "Fruits")]  <- "excluded"



# Create Diet representation in DGE_groups and adding DGE recommendations in ratio (excluding alcohol, coffee, tea, cacao): 
Diets_DGEgroups <- data.frame(cereals_potatoes = c(sum(Y_eaten[index$DGE_group == "Cereals and potatoes"]) / population, NA, 0.3), #order of columns must stay the same)
                              excluded      = c(0, NA, 0),
                              vegetables_legumes = c(sum(Y_eaten[index$DGE_group == "vegetables incl. legumes"]) / population, NA, 0.26),
                              fruits        = c(sum(Y_eaten[index$DGE_group == "Fruits"]) / population, NA, 0.17), 
                              sugar_honey   = c(sum(Y_eaten[index$DGE_group == "Sugar & honey"]) / population, NA, 0),
                              veg_oils       = c(sum(Y_eaten[index$DGE_group == "Vegetable oils"]) / population, NA, 0.02),
                              milk          = c(0.069715, NA, 0.18),                                                      # data from NEOMIT
                              meat_egg_fish = c(sum(Y_eaten[index$DGE_group == "Meat, sausages, fish, eggs"]) / population, NA, 0.07),
                              row.names = c("SQ_capita", "SQ_percentage", "DGE_rec"))

Diets_DGEgroups <- add.percentage(Diets_DGEgroups)# add percentage
Diets_DGEgroups <- data.frame(t(Diets_DGEgroups))[1:8,]
Diets_DGEgroups$DGE_group <- as.character(unique(index$DGE_group))


###### NEW Y-MATRIX ############## 
# create new Y-matrix for DGE recommendations:
Y_DGE_rec <- Y_eaten / Diets_DGEgroups$SQ_percentage[match(index$DGE_group,Diets_DGEgroups$DGE_group)] * 
  Diets_DGEgroups$DGE_rec[match(index$DGE_group,Diets_DGEgroups$DGE_group)]                          
Y_DGE_rec[!is.finite(Y_DGE_rec)] <- 0

sum(Y_DGE_rec)
sum(Y_eaten)

###### Add food waste for each step (create the corresponding hypotetical Y-vector)
Y_DGE <- add.consumer.waste(Y_DGE_rec)
sum(Y_DGE)            #shows that sum Y_DGE is slightly smaller than Y SQ (73 resp. 79 thousand) -> realistic
sum(Y[,"DEU_Food"])

write.csv2(Y_DGE, file = "data/Y_DGE.csv")

#########

# Function that recalculates the amounts of each product according to DGE_rec in % (The total amount of kg remains the same)#
#diet.converter <- function(Yvector){
#  for (i in 1:length(DGE_table)){
#    Yvector[which(index$DGE_group == DGE_table[1,i])] <- Yvector[which(index$DGE_group == DGE_table[1,i])] /    # takes the existing value, divides it with existing % of product group, multiplies it with desired percentage
#      (DGE_table["SQ_percentage", i] * DGE_table["DGE_rec",i])
#  }
#  return(Yvector)
#}

