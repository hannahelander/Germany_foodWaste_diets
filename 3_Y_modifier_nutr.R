##### This script include nutrient calculations

items <- read.csv2("input/items_nutr.csv")

DGErec_diet <- read.csv2("data/DGE_rec.csv")
DGErec_diet <- DGErec_diet[,2]
SQ_diet <- read.csv2("data/SQ_diet")
SQ_diet <- SQ_diet[,2]


DGE_diet_pp <- DGErec_diet/population
SQ_diet_pp  <- SQ_diet/population

Y <- Y[, "DEU_Food"]

items <- read.csv2(file = "input/items_nutr.csv")

#### Create data-frame to store diets in terms of eaten food and consumed nutrients etc.
Diets_matrix <- data.frame(item = items$Item,
                           Kcal_eaten = rep(NA, nrow(items)), 
                           Prot_eaten = rep(NA, nrow(items)), 
                           Fats_eaten = rep(NA, nrow(items))
                               )
items$Kcal = as.numeric(items$Kcal)         # get wrorng numbers!
items$G_prot = as.numeric(items$G_prot)     # problem with decimal (",")
items$G_fat = as.numeric(items$G_fat)       # problem with decimal (",")

for (i in 1:nrow(items)) {
   Diets_matrix$Kcal_eaten[i] <- items$Kcal[i] * sum(Y_eaten[match(index$item_code, items$Item.Code[i])])
   Diets_matrix$Prot_eaten[i] <- items$G_prot[i] * sum(Y_eaten[match(index$item_code, items$Item.Code[i])])
   Diets_matrix$Fats_eaten[i] <- items$G_fat[i] * sum(Y_eaten[match(index$item_code, items$Item.Code[i])])
}
 


   
# Calculate total Kcal/person
Kcal_pp_day <- Y_eaten * as.numeric(items$Kcal) / population / 365

sum(Kcal_pp_day)

# use match!!! 

sum(Y_eaten[which(index$item_code == 2516)]) * as.numeric(items$Kcal[index$item_code ==  2516]) /population

Y_eaten[index$item_code == 2516] 


Kcal_pp_day <- Y_eaten[match(index$item_code, items$Item.Code)] * as.numeric(items$Kcal[match(index$item_code, items$Item.Code)]) /population /356

sum(Kcal_pp_day)
### Use something like a data-frame
SQ_nutr <- data.frame(cereals_potatoes = c(sum(SQ_diet[index$DGE_group == "Cereals and potatoes"]) / population, NA, 0.3), #order of columns must stay the same)
                              excluded      = c(0, NA, 0),
                              vegetables_legumes = c(sum(Y_eaten[index$DGE_group == "vegetables incl. legumes"]) / population, NA, 0.26),
                              fruits        = c(sum(Y_eaten[index$DGE_group == "Fruits"]) / population, NA, 0.17), 
                              sugar_honey   = c(sum(Y_eaten[index$DGE_group == "Sugar & honey"]) / population, NA, 0),
                              alcohol_sugar = c(sum(Y_eaten[index$DGE_group == "Alcohol and sugar"])/population, NA, 0),
                              veg_oils      = c(sum(Y_eaten[index$DGE_group == "Vegetable oils"]) / population, NA, 0.02),
                              milk          = c(0.069715, NA, 0.18),                                                      # data from NEOMIT
                              meat_egg_fish = c(sum(Y_eaten[index$DGE_group == "Meat, sausages, fish, eggs"]) / population, NA, 0.07),
                              row.names = c("SQ_capita", "SQ_percentage", "DGE_rec"))


# match