##### This script include nutrient calculations for diets

items <- read.csv2("input/items_nutr.csv")

#DGErec_diet <- read.csv2("data/DGE_rec.csv")
#DGErec_diet <- DGErec_diet[,2]
#SQ_diet <- read.csv2("data/SQ_diet")
#SQ_diet <- SQ_diet[,2]

Y_eaten <- read.csv2(file = "data/SQ_diet.csv ")
Y_eaten <- Y_eaten[,2]

#DGE_diet_pp <- DGErec_diet/population
#SQ_diet_pp  <- SQ_diet/population

  
items <- read.csv2(file = "input/items_nutr.csv")

#### Create data-frame to store diets in terms of eaten food and consumed nutrients etc.
Diets_matrix <- data.frame(item = items$Item,
                           Kcal_eaten = rep(NA, nrow(items)), 
                           Prot_eaten = rep(NA, nrow(items)), 
                           Fats_eaten = rep(NA, nrow(items))
                               )
items$Kcal = as.numeric(items$Kcal)         # Problem: change the numbers!
#items$G_prot = as.numeric(items$G_prot)     # problem with decimal (",")
#items$G_fat = as.numeric(items$G_fat)       # problem with decimal (",")

for (i in 1:nrow(items)) {
   Diets_matrix$Kcal_eaten[i] <- items$Kcal[i] * sum(Y_eaten[match(index$item_code, items$Item.Code[i])])
   Diets_matrix$Prot_eaten[i] <- items$G_prot[i] * sum(Y_eaten[match(index$item_code, items$Item.Code[i])])
   Diets_matrix$Fats_eaten[i] <- items$G_fat[i] * sum(Y_eaten[match(index$item_code, items$Item.Code[i])])
}
 


# Calculate total Kcal/person
Kcal_pp_day <- sum(Diets_matrix$Kcal_eaten) / population / 365



##### Old trials ######
#sum(Y_eaten[which(index$item_code == 2516)]) * as.numeric(items$Kcal[index$item_code ==  2516]) /population

#Y_eaten[index$item_code == 2516] 

#Kcal_pp_day <- Y_eaten[match(index$item_code, items$Item.Code)] * as.numeric(items$Kcal[match(index$item_code, items$Item.Code)]) /population /356
