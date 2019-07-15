#############################
# Create different scenarios by changing consumer behaviour 
# 1) Diets
# 2) Food waste behaviour (in retail and households)


### 

# DGE Recommendations (https://www.dge.de/fileadmin/public/doc/fm/dgeinfo/DGEinfo-06-2019-Vollwertige-Ernaehrung_aheadofprint.pdf)
# 6 groups (excluding drinks), with subcategories

# redefine some commodity groups in diet-groups:
fruits <- c(2611, 2612, 2613, 2614, 2615, 2616, 2617, 2618,  2619, 2620, 2625)                    # need to be adapted
vegetables <- c(2546, 2547,  2549, 2551, 2601, 2602, 2605,  677, 2640,  2641,  2642, 2645)        # need to be adapted

# Expand index matrix and reallocate products to diet-group: 
index$diet_group <- index$com_group
levels(index$diet_group) <- c(levels(index$diet_group), "Cereals and potatoes", "Vegetables, pulses, spices", "Fruits", "Nuts")

# Diet-group 1: Cereals and potatoes
index$diet_group[index$diet_group == "Cereals"] <- "Cereals and potatoes"
index$diet_group[index$product == "Potatoes and products"] <- "Cereals and potatoes"

# Diet-group 2: Vegetables, roots and legumes
index$diet_group[index$diet_group == "Vegetables, fruit, nuts, pulses, spices"] <- "Vegetables, pulses, spices"
index$diet_group[index$item_code %in% vegetables] <- "Vegetables, pulses, spices"
index$diet_group[index$item_code == "Roots and tubers"] <- "Vegetables, pulses, spices"

# Diet group 3: Fruits and nuts
index$diet_group[index$item_code %in% fruits] <- "Fruits"
index$diet_group[index$product == 	"Nuts and products"] <- "Nuts"


unique(index$diet_group)


######## remove waste from consumption to estimate "real diets"
# read Y-matrices of eaten food (Germany):
Y_eaten_plants <- read.csv2(file = "data/eaten_food_plants.csv")
Y_eaten_lvst <- read.csv2(file = "data/eaten_food_lvst.csv")


# Create data-frame for scenarios, splitted in diet-groups (and sub-groups)
Diets <- data.frame(scenario   = c("SQ_capita", "SQ_percentage"), 
                     cereals_potatoes = c(sum(Y_eaten_plants[index$diet_group == "Cereals and potatoes"])/population, NA), 
                     vegetables = c(sum(Y_eaten_plants[index$diet_group == "Vegetables, pulses, spices"])/population, NA),
                     fruits     = c(sum(Y_eaten_plants[index$diet_group == "Fruits"])/population, NA),
                     nuts       = c(sum(Y_eaten_plants[index$diet_group == "Nuts"])/population, NA),
                     veg_oils   = c(sum(Y_eaten_plants[index$diet_group == "Vegetable oils"])/population, NA),
                     meat       = c(sum(Y_eaten_lvst[index$diet_group == "Meat"])/population, NA),
                     milk       = c(sum(Y_eaten_lvst[index$diet_group == "Milk"])/population, NA),
                     eggs       = c(sum(Y_eaten_lvst[index$diet_group == "Eggs"])/population, NA),
                     fish       = c(sum(Y_eaten_lvst[index$diet_group == "Fish"])/population, NA), 
                     animal_fat = c(sum(Y_eaten_lvst[index$diet_group == "Animal fats"])/population, NA))

tot_eaten <- rowsum(Diets[1]) # ???

# fill in % using tot_eaten






unique(products$Com.Group)
unique(index$diet_group)





# DGE recommended diet
# Do the neccessary changes to meet the proportins of different product groups recommened by the DGE
DGE_rec <- data.frame(food_group = c("fats_oils", "cereals_potatoes", "vegetable_salad", "fruit", "milk", "meat_fish_eggs"),
                      percent = c(2, 30, 26, 17, 18, 7))