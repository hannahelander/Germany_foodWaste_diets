#### Functions to calculate food waste related to German consumption 
library(tidyr)

# aggregate function: This is just a function! 
agg <- function(x){
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)}


path <- "C:/Hanna/Germany_foodWaste_diets/input/"

# load data
countries <- read.csv2(paste0(path,"fabio_countries.csv"))
waste_shares <- read.csv2(paste0(path,"sensitivity_analysis_FAO_shares.csv"))
waste_shares[is.na(waste_shares)] <- 0                            # set NA to 0
# waste_shares_meat <- read.csv2("sensitivity_analysis_FAO_shares.csv")
# waste_shares_meat[is.na(waste_shares_meat)] <- 0

L <- readRDS(paste0(path,"2013_L_mass.rds"))
Y <- readRDS(paste0(path,"2013_Y.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))
X <- readRDS(paste0(path,"2013_X.rds"))

# prepare FABIO extension for Biomass
e <- E$Biomass / X
e[!is.finite(e)] <- 0
MP <- e * L

# see Food waste script 
country <- 'DEU'                                 # for Germany 
NrOfCountries <- 192
Y_waste <- Y[,paste0(country,"_Food")] * rep(waste_shares$final_consumption[waste_shares$Region==as.character(countries$Group[countries$ISO==country])],NrOfCountries) / 100
sum(Y_waste) / sum(Y[,paste0(country,"_Food")])





#########################################################################################################
# create data frame for waste (for each country, for all products in each stage)
NrOfProducts <- 130
waste <- data.frame(ISO = rep(countries$ISO, each=NrOfProducts),                    # 
                    country = rep(countries$Country, each=NrOfProducts),
                    group = rep(countries$Group, each=NrOfProducts))
waste$country <- as.character(waste$country)
waste$group   <- as.character(waste$group)

waste$com_code  <- waste_shares$Com.Code[1:NrOfProducts]          # adds Com.Code
waste$com_name  <- waste_shares$FAO.Name[1:NrOfProducts]          # adds Product names
waste$com_group <- waste_shares$Com.Group[1:NrOfProducts]        # adds Name of Commodity group

waste$harvest_production <- waste_shares$harvest_production[match(paste0(waste$group,waste$com_code),paste0(waste_shares$Region,waste_shares$Com.Code))]
waste$storage_transport  <- waste_shares$storage_transport [match(paste0(waste$group,waste$com_code),paste0(waste_shares$Region,waste_shares$Com.Code))]
waste$processing         <- waste_shares$processing        [match(paste0(waste$group,waste$com_code),paste0(waste_shares$Region,waste_shares$Com.Code))]
waste$distribution       <- waste_shares$distribution      [match(paste0(waste$group,waste$com_code),paste0(waste_shares$Region,waste_shares$Com.Code))]
waste$final_consumption  <- waste_shares$final_consumption [match(paste0(waste$group,waste$com_code),paste0(waste_shares$Region,waste_shares$Com.Code))]


## from Food Waste Script:
# waste$group[waste$country %in% "Germany"] <- waste$country[waste$country %in% "Germany"] ##  change group to "Germany!, why??????


######################################
# Scenarios for Food waste reduction 
######################################
# 1) Halfing food waste in Households

Y_DEU_Food <- Y[,"DEU_Food"]
eaten <- Y_DEU_Food                      # Problem - products lost!  -> # eaten = Y_DEU_Food /(1 +"consumption waste")

# select waste$final_consumption for Germany
# Element-wise halfing the 




# Diet preparation 
# Use Y[, DEU_Food] as status Quo
# Status Quo Diet is: Y[,DEU_Food]/(1 +"consumption waste")


