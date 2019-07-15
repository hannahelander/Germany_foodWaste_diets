
# generate Index-data-frame to modify Y for different scenarios
NrOfCountries <- 192
NrOfProducts <- 130
index <- data.frame(country = rep(countries$ISO, each=NrOfProducts),
                    product = rep(products$Item, NrOfCountries),
                    item_code = rep(products$Item.Code, NrOfCountries),
                    com_group = rep(products$Com.Group, NrOfCountries),
                    product_group = rep(products$Group, NrOfCountries))

write.csv2(waste, file = "data/index_data_frame.csv")     # write to file in data-folder! 
