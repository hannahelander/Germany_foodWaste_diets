
# generate Index-data-frame to modify Y for different scenarios
NrOfCountries <- 192
NrOfProducts <- 130
index <- data.frame(country = rep(countries$ISO, each=NrOfProducts),
                    product = rep(products$Item, NrOfCountries),
                    product_group = rep(products$Group, NrOfCountries))
