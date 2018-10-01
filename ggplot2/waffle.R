#Waffle chart

#1. Install all packages
library(ggplot2)
library(waffle)

#0. Connect external data
profit_companies <- read.csv("https://gist.githubusercontent.com/franramirez688/4e88f438da89fff27677f1b19b937636/raw/810284a689b5da1c5919b9e1010557639654f467/the%2520Most%2520Profitable%2520Companies%2520Make%2520Per%2520Second")
head(profit_companies)
summary(profit_companies)

# Normalizing column names and data
profit_companies["X2016.Net.Income"] = profit_companies["X2016.Net.Income"] / 10e+9
colnames(profit_companies) <- c("comp", "ind", "netinc", "rank", "profit")



Oil_df <- profit_companies[profit_companies$ind == 'Oil/Gas/Cars',]
Telecommunications_df <- profit_companies[profit_companies$ind == 'Telecommunications/Mass Media',]
Retail_df <- profit_companies[profit_companies$ind == 'Retail/Consumer Goods',]
Tobacco_df <- profit_companies[profit_companies$ind == 'Tobacco',]
Medical_df <- profit_companies[profit_companies$ind == 'Medical/Pharmaceutical',]
Banking_df <- profit_companies[profit_companies$ind == 'Banking/Finance',]
Technology_df <- profit_companies[profit_companies$ind == 'Technology/Internet',]


Oil_c <- Oil_df$profit
names(Oil_c) <- Oil_df$comp
Telecommunications_c <- Telecommunications_df$profit
names(Telecommunications_c) <- Telecommunications_df$comp
Retail_c <- Retail_df$profit
names(Retail_c) <- Retail_df$comp
Tobacco_c <- Tobacco_df$profit
names(Tobacco_c) <- Tobacco_df$comp
Medical_c <- Medical_df$profit
names(Medical_c) <- Medical_df$comp
Banking_c <- Banking_df$profit
names(Banking_c) <- Banking_df$comp
Technology_c <- Technology_df$profit
names(Technology_c) <- Technology_df$comp


waffle(
  Oil_c / 5, rows = 5, size = 0.2,
  title = "Oil/Gas/Cars"
) -> A_
waffle(
  Telecommunications_c / 5, rows =10, size = 0.5,
  title = "Telecommunications/Mass Media"
) -> B_
waffle(
  Retail_c / 5, rows = 5, size = 0.2,
  title = "Retail/Consumer Goods"
) -> C_
waffle(
  Tobacco_c / 5, rows = 5, size = 0.2,
  title = "Tobacco"
) -> D_
waffle(
  Medical_c / 5, rows = 5, size = 0.2,
  title = "Medical/Pharmaceutical"
) -> E_
waffle(
  Banking_c / 5, rows = 10, size = 0.5,
  title = "Banking/Finance"
) -> F_
waffle(
  Technology_c / 5, rows = 10, size = 0.5,
  colors = c('#f7fcfd','#e5f5f9','#ccece6','#99d8c9','#66c2a4','#41ae76','#238b45','#006d2c','#00441b', '#f7fcfd'),
  title = "Technology/Internet"
) -> G_

iron(A_, B_, C_, D_, E_, F_, G_)
