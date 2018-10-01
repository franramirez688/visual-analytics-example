# Tufte Charts 

# Connect to the libaries
library(ggthemes)
library(ggplot2)
library(ggExtra)
library(psych)
library(reshape2)

#0. Connect external data 
profit_companies <- read.csv("https://gist.githubusercontent.com/franramirez688/4e88f438da89fff27677f1b19b937636/raw/810284a689b5da1c5919b9e1010557639654f467/the%2520Most%2520Profitable%2520Companies%2520Make%2520Per%2520Second")
head(profit_companies)
summary(profit_companies)

# Normalizing column names and data
profit_companies["X2016.Net.Income"] = profit_companies["X2016.Net.Income"] / 10e+9
colnames(profit_companies) <- c("comp", "ind", "netinc", "rank", "profit")

ggplot(profit_companies, aes(x=netinc, y=profit, color=ind)) +
  theme_tufte(base_size=10, ticks=F) +
  geom_bar(width=2, fill="white", stat = "identity") +
  labs(x = "2016 Net Income (billions)",  y = "Profit/second") +
  ggtitle("WHAT THE MOST PROFITABLE COMPANIES MAKE PER SECOND") +
  scale_colour_tableau() +
  facet_wrap(ind ~ comp)
