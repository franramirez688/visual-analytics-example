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


#1. Apply the tufte theme
ggplot(profit_companies, aes(x=rank, y=profit)) + 
  geom_point(size=10, color="#990000") + geom_rangeframe(size=0.1) 

ggplot(profit_companies, aes(x=rank, y=profit), size = 0.1) + 
  theme_tufte(ticks = FALSE, base_size = 8) + 
  geom_point(size=10, color="#990000") + geom_rangeframe(size=0.1) 

ggplot(profit_companies, aes(x=netinc, y=profit, color=ind)) +
  theme_tufte(base_size=10, ticks=F) +
  geom_bar(width=2, fill="white", stat = "identity") +
  labs(x = "2016 Net Income (billions)",  y = "Profit/second") +
  #theme(axis.title=element_blank()) +
  #scale_y_continuous(breaks=seq(100, 1500, 250)) + 
  #geom_hline(yintercept=seq(100, 1500, 250), col="white", lwd=1)  +
  scale_colour_tableau() +
  facet_wrap(ind ~ comp)

#With the diamond dataset
ggplot(diamonds, aes(factor(cut),price)) + 
  theme_tufte(base_size = 7, ticks=F) +
  geom_tufteboxplot(outlier.colour="transparent", size=0.5, color= "#990000") + 
  theme(axis.title=element_blank()) +
  annotate("text", x = 25, y = 20, adj=1,  family="serif", label = c(""))

#3. Range-frame plot - showing lables only for the quantiles
ggplot(diamonds, aes(carat, price)) + 
  geom_point(size=0.02, alpha=0.09, color="#990000")  + 
  theme_tufte(base_size = 9, ticks=FALSE) +
  xlab("")+ 
  ylab("")+ 
  scale_x_continuous(breaks = round(as.vector(quantile(diamonds$carat)), digits = 1))+
  scale_y_continuous(breaks = round(as.vector(quantile(diamonds$price)), digits = 1))

#4. Dot-dash scatterplot 
ggplot(diamonds, aes(carat, price)) + 
  geom_point(size=0.02, alpha=0.09, color="#990000") + 
  geom_rug(size=0.03, alpha=0.08, color="#990000") + 
  theme_tufte(base_size = 5, ticks=F) + 
  xlab("") + 
  ylab("")

#5. Density scatterplot 
p <- ggplot(faithful, aes(waiting, eruptions)) + 
  geom_point() + 
  theme_tufte(ticks=F) +
  theme(axis.title=element_blank(), axis.text=element_blank())
ggMarginal(p, type = "density")

#With the diamond dataset
pp <- ggplot(diamonds, aes(carat, price)) + 
  geom_point(size=0.02, alpha=0.09, color="blue") + 
  theme_tufte(base_size = 5, ticks=F) + 
  theme(axis.title=element_blank(), axis.text=element_blank())
ggMarginal(pp, type = "density", fill="black", alpha=0.3, color='transparent')

#6. Box-plot scatterplot  
p <- ggplot(faithful, aes(waiting, eruptions)) + 
  geom_point() + 
  theme_tufte(ticks=F) +
  theme(axis.title=element_blank(), axis.text=element_blank())
ggMarginal(p, type = "boxplot", size=15, fill="transparent")

#With the diamond dataset
pp <- ggplot(diamonds, aes(carat, price)) + 
  geom_point(size=0.02, alpha=0.09, color="blue") + 
  theme_tufte(base_size = 5, ticks=F) + 
  theme(axis.title=element_blank(), axis.text=element_blank())
ggMarginal(pp, type = "boxplot", size=30, fill="blue")

#7. Histogram scatterplot 
p <- ggplot(faithful, aes(waiting, eruptions)) + 
  geom_point() + 
  theme_tufte(ticks=F)
ggMarginal(p, type = "histogram", fill="transparent")

#With the diamond dataset
pp <- ggplot(profit_companies, aes(netinc, profit)) + 
  geom_point(size=5, alpha=0.5, color="blue") + 
  theme_tufte(base_size = 5, ticks=F) + 
  theme(axis.title=element_blank(), axis.text=element_blank())
ggMarginal(pp, type = "histogram", size=20, fill="blue")

#8. Minimal line plot 
x <- 1967:1977
y <- c(0.5,1.8,4.6,5.3,5.3,5.7,5.4,5,5.5,6,5)
d <- data.frame(x, y)

ggplot(d, aes(x,y)) + 
  geom_line(size=0.5, color="blue") + 
  theme_tufte(ticks = FALSE, base_size = 8) +
  theme(axis.title=element_blank()) + 
  geom_hline(yintercept = c(4,6), lty=2, size=0.2) + 
  scale_y_continuous(breaks=seq(1, 6, 1), label=sprintf("$%s",seq(300,400,20))) + 
  scale_x_continuous(breaks=x,label=x) +
  annotate("text", x = c(1977,1977.2), y = c(1.5,5.5), adj=1,  family="serif",
           label = c("Per capita\nbudget expandures\nin constant dollars", "5%"), size=3)
