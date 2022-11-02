#####################
# HOUSE PRICE PROJECT
#####################

# Packages:
install.packages("GGally")
library("tidyverse")
library("GGally")

# Import data:
housePrice <- read.table("https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/house_sales.csv", header=T)

# Exploring data:
dim(housePrice)
str(housePrice)
head(housePrice)
summary(housePrice)
unique(housePrice$PropertyType)

# Pairs
pairs(housePrice[,c(2,8,9,11,12,13)])
ggpairs(housePrice[,c(2,8,9,11,12,13)])

# Price per size:

# Price per 