#####################
# HOUSE PRICE PROJECT
#####################

# Packages:
install.packages("GGally")
library("tidyverse")
library("GGally")

# Import data:
dat <- read.table("https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/house_sales.csv", header=T)

# Save to file
saveRDS(dat, "housePrice.rds")

# Load in data
dat <- readRDS("housePrice.rds")

# Exploring data:
dim(housePrice)
str(housePrice)
head(housePrice)
summary(housePrice)
unique(housePrice$PropertyType)

# Transform date column:
dat <- dat %>%
  mutate(Ddate = as.Date(dat$DocumentDate))

# Price per size:
ggpairs(dat[,c(8,10,11,12)])

# Price per units:
ggpairs(dat[,c(8,9,13,14)])