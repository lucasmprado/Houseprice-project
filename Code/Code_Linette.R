## Clear global environment, load packages, set working directory and import data.
rm(list=ls())
library(tidyverse) 
setwd("/Users/linettelim/Documents/GitHub/StatsI_Fall2022")
data <- read.table ("https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/house_sales.csv")

summary(data)
dim(data)
head(data)
str(data)

data <- data %>%
  mutate(Ddate = as.Date(data$DocumentDate))

#save to file
saveRDS(data, "\relative_path\file.rds")

#load in data
data <- readRDS("\relative_path\file.rds")

pairs(data[,c(2,8,9,11,12,13)])

install.packages("GGally")
library("GGally")

ggpairs(data[,c(2,8,9,11,12,13)])
