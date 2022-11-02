house_dat <- read.delim("https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/house_sales.csv")
summary(house_dat)
?read.csv()

options(scipen = 999)

unique(house_dat$YrRenovated)
table(house_dat$Bedrooms)

library(tidyverse)
library(ggplot2)
# Explore bedroom variables
table(house_dat$Bedrooms)
house_dat %>% 
  filter(Bedrooms == 0)

# Explore property type
unique(house_dat$PropertyType)

# Explore BldgGrade
unique(house_dat$BldgGrade)



# Explore ym
house_dat$date_reformat <- paste(substr(house_dat$ym,1,4),"-",substr(house_dat$ym,6,7),sep = "")
house_dat$ym_reformat <- as.Date(house_dat$ym)
house_dat$yr_reformat <- substr(house_dat$ym,1,4)
typeof(house_dat$ym_reformat)
ym_house_dat_mth <- house_dat %>% 
  group_by(ym_reformat) %>% 
  summarise(month_ave_price = mean(AdjSalePrice))
ym_house_dat_mth_2 <- house_dat %>% 
  group_by(date_reformat) %>% 
  summarise(month_ave_price = mean(AdjSalePrice))
ym_house_dat_yr <- house_dat %>% 
  group_by(yr_reformat) %>% 
  summarise(month_ave_price = mean(AdjSalePrice))
ggplot(ym_house_dat_mth, aes(x = ym_reformat, y = month_ave_price)) + geom_point() + 
  xlab("Month and Year") + 
  ylab("Average month price")
ggplot(ym_house_dat_yr, aes(x = yr_reformat, y = month_ave_price)) + geom_point()

# Explore AdjSalePrice
house_dat_price <- house_dat %>% 
  filter(AdjSalePrice<600000)
ggplot(house_dat_price, aes(x = ym_reformat, y = AdjSalePrice)) + geom_point() + facet_wrap(~BldgGrade)

