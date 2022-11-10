# Packages:
install.packages("broom")
install.packages("stargazer")
library("tidyverse")
library("broom")
library("stargazer")

# Import train dataset:
dat <- readRDS("data/train.rds")

# Explore data
dim(dat)
head(dat)
boxplot(AdjSalePrice ~ PropertyType, data=dat)

ggplot(dat, aes(SqFtTotLiving, AdjSalePrice, group = PropertyType)) +
  geom_point(alpha = 0.5, aes(colour = PropertyType)) +
  geom_smooth(method = "lm", aes(colour = PropertyType))

ggplot(dat, aes(SqFtTotLiving, AdjSalePrice, group = Bedrooms)) +
  geom_point(alpha = 0.5, aes(colour = Bedrooms)) +
  geom_smooth(method = "lm", aes(colour = Bedrooms))

# Models:
mod1 <- lm(AdjSalePrice ~ SqFtTotLiving, data=dat)
stargazer(mod1, title = "Adjusted Sale Price and Square Feet of Total Living Area")

mod2 <- lm(AdjSalePrice ~ SqFtTotLiving + PropertyType, data=dat)
dat_add <- augment(mod2)
stargazer(mod1, mod2, title = "Adjusted Sale Price and Square Feet of Total Living Area, Property Type")

mod3 <- lm(AdjSalePrice ~ SqFtTotLiving + Bedrooms, data=dat)
dat_add2 <- augment(mod3)
stargazer(mod1, mod2, mod3, title = "Adjusted Sale Price and Square Feet of Total Living Area, Property Types, and Bedrooms")

ggplot(dat, aes(SqFtTotLiving, AdjSalePrice, group = PropertyType)) +
  geom_point(alpha = 0.5, aes(colour = PropertyType)) +
  geom_smooth(method = "lm", aes(colour = PropertyType))
