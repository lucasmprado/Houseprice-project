# Packages:
install.packages("broom")
install.packages("stargazer")
library("tidyverse")
library("broom")
library("stargazer")

# Import train dataset:
dat <- readRDS("~/GitHub/Houseprice-project/Data/train.rds")

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

# 16 Nov 2022:
dat %>%
  group_by(ZipCode) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(as.factor(reorder(ZipCode, n)), n)) +
  geom_col() +
  coord_flip() +
  xlab("Zip Code")

zip_group <- dat %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(AdjSalePrice),
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))

dat <- dat %>%
  left_join(select(zip_group, ZipCode, ZipGroup), by = "ZipCode")

mod4 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + ZipGroup, data = dat)

stargazer(mod4, type = "text")

#Grouping by year built:
dat %>%
  group_by(YrBuilt) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(as.factor(reorder(YrBuilt, n)), n)) +
  geom_col() +
  coord_flip() +
  xlab("Year Built")

YrBuilt_group <- dat %>%
  group_by(YrBuilt) %>%
  summarise(med_price = median(AdjSalePrice),
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         YrBuiltGroup = ntile(cumul_count, 5))

dat <- dat %>%
  left_join(select(YrBuilt_group, YrBuilt, YrBuiltGroup), by = "YrBuilt")

mod5 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + ZipGroup + YrBuiltGroup, data = dat)

stargazer(mod4, mod5, type = "text")
