##############################
# Final model script: Team ucd #
##############################

### Note: fill in the code below until the line.
### Make sure your model works as expected by running summary().

# Load any packages here
library(tidyverse)
library(stargazer)

setwd("C:/Users/lucas/Documents/GitHub/Houseprice-project")

# Load training data
train <- readRDS("data/train.rds")

# Data transformation
zip_group_pr <- train %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(AdjSalePrice), 
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))

train <- train %>%
  left_join(select(zip_group_pr, ZipCode, ZipGroup), by = "ZipCode")

# Model
mod1 <- lm(AdjSalePrice ~ SqFtTotLiving, data = train)
mod2 <- lm(AdjSalePrice ~ BldgGrade, data = train)
mod3 <- lm(AdjSalePrice ~ ZipGroup, data = train)
mod4 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + as.factor(ZipGroup) + SqFtTotLiving:ZipGroup, 
          data = train,
          na.action = na.omit)
stargazer(mod1, mod2, mod3, mod4)

stargazer(mod5, type = "text")
summary(mod)

# Plots
pairs(train[c(9,12,16,22,24)])
pairs(train[c(12,11,13,14,15)])
ggplot(train, aes(SqFtTotLiving, AdjSalePrice, group = ZipGroup)) +
  geom_point(aes(colour = ZipGroup)) +
  geom_smooth(method = "lm", aes(colour = ZipGroup))

########## do not fill in below the line ###########

# Load test data
test <- readRDS("data/test.rds")

# Transform test data
test <- test %>% 
  # I will copy/paste here the code you use above
  
# Run model on test data
test$prediction <- predict(mod, newdata = test)

# Calculate RMSE
test$residuals <- test$AdjSalePrice - test$prediction
(rmse <- sqrt(mean(test$residuals^2)))

# Calculate R^2
mean_y <- mean(test$AdjSalePrice)
tss <- sum((test$AdjSalePrice - mean_y)^2)
rss <- sum((test$AdjSalePrice - test$prediction)^2)
(r_sq <- 1 - (rss/tss))