library(tidyverse)
library(GGally)
library(emmeans)
library(performance)

#open packages and make sure that they are ticked of in the packages icon in the bottom right 

lsmodel0 <- lm(formula = height ~ 1, data = darwin)

summary(lsmodel0)
# investigation of the linear models via the code (summary())
#after the summery we are going to find out sttistical measurmeents such as mean and se this code 

mean(darwin$height)
#use this code to compare the means

lsmodel1 <- lm(height ~ type, data=darwin)
# note that the following is identical
# lsmodel1 <- lm(height ~ 1 + type, data=darwin

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height))
#