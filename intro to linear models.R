library(tidyverse)
library(GGally)
library(emmeans)
library(performance)
library(broom.helpers)

#open packages and make sure that they are ticked of in the packages icon in the bottom right 

lsmodel0 <- lm(formula = height ~ 1, data = darwin)

summary(lsmodel0)
# investigation of the linear models via the code (summary())
#after the summery we are going to find out statistical measurements such as mean and see this code 

mean(darwin$height)
#use this code to compare the means

lsmodel1 <- lm(height ~ type, data=darwin)
# note that the following is identical
# lsmodel1 <- lm(height ~ 1 + type, data=darwin

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height))
summary(lsmodel1)

#the following code is used for the information information from this model to superimpose the calculated means onto a plot.
darwin %>% 
  ggplot(aes(x=type, 
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()

# no we use confidence intervals after the standard error of difference - our standard errors values have been estimated and now we use confidence to create p-values to understand the co-efficient

GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.95)
#we use these functions to  produces a graph of the estimated mean difference with an approx 95% CI
# We can also include this argument in the tidy() function if we wish to by using the code stated below 

broom::tidy(lsmodel1, conf.int=T, conf.level=0.99)

#finding the treatment mean and standard error - to calculate other" mean and SE then we can get R to do this  by using te following linw of code 
darwin %>% 
  mutate(type=factor(type)) %>% 
  mutate(type=fct_relevel(type, c("Self", "Cross"))) %>% 
  lm(height~type, data=.) %>% 
  broom::tidy()
#intro to packages emmeans also acts s a similar things
means <- emmeans::emmeans(lsmodel1, specs = ~ type)

means

# emmeans also gives us a handy summary to include in data visuals that combine raw data and statistical inferences.

means %>% 
  as_tibble() %>% 
  ggplot(aes(x=type, 
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL))
#assumption check means : it is important to check the assumptions of the model are adequately met so that we know whether our analysis can be trusted. In this first part we are going to check two of our assumptions:
  
#that the residual/unexplained variance in our data is approximately normally distributed.
#that the residual/unexplained variance is approximately equal between our groups

# residual differences analyse fitted values produced by the model against variable.

performance::check_model(lsmodel1)
#the code above is there to check the assumptions of the linear modules by making multiple graphs. 

#types of visual representation that best convey the two variables 
performance::check_model(lsmodel1, check=c("normality","qq"))
# normal distributions rearrange a graph to a specific visual representation that need to be plotted between the 2 variables
plot(lsmodel1, which=c(2,2))

#explanation of Quantile-Quantile (QQ) plot -compares sample distribution and theoretical distribution - distributes your data on the y-axis,
#a theoretical normal distribution on the x-axis. If the residuals follow a normal distribution, they should meet to produce a perfect diagonal line across the plot.

#graph of Equal variance
performance::check_model(lsmodel1, check="homogeneity")
plot(lsmodel1, which=c(1,3))

#outliers
performance::check_model(lsmodel1, check="outliers")
plot(lsmodel1, which=c(4,4))

darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=20.192, yend=20.192-2.617), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)

