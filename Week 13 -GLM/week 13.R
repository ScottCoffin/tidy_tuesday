##Week 13
#required pacakges
library(here)
library(tidyverse)
library(glmmTMB)
library(stats)
library(ggplot2) #plotting
library(tidyverse) #data tidying power
library(gt) #tables
library(MASS) #glm.nb for negative binomial models
library(glmmTMB) #Salamanders dataset and lots of families
library(lme4) #pseudo-R2 and and mixed model functionality
library(MuMIn) #dredge function for comparing models, AIC, marginal R^2 for mixed models
library(sjmisc) #pseudo R^2 - but I think gets loaded as a dependency
library(DHARMa) #model diagnostics
library(effects) #what do my marginal effects look like?
library(performance) #binomial model diagnostics
library(emmeans) #post hoc for categorical predictors

#Great Resource
#file:///C:/Users/SCoffin/Documents/OFFLINE/DATA/R/Training/Tidy%20Tuesday/Week%2013%-GLM/GLM_inR_Tutorial.html

data("Salamanders") #load data
sal <- Salamanders #rename for ease
str(sal) #look at data structure
hist(sal$count) #look at count data to understand distribution

#look at relationship between variables
sal %>% 
  dplyr::select(cover, Wtemp, count) %>% #select predictors and response variables
  pivot_longer(cols = cover:Wtemp,
               names_to = "predictor",
               values_to  = "value") %>% 
  ggplot(aes(x = value, y = count)) +
  geom_point() +#scatterplot
  geom_smooth(method = "lm", se = F) + #linear model with no error
  facet_wrap(~predictor, scales = "free") + #scales free allows to self-adjust axes limits
  theme_bw()

#Build a general linear model
model <- glm(count ~ cover + Wtemp, #response variable ~ predictor variable + predictor variable
             data = sal,
             family = "poisson") #default family for count data.Note that Gaussian is default for GLM, so have to call distribution.
summary(model) #examine model
#Estimate and Pr value are most important. Estimate is correlation, Pr is significance. Intercept usually not meaningful
#note that cover seems to be significant, but water temp does not. 

# Which model is best? Let's use AIC to tell us. AICc corrects for small sample size. It's already printed in summary (not corrected)

#AICc
MuMIn::AICc(model)

model <- glm(count ~ cover + Wtemp, #response variable ~ predictor variable + predictor variable
             data = sal,
             family = "poisson", #default family for count data.Note that Gaussian is default for GLM, so have to call distribution.
             na.action = "na.fail")  #necessary for dredge to work. na.fail causes an error message if your dataset contains NAs, instead of just dropping those rows... if you drop rows based on NAs in a certain column, then models including that column as a predictor will be fit to fewer rows of data than models without that column

#compare every factor against each other using dredge
MuMIn::dredge(model)
# auto orders by smallest AICc on top (best model). See that the best model includes intercept, cover and water temp
# Note that AIC is on log scale

#Choose the best model of the bunch
best_model <- glm(count ~ cover,
                  data = sal,
                  family = "poisson")
# Is this even a good model? DHARMa can tell us!
# DHARMa simulates datasets using model and gives p-value telling us if model truly fits wel
# kind of like qq-plot, but on steroids...
simulationOuput <- DHARMa::simulateResiduals(fittedModel = best_model,
                                             plot = T) #want to see it!
# red lines is not good- shows deviance from expected results

# Determine dispersion
testDispersion(simulationOuput)
# black bars are simulated. Red is from our model. Red should be in middle of distribution if it's correct
# Now we know that most simple model will not work!
# let's try other distributions like a negative binomial! Could add "zero inflation"