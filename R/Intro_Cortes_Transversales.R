#### DAG ####
#Next section based on 
#https://mixtape.scunning.com/dag.html?panelset=r-code#discrimination-and-collider-bias

library(tidyverse)
library(stargazer)

#tb has the data generating process (DGP)
tb <- tibble(
  female = ifelse(runif(10000)>=0.5,1,0),
  ability = rnorm(10000), 
  discrimination = female,
  occupation = 1 + 2*ability + 0*female - 2*discrimination + rnorm(10000),
  wage = 1 - 1*discrimination + 1*occupation + 2*ability + rnorm(10000) 
)
#Ability is a random draw from the standard normal distribution. Therefore it is
#independent of female preferences. 
# Occupations are increasing in unobserved ability but decreasing in 
#discrimination.
#Wages are decreasing in discrimination but increasing in higher-quality jobs 
#and higher ability. 
#Thus, we know that discrimination exists in this simulation because we are 
#hard-coding it that way with the negative coefficients both the occupation and 
#wage processes.

lm_1 <- lm(wage ~ female, tb) 
lm_2 <- lm(wage ~ female + occupation, tb)
lm_3 <- lm(wage ~ female + occupation + ability, tb)

stargazer(lm_1,lm_2,lm_3, type = "text", 
          column.labels = c("Biased Unconditional", #unconditional on occupation
                            "Biased", #biased because does not account for abilities
                            "Unbiased Conditional")) #the only one that detects the true discrimination (compare estimates with DGP)
