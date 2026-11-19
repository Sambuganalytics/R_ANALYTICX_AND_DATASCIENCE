# loading libraries::
library(tidyverse)
library(gtsummary)
library(correlation)
library(ggplot2)
library(ggblanket)
insurance <- read.csv("insurance.csv")
# let's see some fast plot
head(insurance)
# let's run some two way anova 
insurance |> 
  ggplot(
    aes(smoker, charges)
  )+
  geom_boxplot(
    aes(fill = smoker),
    staplewidth = 0.3,
    whisker.linewidth = 0.7,
    median.linewidth = 0.5,
    width = 0.2
  )+
  theme_bw()
#-------------------------------------
# lots of customization right?
#-------------------------------------
modelx <- lm(formula = charges~ smoker + bmi, data = insurance)
summary(modelx)
# let's see it in gtsummary
#----------------------------------
tbl_regression(modelx)

#-----------------------------------
insurance |> 
  select(bmi,age,charges) |> 
  correlation(method = 'spearman')
#-------------------------------------
# ISL
library(ISLR2)
#------------------------------------
# let's dummy code sex and smoker status
df <- insurance |> 
  mutate(
    sex_bin = case_when(
      sex == "female" ~ 0,
      sex == "male" ~ 1),
    smoker_bin = case_when(
      smoker == "yes" ~1,
      smoker == "no"~ 0
      
    )
    
  )
# Let's now run a regression model: correlation matrix
corr <- df |> 
  select(
    where(is.numeric)
  ) |> correlation(method = "pearson")
# let's look at the significant ones and how coefficients changes
corr$r <- round(corr$r,3)

# From simple linear regression to multiple linear regression
ggplot(
  corr,
  aes(Parameter1,Parameter2,fill = r)
)+
  geom_tile()+
  geom_text(aes(label = r))+
  scale_fill_gradient(low = "white", high = "firebrick", limits = c(0, 1))+
  theme_bw()
#----------------------------------------------------------------------------------
# age correlates with bmi, and charges : smoker status correlates with charges
# so the possible source of co-linearity would be age and bmi:
# Let's try to regress charges on bmi and then on bmi and age and check the coefficients
mult_model <- lm(
  formula = charges~ age+bmi+smoker+sex,
  data = insurance
)
summary(mult_model)



