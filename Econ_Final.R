#
# Title: Homework for Final
# Anthor: Cao Xin, Haochen
# Date: 18.6.8
# revised version





### 0.workspace and settings
setwd('G:/R/14.33')
library(ggplot2)
theme_set(theme_bw())
options(digits = 4)

### 1.import my function
source('C:/R/STAT_873/some_stat_functions.R')

### 2.read data
mydata <- read.csv('tspsimr7172.csv')
str(mydata)  #check my data
#summary(mydata)

# check a specific group use ggplot
using_data <- subset(mydata, reg_tsp == 1)
figure0 <- qplot(damtsp1, dimr7271, data = using_data,size = tbirth1, alpha = I(1/5),
                 xlab = 'change in tsps 72-71', 
                 ylab = 'changes in infant death 72-71')
figure0


### 3.explore data
# graph analysis ------------------------------------------------- ##
library(splines)
# plot variable of interest
figure1 <- qplot(damtsp1, dimr7271, data = mydata,
                 xlab = 'change in tsps 72-71', 
                 ylab = 'changes in infant death 72-71')
figure1

# plot with different size
figure2 <- qplot(damtsp1, dimr7271, data = mydata, weight = tbirth1,
                 size = tbirth1, alpha = I(1/2), 
                 xlab = 'change in tsps 72-71',
                 ylab = 'changes in infant death 72-71',
                 main = 'weighted total birth num')
figure2
# zoom in
last_plot() + xlim(-0.5, 0.5) + ylim(-15, 15)
last_plot() + xlim(-0.5, 0.25)


# check data distribution -----------------------------------------
# from plot
hist(x = mydata$damtsp1, breaks = 50, prob = TRUE,
     xlim = c(-1, 1))
lines(density(mydata$damtsp1))

qqnorm(y = mydata$damtsp1, main = 'check normality of y')
qqline(y = mydata$damtsp1)


# from Kolmogorov-Smirnov test
sample_var = Biased_Var(x = mydata$damtsp1)
sample_mean = mean(mydata$damtsp1)
normal_data = dnorm(100, mean = sample_mean, sd = sqrt(sample_var))
ks.test(x = mydata$damtsp1, y = normal_data)
# p_value is big, it's good for sake of test goodness of fit

# principle component analysis -------------------------------------
for_pca_data = na.omit(mydata)
pca(df = for_pca_data)

# regression analysis ----------------------------------------------- ##
attach(mydata)
# regression with variables of interest: 
# infant mortality against air quality 
model1 <- lm(dimr7271 ~ damtsp1, data = mydata)
summary(model1)

# adding other features
model2 <- lm(dimr7271 ~ damtsp1 + dwhite + dothr + dfemale
             + dedudad + dedumom + dmaried + dagemom + dpcare0
             + dpcare2 + dpcare3 + dpcare4, data = mydata)
summary(model2)

## Clean Air Act --------------------------------------------- ##
# tsp is a binary variable
# Air quality with tsp 
model3 <- lm(damtsp1 ~ reg_tsp, data = mydata)
summary(model3)
# adding other features
model3.2 <- lm(damtsp1 ~ reg_tsp + dwhite + dothr + dfemale
               + dedudad + dedumom + dmaried + dagemom + dpcare0
               + dpcare2 + dpcare3 + dpcare4, data = mydata)
summary(model3.2)


# infant death with tsp
model4 <- lm(dimr7271 ~ reg_tsp, data = mydata)
summary(model4)
# adding other features
model4.2 <- lm(dimr7271 ~ reg_tsp + dwhite + dothr + dfemale
               + dedudad + dedumom + dmaried + dagemom + dpcare0
               + dpcare2 + dpcare3 + dpcare4, data = mydata)
summary(model4.2)


## instrumental variable 
library('AER')
library('systemfit')

model5 <- ivreg(dimr7271 ~ damtsp1 | reg_tsp, data = mydata)
summary(model5)
# adding other features
model5.2 <- ivreg(dimr7271 ~ damtsp1 + dwhite + dothr + dfemale
                  + dedudad + dedumom + dmaried + dagemom + dpcare0
                  + dpcare2 + dpcare3 + dpcare4 | dwhite + dothr + dfemale
                  + dedudad + dedumom + dmaried + dagemom + dpcare0
                  + dpcare2 + dpcare3 + dpcare4 + reg_tsp, data = mydata)
summary(model5.2)


### 3. end
detach(mydata)

# More Remarks -------------------------------------------------
# Conclusion:  no evidence  show significant effects of 
# air quality increasing infant mortality.


# Reference: Chay, Kenneth, and Michael Greenstone. (2003). 
# ¡°Air Quality, Infant Mortality and the Clean Air Act of 1970.¡± 


