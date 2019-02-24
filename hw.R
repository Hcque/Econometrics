##ch 10 --------------------------
#2
setwd("G:/R/chap")
d <- read.table("BARIUM.RAW")
e <- read.csv("BARIUM.DES")
head(d)
e
str(d)
d$V19

#5
d <- read.table("EZANDERS.RAW")
e <- read.csv("EZANDERS.DES")
head(d)
e
str(d)
d$V4
timeTrend <- c(1:108)
dim(d)
d2 <- cbind(d, timeTrend)
d2
f <- lm(V13 ~ timeTrend + V15 + V16 + V17 + V18 + V19 + V20 
        + V21 + V22 + V23 + V24 + V25, data = d2)

#6
d <- read.table("FERYIL3.RAW")
e <- read.csv("FERYIL3.DES")
head(d)
e
str(d)

#7
d <- read.table("CONSUMP.RAW")
e <- read.csv("CONSUMP.DES")
f <- lm(log(V10) ~ log(V8), data = d)
f
summary(f)

#11
d <- read.table("TRAFFIC2.RAW")
e <- read.csv("TRAFFIC2.DES")
head(d)
e
f <- lm(V34 ~ V17 + V23 + V24 + V25 + V26 + V27 + V28 
        + V29 + V30 + V31 + V32 + V33, data = d)
f
summary(f)
f2 <- lm(V34 ~ V17, data = d)
anova(f2, f)

f3 <- lm(V34 ~ V17 + V23 + V24 + V25 + V26 + V27 + V28 
        + V29 + V30 + V31 + V32 + V33 + V22 + V19 + V20 + V21, data = d)
summary(f3)


## ch12 -----------------------------------------------------------
library(wooldridge)
setwd("G:/R/exer")
d <- read.table("FERTIL3.RAW")
e <- read.csv("FERTIL3.DES")
str(d)
head(d)
e
f <- glm(V13 ~ V14, data = d)
f
summary(f)

f2 <- lm(V13 ~ V14 + V15 + V16, data = d)


## 2 
d <- read.table("WAGEPRC.RAW")
e <- read.csv("WAGEPRC.DES")
str(d)
head(d)
e
f <- lm(V6 ~ V7 + V8 + V9 + V10 + V11 + V12 + V13
        + V14 + V15 + V16 + V17 + V18 + V19, data = d)
f
summary(f)

## 12
d <- read.table("NYSE.RAW")
e <- read.csv("NYSE.DES")
f <- lm(V2 ~ V3, data = d)
as.numeric(d$V2)
d$V2

## series correlation ---------------------------------------------- ##
setwd('G:/R/chap')

# load data
data1 <- read.table('PHILLIPS.RAW')
desc1 <- read.csv('PHILLIPS.DES')

str(data1)
head(data1)
desc1

# graph approach
library(ggplot2)

tim <- function(x) as.POSIXlt(x)$tim + 1948
figure1 <- qplot(V2, V3, data = data1, 
                 geom = c('point','path'),
                 #colour = tim(year),
                 xlab = 'unemployment',
                 ylab = 'inflation rate')
figure1


# regression
model1 <- lm(V3 ~ V2, data = data1)
summary(model1)

r <- model1$residuals
for i in range(1, 48):
  
  
  