# Loading Libraries
library(XLConnect)
library(tidyverse)
library(tableone)
library(MatchIt)

# Loading Data
setwd("D:/Study/ML/AV-Proj/Propensity model")
wb <- loadWorkbook("Book1.xlsx")
df <- readWorksheet(wb, sheet = "Sheet1", header = TRUE)

# Summary of data
glimpse(df)
head(df)

# Dividing data into Target and control
Target <- subset(df,Response==1)
Control <- subset(df,Response==0)

# Performance of campaign
model1 <- lm(Bought~Response+Age+Income+Product, data=df)
summary(model1)

#
effect<-model1$coefficients[2]
effect

# Finding propensity scores

pscores <- glm(Response~Age+Income+Product, data= df, family=binomial)
summary(pscores)

propensity_scores<-pscores
df$Pscores<- pscores$fitted.values

hist(df$Pscores[Response==1],main = "PScores of Response = 1")
hist(df$Pscores[Response==0],main = "PScores of Response = 0")

# Creating covariates
xvars <- c("Age","Income","Product")
table1<- CreateTableOne(vars=xvars,strata = "Response",data=df, test = FALSE)
print(table1,smd=TRUE)

# Exact matching
match1<-matchit(pscores, method="nearest", data=df, radio=1)
plot(match1, type="jitter")
plot(match1, type="hist")

summary(match1)

match1.data <- match.data(match1)
print(match1.data)

y_trt <- match1.data$Bought[match1.data$Response == 1]
y_con <- match1.data$Bought[match1.data$Response == 0]

diff<-y_trt-y_con

t.test(diff)