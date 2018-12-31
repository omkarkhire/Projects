# Loading libraries
library("tidyverse")
library("miscset")
library("mice")

# Setting path
path <- "D:/Study/ML/Churn Prediction"
setwd(path)

# Loading data set
churn <- read.csv("Telco.csv")

#Dimension and names of column
dim_desc(churn)
names(churn)

# Glimpse of data
glimpse(churn)

# Senior Citizen categorized as int even though it represents Y/N, let's convert to Factor
churn <- churn %>%
  mutate_if(is.character, as.factor)
  churn$SeniorCitizen <- as.factor(churn$SeniorCitizen)
  glimpse(churn)
  
# Check for missing values by column
churn %>% map(~sum(is.na(.)))

# Let's impute the "Total charges" missing values
churn <- churn %>%
    mutate(TotalCharges = replace(TotalCharges, is.na(TotalCharges),median(TotalCharges,na.rm=T)))

# Let's look at distict values of each column
churn_tbl <- churn %>%
  select_if(is.factor)%>%
  summarize_all(n_distinct)

# Let's plot

# Gender
ggplot(churn) +
  geom_bar(aes(x=gender, fill=Churn), position="dodge")

churn %>%
  group_by(gender, Churn) %>%
  summarise(n=n())

# Senior Citizen
ggplot(churn) +
  geom_bar(aes(x=SeniorCitizen, fill=Churn), position="dodge")

churn %>%
  group_by(SeniorCitizen) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

churn %>%
  group_by(SeniorCitizen, Churn) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# Senior Citizens Box plot
ggplot(churn, aes(x = SeniorCitizen, y = TotalCharges)) +
  geom_boxplot()


# Logistic Regression
library(caret)
churn <- churn %>% select(-customerID)
set.seed(5)
inTrain <- createDataPartition(y=churn$Churn, p=0.75, list=FALSE)
train<-churn[inTrain,]
test<-churn[-inTrain,]

fit <- glm(Churn~., data=train, family=binomial)

# making prediction
fit_churn <- predict(fit,test,type="response")
head(fit_churn)

contrasts(churn$Churn)

glm.pred = rep("No", length(fit_churn))
glm.pred[fit_churn > 0.5] = "Yes"

confusionMatrix(glm.pred, test$Churn, positive = "Yes")

library(ROCR)
# need to create prediction object from ROCR
pr <- prediction(fit_churn, test$Churn)

# plotting ROC curve
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# AUC value
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc