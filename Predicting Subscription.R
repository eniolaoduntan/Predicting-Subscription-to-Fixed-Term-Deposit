#Set working directory
getwd() 
setwd("/Users/user/Documents") 
install.packages("readxl") 
library(readxl) 
install.packages("psych") 
library(psych) 
install.packages("skimr") 
library(skimr)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2") 
library(ggplot2) 
install.packages("corrplot") 
library(corrplot) 
install.packages("tidyverse") 
library(tidyverse) 
install.packages("caret") 
library(caret) 
install.packages("lmtest") 
library(lmtest)
install.packages("gbm") 
library(gbm)
install.packages("tree")
library(tree)

install.packages("car") 
library(car)

install.packages("pROC") 
library(pROC)

pROC

#read in data
data <- read_excel("banksv.xlsx")

#perform descriptive statistics
#summarising data numerically summary(data)
skimr::skim(data)
sapply(data, class)
describe(data)
str(data)
head(data)
tail(data)
sum(is.na(data)) 
summary(data$age) 
summary(data$campaign) 
summary(data$pdays)
summary(data$month)
mean(data$pdays, na.rm = TRUE) 
min(data$euribor3m, na.rm = TRUE) 
max(data$euribor3m, na.rm = TRUE) 


data %>%
  select(education, subscribed) %>%
  group_by(education) %>%
  count(subscribed) %>%
  ungroup() %>%
  spread(key = subscribed, value = n) %>%
  mutate(total = no + yes,
         subscription_rate = round((yes/total)*100,2)) %>%
  select(education,total,subscription_rate) %>%
  arrange(desc(subscription_rate))








data %>%
  mutate(euribor3m = round(data$euribor3m)*100,1) %>%
  mutate(test = ifelse(euribor3m > 3.62,"More than 3.62","Less than 3.62")) %>%
  select(age, euribor3m, subscribed, test) %>%
  ggplot(aes(y = age, x = euribor3m, col = subscribed)) +
  geom_point(alpha = 0.6)+
  scale_y_log10()+
  scale_x_log10()+
  facet_wrap(~test, scales = "free")+
  labs(
    y = "euribor3m",
    x = "age",
    col = "Outcome"
  )


data %>% group_by(education) %>% count()
data %>% group_by(day_of_week) %>% count()
data %>% group_by(marital) %>% count()
data %>% group_by(poutcome) %>% count()
data %>% group_by(loan) %>% count()
data %>% group_by(housing) %>% count()
data %>% group_by(default) %>% count()



data %>% group_by(job) %>% count()
data %>% group_by(month) %>% count()
data %>% group_by(contact) %>% count()


round(prop.table(table(data$subscribed))*100,1)

round(prop.table(table(data$education))*100,1)
round(prop.table(table(data$marital))*100,1)
round(prop.table(table(data$poutcome))*100,1)
round(prop.table(table(data$loan))*100,1)
round(prop.table(table(data$housing))*100,1)
round(prop.table(table(data$default))*100,1)


#summarising visually
hist(data$age)
boxplot(data$age)

p_bar <- ggplot(data, aes(previous))
p_bar + geom_bar(color = "black",fill = "blue") + theme(text = element_text(size=20))

subscription_bar <- ggplot(data, aes(subscribed))
subscription_bar + geom_bar(color = "black",fill = "blue") + theme(text = element_text(size=20))

education_bar <- ggplot(data, aes(education))
education_bar + geom_bar(color = "black",fill = "purple") +  theme(text = element_text(size=20), axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1,size=10))

data$day_of_week <- factor(data$day_of_week,levels = c("mon", "tue","wed","thu","fri"))
day_bar <- ggplot(data, aes(day_of_week))
day_bar + geom_bar(color = "black",fill = "purple") +  theme(text = element_text(size=20), axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1,size=10))

camapaign_bar <- ggplot(data, aes(campaign))
camapaign_bar + geom_bar(color = "black",fill = "purple") +  theme(text = element_text(size=20), axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1,size=10))

loan_bar <- ggplot(data, aes(loan))
loan_bar + geom_bar(color = "black",fill = "purple") +  theme(text = element_text(size=20), axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1,size=10))

outcome_bar <- ggplot(data, aes(poutcome))
outcome_bar + geom_bar(color = "black",fill = "purple") +  theme(text = element_text(size=20), axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1,size=10))

mstatus_bar <- ggplot(data, aes(marital))
mstatus_bar + geom_bar(color = "black",fill = "purple") +  theme(text = element_text(size=20), axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1,size=10))

housing_bar <- ggplot(data, aes(housing))
housing_bar + geom_bar(color = "black",fill = "purple") +  theme(text = element_text(size=20), axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1,size=10))

default_bar <- ggplot(data, aes(default))
default_bar + geom_bar(color = "black",fill = "purple") +  theme(text = element_text(size=20), axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1,size=10))


#identifying data quality issues
#imputation errors and Recoding
data$default[data$default == "n"] <-"no"

data$education[data$education == "basic.4y"] <-"basic"
data$education[data$education == "basic.6y"] <-"basic"
data$education[data$education == "basic.9y"] <-"basic"

data$pdays[data$pdays == "999"] <-"no contact"


data$marital <- as.numeric(factor(data$marital,
                                  levels = c("unknown","single", "married", "divorced"), labels = c(0,1,2,3) ,ordered = TRUE))


data$loan <- as.numeric(factor(data$loan, 
                               levels = c("unknown","no", "yes"), labels = c(0,1,2) ,ordered = TRUE))


data$housing <- as.numeric(factor(data$housing, 
                                  levels = c("unknown","no", "yes"), labels = c(0,1,2) ,ordered = TRUE))



data$default <- as.numeric(factor(data$default, 
                                  levels = c("unknown","no", "yes"), labels = c(0,1,2) ,ordered = TRUE))



#outliers
#recode outliers in age with median

data$age[data$age > 99] <- 38
data$age[data$age < 18] <- 38

#Missing variables
data$month <- replace(data$month, is.na(data$month), 0)
data$pdays<- replace(data$pdays, is.na(data$pdays), 0)

#remove near zero variables that might affect prediction
nearzero.data <- nearZeroVar(data, saveMetrics = TRUE)
drop.cols <- rownames(nearzero.data)[nearzero.data$nzv == TRUE]
data <-data[,!names(data)%in% drop.cols] #drops pdays

data<-select(data, -duration)
data<-select(data, -ID)


table(data$subscribed)




data$subscribed[data$subscribed == "no"] <-0
data$subscribed[data$subscribed == "yes"] <-1

data$subscribed <- as.numeric(data$subscribed)



#Visualisation
#selected variables- euribor3m, previous, poutcome, nr.employed


#Additional variables- age, education, day_of_week, default

data$subscribed <- as.numeric(data$subscribed)

#Visualisations using ggplot
#Previous Contact and Subscription
ggplot(data=data)+
  geom_bar(mapping = aes(x=previous, y=subscribed), stat="summary", fun.y="mean")+
  labs(title = "Subscription by Previous Contact", x="Previous Contact", y="Subscription")

ggplot(data=data)+
  geom_bar(mapping = aes(x=previous, y=subscribed), stat="summary", fun.y="mean")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "Subscription by Outcome of Previous Contact", x="Contact Outcome", y="Subscription")

ggplot(data=data)+
  mutate(employee_groups = cut(data$nr.employed, breaks = seq(4960,5230 , 100), include.lowest = T)) %>%
  geom_bar(mapping = aes(x=nr.employed, y=subscribed), stat="summary", fun.y="mean")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "Subscription by Outcome of Previous Contact", x="Contact Outcome", y="Subscription")



#Campaign Outcome and Subscription
ggplot(data=data)+
  geom_bar(mapping = aes(x=subscribed, y=poutcome), stat="summary", fun.y="mean",color = "black",fill = subscribed)+
  labs(title = "Subscription by Outcome of Previous Campaign", x="Previous Campaign Outcome", y="Subscription")


data %>%
  select(poutcome, subscribed) %>%
  group_by(poutcome) %>%
  count(subscribed) %>%
  ungroup() %>%
  mutate(education = str_to_title(poutcome)) %>%
  spread(key= subscribed, value = n) %>%
  mutate(positivity_ratio = round((yes/(yes+no)),3)) %>%
  mutate(poutcome = reorder(poutcome, positivity_ratio)) %>%
  arrange(desc(positivity_ratio)) %>%
  ggplot(aes(positivity_ratio,education, col = poutcome))+
  geom_segment(aes(xend = 0, x = positivity_ratio, y = poutcome, yend = poutcome))+
  labs(
    title = "Campaign Outcome and Subscription Rates",
    y = NULL,
    x = NULL
  )+
  scale_x_continuous(labels = scales::percent_format())









#age and subscription

data %>%
  mutate(age_groups = cut(age, breaks = seq(18, 99, 10), include.lowest = T)) %>%
  ggplot(aes(age_groups, fill = subscribed))+
  geom_bar(stat = "count", position = "fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "Relationship between Age and Fixed Term Deposit Subscription",
       y = "Rate",
       x = "Age Group",
       fill = "Outcome")

#education level and subscription rates

data %>%
  select(education, subscribed) %>%
  group_by(education) %>%
  count(subscribed) %>%
  ungroup() %>%
  mutate(education = str_to_title(education)) %>%
  spread(key= subscribed, value = n) %>%
  mutate(positivity_ratio = round((yes/(yes+no)),3)) %>%
  mutate(education = reorder(education, positivity_ratio)) %>%
  arrange(desc(positivity_ratio)) %>%
  ggplot(aes(positivity_ratio,education, col = education))+
  geom_point()+
  geom_segment(aes(xend = 0, x = positivity_ratio, y = education, yend = education))+
  labs(
    title = "Education and Subscription Rates",
    y = NULL,
    x = NULL
  )+
  scale_x_continuous(labels = scales::percent_format())

#day of week

data %>%
  ggplot(aes(day_of_week, fill = subscribed))+
  geom_bar(stat = "count", position = "fill")+
  scale_y_continuous(labels = scales::percent_format())




data %>%
  ggplot(aes(poutcome, fill = subscribed))+
  geom_bar(stat = "count", position = "fill")+
  scale_y_continuous(labels = scales::percent_format())


#campaign
data %>%
  select(campaign,subscribed) %>%
  ggplot(aes(campaign, fill = subscribed))+
  geom_boxplot()+
  scale_x_log10()+
  coord_flip()


data %>%
  select(age, campaign, subscribed) %>%
  ggplot(aes(y = campaign, x = age, col = subscribed)) +
  geom_point(alpha = 0.3)+
  geom_hline(yintercept = 8)+
  scale_y_log10()+
  scale_x_log10()+
  labs(
    y = "Contact during Campaign",
    x = "Age",
    col = "Outcome"
  )

#data prepocessing for correlation test
data$subscribed[data$subscribed == "no"] <-0
data$subscribed[data$subscribed == "yes"] <-1

data$subscribed <- as.numeric(data$subscribed)


data$poutcome <- as.numeric(factor(data$poutcome, 
                               levels = c("nonexistent","failure", "success"), labels = c(0,1,2) ,ordered = TRUE))


data$poutcome <- as.numeric(data$poutcome)


#Address categorical errors
sapply(data, class)

data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)

sapply(data, class)

table(data$subscribed)



#change certain variables to numeric for better correlation
data$education <- as.numeric(data$education)

data$job <- as.numeric(data$job)

data$contact <- as.numeric(data$contact)
data$poutcome <- as.numeric(data$poutcome)



#correlation
cor(x= data$age,y= data$subscribed, use = "complete.obs", method = "pearson")
cor(x= data$poutcome,y= data$subscribed, use = "complete.obs") #positive
cor(x= data$previous,y= data$subscribed, use = "complete.obs") #positive
cor(x= data$euribor3m,y= data$subscribed, use = "complete.obs", method = "pearson") #negative
cor(x= data$nr.employed ,y= data$subscribed, use = "complete.obs") #negative


t.test(data$subscribed, data$euribor3m)
t.test(data$subscribed,data$age)
t.test(data$subscribed, data$poutcome)
t.test(data$subscribed, data$previous)



cor(x= data$duration ,y= data$subscribed, use = "complete.obs")

cor.test(x= data$age, y=data$subscribed)

cor.test(x= data$euribor3m, y=data$subscribed)

cor.test(x= data$poutcome, y=data$subscribed)

cor.test(x= data$previous, y=data$subscribed)


#make correlation plot

subdata <- data[c("age","euribor3m", "poutcome", "previous", "subscribed")]
cor <- cor(subdata)
cor_sort <- as.matrix(sort(cor[,'subscribed'], decreasing = TRUE))
corrplot.mixed(cor, tl.col = "black", tl.pos = "lt")

sns.heatmap(data[["age", "euribor3m", "poutcome", "previous"]].corr(), cmap='Blues', annot=True)
plt.show()


data$subscribed <- as.factor(data$subscribed)

set.seed(40387286)
index <- createDataPartition(data$subscribed, p= 0.8, list=FALSE)
train <- data[index,]
test <- data[-index,]


test_x = test[, -1] # feature and target array
test_y = test[, 1] 



formula1 <- subscribed ~ age + euribor3m + poutcome + previous
model1 <- glm(formula1, data = train, family = "binomial")
summary(model1)


formula2 <- subscribed ~ age + euribor3m + poutcome + previous+nr.employed
model2 <- glm(formula2, data = train, family = "binomial")
summary(model2)


formula3 <- subscribed ~ age + euribor3m + poutcome + previous+education
model3 <- glm(formula3, data = train, family = "binomial")
summary(model3)

formula4 <- subscribed ~ age + euribor3m + poutcome + previous+day_of_week
model4 <- glm(formula4, data = train, family = "binomial")
summary(model4)

formula5 <- subscribed ~ age + euribor3m + poutcome + previous+campaign
model5 <- glm(formula5, data = train, family = "binomial")
summary(model5)

formula <- subscribed ~ age + euribor3m + poutcome + previous+nr.employed+education+day_of_week+campaign
model <- glm(formula, data = train, family = "binomial")
summary(model)

formula8 <- subscribed ~ euribor3m 
model8 <- glm(formula8, data = train, family = "binomial")
summary(model8)



 logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <- length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}


logisticPseudoR2s(model)

exp(model$coefficients)
exp(confint(model))

train$predictedProbabilities <- fitted(model)
head(train$predictedProbabilities, train$subscribed)


#residual analysis
train$standardisedResiduals <- rstandard(model)
train$studentisedResiduals <- rstudent(model)

sum(train$standardisedResiduals > 1.96)
sum(train_residuals > 1.96)

train_residuals <- resid(model)
train_predictions <- fitted(model)
hist(train_residuals, breaks = 40)


train$cook <- cooks.distance(model)
sum(train$cook > 1)

vif(model)



predictions<- predict(model, test, type = "response")


#Check AUC-ROC
# create roc curve
roc_object <- roc( test$subscribed, predictions)

# calculate area under curve
auc( roc_object )



class_pred <- as.factor(ifelse(predictions > .5, "Yes", "No"))
postResample(class_pred, test$subscribed)



