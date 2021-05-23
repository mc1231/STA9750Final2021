---
title: "Bank Customer Churn"
author: "Michelle Chen, Kenny Li, Le Lou"
output: html_document

---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(ggplot2)
library(reshape2)
require(gridExtra)
```

### STA9750 Final Project

### Introduction

Banking is a competitive market that is saturated with many services offered to customers. The many options give customers ease in changing their banks and therefore customer churn is an issue that banks are finding ways to address. It is important for banks to retain their current customers because the costs to acquire customers are higher than the cost of retention. Banks can use already existing data to learn which customer behaviors or demographics can lead to churning. Being able to identify potential churn customers will allow the bank to target these customers with efforts to prevent churn. 

Our research project explores the potential customer attributes for churned customers for credit card services at a bank. We will identify and visualize the significant factors in customer churn. Classification models such as logistic regression and random forest will be used to predict if a customer will churn or not. We hypothesize that behavioral attributes related to customer's activity will indicate potential customer churn. Through the project, we will identify the types of activities that banks should be aware of to retain customers. 


```{r Overview of Dataset/Clean Data,include=FALSE}
library(dplyr)

#import data
df <- read.csv("BankChurners.csv")

```

### Overview of Dataset
The data for our analysis comprise of customer profile, demographics, and behavior(usage of services) and it is from kaggle which originates from analytticca. It covers one bank with `r dim(df)[1]` customers, and contains `r ncol(df)` columns of customer information. There is `r sum(is.na(df))` missing values in the dataset. 

```{r Drop columns,include=FALSE}
# Remove Client Number and two naive bayes irrelevant columns in dataset
df <- df %>% 
  select(-c(CLIENTNUM, 
            Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1, Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2))
```

```{r Transform variables,include=FALSE}
# Transform values to 1 or 0. 
df$Attrition_Flag <- as.integer(df$Attrition_Flag  == "Attrited Customer")

```

We decided to drop Client Number column because it is nominal and cannot be measured. The target variable is Attrition Flag which indicates if the customer is still existing or attrited. The predictors can be categorized into two types of attributes which is either demographic or behavioral. The demographic attributes include customer age, gender, dependent, education, marital status, and income. The behavioral attributes include Months on Book, Total Relationship Count, Months Inactive, Contacts Count(12 months), Credit Limit, Total Revolving Balance, Average Open to Buy, Total Amount Change Q4 to Q1, Total Transaction Amount, Total Transaction Count, Total Count Change Q4 - Q1, and Average Utilization Ratio. The attributes will be explored in the Exploratory Data Analysis section. 

```{r Attrition value count,include=FALSE}
count_attr <- df %>%
  count(Attrition_Flag)
count_attr[2,2]

round((count_attr[2,2]/count(df['Attrition_Flag']) * 100), 2)
```

### Limitations
The limitations to the dataset is that the data is a snapshot of the customers activity but there is no date to when the information is collected. A date can explain the relevance of the data and insights. 

The goal of our research is to understand the attributes related to "Attrited Customers" but the dataset is relatively small with `r dim(df)[1]` customers and only `r count_attr[2,2]` are attrited which is `r round((count_attr[2,2]/count(df['Attrition_Flag']) * 100), 2)`% of customers in this dataset as shown in the figure below. Therefore there may be a sample size limitation. 

```{r Attrition graphic, fig.width=3, fig.height=1.5, ,echo=FALSE}
df2 <- read.csv("BankChurners.csv")
ggplot(df2 , aes(x=Attrition_Flag, fill = Attrition_Flag, show.legend = FALSE)) + 
  geom_bar() +
  ggtitle("Attrition Count") +
  xlab("Type of Customer") + ylab("Count") +
  theme(legend.position = "none", plot.title = element_text(size = 8), axis.title.y = element_text(size = 6), axis.title.x = element_text(size = 6), axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 6))

```

### Exploratory Data Analysis
##### Demographic Attribute Analysis
First we analyzed the income and card category of customers.

```{r Explore income ,echo=FALSE, include=FALSE}
df2$Income_Category <- factor(df2$Income_Category,levels = c("Less than $40K", "$40K - $60K", "$60K - $80K", "$80K - $120K", "$120K +", "Unknown"))

incomeplot <- ggplot(df2 , aes(Income_Category)) + 
  geom_bar()+
  ggtitle("Income Count") +
  xlab("Income Level") + ylab("Count") +
  theme(legend.position = "none", plot.title = element_text(size = 10), axis.text.x = element_text( 
  size = 6), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8),
  axis.text.y = element_text(size = 6))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4000))


```

```{r Explore card ,echo=FALSE, include=FALSE}
df2$Card_Category <- factor(df2$Card_Category,levels = c("Blue", "Silver", "Gold", "Platinum"))


cardplot <- ggplot(df2 , aes(Card_Category, fill = Card_Category)) + 
  geom_bar() +
  scale_fill_manual(values=c("cornflowerblue", "grey70", "gold" , "slategrey"))+ 
  theme(legend.position = "none", plot.title = element_text(size = 10), axis.text.x = element_text( 
  size = 6), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 8),
  axis.text.y = element_text(size = 6))+
  ggtitle("Card Count") +
  xlab("Type of Credit Card") + ylab("Count") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10000))

```

```{r Combine count graphs ,echo=FALSE, fig.width=8, fig.height=2}
grid.arrange(incomeplot, cardplot, ncol=2)
```

- The income count is right skewed which is fairly normal for income levels. The less than 40k income level accounts for 35% of the dataset which is the highest count for customers. It is also interesting to note that there is an unknown income level for a credit card customer because applying for credit usually requires the customer to have an income to be approved. 

- 93% which is the majority of the credit card customers hold a blue card which most likely means that it is the standard type of credit card. This may also imply that card categories will not be a significant attribute in predicting churn because a majority of this bank's customers are in the blue card category. 

We will now analyze the attrition rate of customer status by income, card, education, and gender. A bar chart is used to compare the categories to see if there is a significant difference in distribution to a particular subgroup.

```{r Explore income attrition ,echo=FALSE, include=FALSE}
income_table <- df2 %>% 
  select(Attrition_Flag, Income_Category) %>% 
  count(Attrition_Flag, Income_Category) %>% 
  group_by(Income_Category) %>% 
  mutate(pct = n / sum(n)) %>%
  mutate(pct1 = scales::percent(pct))

plot1 <- ggplot(income_table , aes(x=Income_Category, y =pct, fill = Attrition_Flag)) + 
  geom_bar(stat='identity', position=position_dodge())+
  facet_wrap(~Attrition_Flag)  + 
  coord_flip() + 
  geom_text(aes(label = round(pct, 2)), size = 2)+
  ggtitle("Attrition Rate by Income") +
  ylab("Attrition Rate") + xlab("Income Category") +
  theme(legend.position = "none", axis.text.x = element_text( 
    size = 6, angle = 0))

```

```{r Explore Card Category Attrition ,echo=FALSE,include=FALSE}
card_table <- df2 %>% 
  select(Attrition_Flag, Card_Category) %>% 
  count(Attrition_Flag, Card_Category) %>% 
  group_by(Card_Category) %>% 
  mutate(pct = n / sum(n)) %>%
  mutate(pct1 = scales::percent(pct))

plot2 <- ggplot(card_table , aes(x=Card_Category, y =pct, fill = Attrition_Flag)) + 
  geom_bar(stat='identity', position=position_dodge())+
  facet_wrap(~Attrition_Flag)  + 
  coord_flip() + 
  geom_text(aes(label = round(pct, 2)), size = 2)+
  ggtitle("Attrition Rate by Card") +
  ylab("Attrition Rate") + xlab("Card Category") +
  theme(legend.position = "none", axis.text.x = element_text( 
    size = 6))

```

```{r Explore Education Level ,echo=FALSE,include=FALSE}
df2$Education_Level <- factor(df2$Education_Level,levels = c("Unknown", "Uneducated", "High School", "College", "Graduate", "Post-Graduate", "Doctorate"))

education_table <- df2 %>% 
  select(Attrition_Flag, Education_Level) %>% 
  count(Attrition_Flag, Education_Level) %>% 
  group_by(Education_Level) %>% 
  mutate(pct = n / sum(n)) %>%
  mutate(pct1 = scales::percent(pct))

plot3 <- ggplot(education_table , aes(x=Education_Level, y =pct, fill = Attrition_Flag)) + 
  geom_bar(stat='identity', position=position_dodge())+
  facet_wrap(~Attrition_Flag)  + 
  coord_flip() + 
  geom_text(aes(label = round(pct, 2)), size = 2)+
  ggtitle("Attrition Rate by Education") +
  ylab("Attrition Rate") + xlab("Education Level") +
  theme(legend.position = "none", axis.text.x = element_text( 
    size = 6, angle = 0))
```

```{r Explore Gender ,echo=FALSE,include=FALSE }
gender_table <- df2 %>% 
  select(Attrition_Flag, Gender) %>% 
  count(Attrition_Flag, Gender) %>% 
  group_by(Gender) %>% 
  mutate(pct = n / sum(n)) %>%
  mutate(pct1 = scales::percent(pct))

plot4 <- ggplot(gender_table , aes(x=Gender, y =pct, fill = Attrition_Flag)) + 
  geom_bar(stat='identity', position=position_dodge())+
  facet_wrap(~Attrition_Flag)  + 
  coord_flip() + 
  geom_text(aes(label = round(pct, 2)), size = 2)+
  ggtitle("Attrition Rate by Gender") +
  ylab("Attrition Rate") + xlab("Gender") +
  theme(legend.position = "none")
```

```{r Combine graphs ,echo=FALSE}
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)
```

- The attrition rate by income shows that a range of 13% to 17% of customers attrited. There is no significant indication of which type of income level customers are most likely to churn. 
- The attrition rate by card shows that platinum  and gold card customers are more likely to churn than silver and blue card customers. 
- The attrition by education shows that the higher education level customers are more likely to churn. Doctorate level customers are the most likely to churn.
- The attrition rate by gender shows that there is no significant difference between attrition rate by gender. 

The figures show some difference between subgroups but the distribution of each category is similar which supports our hypothesis that demographic attributes may be less significant in predicting potential customer churn. 

#### Numerical/Behavioral Attribute Analysis
We used a heat map to explore which numerical attributes show correlation to customer attrition. The correlation ranges from 1 to -1. The darker the cell, the higher the correlation between the two attributes. Lighter shaded cells or values closer to 0 means there is little to no correlation between the attributes. The cell colors and correlation score will be used to determine which attributes will be explored more in this section. Note that this heat map only analyzes numerical attributes. 

```{r heatmap, fig.width=6, fig.height=4,echo=FALSE}
explore_df <- df %>%
  select(Attrition_Flag,Customer_Age,Dependent_count, Months_on_book,	Total_Relationship_Count,	Months_Inactive_12_mon,	Contacts_Count_12_mon,	Credit_Limit,	Total_Revolving_Bal,	Avg_Open_To_Buy,	Total_Amt_Chng_Q4_Q1,	Total_Trans_Amt,	Total_Trans_Ct,	Total_Ct_Chng_Q4_Q1,	Avg_Utilization_Ratio)

cormat <- round(cor(explore_df),2)

melted_cormat <- melt(cormat)

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)}

upper_tri <- get_upper_tri(cormat)


melted_cormat <- melt(upper_tri, na.rm = TRUE)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
 geom_tile(color = "white")+ 
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
   name="Correlation") +
  theme_minimal()+ 
 theme(axis.text.x = element_text(angle = 45, vjust = 1, 
    size = 9, hjust = 1)) +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2)
```

The heat map shows that credit limit, average open to buy, months on book, age, and dependents are insignificant to customer attrition since the correlation  is between -0.1 and 0.1.

Based on this heatmap, we will explore the customer attrition by attributes with high correlation. The following graphs are boxplots to compare attributes between attrited customers and existing customers.

```{r Explore Behavioral Attributes ,echo=FALSE, include=FALSE}
#Plot Transaction Count
plot5 <- ggplot(df2 , aes(x=Total_Trans_Ct,y= Attrition_Flag,  fill = Attrition_Flag)) + 
  geom_boxplot() +
  coord_flip() + 
  ggtitle("1. Transaction Count by Customer Status") +
  xlab("Transaction Count") + ylab("") +
  theme(legend.position = "none", plot.title = element_text(size = 6), axis.title.y = element_text(size = 6),   axis.text.x = element_text(size = 6),   axis.text.y = element_text(size = 8))

#Plot Revolving Balance
plot6 <- ggplot(df2 , aes(x=Total_Revolving_Bal,y= Attrition_Flag,  fill = Attrition_Flag)) +
  geom_boxplot() +
  coord_flip() + 
  ggtitle("2. Revolving Balance by Customer Status") +
  xlab("Revolving Balance($)") + ylab("") +
  theme(legend.position = "none", plot.title = element_text(size = 6), axis.title.y = element_text(size = 6),   axis.text.x = element_text(size = 6),   axis.text.y = element_text(size = 8))

#Plot Count Change
plot7 <- ggplot(df2 , aes(x=Contacts_Count_12_mon,y= Attrition_Flag,  fill = Attrition_Flag)) + 
  geom_boxplot() +
  coord_flip() + 
  ggtitle("3. Contact Count by Customer Status") +
  xlab("Contact Count") + ylab("") +
  theme(legend.position = "none", plot.title = element_text(size = 6), axis.title.y = element_text(size = 6),   axis.text.x = element_text(size = 6),   axis.text.y = element_text(size = 8))

#Plot Transaction Count Change
plot8 <- ggplot(df2 , aes(x=Total_Ct_Chng_Q4_Q1,y= Attrition_Flag,  fill = Attrition_Flag)) + 
  geom_boxplot() +
  coord_flip() + 
  ggtitle("Transaction Count Change by Customer Status") +
  xlab("Transaction Count Change") + ylab("") +
  theme(legend.position = "none", plot.title = element_text(size = 6), axis.title.y = element_text(size = 6),   axis.text.x = element_text(size = 6),   axis.text.y = element_text(size = 8))

#Plot Average Utilization Ratio
plot9 <- ggplot(df2 , aes(x=Avg_Utilization_Ratio,y= Attrition_Flag,  fill = Attrition_Flag)) + 
  geom_boxplot() +
  coord_flip() + 
  ggtitle("5. Average Utilization Ratio by Customer Status") +
  xlab("Average Utilization Ratio") + ylab("") +
  theme(legend.position = "none", plot.title = element_text(size = 6), axis.title.y = element_text(size = 6),   axis.text.x = element_text(size = 6),   axis.text.y = element_text(size = 8))

#Plot Total Transaction Amount
plot10 <- ggplot(df2 , aes(x=Total_Trans_Amt,y= Attrition_Flag,  fill = Attrition_Flag)) + 
  geom_boxplot() +
  coord_flip() + 
  ggtitle("6. Transaction Amount by Customer Status") +
  xlab("Total Transaction Amount") + ylab("") +
  theme(legend.position = "none", plot.title = element_text(size = 6), axis.title.y = element_text(size = 6),   axis.text.x = element_text(size = 6),   axis.text.y = element_text(size = 8))

#Plot Total Relationship Count
plot11 <- ggplot(df2 , aes(x=Total_Relationship_Count,y= Attrition_Flag,  fill = Attrition_Flag)) + 
  geom_boxplot() +
  coord_flip() + 
  ggtitle("Total Relationship Count by Customer Status") +
  xlab("Total Relationship Count") + ylab("") +
  theme(legend.position = "none", plot.title = element_text(size = 6), axis.title.y = element_text(size = 6),   axis.text.x = element_text(size = 6),   axis.text.y = element_text(size = 8))

#Plot Months Inactive
plot12 <- ggplot(df2 , aes(x=Months_Inactive_12_mon,y= Attrition_Flag,  fill = Attrition_Flag)) + 
  geom_boxplot() +
  coord_flip() + 
  ggtitle("4. Months Inactive by Customer Status") +
  xlab("Months") + ylab("") +
  theme(legend.position = "none", plot.title = element_text(size = 6), axis.title.y = element_text(size = 6),   axis.text.x = element_text(size = 6),   axis.text.y = element_text(size = 8))

#Plot Total Amount Change
plot13 <-ggplot(df2 , aes(x=Total_Amt_Chng_Q4_Q1,y= Attrition_Flag,  fill = Attrition_Flag)) + 
  geom_boxplot() +
  coord_flip() + 
  ggtitle("Total Amount Change by Customer Status") +
  xlab("Total Amount Change($)") + ylab("") +
  theme(legend.position = "none", plot.title = element_text(size = 6), axis.title.y = element_text(size = 6),   axis.text.x = element_text(size = 6),   axis.text.y = element_text(size = 8))

#Plot Credit Limit
plot14 <-ggplot(df2 , aes(x=Credit_Limit,y= Attrition_Flag,  fill = Attrition_Flag)) + 
  geom_boxplot() +
  coord_flip() + 
  ggtitle("Credit Limit by Customer Status") +
  xlab("Credit Limit($)") + ylab("") +
  theme(legend.position = "none", plot.title = element_text(size = 6), axis.title.y = element_text(size = 6),   axis.text.x = element_text(size = 6),   axis.text.y = element_text(size = 8))
```


```{r Combine behavioral attribute graphs ,echo=FALSE}
grid.arrange(plot5, plot6, plot7,plot12, plot9, plot10, ncol=3)
```

We note the following...

1. Attrited customers have lower total transaction count than existing customers. All attrited customers have transaction count of less than 100. Therefore, the higher the transaction count, the more likely the customer will continue using the bank's credit card services.
2. Attrited customers have lower revolving balance on the credit card than existing customers. The median revolving balance for attrited customers is 0 which may be due to paying the balance to close the credit card account. 
3. The more a customer contacts a bank more, the more likely to they are to churn. 
4. After 2 months of inactivity, the customer is likely to churn. 
5. The less a customers use the credit card, the more likely they are to churn. 
6. The total transaction amount for attrited customers are lower than existing customers. The lower the total transaction amount, the more likely the customer will churn.

The exploratory analysis of behavioral attributes show that customer activity may have a better indication of potential churn. 

### Model - Logistic Regression
##### Feature Processing/Selection
The feature processing is to convert all text values into numeric values. Here we convert 'female' as 1 and 'male' as 0 in the Gender column. Then we did one-hot encoding to convert multiple category attributes into several new attributes.
```{r Features Processing, warning=FALSE,message=FALSE,echo=FALSE}
# 1 and 0 in 'Gender'
df1<-df
df1$Gender<- ifelse(df$Gender=="M",0, ifelse(df$Gender == "F",1,99))

# one-hot encoding for the rest
library(creditmodel)
df1=one_hot_encoding(df1,c("Marital_Status","Education_Level","Income_Category","Card_Category"))
```

We decided to remove some attributes from logistic regression modeling because logistic regression cannot handle high correlated variables well. Based on the correlation head map shown above, we found three pairs of attributes with strong correlation. They are:

- Customer_Age vs. Months_On_Book 
- Credit_Limit vs. Avg_Open_To_Buy
- Total_Trans_Amt vs. Total_Trans_Ct

We decided to remove one of them from each. We considered Months_on_book as a more direct attribute to show the relationship with customers and the bank. And Credit_Limit is easier to understand comparing with Avg_Pen_To_buy. Besides, we believed the number of transactions is more representative than the total amount of transactions because the number of transactions implies a frequency that customers cooperate with the bank. A larger transaction amount can happen when a customer decides to stop the business with the bank and transfer all money out of the bank. Therefore, we removed attributes Customer_Age, Avg_Open_To_Buy, and Total_Trans_Amt.

In addition, the same problem happens among new attributes created by one-hot encoding. When other categories are all equal to 0, the last categories must be 1. Hence there is a perfect correlation in this situation. We have to remove one category from each original attribute. Here we consider category-Unknown in Marital_Status, Education_Level, Income_Category are meaningless in our analysis of our project. And in Card_Category, the Platinum has only `r sum(df$Card_Category=='Platinum')` active values. It shows less significance in the analysis. Therefore, we removed all Unknown attributes and Card_Category.Platinum.
```{r Features Selection, echo=FALSE}
df1 <- df1 %>% 
  select(-c(Avg_Open_To_Buy,Customer_Age,Total_Trans_Amt, Marital_Status.Unknown., Education_Level.Unknown., Income_Category.Unknown.,Card_Category.Platinum.))
```

```{r Split/Standardize Data, echo=FALSE,message=FALSE}
# split the data into train(70%) and test(30%) randomly.
set.seed(53)
dt = sort(sample(nrow(df1), nrow(df1)*.7))
train<-df1[dt,]
test<-df1[-dt,]

# standardize the data
library(standardize)
sd_train <- train
sd_test <- test
my_range <- 2:30
for (i in my_range){
  train_mean = mean(train[,i])
  train_sd = sd(train[,i])
  sd_train[,i]=(train[,i]-train_mean)/train_sd
  sd_test[,i]=(test[,i]-train_mean)/train_sd
    }
```

##### Model summary
The table is sorted by absolute value of Estimate in descending order. All numbers are round in 4 digits.
```{r, echo=FALSE, message = FALSE}
# run logistic regression model
glm=glm(Attrition_Flag ~ .-Attrition_Flag ,
data=sd_train ,family =binomial)
summary(glm)$call
summary<-round(summary(glm)$coef,4)
knitr::kable(
  summary[order(abs(summary[,1]),decreasing = TRUE),], 
  caption = "Coefficient Table for Logistic Regression"
)
```

The Estimate in summary shows the importance of each attributes to the attrition of the bank. From the table, we found that the total number of transactions contributes the most importance to negatively influence the attrition of customers. And it is statistically significant in the 99% confidence level, which verifies our first finding based on the Behavioral Attribute Analysis. And Total_Relationship_Count represents the number of products held by the customer from bank. Therefore, a customer who holds fewer products from the bank is more likely to be attrition. Besides, a married person is less likely to be attrition based on the result of the model. It might be explained that a married person has a stronger ability to keep a healthy cash flow.

In addition, we also found many attributes that have positive influences on customers' churn. Contacts_Count_12_mon shows that if customers contact the bank more frequently, they probably churn because of dissatisfaction. A long inactive time also implies a churn of customers. Besides, a higher number of dependents also give pressure on customers to avoid using credit cards.

##### Validation on train/test datasets
We decided to use plot of ROC Curve and the Area Under the ROC Curve(AUC) score as the standard to judge the logistic regression model's performance fitted in train and test datasets. 

```{r, echo=FALSE,warning=FALSE,message=FALSE}
library(pROC)

#matrix evaluation for train and test
par(mfcol=c(1,2))
train_prob = predict(glm, newdata = sd_train, type = "response")
train_roc = roc(sd_train$Attrition_Flag ~ train_prob, plot = TRUE, print.auc = TRUE, main='ROC Curve for Train')
train_auc <- as.numeric(train_roc$auc)

test_prob = predict(glm, newdata = sd_test, type = "response")
test_roc = roc(sd_test$Attrition_Flag ~ test_prob, plot = TRUE, print.auc = TRUE, main='ROC Curve for Test')
 test_auc <- as.numeric(test_roc$auc)
```
The more curve closes to the left-upper corner, the better the model performs. Besides, the AUC score of the train is `r round(train_auc,4)`, and the AUC score of the test is `r round(test_auc,4)`, which shown that the model fitted two datasets pretty well. And there is overfitting phenomenon in this model.





