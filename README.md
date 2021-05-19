---
title: "Bank Customer Churn"
author: "Michelle Chen, Kenny Li, Le Lou"
output: html_document

---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(ggplot2)
```

### STA9750 Final Project

### Introduction

Banking is a competitive market that is saturated with many services offered to customers. The many options give customers ease in changing their banks and therefore customer churn is an issue that banks are finding ways to address. It is important for banks to retain their current customers because the costs to acquire customers are higher than the cost of retention. Banks can use already existing data to learn how about customer behaviors or demographics that can lead to churning. Being able to identify potential churn customers will allow the bank to target these customers with efforts to prevent churn. 

Our research project explores the potential customer attributes for churned customers for credit card services at a bank. We will identify and visualize the significant factors in customer churn. Classification models will be used to predict if a customer will churn or not. We hypothesize that behavioral attributes related to customer's activity will indicate potential customer churn. Through the project, we will identify the types of activities that banks should be aware of to retain customers. 


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

We decided to drop Client Number column because it is nominal and cannot be measured. The target variable is Attrition Flag which indicates if the customer is still existing or attrited. The predictors can be categorized into two types of attributes which is either demographic or behavioral. The demographic attributes include customer age, gender, dependent, education, marital status, and income. The behavioral attributes include Months on Book, Total Relationship Count, Months Inactive, Contacts Count(12 months), Credit Limit, Total Revolving Balance, Average Open to Buy, Total Amount Change Q4 to Q1, Total Transaction Amount, Total Transaction Count, Total Count Change Q4 - Q1, and Average Utilization Ratio. The attributes will be explored in the Exploratory Data Analysis section. 

```{r Attrition value count,include=FALSE}
count_attr <- df %>%
  count(Attrition_Flag)
count_attr[1,2]

round((count_attr[1,2]/count(df['Attrition_Flag']) * 100), 2)
```

### Limitations
The limitations to the dataset is that the data is a snapshot of the customers activity but there is no date to when the information is collected. A date can explain the relevance of the data and insights. 

The goal of our research is to understand the attributes related to "Attrited Customers" but the dataset is relatively small with `r dim(df)[1]` customers and only `r count_attr[1,2]` are attrited which is `r round((count_attr[1,2]/count(df['Attrition_Flag']) * 100), 2)`% of customers in this dataset as shown in the figure below. Therefore there may be a sample size limitation. 


```{r Attrition graphic, fig.width=3, fig.height=2, ,echo=FALSE}
ggplot(df , aes(x=Attrition_Flag, fill = Attrition_Flag, show.legend = FALSE)) + 
  geom_bar() +
  ggtitle("Attrition Count") +
  xlab("Type of Customer") + ylab("Count") +
  theme(legend.position = "none")

```

### Exploratory Data Analysis

