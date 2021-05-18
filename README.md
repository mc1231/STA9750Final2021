---
title: "STA9750 Final Project. Bank Customer Churn"
author: "Michelle Chen, Kenny Li, Le Lou"
output: html_document

---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
```

### Introduction

Banking is a competitive market that is saturated with many services offered to customers. The many options give customers ease in changing their banks and therefore customer churn is an issue that banks are finding ways to address. It is important for banks to retain their current customers because the costs to acquire customers are higher than the cost of retention. Banks can use already existing data to learn how about customer behaviors or demographics that can lead to churning. Being able to identify potential churn customers will allow the bank to target these customers with efforts to prevent churn. 

Our research project explores the potential customer attributes for churned customers for credit card services at a bank. We will identify and visualize the significant factors in customer churn. Logistic regression and random forest are classification models that will be used to predict if a customer will churn or not. We hypothesize that behavioral attributes related to customer's activity will indicate potential customer churn. Through the project, we will identify the types of activities that banks should be aware of to retain customers. 

```{r Overview of Dataset/Clean Data}
bank_data <- read.csv("BankChurners.csv")

```

### Overview of Dataset
The data for our analysis comprise of customer profile, demographics, and behavior(usage of services) and it is from kaggle which originates from analytticca. It covers one bank with `r dim(bank_data)[1]` customers, and contains `r ncol(bank_data)` columns of customer information. There is `r sum(is.na(bank_data))` missing values in the dataset. We decided to drop Client Number column because it is nominal and cannot be measured. The target variable is Attrition Flag which indicates if the customer is still existing or attrited. The predictors can be categorized into two types of attributes which is either demographic or behavioral. The demographic attributes include customer age, gender, dependent, education, marital status, and income. 

The behavioral attributes include Months on Book, Total Relationship Count, Months Inactive, Contacts Count(12 months), Credit Limit, Total Revolving Balance, Average Open to Buy, Total Amount Change Q4 to Q1, Total Transaction Amount, Total Transaction Count, Total Count Change Q4 - Q1, Average Utilization Ratio. 

The limitations to the dataset is that the data seems to be a snapshot and there is no date to when the information is collected. A date can explain the relevance of the data and insights. The dataset is relatively small with `r dim(bank_data)[1]` customers and only `r `

### Summary Statistics





