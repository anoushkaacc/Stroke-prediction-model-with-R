---
title: "Build and deploy a stroke prediction model using R"
date: "`r Sys.Date()`"
author: "Anoushka Chatterjee"
---

# About Data Analysis Report

This RMarkdown file contains the report of the data analysis done for the project on building and deploying a stroke prediction model in R. It contains analysis such as data exploration, summary statistics and building the prediction models. The final report was completed on `r date()`. 

**Data Description:**

According to the World Health Organization (WHO) stroke is the 2nd leading cause of death globally, responsible for approximately 11% of total deaths.

This data set is used to predict whether a patient is likely to get stroke based on the input parameters like gender, age, various diseases, and smoking status. Each row in the data provides relevant information about the patient.


# Task One: Import data and data preprocessing

## Load data and install packages

```{r}
if(!require('tidyverse')){
  install.packages('tidyverse')
  library('tidyverse')
}

if(!require('tidyverse')){
  install.packages('tidyverse')
  library('tidyverse')
}

if(!require('ggplot2')){
  install.packages('ggplot2')
  library('ggplot2')
}

if(!require('dplyr')){
  install.packages('dplyr')
  library('dplyr')
}

if(!require('caret')) {
  install.packages('caret')
  library('caret')
}

if(!require('randomForest')) {
  install.packages('randomForest')
  library('randomForest')
}

if(!require('skimr')) {
  install.packages('skimr')
  library('skimr')
}

if(!require('gridExtra')) {
  install.packages('gridExtra')
  library('gridExtra')
}

if(!require('caTools')) {
  install.packages('caTools')
  library('caTools')
}

if(!require('corrplot')) {
  install.packages('corrplot')
  library('corrplot')
}

if(!require('ggcorrplot')) {
  install.packages('ggcorrplot')
  library('ggcorrplot')
}

if(!require('naniar')){
  install.packages('naniar')
  library('naniar')
}

data <- read.csv('healthcare-dataset-stroke-data.csv')
```


## Describe and explore the data

```{r}
summary(data)
```
```{r}
glimpse(data)
```
```{r}
skim(data)

```
```{r}
miss_scan_count(data = data, search = list("Unknown","N/A","Other"))
```

```{r}
data$bmi <- as.numeric(data$bmi)

```
```{r}
idx <- complete.cases(data)
bmi_idx <- is.na(data$bmi)
median_bmi <- median(data$bmi , na.rm = TRUE)
data[bmi_idx,]$bmi <- median_bmi
colSums(is.na(data))

```
```{r}
sum(duplicated(data))

```
```{r}
colSums(data == 'N/A')

```
```{r}
colSums(data == '')
```
```{r}
data %>% count(gender)
```


```{r}
data <- data %>%
  select(-c(id)) %>%
  filter(gender != "other")
str(data)

```
```{r}
data$stroke <- factor(data$stroke , levels = c(0,1), labels = c("no","yes"))

data$hypertension <- factor(data$hypertension, levels= c(0,1), labels = c("no","yes"))

data$heart_disease <- factor(data$heart_disease, levels= c(0,1), labels = c("no","yes"))
```



# Task Two: Build prediction models

```{r}

d1 <- data %>%
  ggplot(aes(x=gender, fill = gender)) +
  geom_bar(fill=c("#FF76CE","#FDFFC2","#94FFD8"))+
  ggtitle("Gender distribution") +
  geom_text(aes(label=after_stat(count)),stat="Count",vjust = 1.0)


d2 <- data %>%
  ggplot(aes(x = hypertension, fill = hypertension)) +
  geom_bar(fill = c("#FF76CE", "#FDFFC2")) +
  ggtitle("Hypertenstion Distribution") +
  geom_text(aes(label=after_stat(count)), stat = "Count", vjust = 1.0)
  

d3 <- data %>%
  ggplot(aes(x = heart_disease, fill = heart_disease)) +
  geom_bar(fill = c("#FF76CE", "#FDFFC2")) +
  ggtitle("Heart Disease Distribution") +
  geom_text(aes(label=after_stat(count)), stat = "Count", vjust = 1.0)

d4 <- data %>%
  ggplot(aes(x = ever_married, fill = ever_married)) +
  geom_bar(fill = c("#FF76CE","#FDFFC2")) +
  ggtitle("Married distribution") +
  geom_text(aes(label=after_stat(count)), stat = "Count", vjust = 1.0)

d5 <- data %>%
  ggplot(aes(x = work_type, fill = work_type)) +
  geom_bar(fill = c("#FF76CE", "#FDFFC2","#94FFD8","#2C4E80","#640D6B")) +
  ggtitle("Work type distribution") +
  geom_text(aes(label=after_stat(count)), stat = "Count", vjust = 1.0)

d6 <- data %>%
  ggplot(aes(x = stroke, fill = stroke)) +
  geom_bar(fill = c("#FF76CE", "#FDFFC2")) +
  ggtitle("Stroke distribution") +
  geom_text(aes(label=after_stat(count)), stat = "Count", vjust = 1.0)

d7 <- data %>%
  ggplot(aes(x = Residence_type, fill = Residence_type)) +
  geom_bar(fill = c("#FF76CE", "#FDFFC2")) +
  ggtitle("Residence distribution") +
  geom_text(aes(label=after_stat(count)), stat = "Count", vjust = 1.0)


grid.arrange(d1,d2,d3,d4,d5,d6,d7, ncol=3)
  

```

```{r}
data %>%
  ggplot(aes(x = gender, fill = stroke)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values=c("maroon",
                             "pink")) +
  ggtitle("Gender vs. Stroke") 
```

```{r}
data %>%
  ggplot(aes(x = hypertension, fill = stroke)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values=c("maroon",
                             "pink")) +
  ggtitle("Hypertension vs. Stroke")

```

```{r}
data %>%
  ggplot(aes(x = heart_disease, fill = stroke)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values=c("maroon",
                             "pink")) +
  ggtitle("Heart disease vs. Stroke") 

```


```{r}
data %>%
  ggplot(aes(x = Residence_type, fill = stroke)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values=c("maroon",
                             "pink")) +
  ggtitle("Residence type vs. Stroke")
```

```{r}
data %>%
  ggplot(aes(x = smoking_status, fill = stroke)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values=c("maroon",
                             "pink")) +
  ggtitle("Smoking status vs. Stroke")
```

```{r}
data %>%
  ggplot(aes(x = work_type, fill = stroke)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values=c("maroon",
                             "pink"
                             )) +
  ggtitle("Type of Work vs. Stroke")
```

```{r}
data %>%
  ggplot(aes(x = avg_glucose_level, fill = stroke)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values=c("#10439F",
                             "#C65BCF"
  )) +
  ggtitle("Average Glucose level vs. Stroke") 

```

```{r}
data %>% filter(between(bmi, 0, 60)) %>%
  ggplot(aes(x = bmi, fill = stroke)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values=c("#10439F",
                             "#C65BCF"
  )) +
  ggtitle("Body Mass Index vs. Stroke")
```




# Task Three: Evaluate and select prediction models

```{r}
sample.split(data$stroke,SplitRatio = 0.8)->split_tag
train<-subset(data,split_tag==TRUE)
test<-subset(data,split_tag==FALSE)
dim(train)
```
```{r}
dim(test)

```


# Task Four: Deploy the prediction model

```{r}

set.seed(123)
rf <- randomForest(formula = stroke~.,data = train)
rf

```

```{r}
plot(rf)
```


```{r}
confusionMatrix(predict(rf,test),test$stroke)
```

# Task Five: Findings and Conclusions
Our model provides more than 95% accuracy.
