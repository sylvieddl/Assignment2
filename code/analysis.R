# Code for Assignment 2 - EPI 203
# Author: Sylvie Dobrota Lai
# Date: Apr 27, 2026

library(tidyverse)
library(tableone)

#Load data 
hw2_df <- read_csv("raw-data/cohort.csv")
head(hw2_df)

#Prepare variables
clean_df <- hw2_df %>% 
  mutate(
    smoke=as.factor(smoke),
    female=as.factor(female),
    cardiac=as.factor(cardiac)
  )

#Table 1
myVars<-c("smoke", "female", "age", "cardiac", "cost")
catVars<-c("smoke", "female", "cardiac")

table1<- CreateTableOne(vars=myVars, data=hw2_df, factorVars = catVars)

print(table1, showAllLevels = TRUE)

# Logistic model, cardiac as outcome
model <- glm(cardiac~ cost + age + female + smoke, 
             data=clean_df, 
             family="binomial")

summary(model)
exp(coef(model))

# Distribution of cost by cardiac status
p <- clean_df %>% 
  ggplot( aes(x=cost, fill=cardiac)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_classic() +
  labs(fill="cardiac",
       title="Distribution of cost by cardiac")

p

ggsave("figures/Figure1.png", plot=p)

