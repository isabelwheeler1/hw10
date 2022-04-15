# hw10

---
title: "hw10_Isabel_Wheeler"
author: "Isabel Wheeler"
date: "4/13/2022"
output: html_document
editor_options: 
  chunk_output_type: console
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
accident <- read.csv("https://raw.githubusercontent.com/yumouqiu/DS202-Spring2022/main/Practice/data/fars2016/accident.csv")
person <- read.csv("https://raw.githubusercontent.com/yumouqiu/DS202-Spring2022/main/Practice/data/fars2016/person.csv")
vechicle <- read.csv("https://raw.githubusercontent.com/yumouqiu/DS202-Spring2022/main/Practice/data/fars2016/vehicle.csv")

#are there some days of the week where more accidents happen than on others (use variable DAY_WEEK)?
summary(accident$DAY_WEEK)
accident %>% group_by(DAY_WEEK) %>% tally()
  #accidents seem to happen more on the weekends

#what time of the day do accidents happen (use variable HOUR)?
summary(accident$HOUR) #max is 99 which is a problem
accident.hour <- accident %>% filter(HOUR <= 24) %>% group_by(HOUR) %>% tally()
  #accidents at night happen more often

#what is the number of accidents with at least one drunk driver (use variable DRUNK_DR)?
summary(accident$DRUNK_DR)
accident %>% filter(DRUNK_DR >= 1) %>% group_by(DRUNK_DR) %>% tally()

#Connect to the person table. Identify drivers (PER_TYP == 1, see fars manual ) and subset on them.
#Join accident and driver table (work out which variable(s) to use)
drivers <- subset(person, person$PER_TYP == 1)

combined <- full_join(accident, drivers, by = "ST_CASE")
combined$SEX <- as.factor(combined$SEX)

labeled_sex <- combined$SEX %>%
  str_replace_all('1', "Male") %>%
  str_replace_all('2', "Female")
combined$SEX <- labeled_sex
table(combined$SEX)

#Tally the number of accidents by day of the week (DAY_WEEK), hour of the day (HOUR) and gender (SEX). Visualize the results!
ggplot(data = subset(combined, combined$HOUR.x != 99 & combined$SEX != 8 & combined$SEX != 9), aes(x = HOUR.x, fill = SEX)) + 
  geom_bar() + 
  facet_wrap(~DAY_WEEK) + 
  xlab("Hour of the Day") + 
  ylab("Accidents") + 
  ggtitle("Accidents by Hour and Sex Across Days of the Week")
```
