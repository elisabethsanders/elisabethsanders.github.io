library(ggplot2)
library(tidyverse)

data("speed_dating_data")
ggplot(data = speed_dating_data, aes(x = attr_o, y = age)) + geom_point() + 
  geom_smooth(formula = y ~ poly(x,2), method = "lm", se = T, level = 0.9) +
  scale_color_manual(values=c('Yellow','Green'))

library("ggplot2")

df<-read.csv("speed_dating_data.csv")

ggplot(df,aes(x=attr_o,y=age,group=match))+
  geom_point(aes(color=match))

data("speed_dating_data")
df<-read.csv("speed_dating_data.csv")
plot(df$race,df$attr_o,pch=16) 



library("ggplot2")

df<-read.csv("speed_dating_data.csv")



library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)

library("speed_dating_data.csv")
data <- speed_dating_data %>% 
  
ggplot(data = speed_dating_data, aes(x = race, y = attr_o, size = match())) +
  geom_point(alpha=0.7)

library(ggplot2)
theme_set(
  theme_bw() + 
    theme(legend.position = "top")
)

install.packages("ggplot2")                                  
library("ggplot2")

data("speed_dating_data")
df <- speed_dating_data

df$attr_o <- as.factor(df$attr_o)
head(df[, c("race", "match", "attr_o", "like_o")], 4)

ggplot(speed_dating_data, aes(x = race, y = attr_o)) + 
  geom_point(aes(color = as.factor(df$attr_o), size = match), alpha = 10) +
  scale_color_manual(values = c("#af002a", "#ff7e00", "#ffbf00", "#3b7a57", "#34b334", "#3b3b6d", "#003a6c", "#9966cc", "#431c53", "#551b8c", "#915c83", "#f4c2c2", "#e85395", "#f7238a", "#9f8170", "#98777b", "#848482", "#352925")) +
  scale_size(range = c(0.5, 1.75)) +

Races <- c("Black/African American", "European/Caucasian", "Latino/Hispanic-American", "Asian/Pacific Islander/Asian-American", "Native American", "Other")+
  scale_x_discrete(labels = Races)