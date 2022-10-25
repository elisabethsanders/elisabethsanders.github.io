library(tidyverse)
data_path <- "speed_dating_data.csv"
data <- read.csv(data_path)
summary(data)
data_match = data[data$match == 1,] # get all those who matched

data_bar = subset(data_match, select=c(goal, gender))
data_bar = na.omit(data_bar)

# change variables character type as a categorical variables
class(data_bar$goal) = "character"
class(data_bar$gender) = "character"

ggplot(data_bar) +
  geom_bar(aes(x=goal, fill=gender), position="dodge") +
  scale_x_discrete(breaks=c(1, 2, 3, 4, 5, 6),
                   labels=c("Seemed Fun", "Meet New People",
                            "Get a Date", "Serious Relationship",
                            "To Say I Did It", "Other")) +
  scale_fill_discrete(labels=c("Female", "Male"), type=c("#7b3494", "#008837")) +
  labs(title = "Goal of Speed Dating",
       subtitle = "Limited to Participants that Matched",
       fill = "Gender") +
  xlab("Goal") +
  ylab("Number of Participants") +
  theme_light() +
  theme(text = element_text(family="Avenir")) +
  coord_flip()

data_col = subset(data, select=c(goal, match, gender))
data_col = na.omit(data_col)

# change variables to character type as a categorical variables
class(data_col$goal) = "character"
class(data_col$gender) = "character"

data_col = subset(data, select=c(goal, match, gender))
data_col = na.omit(data_col)

# change variables to character type as a categorical variables
class(data_col$goal) = "character"
class(data_col$gender) = "character"

ggplot(data_col) +
  geom_col(aes(x=goal, y=match, fill=gender)) +
  scale_x_discrete(breaks=c(1, 2, 3, 4, 5, 6),
                   labels=c("Seemed Fun", "Meet New People",
                            "Get a Date", "Serious Relationship",
                            "To Say I Did It", "Other")) +
  scale_fill_discrete(labels=c("Female", "Male"), type=c("#7b3494", "#008837")) +
  labs(title = "Goal of Speed Dating and Number of Matches",
       fill = "Gender") +
  xlab("Goal") +
  ylab("Total Number of Matches") +
  theme_light() +
  theme(text = element_text(family="Avenir"),
        axis.text.x = element_text(angle=10, vjust = 0.75))

data_cir = subset(data, select=c(shar1_1, shar1_s, shar7_2, shar1_2, match))
data_cir = na.omit(data_cir)

data_cir = data.frame(time=c(1, 2, 3, 4,
                             1, 2, 3, 4)
                      ,match=c(0, 0, 0, 0,
                               1, 1, 1, 1)
                      ,shar_imp=c(mean(data_cir[!is.na(data_cir$shar1_1) & data_cir$match == 0,]$shar1_1),
                                  mean(data_cir[!is.na(data_cir$shar1_s) & data_cir$match == 0,]$shar1_s),
                                  mean(data_cir[!is.na(data_cir$shar7_2) & data_cir$match == 0,]$shar7_2),
                                  mean(data_cir[!is.na(data_cir$shar1_2) & data_cir$match == 0,]$shar1_2),
                                  mean(data_cir[!is.na(data_cir$shar1_1) & data_cir$match == 1,]$shar1_1),
                                  mean(data_cir[!is.na(data_cir$shar1_s) & data_cir$match == 1,]$shar1_s),
                                  mean(data_cir[!is.na(data_cir$shar7_2) & data_cir$match == 1,]$shar7_2),
                                  mean(data_cir[!is.na(data_cir$shar1_2) & data_cir$match == 1,]$shar1_2)))

# change match to character type as a categorical variable
class(data_cir$match) = "character"

ggplot(data_cir, aes(x=time, y=shar_imp, group=match, fill=match)) +
  geom_area() +
  coord_polar() +
  scale_fill_discrete(labels=c("Match", "No Match"), type=c("#7b3494", "#008837")) +
  scale_x_continuous(labels=c("Before", "During", "Days After", "Weeks After")) +
  labs(title = "Rated Imporatance of Shared Interests over Time",
       fill = "Match") +
  xlab("Time Surveyed") +
  ylab("Importance of Shared Interests") +
  theme_light() +
  theme(text = element_text(family="Avenir"))