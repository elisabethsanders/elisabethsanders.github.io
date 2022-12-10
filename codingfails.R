library(ggplot2)
library(dplyr)
library(tidyr)
install.packages("viridis")
library(viridis)
library(ggplot2)
df = read.csv("speed_dating_data.csv", header= TRUE)
library(tidyverse)
data_path <- "speed_dating_data.csv"
data <- read.csv(data_path)
xx = ggplot(df, aes(x = match, y = race)) + 
  geom_point(size = 6) + 
  labs(y = "race", x = "match") + 
  theme( axis.text.x = element_text(face = "bold",colour = "pink", size = 12), 
         axis.text.y = element_text(face = "bold", size = 11, colour = "blue"), 
         axis.title= element_text(face = "bold", size = 14, colour = "purple"), 
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"))
         
xx 
library(ggplot2)

# create bubble chart
ggplot(df, aes(x = age, y = exercise, size=attr, color='steelblue')) +
  geom_point(alpha=0.5) +
  scale_size(range=c(-.0155,2), name='Attractiveness by Exercise')

xx = ggplot(df, aes(x = match, y = race)) + 
  geom_point(aes(colour = Latitude), size = 4) + 
  labs(y = "race", x = "match") + 
  theme( axis.text.x = element_text(face = "bold",colour = "black", size = 12), 
         axis.text.y = element_text(face = "bold", size = 11, colour = "black"), 
         axis.title= element_text(face = "bold", size = 14, colour = "black"), 
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"), 
         legend.title = element_text(size =12, face = "bold", colour = "black"),
         legend.text = element_text(size = 10, face = "bold", colour = "black")) +
  scale_colour_continuous(high = "navy", low = "salmon")

xx

xx = ggplot(df, aes(x = match, y = exercise)) + 
     geom_point(size = 6) + 
     labs(y = "exercise", x = "match") + 
     theme( axis.text.x = element_text(face = "bold",colour = "pink", size = 12), 
                       axis.text.y = element_text(face = "bold", size = 11, colour = "blue"), 
                       axis.title= element_text(face = "bold", size = 14, colour = "purple"), 
                       panel.background = element_blank(), 
                       panel.border = element_rect(fill = NA, colour = "black"))
xx 

library(ggplot2)

# create data
match <- 1:10
race <- cumsum(rnorm(10))
data <- data.frame(match,race)

# Plot
ggplot(df, aes(x=age, y=exercise)) +
  geom_line( color="#69b3a2", size=2, alpha=0.9, linetype=2) +
  ggtitle("Will attractiveness be impacted by amount of exercise?")

library(ggplot2)
library(dplyr) # easier data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
library(Interpol.T) #  will generate a large dataset on initial load
library(lubridate) # for easy date manipulation
library(ggExtra) # because remembering ggplot theme options is beyond me
library(tidyr)

library("Interpol.T") 
pal <-choose_palette()
display.brewer.all()

data <- data(speed_dating_data)

names(h_d_t)[1:5]<- c("stationid","date","hour","temp","flag")
df <- tbl_df(h_d_t) %>%
  filter(stationid =="T0001")

df <- df %>% mutate(year = year(date),
                    month = month(date, label=TRUE),
                    day = day(date))

install.packages("reshape")                                       
library("reshape") 

data_melt <- melt(data)                                           # Reorder data
head(data_melt) 

install.packages("ggplot2")                                       # Install ggplot2 package
library("ggplot2")  

ggp <- ggplot(data_melt, aes(match, race)) +                           # Create heatmap with ggplot2
  geom_tile(aes(fill = attr))
ggp     