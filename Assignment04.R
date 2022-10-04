library(tidyverse)
library(readxl)
path <- "Happy_Planet.xlsx"
hpi <- read_xlsx(path, sheet = "Sheet1", range="A1:L141")
summary(hpi)
hpi <- subset(hpi, select=-...11) # drop ...4 column
names(hpi) = c('rank', 'country', 'iso', 'continent', 'population', 'life_exp', 'wellbeing', 'footprint', 'hpi', 'biocapacity', 'gdp') # rename columns
class(hpi$gdp) = "double" # set gdp from character to double
ggplot(hpi) + 
  geom_dotplot(aes(x=hpi)) + 
  facet_wrap(vars(continent)) +
  theme(text = element_text(family="Times"))
  mtcars2 <- within(mtcars, {
    vs <- factor(vs, labels = c("V-shaped", "Straight"))
    am <- factor(am, labels = c("Automatic", "Manual"))
    cyl  <- factor(cyl)
    gear <- factor(gear)
  })
p2 <- ggplot(hpi) + 
  geom_dotplot(aes(x=hpi),
                 color='black',
                 fill='salmon',
                 breaks=unique(test$weight)) + 
  labs(
    title = "Happy Planet Index", 
    subtitle = "Year of 2022", 
    caption = "Data of all countries.", 
    tag = "Figure 1", 
    x = "Population", 
    y = "HRI Rank", 
    colour = "GrandBudapest"
  )
p2 + theme_light()
test <- read_xlsx("test-data.xlsx")
summary(test)
mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V-shaped", "Straight"))
  am <- factor(am, labels = c("Automatic", "Manual"))
  cyl  <- factor(cyl)
  gear <- factor(gear)
})
p2 <- ggplot(test) + 
  geom_histogram(aes(x=height),
                color='black',
                fill='salmon',
                breaks=unique(test$weight)) + 
  labs(
    title = "Test Data", 
    subtitle = "Year of 2022", 
    caption = "Data of all countries.", 
    tag = "Figure 1", 
    x = "height", 
    y = "weight", 
    colour = "GrandBudapest"
  )
p1 + theme_light()
# Install
install.packages("wesanderson")
# Load
library(wesanderson)