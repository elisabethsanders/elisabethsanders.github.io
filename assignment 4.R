library(tidyverse)
library(readxl)
path <- "Happy_Planet.xlsx"
hpi <- read_xlsx(path, sheet = "Sheet1", range="A1:J153")
summary(hpi)
hpi <- subset(hpi, select=-...4) # drop ...4 column
names(hpi) = c('rank', 'country', 'iso', 'continent', 'population', 'life_exp', 'wellbeing', 'footprint', 'hpi', 'biocapacity', 'gdp') # rename columns
ggplot(hpi) + geom_dotplot(aes(x=hpi)) + facet_wrap(vars(continent))