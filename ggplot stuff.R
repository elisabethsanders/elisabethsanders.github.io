tidyverse::tidyverse_packages() "ggplot"
library(tidyverse)
ggplot(data = <DATA>, mapping = aes(<MAPPINGS>)) +  <GEOM_FUNCTION>()
ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length)) + geom_point()