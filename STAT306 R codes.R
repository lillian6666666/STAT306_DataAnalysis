## R Source

Life.Expectancy.Data.2 <- read.csv("~/Downloads/Life Expectancy Data 2.csv", 
                                   header=FALSE)
Data <- Life.Expectancy.Data.2

## Directly read 
library(tidyverse)
data <- read_csv("https://drive.google.com/uc?export=download&id=1A_Nkqsxh4ymFIJDj7Fbo1SOCoVujhbjb")
data

mean(data$`Life expectancy`, na.rm = TRUE)
median(data$`Life expectancy`, na.rm = TRUE)
sd(data$`Life expectancy`, na.rm = TRUE)
nrow(data$`Life expectancy`, na.rm = TRUE)