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
nrow(data$`Life expectancy`)

# rename variable for easier use
data <- data %>%
  rename(
    LifeExpectancy = "Life expectancy",
  )

data<- data|> select(-Country)|> filter(Year==2015)
data|> ggplot(aes(x=Schooling, y= LifeExpectancy, color=Status))+geom_point()
# reg <-lm(Life_Expectancy ~ Schooling *Status, data)
# summary(reg)
# qqplot(reg)
# resid.plot(reg)
data$Status <- as.factor(data$Status)

# Fit regression
model <- lm(LifeExpectancy ~ Schooling *Status, data = data)
summary(model)

# Residuals vs Fitted
plot(model$fitted.values, model$residuals,
     xlab="Fitted value", ylab="Residual")

# Q-Q plot
qqnorm(resid(model))
qqline(resid(model))
