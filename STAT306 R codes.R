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
sum(!is.na(data$Schooling))

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

# Fit regression(additive)
model1 <- lm(LifeExpectancy ~ Schooling + Status, data = data)
summary(model1)

# Residuals vs Fitted
plot(model1$fitted.values, model1$residuals,
     xlab="Fitted value", ylab="Residual")
abline(h=0)

# Q-Q plot
qqnorm(resid(model1))
qqline(resid(model1))

# Fit regression with interaction term
model2 <- lm(LifeExpectancy ~ Schooling *Status, data = data)
summary(model2)

# Residuals vs Fitted
plot(model2$fitted.values, model2$residuals,
     xlab="Fitted value", ylab="Residual")
abline(h=0)

# Q-Q plot
qqnorm(resid(model2))
qqline(resid(model2))

