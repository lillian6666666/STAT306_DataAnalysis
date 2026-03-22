library(tidyverse)
library(leaps)
library(car)
## Directly read
data <- read_csv("https://drive.google.com/uc?export=download&id=1A_Nkqsxh4ymFIJDj7Fbo1SOCoVujhbjb")

data <- data %>%
  rename(
    LifeExpectancy = "Life expectancy",
  ) %>%
  filter(Year == 2015)
data_sel <- data %>%
  select(-Year, -Country, -Alcohol, -"Total expenditure") %>%
  na.omit()

data_sel
nrow(data_sel)

forward_model<-regsubsets(LifeExpectancy ~ ., data=data_sel, method="forward")
ss = summary(forward_model)
summary(forward_model)
matrics = data.frame(
  R2 = ss$rsq,
  AdjR2 = ss$adjr2,
  Cp = ss$cp
)

matrics
#backward selection
backward_model <- regsubsets(LifeExpectancy ~ ., data=data_sel, method="backward")

ss2=summary(backward_model)
ss2
metrics_backward = data.frame(
  R2 = ss2$rsq,
  AdjR2 = ss2$adjr2,
  Cp = ss2$cp
)
metrics_backward

model <- regsubsets(LifeExpectancy ~ ., data=data_sel, method="exhaustive")
summary(model)
ss3=summary(model)
metrics1 = data.frame(
  R2 = ss3$rsq,
  AdjR2 = ss3$adjr2,
  Cp = ss3$cp
)
metrics1

mean(data$LifeExpectancy, na.rm = TRUE)
median(data$LifeExpectancy, na.rm = TRUE)
sd(data$LifeExpectancy, na.rm = TRUE)
sum(!is.na(data$Schooling))

data|> ggplot(aes(x=Schooling, y= LifeExpectancy, color=Status))+geom_point()
data$Status <- as.factor(data$Status)

# Fit regression
model1 <- lm(LifeExpectancy ~ `Income composition of resources`+ `Adult Mortality`+ `Hepatitis B` + Schooling + Status, data = data_sel)
summary(model1)

# Residuals vs Fitted
plot(model1$fitted.values, model1$residuals,
     main = "Model 1: Residuals vs Fitted (Additive Model)",
     xlab="Fitted value", ylab="Residual")
abline(h=0)

# Q-Q plot
qqnorm(resid(model1),
       main = "Model 1: Normal Q-Q Plot (Additive Model)")
qqline(resid(model1))

#VIF
vif(model1)

# Fit regression with interaction term(schooling and status)
model2 <- lm(LifeExpectancy ~ `Income composition of resources` + `Adult Mortality` + `Hepatitis B` + Schooling *Status, data = data_sel)
summary(model2)

# Residuals vs Fitted
plot(model2$fitted.values, model2$residuals,
     main = "Model 2: Residuals vs Fitted (Interaction Model)",
     xlab="Fitted value", ylab="Residual")
abline(h=0)

# Q-Q plot
qqnorm(resid(model2),main = "Model 2: Normal Q-Q Plot (Interaction Model)")
qqline(resid(model2))

#VIF

vif(model2)