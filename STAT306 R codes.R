
# STAT 306 Project: Life Expectancy Analysis
# Authors: Ruiting Jiang, Mark Cao, Lillian Chen, Jiayi Lyu
# Date: March 23, 2025

library(tidyverse)
library(leaps)
library(car)
library(knitr)
library(broom)

options(knitr.kable.NA = "")



# 1. DATA LOADING AND CLEANING

data <- read_csv("https://drive.google.com/uc?export=download&id=1A_Nkqsxh4ymFIJDj7Fbo1SOCoVujhbjb")

data <- data %>%
  rename(LifeExpectancy = `Life expectancy`) %>%
  filter(Year == 2015)

data_sel <- data %>%
  select(-Year, -Country, -Alcohol, -`Total expenditure`) %>%
  na.omit()

data_sel$Status <- as.factor(data_sel$Status)



# 2. PRELIMINARY MODELS

model1 <- lm(LifeExpectancy ~ Schooling + Status, data = data_sel)
model2 <- lm(LifeExpectancy ~ Schooling * Status, data = data_sel)



# 3. VARIABLE SELECTION

forward_model  <- regsubsets(LifeExpectancy ~ ., data = data_sel, method = "forward")
backward_model <- regsubsets(LifeExpectancy ~ ., data = data_sel, method = "backward")
best_model     <- regsubsets(LifeExpectancy ~ ., data = data_sel, method = "exhaustive")

ss_e <- summary(best_model)



# 4. FINAL MODELS WITH SELECTED CONTROLS

model3 <- lm(LifeExpectancy ~ `Income composition of resources` +
               `Adult Mortality` + `Hepatitis B` + Schooling + Status,
             data = data_sel)

model4 <- lm(LifeExpectancy ~ `Income composition of resources` +
               `Adult Mortality` + `Hepatitis B` + Schooling * Status,
             data = data_sel)



# 5. TABLES

sample_summary <- tibble(
  Statistic = c("Sample size", "Mean life expectancy", "Median life expectancy",
                "SD of life expectancy", "Mean schooling", "SD of schooling"),
  Value = c(
    nrow(data_sel),
    round(mean(data_sel$LifeExpectancy), 2),
    round(median(data_sel$LifeExpectancy), 2),
    round(sd(data_sel$LifeExpectancy), 2),
    round(mean(data_sel$Schooling), 2),
    round(sd(data_sel$Schooling), 2)
  )
)
kable(sample_summary, caption = "Summary statistics for the analysis sample")

cp_table <- tibble(
  `Number of predictors` = 1:length(ss_e$cp),
  `Adjusted R^2` = round(ss_e$adjr2, 4),
  `Cp` = round(ss_e$cp, 4),
  `Target p = k + 1` = 2:(length(ss_e$cp) + 1)
)
kable(cp_table, caption = "Exhaustive subset selection results")

coef_m1 <- tidy(model1) %>% mutate(across(where(is.numeric), ~ round(.x, 4)))
coef_m2 <- tidy(model2) %>% mutate(across(where(is.numeric), ~ round(.x, 4)))
coef_m3 <- tidy(model3) %>% mutate(across(where(is.numeric), ~ round(.x, 4)))
coef_m4 <- tidy(model4) %>% mutate(across(where(is.numeric), ~ round(.x, 4)))

kable(coef_m1, caption = "Model 1 coefficient estimates")
kable(coef_m2, caption = "Model 2 coefficient estimates")
kable(coef_m3, caption = "Model 3 coefficient estimates")
kable(coef_m4, caption = "Model 4 coefficient estimates")

vif_m3 <- tibble(
  Variable = names(vif(model3)),
  VIF = round(as.numeric(vif(model3)), 3)
)
kable(vif_m3, caption = "VIF values for the final model")



# 6. FIGURES

# Figure 1: Boxplot — Life Expectancy by Development Status
ggplot(data_sel, aes(x = Status, y = LifeExpectancy, fill = Status)) +
  geom_boxplot() +
  labs(
    title = "Life Expectancy by Development Status",
    x = "Development Status",
    y = "Life Expectancy (years)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Figure 2: Scatterplot — Life Expectancy vs Schooling by Status
ggplot(data_sel, aes(x = Schooling, y = LifeExpectancy, color = Status)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Life Expectancy vs Schooling by Development Status",
    x = "Schooling (years)",
    y = "Life Expectancy (years)"
  ) +
  theme_minimal()

# Figure 3: Diagnostics for Model 3
par(mfrow = c(1, 2))
plot(model3$fitted.values, model3$residuals,
     main = "Model 3: Residuals vs Fitted",
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

qqnorm(resid(model3), main = "Model 3: Normal Q-Q Plot")
qqline(resid(model3), col = "red", lty = 2)
par(mfrow = c(1, 1))