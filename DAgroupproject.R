library(dplyr)
library(ggplot2)
library(readr)

data <- read_csv("DAProject7 (1).csv")
head(data)
data$Obese <- ifelse(data$Obese =="Yes", 1, 0)

data$sex <- as.factor(data$Sex)
data$employment <- as.factor(data$Employment)
data$fruit <- as.factor(data$Fruit)
data$veg <- as.factor(data$Veg)


model <- glm(Obese ~ Year + AgeGroup + Sex + Employment + Fruit + Veg, 
             data = data, 
             family = binomial)


summary(model)

plot(model)

predictions <- predict(model, type = "response")


data$predicted_prob <- predictions


head(data$predicted_prob)

AIC(model)

stepwise_model <- stepAIC(model, direction = "both", trace = 1)
summary(stepwise_model)

odds_ratios <- exp(coef(stepwise_model))
conf_intervals <- exp(confint(stepwise_model))

odds_ratios
conf_intervals

model_summary <- summary(stepwise_model)
p_values <- model_summary$coefficients[, 4]
p_values


odds_ratios_df <- data.frame(
  Variable = names(odds_ratios),
  OR = odds_ratios,
  LowerCI = conf_intervals[, 1],
  UpperCI = conf_intervals[, 2]
)

odds_ratios_df <- odds_ratios_df %>% filter(Variable != "(Intercept)")


summary <- 


ggplot(odds_ratios_df, aes(x = Variable, y = OR, ymin = LowerCI, ymax = UpperCI)) +
  geom_pointrange() +  # Points with lines for the CIs
  geom_hline(yintercept = 1, linetype = "dashed", color = "pink") +  # Reference line at OR = 1
  coord_flip() +  # Flip the axes for better readability
  labs(x = "Variable", y = "Odds Ratio (95% CI)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12))