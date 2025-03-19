---
title: "Data Analysis: Obesity Prevalence"
number-sections: true
format:
  html:
    embed-resources: true
    code-tools: true
execute: 
  echo: false
  eval: true
  warning: false
  message: false
editor_options: 
  chunk_output_type: console
---

```{r}
#| echo: false
#| warning: false
#| message: false

library(ggplot2)
library(corrplot)
library(tidyverse)
library(patchwork)
library(gridExtra)
library(knitr)
library(kableExtra)
library(dplyr)
library(broom)
library(MASS)
library(gt)
library(caret)
library(ResourceSelection)
library(pROC)
library(car)
library(performance)
```

# Introduction {#sec-intro}

Obesity is a significant public health concern, influenced by various socio-economic and lifestyle factors. This analysis uses data from the 2008-2012 Scottish Health Surveys to explore the relationship between demographic variables, lifestyle habits, and obesity prevalence in Scotland. The dataset includes information on age, gender, employment, dietary habits, and obesity status.

The objective is to examine trends in obesity prevalence over the survey years and identify differences in obesity rates based on socio-economic and lifestyle factors. The findings will inform public health strategies aimed at addressing obesity.

@sec-eda provides an exploratory analysis of the dataset, including trends in obesity across different groups. @sec-fda presents the results of a regression analysis, including model diagnostics to assess the model's fit and assumptions. Concluding remarks are in @sec-con.

# Exploratory data analysis {#sec-eda}

## Distribution of each variable

@fig-variable-distribution illustrates the distribution of key categorical variables in the dataset, including Age Group, Employment Status, Vegetable and Fruit Consumption, and Sex. Age Group (AgeGroup) distribution indicates that middle-aged individuals (35-64 years) constitute the largest proportion, while younger (16-24 years) and older (75+ years) groups have lower representation, with females slightly outnumbering males across most age groups. Employment Status (Employment) shows that females have a higher proportion in the employed category but also dominate in "looking after home/family" and "permanently unable to work" categories, reflecting gender differences in social roles. Vegetable (Veg) and Fruit (Fruit) consumption data reveal that most individuals report regular intake, with females exhibiting a higher proportion of consumption compared to males, potentially indicating gender-related differences in health behaviors. Sex (Sex) distribution suggests a slightly higher number of females than males in the dataset, though the overall distribution remains relatively balanced.

```{r}
#| echo: false
#| label: fig-variable-distribution
#| fig-cap: Distribution of Age Group, Employment Status, Vegetable Consumption, Fruit Consumption, and Sex by Gender in the Dataset.
#| fig-align: center
#| fig-width: 8
#| fig-height: 6

df <- read.csv("D:/HuaweiMoveData/Users/13329/Desktop/DAProject7.csv", stringsAsFactors = TRUE)
data <- df

data <- data %>%
  mutate(Employment = case_when(
    Employment == "In paid employment, self-employed or on gov't training" ~ "Employed",
    Employment == "Looking for/intending to look for paid work" ~ "Unemployed",
    TRUE ~ Employment
  ))
p1 <- ggplot(data, aes(x = AgeGroup, fill = Sex)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p2 <- ggplot(data, aes(x = Sex, fill = Sex)) +
  geom_bar() +
  scale_fill_manual(values = c("Female" = "lightcoral", "Male" = "cyan3")) + 
  theme_minimal()

p3 <- ggplot(data, aes(x = Employment, fill = Sex)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p4 <- ggplot(data, aes(x = Veg, fill = Sex)) +
  geom_bar(position = "dodge") +
  theme_minimal()

p5 <- ggplot(data, aes(x = Fruit, fill = Sex)) +
  geom_bar(position = "dodge") +
  theme_minimal()

grid.arrange(
  p1, p2, p3, p4, p5,    
  layout_matrix = rbind(c(1, 2, 3), 
                        c(4, 5, 3)))
```

## Obesity changes over time (2008-2012)

@tbl-yearly-obesity-rate presents the yearly obesity rates in Scotland between 2008 and 2012. Overall, the obesity rate dropped to its lowest point in 2009 (28.79%), and then rose sharply in 2010, reaching its highest point in five years (30.47%). After that, the obesity rate fell back and stabilized in 2011-2012. The data shows that there were some fluctuations in the obesity rate during this period, but it generally remained between 29% and 30.5%.

```{r}
#| echo: false
#| label: tbl-yearly-obesity-rate
#| tbl-cap: Yearly Obesity Rate in Scotland from 2008 to 2012.

yearly_rate <- data %>%
  group_by(Year) %>%
  summarise(ObesityRate = mean(Obese == "Yes") * 100)

yearly_rate %>%
  gt() %>%
  tab_header(title = "Yearly Obesity Rate (2008-2012)") %>%
  fmt_number(columns = ObesityRate, decimals = 2)
```

## Obesity trend over time (2008-2012)

@fig-yearly-obesity shows the yearly trend of obesity rate in Scotland from 2008 to 2012. The trend shows that after the obesity rate fell in 2009, it rose significantly in 2010, and then gradually fell back and stabilized. The fluctuation in 2010 may reflect the increase in health risk factors that year, while the slowdown after 2011 may be related to health intervention measures and the improvement of awareness of physiological health management. Overall, the obesity rate shows periodic fluctuations but remains at a relatively high level.

```{r}
#| echo: false
#| fig-cap: Yearly Obesity Rate Trend in Scotland (2008-2012)
#| label: fig-yearly-obesity
#| fig-align: center
#| fig-width: 6
#| fig-height: 4

ggplot(yearly_rate, aes(x = Year, y = ObesityRate)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(title = "Yearly Obesity Rate Trend (2008-2012)",
       y = "Obesity Rate (%)", x = "Year") +
  theme_minimal()

```

## Group-wise Obesity Rate Analysis

In this section, we explore the obesity rates by various grouping variables (Age Group, Employment, Vegetable Intake, and Fruit Intake) stratified by Sex. The obesity rates are counted and generates @fig-group-wise.

```{r}
#| warning: false
#| message: false
#| label: fig-group-wise
#| fig-cap: Obesity Rates by Demographic and Lifestyle Factors.
#| fig-align: center

# Create a function to calculate obesity rates
calculate_obesity_rates <- function(data, group_var, group_name) {
  grouped_data <- data %>%
    group_by(!!sym(group_var), Sex) %>%
    summarise(ObesityRate = mean(Obese == "Yes") * 100, .groups = "drop")
  return(grouped_data)
}

# Calculate obesity rates for each grouping variable
grouped_age        <- calculate_obesity_rates(df, "AgeGroup",     "Age Group")
grouped_employment <- calculate_obesity_rates(df, "Employment",   "Employment")
grouped_veg        <- calculate_obesity_rates(df, "Veg",          "Vegetable Intake")
grouped_fruit      <- calculate_obesity_rates(df, "Fruit",        "Fruit Intake")

#=== Plot 1: Age Group ===#
p1 <- ggplot(grouped_age, aes(x = AgeGroup, y = ObesityRate, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  theme_minimal() +
  labs(y = "Obesity Rate (%)", x = "Age Group") +
  scale_fill_manual(values = c("Female" = "#FF4136", "Male" = "#0074D9")) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "right",
    plot.title   = element_text(size = 11, face = "bold"),
    axis.title   = element_text(size = 10)
  ) +
  ggtitle("(1) by Age Group and Sex") +
  ylim(0, 45)

#=== Plot 2: Employment (flip to avoid crowded x-axis) ===#
p2 <- ggplot(grouped_employment, aes(x = Employment, y = ObesityRate, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  coord_flip() +  # Flip coordinates to avoid a crowded x-axis
  theme_minimal() +
  labs(y = "Obesity Rate (%)", x = "Employment") +
  scale_fill_manual(values = c("Female" = "#FF4136", "Male" = "#0074D9")) +
  theme(
    axis.text.x  = element_text(size = 9),
    axis.text.y  = element_text(size = 9),
    legend.position = "right",
    plot.title   = element_text(size = 11, face = "bold"),
    axis.title   = element_text(size = 10)
  ) +
  ggtitle("(2) by Employment") +
  ylim(0, 45)

#=== Plot 3: Vegetable Intake ===#
p3 <- ggplot(grouped_veg, aes(x = Veg, y = ObesityRate, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  theme_minimal() +
  labs(y = "Obesity Rate (%)", x = "Vegetable Intake") +
  scale_fill_manual(values = c("Female" = "#FF4136", "Male" = "#0074D9")) +
  theme(
    axis.text.x  = element_text(size = 9),
    legend.position = "right",
    plot.title   = element_text(size = 11, face = "bold"),
    axis.title   = element_text(size = 10)
  ) +
  ggtitle("(3) by Vegetable Intake and Sex") +
  ylim(0, 45)

#=== Plot 4: Fruit Intake ===#
p4 <- ggplot(grouped_fruit, aes(x = Fruit, y = ObesityRate, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  theme_minimal() +
  labs(y = "Obesity Rate (%)", x = "Fruit Intake") +
  scale_fill_manual(values = c("Female" = "#FF4136", "Male" = "#0074D9")) +
  theme(
    axis.text.x  = element_text(size = 9),
    legend.position = "right",
    plot.title   = element_text(size = 11, face = "bold"),
    axis.title   = element_text(size = 10)
  ) +
  ggtitle("(4) by Fruit Intake and Sex") +
  ylim(0, 45)

#=== Combine plots ===#
# Use patchwork to arrange p1 and p2 in the top row, and p3 and p4 in the bottom row
combined_plot <- (p1 + p2) / (p3 + p4) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

# Add a main title to the entire plot
combined_plot <- combined_plot + plot_annotation(
  theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
)

#=== Adjust plot size and output ===#
options(repr.plot.width = 12, repr.plot.height = 8)
combined_plot

```

1.  Obesity Rate by Age Group and Sex:Obesity rates increase with age, peaking at 55-64 years (36.9% males, 35.4% females), before declining in older adults. Gender differences are evident in younger age groups, with females showing higher rates, but the trend reverses slightly in middle age, indicating that metabolic and lifestyle changes play a role.

2.  Obesity Rate by Employment Status and Sex: Obesity prevalence varies by employment status, highest among those permanently unable to work (41.9% females, 36.2% males) and lowest among students (13.9% females, 8.6% males). Gender differences appear in caregiving roles and job-seeking categories, emphasizing the link between socioeconomic conditions and obesity.

3.  Obesity Rate by Vegetable Intake and Sex: Higher vegetable intake is associated with lower obesity rates for both sexes, with a stronger effect in females (from 33.4% to 28.9%). This suggests a protective role of vegetable consumption against obesity, reinforcing the importance of dietary habits in obesity prevention.

4.  Obesity Rate by Fruit Intake and Sex:Unlike vegetables, fruit intake shows minimal association with obesity, as obesity rates remain nearly unchanged regardless of fruit consumption (29.9% vs. 29.8% for females, 28.9% vs. 29.6% for males). This highlights potential differences in how various dietary components impact obesity.

Age shows the most consistent pattern, with employment status exhibiting the widest variation, particularly for students and those unable to work. Vegetable intake correlates with lower obesity rates, whereas fruit intake shows little impact. Gender differences vary across factors, illustrating the complexity of obesity risk.

## Variable Relationships

In this section, we perform chi-square tests to assess the association between categorical variables and obesity status, and then compute a correlation matrix (and its heatmap) on a numeric copy of the data.

### Chi-square Tests

```{r}
# Define categorical variables
cat_vars <- c("AgeGroup", "Sex", "Employment", "Veg", "Fruit")
chi_results <- list()

# Perform chi-square tests comparing each categorical variable with Obese status
for (var in cat_vars) {
  chi_results[[var]] <- chisq.test(table(df[[var]], df$Obese))
}

# Extract and print p-values from the chi-square tests
chi_p_values <- sapply(chi_results, function(x) x$p.value)
print(chi_p_values)

```

Age group, employment status, and vegetable intake show significant associations with obesity, while sex and fruit intake do not reach statistical significance. These findings validate previous observations and highlight key influencing factors.

### Correlation Analysis

For the correlation analysis, we create a numeric copy of the data to convert categorical variables into numerical codes.

```{r}
#| label: fig-heatmap
#| fig-cap: Correlation Heatmap.
#| fig-align: center

# Create a numeric copy (do NOT overwrite the original df)
df_num <- df

# Convert categorical variables to numeric
df_num$Obese      <- as.numeric(df_num$Obese == "Yes")  # 1 if Yes, 0 if No
df_num$Veg        <- as.numeric(df_num$Veg == "Yes")
df_num$Fruit      <- as.numeric(df_num$Fruit == "Yes")
df_num$Sex        <- as.numeric(df_num$Sex == "Female")   # 1 if Female, 0 if Male

# Convert factor variables to numeric codes
df_num$AgeGroup   <- as.numeric(as.factor(df_num$AgeGroup))
df_num$Employment <- as.numeric(as.factor(df_num$Employment))

# Subset columns for correlation analysis
df_cor <- df_num[, c("AgeGroup", "Sex", "Employment", "Veg", "Fruit", "Obese")]

# Remove columns with zero variance (only 1 unique value)
non_constant_cols <- sapply(df_cor, function(x) length(unique(x)) > 1)
df_cor <- df_cor[, non_constant_cols]

# Calculate the correlation matrix
cor_matrix <- cor(df_cor, use = "pairwise.complete.obs")
print(cor_matrix)

# Plot the correlation heatmap
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  col = colorRampPalette(c("steelblue", "white", "firebrick"))(200),
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 0.8,
  diag = FALSE,
  mar = c(0, 0, 2, 0)
)

```

In @fig-heatmap, age shows the strongest correlation with obesity (r = 0.10), followed by employment status (r = 0.06), while vegetable intake is negatively correlated (r = -0.03). Sex and fruit intake exhibit near-zero correlations, aligning with chi-square results, reinforcing their weaker influence.

### Potential Influencing factors

```{r}
#| label: fig-chi-square
#| fig-cap: Chi-square Test P-values for Association with Obesity.
#| fig-align: center

# Analyzing the significant factors from chi-square tests and correlation analysis

# Create a data frame of chi-square p-values for visualization
chi_results_df <- data.frame(
  Variable = names(chi_p_values),
  P_Value = chi_p_values,
  Significant = chi_p_values < 0.05
)

ggplot(chi_results_df, aes(x = reorder(Variable, P_Value), y = P_Value, fill = Significant)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "firebrick") +
  coord_flip() +
  scale_fill_manual(values = c("grey", "steelblue")) +
  theme_minimal() +
  labs(
    x = "Variable",
    y = "P-value",
    caption = "Red dashed line indicates significance level (0.05)"
  )
```

```{r}
#| label: fig-correlations
#| fig-cap: Correlations with Obesity Status.
#| fig-align: center

# Identify top correlations with obesity
obesity_correlations <- data.frame(
  Variable = colnames(cor_matrix),
  Correlation = cor_matrix[, "Obese"]
) %>%
  filter(Variable != "Obese") %>%
  arrange(desc(abs(Correlation)))

ggplot(obesity_correlations, aes(x = reorder(Variable, abs(Correlation)), 
                                y = Correlation, 
                                fill = Correlation > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("firebrick", "steelblue"), 
                    labels = c("Negative", "Positive")) +
  theme_minimal() +
  labs(
    x = "Variable",
    y = "Correlation Coefficient",
    fill = "Direction"
  )
```

Based on the @fig-chi-square and @fig-correlations, the following factors appear to have the strongest association with obesity status:

```{r}
# Print the significant factors from chi-square tests
sig_chi <- chi_results_df %>% filter(Significant == TRUE) %>% arrange(P_Value)
cat("Significant factors (chi-square):", paste(sig_chi$Variable, collapse = ", "), "\n")

# Print the top correlating factors
top_corr <- obesity_correlations %>% filter(abs(Correlation) > 0.05) %>% arrange(desc(abs(Correlation)))
cat("Top correlating factors:", paste(top_corr$Variable, collapse = ", "), "\n")

```

Age emerges as the most critical factor in obesity risk, followed by employment status and vegetable intake, while sex and fruit intake show weaker associations. The interaction between age and employment suggests socioeconomic factors play a key role in obesity prevalence.

# Formal Data Analysis {#sec-fda}

## Variable Selection & Regression Modelling

In this analysis, the primary objective is to understand the relationship between various socio-economic and lifestyle factors and the likelihood of an individual being obese. We will employ a logistic regression model where the response variable is Obese (Yes/No), and the predictors include Year, AgeGroup, Sex, Employment, Veg, and Fruit.

We start by fitting the full logistic regression model containing all explanatory variables, which can be written as:

$$
\begin{aligned}
y_i &= \beta_0 + \beta_1 \cdot x_{1i} + \beta_2 \cdot x_{2i} + \beta_3 \cdot x_{3i} + \beta_4 \cdot x_{4i} + \beta_5 \cdot x_{5i} + \beta_6 \cdot x_{6i} \\
    &= \beta_0 + \beta_{\text{Year}} \cdot \text{Year} + \beta_{\text{AgeGroup}} \cdot \text{AgeGroup} + \beta_{\text{Sex}} \cdot \text{Sex}+ \beta_{\text{Employment}} \cdot \text{Employment} + \beta_{\text{Fruit}} \cdot \text{Fruit} + \beta_{\text{Veg}} \cdot \text{Veg}
\end{aligned}
$$

where

-   $y_i$ denotes the log-odds of obesity for individual $i$,

-   $\beta_0$ is the intercept, representing the baseline log-odds of obesity,

-   $\beta_{\text{Year}}$,$\beta_{\text{AgeGroup}}$, $\beta_{\text{Sex}}$, $\beta_{\text{Employment}}$,$\beta_{\text{Fruit}}$ and $\beta_{\text{Veg}}$ are the regression coefficients for the corresponding explanatory variables, indicating the change in log-odds of obesity for each unit change in the respective variable.

Stepwise regression with backward selection will be used to determine whether the full model can be reduced based on the Akaike information criterion (AIC). Hence, the model which results in the lowest AIC will result in the final model fitted to the data. Based on @tbl-aic-summary, we can conclude the stepwise Logistic Regression model, with an AIC of 30078.14 is a better fit than the full model, with AIC of 30080.98. The Stepwise function has retained variables of AgeGroup, Sex, Employment and Veg, so the only removed variables are fruit and year.

```{r}
#| echo: false
#| label: tbl-aic-summary
#| tbl-cap: AIC comparison between full and stepwise logistic regression models with retained variables. 
    
df <- read.csv("D:/HuaweiMoveData/Users/13329/Desktop/DAProject7.csv", stringsAsFactors = TRUE)
data <- df

data$Obese <- ifelse(data$Obese == "Yes", 1, 0)
data$Sex <- as.factor(data$Sex)
data$Employment <- as.factor(data$Employment)
data$Fruit <- as.factor(data$Fruit)
data$Veg <- as.factor(data$Veg)


model <- glm(Obese ~ Year + AgeGroup + Sex + Employment + Fruit + Veg, 
             data = data, 
             family = binomial)

full_model_aic <- AIC(model)

stepwise_model <- stepAIC(model, direction = "both", trace = 1)
stepwise_model_aic <- AIC(stepwise_model)

retained_variables <- labels(terms(stepwise_model)) 
retained_summary <- ifelse(length(retained_variables) > 0, 
                           paste(retained_variables, collapse = ", "), 
                           "None")


aic_table <- data.frame(
  Model = c("Full Model", "Stepwise Model"),
  AIC = c(full_model_aic, stepwise_model_aic),
  Retained_Variables = c("All", retained_summary)
) %>%
  gt() %>%
  tab_header(title = "AIC Comparison and Retained Variables")
aic_table

```

@tbl-stepwise-coeff presents the regression coefficients, standard errors, z-values, and p-values for each variable category in the stepwise logistic regression model, assessing the statistical significance and impact of different variable on the odds of obesity.

```{r}
#| echo: false
#| label: tbl-stepwise-coeff
#| tbl-cap: Stepwise logistic regression model coefficients by variable.

model_summary <- summary(stepwise_model)

coefficients <- model_summary$coefficients[, 1]

std_errors <- model_summary$coefficients[, 2]
z_values <- model_summary$coefficients[, 3]
p_values <- model_summary$coefficients[, 4]

coefficients_df <- data.frame(
  Variable = rownames(model_summary$coefficients),
  Coefficient = coefficients,
  StdError = std_errors,
  ZValue = z_values,
  PValue = p_values
)

coefficients_df <- coefficients_df %>%
  mutate(
    Category = case_when(
       Variable == "(Intercept)" ~ "Intercept",
      str_detect(Variable, "AgeGroup") ~ "Age Group",
      str_detect(Variable, "Employment") ~ "Employment Status",
      Variable == "SexMale" ~ "Sex",
      Variable == "VegYes" ~ "Veg"
    )
  )

summary_df <- coefficients_df %>%
  group_by(Category) %>%
  summarise(
    Coefficient = mean(Coefficient, na.rm = TRUE), 
    StdError = mean(StdError, na.rm = TRUE),
    ZValue = mean(ZValue, na.rm = TRUE),
    PValue = min(PValue, na.rm = TRUE)
  ) %>%
  ungroup()

summary_df <- summary_df %>%
  gt() %>%
  tab_header(title = "Stepwise Logistic Regression Model Coefficients by variable") %>%
  fmt_number(columns = c("Coefficient", "StdError", "ZValue"), decimals = 3) %>%
  fmt(columns = "PValue", fns = function(x) formatC(x, format = "e", digits = 3)) %>%
  cols_label(
    Category = "Variable Category",
    Coefficient = "Regression Coefficient (β)",
    StdError = "Standard Error",
    ZValue = "Z-Value",
    PValue = "P-Value"
  )

summary_df


```

Hence, from @tbl-stepwise-coeff we obtain the Stepwise logistic regression model looks like this:

$$
\begin{aligned}
y_i &= \beta_0 + \beta_{\text{AgeGroup}} \cdot \text{AgeGroup} + \beta_{\text{Sex}} \cdot \text{Sex} + \beta_{\text{Employment}} \cdot \text{Employment} + \beta_{\text{Veg}} \cdot \text{Veg} \\
    &= -1.372 + 0.719 \cdot \text{AgeGroup} - 0.044 \cdot \text{Sex} - 0.072 \cdot \text{Employment} - 0.199 \cdot \text{Veg}
\end{aligned}
$$ @tbl-stepwise show the detailed results of the stepwise logistic regression.The odds ratio assesses how the odds of the dependent variable(Obese) change with a 1 unit change in the independent variable(AgeGroup, Sex, Employment, Veg). The lower and Upper 95% Confidence Interval indicates the range in which the Odds Ratio will lie for each variable. The P-Value tests the null hypothesis that the Odds Ratio is equal to 1. For all variables, the p-value \< 0.05, meaning it is statistically significant.

```{r}
#| echo: false
#| label: tbl-stepwise
#| tbl-cap: "Stepwise Logistic Regression Results: Odds Ratios and Confidence Intervals for Obesity Prevalence."


model_summary <- summary(stepwise_model)
p_values <- model_summary$coefficients[,4]

odds_ratios <- exp(coef(stepwise_model))
conf_intervals <- exp(confint(stepwise_model))

odds_ratios_t <- data.frame(
  Statistic = c("Odds Ratio", "Lower 95% CI", "Upper 95% CI", "P-Value")
)

for (var_name in names(odds_ratios)) {
  odds_ratios_t[[var_name]] <- c(
    odds_ratios[var_name],
    conf_intervals[var_name, 1],
    conf_intervals[var_name, 2],
    p_values[var_name]
  )
}

or_table_stepwise <- odds_ratios_t %>%
  gt() %>%
  tab_header(title = "Stepwise Logistic Regression Results: Odds Ratios, Confidence Intervals, and P-Values") %>%
  fmt_number(columns = -1, rows = 1:3, decimals = 3) %>%
  fmt(columns = -1, rows = 4, fns = function(x) formatC(x, format = "e", digits = 3))

or_table_stepwise

```

@fig-forest-plot illustrates the impact of various factors on obesity prevalence. The results show that the older the age, the higher the risk of obesity, especially in the 55-74 age group, where the risk is highest. Unemployed or inactive individuals have a higher likelihood of obesity, while full-time students have the lowest obesity risk. Additionally, vegetarians have a lower risk of obesity, suggesting that dietary habits play an important role in obesity development. Gender does not have a significant effect on obesity, with males having an odds ratio close to 1. Overall, age, employment status, and dietary habits are the main factors influencing obesity, while gender has a weaker effect.

```{r}
#| echo: false
#| label: fig-forest-plot
#| fig-cap: Forest plot of Odds Ratios from the Stepwise Logistic Regression model with 95% confidence intervals.
#| fig-align: center
#| fig-width: 6
#| fig-height: 4
#| theme: minimal


or_table_stepwise <- broom::tidy(stepwise_model, exponentiate = TRUE, conf.int = TRUE)


or_table_stepwise <- or_table_stepwise[, c("term", "estimate", "conf.low", "conf.high", "p.value")]
colnames(or_table_stepwise) <- c("Term", "OR", "2.5% CI", "97.5% CI", "P-value")


or_table_stepwise$Term <- factor(or_table_stepwise$Term, levels = rev(or_table_stepwise$Term))


ggplot(or_table_stepwise, aes(x = OR, y = Term)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = `2.5% CI`, xmax = `97.5% CI`), height = 0.3, color = "steelblue") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
  labs(x = "Odds Ratio (95% CI)",
       y = "Variable") +
  theme_minimal(base_size = 12)
```

## Model Diagnostics & Evaluation

```{r}
#| echo: false
#| warning: false
#| message: false
#| 
stepwise_model <- stepAIC(model, direction = "both", trace = 0)

```

### Check Outliers & Cook's Distance

```{r}
#| echo: false
#| label: fig-cooks-distance
#| fig-cap: Cook's Distance for Identifying Influential Observations.
#| fig-align: center
#| fig-width: 8
#| fig-height: 6

cooks_distance <- cooks.distance(stepwise_model)
plot(cooks_distance, type = "h", main = "Cook's Distance")
abline(h = 4 / length(cooks_distance), col = "red") 

```

We cannot check outliers on the stepwise model as we only have factor, and not numerical data Therefore we use cook's distance, and because few datapoints are far from the line threshold, there are no overly influential outliers.

### Check Collinearity (VIF Check)

```{r}
#| echo: false
#| label: tbl-vif-values
#| tbl-cap: Variance Inflation Factor (VIF) for stepwise logistic regression Model Predictors.

vif_results <- check_collinearity(stepwise_model)

gt(vif_results) %>%
  tab_header(
    title = "Variance Inflation Factors (VIF)",
    subtitle = "Multicollinearity Check"
  ) %>%
  fmt_number(columns = 2, decimals = 2)

```

The colliearity check, returns low correlation, which is good, as we don't want high correlation between predictor variables.

### Goodness of Fit (Hosmer-Lemeshow Test)

```{r}
#| echo: false
#| label: tbl-hoslem-test
#| tbl-cap: Hosmer-Lemeshow Test for stepwise logistic regression Model Fit.

predicted_probabilities <- predict(stepwise_model, type = "response")
hoslem_result <- hoslem.test(data$Obese, predicted_probabilities)

hoslem_summary <- data.frame(
  Statistic = "Chi-squared",
  Value = round(hoslem_result$statistic, 3),
  df = hoslem_result$parameter,
  p_value = round(hoslem_result$p.value, 3)
)

hoslem_summary %>%
  gt() %>%
  tab_header(title = "Hosmer-Lemeshow Test Results") %>%
  fmt_number(columns = 2:4, decimals = 4) %>%
  cols_label(Statistic = "Test Statistic", Value = "Value", df = "Degrees of Freedom", p_value = "P-value") %>%
  tab_options(table.font.size = "small")

```

The P value (0.1320) is much greater than 0.05, and therefore there is not enough evidence to reject the null hypothesis that the model fits the data: ie. that our model reasonably fits the data.

### Deviance Graphs

```{r}
#| echo: false
#| fig-cap: Histogram of Deviance Residuals for stepwise logistic regression Model
#| label: fig-deviance-hist
#| fig-align: center
#| fig-width: 5
#| fig-height: 4

hist(residuals(stepwise_model, type = "deviance"), main = "Histogram of Deviance Residuals")

```

These show clear patterns in the deviances, that there are two clear sections of data.

### ROC Curve & AUC

```{r}
#| echo: false
#| fig-cap: ROC Curve for stepwise logistic regression Model with AUC
#| label: fig-roc-curve
#| fig-align: center
#| fig-width: 5
#| fig-height: 4

predicted_prob <- predict(stepwise_model, type = "response")
roc_curve <- roc(data$Obese, predicted_prob) 

plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve (Stepwise Model)",
     xlab = "False Positive Rate (1 - Specificity)", 
     ylab = "True Positive Rate (Sensitivity)")


legend("topleft",legend = paste("AUC =", round(auc(roc_curve), 3)), 
       col = "blue", lwd = 2, box.lty = 0)  

```

### Confusion Matrix & Accuracy

```{r}
#| echo: false
#| label: tbl-accuracy
#| tbl-cap: Confusion Matrix and Accuracy for stepwise logistic regression Model.


data <- read.csv("DAProject7.csv", na.strings = c("", "NA"))
data$Obese <- factor(data$Obese, levels = c("No", "Yes"), labels = c("Not Obese", "Obese"))

predicted_probabilities <- predict(stepwise_model, type = "response")

threshold <- 0.4  
predicted_classes <- ifelse(predicted_probabilities > threshold, "Obese", "Not Obese")
predicted_classes <- factor(predicted_classes, levels = c("Not Obese", "Obese"))

conf_matrix <- confusionMatrix(predicted_classes, factor(data$Obese, levels = c("Not Obese", "Obese")))

cm_table <- as.data.frame(conf_matrix$table)
colnames(cm_table) <- c("Predicted Class", "Actual Class", "Count")

accuracy <- round(conf_matrix$overall["Accuracy"], 4)

cm_table %>%
  gt() %>%
  tab_header(title = "Confusion Matrix for Stepwise Model") %>%
  cols_label(`Predicted Class` = "Predicted Class", 
             `Actual Class` = "Actual Class", 
             `Count` = "Count") %>%
  fmt_number(columns = "Count", decimals = 0) %>%
  tab_source_note(md(paste0("**Model Accuracy:** ", accuracy)))




```

# Conclusions {#sec-con}
