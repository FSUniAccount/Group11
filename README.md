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

4.  Obesity Rate by Fruit Intake and Sex:Unlike vegetables, fruit intake shows minimal association with obesity, as obesity rates remain nearly unchanged regardless of fruit consumption (29.9% vs. 29.8% for females, 28.9% vs. 29.6% for males). This highlights potential differences in how various dietary components impact obesity.

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

## Model Diagnostics & Evaluation

# Conclusions {#sec-con}

# References

# 

