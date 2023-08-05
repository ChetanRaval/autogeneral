# install.packages(c("tidyverse", "lubridate", "MASS", "reshape2", "quantreg"))

library(tidyverse)
library(MASS)
library(lubridate)
library(reshape2)
library(quantreg)

# set seed for reproducibility when generating random data
set.seed(123)

# set number of total employees
total_employees <- 200

# create row names for each tenure cohort
tenure_groups <- c("0-6 months", "6-12 months", "12-18 months", "18-24 months", "24+ months")
# distribute tenure cohorts into 40% 0-6 months and distribute the remainder among other cohorts
tenure_distribution <- c(0.4, 0.2, 0.15, 0.15, 0.1)
# get integer values of each tenure group depending on the percentage distribution
employees_per_group <- round(total_employees * tenure_distribution)

# create empty dataframe to store generated employee data
df <- data.frame()

# synthesise data
for (i in 1:length(tenure_groups)) {
  group_df <- tibble(
    EmployeeID = seq(from = 1, to = employees_per_group[i]),
    TenureGroup = tenure_groups[i],
    # generate values for KPIs from a normal distribution - sqrt function is used here to simulate the increase as tenure increases
    AHT = rnorm(n = employees_per_group[i], mean = (600 - sqrt(i)*60), sd = 50), # Increased variability
    NPS = round(rnorm(n = employees_per_group[i], mean = (5 + sqrt(i)*15), sd = 15)),  # Increased variability
    CallsPerDay = round(rnorm(n = employees_per_group[i], mean = (10 + sqrt(i)*10), sd = 10)),  # Increased variability
    # Poisson distribution used to create the number of assessments completed by each employee
    QA_Assessments = rpois(n = employees_per_group[i], lambda = 10 + (i-1)*2),  
    # Placeholder for SQA values
    SQA = rep(0, employees_per_group[i])  
  )
  
  # SQA is now the sum of individual assessment scores divided by the number of assessments
  group_df$SQA <- sapply(group_df$QA_Assessments, function(x) {
    scores = rnorm(n = x, mean = 70 + sqrt(i)*5, sd = 10) # Increased variability
    return(sum(pmin(pmax(scores, 0), 100))/x)
  })
  
  df <- bind_rows(df, group_df)
}


# ensure no negative AHT values
df$AHT <- ifelse(df$AHT < 0, abs(df$AHT), df$AHT)
# ensure no NPS scores are out of bounds (-100 - 100)
df$NPS <- pmin(pmax(df$NPS, -100), 100)
# ensure no negative calls per day
df$CallsPerDay <- ifelse(df$CallsPerDay < 0, abs(df$CallsPerDay), df$CallsPerDay)
# ensure no SQA values are out of bounds
df$SQA <- pmin(pmax(df$SQA, 0), 100)

# Add a random number of days within the appropriate range for each tenure group
# This could provide more granuluarity when fitting regression models
df$DaysEmployed <- ifelse(df$TenureGroup == "0-6 months", sample(0:180, size = nrow(df), replace = TRUE), 
                          ifelse(df$TenureGroup == "6-12 months", sample(181:365, size = nrow(df), replace = TRUE), 
                                 ifelse(df$TenureGroup == "12-18 months", sample(366:545, size = nrow(df), replace = TRUE),
                                        ifelse(df$TenureGroup == "18-24 months", sample(546:730, size = nrow(df), replace = TRUE), sample(731:1095, size = nrow(df), replace = TRUE)))))

# EDA
# create levels/factors from tenure cohorts for plotting purposes
df$TenureGroupFact <- factor(df$TenureGroup, 
                         levels = c("0-6 months", 
                                    "6-12 months", 
                                    "12-18 months", 
                                    "18-24 months", 
                                    "24+ months"))

# AHT plot
ggplot(df, aes(x = TenureGroupFact, y = AHT)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, size = 1, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Average Handle Time per Tenure Cohort",
       x = "Tenure Cohort",
       y = "Average Handle Time (seconds)")

# NPS plot
ggplot(df, aes(x = TenureGroupFact, y = NPS)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, size = 1, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Net Promoter Score per Tenure Cohort",
       x = "Tenure Cohort",
       y = "Net Promoter Score")

# CallsPerDay plot
ggplot(df, aes(x = TenureGroupFact, y = CallsPerDay)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, size = 1, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Calls Per Day per Tenure Cohort",
       x = "Tenure Cohort",
       y = "Calls Per Day")

# SQA plot
ggplot(df, aes(x = TenureGroupFact, y = SQA)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, size = 1, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Service Quality Assurance per Tenure Cohort",
       x = "Tenure Cohort",
       y = "Service Quality Assurance Score")


# increase performance of more tenured employees
# have more people at either end of the range rather than in the middle
# have better performing newer staff at lower end of AHT and NPS etc.
# make increases less aggressive between cohorts but remain linear

# create summary table showing mean/median/SD of each KPI per tenure group
df_summary <- df %>% 
  group_by(TenureGroup) %>% 
  summarise(
    Mean_AHT = mean(AHT),
    Median_AHT = median(AHT),
    SD_AHT = sd(AHT),
    Mean_NPS = mean(NPS),
    Median_NPS = median(NPS),
    SD_NPS = sd(NPS),
    Mean_CallsPerDay = mean(CallsPerDay),
    Median_CallsPerDay = median(CallsPerDay),
    SD_CallsPerDay = sd(CallsPerDay),
    Mean_SQA = mean(SQA),
    Median_SQA = median(SQA),
    SD_SQA = sd(SQA)
  )

# Fit linear models
# KPI as a function of cohort
model_AHT <- lm(AHT ~ DaysEmployed, data = df)
model_NPS <- lm(NPS ~ DaysEmployed, data = df)
model_CallsPerDay <- lm(CallsPerDay ~ DaysEmployed, data = df)
model_SQA <- lm(SQA ~ DaysEmployed, data = df)

# Calculate standard errors for each model
se_AHT <- summary(model_AHT)$coefficients["DaysEmployed", "Std. Error"]
se_NPS <- summary(model_NPS)$coefficients["DaysEmployed", "Std. Error"]
se_CallsPerDay <- summary(model_CallsPerDay)$coefficients["DaysEmployed", "Std. Error"]
se_SQA <- summary(model_SQA)$coefficients["DaysEmployed", "Std. Error"]

# Combine standard errors into a data frame
se <- data.frame(AHT = se_AHT, NPS = se_NPS, CallsPerDay = se_CallsPerDay, SQA = se_SQA)

# plot assumptions of each model
par(mfrow=c(2,2))
plot(model_AHT)

par(mfrow=c(2,2))
plot(model_NPS)

par(mfrow=c(2,2))
plot(model_CallsPerDay)

par(mfrow=c(2,2))
plot(model_SQA)

# The KPI with the highest R-squared value has the strongest linear relationship with tenure. 
# Thus, the benchmarks for this KPI could be considered the most significant or reliable, 
# in terms of being predicted by tenure
summary(model_AHT)$r.squared
summary(model_NPS)$r.squared
summary(model_CallsPerDay)$r.squared
summary(model_SQA)$r.squared


# make predictions for KPI benchmarks per tenure cohort using quantile regression

# Define tenure midpoints
midpoints <- c(90, 270, 450, 630, 900)

# Initialize benchmarks data frame
benchmarks <- data.frame(TenureGroup = c("0-6 months", "6-12 months", "12-18 months", "18-24 months", "24+ months"),
                         DaysEmployed = midpoints)

# List of KPIs
kpis <- c("AHT", "NPS", "CallsPerDay", "SQA")

# Loop over KPIs
for (kpi in kpis) {
  # Fit quantile regression model for current KPI
  model <- rq(as.formula(paste(kpi, "~ DaysEmployed")), tau = c(0.25, 0.5, 0.75), data = df)
  
  # Make predictions for each tenure midpoint
  predictions <- predict(model, newdata = benchmarks)
  
  # Add predictions to benchmarks data frame
  benchmarks <- cbind(benchmarks, predictions)
}

# Print benchmarks dataframe
print(benchmarks)

# Make informative column names
colnames(benchmarks) <- c("TenureCohort", "Days_Midpoint",
                          "ATH_25th", "ATH_50th", "ATH_75th",
                          "NPS_25th", "NPS_50th", "NPS_75th",
                          "Calls_25th", "Calls_50th", "Calls_75th",
                          "SQA_25th", "SQA_50th", "SQA_75th")

# Convert TenureCohort to an ordered factor so it plots in the correct order
benchmarks$TenureCohort <- factor(benchmarks$TenureCohort, 
                                  levels = c("0-6 months", "6-12 months", "12-18 months", "18-24 months", "24+ months"), 
                                  ordered = TRUE)

# convert benchmarks dataframe to long format for plotting
benchmarks_long <- benchmarks %>%
  pivot_longer(cols = -c(TenureCohort, Days_Midpoint), 
               names_to = c("KPI", "Percentile"), 
               names_sep = "_", 
               values_to = "Value")


# ATH Plot - benchmark
ggplot(subset(benchmarks_long, KPI == "ATH"), aes(x = TenureCohort, y = Value, color = Percentile, group = Percentile)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Tenure Cohort", 
       y = "Average Handle Time (seconds)", 
       color = "Percentile", 
       title = "Benchmark Average Handle Time (seconds) per Tenure Cohort") +
  theme(legend.position = "bottom")

# NPS Plot - benchmark
ggplot(subset(benchmarks_long, KPI == "NPS"), aes(x = TenureCohort, y = Value, color = Percentile, group = Percentile)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Tenure Cohort", 
       y = "Net Promoter Score", 
       color = "Percentile", 
       title = "Benchmark Net Promoter Score per Tenure Cohort") +
  theme(legend.position = "bottom")

# Calls per day Plot - benchmark
ggplot(subset(benchmarks_long, KPI == "Calls"), aes(x = TenureCohort, y = Value, color = Percentile, group = Percentile)) +
  geom_line() +
  labs(x = "Tenure Cohort", 
       y = "Calls per Day", 
       color = "Percentile", 
       title = "Benchmark Number of Calls per day per Tenure Cohort") +
  theme_minimal()

# SQA Plot - benchmark
ggplot(subset(benchmarks_long, KPI == "SQA"), aes(x = TenureCohort, y = Value, color = Percentile, group = Percentile)) +
  geom_line() +
  labs(x = "Tenure Cohort", 
       y = "Service Quality Assurance Score", color = "Percentile", title = "Service Quality Assurance Score per Tenure Cohort") +
  theme_minimal()



## STRETCH GOAL
# make predictions for KPI benchmarks per tenure cohort using quantile regression

# Define tenure midpoints
midpoints <- c(90, 270, 450, 630, 900)

# Initialize benchmarks data frame
benchmarks_stretch <- data.frame(TenureGroup = c("0-6 months", "6-12 months", "12-18 months", "18-24 months", "24+ months"),
                         DaysEmployed = midpoints)

# List of KPIs
kpis <- c("AHT", "NPS", "CallsPerDay", "SQA")

# Loop over KPIs
for (kpi in kpis) {
  # Fit quantile regression model for current KPI
  model <- rq(as.formula(paste(kpi, "~ DaysEmployed")), tau = c(0.50, 0.75, 0.90), data = df)
  
  # Make predictions for each tenure midpoint
  predictions <- predict(model, newdata = benchmarks_stretch)
  
  # Add predictions to benchmarks data frame
  benchmarks_stretch <- cbind(benchmarks_stretch, predictions)
}

# Print benchmarks dataframe
head(benchmarks_stretch)

# Make informative column names
colnames(benchmarks_stretch) <- c("TenureCohort", "Days_Midpoint",
                          "ATH_50th", "ATH_75th", "ATH_90th",
                          "NPS_50th", "NPS_75th", "NPS_90th",
                          "Calls_50th", "Calls_75th", "Calls_90th",
                          "SQA_50th", "SQA_75th", "SQA_90th")

# Convert TenureCohort to an ordered factor so it plots in the correct order
benchmarks_stretch$TenureCohort <- factor(benchmarks_stretch$TenureCohort, 
                                  levels = c("0-6 months", "6-12 months", "12-18 months", "18-24 months", "24+ months"), 
                                  ordered = TRUE)

# convert benchmarks dataframe to long format for plotting
benchmarks_stretch_long <- benchmarks_stretch %>%
  pivot_longer(cols = -c(TenureCohort, Days_Midpoint), 
               names_to = c("KPI", "Percentile"), 
               names_sep = "_", 
               values_to = "Value")

# ATH Plot - benchmark
ggplot(subset(benchmarks_stretch_long, KPI == "ATH"), aes(x = TenureCohort, y = Value, color = Percentile, group = Percentile)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Tenure Cohort", 
       y = "Average Handle Time (seconds)", 
       color = "Percentile", 
       title = "Benchmark Average Handle Time (seconds) per Tenure Cohort (Stretch Goal)") +
  theme(legend.position = "bottom")

# NPS Plot - benchmark
ggplot(subset(benchmarks_stretch_long, KPI == "NPS"), aes(x = TenureCohort, y = Value, color = Percentile, group = Percentile)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Tenure Cohort", 
       y = "Net Promoter Score", 
       color = "Percentile", 
       title = "Benchmark Net Promoter Score per Tenure Cohort (Stretch Goal)") +
  theme(legend.position = "bottom")

# Calls per day Plot - benchmark
ggplot(subset(benchmarks_stretch_long, KPI == "Calls"), aes(x = TenureCohort, y = Value, color = Percentile, group = Percentile)) +
  geom_line() +
  labs(x = "Tenure Cohort", 
       y = "Calls per Day", 
       color = "Percentile", 
       title = "Benchmark Number of Calls per day per Tenure Cohort (Stretch Goal)") +
  theme_minimal()

# SQA Plot - benchmark
ggplot(subset(benchmarks_stretch_long, KPI == "SQA"), aes(x = TenureCohort, y = Value, color = Percentile, group = Percentile)) +
  geom_line() +
  labs(x = "Tenure Cohort", 
       y = "Service Quality Assurance Score", 
       color = "Percentile", 
       title = "Service Quality Assurance Score per Tenure Cohort (Stretch Goal)") +
  theme_minimal()


