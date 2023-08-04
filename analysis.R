# install.packages(c("tidyverse", "lubridate", "MASS", "reshape2"))

library(tidyverse)
library(MASS)
library(lubridate)
library(reshape2)

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
  labs(title = "Average Handle Time per Tenure Group",
       x = "Tenure Group",
       y = "Average Handle Time (seconds)")

# NPS plot
ggplot(df, aes(x = TenureGroupFact, y = NPS)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, size = 1, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Net Promoter Score per Tenure Group",
       x = "Tenure Group",
       y = "Net Promoter Score")

# CallsPerDay plot
ggplot(df, aes(x = TenureGroupFact, y = CallsPerDay)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, size = 1, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Calls Per Day per Tenure Group",
       x = "Tenure Group",
       y = "Calls Per Day")

# SQA plot
ggplot(df, aes(x = TenureGroupFact, y = SQA)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, size = 1, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Service Quality Assurance per Tenure Group",
       x = "Tenure Group",
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


# Convert tenure groups to numerical values
# integers in ifelse are the middle value for the tenure range
# i.e. 0-6 months is 3 months, 6-12 months is 9 months etc.
# df$TenureNumerical <- ifelse(df$TenureGroup == "0-6 months", 3, 
#                              ifelse(df$TenureGroup == "6-12 months", 9, 
#                                     ifelse(df$TenureGroup == "12-18 months", 15, 
#                                            ifelse(df$TenureGroup == "18-24 months", 21, 30))))



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







# Create a new data frame for days of employment
days <- seq(0, 1080, by = 15) # 15-day intervals from 30 to 720
benchmarks <- data.frame(DaysEmployed = days)

# Predict KPIs at each specified day of employment
benchmarks$AHT <- predict(model_AHT, newdata = benchmarks)
benchmarks$NPS <- predict(model_NPS, newdata = benchmarks)
benchmarks$CallsPerDay <- round(predict(model_CallsPerDay, newdata = benchmarks))
benchmarks$SQA <- predict(model_SQA, newdata = benchmarks)

# Define breaks for the tenure cohort categories
breaks <- c(0, 180, 360, 540, 720, Inf) # corresponds to 0-6, 6-12, 12-18, 18-24, 24+ months

# Define labels for the tenure cohort categories
labels <- c("0-6 months", "6-12 months", "12-18 months", "18-24 months", "24+ months")

# Create TenureCohort column
benchmarks$TenureCohort <- cut(benchmarks$DaysEmployed, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)

# Repeat se data frame for each row of benchmarks
se_rep <- se[rep(1, nrow(benchmarks)), ]

# Use standard errors to calculate confidence intervals
benchmarks <- cbind(benchmarks, lower = benchmarks[, 2:5] - 1.96 * se_rep, upper = benchmarks[, 2:5] + 1.96 * se_rep)

# The KPI with the highest R-squared value has the strongest linear relationship with tenure. 
# Thus, the benchmarks for this KPI could be considered the most significant or reliable, 
# in terms of being predicted by tenure
summary(model_AHT)$r.squared
summary(model_NPS)$r.squared
summary(model_CallsPerDay)$r.squared
summary(model_SQA)$r.squared


