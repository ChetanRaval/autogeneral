# install.packages(c("tidyverse", "lubridate", "MASS"))

library(tidyverse)
library(MASS)
library(lubridate)

# set seed for reproducibility when generating random data
set.seed(123)

total_employees <- 200

tenure_groups <- c("0-6 months", "6-12 months", "12-18 months", "18-24 months", "24+ months")
tenure_distribution <- c(0.4, 0.2, 0.15, 0.15, 0.1)

employees_per_group <- round(total_employees * tenure_distribution)

df <- data.frame()

# synthesise data
for (i in 1:length(tenure_groups)) {
  group_df <- tibble(
    EmployeeID = seq(from = 1, to = employees_per_group[i]),
    TenureGroup = tenure_groups[i],
    # generate values for KPIs from a normal distribution - (i-1)*x is used here to simulate the decrease as tenure increases
    AHT = rnorm(n = employees_per_group[i], mean = (350 - (i-1)*30), sd = 20),
    NPS = round(rnorm(n = employees_per_group[i], mean = (20 + (i-1)*15), sd = 10)),
    CallsPerDay = round(rnorm(n = employees_per_group[i], mean = (25 + (i-1)*5), sd = 5)),
    # Poisson distribution used to create the number of assessments completed by each employee
    QA_Assessments = rpois(n = employees_per_group[i], lambda = 10 + (i-1)*2),  
    # Placeholder for SQA values
    SQA = rep(0, employees_per_group[i])  
  )
  
  # SQA is now the sum of individual assessment scores divided by the number of assessments
  group_df$SQA <- sapply(group_df$QA_Assessments, function(x) {
    scores = rnorm(n = x, mean = 80, sd = 5)
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

# EDA

# AHT plot
ggplot(df, aes(x = TenureGroup, y = AHT)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Average Handle Time per Tenure Group",
       x = "Tenure Group",
       y = "Average Handle Time (seconds)")

# NPS plot
ggplot(df, aes(x = TenureGroup, y = NPS)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Net Promoter Score per Tenure Group",
       x = "Tenure Group",
       y = "Net Promoter Score")

# CallsPerDay plot
ggplot(df, aes(x = TenureGroup, y = CallsPerDay)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Calls Per Day per Tenure Group",
       x = "Tenure Group",
       y = "Calls Per Day")

# SQA plot
ggplot(df, aes(x = TenureGroup, y = SQA)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Service Quality Assurance per Tenure Group",
       x = "Tenure Group",
       y = "Service Quality Assurance Score")

