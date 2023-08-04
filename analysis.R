# install.packages(c("tidyverse", "lubridate", "MASS", "reshape2"))

library(tidyverse)
library(MASS)
library(lubridate)
library(reshape2)

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
    AHT = rnorm(n = employees_per_group[i], mean = (350 - (i-1)*15), sd = 20),
    NPS = round(rnorm(n = employees_per_group[i], mean = (20 + (i-1)*10), sd = 10)),
    CallsPerDay = round(rnorm(n = employees_per_group[i], mean = (25 + (i-1)*2), sd = 5)),
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

df$TenureGroup <- factor(df$TenureGroup, levels = c("0-6 months", "6-12 months", "12-18 months", "18-24 months", "24+ months"))



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




# increase performance of more tenured employees
# have more people at either end of the range rather than in the middle
# have better performing newer staff at lower end of AHT and NPS etc.
# make increases less aggressive between cohorts but remain linear


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
df$TenureNumerical <- ifelse(df$TenureGroup == "0-6 months", 3, 
                             ifelse(df$TenureGroup == "6-12 months", 9, 
                                    ifelse(df$TenureGroup == "12-18 months", 15, 
                                           ifelse(df$TenureGroup == "18-24 months", 21, 30))))

# Build linear models
model_AHT <- lm(AHT ~ TenureNumerical, data = df)
model_NPS <- lm(NPS ~ TenureNumerical, data = df)
model_CallsPerDay <- lm(CallsPerDay ~ TenureNumerical, data = df)
model_SQA <- lm(SQA ~ TenureNumerical, data = df)

# check assumptions
par(mfrow=c(2,2))
plot(model_AHT)

par(mfrow=c(2,2))
plot(model_NPS)

par(mfrow=c(2,2))
plot(model_CallsPerDay)

par(mfrow=c(2,2))
plot(model_SQA)

# Predict KPIs at the midpoint of each tenure group
benchmarks <- data.frame(TenureNumerical = c(3, 9, 15, 21, 30))
benchmarks$AHT <- predict(model_AHT, newdata = benchmarks)
benchmarks$NPS <- predict(model_NPS, newdata = benchmarks)
benchmarks$CallsPerDay <- predict(model_CallsPerDay, newdata = benchmarks)
benchmarks$SQA <- predict(model_SQA, newdata = benchmarks)


# The KPI with the highest R-squared value has the strongest linear relationship with tenure. 
# Thus, the benchmarks for this KPI could be considered the most significant or reliable, 
# in terms of being predicted by tenure.
summary(model_AHT)$r.squared
summary(model_NPS)$r.squared
summary(model_CallsPerDay)$r.squared
summary(model_SQA)$r.squared

