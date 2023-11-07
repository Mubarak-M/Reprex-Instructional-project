rm(list = ls())


library(ggplot2)
library(tidyverse)
library(caTools)
#Library()

df <- read.csv("example_data.csv")

names(df)

head(df)





vars_of_choice <- c('veh_value', 'exposure', 'veh_body', 'veh_age', 'gender', 
                    'area', 'agecat', 'engine_type', 'max_power', 
                    'driving_history_score', 'e_bill', 'time_of_week_driven',
                    'time_driven', 'trm_len', 'credit_score', 'high_education_ind', 'clm')

df_vars <- df[vars_of_choice]
df_vars$clm <- factor(df_vars$clm)

split_df <- sample.split(df_vars$clm, SplitRatio = 0.8)

training_df <- df_vars[split_df, ]
test_df <- df_vars[!split_df, ]








#Logistic Regression
logistic.fit <- glm(
  factor(clm) ~ veh_value + exposure + factor(veh_body) + factor(veh_age) +
    factor(gender) + factor(area) + factor(agecat) + factor(engine_type) + 
    max_power + driving_history_score + factor(e_bill) + factor(time_of_week_driven) + 
    factor(time_driven) + factor(trm_len) + credit_score + factor(high_education_ind),
  data = training_df, family = binomial
)

summary(logistic.fit)

logistic.probs <- predict(logistic.fit, newdata = test_df, type = 'response')
logistic.pred <- rep(0, dim(test_df)[1])
logistic.pred[logistic.probs > 0.2] <- 1

table(logistic.pred, test_df$clm)
logistic_test_error <- mean(logistic.pred != test_df$clm)
cat('Logistic Regression Test Error: ', logistic_test_error, '\n')

logistic.pred.none <- rep(0, dim(test_df)[1])
table(logistic.pred.none, test_df$clm)
logistic_test_error_none <- mean(logistic.pred.none != test_df$clm)
cat('Test Error with all 0 Predictions: ', logistic_test_error_none, '\n')

## Logistic Regression is doing a poor job at predicting when a claim occurs.
## This can be made better by lowering the threshold; however, it comes
## with a high tradeoff.



#Trying various KNN classifiers

#More Data Manipulation
df_vars_knn <- df_vars
df_vars_knn$clm <- factor(df_vars_knn$clm)


df_vars_knn$gender <- ifelse(df_vars_knn$gender == "M", 1, 0)
df_vars_knn$e_bill <- ifelse(df_vars_knn$e_bill == 1, 1, 0)
df_vars_knn$time_of_week_driven <- ifelse(df_vars_knn$time_of_week_driven == "weekday", 1, 0)
df_vars_knn$trm_len <- ifelse(df_vars_knn$trm_len == "6", 1, 0)
df_vars_knn$high_education_ind <- ifelse(df_vars_knn$high_education_ind == 1, 1, 0)



#ERROR
ggplot(data = df, x = veh_value, y = exposure) +
  geom_point()


reprex(
  input = c(
    'if (True) "true branch" else {',
    '"else branch"',
    '             }'
  ),
  style = TRUE
)














head(df, 5)[, c('veh_value', 'exposure')]


library(reprex)
reprex({
  library(ggplot2)
  
  reprex_df <- data.frame(stringsAsFactors = FALSE,
                          veh_value = c(0.77, 4.45, 4.90, 0.48, 0.85),
                          exposure = c(0.4445044, 0.5621830, 0.4652436, 0.2710387, 0.1416235)
  )
  
  ggplot(data = reprex_df, x = veh_value, y = exposure) +
    geom_point()
  
}, venue = "gh", session_info = TRUE)






















#Correct
ggplot(data = df, aes(x = veh_value, y = exposure)) +
  geom_point()



