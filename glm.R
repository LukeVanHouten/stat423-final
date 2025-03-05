library(tidyverse)
library(glmnet)

set.seed(123)

data <- read.csv("heart_failure_clinical_records_dataset.csv")

R <- 10000
boot_index_10000 <- sample(nrow(data), R, replace=TRUE)
boot_heart_10000 <- data[boot_index_10000, ]
boot_heart_10000$n <- 1:R

train <- boot_heart_10000 %>% 
    sample_frac(0.8)
test <- anti_join(boot_heart_10000, train, by = "n") %>%
    select(-n)
train <- select(train, -n)

x_train <- as.matrix(select(train, age, creatinine_phosphokinase, 
                            ejection_fraction, platelets, 
                            serum_creatinine, serum_sodium, time))
x_test <- as.matrix(select(test, age, creatinine_phosphokinase, 
                           ejection_fraction, platelets, 
                           serum_creatinine, serum_sodium, time))
y_train <- as.matrix(train$DEATH_EVENT)
y_test <- test$DEATH_EVENT 

logistic_model2 <- glm(DEATH_EVENT ~ ., data = train, family = "binomial")
summary(logistic_model2)

logistic_model <- glmnet(x_train, y_train, family = "binomial", alpha = 0.2)
plot(logistic_model, xvar = "lambda", label = TRUE)

predictions <- predict(logistic_model, newx = x_test, type = "response", 
                       s = 0.01) %>%
    as.data.frame() %>%
    `colnames<-`(c("death_prob")) %>%
    mutate(pred = as.numeric(death_prob > 0.5)) %>%
    cbind(., y_test)

ct <- table(predictions$pred, predictions$y_test)
ct

accuracy <- sum(diag(ct)) / sum(ct)
accuracy

pred_data <- cbind(select(boot_heart_10000, -DEATH_EVENT), predictions)
