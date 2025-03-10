library(tidyverse)
library(glmnet)

set.seed(123)

data <- read.csv("heart_failure_clinical_records_dataset.csv")

R <- 10000
boot_index_10000 <- sample(nrow(data), R, replace=TRUE)
boot_heart_10000 <- data[boot_index_10000, ]
boot_heart_10000$n <- 1:R

boot_data <- boot_heart_10000 %>%
    mutate(log_age = log(age), log_serum_creatinine = log(serum_creatinine),
           log_creatinine_phosphokinase = log(creatinine_phosphokinase),
           anaemia = as.factor(anaemia), diabetes = as.factor(diabetes), 
           high_blood_pressure = as.factor(high_blood_pressure), 
           sex = as.factor(sex), smoking = as.factor(smoking)) %>%
    select(-age, -serum_creatinine, -creatinine_phosphokinase) %>%
    mutate(n = 1:n())

train <- boot_data %>% 
    sample_frac(0.8)
test <- anti_join(boot_data, train, by = "n") %>%
    select(-n)
train <- select(train, -n)

x_train <- as.matrix(select(train, -DEATH_EVENT))
x_test <- as.matrix(select(test, -DEATH_EVENT))
y_train <- as.matrix(train$DEATH_EVENT)
y_test <- test$DEATH_EVENT 

logistic_model2 <- glm(DEATH_EVENT ~ ., data = train, family = "binomial")
summary(logistic_model2)

logistic_model <- glmnet(x_train, y_train, family = "binomial", alpha = 0)
plot(logistic_model, xvar = "lambda", label = TRUE)

cv_glm <- cv.glmnet(x_train, y_train, family = "binomial", k = 5)

predictions <- predict(logistic_model, newx = x_test, type = "response", 
                       s = cv_glm$lambda.min) %>%
    as.data.frame() %>%
    `colnames<-`(c("death_prob")) %>%
    mutate(pred = as.numeric(death_prob > 0.5)) %>%
    cbind(., y_test)

ct <- table(predictions$pred, predictions$y_test)
ct

accuracy <- sum(diag(ct)) / sum(ct)
accuracy

# pred_data <- cbind(select(boot_data, -DEATH_EVENT), predictions)

x_train_reduced <- as.matrix(select(as.data.frame(x_train), -anaemia, -diabetes, 
                                    -high_blood_pressure, -smoking, -time))
x_test_reduced <- as.matrix(select(as.data.frame(x_test), -anaemia, -diabetes, 
                                   -high_blood_pressure, -smoking, -time))

new_train <- select(as.data.frame(train), -anaemia, -diabetes,
                    -high_blood_pressure, -smoking)

logistic_model4 <- glm(DEATH_EVENT ~ ., data = new_train, family = "binomial")
summary(logistic_model4)

logistic_model3 <- glmnet(x_train_reduced, y_train, family = "binomial", 
                         alpha = 0)

predictions <- predict(logistic_model3, newx = x_test_reduced, 
                       type = "response", s = 0.01) %>%
    as.data.frame() %>%
    `colnames<-`(c("death_prob")) %>%
    mutate(pred = as.numeric(death_prob > 0.5)) %>%
    cbind(., y_test)

ct_reduced <- table(predictions$pred, predictions$y_test)
ct_reduced

sum(diag(ct_reduced)) / sum(ct_reduced)
