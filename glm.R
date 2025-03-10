library(tidyverse)
library(glmnet)

data <- read.csv("heart_failure_clinical_records_dataset.csv")

# Untouched data, no seed set
accs <- numeric(20)
for (i in 1:20) {
    data$n <- 1:nrow(data)
    train_basic <- data %>% 
        sample_frac(0.8)
    test_basic <- anti_join(data, train_basic, by = "n") %>%
        select(-n)
    train_basic <- select(train_basic, -n)
    x_test_basic <- select(test_basic, -DEATH_EVENT)
    y_test_basic <- test_basic$DEATH_EVENT
    
    logistic_model_i <- glm(DEATH_EVENT ~ ., data = train_basic, 
                            family = "binomial")
    summary(logistic_model_i)
    
    predictions_i <- predict(logistic_model_i, newdata = x_test_basic, 
                           type = "response") %>%
        as.data.frame() %>%
        `colnames<-`(c("death_prob")) %>%
        mutate(pred = as.numeric(death_prob > 0.5)) %>%
        cbind(., y_test_basic)
    
    ct_basic <- table(predictions_i$pred, predictions_i$y_test_basic)
    
    accuracy_basic <- sum(diag(ct_basic)) / sum(ct_basic)
    accs[i] <- accuracy_basic
}

accs
mean(accs)
sd(accs)

transformed_df <- read.csv("death_transformed.csv") 

boot_data <- transformed_df %>%
    select(-time) %>%
    mutate(anaemia = as.factor(anaemia), diabetes = as.factor(diabetes),
           high_blood_pressure = as.factor(high_blood_pressure),
           sex = as.factor(sex), smoking = as.factor(smoking))

set.seed(123)

train <- boot_data %>% 
    sample_frac(0.8)
test <- anti_join(boot_data, train, by = "X") %>%
    select(-X)
train <- select(train, -X)

x_train <- select(train, -DEATH_EVENT)
x_test <- select(test, -DEATH_EVENT)
y_train <- train$DEATH_EVENT
y_test <- test$DEATH_EVENT 

set.seed(123)

# Resampled data, time removed, transformations applied
logistic_model <- glm(DEATH_EVENT ~ ., data = train, family = "binomial")
summary(logistic_model)

predictions <- predict(logistic_model, newdata = x_test, type = "response") %>%
    as.data.frame() %>%
    `colnames<-`(c("death_prob")) %>%
    mutate(pred = as.numeric(death_prob > 0.5)) %>%
    cbind(., y_test)

x_train_reduced <- as.matrix(select(x_train, -diabetes))
x_test_reduced <- as.matrix(select(x_test, -diabetes))

set.seed(123)

# K-fold CV lambda, alpha controlled
logistic_model_2 <- glmnet(x_train_reduced, y_train, family = "binomial", 
                           alpha = 0)

cv_glm <- cv.glmnet(x_train_reduced, y_train, family = "binomial", k = 10)

predictions_2 <- predict(logistic_model_2, newx = x_test_reduced, 
                         type = "response", s = cv_glm$lambda.min) %>%
    as.data.frame() %>%
    `colnames<-`(c("death_prob")) %>%
    mutate(pred = as.numeric(death_prob > 0.5)) %>%
    cbind(., y_test)

ct <- table(predictions$pred, predictions$y_test)
ct

accuracy <- sum(diag(ct)) / sum(ct)
accuracy

ct_reduced <- table(predictions_2$pred, predictions_2$y_test)
ct_reduced

sum(diag(ct_reduced)) / sum(ct_reduced)

# Precision
ct[2, 2] / (ct[1, 2] + ct[2, 2])
ct_reduced[2, 2] / (ct_reduced[1, 2] + ct_reduced[2, 2])

# Recall
ct[2, 2] / (ct[2, 1] + ct[2, 2])
ct_reduced[2, 2] / (ct_reduced[2, 1] + ct_reduced[2, 2])
