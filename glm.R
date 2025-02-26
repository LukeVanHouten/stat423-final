library(tidyverse)
library(glmnet)

data <- read.csv("heart_failure_clinical_records_dataset.csv")

train <- data %>% 
    sample_frac(0.75)
test <- anti_join(data, train)

x_train <- as.matrix(select(train, age, creatinine_phosphokinase, 
                            ejection_fraction, platelets, 
                            serum_creatinine, serum_sodium, time))
x_test <- as.matrix(select(test, age, creatinine_phosphokinase, 
                           ejection_fraction, platelets, 
                           serum_creatinine, serum_sodium, time))
y_train <- as.matrix(train$DEATH_EVENT)
y_test <- test$DEATH_EVENT 


logistic_model <- glmnet(x_train, y_train, family = "gaussian", alpha = 0.2)
plot(logistic_model, xvar = "lambda", label = TRUE)

predictions <- predict(logistic_model, newx = x_test, type = "response", 
                       s = 0.01) %>%
    as.data.frame() %>%
    `colnames<-`(c("DEATH_EVENT")) %>%
    mutate(pred = as.numeric(DEATH_EVENT > 0.5)) %>%
    cbind(., y_test)

ct <- table(predictions$pred, predictions$y_test)
ct

accuracy <- sum(diag(ct)) / sum(ct)
accuracy
