library(tidyverse)
library(ggsurvfit)
library(survival)
library(survminer)

data <- read.csv("heart_failure_clinical_records_dataset.csv")

survfit2(Surv(time, DEATH_EVENT) ~ 1, data = data) %>%
  ggsurvfit() + 
  labs(
    x = "Days",
    y = "Overall Survival"
  )

m = 500
n = 299

big_data = data.frame()

for(i in 1:m){
    indecies = sample(c(1:n), n, replace = TRUE)
    big_data = rbind(big_data, data[indecies, ])
}

### Wilson-Cox For each categorical variable
res.cox = coxph(Surv(time, DEATH_EVENT) ~ ., data = big_data)
summary(res.cox)

anaemia_df = with(big_data,
                  data.frame(anaemia = c(0,1),
                             age = rep(mean(age), 2),
                             creatinine_phosphokinase = rep(mean(creatinine_phosphokinase), 2),
                             diabetes = c(0,0),
                             ejection_fraction = rep(mean(ejection_fraction), 2),
                             high_blood_pressure = c(0,0),
                             platelets = rep(mean(platelets), 2),
                             serum_creatinine = rep(mean(serum_creatinine), 2),
                             serum_sodium = rep(mean(serum_sodium), 2),
                             sex = c(0,0),
                             smoking = c(0,0)
                             )
                  )

diabetes_df = with(big_data,
                  data.frame(anaemia = c(0,0),
                             age = rep(mean(age), 2),
                             creatinine_phosphokinase = rep(mean(creatinine_phosphokinase), 2),
                             diabetes = c(0,1),
                             ejection_fraction = rep(mean(ejection_fraction), 2),
                             high_blood_pressure = c(0,0),
                             platelets = rep(mean(platelets), 2),
                             serum_creatinine = rep(mean(serum_creatinine), 2),
                             serum_sodium = rep(mean(serum_sodium), 2),
                             sex = c(0,0),
                             smoking = c(0,0)
                             )
                  )

high_bp_df = with(big_data,
                  data.frame(anaemia = c(0,0),
                             age = rep(mean(age), 2),
                             creatinine_phosphokinase = rep(mean(creatinine_phosphokinase), 2),
                             diabetes = c(0,0),
                             ejection_fraction = rep(mean(ejection_fraction), 2),
                             high_blood_pressure = c(0,1),
                             platelets = rep(mean(platelets), 2),
                             serum_creatinine = rep(mean(serum_creatinine), 2),
                             serum_sodium = rep(mean(serum_sodium), 2),
                             sex = c(0,0),
                             smoking = c(0,0)
                             )
                  )

sex_df = with(big_data,
                  data.frame(anaemia = c(0,0),
                             age = rep(mean(age), 2),
                             creatinine_phosphokinase = rep(mean(creatinine_phosphokinase), 2),
                             diabetes = c(0,0),
                             ejection_fraction = rep(mean(ejection_fraction), 2),
                             high_blood_pressure = c(0,0),
                             platelets = rep(mean(platelets), 2),
                             serum_creatinine = rep(mean(serum_creatinine), 2),
                             serum_sodium = rep(mean(serum_sodium), 2),
                             sex = c(0,1),
                             smoking = c(0,0)
                             )
                  )

smoking_df = with(big_data,
                  data.frame(anaemia = c(0,1),
                             age = rep(mean(age), 2),
                             creatinine_phosphokinase = rep(mean(creatinine_phosphokinase), 2),
                             diabetes = c(0,0),
                             ejection_fraction = rep(mean(ejection_fraction), 2),
                             high_blood_pressure = c(0,0),
                             platelets = rep(mean(platelets), 2),
                             serum_creatinine = rep(mean(serum_creatinine), 2),
                             serum_sodium = rep(mean(serum_sodium), 2),
                             sex = c(0,0),
                             smoking = c(0,1)
                             )
                  )


fit_anemia = survfit(res.cox, newdata = anaemia_df)
fit_diabetes = survfit(res.cox, newdata = diabetes_df)
fit_high_bp = survfit(res.cox, newdata = high_bp_df)
fit_sex = survfit(res.cox, newdata = sex_df)
fit_smoking = survfit(res.cox, newdata = smoking_df)

ggsurvplot(fit_anemia, data = anaemia_df, coef.int = TRUE, legend.labs = c("Not Anaemic", "Anaemic"))
ggsurvplot(fit_diabetes, data = diabetes_df, coef.int = TRUE, legend.labs = c("Not Diabetic", "Diabetic"))
ggsurvplot(fit_high_bp, data = high_bp_df, coef.int = TRUE, legend.labs = c("Regular", "High Blood Pressure"))
ggsurvplot(fit_sex, data = sex_df, coef.int = TRUE, legend.labs = c("Female", "Male"))
ggsurvplot(fit_smoking, data = smoking_df, coef.int = TRUE, legend.labs = c("Non Smoker", "Smoker"))



