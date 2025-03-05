
data <- read.csv("C:/Users/Justin/Downloads/heart_failure_clinical_records_dataset.csv")
m = 500
n = 299

big_data = data.frame()

for(i in 1:m){
  indecies = sample(c(1:n), n, replace = TRUE)
  big_data = rbind(big_data, data[indecies, ])
}

