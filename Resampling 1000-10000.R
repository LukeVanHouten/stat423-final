
set.seed(123)
R <- 1000
boot_index_1000 <- sample(nrow(heart_new), R, replace=TRUE)
boot_heart_1000 <- heart_new[boot_index_1000,]
# boot_heart_1000$n <- 1:R

R <- 10000
boot_index_10000 <- sample(nrow(heart_new), R, replace=TRUE)
boot_heart_10000 <- heart_new[boot_index_10000,]
# boot_heart_10000$n <- 1:R
