set.seed(300)

## Delhin
delhin_total_men <- 893
delhin_total_women <- 166
delhin_total <-delhin_total_men+delhin_total_women

delhin_data <- data.frame(CP = rep(c("Church counseling", "Psychotherapy","Support groups","Group therapy","Group retreats", "Psychiatry", "Family therapy"), each =2),
                          gender = rep(c("Men", "Women"), 7),
                          n = c(448,54,330,34,138,7,126,6,56,3,33,2,34,1),
                          mean = c(21.1,21.61,24.29,23.11,28.34,26.29,27.93,32,29.88,26.33,25.52,25.5,24.42,21),
                          sd = c(7.86,7.25,9.06,6.75,10.16,6.55,10.44,9.1,11.18,3.51,10.73,3.54,9.21,0.01))



overall_mean <- sum(delhin_data$mean * delhin_data$n) / sum(delhin_data$n)
pooled_variance <- (sum((delhin_data$n - 1) * delhin_data$sd^2) + 
                      sum(delhin_data$n * (delhin_data$mean_age - overall_mean)^2)) / (sum(delhin_data$n) - 1)
pooled_sd <- sqrt(pooled_variance)
z_score <- (18 - overall_mean) / pooled_sd
cumulative_prob <- pnorm(z_score)
proportion_adult <- 1 - cumulative_prob
delhin_adults <- round(619*proportion_adult)
delhin_underage <- 619 - delhin_adults

# Meanley
meanly_ages <- rnorm(219, 22.67,10.56)
meanly_adults <- table(meanly_ages>=18)[2]
meanly_underage <- table(meanly_ages>=18)[1]

#Three final
three_data <- data.frame(CP = c("Delhin", "Meanley","Seiller", "Lobacco"),
                          n = c(619,219,30,24),
                          mean = c(overall_mean,22.67,28,17.7),
                          sd = c(pooled_sd,10.56,NA,NA))

three_mean <- sum(three_data$mean * three_data$n) / sum(three_data$n)
