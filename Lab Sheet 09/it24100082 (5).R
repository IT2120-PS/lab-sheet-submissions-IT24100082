setwd("C:\\Users\\User\\Desktop\\it24100082")
getwd()
# Generate a random sample of size 25 for the baking time
set.seed(123)
baking_time <- rnorm(25, mean = 45, sd = 2)
baking_time

# One-sample t-test to test if the mean baking time is less than 46 minutes
t_test_result <- t.test(baking_time, mu = 46, alternative = "less", conf.level = 0.95)

t_test_result