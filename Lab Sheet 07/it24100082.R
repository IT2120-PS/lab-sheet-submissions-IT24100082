getwd()
setwd("C:\\Users\\User\\Desktop\\it24100082")


a <- 10
b <- 25
lower <- 0
upper <- 40
p_uniform <- (b - a) / (upper - lower)
cat("between 8:10 and 8:25 =", p_uniform)




lambda <- 1/3
p_exp <- pexp(2, rate = lambda)
cat("at most 2 hours =", p_exp)

mu <- 100
sigma <- 15
#1
p_norm_above130 <- 1 - pnorm(130, mean = mu, sd = sigma)
cat("IQ > 130 =", p_norm_above130)
#2
q_norm_95 <- qnorm(0.95, mean = mu, sd = sigma)
cat("95th percentile =", q_norm_95)