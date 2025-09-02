getwd()
setwd("C:\\Users\\User\\Desktop\\IT24100082")

n <- 50
p <- 0.85


cat("X ~ Binomial(50, 0.85)\n")


prob_47_or_more <- sum(dbinom(47:50, size=n, prob=p))
prob_47_or_more


lambda <- 12
cat("X = Number of calls received in 1 hour\n")
cat("X ~ Poisson(12)\n")
prob_15 <- dpois(15, lambda=lambda)
prob_15
