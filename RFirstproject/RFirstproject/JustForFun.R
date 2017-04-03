# normal distribution

norm_numbers <- rnorm(1000)

boxplot(norm_numbers)

hist(norm_numbers)

# BINOMIAL DISTRIBUTION

b_numbers <- rbinom(10000, size = 100, prob = 0.1)

boxplot(b_numbers)

hist(b_numbers)

# poissone distribution

p_numbers <- rpois(10000, 1)

boxplot(p_numbers)

hist(p_numbers)

# exponential distribution

e_numbers <- rexp(10000, 1)

boxplot(e_numbers)

hist(e_numbers)
