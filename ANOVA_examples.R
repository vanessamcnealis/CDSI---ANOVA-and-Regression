# ANOVA examples


# First example -----------------------------------------------------------
# The true means are all equal to 0.

level1 <- c(-0.88, 0.24, -0.46, 0.78, -0.47, -0.38)
level2 <- c(-0.75, 0.11, 0.64, 1.98, -1.03, 1.84)
level3 <- c(1.38, 1.20, 0.42, 0.05, -1.29, -0.04)
n1 <- 6
n2 <- 6
n3 <- 6
k <- 3
n <- n1+n2+n3

mean1 <- mean(level1)
var1 <- var(level1)

mean2 <- mean(level2)
var2 <- var(level2)

mean3 <- mean(level3)
var3 <- var(level3)

mean <- mean(c(level1, level2, level3))

# Using the formulas on slide 89: -----------------------------------------

SSE <- sum((level1 - mean1)^2) + sum((level2 - mean2)^2) + sum((level3 - mean3)^2) 

# alternatively
# SSE <- (n1-1)*var1 + (n2-1)*var2 + (n3-1)*var3

SST <- sum((c(level1, level2, level3)-mean)^2)

SSM <- sum(n1*(mean1-mean)^2 + n2*(mean2-mean)^2 + n3*(mean3-mean)^2)

# alternatively
# SSM <- SST - SSE

F.statistic <- (SSM/(k-1))/(SSE/(n-k))
# 0.722

# 95%-level quantile of the F(k-1,n-k) distribution
qf(0.95, df1 = k-1, df2 = n-k)

# Since 0.722 < 3.68, we do not reject the hypothesis of no difference
# between the group means

# Using the lm() and anova() functions ------------------------------------

# Create a data frame
dat <- rbind(data.frame(y = level1, x = 1),
             data.frame(y = level2, x = 2),
             data.frame(y = level3, x = 3))

# declare x as an unordered factor variable
dat$x <- as.factor(dat$x)

fit1 <- lm(y ~ x, data = dat)
summary(fit1)
# Notice that the intercept corresponds to the mean of the first level (mean1)


# notice that the residual standard error is 0.9845
# this is exactly sqrt(SSE/(n-k)):
sqrt(SSE/(n-k))

# Output the anova table:
anova(fit1)

# We obtain the same F-statistic value and achieve the same conclusion as before.



# Second example ----------------------------------------------------------
# mu1 = 0, mu2 = 10, mu3 = 20

level1 <- c(-0.88, 0.24, -0.46, 0.78, -0.47, -0.38)
level2 <- c(9.25, 10.11, 10.64, 11.98, 8.97, 11.84)
level3 <- c(21.38, 21.20, 20.42, 20.05, 18.71, 19.96)
n1 <- 6
n2 <- 6
n3 <- 6
k <- 3
n <- n1+n2+n3


mean1 <- mean(level1)
var1 <- var(level1)

mean2 <- mean(level2)
var2 <- var(level2)

mean3 <- mean(level3)
var3 <- var(level3)

mean <- mean(c(level1, level2, level3))

# Using the formulas on slide 89: -----------------------------------------

SSE <- sum((level1 - mean1)^2) + sum((level2 - mean2)^2) + sum((level3 - mean3)^2) 
# SSE: 14.538 (same as in the first example)

# alternatively
# SSE <- (n1-1)*var1 + (n2-1)*var2 + (n3-1)*var3

SST <- sum((c(level1, level2, level3)-mean)^2)

SSM <- sum(n1*(mean1-mean)^2 + n2*(mean2-mean)^2 + n3*(mean3-mean)^2)

# alternatively
# SSM <- SST - SSE

F.statistic <- (SSM/(k-1))/(SSE/(n-k))
# 649.5701

# 95%-level quantile of the F(k-1,n-k) distribution
qf(0.95, df1 = k-1, df2 = n-k)

# Since 649.570 > 3.68, we reject the hypothesis of no difference
# between the group means

# Using the lm() and anova() functions ------------------------------------

# Create a data frame
dat <- rbind(data.frame(y = level1, x = 1),
             data.frame(y = level2, x = 2),
             data.frame(y = level3, x = 3))

# declare x as an unordered factor variable
dat$x <- as.factor(dat$x)

fit1 <- lm(y ~ x, data = dat)
summary(fit1)

# Notice that the intercept corresponds to the mean of the first level (mean1)

# notice that the residual standard error is 0.9845
# this is exactly sqrt(SSE/(n-k)):
sqrt(SSE/(n-k))

# Output the anova table:
anova(fit1)

# We obtain the same F-statistic value and achieve the same conclusion as before.


