dmf <- c(236,246,252,258,281,303,323,343,412,444,556,652,673,703,706,722,733,772,810,823,1027)
fconc <- c(1.9,2.6,1.8,1.2,1.2,1.2,1.3,0.9,0.6,0.5,0.4,0.3,0.0,0.2,0.1,0.0,0.2,0.1,0.0,0.1,0.1)

postscript("DMF-scatter.eps",horiz=F)
plot(fconc,dmf,xlab="Fluoride concentration",ylab="Teeth Decayed, Missing, or Filled per 100 children",pch=19)
dev.off()
postscript("DMF-scatterLine.eps",horiz=F)
plot(fconc,dmf,xlab="Fluoride concentration",ylab="Teeth Decayed, Missing, or Filled per 100 children",pch=19)
abline(lm(dmf~fconc),col=4,lwd=3)
dev.off()

mean(dmf)
mean(fconc)


# Hand calculations -------------------------------------------------------

n <- length(dmf)
SS.dmf <- sum( (dmf - mean(dmf))^2 )        ## SS.yy
SS.fconc <- sum( (fconc - mean(fconc))^2 )  ## SS.xx
SS.dmf.fconc <- sum( (dmf - mean(dmf)) * (fconc - mean(fconc)) ) ## SS.xy

# Estimate of the slope
beta1.hat <- SS.dmf.fconc/SS.fconc          ## SS.xy/SS.xx
beta1.hat 
# Estimate of the intercept
beta0.hat <- mean(dmf) - beta1.hat*mean(fconc)
beta0.hat 

fitted.dmf <- beta0.hat + beta1.hat*fconc

# Sum of squared errors
SSE <- sum( (dmf-fitted.dmf)^2 )

# Total sum of squares
SST <- sum( (dmf - mean(dmf))^2 )

# Model sum of squares
SSM <- beta1.hat*SS.dmf.fconc # beta1.hat*SS.xy

# Estimate of the residual standard error
sigma.hat <- sqrt(SSE/(n-2))
sigma.hat

sqrt( (SST- SSM)/(n-2) )

# The two give the same answer, because SST - SSM = SSE, or SST= SSM + SSE

# Computing the least-squares line of best fit with lm() ------------------

dmf.model <- lm(dmf~fconc)
summary(dmf.model)

# Other way of calculating fitted/predicted values
fitted.dmf <- fitted(dmf.model)
# manually:
# fitted.dmf <- beta0.hat + beta1.hat*fconc


# 3 - Confidence interval for the slope parameter -----------------------------

# Manually
ci.slope <- dmf.model$coefficients[2] + c(-1,1)*qt(0.975, df = n-2)*sigma.hat/sqrt(SS.fconc)
ci.slope

# Using a built-in function
confint(dmf.model,level = 0.95)

# 4 - Prediction of the average for x = 1.8ppm ----------------------------

predmean <- beta0.hat + beta1.hat*1.8
predmean

# 5- Confidence interval for the average given x=1.8ppm -------------------

se.predmean <- sigma.hat*sqrt(1/n + (1.8 - mean(fconc))^2/SS.fconc)
ci.predmean <- predmean + c(-1,1)*qt(0.975, df = n-2)*se.predmean
ci.predmean

# using the built-in function predict()
predict(dmf.model, newdata = data.frame(fconc = 1.8), interval = "confidence",
        level = 0.95)

# 95% prediction interval 
predict(dmf.model, newdata = data.frame(fconc = 1.8), interval = "prediction",
        level = 0.95)

# notice that the prediction interval is wider


# Plot comparing confidence and prediction bands over the range of --------

newx = seq(min(fconc),max(fconc),by = 0.05)
conf_interval <- predict(dmf.model, newdata=data.frame(fconc=newx), interval="confidence",
                         level = 0.95)
pred_interval <- predict(dmf.model, newdata=data.frame(fconc=newx), interval="prediction",
                         level = 0.95)

postscript("DMF-conf.eps",horiz=T)
plot(fconc, dmf, xlab="x", ylab="y", main="Regression",
     ylim = c(min(dmf), max(dmf)))
abline(a=beta0.hat, b = beta1.hat, col="lightblue")
lines(newx, conf_interval[,2], col="blue", lty=2)
lines(newx, conf_interval[,3], col="blue", lty=2)
lines(newx, pred_interval[,2], col="orange", lty=2)
lines(newx, pred_interval[,3], col="orange", lty=2)
legend(legend = c("Prediction interval", "Confidence interval"),
       x = 1.0, y =1000, col = c("orange", "blue"), lty = 2, bty = "n")
dev.off()

# Diagnostics -------------------------------------------------------------

# estimated residuals
raw.resid <- dmf - fitted.dmf
postscript("DMF-resid1.eps",horiz=F)
plot(fconc,raw.resid,pch=19,xlab="Fluoride concentration",ylab="Raw residuals")
abline(0,0,lwd=2)
lines(lowess(fconc,raw.resid),col=2,lwd=2)
dev.off()
postscript("DMF-resid2.eps",horiz=F)
plot(fitted.dmf,raw.resid,pch=19,xlab="Fitted values",ylab="Raw residuals")
abline(0,0,lwd=2)
lines(lowess(fitted.dmf,raw.resid),col=2,lwd=2)
dev.off()
hist(raw.resid,nclass = 8,xlab="Raw residuals")
boxplot(raw.resid,ylab="Raw residuals")
abline(0,0,lwd=2,col=4)

# Alternatively, you can also use plot(dmf.model)

plot(dmf.model)


# Using ANOVA to test whether the slope is equal to 0 ---------------------
# (approach 3)

anova(dmf.model)

# Notice how the test p-value is identical to the one displayed in the summary
# of lm() which uses the t-test (approach 2)

summary(dmf.model)

# This is what we would expect.


# Relationship between the correlation coefficient and R2 -----------------

R <- cor(fconc, dmf)
# R: -0.859

# Coefficient of determination or Multiple R-squared
R2 <- 1-SSE/SS.dmf
# R2: 0.7378
# It can also be found in the regression summary
summary(dmf.model)

# It corresponds to the square of the correlation coefficient:
R^2

# NOTE: This relationship only holds in the simple linear regression setting!