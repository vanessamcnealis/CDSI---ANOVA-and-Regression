#uncomment to install the package
#install.packages("alr4")
library(alr4)
head(fuel2001)
fuel2001$Fuel <- 1000*fuel2001$FuelC/fuel2001$Pop
fuel2001$Dlic <- 1000*fuel2001$Drivers/fuel2001$Pop
fuel2001$logMiles <- log(fuel2001$Miles)

# Scatterplot matrix

postscript("Fuel-matrix.eps",horiz=T)
pairs(fuel2001[,c("Fuel", "Tax", "Dlic", "Income", "logMiles")])
dev.off()

# Find the least-squares linear regression fit
fit <- lm(Fuel ~ Tax + Dlic + Income + logMiles, data = fuel2001)
summary(fit)

# Plot the residuals versus fitted values
fuel_fitted <- fitted(fit)
resid <- residuals(fit)

postscript("Fuel-resid.eps",horiz=T)
plot(fuel_fitted, resid, xlab = "Fitted values", ylab = "Residuals")
abline(0,0,lwd=2)
lines(lowess(fuel_fitted,resid),col=2,lwd=2)
dev.off()
