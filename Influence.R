n <- 99
x <- rnorm(n,5,1)
y <- rnorm(n,10,1)
x <- c(x,24)
y <- c(y,8)


plot(x,y,xlim=c(-5,25),ylim=c(5,15),pch=19)
summary(lm(y~x))
abline(lm(y~x),col=2,lwd=2)
summary(lm(y[x<20]~x[x<20]),col=4,lwd=2)
abline(lm(y[x<20]~x[x<20]),col=4,lwd=2)


#postscript("Influence1.eps",horiz=F)
#postscript("Influence2.eps",horiz=F)
plot(x,y,xlim=c(-5,25),ylim=c(5,15),pch=19)
summary(lm(y~x))
abline(lm(y~x),col=2,lwd=2)
#dev.off()
summary(lm(y[x<20]~x[x<20]),col=4,lwd=2)
abline(lm(y[x<20]~x[x<20]),col=4,lwd=2)
#dev.off()

log.x <- log(x)
#postscript("Influence3.eps",horiz=F)
plot(log.x,y,xlab="log(x)",ylim=c(5,15),pch=19)
summary(lm(y~log.x))
abline(lm(y~log.x),col=2,lwd=2)
abline(lm(y[x<20]~log.x[x<20]),col=4,lwd=2)
#dev.off()

### Outlier in the other direction
XX <- y
YY <- x
XX[100] <- 8.75
#postscript("Influence4.eps",horiz=F)
plot(XX,YY,xlab="x",ylab="y",ylim=c(-5,25),xlim=c(5,15),pch=19)
summary(lm(YY~XX))
abline(lm(YY~XX),col=2,lwd=2)
summary(lm(YY[YY<20]~XX[YY<20]),col=4,lwd=2)
abline(lm(YY[YY<20]~XX[YY<20]),col=4,lwd=2)
#dev.off()




