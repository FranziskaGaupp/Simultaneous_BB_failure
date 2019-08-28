
# Threshold estimation using linear regression between climate indicators and detrended yields 
# ...climate_all refers to climate indicator set with precip based indicators where possible
# version 2 refers to climate indicator set with temperature based indicators where possible

### wheat ######
setwd("~/Data collection")

res <- read.table("resid_wheat_1967_2012_github_v2.csv", sep=",", header=T) 
clim <- read.table("wheat_climate_all_github_v2.csv", sep=",", header=T)
# without years
clim <- clim[,-1]
# without Western Australia
res <- res[,-30]

plot(res[,14], clim[,14], xlab="Wheat yield residuals", ylab="Climate indicator",
     pch=16)
abline(v=0, lty=2)

# threshold: the lowest 25% percentile of detrended wheat yields

THRES25 <- matrix(nrow=1,ncol=37)
Q25 <- matrix(nrow=1,ncol=37)
i=24
for (i in 1:37){
lin <- lm(formula = clim[,i] ~ res[,i])
plot (res[,i], clim[,i])
abline(lm(clim[,i] ~ res[,i]), col="red")

q <- quantile(res[,i], probs=0.25, na.rm=T)
thres25 <- lin$coefficients[1] +lin$coefficients[2]*q
THRES25[,i] <- thres25
Q25[,i] <- q
}

colnames(THRES25)<- colnames(res[,1:37])
colnames(Q25)<- colnames(res[,1:37])

write.table(THRES25, "wheat_threshold25_github_v2.csv", sep=",",row.names=F)
write.table(Q25, "wheat_q25_github_v2.csv", sep=",",row.names=F)



# R^2
i=12
R2 <- matrix(nrow=1,ncol=37)
for (i in 1:37){
  lin <- lm(formula = clim[,i] ~ res[,i])
  #abline(lin, lty=2)
  sum <- summary (lin)
  R2[,i] <- sum$r.squared
  #anova(lin)
  
}
colnames(R2)<- colnames(res[,1:37])

write.table(R2, "wheat_threshold25_github_R2.csv", sep=",",row.names=F)


### wheat version 2 ######

setwd("~/Data collection")
res <- read.table("resid_wheat_1967_2012_github_v2.csv", sep=",", header=T)
clim <- read.table("wheat_climate_all_version_2_github_v2.csv", sep=",", header=T)
# without years
clim <- clim[,-1]
# without Western Australia
res <- res[,-30]

plot(res[,2], clim[,2], xlab="Wheat yield residuals", ylab="Climate indicator",
     pch=16)
abline(v=0, lty=2)


THRES25 <- matrix(nrow=1,ncol=37)
Q25 <- matrix(nrow=1,ncol=37)

i=14
for (i in 1:37){
  lin <- lm(formula = clim[,i] ~ res[,i])
  #abline(lin, lty=2)
  summary (lin)
  #anova(lin)

  q <- quantile(res[,i], probs=0.25, na.rm=T)
  thres25 <- lin$coefficients[1] +lin$coefficients[2]*q
  THRES25[,i] <- thres25
  Q25[,i] <- q
}

colnames(THRES25)<- colnames(res[,1:37])
colnames(Q25)<- colnames(res[,1:37])

write.table(THRES25, "wheat_threshold25_version_2_github_v2.csv", sep=",",row.names=F)


R^2
i=12
R2 <- matrix(nrow=1,ncol=37)
for (i in 1:37){
  lin <- lm(formula = clim[,i] ~ res[,i])
  #abline(lin, lty=2)
  sum <- summary (lin)
  R2[,i] <- sum$r.squared
  #anova(lin)
  
}
colnames(R2)<- colnames(res[,1:37])

write.table(R2, "wheat_threshold25_version2_github_R2.csv", sep=",",row.names=F)


# wheat BB scale ##########

setwd("~/Data collection")
res <- read.table("wheat_res_bb_github_weighted_v2.csv", sep=",", header=T)
setwd("~/Analysis/BB climate indicators")
clim <- read.csv("wheat_climate_indicators_BB_version3_github_weighted.csv", header=T, sep=",")
# without years
clim <- clim[,-1] 
res <- res[,-1]

# version 1, mostly precip indicators
r=c(2,3,4,1,6,8,10)
clim <- cbind(clim[,r])

THRES25 <- matrix(nrow=1,ncol=7)
Q25 <- matrix(nrow=1,ncol=7)

i=14
for (i in 1:7){
  lin <- lm(formula = clim[,i] ~ res[,i])
  #abline(lin, lty=2)
  summary (lin)
  #anova(lin)
  
  q <- quantile(res[,i], probs=0.25, na.rm=T)
  thres25 <- lin$coefficients[1] +lin$coefficients[2]*q
  THRES25[,i] <- thres25
  Q25[,i] <- q
}

colnames(THRES25)<- colnames(clim[,1:7])
colnames(Q25)<- colnames(clim[,1:7])




plot.add.ci <- function(x, y, interval='confidence', level=0.9, regressionColor= "black",...) {
  xOrder  <- order(x)
  x       <- x[xOrder]  
  y       <- y[xOrder]
  fit     <- lm(y ~ x, data=data.frame(x=x, y=y))
  newX    <- data.frame(x=jitter(x))
  fitPred <- predict.lm(fit, newdata=newX, interval=interval, level=level, ...)
  abline(lm(y ~ x), col=regressionColor, lwd=2)
  lines(newX$x, fitPred[,2], lty=2, ...)
  lines(newX$x, fitPred[,3], lty=2, ...)
}


l=2
# to calculate confidence intervals

xvalue <- matrix(nrow=3,ncol=7)

for ( l in 1:7){
  #x = na.omit(res[,l])
  #y = na.omit(clim[,l])
  x = res[,l]
  y = clim[,l]
  xOrder  <- order(x)
  x       <- x[xOrder]
  y       <- y[xOrder]
  fit     <- lm(y ~ x, data=data.frame(x=x, y=y))
  newX    <- data.frame(x=jitter(x))
  fitPred <- predict.lm(fit, newdata=newX, interval='confidence', level=0.9)
  
  
  #frame <-cbind(na.omit(newX), na.omit(fitPred))
  
  vec = seq(from=-3, to=1, by= 0.01)
  table <- matrix(nrow=401,ncol=3)
  for ( i in 1:401){
    p = predict(fit, newdata=data.frame(x=vec[i]), interval="confidence", level=0.9)
    table[i,1]<-p[1]
    table[i,2]<-p[2]
    table[i,3]<-p[3]
  }
  
  Table=cbind(vec,table)
  
  n <- which.min(abs(Table[,3] - THRES25[1,l])) 
  xvalue[1,l] <-Table[n,1]
  n <- which.min(abs(Table[,4] - THRES25[1,l])) 
  xvalue[2,l] <-Table[n,1]
  n <- which.min(abs(Table[,2] - THRES25[1,l])) 
  xvalue[3,l] <-Table[n,1]
  
}

colnames(xvalue)<-colnames(THRES25)

write.table(xvalue, "wheat_q25_0.9_confidence_version1_github.csv", sep=",",row.names=F)
write.table(xvalue, "wheat_q25_0.9_confidence_version2_github.csv", sep=",",row.names=F)


x = res[,2];   
y = clim[,2]
plot(x, y, pch=16, xlab="Wheat yield residuals", ylab="SPI") #,xlim= c(-2.5,2))
plot.add.ci(x, y, col="#958A86", lwd=2, xlim=c(-2.5,5))
abline(v=Q25[,2])


# threshold prediction interval

plot.add.ci <- function(x, y, interval='prediction', level=0.9, regressionColor= "black",...) {
  xOrder  <- order(x)
  x       <- x[xOrder]  
  y       <- y[xOrder]
  fit     <- lm(y ~ x, data=data.frame(x=x, y=y))
  newX    <- data.frame(x=jitter(x))
  fitPred <- predict.lm(fit, newdata=newX, interval=interval, level=level, ...)
  abline(lm(y ~ x), col=regressionColor, lwd=2)
  lines(newX$x, fitPred[,2], lty=2, ...)
  lines(newX$x, fitPred[,3], lty=2, ...)
}


l=2
# to calculate prediction intervals

xvalue <- matrix(nrow=3,ncol=37)

for ( l in 1:37){
  #x = na.omit(res[,l])
  #y = na.omit(clim[,l])
  x = res[,l]
  y = clim[,l]
  xOrder  <- order(x)
  x       <- x[xOrder]
  y       <- y[xOrder]
  fit     <- lm(y ~ x, data=data.frame(x=x, y=y))
  newX    <- data.frame(x=jitter(x))
  fitPred <- predict.lm(fit, newdata=newX, interval='prediction', level=0.9)
  

  
  #frame <-cbind(na.omit(newX), na.omit(fitPred))
  

    p = predict(fit, newdata=data.frame(x=Q25[1,l]), interval="prediction", level=0.9)
 
  
  xvalue[1,l] <-p[1,1]
  xvalue[2,l] <-p[1,2]
  xvalue[3,l] <-p[1,3]
  
}

colnames(xvalue)<-colnames(THRES25)
write.table(xvalue, "wheat_t25_0.9_prediction_version1_github.csv", sep=",",row.names=F)
write.table(xvalue, "wheat_t25_0.9_prediction_version2_github.csv", sep=",",row.names=F)

### maize ######

setwd("~/Data collection")
res <- read.table("residual_maize_github.csv", sep=",", header=T)
clim <- read.table("maize_climate_all_github_version1.csv", sep=",", header=T)


plot(res[,22], clim[,22], xlab="maize yield residuals", ylab="Climate indiactor",
     pch=16)
abline(v=0, lty=2)

# thresholds: the lowest 25% percentile of detrended maize yields

THRES25 <- matrix(nrow=1,ncol=39)
Q25 <-  matrix(nrow=1,ncol=39)

for (i in 2:40){
  lin <- lm(formula = clim[,i] ~ res[,i])
  #abline(lin, lty=2)
  q <- quantile(res[,i], probs=0.25, na.rm=T)
  thres25 <- lin$coefficients[1] +lin$coefficients[2]*q
  THRES25[,i-1] <- thres25
  Q25[,i-1]<- q
}

colnames(THRES25)<- colnames(res[,2:40])
colnames(Q25)<- colnames(res[,2:40])

write.table(THRES25, "maize_threshold25_github.csv", sep=",",row.names=F)
write.table(Q25, "maize_q25_github.csv", sep=",",row.names=F)


# an average of each breadbaskets thresholds can be found under 
#M:\franziska\DPhil\breadbaskets\Data collection\maize_threshold0_bb.csv


# R^2

R2 <- matrix(nrow=1,ncol=39)
for (i in 2:40){
  lin <- lm(formula = clim[,i] ~ res[,i])
  #abline(lin, lty=2)
  sum <- summary (lin)
  R2[,i-1] <- sum$r.squared
  #anova(lin)
  
}
colnames(R2)<- colnames(res[,2:40])

write.table(R2, "maize_threshold25_github_R2.csv", sep=",",row.names=F)



# maize version 2-----

setwd("~/Data collection")
res <- read.table("residual_maize_github.csv", sep=",", header=T)
clim <- read.table("maize_climate_all_github_version2.csv", sep=",", header=T)
# without years
clim <- clim[,-1]
res <- res[,-1]


THRES25 <- matrix(nrow=1,ncol=39)
Q25 <-  matrix(nrow=1,ncol=39)

for (i in 1:39){
  lin <- lm(formula = clim[,i] ~ res[,i])
  q <- quantile(res[,i], probs=0.25, na.rm=T)
  thres25 <- lin$coefficients[1] +lin$coefficients[2]*q
  THRES25[,i] <- thres25
  Q25[,i]<- q
}
colnames(THRES25)<- colnames(res)
colnames(Q25)<- colnames(res)

write.table(THRES25, "maize_threshold25_version_2_github.csv", sep=",",row.names=F)


# maize plot for SI Figure in paper 3: ##########

# use maize_version1 data

plot(res[,15], clim[,15], main="Santa Fe, Argentina",xlab="Maize yield residuals (t/ha)", ylab="Max. temperature Dec - Feb (°C)",
     pch=16, xlim= c(-2.5,2))
abline(v=-0.65, lty=2)
segments(-2.6,31.53, -0.65,31.53, lty=2)
i=14
text(-1.7,31.7,"threshold", cex=0.8)
lin <- lm(formula = clim[,15] ~ res[,15])

plot.add.ci <- function(x, y, interval='prediction', level=0.9, regressionColor= "black",...) {
  xOrder  <- order(x)
  x       <- x[xOrder]  
  y       <- y[xOrder]
  fit     <- lm(y ~ x, data=data.frame(x=x, y=y))
  newX    <- data.frame(x=jitter(x))
  fitPred <- predict.lm(fit, newdata=newX, interval=interval, level=level, ...)
  abline(lm(y ~ x), col=regressionColor, lwd=2)
  lines(newX$x, fitPred[,2], lty=2, ...)
  lines(newX$x, fitPred[,3], lty=2, ...)
}



# to calculate confidence intervals

xvalue <- matrix(nrow=3,ncol=39)

for ( l in 1:39){
x = res[,l]
y = clim[,l]
xOrder  <- order(x)
x       <- x[xOrder]
y       <- y[xOrder]
fit     <- lm(y ~ x, data=data.frame(x=x, y=y))
newX    <- data.frame(x=jitter(x))
fitPred <- predict.lm(fit, newdata=newX, interval='confidence', level=0.9)


#frame <-cbind(na.omit(newX), na.omit(fitPred))

vec = seq(from=-3, to=1, by= 0.01)
table <- matrix(nrow=401,ncol=3)
for ( i in 1:401){
p = predict(fit, newdata=data.frame(x=vec[i]), interval="confidence", level=0.9)
table[i,1]<-p[1]
table[i,2]<-p[2]
table[i,3]<-p[3]
}

Table=cbind(vec,table)

n <- which.min(abs(Table[,3] - THRES25[1,l])) 
xvalue[1,l] <-Table[n,1]
n <- which.min(abs(Table[,4] - THRES25[1,l])) 
xvalue[2,l] <-Table[n,1]
n <- which.min(abs(Table[,2] - THRES25[1,l])) 
xvalue[3,l] <-Table[n,1]

}

colnames(xvalue)<-colnames(THRES25)

write.table(xvalue, "maize_q25_0.9_confidence_version1_github.csv", sep=",",row.names=F)
write.table(xvalue, "maize_q25_0.9_confidence_version2_github.csv", sep=",",row.names=F)

l=7

x = res[4:46,15];   
y = clim[4:46,15]
plot(x, y,xlab="Maize yield residuals (t/ha)", ylab="Max. temperature Dec - Feb (°C)",
     pch=16,xlim= c(-2.5,2))
plot.add.ci(x, y, col="#958A86", lwd=2, xlim=c(-2.5,5))

abline(v=-0.65, lty=2)
segments(-2.7,31.53, -0.65,31.53, lty=2)
text(-1.7,31.8,"threshold", cex=0.8)

# confidence interval
segments(-2.6,30.12, -0.65,30.12, lty=2, lwd=2, col="#958A86")
segments(-2.6,32.88, -0.65,32.88, lty=2, lwd=2, col="#958A86")
text(-1.7,30.4," high-risk threshold", cex=0.8, col="#958A86")
text(-1.7,33.2,"low-risk threshold", cex=0.8, col="#958A86")



#prediction interval
segments(-2.6,31.53, -0.32,31.53, lty=2)
segments(-1.05,31.53, -1.05,0, lty=2, col="red", lwd=3)
segments(-0.32,31.53, -0.32,0, lty=2, col="red", lwd=3)


hist(res[,14], xlab="Maize yield residuals in Santa Fe 1967-2012 (t/ha)", freq=F,
     main="", xlim=c(-2.5, 2))
text(-0.8,0.5,"q25", cex=0.8)

set.seed(43)
library(boot)

x <- x[4:46]
y <- y[4:46]
sims <- boot(residuals(fit), function(r, i, d = data.frame(x, y), yhat = fitted(fit)) {
  
  d$y <- yhat + r[i]
  
  fitb <- lm(y ~ x, data = d)
  
  -coef(fitb)[1]/coef(fitb)[2]
}, R = 1e4)
quantile(sims$t, c(0.1, 0.9))
lines(quantile(sims$t, c(0.025, 0.975)), c(31.65, 31.65), col = "blue")



# threshold prediction interval

plot.add.ci <- function(x, y, interval='prediction', level=0.9, regressionColor= "black",...) {
  xOrder  <- order(x)
  x       <- x[xOrder]  
  y       <- y[xOrder]
  fit     <- lm(y ~ x, data=data.frame(x=x, y=y))
  newX    <- data.frame(x=jitter(x))
  fitPred <- predict.lm(fit, newdata=newX, interval=interval, level=level, ...)
  abline(lm(y ~ x), col=regressionColor, lwd=2)
  lines(newX$x, fitPred[,2], lty=2, ...)
  lines(newX$x, fitPred[,3], lty=2, ...)
}


l=2
# to calculate prediction intervals

xvalue <- matrix(nrow=3,ncol=39)

for ( l in 2:40){
  #x = na.omit(res[,l])
  #y = na.omit(clim[,l])
  x = res[,l-1]
  y = clim[,l-1]
  xOrder  <- order(x)
  x       <- x[xOrder]
  y       <- y[xOrder]
  fit     <- lm(y ~ x, data=data.frame(x=x, y=y))
  newX    <- data.frame(x=jitter(x))
  fitPred <- predict.lm(fit, newdata=newX, interval='prediction', level=0.9)
  
  
  
  #frame <-cbind(na.omit(newX), na.omit(fitPred))
  
  
  p = predict(fit, newdata=data.frame(x=Q25[1,l-1]), interval="prediction", level=0.9)
  
  
  xvalue[1,l-1] <-p[1,1]
  xvalue[2,l-1] <-p[1,2]
  xvalue[3,l-1] <-p[1,3]
  
}

colnames(xvalue)<-colnames(THRES25)
write.table(xvalue, "maize_t25_0.9_prediction_version1_github.csv", sep=",",row.names=F)
write.table(xvalue, "maize_t25_0.9_prediction_version2_github.csv", sep=",",row.names=F)



#### soybean #######

setwd("~/Data collection")
res <- read.table("resid_soybean.csv", sep=",", header=T)
clim <- read.table("soybean_climate_all.csv", sep=",", header=T)

# soybean from 1967 to 2012
res <- res[-47,]

plot(res[,37], clim[,37], xlab="Soybean yield residuals", ylab="Cumulative precipitation during growing season (mm)", ycex=0.8,
     pch=16, main="Santa Fe province, Argentina")
abline(v=-0.217, lty=2)
abline(h=391, lty=2)
y= cbind(clim[1:14,13], rep(-0.0991,14))
points(y[,2], y[,1], pch=16)
hist(res[,37], xlab="Soybean yield residuals in Santa Fe 1967-2012", freq=F, main="", xlim=c(-1,1))
text(-0.3,1.2,"q25", cex=0.8)
segments(-1,572,-0.217,572, lty=2)
text(-0.7,600,"threshold", cex=0.8)

i=37
THRES25 <- matrix(nrow=1,ncol=36)
Q25 <- matrix(nrow=1,ncol=36)

for (i in 2:37){
  lin <- lm(formula = clim[,i] ~ res[,i])
  #abline(lin, lty=2)
  q <- quantile(res[,i], probs=0.25, na.rm=T)
  thres25 <- lin$coefficients[1] +lin$coefficients[2]*q
  THRES25[,i-1] <- thres25
  Q25[,i-1]<- q
}
colnames(THRES25)<- colnames(res[,2:37])
colnames(Q25)<- colnames(res[,2:37])

write.table(THRES25, "soybean_threshold25.csv", sep=",",row.names=F)
write.table(Q25, "soybean_q25.csv", sep=",",row.names=F)




## soybean version 2 ----------

setwd("~/Data collection")
res <- read.table("resid_soybean.csv", sep=",", header=T)
clim <- read.table("soybean_climate_all_version_2.csv", sep=",", header=T)

# soybean from 1967 to 2012
res <- res[-47,]

plot(res[,9], clim[,9], xlab="soybean yield residuals", ylab="Climate indiactor",
     pch=16)
abline(v=0, lty=2)

# two thresholds: 0 yield deviation and the lowest 25% percentile of wheat yields
THRES25 <- matrix(nrow=1,ncol=36)
Q25 <- matrix(nrow=1,ncol=36)

for (i in 2:37){
  lin <- lm(formula = clim[,i] ~ res[,i])
  #abline(lin, lty=2)
  q <- quantile(res[,i], probs=0.25, na.rm=T)
  thres25 <- lin$coefficients[1] +lin$coefficients[2]*q
  THRES25[,i-1] <- thres25
  Q25[,i-1]<- q
}
colnames(THRES25)<- colnames(res[,2:37])
colnames(Q25)<- colnames(res[,2:37])


write.table(THRES25, "soybean_threshold25_version2.csv", sep=",",row.names=F)


# uncertainty analysis: confidence interval


plot.add.ci <- function(x, y, interval='confidence', level=0.9, regressionColor= "black",...) {
  xOrder  <- order(x)
  x       <- x[xOrder]  
  y       <- y[xOrder]
  fit     <- lm(y ~ x, data=data.frame(x=x, y=y))
  newX    <- data.frame(x=jitter(x))
  fitPred <- predict.lm(fit, newdata=newX, interval=interval, level=level, ...)
  abline(lm(y ~ x), col=regressionColor, lwd=2)
  lines(newX$x, fitPred[,2], lty=2, ...)
  lines(newX$x, fitPred[,3], lty=2, ...)
}



# to calculate confidence intervals

xvalue <- matrix(nrow=3,ncol=36)

for ( l in 2:37){
  #x = na.omit(res[,l])
  #y = na.omit(clim[,l])
  x = res[,l]
  y = clim[,l]
  xOrder  <- order(x)
  x       <- x[xOrder]
  y       <- y[xOrder]
  fit     <- lm(y ~ x, data=data.frame(x=x, y=y))
  newX    <- data.frame(x=jitter(x))
  fitPred <- predict.lm(fit, newdata=newX, interval='confidence', level=0.9)
  
  
  #frame <-cbind(na.omit(newX), na.omit(fitPred))
  
  vec = seq(from=-3, to=1, by= 0.01)
  table <- matrix(nrow=401,ncol=3)
  for ( i in 1:401){
    p = predict(fit, newdata=data.frame(x=vec[i]), interval="confidence", level=0.9)
    table[i,1]<-p[1]
    table[i,2]<-p[2]
    table[i,3]<-p[3]
  }
  
  Table=cbind(vec,table)
  
  n <- which.min(abs(Table[,3] - THRES25[1,l-1])) 
  xvalue[1,l-1] <-Table[n,1]
  n <- which.min(abs(Table[,4] - THRES25[1,l-1])) 
  xvalue[2,l-1] <-Table[n,1]
  n <- which.min(abs(Table[,2] - THRES25[1,l-1])) 
  xvalue[3,l-1] <-Table[n,1]
  
}

colnames(xvalue)<-colnames(THRES25)

write.table(xvalue, "soybean_q25_0.9_confidence_version1_github.csv", sep=",",row.names=F)
write.table(xvalue, "soybean_q25_0.9_confidence_version2_github.csv", sep=",",row.names=F)


# threshold prediction interval

plot.add.ci <- function(x, y, interval='prediction', level=0.9, regressionColor= "black",...) {
  xOrder  <- order(x)
  x       <- x[xOrder]  
  y       <- y[xOrder]
  fit     <- lm(y ~ x, data=data.frame(x=x, y=y))
  newX    <- data.frame(x=jitter(x))
  fitPred <- predict.lm(fit, newdata=newX, interval=interval, level=level, ...)
  abline(lm(y ~ x), col=regressionColor, lwd=2)
  lines(newX$x, fitPred[,2], lty=2, ...)
  lines(newX$x, fitPred[,3], lty=2, ...)
}


l=2
# to calculate prediction intervals

xvalue <- matrix(nrow=3,ncol=36)

for ( l in 2:37){
  #x = na.omit(res[,l])
  #y = na.omit(clim[,l])
  x = res[,l]
  y = clim[,l]
  xOrder  <- order(x)
  x       <- x[xOrder]
  y       <- y[xOrder]
  fit     <- lm(y ~ x, data=data.frame(x=x, y=y))
  newX    <- data.frame(x=jitter(x))
  fitPred <- predict.lm(fit, newdata=newX, interval='prediction', level=0.9)
  
  
  
  #frame <-cbind(na.omit(newX), na.omit(fitPred))
  
  
  p = predict(fit, newdata=data.frame(x=Q25[1,l-1]), interval="prediction", level=0.9)
  
  
  xvalue[1,l-1] <-p[1,1]
  xvalue[2,l-1] <-p[1,2]
  xvalue[3,l-1] <-p[1,3]
  
}

colnames(xvalue)<-colnames(THRES25)
write.table(xvalue, "soybean_t25_0.9_prediction_version1_github.csv", sep=",",row.names=F)
write.table(xvalue, "soybean_t25_0.9_prediction_version2_github.csv", sep=",",row.names=F)


### rice #########

## version 3  ## 

setwd("~/Data collection")
res <- read.table("residual_rice_github.csv", sep=",", header=T)
clim <- read.table("rice_climate_all_version_3_github.csv", sep=",", header=T)



# thresholds: the lowest 25% percentile of detrended rice yields (version 1 includes solar radiation)


# version 1: tmax
clim <- clim[,-23:-42]

# version 2: cum_prec
clim <- clim[,-33:-42]
clim <- clim[,-13:-22]

# version 3: dswr
clim <- clim[,-13:-32]


THRES25 <- matrix(nrow=1,ncol=26)
Q25 <- matrix(nrow=1,ncol=26)

for (i in 2:27){
  lin <- lm(formula = clim[,i] ~ res[,i])
  #abline(lin, lty=2)
  q <- quantile(res[,i], probs=0.25, na.rm=T)
  thres25 <- lin$coefficients[1] +lin$coefficients[2]*q
  THRES25[,i-1] <- thres25
  Q25[, i-1] <- q
}
colnames(THRES25)<- colnames(res[,2:27])
colnames(Q25)<- colnames(res[,2:27])

write.table(THRES25, "rice_threshold25_github_version_c.csv", sep=",",row.names=F)
write.table(Q25, "rice_q25_github.csv", sep=",",row.names=F)



# uncertainty analysis: confidence interval


plot.add.ci <- function(x, y, interval='confidence', level=0.9, regressionColor= "black",...) {
  xOrder  <- order(x)
  x       <- x[xOrder]  
  y       <- y[xOrder]
  fit     <- lm(y ~ x, data=data.frame(x=x, y=y))
  newX    <- data.frame(x=jitter(x))
  fitPred <- predict.lm(fit, newdata=newX, interval=interval, level=level, ...)
  abline(lm(y ~ x), col=regressionColor, lwd=2)
  lines(newX$x, fitPred[,2], lty=2, ...)
  lines(newX$x, fitPred[,3], lty=2, ...)
}



# to calculate confidence intervals

xvalue <- matrix(nrow=3,ncol=26)

for ( l in 2:27){
  #x = na.omit(res[,l])
  #y = na.omit(clim[,l])
  x = res[,l]
  y = clim[,l]
  xOrder  <- order(x)
  x       <- x[xOrder]
  y       <- y[xOrder]
  fit     <- lm(y ~ x, data=data.frame(x=x, y=y))
  newX    <- data.frame(x=jitter(x))
  fitPred <- predict.lm(fit, newdata=newX, interval='confidence', level=0.9)
  
  
  #frame <-cbind(na.omit(newX), na.omit(fitPred))
  
  vec = seq(from=-3, to=1, by= 0.01)
  table <- matrix(nrow=401,ncol=3)
  for ( i in 1:401){
    p = predict(fit, newdata=data.frame(x=vec[i]), interval="confidence", level=0.9)
    table[i,1]<-p[1]
    table[i,2]<-p[2]
    table[i,3]<-p[3]
  }
  
  Table=cbind(vec,table)
  
  n <- which.min(abs(Table[,3] - THRES25[1,l-1])) 
  xvalue[1,l-1] <-Table[n,1]
  n <- which.min(abs(Table[,4] - THRES25[1,l-1])) 
  xvalue[2,l-1] <-Table[n,1]
  n <- which.min(abs(Table[,2] - THRES25[1,l-1])) 
  xvalue[3,l-1] <-Table[n,1]
  
}

colnames(xvalue)<-colnames(THRES25)

write.table(xvalue, "rice_q25_0.9_confidence_version_c_github.csv", sep=",",row.names=F)
write.table(xvalue, "rice_q25_0.9_confidence_version_b_github.csv", sep=",",row.names=F)



# threshold prediction interval

plot.add.ci <- function(x, y, interval='prediction', level=0.9, regressionColor= "black",...) {
  xOrder  <- order(x)
  x       <- x[xOrder]  
  y       <- y[xOrder]
  fit     <- lm(y ~ x, data=data.frame(x=x, y=y))
  newX    <- data.frame(x=jitter(x))
  fitPred <- predict.lm(fit, newdata=newX, interval=interval, level=level, ...)
  abline(lm(y ~ x), col=regressionColor, lwd=2)
  lines(newX$x, fitPred[,2], lty=2, ...)
  lines(newX$x, fitPred[,3], lty=2, ...)
}


l=2
# to calculate prediction intervals

xvalue <- matrix(nrow=3,ncol=26)

for ( l in 2:27){
  #x = na.omit(res[,l])
  #y = na.omit(clim[,l])
  x = res[,l]
  y = clim[,l]
  xOrder  <- order(x)
  x       <- x[xOrder]
  y       <- y[xOrder]
  fit     <- lm(y ~ x, data=data.frame(x=x, y=y))
  newX    <- data.frame(x=jitter(x))
  fitPred <- predict.lm(fit, newdata=newX, interval='prediction', level=0.9)
  
  
  
  #frame <-cbind(na.omit(newX), na.omit(fitPred))
  
  
  p = predict(fit, newdata=data.frame(x=Q25[1,l-1]), interval="prediction", level=0.9)
  
  
  xvalue[1,l-1] <-p[1,1]
  xvalue[2,l-1] <-p[1,2]
  xvalue[3,l-1] <-p[1,3]
  
}

colnames(xvalue)<-colnames(THRES25)
write.table(xvalue, "rice_t25_0.9_prediction_version1_github.csv", sep=",",row.names=F)
write.table(xvalue, "rice_t25_0.9_prediction_version2_github.csv", sep=",",row.names=F)
write.table(xvalue, "rice_t25_0.9_prediction_version3_github.csv", sep=",",row.names=F)





## threshold India BB scale

## version 3  ## 

setwd("~/Analysis/BB climate indicators")
clim <- read.table("India_rice_correl_bb.csv", sep=",", header=T)

plot(clim[,1], clim[,4], xlab="rice yield residuals", ylab="Climate indiactor",pch=16)
abline(v=-0.036, lty=2)
abline(h=198.88, lty=2)

hist(clim[,1], freq=F)
s <- apply(sr,1,mean)
for ( i in 1:10){
ecdfPlot(sr[,i], add=T, ecdf.col="blue")
}
ecdfPlot(s, add=T, ecdf.col="dark green")
for ( i in 1:10){
  ecdfPlot(normal[,i], add=T, ecdf.col="grey")
}
ecdfPlot(clim[2:25,4], add=T, ecdf.col="red")

# thresholds: the lowest 25% percentile of detrended rice yields (version 1 includes solar radiation)



  lin <- lm(formula = clim[,4] ~ clim[,1])
  abline(lin, lty=2)
  q <- quantile(clim[,1], probs=0.25, na.rm=T)
  thres25 <- lin$coefficients[1] +lin$coefficients[2]*q
  

colnames(THRES25)<- colnames(res[,2:27])

write.table(THRES25, "rice_threshold25_github_version_c.csv", sep=",",row.names=F)



