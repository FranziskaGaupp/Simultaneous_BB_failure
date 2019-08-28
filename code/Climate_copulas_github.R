install.packages("foreach", repos="http://cran.rstudio.com/")
install.packages("copula", repos="http://cran.rstudio.com/")
install.packages("CDVine", repos="http://cran.rstudio.com/")
install.packages("VineCopula", repos="http://cran.rstudio.com/")
install.packages("EnvStats", repos="http://cran.rstudio.com/")
install.packages("QRM", repos="http://cran.rstudio.com/")
install.packages("doParallel", repos="http://cran.rstudio.com/")
install.packages("logspline", repos="http://cran.rstudio.com/")
install.packages("fitdistrplus", repos="http://cran.rstudio.com/")



require(copula)
require(CDVine)
require(VineCopula)
library(EnvStats)
#require(PerformanceAnalytics)
require(QRM)
require(foreach)
require(gumbel)
require(logspline)
require(fitdistrplus)
require(PerformanceAnalytics)

# Gumbel distribution fit

dgumbel <- function(x,a,b) {1/b*exp((a-x)/b)*exp(-exp((a-x)/b))}
pgumbel <- function(q,a,b) {exp(-exp((a-q)/b))}
qgumbel <- function(p,a,b) {a-b*log(-log(p))}
rgumbel <- rGumbel

library(doParallel)
cl <- makeCluster(30)
cl <- makeCluster(4)
clusterExport(cl, c("dgumbel", "pgumbel", "qgumbel", "rgumbel"))
registerDoParallel(cl)

stopCluster(cl)

setwd("H:/DPhil/breadbaskets/Data collection")
wt <- read.table("wheat_average_area_weight_v2.csv", sep=",", header=T)
wt_us <- wt[,15:24]
wt_in <- wt[,2:9]
wt_ch <- wt[,10:14]
wt_au <- wt[,29:30]
wt_ar <- wt[,25:28]
wt_eu <- wt[,31:34]
wt_ruk <- wt[,35:38]

setwd("H:/DPhil/breadbaskets/Data collection")
res <- read.table("resid_wheat_1967_2012_github_v2.csv", sep=",", header=T)


################################## WHEAT #########
### USA --------
setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
cum_prec <- read.csv("USA_wheat_BB_cum_precip_Oct_May.csv", header=T, sep=",")
t34_us <- read.csv("USA_wheat_BB_cum_t34_Mar_May.csv", header=T, sep=",")


# plot(res[,14], cum_prec[,2], xlab="Kansas wheat yield residuals", ylab="Cumulative growing season precipitation (mm)",
#      pch=16, main="Climate impact on yield in Kansas")
# abline(v=0, lty=2)
# lin <- lm(formula = cum_prec[,2] ~ res[,14])
# abline(lin, lty=2)


## usa precipitation -----
u_usa_prec <- pobs(cum_prec[1:24,2:11]) # c)
u_usa_prec <- pobs(cum_prec[25:46,2:11]) # b)
u_usa_prec <- pobs(cum_prec[,2:11]) # a)

RVM = RVineStructureSelect(u_usa_prec,c(1:6),progress=TRUE)



# start loop


cum_prec <- cum_prec[25:46,]
cum_prec <- cum_prec[1:24,]

#CLIM <- matrix(ncol=1000,nrow=10000)

 clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {

  
#for (n in 1:1000){

rep=10000
simdata_usa_prec <- RVineSim(rep, RVM)

## ### ### ##   ## ### ## ## ## ## 
# a) marginal distributions 1967-2012


# #Gumbel <- list("numeric", 10)
# #Normal <- list("numeric", 10)
# for ( i in 2:11){
#   plot(fitdist(cum_prec[,i],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
#   plot(fitdist(cum_prec[,i],"norm"))
# }
# 
# for ( i in 2:11){
# a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
# b <- fitdist(cum_prec[,i],"norm")
# print(summary(a)$aic)
# print(summary(b)$aic)
# print(summary(a)$bic)
# print(summary(b)$bic)}
#
# 
# #norm better, gumbel: 8

        # normal <- matrix(nrow=rep, ncol=10)
        # x = c(1:6, 8:10)
        # for ( i in x){
        #   Normal<-fitdist(cum_prec[,i+1],"norm")
        #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        # }
        # colnames(normal) <- colnames(cum_prec[,2:11])
        # 
        # 
        # Gumbel<-fitdist(cum_prec[,8],"gumbel", start=list(a=0,b=1),optim.method="Nelder-Mead")
        # normal[,7] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])

## ## ## ###
# b) marginal distributions, devided into 1991-2012 


     #   cum_prec <- cum_prec[25:46,]
# 
# 
#   plot(fitdist(cum_prec[,11],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
#   plot(fitdist(cum_prec[,11],"norm"))
# 
# 
# for ( i in 2:11){
# a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
# b <- fitdist(cum_prec[,i],"norm")
# print(summary(a)$aic)
# print(summary(b)$aic)
# print(summary(a)$bic)
# print(summary(b)$bic)}

# 
# 
# #norm: 4,5,6,7,8,9,10,11 gumbel: 2,3

        normal <- matrix(nrow=rep, ncol=10)
        x = c(3:10)
        for ( i in x){
          Normal<-fitdist(cum_prec[,i+1],"norm")
          normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        }
        colnames(normal) <- colnames(cum_prec[,2:11])


        for ( y in 1:2){
        Gumbel<-fitdist(cum_prec[,y+1],"gumbel", start=list(a=0,b=1),optim.method="Nelder-Mead")
        normal[,y] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
        }

# c) marginal distributions, divided into 1967-1990

        
        
#         
#  plot(fitdist(cum_prec[,5],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[,5],"norm"))
# 
#   for ( i in 2:11){
#  a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#  b <- fitdist(cum_prec[,i],"norm")
#  print(summary(a)$aic)
#  print(summary(b)$aic)
#  print(summary(a)$bic)
#  print(summary(b)$bic)}
# # 
# #norm better, gumbel: 7,8 (-3,3)

        # normal <- matrix(nrow=rep, ncol=10)
        # x = c(1:5, 8:10)
        # for ( i in x){
        #   Normal<-fitdist(cum_prec[,i+1],"norm")
        #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        # }
        # colnames(normal) <- colnames(cum_prec[,2:11])
        # 
        # 
        # Gumbel<-fitdist(cum_prec[,8],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        # normal[,7] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
        # Gumbel<-fitdist(cum_prec[,7],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        # normal[,6] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])

        

# # CDF
# SU <- vector("numeric", rep)
# for (i in 1:rep){
#   su = (quantile(normal[,1], probs=simdata_usa_prec[i,1])* wt_us[,1] + quantile(normal[,2], probs=simdata_usa_prec[i,2])* wt_us[,2] + quantile(normal[,3], probs=simdata_usa_prec[i,3])* wt_us[,3]
#         + quantile(normal[,4], probs=simdata_usa_prec[i,4])* wt_us[,4]+ quantile(normal[,5], probs=simdata_usa_prec[i,5])* wt_us[,5]+  quantile(normal[,6], probs=simdata_usa_prec[i,6])* wt_us[,6]
#         + quantile(normal[,7], probs=simdata_usa_prec[i,7])* wt_us[,7]+  quantile(normal[,8], probs=simdata_usa_prec[i,8])* wt_us[,8]+  quantile(normal[,9], probs=simdata_usa_prec[i,9])* wt_us[,9]
#         + quantile(normal[,10], probs=simdata_usa_prec[i,10])* wt_us[,10])
# 
#   SU[i] <- su
# }


SUi <- vector("numeric", rep)
for (i in 1:rep){
  sui = (quantile(normal[,1], probs=runif(1,0,1))* wt_us[,1]+ quantile(normal[,2], probs=runif(1,0,1))* wt_us[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_us[,3] 
         + quantile(normal[,4], probs=runif(1,0,1))* wt_us[,4]+ quantile(normal[,5], probs=runif(1,0,1))* wt_us[,5]+  quantile(normal[,6], probs=runif(1,0,1))* wt_us[,6]
         + quantile(normal[,7], probs=runif(1,0,1))* wt_us[,7]+  quantile(normal[,8], probs=runif(1,0,1))* wt_us[,8]+  quantile(normal[,9], probs=runif(1,0,1))* wt_us[,9]
         + quantile(normal[,10], probs=runif(1,0,1))* wt_us[,10])
  
  SUi[i] <- sui 
}

return(SUi)
}

#stopCluster(cl)
#registerDoSEQ() # if foreach doesn't stop after stopCluster()


clim <- as.data.frame(clim)
write.table(clim, "USA_wheat_cum_prec_simulations_github_period1_indep.csv", sep=",", row.names=F)


      
        
# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_wheat <- read.table("wheat_threshold25_version3_github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("USA_wheat_cum_prec_simulations_github_period2.csv", header=T, sep=",")

# # ecdf for all simulations
# ecdfPlot(clim[,1], main=" Cumulative growing season precipitation USA", xlab="Precipitation in mm", ylab="F(x)")
# 
# for ( u in 1:1000){
#   ecdfPlot(clim[,u], add=T, ecdf.col="grey55", ecdf.lwd=0.5)
#   
# }

# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white",main=" Cumulative precipitation in the USA Oct - May ", xlab="Precipitation (in mm)", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
    s <- sort(clim[,i], decreasing=F)
    ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
error <- sd(ECDF[i,]/sqrt(1000))
ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# 0.023153 period 1
# period 2: 0.030928


# require(EnvStats)
# 
# ecdfPlot(SU, main=" Cumulative growing season pecipitation USA", xlab="Precipitation in mm", ylab="F(x)")
# ecdfPlot(SU, add=T, ecdf.col="grey55")
# 
legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_wheat [,4], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.023" , "SE2 = 0.031", c(paste("threshold = ", round(thres_wheat[1,4], digits =2), sep=""))))

# 

# 
# quantile(SU, probs=0.2)
# quantile(SU, probs=0.1)
# quantile(SU, probs=0.05)
# quantile(SU, probs=0.01)

# CDF independent

SUi <- vector("numeric", rep)
for (i in 1:rep){
  sui = (quantile(normal[,1], probs=runif(1,0,1))* wt_us[,1]+ quantile(normal[,2], probs=runif(1,0,1))* wt_us[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_us[,3] 
        + quantile(normal[,4], probs=runif(1,0,1))* wt_us[,4]+ quantile(normal[,5], probs=runif(1,0,1))* wt_us[,5]+  quantile(normal[,6], probs=runif(1,0,1))* wt_us[,6]
        + quantile(normal[,7], probs=runif(1,0,1))* wt_us[,7]+  quantile(normal[,8], probs=runif(1,0,1))* wt_us[,8]+  quantile(normal[,9], probs=runif(1,0,1))* wt_us[,9]
        + quantile(normal[,10], probs=runif(1,0,1))* wt_us[,10])
  
  SUi[i] <- sui 
}







## usa t34 (version 2) -----


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
t34_us <- read.csv("USA_wheat_BB_cum_t34_Mar_May.csv", header=T, sep=",")



u_usa_t34 <- pobs(t34_us[,2:11]) # a)
u_usa_t34 <- pobs(t34_us[1:24,2:11]) # b)
u_usa_t34 <- pobs(t34_us[25:46,2:11]) #c)

#Minnesota is always 0 in the second period

# 1967-1990
      RVM = RVineStructureSelect(u_usa_t34,c(1:6),progress=TRUE)

# 1991-2012
      h=c(1:7,9:10)
      RVM = RVineStructureSelect(u_usa_t34[,h],c(1:6),progress=TRUE)
#
      
      
      t34_us <- t34_us[1:24,2:11] #b
      t34_us <- t34_us[25:46,2:11] #c
      t34_us <- t34_us[1:46,2:11] #a
     
      
# loop
      
clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus")) %dopar% {
        
      
rep=10000
#1967-1990
     simdata_usa_t34 <- RVineSim(rep, RVM)

# 1991-2012
     #simdata_usa_t34 <-cbind(simdata_usa_t34[,1:7], rep(0,1000),simdata_usa_t34[,8:9] )

# marginal distributions

      # for ( i in 1:10){
      #    a <- fitdist(t34_us[,i],"gamma",start=list(a=0,b=1),method="mme")
      #    b <- fitdist(t34_us[,i],"exp")
      #    print(summary(a)$aic)
      #    print(summary(b)$aic)
      #    print(summary(a)$bic)
      #    print(summary(b)$bic)}
      #   

  # plot(fitdist(t34_us[,11],"gamma",start=list(a=0,b=1),method="mme"))
  # plot(fitdist(t34_us[,11],"exp"))
  # 
#gamma fits best

#a) 

        # gamma <- matrix(nrow=rep, ncol=10)
        # for ( i in 1:10){
        #   Gamma<-fitdist(t34_us[,i+1],"gamma", method="mme")
        #   gamma[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
        # }
        # colnames(gamma) <- colnames(t34_us[,2:11])



## 1967-1990

      #t34_us <- t34_us[1:24,2:11]

# i=10
# plot(fitdist(t34_us[,i],"gamma",start=list(a=0,b=1),method="mme"))
# plot(fitdist(t34_us[,i],"exp"))
# 
# plot(fitdist(t34_us[,i],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(t34_us[,i],"norm"))

      
      # for ( i in 1:10){
      #      a <- fitdist(t34_us[,i],"gamma",start=list(a=0,b=1),method="mme")
      #      b <- fitdist(t34_us[,i],"exp")
      #      print(summary(a)$aic)
      #      print(summary(b)$aic)
      #      print(summary(a)$bic)
      #      print(summary(b)$bic)}

# gamma: all

#b) 

      gamma <- matrix(nrow=rep, ncol=10)
      for ( i in 1:10){
        Gamma<-fitdist(t34_us[,i],"gamma", method="mme")
        gamma[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      }
      colnames(gamma) <- colnames(t34_us)

## 1991_2012

     # t34_us <- t34_us[25:46,2:11]

# 
# plot(fitdist(t34_us[,i],"gamma",start=list(a=0,b=1),method="mme"))
# plot(fitdist(t34_us[,i],"exp"))
# 
# plot(fitdist(t34_us[,i],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(t34_us[,i],"norm"))

      # for ( i in 2:10){
      #        a <- fitdist(t34_us[,i],"gamma",start=list(a=0,b=1),method="mme")
      #        b <- fitdist(t34_us[,i],"exp")
      #        print(summary(a)$aic)
      #        print(summary(b)$aic)
      #        print(summary(a)$bic)
      #        print(summary(b)$bic)}

      # gamma: all except 8 (all zero)

#c)
# 
#       gamma <- matrix(nrow=rep, ncol=10)
#       x=c(1:7,9:10)
#       for ( i in x){
#         Gamma<-fitdist(t34_us[,i],"gamma", method="mme")
#         gamma[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
#       }
#       gamma[,8]<- rep(0,rep)
#       colnames(gamma) <- colnames(t34_us)


# CDF
# SU2 <- vector("numeric", rep)
# for (i in 1:rep){
#   su = (quantile(gamma[,1], probs=simdata_usa_t34[i,1])* wt_us[,1]+ quantile(gamma[,2], probs=simdata_usa_t34[i,2])* wt_us[,2] + quantile(gamma[,3], probs=simdata_usa_t34[i,3]) * wt_us[,3]
#         + quantile(gamma[,4], probs=simdata_usa_t34[i,4])* wt_us[,4]+ quantile(gamma[,5], probs=simdata_usa_t34[i,5])* wt_us[,5]+  quantile(gamma[,6], probs=simdata_usa_t34[i,6])* wt_us[,6]
#         + quantile(gamma[,7], probs=simdata_usa_t34[i,7])* wt_us[,7]+  quantile(gamma[,8], probs=simdata_usa_t34[i,8])* wt_us[,8]+  quantile(gamma[,9], probs=simdata_usa_t34[i,9])* wt_us[,9]
#         + quantile(gamma[,10], probs=simdata_usa_t34[i,10])* wt_us[,10])
# 
#   SU2[i] <- su
# }

# CDF independent
SU2i <- vector("numeric", rep)
for (i in 1:rep){
  sui = (quantile(gamma[,1], probs=runif(1,0,1))* wt_us[,1] + quantile(gamma[,2], probs=runif(1,0,1))* wt_us[,2] + quantile(gamma[,3], probs=runif(1,0,1))* wt_us[,3] 
         + quantile(gamma[,4], probs=runif(1,0,1))* wt_us[,4] + quantile(gamma[,5], probs=runif(1,0,1))* wt_us[,5]+  quantile(gamma[,6], probs=runif(1,0,1))* wt_us[,6]
         + quantile(gamma[,7], probs=runif(1,0,1))* wt_us[,7] +  quantile(gamma[,8], probs=runif(1,0,1))* wt_us[,8]+  quantile(gamma[,9], probs=runif(1,0,1))* wt_us[,9]
         + quantile(gamma[,10], probs=runif(1,0,1))* wt_us[,10])
  
  SU2i[i] <- sui 
}

return(SU2i)

}

clim <- as.data.frame(clim)
write.table(clim, "USA_wheat_t34_simulations_github_period1_indep.csv", sep=",", row.names=F)


  
# ecdf
ecdfPlot(clim[,1], main=" Days above 34°C during growing season USA", xlab="Days above 34°C", ylab="F(x)", ecdf.lwd=0.5)


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_wheat <- read.table("wheat_threshold25_version3_github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("USA_wheat_t34_simulations_github_period1.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white",main=" Days above 34°C during growing season USA", xlab="Days above 34°C", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.0083
# period 2: 0.00032


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_wheat [,5], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.008" , "SE2 = 0.0", c(paste("threshold = ", round(thres_wheat[1,5], digits =2), sep=""))))

# ecdfPlot(SU, main=" Days above 34°C during growing season USA", xlab="Days above 34°C", ylab="F(x)")
# ecdfPlot(SU, add=T, ecdf.col="grey55")


# CDF independent
SU2i <- vector("numeric", rep)
for (i in 1:rep){
  sui = (quantile(gamma[,1], probs=runif(1,0,1))+ quantile(gamma[,2], probs=runif(1,0,1)) + quantile(gamma[,3], probs=runif(1,0,1)) 
        + quantile(gamma[,4], probs=runif(1,0,1))+ quantile(gamma[,5], probs=runif(1,0,1))+  quantile(gamma[,6], probs=runif(1,0,1))
        + quantile(gamma[,7], probs=runif(1,0,1))+  quantile(gamma[,8], probs=runif(1,0,1))+  quantile(gamma[,9], probs=runif(1,0,1))
        + quantile(gamma[,10], probs=runif(1,0,1)))/10
  
  SU2i[i] <- sui 
}




####### India ----------

#### spi ---------

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
spi <- read.csv("India_wheatBB_SPI1_Jun_Sep_average.csv", header=T, sep=",")
# 1966 spi has influence on yield in 1967
spi<- spi[-47,]
setwd("H:/DPhil/breadbaskets/Data collection")
res <- read.table("resid_wheat_1967_2012.csv", sep=",", header=T)


u_in_spi <- pobs(spi[1:24,2:9])  #b
u_in_spi <- pobs(spi[25:46,2:9]) #c
u_in_spi <- pobs(spi[,2:9]) #a

RVM = RVineStructureSelect(u_in_spi,c(1:6),progress=TRUE)

spi<- spi[25:46,] #c
spi<- spi[1:24,]

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
simdata_in_spi <- RVineSim(rep, RVM)


# marginal distributions



#   plot(fitdist(-spi[,7],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
#   plot(fitdist(spi[,7],"norm"))

# for ( i in 2:9){
#  a <- fitdist(-spi[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#  b <- fitdist(spi[,i],"norm")
#  print(summary(a)$aic)
#  print(summary(b)$aic)
#  print(summary(a)$bic)
#  print(summary(b)$bic)}

# #norm: 3,4,5,9, neg gumbel: 2,6,7,8

#a) 
          # normal <- matrix(nrow=rep, ncol=8)
          # x = c(2:4, 8)
          # for ( i in x){
          #   Normal <-fitdist(spi[,i+1],"norm")
          #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
          # }
          # colnames(normal) <- colnames(spi[,2:9])
          # 
          # 
          # Gumbel<-fitdist(-spi[,2],"gumbel", start=list(a=0,b=1),optim.method="Nelder-Mead")
          # normal[,1] <- -rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
          # Gumbel<-fitdist(-spi[,6],"gumbel", start=list(a=0,b=1),optim.method="Nelder-Mead")
          # normal[,5] <- -rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
          # Gumbel<-fitdist(-spi[,7],"gumbel", start=list(a=0,b=1),optim.method="Nelder-Mead")
          # normal[,6] <- -rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
          # Gumbel<-fitdist(-spi[,8],"gumbel", start=list(a=0,b=1),optim.method="Nelder-Mead")
          # normal[,7] <- -rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])


### 1967-1990

#

 # plot(fitdist(spi[,9],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
 # plot(fitdist(spi[,9],"norm"))
# 

# 
# for ( i in 2:9){
#   a <- fitdist(-spi[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(spi[,i],"norm")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(a)$bic)
#   print(summary(b)$bic)}
# # 


#norm 4,5,7  neg gumbel: 2,3,6,8,9

#b) 
# 
      normal <- matrix(nrow=rep, ncol=8)
      x = c(4:5,7)
      for ( i in x){
        Normal <-fitdist(spi[,i],"norm")
        normal[,i-1] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      }
      x = c(2:3,6,8:9)
      for ( i in x){
      Gumbel<-fitdist(-spi[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
       normal[,i-1] <- -rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      }

      colnames(normal) <- colnames(spi[,2:9])

# 1991-2012



# plot(fitdist(spi[,3],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(spi[,3],"norm"))
# 

# for ( i in 2:9){
#     a <- fitdist(-spi[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#     b <- fitdist(spi[,i],"norm")
#     print(summary(a)$aic)
#     print(summary(b)$aic)
#     print(summary(a)$bic)
#     print(summary(b)$bic)}
  #


#norm:2,3 6,9 gumbel:5 , neg gumbel : 4, 7,8

# c)  
# # # 
        # normal <- matrix(nrow=rep, ncol=8)
        # x = c(1:2,5,8)
        # for ( i in x){
        #   Normal <-fitdist(spi[,i+1],"norm")
        #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        # }
        # colnames(normal) <- colnames(spi[,2:9])
        # 
        # 
        # Gumbel<-fitdist(-spi[,4],"gumbel", start=list(a=0,b=1),optim.method="Nelder-Mead")
        # normal[,3] <- -rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
        # Gumbel<-fitdist(spi[,5],"gumbel", start=list(a=0,b=1),optim.method="Nelder-Mead")
        # normal[,4] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
        # Gumbel<-fitdist(-spi[,8],"gumbel", start=list(a=0,b=1),optim.method="Nelder-Mead")
        # normal[,7] <- -rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
        # Gumbel<-fitdist(-spi[,7],"gumbel", start=list(a=0,b=1),optim.method="Nelder-Mead")
        # normal[,6] <- -rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])


# CDF
# SI <- vector("numeric", rep)
# for (i in 1:rep){
#   si = (quantile(normal[,1], probs=simdata_in_spi[i,1])* wt_in[,1] + quantile(normal[,2], probs=simdata_in_spi[i,2])* wt_in[,2] + quantile(normal[,3], probs=simdata_in_spi[i,3])* wt_in[,3]
#   + quantile(normal[,4], probs=simdata_in_spi[i,4])* wt_in[,4]+ quantile(normal[,5], probs=simdata_in_spi[i,5])* wt_in[,5] +  quantile(normal[,6], probs=simdata_in_spi[i,6])* wt_in[,6]
#   + quantile(normal[,7], probs=simdata_in_spi[i,7])* wt_in[,7]+  quantile(normal[,8], probs=simdata_in_spi[i,8])* wt_in[,8]
#   )
# 
#   SI[i] <- si
# }

# CDF indep
SIi <- vector("numeric", rep)
for (i in 1:rep){
  sii = (quantile(normal[,1], probs=runif(1,0,1))* wt_in[,1] + quantile(normal[,2], probs=runif(1,0,1))* wt_in[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_in[,3] 
         + quantile(normal[,4], probs=runif(1,0,1))* wt_in[,4] + quantile(normal[,5], probs=runif(1,0,1))* wt_in[,5] +  quantile(normal[,6], probs=runif(1,0,1))* wt_in[,6]
         + quantile(normal[,7], probs=runif(1,0,1))* wt_in[,7] +  quantile(normal[,8], probs=runif(1,0,1))* wt_in[,8]
  )
  
  SIi[i] <- sii 
}

return(SIi)
}


clim <- as.data.frame(clim)
write.table(clim, "India_wheat_spi_simulations_github_period1_indep.csv", sep=",", row.names=F)




# # ecdf
# ecdfPlot(clim[,1], main=" SPI India", xlab="SPI", ylab="F(x)", ecdf.lwd=0.5, xlim=c(-1.8,0), ylim=c(0,0.5))
# 
# for ( u in 1:10){
#   ecdfPlot(clim[,u], add=T, ecdf.col="grey55", ecdf.lwd=0.5)
#   
# }

# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_wheat <- read.table("wheat_threshold25_version3_github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("India_wheat_spi_simulations_github_period1.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white",main=" SPI in India Jun-Sep ", xlab="SPI", ylab="F(x)", xlim=c(-1.5, 1.5))

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.000264
# period 2: 0.000269


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_wheat [,2], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.00026" , "SE2 = 0.00027", c(paste("threshold = ", round(thres_wheat[1,2], digits =2), sep=""))))




# ecdfPlot(SI, main=" SPI India", xlab="SPI", ylab="F(x)")
# 
# ecdfPlot(SI, main=" SPI India", xlab="SPI", ylab="F(x)", xlim=c(-1.8,0), ylim=c(0,0.5))
# ecdfPlot(SI, add=T, ecdf.col="grey55")
# 
# legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))




# CDF indep
SIi <- vector("numeric", rep)
for (i in 1:rep){
  sii = (quantile(normal[,1], probs=runif(1,0,1))* wt_in[,1] + quantile(normal[,2], probs=runif(1,0,1))* wt_in[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_in[,3] 
        + quantile(normal[,4], probs=runif(1,0,1))* wt_in[,4] + quantile(normal[,5], probs=runif(1,0,1))* wt_in[,5] +  quantile(normal[,6], probs=runif(1,0,1))* wt_in[,6]
        + quantile(normal[,7], probs=runif(1,0,1))* wt_in[,7] +  quantile(normal[,8], probs=runif(1,0,1))* wt_in[,8]
  )
  
  SIi[i] <- sii 
}





######### China -------


setwd ("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
cum_prec <- read.csv("China_wheat_BB_cum_precip_Jun_Sep.csv", header=T, sep=",")

## china precipitation -----
# 2012 (which would influence yields in 2013) are taken out
cum_prec <- cum_prec[-47,]

u_ch_prec <- pobs(cum_prec[1:24,])  #b
u_ch_prec <- pobs(cum_prec[25:46,])  #c
u_ch_prec <- pobs(cum_prec)  #a

RVM = RVineStructureSelect(u_ch_prec,c(1:6),progress=TRUE)

cum_prec <- cum_prec[1:24,]
cum_prec <- cum_prec[25:46,]


clim <- foreach (z=1:10,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
simdata_ch_prec <- RVineSim(rep, RVM)


# marginal distributions

  # plot(fitdist(cum_prec[,5],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
  # plot(fitdist(cum_prec[,5],"norm"))

# 
# for ( i in 1:5){
#       a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#       b <- fitdist(cum_prec[,i],"norm")
#       print(summary(a)$aic)
#       print(summary(b)$aic)
#       print(summary(a)$bic)
#       print(summary(b)$bic)}

#norm for 1,2,3,4,5  gumbel: 

#a) 
      # normal <- matrix(nrow=rep, ncol=5)
      # x = c(1:5)
      # for ( i in x){
      #   Normal<-fitdist(cum_prec[,i],"norm")
      #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # colnames(normal) <- colnames(cum_prec[,1:5])

#### 1967-1990
#cum_prec <- cum_prec[1:24,]

 # plot(fitdist(cum_prec[,1],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
 # plot(fitdist(cum_prec[,1],"norm"))
 #  
 #  for ( i in 1:5){
 #        a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
 #        b <- fitdist(cum_prec[,i],"norm")
 #        print(summary(a)$aic)
 #        print(summary(b)$aic)
 #        print(summary(a)$bic)
 #        print(summary(b)$bic)}

#gumbel: 1 , normal: 2,3,4,5

#b) 

      normal <- matrix(nrow=rep, ncol=5)
      x = c(1)
      for ( i in x){
        Gumbel<-fitdist(cum_prec[,1],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      }
      Normal<-fitdist(cum_prec[,2],"norm")
      normal[,2] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      Normal<-fitdist(cum_prec[,3],"norm")
      normal[,3] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      Normal<-fitdist(cum_prec[,4],"norm")
      normal[,4] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      Normal<-fitdist(cum_prec[,5],"norm")
      normal[,5] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])

      colnames(normal) <- colnames(cum_prec[,1:5])

# 1991-2012

#cum_prec <- cum_prec[25:46,]

# 
# plot(fitdist(cum_prec[,2],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[,2],"norm"))

      
      # for ( i in 1:5){
      #       a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
      #       b <- fitdist(cum_prec[,i],"norm")
      #       print(summary(a)$aic)
      #       print(summary(b)$aic)
      #       print(summary(a)$bic)
      #       print(summary(b)$bic)}
      # 
#gumbel:4  , normal: 1,2,3,5

#c)
      
      # normal <- matrix(nrow=rep, ncol=5)
      # x = c(1:3,5)
      # for ( i in x){
      # Normal<-fitdist(cum_prec[,i],"norm")
      # normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # 
      # }
      # 
      # Gumbel<-fitdist(cum_prec[,4],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
      # normal[,4] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      # 
      # colnames(normal) <- colnames(cum_prec[,1:5])

      
# 
# CDF
PC <- vector("numeric", rep)
for (i in 1:rep){
  pc = (quantile(normal[,1], probs=simdata_ch_prec[i,1])* wt_ch[,1] + quantile(normal[,2], probs=simdata_ch_prec[i,2])* wt_ch[,2] + quantile(normal[,3], probs=simdata_ch_prec[i,3])* wt_ch[,3]
        + quantile(normal[,4], probs=simdata_ch_prec[i,4])* wt_ch[,4] + quantile(normal[,5], probs=simdata_ch_prec[i,5])* wt_ch[,5]
        )

  PC[i] <- pc
}

# CDF independent

# PCi <- vector("numeric", rep)
# for (i in 1:rep){
#   pci = (quantile(normal[,1], probs=runif(1,0,1))* wt_ch[,1] + quantile(normal[,2], probs=runif(1,0,1))* wt_ch[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_ch[,3]
#          + quantile(normal[,4], probs=runif(1,0,1))* wt_ch[,4] + quantile(normal[,5], probs=runif(1,0,1))* wt_ch[,5]
#   )
#   
#   PCi[i] <- pci 
# }

return(PC)
}

clim <- as.data.frame(clim)
write.table(clim, "China_wheat_cum_prec_simulations_github_period1.csv", sep=",", row.names=F)

      
      
# # ecdf
# 
# ecdfPlot(clim[,1], main=" Cumulative growing season precipitation China", xlab="Precipitation in mm", ylab="F(x)")
# 
# for ( u in 1:10){
#   ecdfPlot(clim[,u], add=T, ecdf.col="grey55", ecdf.lwd=0.5)
#   
# }

      

# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_wheat <- read.table("wheat_threshold25_version3_github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("China_wheat_cum_prec_simulations_github_period2.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white",main=" Cumulative precipitation in China Jun-Sep ", xlab="Precipitation (in mm)", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.03945
# period 2: 0.04449


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_wheat [,3], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.04" , "SE2 = 0.044", c(paste("threshold = ", round(thres_wheat[1,3], digits =2), sep=""))))


# 
# ecdfPlot(PC, main=" Cumulative growing season precipitation China", xlab="Precipitation in mm", ylab="F(x)")
# 
# ecdfPlot(PC, add=T, ecdf.col="grey55")
# ecdfPlot(PC, add=T)
# 
# legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
# 


# CDF independent

PCi <- vector("numeric", rep)
for (i in 1:rep){
  pci = (quantile(normal[,1], probs=runif(1,0,1))* wt_ch[,1] + quantile(normal[,2], probs=runif(1,0,1))* wt_ch[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_ch[,3]
        + quantile(normal[,4], probs=runif(1,0,1))* wt_ch[,4] + quantile(normal[,5], probs=runif(1,0,1))* wt_ch[,5]
  )
  
  PCi[i] <- pci 
}


ecdfPlot(PC, main=" Cumulative growing season precipitation China", xlab="Precipitation in mm", ylab="F(x)")
ecdfPlot(PCi, add=T, ecdf.col="grey55")




####### Australia ------

### australia precipitation ----

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
cum_prec <- read.csv("Australia_wheat_BB_cum_precip_May_Nov.csv", header=T, sep=",")


u_au_prec <- pobs(cum_prec[2:47,2:3])  #a
u_au_prec <- pobs(cum_prec[2:25,2:3])  #b
u_au_prec <- pobs(cum_prec[26:47,2:3])   #c

#BiCopSelect(u_au_prec[,1], u_au_prec[,2],3)
# -> Clayton copula (3)


clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
#simdata_au_prec <- BiCopSim(rep, 3, 1.537367)   #c
simdata_au_prec <- BiCopSim(rep, 3, 1.452076)   # b
#simdata_au_prec <- BiCopSim(rep, 3, 1.61) #a

#a

  # plot(fitdist(cum_prec[2:47,3],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
  # plot(fitdist(cum_prec[2:47,3],"norm"))

# for ( i in 2:3){
#         a <- fitdist(cum_prec[2:47,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#         b <- fitdist(cum_prec[2:47,i],"norm")
#         print(summary(a)$aic)
#         print(summary(b)$aic)
#         print(summary(a)$bic)
#         print(summary(b)$bic)}


#norm better
# a


    # normal <- matrix(nrow=rep, ncol=2)
    # x = c(1:2)
    # for ( i in x){
    #   Normal<-fitdist(cum_prec[,i+1],"norm")
    #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
    # }
    # colnames(normal) <- colnames(cum_prec[,2:3])
    # 

# 1967-1990

# plot(fitdist(cum_prec[2:25,3],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[2:25,3],"norm"))

# for ( i in 2:3){
#         a <- fitdist(cum_prec[2:25,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#         b <- fitdist(cum_prec[2:25,i],"norm")
#         print(summary(a)$aic)
#         print(summary(b)$aic)
#         print(summary(a)$bic)
#         print(summary(b)$bic)}

#norm 2, gumbel3

    #b
    
    normal <- matrix(nrow=rep, ncol=2)

      Normal<-fitdist(cum_prec[2:25,2],"norm")
      normal[,1] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      Gumbel<-fitdist(cum_prec[2:25,3],"gumbel", start=list(a=0,b=1),optim.method="Nelder-Mead")
      normal[,2] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])


    colnames(normal) <- colnames(cum_prec[,2:3])

# 1991-2012


# plot(fitdist(cum_prec[26:47,3],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[26:47,3],"norm"))
    
    # for ( i in 2:3){
    #           a <- fitdist(cum_prec[26:47,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
    #           b <- fitdist(cum_prec[26:47,i],"norm")
    #           print(summary(a)$aic)
    #           print(summary(b)$aic)
    #           print(summary(a)$bic)
    #           print(summary(b)$bic)}


#norm better

    #c
    

      # normal <- matrix(nrow=rep, ncol=2)
      # 
      # Normal<-fitdist(cum_prec[26:47,2],"norm")
      # normal[,1] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # Normal<-fitdist(cum_prec[26:47,3],"norm")
      # normal[,2] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # 
      # 
      # colnames(normal) <- colnames(cum_prec[,2:3])


# CDF
# AUP <- vector("numeric", rep)
# for (i in 1:rep){
#   aup = (quantile(normal[,1], probs=simdata_au_prec[i,1])* wt_au[,1]+ quantile(normal[,2], probs=simdata_au_prec[i,2])* wt_au[,2])
#   AUP[i] <- aup
# }


# CDF independent

AUPi <- vector("numeric", rep)
for (i in 1:rep){
  aupi = (quantile(normal[,1], probs=runif(1,0,1))* wt_au[,1] + quantile(normal[,2], probs=runif(1,0,1))* wt_au[,2])
  AUPi[i] <- aupi 
}

return(AUPi)
}

clim <- as.data.frame(clim)
write.table(clim, "Australia_wheat_cum_prec_simulations_github_period1_indep.csv", sep=",", row.names=F)

      
# # ecdf
# 
# ecdfPlot(clim[,1], main=" Cumulative growing season precipitation Australia", xlab="Precipitation in mm", ylab="F(x)", xlim=c(0,650))
# 
# for ( u in 1:10){
#   ecdfPlot(clim[,u], add=T, ecdf.col="grey55", ecdf.lwd=0.5)
#   
# }


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_wheat <- read.table("wheat_threshold25_version3_github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("Australia_wheat_cum_prec_simulations_github_period2.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main=" Cumulative precipitation in Australia May-Nov", xlab="Precipitation (in mm)", ylab="F(x)", xlim=c(50,450))

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.03731
# period 2: 0.041779


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_wheat [,6], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.037" , "SE2 = 0.042", c(paste("threshold = ", round(thres_wheat[1,6], digits =2), sep=""))))


# ecdfPlot(AUP, main=" Cumulative growing season precipitation Australia", xlab="Precipitation in mm", ylab="F(x)", xlim=c(0,650))
# ecdfPlot(AUP, main=" Cumulative growing season precipitation Australia", xlab="Precipitation in mm", ylab="F(x)")
# ecdfPlot(AUP, add=T, ecdf.col="grey55")
# legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
# 


# CDF independent

AUPi <- vector("numeric", rep)
for (i in 1:rep){
  aupi = (quantile(normal[,1], probs=runif(1,0,1))* wt_au[,1] + quantile(normal[,2], probs=runif(1,0,1))* wt_au[,2])
  AUPi[i] <- aupi 
}



### tmax australia (version 2) ----

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
tmax <- read.csv("Australia_wheat_BB_tmax_May_Nov.csv", header=T, sep=",")
tmax <- tmax[-47,]

u_au_tmax <- pobs(tmax[,2:3])  #a
u_au_tmax <- pobs(tmax[1:24,2:3])  #b
u_au_tmax <- pobs(tmax[25:46,2:3])  #c


#BiCopSelect(u_au_tmax[,1], u_au_tmax[,2], familyset=c(1:6))
# -> Gaussian copula (1)
# 1967-1990: Gumbel (4)
# 1991-2012 Gumbel (4)

# 1967 -1990
tmax <- tmax[1:24,2:3]#b
tmax <- tmax[25:46,2:3]#c

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
#simdata_au_tmax <- BiCopSim(rep, 1, 0.8959008)
#1967-1990
simdata_au_tmax <- BiCopSim(rep, 4, 3.39)
#1991-2012
#simdata_au_tmax <- BiCopSim(rep, 4, 2.38)


# marginal distributions

#Gumbel <- list("numeric", 10)
#Normal <- list("numeric", 10)

# plot(fitdist(tmax[,3],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(tmax[,3],"norm"))
# plot(fitdist(tmax[,2],"unif",method="mme"))

# for ( i in 2:3){
#             a <- fitdist(tmax[2:47,i],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead")
#             b <- fitdist(tmax[2:47,i],"norm")
#             print(summary(a)$aic)
#             print(summary(b)$aic)
#             print(summary(a)$bic)
#             print(summary(b)$bic)}

#norm fits best

#a) 

    # normal <- matrix(nrow=rep, ncol=2)
    # x = c(1:2)
    # for ( i in x){
    #   Normal<-fitdist(tmax[,i+1],"norm")
    #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
    # }
    # colnames(normal) <- colnames(tmax[,2:3])


# 1967 -1990
#tmax <- tmax[1:24,2:3]

# plot(fitdist(tmax[,2],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(tmax[,2],"norm"))
  
  # for ( i in 2:3){
  #             a <- fitdist(tmax[2:25,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #             b <- fitdist(tmax[2:25,i],"norm")
  #             print(summary(a)$aic)
  #             print(summary(b)$aic)
  #             print(summary(a)$bic)
  #             print(summary(b)$bic)}

# gumbel: 1 norm: 2

#b
# 
      normal <- matrix(nrow=rep, ncol=2)

      for ( i in 2){
        Normal<-fitdist(tmax[,i],"norm")
        normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      }
      Gumbel<-fitdist(tmax[,1],"gumbel", start=list(a=0,b=1),optim.method="Nelder-Mead")
      normal[,1] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])

      colnames(normal) <- colnames(tmax)

# 1991-2012



# plot(fitdist(tmax[,2],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(tmax[,2],"norm"))
# plot(fitdist(tmax[,2],"gamma",method="mme"))
      
      # for ( i in 2:3){
      #             a <- fitdist(tmax[26:47,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
      #             b <- fitdist(tmax[26:47,i],"norm")
      #             print(summary(a)$aic)
      #             print(summary(b)$aic)
      #             print(summary(a)$bic)
      #             print(summary(b)$bic)}

#  norm: 1,2


        # normal <- matrix(nrow=rep, ncol=2)
        # 
        # for ( i in 1:2){
        #   Normal<-fitdist(tmax[,i],"norm")
        #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        # }
        # colnames(normal) <- colnames(tmax)


# CDF
# AUP2 <- vector("numeric", rep)
# for (i in 1:rep){
#   aup = (quantile(normal[,1], probs=simdata_au_tmax[i,1])* wt_au[,1] + quantile(normal[,2], probs=simdata_au_tmax[i,2])* wt_au[,2])
#   AUP2[i] <- aup
# }

# CDF independent

AUP2i <- vector("numeric", rep)
for (i in 1:rep){
  aupi = (quantile(normal[,1], probs=runif(1,0,1))* wt_au[,1] + quantile(normal[,2], probs=runif(1,0,1))* wt_au[,2])
  AUP2i[i] <- aupi
}

return(AUP2i)
}

clim <- as.data.frame(clim)
write.table(clim, "Australia_wheat_tmax_simulations_github_period1_indep.csv", sep=",", row.names=F)




# t25_prop_wheat <- matrix(ncol=2, nrow=1) 
# for ( i in 1:2){
#   t25_prop_wheat[,i] <- length( which( AUP0[i]>t25[i][1,] ) )/10000
# }
# return(t25_prop_wheat)
# }
# 
# 
# clim <- as.data.frame(clim)
# colnames(clim) <- colnames(normal)
# write.table(clim, "Australia_tmax_proportion_wheat_25_github_period1.csv", sep=",", row.names=F)


# ecdf

# ecdfPlot(clim[,1], main=" Average growing season max temperature Australia", xlab="Temperature in °C ", ylab="F(x)")
# 
# for ( u in 1:10){
#   ecdfPlot(clim[,u], add=T, ecdf.col="grey55", ecdf.lwd=0.5)
#   
# }


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_wheat <- read.table("wheat_threshold25_version3_github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("Australia_wheat_tmax_simulations_github_period1.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main=" Maximum temperature in Australia May-Nov ", xlab="Temperature (in °C) ", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.0005
# period 2: 0.0004


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_wheat [,7], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.0005" , "SE2 = 0.0004", c(paste("threshold = ", round(thres_wheat[1,7], digits =2), sep=""))))


# ecdfPlot(AUP, main=" Average growing season max temperature Australia", xlab="Temperature in °C ", ylab="F(x)")
# ecdfPlot(AUP, add=T, ecdf.col="grey55")



# CDF independent

AUP2i <- vector("numeric", rep)
for (i in 1:rep){
  aupi = (quantile(normal[,1], probs=runif(1,0,1))* wt_au[,1] + quantile(normal[,2], probs=runif(1,0,1))* wt_au[,2])
  AUP2i[i] <- aupi
}



######### Argentina --------

## argentina tmin------

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
tmin <- read.csv("Argentina_wheat_BB_tmin_May_Dec.csv", header=T, sep=",")


u_ar_tmin <- pobs(tmin[1:46,2:5])  #a
u_ar_tmin <- pobs(tmin[25:46,2:5])  #c
u_ar_tmin <- pobs(tmin[1:24,2:5])   #b

RVM = RVineStructureSelect(u_ar_tmin,c(1:6),progress=TRUE)

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
simdata_ar_tmin <- RVineSim(rep, RVM)


# marginal distributions


# plot(fitdist(tmin[,5],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(tmin[,2],"norm"))

# for ( i in 2:5){
#               a <- fitdist(tmin[2:47,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#               b <- fitdist(tmin[2:47,i],"norm")
#               print(summary(a)$aic)
#               print(summary(b)$aic)
#               print(summary(a)$bic)
#               print(summary(b)$bic)}
# 
# # normal: 2,3, 4,5



#a

      # normal <- matrix(nrow=rep, ncol=4)
      # for ( i in 1:4){
      # Normal<-fitdist(tmin[,i+1],"norm")
      # normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # 
      # colnames(normal) <- colnames(tmin[,2:5])

# 1967-1990

# plot(fitdist(tmin[2:25,4],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(tmin[2:25,4],"norm"))
# plot(fitdist(tmin[2:25,4],"gamma",method="mme"))
# 
  
  # for ( i in 2:5){
  #               a <- fitdist(tmin[2:25,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #               b <- fitdist(tmin[2:25,i],"norm")
  #               print(summary(a)$aic)
  #               print(summary(b)$aic)
  #               print(summary(a)$bic)
  #               print(summary(b)$bic)}
  #   
#normal: 2,3,4 gumbel:5

  #b

      normal <- matrix(nrow=rep, ncol=4)
      for ( i in 1:3){
        Normal<-fitdist(tmin[2:25,i+1],"norm")
        normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      }
      Gumbel<-fitdist(tmin[2:25,5],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
      normal[,4] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])


# 1991-2012

# plot(fitdist(tmin[26:47,5],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(tmin[26:47,5],"norm"))
      
      # for ( i in 2:5){
      #               a <- fitdist(tmin[26:47,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
      #               b <- fitdist(tmin[26:47,i],"norm")
      #               print(summary(a)$aic)
      #               print(summary(b)$aic)
      #               print(summary(a)$bic)
      #               print(summary(b)$bic)}

#gumbel:4 , norm=2,3,5

  #c
      # normal <- matrix(nrow=rep, ncol=4)
      # x=c(1:2,4)
      # for ( i in x){
      #   Normal<-fitdist(tmin[26:47,i+1],"norm")
      #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # 
      # Gumbel<-fitdist(tmin[26:47,4],"gumbel", start=list(a=0,b=1),optim.method="Nelder-Mead")
      # normal[,3] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])

      
      
# CDF
ART <- vector("numeric", rep)
for (i in 1:rep){
  art = (quantile(normal[,1], probs=simdata_ar_tmin[i,1])* wt_ar[,1] + quantile(normal[,2], probs=simdata_ar_tmin[i,2])* wt_ar[,2] + quantile(normal[,3], probs=simdata_ar_tmin[i,3])* wt_ar[,3]
        + quantile(normal[,4], probs=simdata_ar_tmin[i,4])* wt_ar[,4])

  ART[i] <- art
}


# ARTi <- vector("numeric", rep)
# for (i in 1:rep){
#   arti = (quantile(normal[,1], probs=runif(1,0,1))* wt_ar[,1] + quantile(normal[,2], probs=runif(1,0,1))* wt_ar[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_ar[,3] 
#           + quantile(normal[,4], probs=runif(1,0,1))* wt_ar[,4])
#   
#   ARTi[i] <- arti
# }

return(ART)
}

clim <- as.data.frame(clim)
write.table(clim, "Argentina_wheat_tmin_simulations_github_period1_v2.csv", sep=",", row.names=F)

# ecdf

# ecdfPlot(clim[,1], main=" Average growing season min temperature in °C Argentina", xlab="Temperature in °C", ylab="F(x)")
# 
# for ( u in 1:10){
#   ecdfPlot(clim[,u], add=T, ecdf.col="grey55", ecdf.lwd=0.5)
#   
# }

# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_wheat <- read.table("wheat_threshold25_version3_github_weighted_v2.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("Argentina_wheat_tmin_simulations_github_period2_v2.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main=" Minimum temperature in Argentina May-Dec ", xlab="Temperature (in °C) ", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.000297
# period 2: 0.000349


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_wheat [,1], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.00030" , "SE2 = 0.00035", c(paste("threshold = ", round(thres_wheat[1,1], digits =2), sep=""))))


# ecdfPlot(ART, main=" Average growing season min temperature in °C Argentina", xlab="Temperature in °C", ylab="F(x)")
# ecdfPlot(ART, add=T, ecdf.col="grey55")
# 
# 
# legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))



## independent

ARTi <- vector("numeric", rep)
for (i in 1:rep){
  arti = (quantile(normal[,1], probs=runif(1,0,1))* wt_ar[,1] + quantile(normal[,2], probs=runif(1,0,1))* wt_ar[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_ar[,3] 
         + quantile(normal[,4], probs=runif(1,0,1))* wt_ar[,4])
  
  ARTi[i] <- arti
}
ecdfPlot(ARTi,ecdf.lty=5, ecdf.col="grey55", add=T)

legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))




###### Europe ----

## EU t34 April to Aug------

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
t34 <- read.csv("EU_wheat_BB_t34_Apr_Aug.csv", header=T, sep=",")


u_eu_t34 <- pobs(t34[2:47,2:5])  #a
u_eu_t34 <- pobs(t34[26:47,2:5])  #c
u_eu_t34 <- pobs(t34[2:25,2:5])   #b


# t34 in UK is always zero

h=c(1,3:4)
RVM = RVineStructureSelect(u_eu_t34[,h],c(1:6),progress=TRUE)

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
  rep=10000
  simdata_eu_t34 <- RVineSim(rep, RVM)
  simdata_eu_t34 <-cbind(simdata_eu_t34[,1], rep(0,1000),simdata_eu_t34[,2:3] )
  colnames(simdata_eu_t34) <- colnames(t34[,2:5])
  
  # marginal distributions
  
  #  plot(fitdist(t34[,5],"gamma",start=list(a=0,b=1),method="mme"))
  #  plot(fitdist(t34[,5],"exp"))
  #  plot(fitdist(t34[,5],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
  
  # for ( i in 2:5){
  #               a <- fitdist(t34[2:47,i],"gamma",start=list(a=0,b=1),method="mme")
  #               b <- fitdist(t34[2:47,i],"exp")
  #               print(summary(a)$aic)
  #               print(summary(b)$aic)
  #               print(summary(a)$bic)
  #               print(summary(b)$bic)}

  #  gamma:2,4,5 
  
  
  #a
  
        # normal <- matrix(nrow=rep, ncol=4)
        # l=c(2,4:5)
        # for ( i in l){
        #   Gamma<-fitdist(t34[2:47,i],"gamma", method="mme")
        #   normal[,l-1] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
        # }
        # normal[,2]<- rep(0,rep)
        #   colnames(normal) <- colnames(t34[,2:5])
        # 
    
  # 1967-1990
    
    # plot(fitdist(t34[2:25,5],"gamma",start=list(a=0,b=1),method="mme"))
    # plot(fitdist(t34[2:25,5],"exp"))
    # plot(fitdist(t34[2:25,5],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
  
  # for ( i in 2:5){
  #                 a <- fitdist(t34[2:25,i],"gamma",start=list(a=0,b=1),method="mme")
  #                 b <- fitdist(t34[2:25,i],"exp")
  #                 print(summary(a)$aic)
  #                 print(summary(b)$aic)
  #                 print(summary(a)$bic)
  #                 print(summary(b)$bic)}
    # 
    # # gamma:2,4,5
    # 
    # i=2
    
#b
    
        normal <- matrix(nrow=rep, ncol=4)
        l=c(2,4:5)
        for ( i in l){
          Gamma<-fitdist(t34[2:25,i],"gamma", method="mme")
          normal[,l-1] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
        }
        normal[,2]<- rep(0,rep)
        colnames(normal) <- colnames(t34[,2:5])
        
  
  # 1991-2012
  
    
    # plot(fitdist(t34[26:47,5],"gamma",start=list(a=0,b=1),method="mme"))
    # plot(fitdist(t34[26:47,5],"exp"))
    # plot(fitdist(t34[26:47,5],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
        
    #     for ( i in 2:5){
    #                       a <- fitdist(t34[26:47,i],"gamma",start=list(a=0,b=1),method="mme")
    #                       b <- fitdist(t34[26:47,i],"exp")
    #                       print(summary(a)$aic)
    #                       print(summary(b)$aic)
    #                       print(summary(a)$bic)
    #                       print(summary(b)$bic)}
    # # 
    # # gamma:2,4,5
    # 
    
    
#c
    
          # normal <- matrix(nrow=rep, ncol=4)
          # l=c(2,4:5)
          # for ( i in l){
          #   Gamma<-fitdist(t34[26:47,i],"gamma", method="mme")
          #   normal[,l-1] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
          # }
          # normal[,2]<- rep(0,rep)
          # colnames(normal) <- colnames(t34[,2:5])

  
  
  # CDF
  # EUT <- vector("numeric", rep)
  # for (i in 1:rep){
  #   eut = (quantile(normal[,1], probs=simdata_eu_t34[i,1])* wt_eu[,1] + quantile(normal[,2], probs=simdata_eu_t34[i,2])* wt_eu[,2] + quantile(normal[,3], probs=simdata_eu_t34[i,3])* wt_eu[,3] 
  #          + quantile(normal[,4], probs=simdata_eu_t34[i,4])* wt_eu[,4])
  #   
  #   EUT[i] <- eut
  # }
  
  
  ## independent
  
  EUTi <- vector("numeric", rep)
  for (i in 1:rep){
    euti = (quantile(normal[,1], probs=runif(1,0,1))* wt_eu[,1] + quantile(normal[,2], probs=runif(1,0,1))* wt_eu[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_eu[,3] 
            + quantile(normal[,4], probs=runif(1,0,1))* wt_eu[,4])
    
    EUTi[i] <- euti
  }
  
  return(EUTi)
}

clim <- as.data.frame(clim)
write.table(clim, "Europe_wheat_t34_simulations_github_period1_indep.csv", sep=",", row.names=F)


# # ecdf
# 
# ecdfPlot(clim[,1], main=" Days above 34°C during growing season in Europe", xlab="Days above 34°C", ylab="F(x)")
# 
# for ( u in 1:10){
#   ecdfPlot(clim[,u], add=T, ecdf.col="grey55", ecdf.lwd=0.5)
#   
# }


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_wheat <- read.table("wheat_threshold25_version3_github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("Europe_wheat_t34_simulations_github_period1.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main=" Days above 34°C in Europe Apr-Aug", xlab="Number of days", ylab="F(x)", xlim=c(0,5))

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.000238
# period 2: 0.000526


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_wheat [,9], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.00024" , "SE2 = 0.00053", c(paste("threshold = ", round(thres_wheat[1,9], digits =2), sep=""))))


# ecdfPlot(EUT, main=" Number of days above 34°C in Europe", xlab="Number of days ", ylab="F(x)")
# ecdfPlot(EUT, add=T, ecdf.col="grey55")
# 
# 
# legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))

## independent

EUTi <- vector("numeric", rep)
for (i in 1:rep){
  euti = (quantile(normal[,1], probs=runif(1,0,1))* wt_eu[,1] + quantile(normal[,2], probs=runif(1,0,1))* wt_eu[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_eu[,3] 
          + quantile(normal[,4], probs=runif(1,0,1))* wt_eu[,4])
  
  EUTi[i] <- euti
}
ecdfPlot(EUTi,ecdf.lty=5, ecdf.col="grey55", add=T)

legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
legend("bottomright", legend=c("correlations included",  "independent provinces"), cex=0.9,lty=c (1,5),lwd=2)




# cum_prec Nov-Dec (year before harvest) Europe #######

setwd ("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
cum_prec <- read.csv("EU_wheat_BB_cum_precip_Nov_Dec.csv", header=T, sep=",")

cum_prec <- cum_prec[-47,-1]

u_eu_prec <- pobs(cum_prec[1:25,])  #b
u_eu_prec <- pobs(cum_prec[26:46,])  #c
u_eu_prec <- pobs(cum_prec)  #a

RVM = RVineStructureSelect(u_eu_prec,c(1:6),progress=TRUE)

cum_prec <- cum_prec[1:25,] #b
cum_prec <- cum_prec[26:46,] #c


clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
  rep=10000
  simdata_eu_prec <- RVineSim(rep, RVM)
  
  
  # marginal distributions
  # for (i in 1:4){
  # a<- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  # b <- fitdist(cum_prec[,i],"norm")
  # 
  # print(summary(a)$aic)
  # print(summary(b)$aic)
  # print(summary(a)$bic)
  # print(summary(b)$bic)
  # } 
  
  #norm for 2   gumbel: 1,3,4
  
  #a)
    # normal <- matrix(nrow=rep, ncol=4)
    # x = c(1,3:4)
    # for ( i in x){
    #   Normal<-fitdist(cum_prec[,2],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
    #   normal[,i] <- rgumbel(rep,Normal$estimate[1], Normal$estimate[2])
    #  }
    # Normal<-fitdist(cum_prec[,2],"norm")
    # normal[,2] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
    # colnames(normal) <- colnames(cum_prec[,1:4])
    # 
   
  #### 1967-1990
  #cum_prec <- cum_prec[1:24,]
  
  # plot(fitdist(cum_prec[,4],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
  # plot(fitdist(cum_prec[,4],"norm"))
  
  # a<- fitdist(cum_prec[,4],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  # b <- fitdist(cum_prec[,4],"norm")
  # 
  # summary(a)$aic
  # summary(b)$aic
  # summary(a)$bic
  # summary(b)$bic
  # 
  #gumbel:2,4  , normal:1,3
  
  #b) 
  
      normal <- matrix(nrow=rep, ncol=4)

        Normal<-fitdist(cum_prec[,1],"norm")
        normal[,1] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        Normal<-fitdist(cum_prec[,3],"norm")
        normal[,3] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        Gumbel<-fitdist(cum_prec[,2],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        normal[,2] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
        Gumbel<-fitdist(cum_prec[,4],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        normal[,4] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])


      colnames(normal) <- colnames(cum_prec[,1:4])
      
  # 1991-2012
  
#    for (i in 1:4){
#   a<- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(cum_prec[,i],"norm")
#   
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(a)$bic)
#   print(summary(b)$bic)
# } 
#   
  # plot(fitdist(cum_prec[,2],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
  # plot(fitdist(cum_prec[,2],"norm"))
  
  #gumbel:1,3  , normal: 2,4
  
  #c)
  # 

        # normal <- matrix(nrow=rep, ncol=4)
        # 
        # Normal<-fitdist(cum_prec[,2],"norm")
        # normal[,2] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        # Normal<-fitdist(cum_prec[,4],"norm")
        # normal[,4] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        # Gumbel<-fitdist(cum_prec[,1],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        # normal[,1] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
        # Gumbel<-fitdist(cum_prec[,3],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        # normal[,3] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
        # 
        # colnames(normal) <- colnames(cum_prec[,1:4])

  
  
  # CDF
  # EUC <- vector("numeric", rep)
  # for (i in 1:rep){
  #   euc = (quantile(normal[,1], probs=simdata_eu_prec[i,1])* wt_eu[,1] + quantile(normal[,2], probs=simdata_eu_prec[i,2])* wt_eu[,2] + quantile(normal[,3], probs=simdata_eu_prec[i,3])* wt_eu[,3] 
  #         + quantile(normal[,4], probs=simdata_eu_prec[i,4])* wt_eu[,4]
  #   )
  #   
  #   EUC[i] <- euc 
  # }
  
  # CDF indep
  EUCi <- vector("numeric", rep)
  for (i in 1:rep){
    euci = (quantile(normal[,1], probs=runif(1,0,1))* wt_eu[,1] + quantile(normal[,2], probs=runif(1,0,1))* wt_eu[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_eu[,3] 
           + quantile(normal[,4], probs=runif(1,0,1))* wt_eu[,4]
    )
    
    EUCi[i] <- euci 
  }
  
  return(EUCi)
}

clim <- as.data.frame(clim)
write.table(clim, "Europe_wheat_cum_prec_simulations_github_period1_indep.csv", sep=",", row.names=F)

# # ecdf
# 
# ecdfPlot(clim[,1], main=" Cumulative precipitation November and December Europe", xlab="Precipitation in mm", ylab="F(x)")
# 
# for ( u in 1:10){
#   ecdfPlot(clim[,u], add=T, ecdf.col="grey55", ecdf.lwd=0.5)
#   
# }

# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_wheat <- read.table("wheat_threshold25_version3_github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("Europe_wheat_cum_prec_simulations_github_period2.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main=" Cumulative precipitation in Europe Nov-Dec", xlab="Precipitation (in mm)", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.0138
# period 2: 0.0151


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_wheat [,8], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.014" , "SE2 = 0.015", c(paste("threshold = ", round(thres_wheat[1,8], digits =2), sep=""))))


## Russia/Ukraine tmean May to Aug -----


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
tmean <- read.csv("RussUkr_wheat_BB_tmean_May_Aug.csv", header=T, sep=",")


u_ruk_tmean <- pobs(tmean[2:47,2:5])  #a
u_ruk_tmean <- pobs(tmean[26:47,2:5])  #c
u_ruk_tmean <- pobs(tmean[2:25,2:5])   #b


RVM = RVineStructureSelect(u_ruk_tmean,c(1:6),progress=TRUE)


tmean <- tmean[2:25,]  #b
tmean <- tmean[26:47,]  #c

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
  rep=10000
  simdata_ruk_tmean <- RVineSim(rep, RVM)
  colnames(simdata_ruk_tmean) <- colnames(tmean[,2:5])
  
  # marginal distributions

#    plot(fitdist(tmean[,2],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
#    plot(fitdist(tmean[,2],"norm"))
#   
#      
# for (i in 2:5){
#   a<- fitdist(tmean[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(tmean[,i],"norm")
# 
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(a)$bic)
#   print(summary(b)$bic )}

  #gumbel:2  , normal:3,4,5

  
  #a
  
        # normal <- matrix(nrow=rep, ncol=4)
        # 
        # for ( i in 2:5){
        #   Normal<-fitdist(tmean[,i],"norm")
        #   normal[,i-1] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        #    }
        #   
        #   Normal<-fitdist(tmean[,2],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        #   normal[,1] <- rgumbel(rep,Normal$estimate[1], Normal$estimate[2])
        #   colnames(normal) <- colnames(tmean[,2:5])

  
  # 1967-1990
  
  #   for (i in 2:5){
  #     a<- fitdist(tmean[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #     b <- fitdist(tmean[,i],"norm")
  #     
  #     print(summary(a)$aic)
  #     print(summary(b)$aic)
  #     print(summary(a)$bic)
  #     print(summary(b)$bic )}
  # # 
  # gumbel: 2:5
  
  #b
          normal <- matrix(nrow=rep, ncol=4)

          for ( i in 2:5){
            Normal<-fitdist(tmean[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
            normal[,i-1] <- rgumbel(rep,Normal$estimate[1], Normal$estimate[2])

          }

          colnames(normal) <- colnames(tmean[,2:5])

  
  # 1991-2012
          
          #  plot(fitdist(normal[,1], "gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead"))
          # 
          #  plot(fitdist(tmean[,2],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
          #  plot(fitdist(tmean[,2],"norm"))
          # #   
  
   # for (i in 2:5){
   #    a<- fitdist(tmean[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
   #    b <- fitdist(tmean[,i],"norm")
   # 
   #    print(summary(a)$aic)
   #    print(summary(b)$aic)
   #    print(summary(a)$bic)
   #    print(summary(b)$bic )}
    # # 
    # gumbel: 2,4,5  normal: 3

  # 
  
  #c
  
        # normal <- matrix(nrow=rep, ncol=4)
        # x = c(2,4:5)
        # for ( i in x){
        #   Normal<-fitdist(tmean[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        #   normal[,i-1] <- rgumbel(rep,Normal$estimate[1], Normal$estimate[2])
        #  }
        # Normal<-fitdist(tmean[,3],"norm")
        # normal[,2] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        # colnames(normal) <- colnames(tmean[,2:5])


  
  # # CDF
  # RUKT <- vector("numeric", rep)
  # for (i in 1:rep){
  #   rukt = (quantile(normal[,1], probs=simdata_ruk_tmean[i,1]) * wt_ruk[,1]+ quantile(normal[,2], probs=simdata_ruk_tmean[i,2])* wt_ruk[,2] + quantile(normal[,3], probs=simdata_ruk_tmean[i,3])* wt_ruk[,3] 
  #          + quantile(normal[,4], probs=simdata_ruk_tmean[i,4])* wt_ruk[,4])
  #   
  #   RUKT[i] <- rukt
  # }
  
  ## independent
  
  RUKTi <- vector("numeric", rep)
  for (i in 1:rep){
    rukti = (quantile(normal[,1], probs=runif(1,0,1))* wt_ruk[,1]+ quantile(normal[,2], probs=runif(1,0,1))* wt_ruk[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_ruk[,3] 
             + quantile(normal[,4], probs=runif(1,0,1))* wt_ruk[,4])
    
    RUKTi[i] <- rukti
  }
  
  
  return(RUKTi)
}

clim <- as.data.frame(clim)
write.table(clim, "RussUkr_wheat_tmean_simulations_github_period1_indep.csv", sep=",", row.names=F)



# ecdf

# ecdfPlot(clim[,1], main=" Average temperature during growing season in Russia/Ukraine", xlab="Temperature in °C", ylab="F(x)")
# 
# for ( u in 1:10){
#   ecdfPlot(clim[,u], add=T, ecdf.col="grey55", ecdf.lwd=0.5)
#   
# }


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_wheat <- read.table("wheat_threshold25_version3_github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("RussUkr_wheat_tmean_simulations_github_period2.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main=" Average temperature in Russia/Ukraine Mar-Jun", xlab="Temperature (in °C)", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.00059
# period 2: 0.00059


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_wheat [,11], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.0006" , "SE2 = 0.0006", c(paste("threshold = ", round(thres_wheat[1,11], digits =2), sep=""))))


# ecdfPlot(RUKT, main=" Average temperature during growing season in Russia/Ukraine", xlab="Temperature in °C  ", ylab="F(x)")
# ecdfPlot(RUKT, add=T, ecdf.col="grey55")
# 

# 
# legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))

## independent

RUKTi <- vector("numeric", rep)
for (i in 1:rep){
  rukti = (quantile(normal[,1], probs=runif(1,0,1))* wt_ruk[,1]+ quantile(normal[,2], probs=runif(1,0,1))* wt_ruk[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_ruk[,3] 
          + quantile(normal[,4], probs=runif(1,0,1))* wt_ruk[,4])
  
  RUKTi[i] <- rukti
}
ecdfPlot(RUKTi,ecdf.lty=5, ecdf.col="grey55", add=T)

legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
legend("bottomright", legend=c("correlations included",  "independent provinces"), cex=0.9,lty=c (1,5),lwd=2)



# cum_prec Mar-Jun Russia/Ukraine #######

setwd ("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
cum_prec <- read.csv("RussUkr_wheat_BB_cum_precip_Mar_Jun.csv", header=T, sep=",")


u_ruk_prec <- pobs(cum_prec[2:25,2:5])  #b
u_ruk_prec <- pobs(cum_prec[26:47,2:5])  #c
u_ruk_prec <- pobs(cum_prec[2:47,2:5 ])  #a

RVM = RVineStructureSelect(u_ruk_prec,c(1:6),progress=TRUE)

cum_prec <- cum_prec[2:25,] #b
cum_prec <- cum_prec[26:46,] #c


clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
  rep=10000
  simdata_ruk_prec <- RVineSim(rep, RVM)
  
  
  # marginal distributions
  
  # for (i in 2:5){
  # a<- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  # b <- fitdist(cum_prec[,i],"norm")
  # 
  # print(summary(a)$aic)
  # print(summary(b)$aic)
  # print(summary(a)$bic)
  # print(summary(b)$bic)
  # }

  #norm for 2:5
  
  #a)

  # normal <- matrix(nrow=rep, ncol=4)
  # for ( i in 2:5){
  #   Normal<-fitdist(cum_prec[,i],"norm")
  #   normal[,i-1] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
  #   
  #  }
  # 
  # colnames(normal) <- colnames(cum_prec[,2:5])


  #### 1967-1990

  # plot(fitdist(cum_prec[,4],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
  # plot(fitdist(cum_prec[,4],"norm"))
  
  # for (i in 2:5){
  # a<- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  # b <- fitdist(cum_prec[,i],"norm")
  # 
  # print(summary(a)$aic)
  # print(summary(b)$aic)
  # print(summary(a)$bic)
  # print(summary(b)$bic)}
  # # 
  # #normal: 2:5
  
  #b) 
  
        normal <- matrix(nrow=rep, ncol=4)
        for ( i in 2:5){
          Normal<-fitdist(cum_prec[,i],"norm")
          normal[,i-1] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])

        }

        colnames(normal) <- colnames(cum_prec[,2:5])
    
  
  # 1991-2012
  
  #    for (i in 2:5){
  #   a<- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #   b <- fitdist(cum_prec[,i],"norm")
  # 
  #   print(summary(a)$aic)
  #   print(summary(b)$aic)
  #   print(summary(a)$bic)
  #   print(summary(b)$bic)
  # }

  # plot(fitdist(cum_prec[,2],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
  # plot(fitdist(cum_prec[,2],"norm"))
  
  # normal: 2:5
  
  #c)
  # 
  # 
        # normal <- matrix(nrow=rep, ncol=4)
        # for ( i in 2:5){
        #   Normal<-fitdist(cum_prec[,i],"norm")
        #   normal[,i-1] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        # 
        # }
        # 
        # colnames(normal) <- colnames(cum_prec[,2:5])

  
  # CDF
  # RUKC <- vector("numeric", rep)
  # for (i in 1:rep){
  #   rukc = (quantile(normal[,1], probs=simdata_ruk_prec[i,1])* wt_ruk[,1] + quantile(normal[,2], probs=simdata_ruk_prec[i,2])* wt_ruk[,2] + quantile(normal[,3], probs=simdata_ruk_prec[i,3])* wt_ruk[,3] 
  #          + quantile(normal[,4], probs=simdata_ruk_prec[i,4])* wt_ruk[,4]
  #   )
  #   
  # RUKC[i] <- rukc
  # }
  
  
  # CDF indep
  RUKCi <- vector("numeric", rep)
  for (i in 1:rep){
    rukci = (quantile(normal[,1], probs=runif(1,0,1))* wt_ruk[,1] + quantile(normal[,2], probs=runif(1,0,1))* wt_ruk[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_ruk[,3] 
            + quantile(normal[,4], probs=runif(1,0,1))* wt_ruk[,4]
    )
    
    RUKCi[i] <- rukci
  }
  
  return(RUKCi)
}

clim <- as.data.frame(clim)
write.table(clim, "RussUkr_wheat_cum_prec_simulations_github_period1_indep.csv", sep=",", row.names=F)


# ecdf

# ecdfPlot(clim[,1], main=" Precipitation in Russia/Ukraine", xlab="Precipitation (in mm)", ylab="F(x)")
# 
# for ( u in 1:10){
#   ecdfPlot(clim[,u], add=T, ecdf.col="grey55", ecdf.lwd=0.5)
#   
# }


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_wheat <- read.table("wheat_threshold25_version3_github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("RussUkr_wheat_cum_prec_simulations_github_period1.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main=" Precipitation in Russia/Ukraine Mar-Jun", xlab="Precipitation (in mm)", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.0146
# period 2: 0.0145


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_wheat [,10], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.015" , "SE2 = 0.015", c(paste("threshold = ", round(thres_wheat[1,10], digits =2), sep=""))))



######## wheat all ---------

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")

# version 3: both indicators for USA and Australia
var <- read.csv("wheat_climate_indicators_BB_version3_github_weighted.csv", header=T, sep=",")

require(PerformanceAnalytics)
chart.Correlation (var[,2:12], method="kendall", pch=20, smooth=F,lines=F, main=" Wheat climate correlation structures", cex.main=0.9)

# extracting correlations between indicators of the same BB
u_var1 <- pobs(var[,5:6])
u_var2 <- pobs(var[,7:8])
u_var3 <- pobs(var[,9:10])
u_var4 <- pobs(var[,11:12])

a <- BiCopSelect(u_var1[,1], u_var1[,2], familyset=c(1:6))
b <- BiCopSelect(u_var2[,1], u_var2[,2], familyset=c(1:6))
c <- BiCopSelect(u_var3[,1], u_var3[,2], familyset=c(1:6))
d <- BiCopSelect(u_var4[,1], u_var4[,2], familyset=c(1:6))

rep=10000
simdata_var1 <- BiCopSim(rep, a$family, a$par)
simdata_var2 <- BiCopSim(rep, b$family, b$par)
simdata_var3 <- BiCopSim(rep, c$family, c$par)
simdata_var4 <- BiCopSim(rep, d$family, d$par)


####### risks of simultaneous negative climate conditions ###
## version 3 -----
setwd("H:/DPhil/breadbaskets/Data collection")
t25 <- read.csv("wheat_threshold25_version3_github_weighted_v2.csv", header=T, sep=",")
# uncertainty test:
t25 <- read.csv("wheat_threshold25_prediction_0.9_version3_github_weighted_v2.csv", header=T, sep=",")
t25l<- t25[2,]
t25u<- t25[3,]

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
ART <- read.csv("Argentina_wheat_tmin_simulations_github_period2_v2.csv", header=T, sep=",")
SI <- read.csv("India_wheat_spi_simulations_github_period2.csv", header=T, sep=",")
PC <- read.csv("China_wheat_cum_prec_simulations_github_period2.csv", header=T, sep=",")
SU <- read.csv("USA_wheat_cum_prec_simulations_github_period2.csv", header=T, sep=",")
SU2 <- read.csv("USA_wheat_t34_simulations_github_period2.csv", header=T, sep=",")
AUP <- read.csv("Australia_wheat_cum_prec_simulations_github_period2.csv", header=T, sep=",")
AUP2 <- read.csv("Australia_wheat_tmax_simulations_github_period2.csv", header=T, sep=",")
EUC <- read.csv("Europe_wheat_cum_prec_simulations_github_period2.csv", header=T, sep=",")
EUT <- read.csv("Europe_wheat_t34_simulations_github_period2.csv", header=T, sep=",")
RUKC <- read.csv("RussUkr_wheat_cum_prec_simulations_github_period2.csv", header=T, sep=",")
RUKT <- read.csv("RussUkr_wheat_tmean_simulations_github_period2.csv", header=T, sep=",")

# version 3
u_var <- pobs(var[,2:12])

RVM = RVineStructureSelect(u_var,c(1:6),progress=TRUE)


clim <- foreach (i=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula")) %dopar% {
  

rep=1000
simdata_var <- RVineSim(rep, RVM)

# version 3
ALL0 <- matrix(nrow=rep, ncol=11)
for (n in 1:rep){
  all0 = c(quantile(ART[,i], probs=simdata_var[n,1]),quantile(SI[,i], probs=simdata_var[n,2]), quantile(PC[,i], probs=simdata_var[n,3]),
           quantile(SU[,i], probs=simdata_var[n,4]), quantile(SU2[,i], probs=simdata_var[n,5]),quantile(AUP[,i], probs=simdata_var[n,6]),quantile(AUP2[,i], probs=simdata_var[n,7]),
           quantile(EUC[,i], probs=simdata_var[n,8]),quantile(EUT[,i], probs=simdata_var[n,9]),quantile(RUKC[,i], probs=simdata_var[n,10]),quantile(RUKT[,i], probs=simdata_var[n,11]))

  ALL0[n,] <- all0
}

ALL0 <- as.data.frame(ALL0)



# # version 3 (independent BBs, but dependence between climate variables of same BB)

# ALL0 <- matrix(nrow=rep, ncol=11)
# for (n in 1:rep){
#   all0i = c(quantile(ART[,i], probs=runif(1,0,1)),quantile(SI[,i], probs=runif(1,0,1)), quantile(PC[,i], probs=runif(1,0,1)),
# quantile(SU[,i], probs=simdata_var1[n,1]), quantile(SU2[,i], probs=simdata_var1[n,2]),quantile(AUP[,i], probs=simdata_var2[n,1]),quantile(AUP2[,i], probs=simdata_var2[n,2]),
# quantile(EUC[,i], probs=simdata_var3[n,1]),quantile(EUT[,i], probs=simdata_var3[n,2]),quantile(RUKC[,i], probs=simdata_var4[n,1]),quantile(RUKT[,i], probs=simdata_var4[n,2]))
# 
#   ALL0[n,] <- all0i
# }
# 
# ALL0 <- as.data.frame(ALL0)



###
# portion below 25% threshold ## version 3 -----------

for (l in 1:rep) {
  ALL0$V12[l] <- sum((ifelse(ALL0$V1[l]>t25l[1],1,0))[1,],(ifelse(ALL0$V2[l]<t25u[2],1,0))[1,],(ifelse(ALL0$V3[l]<t25u[3],1,0))[1,],
                    (ifelse(ALL0$V4[l]<t25u[4] | ALL0$V5[l]>t25l[5],1,0))[1,],(ifelse(ALL0$V6[l]<t25u[6] | ALL0$V7[l]>t25l[7],1,0))[1,],
                    (ifelse(ALL0$V8[l]<t25u[8] | ALL0$V9[l]>t25l[9],1,0))[1,],(ifelse(ALL0$V10[l]<t25u[10] | ALL0$V11[l]>t25l[11],1,0))[1,])
  
}
col <- c(colnames(var[,2:12]), "BBs")
colnames(ALL0) <- col

return(ALL0[,12])
}


write.table(clim, "wheat_all_version3_t25_period1_github_weighted_indep_v2.csv", sep=",", row.names = F)
write.table(clim, "wheat_all_version3_t25_highrisk_prediction_90_period2_github_weighted_v2.csv", sep=",", row.names = F)


# sum(ALL0$V6)/10000
# hist(ALL0$V7, freq=F)
# mean(ALL0$V7)




ALL0<- clim
setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
ALL0 <- read.csv("wheat_all_version3_t25_period1_github_weighted_v2.csv", sep=",", header=T)
ALL0 <- read.csv("wheat_all_version3_t25_lowrisk_prediction_90_period1_github_weighted_v2.csv", sep=",", header=T)

# indep
ALL0 <- read.csv("wheat_all_version3_t25_period1_github_weighted_indep_v2.csv", sep=",", header=T)


ALL0_perc <- matrix(nrow=1000, ncol=8)
#period1
for (o in 1:1000){
for (i in 0:7){
  ALL0_perc[o,i+1]<- sum(ALL0[,o]==i)/1000 
}}

m1 <- apply(ALL0_perc,2,mean)
s1 <- apply(ALL0_perc,2,sd)


ALL0 <- read.csv("wheat_all_version3_t25_period2_github_weighted_v2.csv", sep=",", header=T)
ALL0 <- read.csv("wheat_all_version3_t25_lowrisk_prediction_90_period2_github_weighted_v2.csv", sep=",", header=T)

# indep
ALL0 <- read.csv("wheat_all_version3_t25_period2_github_weighted_indep_v2.csv", sep=",", header=T)

ALL0_perc <- matrix(nrow=1000, ncol=8)
#period2
for (o in 1:1000){
  for (i in 0:7){
    ALL0_perc[o,i+1]<- sum(ALL0[,o]==i)/1000 
  }}

m2 <- apply(ALL0_perc,2,mean)
s2 <- apply(ALL0_perc,2,sd)

stats<- rbind(m1,s1,m2,s2)



write.table(stats, "wheat_global_simultaneous_pdf_25_version_3_github_weighted_indep_v2.csv", sep=",", #row.names=c("var" ,"mean_period1","sd_period1",  "mean_period2","sd_period2"),
            col.names=c(0:7))

write.table(stats, "wheat_global_simultaneous_pdf_25_version_3_lowrisk_prediction_90_github_weighted_v2.csv", sep=",", #row.names=c("var" ,"mean_period1","sd_period1",  "mean_period2","sd_period2"),
            col.names=c(0:7))

## # ## ###
# significant change between 2 periods?? 


period1 <- read.csv("wheat_all_version3_t25_period1_github_weighted_v2.csv", sep=",", header=T)
period2 <- read.csv("wheat_all_version3_t25_period2_github_weighted_v2.csv", sep=",", header=T)

# threshold uncertainty analysis:
period1 <- read.csv("wheat_all_version3_t25_highrisk_prediction_90_period1_github_weighted_v2.csv", sep=",", header=T)
period2 <- read.csv("wheat_all_version3_t25_highrisk_prediction_90_period2_github_weighted_v2.csv", sep=",", header=T)



period1_mean <- apply(period1,2,sort)
period1_mean <- apply(period1_mean, 1, mean)
period2_mean <- apply(period2, 2, sort)
period2_mean <- apply(period2_mean, 1, mean)

period1i <- read.csv("wheat_all_version3_t25_period1_github_weighted_indep_v2.csv", sep=",", header=T)
period2i <- read.csv("wheat_all_version3_t25_period2_github_weighted_indep_v2.csv", sep=",", header=T)

period1i_mean <- apply(period1i,2,sort)
period1i_mean <- apply(period1i_mean, 1, mean)
period2i_mean <- apply(period2i, 2, sort)
period2i_mean <- apply(period2i_mean, 1, mean)


# Wilcoxon (or Mann-Whitney-Wilcoxon) test -> assumes normal distribution for large sample numbers
wilcox.test(period2_mean, period1_mean)
# -> p=0; p<=0.05 or 0.001 means populations are nonidentical

# between dependencies:
# Wilcoxon (or Mann-Whitney-Wilcoxon) test -> assumes normal distribution
wilcox.test(period2_mean, period2i_mean)
wilcox.test(period1_mean, period1i_mean)

# between periods:
# Wilcoxon (or Mann-Whitney-Wilcoxon) test -> assumes normal distribution
wilcox.test(period1i_mean, period2i_mean, alternative = "greater")

mean(period2_mean)-mean(period1_mean)
# diff= 0.5759



# all dependencies and periods:
setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")

period1 <- read.csv("wheat_all_version3_t25_period1_github_weighted_v2.csv", sep=",", header=T)
period2 <- read.csv("wheat_all_version3_t25_period2_github_weighted_v2.csv", sep=",", header=T)
period1 <- period1[,8]
period2 <- period2[,8]

period1i <- read.csv("wheat_all_version3_t25_period1_github_weighted_indep_v2.csv", sep=",", header=T)
period2i <- read.csv("wheat_all_version3_t25_period2_github_weighted_indep_v2.csv", sep=",", header=T)
period1i <- period1i[,8]
period2i <- period2i[,8]



# change of risks between two periods ##########


t25_prop_wheat_bb <- matrix(ncol=11, nrow=1000)

for ( i in 1:1000){
t25_prop_wheat_bb[i,1] <- length( which( ART[,i] >t25[1][1,] ) )/10000
t25_prop_wheat_bb[i,2] <- length( which( SI[,i]<t25[2][1,] ) )/10000
t25_prop_wheat_bb[i,3] <- length( which( PC[,i]<t25[3][1,] ) )/10000
t25_prop_wheat_bb[i,4] <- length( which( SU[,i]<t25[4][1,] ) )/10000
t25_prop_wheat_bb[i,5] <- length( which( SU2[,i]>t25[5][1,] ) )/10000
t25_prop_wheat_bb[i,6] <- length( which( AUP[,i] <t25[6][1,] ) )/10000
t25_prop_wheat_bb[i,7] <- length( which( AUP2[,i]>t25[7][1,] ) )/10000
t25_prop_wheat_bb[i,8] <- length( which( EUC[,i] <t25[8][1,] ) )/10000
t25_prop_wheat_bb[i,9] <- length( which( EUT[,i] >t25[9][1,] ) )/10000
t25_prop_wheat_bb[i,10] <- length( which( RUKC[,i] <t25[10][1,] ) )/10000
t25_prop_wheat_bb[i,11] <- length( which( RUKT[,i] >t25[11][1,] ) )/10000

}

colnames(t25_prop_wheat_bb) <- colnames(t25)
write.table(t25_prop_wheat_bb, "proportion_wheat_25_global_bb_period2_github.csv", sep=",", row.names=F)

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
prop1_25 <- read.table("proportion_wheat_25_global_bb_period1_github.csv", sep=",", header=T)
prop2_25 <- read.table("proportion_wheat_25_global_bb_period2_github.csv", sep=",", header=T)

diff_25 <- prop2_25 - prop1_25
diff_25_stats <- matrix(ncol=11, nrow=2)
diff_25_stats[1,] <- apply(diff_25, 2, mean)
diff_25_stats[2,] <- apply(diff_25, 2, sd)
colnames(diff_25_stats) <- colnames(diff_25)
write.table(diff_25_stats, "proportion_diff_stats_wheat_25_global_bb_github.csv", sep=",", row.names=F)


################## MAIZE ----------

setwd("H:/DPhil/breadbaskets/Data collection")
res <- read.table("residual_maize.csv", sep=",", header=T)

t25_prop_maize <- matrix(nrow=2, ncol=33)
colnames(t25_prop_maize) <- colnames(res[,2:34])

setwd("H:/DPhil/breadbaskets/Data collection")
wt <- read.table("maize_average_area_weight.csv", sep=",", header=T)
wt_us <- wt[,24:33]
wt_in <- wt[,17:23]
wt_ch <- wt[,1:7]
wt_br <- wt[,8:12]
wt_ar <- wt[,13:16]
wt_eu <- wt[,34:39]


### USA --------
setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
t29_us <- read.csv("USA_maize_BB_t29_May_Nov.csv", header=T, sep=",")

## usa t29 -----
u_usa_t29 <- pobs(t29_us[,2:11])
RVM = RVineStructureSelect(u_usa_t29,c(1:6),progress=TRUE)

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  

rep=10000
simdata_usa_t29 <- RVineSim(rep, RVM)


# marginal distributions

# plot(fitdist(t29_us[,i],"gamma",start=list(a=0,b=1),method="mme"))
# #plot(fitdist(t29_us[,i],"exp"))
# plot(fitdist(t29_us[,i],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(t29_us[,i],"norm"))

# for ( i in 2:11){
#                 a <- fitdist(t29_us[2:47,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#                 b <- fitdist(t29_us[2:47,i],"norm")
#                 c <- fitdist(t29_us[2:47,i],"gamma",start=list(a=0,b=1),method="mme")
#                 print(summary(a)$aic)
#                 print(summary(b)$aic)
#                 print(summary(c)$aic)
#                 print(summary(a)$bic)
#                 print(summary(b)$bic)
#                 print(summary(c)$bic)}

#gamma: 4,5,6,11, norm: 2,3,7,8, 10  gumbel:9

###

#a
      x= c(1,2,6,7,9)
      normal <- matrix(nrow=rep, ncol=10)
      for ( i in x){
        Normal<-fitdist(t29_us[,i+1],"norm")
        normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      }
      y=c(8)
      for ( i in y){
        Gumbel<-fitdist(t29_us[,i+1],"gumbel", start=list(a=0,b=1),optim.method="Nelder-Mead")
        normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      }
      
      f=c(3:5,10)
      for ( i in f){
        Normal<-fitdist(t29_us[,i+1],"gamma", method="mme")
        normal[,i] <- rgamma(rep,Normal$estimate[1], Normal$estimate[2])
      }
      colnames(normal) <- colnames(t29_us[,2:11])


# CDF
TU <- vector("numeric", rep)
for (i in 1:rep){
  tu = (quantile(normal[,1], probs=simdata_usa_t29[i,1])*wt_us[,1]+ quantile(normal[,2], probs=simdata_usa_t29[i,2])*wt_us[,2] + quantile(normal[,3], probs=simdata_usa_t29[i,3])*wt_us[,3] 
        + quantile(normal[,4], probs=simdata_usa_t29[i,4])*wt_us[,4]+ quantile(normal[,5], probs=simdata_usa_t29[i,5])*wt_us[,5]+  quantile(normal[,6], probs=simdata_usa_t29[i,6])*wt_us[,6]
        + quantile(normal[,7], probs=simdata_usa_t29[i,7])*wt_us[,7]+  quantile(normal[,8], probs=simdata_usa_t29[i,8])*wt_us[,8]+  quantile(normal[,9], probs=simdata_usa_t29[i,9])*wt_us[,9]
        + quantile(normal[,10], probs=simdata_usa_t29[i,10])*wt_us[,10] )
  
  TU[i] <- tu 
}
return(TU)
}

# ecdfPlot(TU, main=" Days above 29°C during growing season USA", xlab="Days above 29°C", ylab="F(x)")
# ecdfPlot(TU, add=T, ecdf.col="grey55")

clim <- as.data.frame(clim)
write.table(clim, "USA_maize_t29_simulations_github.csv", sep=",", row.names=F)

# ecdf

# ecdfPlot(clim[,1], main=" Days above 29°C during growing season USA", xlab="Days above 29°C during growing season USA", ylab="F(x)")
# 
# for ( u in 1:10){
#   ecdfPlot(clim[,u], add=T, ecdf.col="grey55", ecdf.lwd=0.5)
#   
# }



#### 1967-1990

#t29_us<- t29_us[2:25,]
u_usa_t29 <- pobs(t29_us[2:25,2:11])
RVM = RVineStructureSelect(u_usa_t29,c(1:6),progress=TRUE)

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
simdata_usa_t29 <- RVineSim(rep, RVM)


# marginal distributions

# plot(fitdist(t29_us[,i],"gamma",start=list(a=0,b=1),method="mme"))
# #plot(fitdist(t29_us[,i],"exp"))
# plot(fitdist(t29_us[,i],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(t29_us[,i],"norm"))
# 
# for ( i in 2:11){
#                   a <- fitdist(t29_us[2:25,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#                   b <- fitdist(t29_us[2:25,i],"norm")
#                   c <- fitdist(t29_us[2:25,i],"gamma",start=list(a=0,b=1),method="mme")
#                   print(summary(a)$aic)
#                   print(summary(b)$aic)
#                   print(summary(c)$aic)
#                   print(summary(a)$bic)
#                   print(summary(b)$bic)
#                   print(summary(c)$bic)}
#   

#gamma: 7  norm: 2, 10  gumbel: 3,4, 5,6,8, 9,11



###
#b

        x= c(1,9)
        normal <- matrix(nrow=rep, ncol=10)
        for ( i in x){
          Normal<-fitdist(t29_us[2:25,i+1],"norm")
          normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        }
        y=c(2,3,4,5,7,8,10)
        for ( i in y){
          Gumbel<-fitdist(t29_us[2:25,i+1],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
          normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
        }
        r=c(6)
        for ( i in r){
          Normal<-fitdist(t29_us[2:25,i+1],"gamma", method="mme")
          normal[,i] <- rgamma(rep,Normal$estimate[1], Normal$estimate[2])
        }
        colnames(normal) <- colnames(t29_us[,2:11])
        

# # CDF
# TU <- vector("numeric", rep)
# for (i in 1:rep){
#   tu = (quantile(normal[,1], probs=simdata_usa_t29[i,1])*wt_us[,1]+ quantile(normal[,2], probs=simdata_usa_t29[i,2])*wt_us[,2] + quantile(normal[,3], probs=simdata_usa_t29[i,3])*wt_us[,3] 
#         + quantile(normal[,4], probs=simdata_usa_t29[i,4])*wt_us[,4]+ quantile(normal[,5], probs=simdata_usa_t29[i,5])*wt_us[,5]+  quantile(normal[,6], probs=simdata_usa_t29[i,6])*wt_us[,6]
#         + quantile(normal[,7], probs=simdata_usa_t29[i,7])*wt_us[,7]+  quantile(normal[,8], probs=simdata_usa_t29[i,8])*wt_us[,8]+  quantile(normal[,9], probs=simdata_usa_t29[i,9])*wt_us[,9]
#         + quantile(normal[,10], probs=simdata_usa_t29[i,10])*wt_us[,10] )
#   
#   TU[i] <- tu 
# }

# CDF independent

TUi<- vector("numeric", rep)
for (i in 1:rep){
  tui = (quantile(normal[,1], probs=runif(1,0,1))*wt_us[,1]+ quantile(normal[,2], probs=runif(1,0,1))*wt_us[,2] + quantile(normal[,3], probs=runif(1,0,1))*wt_us[,3] 
         + quantile(normal[,4], probs=runif(1,0,1))*wt_us[,4] + quantile(normal[,5], probs=runif(1,0,1))*wt_us[,5] +  quantile(normal[,6], probs=runif(1,0,1))*wt_us[,6]
         + quantile(normal[,7], probs=runif(1,0,1))*wt_us[,7] +  quantile(normal[,8], probs=runif(1,0,1))*wt_us[,8] +  quantile(normal[,9], probs=runif(1,0,1))*wt_us[,9]
         + quantile(normal[,10], probs=runif(1,0,1))*wt_us[,10])
  
  TUi[i] <- tui 
}

return(TUi)
}

#ecdfPlot(TU, main=" Days above 29°C during growing season USA", xlab="Days above 29°C", ylab="F(x)")

clim <- as.data.frame(clim)
write.table(clim, "USA_maize_t29_simulations_github_period1_indep.csv", sep=",", row.names=F)


# ecdf

ecdfPlot(clim[,1], main=" Days above 29°C during growing season USA", xlab="Days above 29°C ", ylab="F(x)")

for ( u in 1:10){
  ecdfPlot(clim[,u], add=T, ecdf.col="grey55", ecdf.lwd=0.5)
  
}



# CDF independent

TUi<- vector("numeric", rep)
for (i in 1:rep){
  tui = (quantile(normal[,1], probs=runif(1,0,1))*wt_us[,1]+ quantile(normal[,2], probs=runif(1,0,1))*wt_us[,2] + quantile(normal[,3], probs=runif(1,0,1))*wt_us[,3] 
        + quantile(normal[,4], probs=runif(1,0,1))*wt_us[,4] + quantile(normal[,5], probs=runif(1,0,1))*wt_us[,5] +  quantile(normal[,6], probs=runif(1,0,1))*wt_us[,6]
        + quantile(normal[,7], probs=runif(1,0,1))*wt_us[,7] +  quantile(normal[,8], probs=runif(1,0,1))*wt_us[,8] +  quantile(normal[,9], probs=runif(1,0,1))*wt_us[,9]
        + quantile(normal[,10], probs=runif(1,0,1))*wt_us[,10])
  
  TUi[i] <- tui 
}



#### 1991-2012

t29_us<- t29_us[26:47,]

u_usa_t29 <- pobs(t29_us[26:47,2:11])
RVM = RVineStructureSelect(u_usa_t29,c(1:6),progress=TRUE)

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
simdata_usa_t29 <- RVineSim(rep, RVM)


# marginal distributions

# i=11
# plot(fitdist(t29_us[,i],"gamma",start=list(a=0,b=1),method="mme"))
# #plot(fitdist(t29_us[,i],"exp"))
# plot(fitdist(t29_us[,i],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(t29_us[,i],"norm"))



# plot(fitdist(t29_us[,i],"gamma",start=list(a=0,b=1),method="mme"))
# #plot(fitdist(t29_us[,i],"exp"))
# plot(fitdist(t29_us[,i],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(t29_us[,i],"norm"))
# 
# for ( i in 2:11){
#                   a <- fitdist(t29_us[26:47,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#                   b <- fitdist(t29_us[26:47,i],"norm")
#                   c <- fitdist(t29_us[26:47,i],"gamma",start=list(a=0,b=1),method="mme")
#                   print(summary(a)$aic)
#                   print(summary(b)$aic)
#                   print(summary(c)$aic)
#                   print(summary(a)$bic)
#                   print(summary(b)$bic)
#                   print(summary(c)$bic)}

#gamma: 8  norm: 10,11  gumbel: 2,3,4,5,6,7,9


#c 

      x= c(9,10)
      normal <- matrix(nrow=rep, ncol=10)
      for ( i in x){
        Normal<-fitdist(t29_us[,i+1],"norm")
        normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      }
      y=c(1,2,3,4,5,6,8)
      for ( i in y){
        Gumbel<-fitdist(t29_us[,i+1],"gumbel", start=list(a=0,b=1),optim.method="Nelder-Mead")
        normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      }
      r=c(7)
      for ( i in r){
        Normal<-fitdist(t29_us[,i+1],"gamma", method="mme")
        normal[,i] <- rgamma(rep,Normal$estimate[1], Normal$estimate[2])
      }
      colnames(normal) <- colnames(t29_us[,2:11])



# CDF
TU <- vector("numeric", rep)
for (i in 1:rep){
  tu = (quantile(normal[,1], probs=simdata_usa_t29[i,1])*wt_us[,1]+ quantile(normal[,2], probs=simdata_usa_t29[i,2])*wt_us[,2] + quantile(normal[,3], probs=simdata_usa_t29[i,3])*wt_us[,3] 
        + quantile(normal[,4], probs=simdata_usa_t29[i,4])*wt_us[,4]+ quantile(normal[,5], probs=simdata_usa_t29[i,5])*wt_us[,5]+  quantile(normal[,6], probs=simdata_usa_t29[i,6])*wt_us[,6]
        + quantile(normal[,7], probs=simdata_usa_t29[i,7])*wt_us[,7]+  quantile(normal[,8], probs=simdata_usa_t29[i,8])*wt_us[,8]+  quantile(normal[,9], probs=simdata_usa_t29[i,9])*wt_us[,9]
        + quantile(normal[,10], probs=simdata_usa_t29[i,10])*wt_us[,10] )
  
  TU[i] <- tu 
}
return(TU)
}

clim <- as.data.frame(clim)
write.table(clim, "USA_maize_t29_simulations_github_period2.csv", sep=",", row.names=F)

# ecdf

# ecdfPlot(clim[,1], main= "Days above 29°C during growing season USA", xlab="Days above 29°C during growing season USA", ylab="F(x)")
# 
# for ( u in 1:10){
#   ecdfPlot(clim[,u], add=T, ecdf.col="grey55", ecdf.lwd=0.5)
#   
# }


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_maize <- read.table("maize_threshold25_version3_github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("USA_maize_t29_simulations_github_period2.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main="Days above 29°C in the USA May-Nov", xlab="Number of days", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.00595
# period 2: 0.00785


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_maize [,7], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.006" , "SE2 = 0.008", c(paste("threshold = ", round(thres_maize[1,7], digits =2), sep=""))))



# CDF independent
TUi <- vector("numeric", rep)
for (i in 1:rep){
  tui = (quantile(normal[,1], probs=runif(1,0,1))*wt_us[,1]+ quantile(normal[,2], probs=runif(1,0,1))*wt_us[,2] + quantile(normal[,3], probs=runif(1,0,1))*wt_us[,3] 
         + quantile(normal[,4], probs=runif(1,0,1))*wt_us[,4] + quantile(normal[,5], probs=runif(1,0,1))*wt_us[,5] +  quantile(normal[,6], probs=runif(1,0,1))*wt_us[,6]
         + quantile(normal[,7], probs=runif(1,0,1))*wt_us[,7] +  quantile(normal[,8], probs=runif(1,0,1))*wt_us[,8] +  quantile(normal[,9], probs=runif(1,0,1))*wt_us[,9]
         + quantile(normal[,10], probs=runif(1,0,1))*wt_us[,10])
  
  TUi[i] <- tui 
}




#### India -------
# tmax
setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
tmax <- read.csv("India_maize_BB_tmax_Jun_Oct.csv", header=T, sep=",")
tmax<- tmax[-47,]

u_in_tmax <- pobs(tmax[1:24,])  #b
u_in_tmax <- pobs(tmax[25:46,]) #c
u_in_tmax <- pobs(tmax) #a

RVM = RVineStructureSelect(u_in_tmax,c(1:6),progress=TRUE)

tmax<- tmax[1:24,] #b
tmax<- tmax[25:46,] #c

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
simdata_in_tmax <- RVineSim(rep, RVM)


# marginal distributions

# 
# plot(fitdist(tmax[,7],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(tmax[,7],"norm"))


# for ( i in 1:7){
#                     a <- fitdist(tmax[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#                     b <- fitdist(tmax[,i],"norm")
#                     print(summary(a)$aic)
#                     print(summary(b)$aic)
#                     print(summary(a)$bic)
#                     print(summary(b)$bic)}

#norm 1,2,3,4,5,6,7 , gumbel:  

#a

    # normal <- matrix(nrow=rep, ncol=7)
    # x = c(1:7)
    # for ( i in x){
    #   Normal <-fitdist(tmax[,i],"norm")
    #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
    # }
    # colnames(normal) <- colnames(tmax)
    # 
    
    


#### 1967-1990



# plot(fitdist(tmax[,7],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(tmax[,7],"norm"))
# 
# 
# for ( i in 1:7){
#                       a <- fitdist(tmax[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#                       b <- fitdist(tmax[,i],"norm")
#                       print(summary(a)$aic)
#                       print(summary(b)$aic)
#                       print(summary(a)$bic)
#                       print(summary(b)$bic)}

#norm 1,2,3,4,6  gumbel: 5,7

#b

        normal <- matrix(nrow=rep, ncol=7)
        x = c(1,2,3,4,6)
        for ( i in x){
          Normal <-fitdist(tmax[,i],"norm")
          normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        }
        t= c(5,7)
        for (i in t){
        Gumbel<-fitdist(tmax[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
        }

        colnames(normal) <- colnames(tmax)

# 1991-2012



#plot(fitdist(tmax[,7],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
#plot(fitdist(tmax[,7],"norm"))
# 
# for ( i in 1:7){
#   a <- fitdist(tmax[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(tmax[,i],"norm")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(a)$bic)
#   print(summary(b)$bic)}

#norm:1,2,5,6,7  gumbel:3,4

#c 

        # normal <- matrix(nrow=rep, ncol=7)
        # x = c(1:2,5:7)
        # for ( i in x){
        #   Normal <-fitdist(tmax[,i],"norm")
        #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        # }
        # t = c(3,4)
        # for (i in t){
        #   Gumbel<-fitdist(tmax[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        #   normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
        # }
        # colnames(normal) <- colnames(tmax)


# # CDF
# SI <- vector("numeric", rep)
# for (i in 1:rep){
#   si = (quantile(normal[,1], probs=simdata_in_tmax[i,1])* wt_in[,1] + quantile(normal[,2], probs=simdata_in_tmax[i,2])* wt_in[,2] + quantile(normal[,3], probs=simdata_in_tmax[i,3])* wt_in[,3] 
#         + quantile(normal[,4], probs=simdata_in_tmax[i,4])* wt_in[,4] + quantile(normal[,5], probs=simdata_in_tmax[i,5])* wt_in[,5]+  quantile(normal[,6], probs=simdata_in_tmax[i,6])* wt_in[,6]
#         + quantile(normal[,7], probs=simdata_in_tmax[i,7])* wt_in[,7])
#   
#   SI[i] <- si 
# }

# CDF independent

SIi <- vector("numeric", rep)
for (i in 1:rep){
  sii = (quantile(normal[,1], probs=runif(1,0,1))* wt_in[,1]+ quantile(normal[,2], probs=runif(1,0,1))* wt_in[,2] + quantile(normal[,3], probs=runif(1,0,1)) * wt_in[,3]
         + quantile(normal[,4], probs=runif(1,0,1))* wt_in[,4]+ quantile(normal[,5], probs=runif(1,0,1))* wt_in[,5]+  quantile(normal[,6], probs=runif(1,0,1))* wt_in[,6]
         + quantile(normal[,7], probs=runif(1,0,1))* wt_in[,7])
  
  SIi[i] <- sii 
}

return(SIi)
}

clim <- as.data.frame(clim)
write.table(clim, "India_maize_tmax_simulations_github_period1_indep.csv", sep=",", row.names=F)

# ecdf
# 
# ecdfPlot(clim[,1], main=" Max temperature in India", xlab="Max temperature", ylab="F(x)")
# 
# for ( u in 1:10){
#   ecdfPlot(clim[,u], add=T, ecdf.col="grey55", ecdf.lwd=0.5)
#   
# }

# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_maize <- read.table("maize_threshold25_version3_github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("India_maize_tmax_simulations_github_period2.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main=" Maximum temperature in India Jun-Oct", xlab="Maximum temperature (in °C) ", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3 
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=10)

for ( i in 1:10){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.0002933
# period 2: 0.0001766


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_maize [,6], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.0003" , "SE2 = 0.0002", c(paste("threshold = ", round(thres_maize[1,6], digits =2), sep=""))))





ecdfPlot(SI, main=" Max temperature in India", xlab="Max temperature", ylab="F(x)")
ecdfPlot(SI, add=T, ecdf.col="grey55")




# CDF independent

SIi <- vector("numeric", rep)
for (i in 1:rep){
  sii = (quantile(normal[,1], probs=runif(1,0,1))* wt_in[,1]+ quantile(normal[,2], probs=runif(1,0,1))* wt_in[,2] + quantile(normal[,3], probs=runif(1,0,1)) * wt_in[,3]
        + quantile(normal[,4], probs=runif(1,0,1))* wt_in[,4]+ quantile(normal[,5], probs=runif(1,0,1))* wt_in[,5]+  quantile(normal[,6], probs=runif(1,0,1))* wt_in[,6]
        + quantile(normal[,7], probs=runif(1,0,1))* wt_in[,7])
  
  SIi[i] <- sii 
}




######### China -------

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
cum_prec <- read.csv("China_maize_BB_cum_prec_Jun_Aug.csv", header=T, sep=",")
cum_prec<- cum_prec[,-1]

## china precipitation -----

u_ch_prec <- pobs(cum_prec[2:25,])
u_ch_prec <- pobs(cum_prec[26:47,])
u_ch_prec <- pobs(cum_prec[2:47,])

RVM = RVineStructureSelect(u_ch_prec,c(1:6),progress=TRUE)

cum_prec <- cum_prec[26:47,] #c
cum_prec <- cum_prec[2:25,] #b

clim <- foreach (z=1:10,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
simdata_ch_prec <- RVineSim(rep, RVM)


# marginal distributions


# plot(fitdist(cum_prec[,7],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[,7],"norm"))
# 
# 
# # 
# for ( i in 1:7){
#   a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(cum_prec[,i],"norm")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(a)$bic)
#   print(summary(b)$bic)}
# 
# #norm for 1,2,3,6,7  gumbel: 4,5

#a 
# 
#         normal <- matrix(nrow=rep, ncol=7)
#         x = c(4,5)
#         for ( i in x){
#           Gumbel<-fitdist(cum_prec[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
#           normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
#         }
#         t=c(1,2,3,6,7)
#         for (i in t){
#         Normal<-fitdist(cum_prec[,i],"norm")
#         normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
#         }
#         colnames(normal) <- colnames(cum_prec[,1:7])
        

#### 1967-1990


# plot(fitdist(cum_prec[,7],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[,7],"norm"))
# 
# for ( i in 1:7){
#   a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(cum_prec[,i],"norm")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(a)$bic)
#   print(summary(b)$bic)}


#gumbel: 1,4,5,6 , normal: 2,3,7

#b

      # normal <- matrix(nrow=rep, ncol=7)
      # x = c(1,4,5,6)
      # for ( i in x){
      #   Gumbel<-fitdist(cum_prec[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
      #   normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      # }
      # t=c(2,3,7)
      # for (i in t){
      #   Normal<-fitdist(cum_prec[,i],"norm")
      #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # colnames(normal) <- colnames(cum_prec[,1:7])


# 1991-2012



# plot(fitdist(cum_prec[,7],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[,7],"norm"))
# 
for ( i in 1:7){
    a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
    b <- fitdist(cum_prec[,i],"norm")
    print(summary(a)$aic)
    print(summary(b)$aic)
    print(summary(a)$bic)
    print(summary(b)$bic)}

#gumbel: 2,7 , normal: 1,3,4,5,6

#c

    normal <- matrix(nrow=rep, ncol=7)
    x = c(2,7)
    for ( i in x){
      Gumbel<-fitdist(cum_prec[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
      normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
    }

    l = c(1,3:6)
    for ( i in l){
    Normal<-fitdist(cum_prec[,i],"norm")
    normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])}

    colnames(normal) <- colnames(cum_prec[,1:7])



# # CDF
PC <- vector("numeric", rep)
for (i in 1:rep){
  pc = (quantile(normal[,1], probs=simdata_ch_prec[i,1])* wt_ch[,1] + quantile(normal[,2], probs=simdata_ch_prec[i,2])* wt_ch[,2] + quantile(normal[,3], probs=simdata_ch_prec[i,3])* wt_ch[,3]
        + quantile(normal[,4], probs=simdata_ch_prec[i,4])* wt_ch[,4]+ quantile(normal[,5], probs=simdata_ch_prec[i,5])* wt_ch[,5]
        + quantile(normal[,6], probs=simdata_ch_prec[i,6])* wt_ch[,6]+ quantile(normal[,7], probs=simdata_ch_prec[i,7])* wt_ch[,7]
  )

  PC[i] <- pc
}


# CDF independent
# PCi <- vector("numeric", rep)
# for (i in 1:rep){
#   pci = (quantile(normal[,1], probs=runif(1,0,1))* wt_ch[,1]+ quantile(normal[,2], probs=runif(1,0,1)) + quantile(normal[,3], probs=runif(1,0,1))* wt_ch[,3] 
#          + quantile(normal[,4], probs=runif(1,0,1))* wt_ch[,4]+ quantile(normal[,5], probs=runif(1,0,1))* wt_ch[,5]
#          + quantile(normal[,6], probs=runif(1,0,1))* wt_ch[,6]+ quantile(normal[,7], probs=runif(1,0,1))* wt_ch[,7]
#   )
#   
#   PCi[i] <- pci
# }

return(PC)
}

clim <- as.data.frame(clim)
write.table(clim, "China_maize_cum_prec_simulations_github_period1_indep.csv", sep=",", row.names=F)

# ecdf
# 
# ecdfPlot(clim[,1], main=" Cumulative growing season precipitation China", xlab="Precipitation in mm", ylab="F(x)")
# 
# for ( u in 1:10){
#   ecdfPlot(clim[,u], add=T, ecdf.col="grey55", ecdf.lwd=0.5)
#   
# }


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_maize <- read.table("maize_threshold25_version3_github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("China_maize_cum_prec_simulations_github_period2.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main=" Cumulative precipitation in China Jun-Aug", xlab="Precipitation (in mm)", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.024436
# period 2: 0.0269


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_maize [,2], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.024" , "SE2 = 0.027", c(paste("threshold = ", round(thres_maize[1,2], digits =2), sep=""))))



ecdfPlot(PC, main=" Cumulative growing season precipitation China", xlab="Precipitation in mm", ylab="F(x)")

ecdfPlot(PC, add=T, ecdf.col="grey55")

legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))



# CDF independent
PCi <- vector("numeric", rep)
for (i in 1:rep){
  pci = (quantile(normal[,1], probs=runif(1,0,1))* wt_ch[,1]+ quantile(normal[,2], probs=runif(1,0,1)) + quantile(normal[,3], probs=runif(1,0,1))* wt_ch[,3] 
        + quantile(normal[,4], probs=runif(1,0,1))* wt_ch[,4]+ quantile(normal[,5], probs=runif(1,0,1))* wt_ch[,5]
        + quantile(normal[,6], probs=runif(1,0,1))* wt_ch[,6]+ quantile(normal[,7], probs=runif(1,0,1))* wt_ch[,7]
  )
  
  PCi[i] <- pci
}



### tmax version 2 ----------

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
tmax <- read.csv("China_maize_BB_tmax_May_Sep.csv", header=T, sep=",")
tmax<- tmax[-1,-1]

u_ch_tmax <- pobs(tmax[1:24,])
u_ch_tmax <- pobs(tmax[25:46,])
u_ch_tmax <- pobs(tmax)

RVM = RVineStructureSelect(u_ch_tmax,c(1:6),progress=TRUE)


tmax <- tmax[25:46,] #c
tmax <- tmax[1:24,]  #b

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
simdata_ch_tmax <- RVineSim(rep, RVM)


# marginal distributions

# i=1
# plot(fitdist(tmax[,i],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(tmax[,i],"norm"))
# plot(fitdist(tmax[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# for ( i in 1:7){
#                   a <- fitdist(tmax[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#                   b <- fitdist(tmax[,i],"norm")
#                   c <- fitdist(tmax[,i],"gamma",start=list(a=0,b=1),method="mme")
#                   print(summary(a)$aic)
#                   print(summary(b)$aic)
#                   print(summary(c)$aic)
#                   print(summary(a)$bic)
#                   print(summary(b)$bic)
#                   print(summary(c)$bic)}

#norm for 2,6   gumbel:7  gamma: 1,3,4,5

#a

      # normal <- matrix(nrow=rep, ncol=7)
      # 
      # for ( i in 7){
      #   Gumbel<-fitdist(tmax[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
      #   normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      # }
      # 
      # x<- c(2,6)
      # for (i in x){
      #   Normal<-fitdist(tmax[,i],"norm")
      #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # t=c(1,3,4,5)
      # for ( i in t){
      #   Gamma<-fitdist(tmax[,i],"gamma", method="mme")
      #   normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      # }
      # colnames(normal) <- colnames(tmax)


#### 1967-1990

# i=7
# plot(fitdist(tmax[,i],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(tmax[,i],"norm"))
# plot(fitdist(tmax[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# for ( i in 1:7){
#                     a <- fitdist(tmax[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#                     b <- fitdist(tmax[,i],"norm")
#                     c <- fitdist(tmax[,i],"gamma",start=list(a=0,b=1),method="mme")
#                     print(summary(a)$aic)
#                     print(summary(b)$aic)
#                     print(summary(c)$aic)
#                     print(summary(a)$bic)
#                     print(summary(b)$bic)
#                     print(summary(c)$bic)}

#gumbel:4  , normal:2,5,6,7  gamma: 1,3

# b

        normal <- matrix(nrow=rep, ncol=7)

        for ( i in 4){
          Gumbel<-fitdist(tmax[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
          normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
        }
        x = c(2,5:7)
        for (i in x){
          Normal<-fitdist(tmax[,i],"norm")
          normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        }

        t=c(1,3)
        for ( i in t){
        Gamma<-fitdist(tmax[,i],"gamma", method="mme")
        normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
        }

        colnames(normal) <- colnames(tmax)


# 1991-2012


# i=7
# 
# plot(fitdist(tmax[,i],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(tmax[,i],"norm"))
# plot(fitdist(tmax[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# for ( i in 1:7){
#                     a <- fitdist(tmax[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#                     b <- fitdist(tmax[,i],"norm")
#                     c <- fitdist(tmax[,i],"gamma",start=list(a=0,b=1),method="mme")
#                     print(summary(a)$aic)
#                     print(summary(b)$aic)
#                     print(summary(c)$aic)
#                     print(summary(a)$bic)
#                     print(summary(b)$bic)
#                     print(summary(c)$bic)}

#gumbel: 1 , normal: 3,4  gamma: 2,5,6,7

 #c

        # normal <- matrix(nrow=rep, ncol=7)
        # x = c(1,2,7)
        # for ( i in 1){
        #   Gumbel<-fitdist(tmax[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        #   normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
        # }
        # for ( i in 3:5){
        # Normal<-fitdist(tmax[,i],"norm")
        # normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        # }
        # x = c(2,5:7)
        # for ( i in x){
        #   Gamma<-fitdist(tmax[,i],"gamma", method="mme")
        #   normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
        # }
        # colnames(normal) <- colnames(tmax)



# # CDF
# PC2 <- vector("numeric", rep)
# for (i in 1:rep){
#   pc = (quantile(normal[,1], probs=simdata_ch_tmax[i,1])* wt_ch[,1]+ quantile(normal[,2], probs=simdata_ch_tmax[i,2])* wt_ch[,2] + quantile(normal[,3], probs=simdata_ch_tmax[i,3])* wt_ch[,3] 
#         + quantile(normal[,4], probs=simdata_ch_tmax[i,4])* wt_ch[,4]+ quantile(normal[,5], probs=simdata_ch_tmax[i,5])* wt_ch[,5]
#         + quantile(normal[,6], probs=simdata_ch_tmax[i,6])* wt_ch[,6]+ quantile(normal[,7], probs=simdata_ch_tmax[i,7])* wt_ch[,7]
#   )
#   
#   PC2[i] <- pc 
# }

# CDF independent
PC2i <- vector("numeric", rep)
for (i in 1:rep){
  pci = (quantile(normal[,1], probs=runif(1,0,1))* wt_ch[,1]+ quantile(normal[,2], probs=runif(1,0,1))* wt_ch[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_ch[,3] 
         + quantile(normal[,4], probs=runif(1,0,1))* wt_ch[,4]+ quantile(normal[,5], probs=runif(1,0,1))* wt_ch[,5]
         + quantile(normal[,6], probs=runif(1,0,1))* wt_ch[,6]+ quantile(normal[,7], probs=runif(1,0,1))* wt_ch[,7]
  )
  
  PC2i[i] <- pci 
}


return(PC2i)
}

clim <- as.data.frame(clim)
write.table(clim, "China_maize_tmax_simulations_github_period1_indep.csv", sep=",", row.names=F)

# ecdf

# ecdfPlot(clim[,1], main=" Maxiumum temperature in °C in  China", xlab="Maximum temperature in °C ", ylab="F(x)")
# 
# for ( u in 1:10){
#   ecdfPlot(clim[,u], add=T, ecdf.col="grey55", ecdf.lwd=0.5)
#   
# }


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_maize <- read.table("maize_threshold25_version3_github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("China_maize_tmax_simulations_github_period2.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white",main=" Maxiumum temperature in  China May-Sep", xlab="Maximum temperature (in °C) ", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])

# period 1:0.000218
# period 2: 0.0002894

legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_maize [,1], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.0002" , "SE2 = 0.0003", c(paste("threshold = ", round(thres_maize[1,1], digits =2), sep=""))))



# ecdfPlot(PC2, main=" Maxiumum temperature in °C in  China", xlab="Temperature in °C ", ylab="F(x)")
# ecdfPlot(PC2, add=T, ecdf.col="grey55")



# CDF independent
PC2i <- vector("numeric", rep)
for (i in 1:rep){
  pci = (quantile(normal[,1], probs=runif(1,0,1))* wt_ch[,1]+ quantile(normal[,2], probs=runif(1,0,1))* wt_ch[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_ch[,3] 
        + quantile(normal[,4], probs=runif(1,0,1))* wt_ch[,4]+ quantile(normal[,5], probs=runif(1,0,1))* wt_ch[,5]
        + quantile(normal[,6], probs=runif(1,0,1))* wt_ch[,6]+ quantile(normal[,7], probs=runif(1,0,1))* wt_ch[,7]
  )
  
  PC2i[i] <- pci 
}



######### Brazil -------

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
### cum precipitation -------

cum_prec <- read.csv("Brazil_maize_BB_cum_prec_Nov_Feb.csv", header=T, sep=",")
cum_prec<- cum_prec[,-1]

u_br_prec <- pobs(cum_prec[1:24,])
u_br_prec <- pobs(cum_prec[25:46,])
u_br_prec <- pobs(cum_prec)

RVM = RVineStructureSelect(u_br_prec,c(1:6),progress=TRUE)


cum_prec <- cum_prec[25:46,] #c
cum_prec <- cum_prec[1:24,]  #b

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
simdata_br_prec <- RVineSim(rep, RVM)


# marginal distributions
# 
# plot(fitdist(cum_prec[,5],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[,5],"norm"))
# plot(fitdist(cum_prec[,5],"gamma",start=list(a=0,b=1),method="mme"))
# 
# for ( i in 1:5){
#                     a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#                     b <- fitdist(cum_prec[,i],"norm")
#                     c <- fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme")
#                     print(summary(a)$aic)
#                     print(summary(b)$aic)
#                     print(summary(c)$aic)
#                     print(summary(a)$bic)
#                     print(summary(b)$bic)
#                     print(summary(c)$bic)}

#norm 2,5   gumbel: 3   gamma: 1,4

#a

      # normal <- matrix(nrow=rep, ncol=5)
      # t = c(2,5)
      # for (i in t){
      #   Normal<-fitdist(cum_prec[,i],"norm")
      #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # x = c(3)
      # for ( i in x){
      #   Gumbel<-fitdist(cum_prec[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
      #   normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      # }
      # j=c(1,4)
      # for ( i in j){
      #   Gamma<-fitdist(cum_prec[,i],"gamma", method="mme")
      #   normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      # }
      # colnames(normal) <- colnames(cum_prec[,1:5])

####  1967-1990


# i=5
# plot(fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[,i],"norm"))
# plot(fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# for ( i in 1:5){
#   a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(cum_prec[,i],"norm")
#   c <- fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(c)$aic)
#   print(summary(a)$bic)
#   print(summary(b)$bic)
#   print(summary(c)$bic)}


#gamma: 2,3,4,5  gumbel: 1 

#b

      normal <- matrix(nrow=rep, ncol=5)
      j=c(2:5)
      for ( i in j){
        Gamma<-fitdist(cum_prec[,i],"gamma", method="mme")
        normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      }
      x = c(1)
      for ( i in x){
        Gumbel<-fitdist(cum_prec[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      }
      colnames(normal) <- colnames(cum_prec[,1:5])


# 1991-2012

# i=1
# plot(fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[,i],"norm"))
# plot(fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# for ( i in 1:5){
#   a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(cum_prec[,i],"norm")
#   c <- fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(c)$aic)
#   print(summary(a)$bic)
#   print(summary(b)$bic)
#   print(summary(c)$bic)}

#gumbel:3,4  , normal:2 , gamma: 1,5 

#c
# 
      # normal <- matrix(nrow=rep, ncol=5)
      # x = c(3,4)
      # for ( i in x){
      #   Gumbel<-fitdist(cum_prec[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
      #   normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      # }
      # Normal<-fitdist(cum_prec[,2],"norm")
      # normal[,2] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # j=c(1,5)
      # for ( i in j){
      #   Gamma<-fitdist(cum_prec[,i],"gamma", method="mme")
      #   normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      # }
      # colnames(normal) <- colnames(cum_prec[,1:5])



# # CDF
# PB <- vector("numeric", rep)
# for (i in 1:rep){
#   pb = (quantile(normal[,1], probs=simdata_br_prec[i,1])* wt_br[,1]+ quantile(normal[,2], probs=simdata_br_prec[i,2])* wt_br[,2] + quantile(normal[,3], probs=simdata_br_prec[i,3])* wt_br[,3] 
#         + quantile(normal[,4], probs=simdata_br_prec[i,4])* wt_br[,4]+ quantile(normal[,5], probs=simdata_br_prec[i,5])* wt_br[,5]
#         )
#   
#   PB[i] <- pb 
# }

# CDF independent
PBi <- vector("numeric", rep)
for (i in 1:rep){
  pbi = (quantile(normal[,1], probs=runif(1,0,1))* wt_br[,1]+ quantile(normal[,2], probs=runif(1,0,1)) * wt_br[,2]+ quantile(normal[,3], probs=runif(1,0,1))* wt_br[,3] 
         + quantile(normal[,4], probs=runif(1,0,1))* wt_br[,4]+ quantile(normal[,5], probs=runif(1,0,1))* wt_br[,5]
  )
  
  PBi[i] <- pbi 
}

return(PBi)
}

clim <- as.data.frame(clim)
write.table(clim, "Brazil_maize_cum_prec_simulations_github_period1_indep.csv", sep=",", row.names=F)

# # ecdf
# clim <-  read.csv("Brazil_maize_cum_prec_simulations_github_period1.csv", header=T, sep=",")
# 
# ecdfPlot(clim[,1], main=" Cumulative growing season precipitation Brazil", xlab="Precipitation in mm", ylab="F(x)", xlim=c(650,1150))
# 
# for ( u in 1:10){
#   ecdfPlot(clim[,u], add=T, ecdf.col="grey55", ecdf.lwd=0.5)
#   
# }


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_maize <- read.table("maize_threshold25_version3_github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("Brazil_maize_cum_prec_simulations_github_period2.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main=" Cumulative precipitation in Brazil Nov-Feb", xlab="Precipitation (in mm)", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.046831
# period 2: 0.04429


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_maize [,3], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.047" , "SE2 = 0.044", c(paste("threshold = ", round(thres_maize[1,3], digits =2), sep=""))))


# ecdfPlot(PB, main=" Cumulative growing season precipitation Brazil", xlab="Precipitation in mm", ylab="F(x)")
# ecdfPlot(PB, add=T, ecdf.col="grey55")
# legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))



# CDF independent
PBi <- vector("numeric", rep)
for (i in 1:rep){
  pbi = (quantile(normal[,1], probs=runif(1,0,1))* wt_br[,1]+ quantile(normal[,2], probs=runif(1,0,1)) * wt_br[,2]+ quantile(normal[,3], probs=runif(1,0,1))* wt_br[,3] 
        + quantile(normal[,4], probs=runif(1,0,1))* wt_br[,4]+ quantile(normal[,5], probs=runif(1,0,1))* wt_br[,5]
  )
  
  PBi[i] <- pbi 
}



######### Argentina -------

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")

### cum precipitation -------

cum_prec <- read.csv("Argentina_maize_BB_cum_prec_Nov_Jan.csv", header=T, sep=",")
cum_prec<- cum_prec[,-1]


u_ar_prec <- pobs(cum_prec[1:24,])
u_ar_prec <- pobs(cum_prec[25:46,])
u_ar_prec <- pobs(cum_prec)

RVM = RVineStructureSelect(u_ar_prec,c(1:6),progress=TRUE)

cum_prec <- cum_prec[25:46,] #c
cum_prec <- cum_prec[1:24,] #b

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
simdata_ar_prec <- RVineSim(rep, RVM)


# marginal distributions


# i=2
# plot(fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[,i],"norm"))
# plot(fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# for ( i in 1:4){
#     a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#     b <- fitdist(cum_prec[,i],"norm")
#     c <- fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme")
#     print(summary(a)$aic)
#     print(summary(b)$aic)
#     print(summary(c)$aic)
#     print(summary(a)$bic)
#     print(summary(b)$bic)
#     print(summary(c)$bic)}

#norm 1   gumbel:  gamma: 2,3,4

#a
# 
#       normal <- matrix(nrow=rep, ncol=4)
#       t = c(1)
#       for (i in t){
#         Normal<-fitdist(cum_prec[,i],"norm")
#         normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
#       }
#       
#       x=c(2:4)
#       for (i in x){
#         Gamma<-fitdist(cum_prec[,i],"gamma", method="mme")
#         normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
#       }
#       colnames(normal) <- colnames(cum_prec[,1:4])

####  1967-1990
      

# i=4
# plot(fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[,i],"norm"))
# plot(fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# for ( i in 1:4){
#     a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#     b <- fitdist(cum_prec[,i],"norm")
#     c <- fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme")
#     print(summary(a)$aic)
#     print(summary(b)$aic)
#     print(summary(c)$aic)
#     print(summary(a)$bic)
#     print(summary(b)$bic)
#     print(summary(c)$bic)}
  
#normal:2,3,4   gamma: 1,  

#b

      normal <- matrix(nrow=rep, ncol=4)
      x = c(2:4)
      for ( i in x){
      Normal<-fitdist(cum_prec[,i],"norm")
      normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      }
      Gamma<-fitdist(cum_prec[,1],"gamma", method="mme")
      normal[,1] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])

      colnames(normal) <- colnames(cum_prec[,1:4])


# 1991-2012


# i=4
# plot(fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[,i],"norm"))
# plot(fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# for ( i in 1:4){
#     a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#     b <- fitdist(cum_prec[,i],"norm")
#     c <- fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme")
#     print(summary(a)$aic)
#     print(summary(b)$aic)
#     print(summary(c)$aic)
#     print(summary(a)$bic)
#     print(summary(b)$bic)
#     print(summary(c)$bic)}

#gumbel:3  , normal:1  gamma:2,4

#c
# 
      # normal <- matrix(nrow=rep, ncol=4)
      # x = c(3)
      # for ( i in x){
      #   Gumbel<-fitdist(cum_prec[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
      #   normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      # }
      # Normal<-fitdist(cum_prec[,1],"norm")
      # normal[,1] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # 
      # Gamma<-fitdist(cum_prec[,2],"gamma", method="mme")
      # normal[,2] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      # Gamma<-fitdist(cum_prec[,4],"gamma", method="mme")
      # normal[,4] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      # 
      # colnames(normal) <- colnames(cum_prec[,1:4])



# # CDF
# PAr <- vector("numeric", rep)
# for (i in 1:rep){
#   par = (quantile(normal[,1], probs=simdata_ar_prec[i,1])* wt_ar[,1]+ quantile(normal[,2], probs=simdata_ar_prec[i,2])* wt_ar[,2] + quantile(normal[,3], probs=simdata_ar_prec[i,3])* wt_ar[,3] 
#         + quantile(normal[,4], probs=simdata_ar_prec[i,4])* wt_ar[,4]
#   )
#   
#   PAr[i] <- par 
# }

# CDF independent
PAri <- vector("numeric", rep)
for (i in 1:rep){
  pari = (quantile(normal[,1], probs=runif(1,0,1))* wt_ar[,1]+ quantile(normal[,2], probs=runif(1,0,1))* wt_ar[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_ar[,3] 
          + quantile(normal[,4], probs=runif(1,0,1))* wt_ar[,4]
  )
  
  PAri[i] <- pari 
}

return(PAri)
}

clim <- as.data.frame(clim)
write.table(clim, "Argentina_maize_cum_prec_simulations_github_period1_indep.csv", sep=",", row.names=F)

# ecdf

# ecdfPlot(clim[,1], main=" Cumulative growing season precipitation Argentina", xlab="Precipitation in mm", ylab="F(x)")
# 
# for ( u in 1:10){
#   ecdfPlot(clim[,u], add=T, ecdf.col="grey55", ecdf.lwd=0.5)
#   
# }


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_maize <- read.table("maize_threshold25_version3_github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("Argentina_maize_cum_prec_simulations_github_period2.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main=" Cumulative precipitation in Argentina Nov-Jan", xlab="Precipitation (in mm)", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.03324
# period 2: 0.033977


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_maize [,5], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.033" , "SE2 = 0.034", c(paste("threshold = ", round(thres_maize[1,5], digits =2), sep=""))))


ecdfPlot(PAr, main=" Cumulative growing season precipitation Argentina", xlab="Precipitation in mm", ylab="F(x)")
ecdfPlot(PAr, add=T, ecdf.col="grey55")




# CDF independent
PAri <- vector("numeric", rep)
for (i in 1:rep){
  pari = (quantile(normal[,1], probs=runif(1,0,1))* wt_ar[,1]+ quantile(normal[,2], probs=runif(1,0,1))* wt_ar[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_ar[,3] 
         + quantile(normal[,4], probs=runif(1,0,1))* wt_ar[,4]
  )
  
  PAri[i] <- pari 
}


### tmax version 2-----------

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
tmax <- read.csv("Argentina_maize_BB_tmax_Dec_Feb.csv", header=T, sep=",")
tmax<- tmax[,-1]
tmax<- tmax[,-5]

u_ar_tmax <- pobs(tmax[1:24,])
u_ar_tmax <- pobs(tmax[25:46,])
u_ar_tmax <- pobs(tmax)

RVM = RVineStructureSelect(u_ar_tmax,c(1:6),progress=TRUE)

tmax <- tmax[25:46,] #c
tmax <- tmax[1:24,] #b

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
simdata_ar_tmax <- RVineSim(rep, RVM)


# marginal distributions

# 
# i=4
# plot(fitdist(tmax[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(tmax[,i],"norm"))
# plot(fitdist(tmax[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# for ( i in 1:4){
#     a <- fitdist(tmax[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#     b <- fitdist(tmax[,i],"norm")
#     c <- fitdist(tmax[,i],"gamma",start=list(a=0,b=1),method="mme")
#     print(summary(a)$aic)
#     print(summary(b)$aic)
#     print(summary(c)$aic)
#     print(summary(a)$bic)
#     print(summary(b)$bic)
#     print(summary(c)$bic)}

#norm 1,  gumbel:3  gamma:2,4 

#a

      # normal <- matrix(nrow=rep, ncol=4)
      # t = c(1)
      # for (i in t){
      #   Normal<-fitdist(tmax[,i],"norm")
      #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # 
      # for (i in 3){
      # Gumbel<-fitdist(tmax[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
      # normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      # }
      # Gamma<-fitdist(tmax[,2],"gamma", method="mme")
      # normal[,2] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      # Gamma<-fitdist(tmax[,4],"gamma", method="mme")
      # normal[,4] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      # colnames(normal) <- colnames(tmax[,1:4])

####  1967-1990


# i=4
# plot(fitdist(tmax[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(tmax[,i],"norm"))
# plot(fitdist(tmax[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# 
# for ( i in 1:4){
#     a <- fitdist(tmax[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#     b <- fitdist(tmax[,i],"norm")
#     c <- fitdist(tmax[,i],"gamma",start=list(a=0,b=1),method="mme")
#     print(summary(a)$aic)
#     print(summary(b)$aic)
#     print(summary(c)$aic)
#     print(summary(a)$bic)
#     print(summary(b)$bic)
#     print(summary(c)$bic)}

#normal:   gumbel:2,3,4  gamma: 1 

#b

      normal <- matrix(nrow=rep, ncol=4)

      for ( i in 2:4){
      Gumbel<-fitdist(tmax[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
      normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      }

      Gamma<-fitdist(tmax[,1],"gamma", method="mme")
      normal[,1] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])

      colnames(normal) <- colnames(tmax[,1:4])


# 1991-2012


# i=4
# plot(fitdist(tmax[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(tmax[,i],"norm"))
# plot(fitdist(tmax[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# for ( i in 1:4){
#     a <- fitdist(tmax[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#     b <- fitdist(tmax[,i],"norm")
#     c <- fitdist(tmax[,i],"gamma",start=list(a=0,b=1),method="mme")
#     print(summary(a)$aic)
#     print(summary(b)$aic)
#     print(summary(c)$aic)
#     print(summary(a)$bic)
#     print(summary(b)$bic)
#     print(summary(c)$bic)}

#gumbel:  , normal:1,3,4  gamma: 2

#c

      # normal <- matrix(nrow=rep, ncol=4)
      # x = c(1,3,4)
      # for ( i in x){
      # Normal<-fitdist(tmax[,i],"norm")
      # normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # Gamma<-fitdist(tmax[,2],"gamma", method="mme")
      # normal[,2] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      # 
      # colnames(normal) <- colnames(tmax[,1:4])


# # CDF
# PAr2 <- vector("numeric", rep)
# for (i in 1:rep){
#   par = (quantile(normal[,1], probs=simdata_ar_tmax[i,1])* wt_ar[,1]+ quantile(normal[,2], probs=simdata_ar_tmax[i,2])* wt_ar[,2] + quantile(normal[,3], probs=simdata_ar_tmax[i,3])* wt_ar[,3] 
#          + quantile(normal[,4], probs=simdata_ar_tmax[i,4])* wt_ar[,4]
#   )
#   
#   PAr2[i] <- par 
# }

# CDF independent
PAr2i <- vector("numeric", rep)
for (i in 1:rep){
  pari = (quantile(normal[,1], probs=runif(1,0,1))*wt_ar[,1]+ quantile(normal[,2], probs=runif(1,0,1))*wt_ar[,2] + quantile(normal[,3], probs=runif(1,0,1)) *wt_ar[,3]
          + quantile(normal[,4], probs=runif(1,0,1))*wt_ar[,4]
  )
  
  PAr2i[i] <- pari 
}

return(PAr2i)
}

clim <- as.data.frame(clim)
write.table(clim, "Argentina_maize_tmax_simulations_github_period1_indep.csv", sep=",", row.names=F)

# ecdf

# ecdfPlot(clim[,1], main=" Maximum temperature  in °C in Argentina", xlab=" Temperature in °C", ylab="F(x)")
# 
# for ( u in 1:10){
#   ecdfPlot(clim[,u], add=T, ecdf.col="grey55", ecdf.lwd=0.5)
#   
# }


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_maize <- read.table("maize_threshold25_version3_github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("Argentina_maize_tmax_simulations_github_period2.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main=" Maximum temperature in Argentina Dec-Feb", xlab=" Maximum temperature (in °C) ", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.0005861
# period 2: 0.00041


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_maize [,4], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.0006" , "SE2 = 0.0004", c(paste("threshold = ", round(thres_maize[1,4], digits =2), sep=""))))


ecdfPlot(PAr, main=" Maximum temperature  in °C in Argentina", xlab=" Temperature in °C", ylab="F(x)")
ecdfPlot(PAr, add=T, ecdf.col="grey55")



# CDF independent
PAr2i <- vector("numeric", rep)
for (i in 1:rep){
  pari = (quantile(normal[,1], probs=runif(1,0,1))+ quantile(normal[,2], probs=runif(1,0,1)) + quantile(normal[,3], probs=runif(1,0,1)) 
         + quantile(normal[,4], probs=runif(1,0,1))
  )/4
  
  PAr2i[i] <- pari 
}



#### Europe ----------


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")

### cum precipitation -------

cum_prec <- read.csv("EU_maize_BB_cum_prec_Mar_Aug.csv", header=T, sep=",")
cum_prec<- cum_prec[-1,]


u_eu_prec <- pobs(cum_prec[1:24,])
u_eu_prec <- pobs(cum_prec[25:46,])
u_eu_prec <- pobs(cum_prec)

RVM = RVineStructureSelect(u_eu_prec,c(1:6),progress=TRUE)

cum_prec <- cum_prec[25:46,] #c
cum_prec <- cum_prec[1:24,] #b

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
  rep=10000
  simdata_eu_prec <- RVineSim(rep, RVM)
  
  
  # marginal distributions
  
  
  # i=2
  # plot(fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
  # plot(fitdist(cum_prec[,i],"norm"))
  # plot(fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme"))
  # # 
  # for ( i in 1:6){
  #     a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #     b <- fitdist(cum_prec[,i],"norm")
  #     c <- fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme")
  #     print(summary(a)$aic)
  #     print(summary(b)$aic)
  #     print(summary(c)$aic)
  #     print(summary(a)$bic)
  #     print(summary(b)$bic)
  #     print(summary(c)$bic)}

  #norm 2,3,4,6   gumbel:   gamma: 1,5
  
  #a
  # 
  #       normal <- matrix(nrow=rep, ncol=6)
  #       t = c(2:4,6)
  #       for (i in t){
  #         Normal<-fitdist(cum_prec[,i],"norm")
  #         normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
  #       }
  #       
  #       x=c(1,5)
  #       for (i in x){
  #         Gamma<-fitdist(cum_prec[,i],"gamma", method="mme")
  #         normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
  #       }
  #       colnames(normal) <- colnames(cum_prec[,1:6])
  
  ####  1967-1990
  
  
  # i=4
  # plot(fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
  # plot(fitdist(cum_prec[,i],"norm"))
  # plot(fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme"))
  # # 
  # for ( i in 1:6){
  #     a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #     b <- fitdist(cum_prec[,i],"norm")
  #     c <- fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme")
  #     print(summary(a)$aic)
  #     print(summary(b)$aic)
  #     print(summary(c)$aic)
  #     print(summary(a)$bic)
  #     print(summary(b)$bic)
  #     print(summary(c)$bic)}
  
  #normal:3,6   gamma: 4,  gumbel:1,2,5
  
  #b
  
        normal <- matrix(nrow=rep, ncol=6)
        x = c(3,6)
        for ( i in x){
          Normal<-fitdist(cum_prec[,i],"norm")
          normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        }
        Gamma<-fitdist(cum_prec[,4],"gamma", method="mme")
        normal[,4] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])

        x=c(1,2,5)
        for (i in x){
        Gumbel<-fitdist(cum_prec[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
        }
        colnames(normal) <- colnames(cum_prec[,1:6])

  
  # 1991-2012
  
  
  # i=4
  # plot(fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
  # plot(fitdist(cum_prec[,i],"norm"))
  # plot(fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme"))
  # 
  # for ( i in 1:6){
  #     a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #     b <- fitdist(cum_prec[,i],"norm")
  #     c <- fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme")
  #     print(summary(a)$aic)
  #     print(summary(b)$aic)
  #     print(summary(c)$aic)
  #     print(summary(a)$bic)
  #     print(summary(b)$bic)
  #     print(summary(c)$bic)}
  
  #gumbel:  , normal:2,3,4,6  gamma:1,5
  
  #c
    
      # normal <- matrix(nrow=rep, ncol=6)
      # x = c(2:4,6)
      # for ( i in x){
      # Normal<-fitdist(cum_prec[,i],"norm")
      # normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # 
      # Gamma<-fitdist(cum_prec[,1],"gamma", method="mme")
      # normal[,1] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      # Gamma<-fitdist(cum_prec[,5],"gamma", method="mme")
      # normal[,5] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      # 
      # colnames(normal) <- colnames(cum_prec[,1:6])

  
  
  # # CDF
  # EUp <- vector("numeric", rep)
  # for (i in 1:rep){
  #   eup = (quantile(normal[,1], probs=simdata_eu_prec[i,1])* wt_eu[,1]+ quantile(normal[,2], probs=simdata_eu_prec[i,2])* wt_eu[,2] + quantile(normal[,3], probs=simdata_eu_prec[i,3])* wt_eu[,3] 
  #          + quantile(normal[,4], probs=simdata_eu_prec[i,4])* wt_eu[,4] + quantile(normal[,5], probs=simdata_eu_prec[i,5]) * wt_eu[,5]
  #                     + quantile(normal[,6], probs=simdata_eu_prec[i,6])* wt_eu[,6]
  #   )
  #   
  #   EUp[i] <- eup 
  # }
  
  
  # CDF
  EUpi <- vector("numeric", rep)
  for (i in 1:rep){
    eupi = (quantile(normal[,1], probs=runif(1,0,1))* wt_eu[,1]+ quantile(normal[,2], probs=runif(1,0,1))* wt_eu[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_eu[,3] 
           + quantile(normal[,4], probs=runif(1,0,1))* wt_eu[,4] + quantile(normal[,5], probs=runif(1,0,1)) * wt_eu[,5]
           + quantile(normal[,6], probs=runif(1,0,1))* wt_eu[,6]
    )
    
    EUpi[i] <- eupi 
  }
  
  return(EUpi)
}

clim <- as.data.frame(clim)
write.table(clim, "Europe_maize_cum_prec_simulations_github_period1_indep.csv", sep=",", row.names=F)

# ecdf

# ecdfPlot(clim[,1], main=" Cumulative precipitation Europe", xlab="Precipitation in mm", ylab="F(x)")
# 
# for ( u in 1:10){
#   ecdfPlot(clim[,u], add=T, ecdf.col="grey55", ecdf.lwd=0.5)
#   
# }


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_maize <- read.table("maize_threshold25_version3_github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("Europe_maize_cum_prec_simulations_github_period2.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main="Cumulative precipitation Europe Mar-Aug ", xlab="Precipitation (in mm)", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.0270
# period 2: 0.03115


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_maize [,9], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.027" , "SE2 = 0.031", c(paste("threshold = ", round(thres_maize[1,9], digits =2), sep=""))))


### tmean Europe -----------

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
tmean <- read.csv("EU_maize_BB_tmean_May_Aug.csv", header=T, sep=",")
tmean<- tmean[-1,]

u_ar_tmean <- pobs(tmean[1:24,])
u_ar_tmean <- pobs(tmean[25:46,])
u_ar_tmean <- pobs(tmean)

RVM = RVineStructureSelect(u_ar_tmean,c(1:6),progress=TRUE)

tmean <- tmean[25:46,] #c
tmean <- tmean[1:24,] #b

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
  rep=10000
  simdata_eu_tmean <- RVineSim(rep, RVM)
  
  
  # marginal distributions
  
  # 
  # i=4
  # plot(fitdist(tmean[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
  # plot(fitdist(tmean[,i],"norm"))
  # plot(fitdist(tmean[,i],"gamma",start=list(a=0,b=1),method="mme"))
  # 
  # 
  # for ( i in 1:6){
  #     a <- fitdist(tmean[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #     b <- fitdist(tmean[,i],"norm")
  #     c <- fitdist(tmean[,i],"gamma",start=list(a=0,b=1),method="mme")
  #     print(summary(a)$aic)
  #     print(summary(b)$aic)
  #     print(summary(c)$aic)
  #     print(summary(a)$bic)
  #     print(summary(b)$bic)
  #     print(summary(c)$bic)}

  #norm 3,6  gumbel:  gamma:1,2,4,5
  
  #a
      # 
      # normal <- matrix(nrow=rep, ncol=6)
      # t = c(3,6)
      # for (i in t){
      #   Normal<-fitdist(tmean[,i],"norm")
      #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # x=c(1,2,4,5)
      # for (i in x){
      # Gamma<-fitdist(tmean[,i],"gamma", method="mme")
      # normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      # }
  
  # colnames(normal) <- colnames(tmean[,1:4])
  
  ####  1967-1990
  
  
  # i=4
  # plot(fitdist(tmean[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
  # plot(fitdist(tmean[,i],"norm"))
  # plot(fitdist(tmean[,i],"gamma",start=list(a=0,b=1),method="mme"))
  # 
  # 
  # for ( i in 1:6){
  #     a <- fitdist(tmean[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #     b <- fitdist(tmean[,i],"norm")
  #     c <- fitdist(tmean[,i],"gamma",start=list(a=0,b=1),method="mme")
  #     print(summary(a)$aic)
  #     print(summary(b)$aic)
  #     print(summary(c)$aic)
  #     print(summary(a)$bic)
  #     print(summary(b)$bic)
  #     print(summary(c)$bic)}
  
  #normal:1:4   gumbel:6  gamma: 5 
  
  #b

        normal <- matrix(nrow=rep, ncol=6)
        t = c(1:4)
        for (i in t){
          Normal<-fitdist(tmean[,i],"norm")
          normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        }
        for ( i in 6){
          Gumbel<-fitdist(tmean[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
          normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
        }

        Gamma<-fitdist(tmean[,5],"gamma", method="mme")
        normal[,5] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])

        colnames(normal) <- colnames(tmean[,1:6])
  
  
  # 1991-2012
  
  
  # i=4
  # plot(fitdist(tmean[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
  # plot(fitdist(tmean[,i],"norm"))
  # plot(fitdist(tmean[,i],"gamma",start=list(a=0,b=1),method="mme"))
  # 
  # for ( i in 1:6){
  #     a <- fitdist(tmean[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #     b <- fitdist(tmean[,i],"norm")
  #     c <- fitdist(tmean[,i],"gamma",start=list(a=0,b=1),method="mme")
  #     print(summary(a)$aic)
  #     print(summary(b)$aic)
  #     print(summary(c)$aic)
  #     print(summary(a)$bic)
  #     print(summary(b)$bic)
  #     print(summary(c)$bic)}
  
  #gumbel:1,4,6  , normal:3  gamma: 2,5
  
  #c
  
        # normal <- matrix(nrow=rep, ncol=6)
        # t = c(3)
        # for (i in t){
        #   Normal<-fitdist(tmean[,i],"norm")
        #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        # }
        # x=c(1,4,6)
        # for ( i in x){
        #   Gumbel<-fitdist(tmean[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        #   normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
        # }
        # 
        # Gamma<-fitdist(tmean[,2],"gamma", method="mme")
        # normal[,2] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
        # Gamma<-fitdist(tmean[,5],"gamma", method="mme")
        # normal[,5] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
        # colnames(normal) <- colnames(tmean[,1:6])


  
  # # CDF
  # EUT <- vector("numeric", rep)
  # for (i in 1:rep){
  #   eut = (quantile(normal[,1], probs=simdata_eu_tmean[i,1])* wt_eu[,1]+ quantile(normal[,2], probs=simdata_eu_tmean[i,2])* wt_eu[,2] + quantile(normal[,3], probs=simdata_eu_tmean[i,3])* wt_eu[,3] 
  #          + quantile(normal[,4], probs=simdata_eu_tmean[i,4])* wt_eu[,4] + quantile(normal[,5], probs=simdata_eu_tmean[i,5])* wt_eu[,5] 
  #          + quantile(normal[,6], probs=simdata_eu_tmean[i,6])* wt_eu[,6]
  #   )
  #   
  #   EUT[i] <- eut 
  # }
  
        
  # CDF
  EUTi <- vector("numeric", rep)
  for (i in 1:rep){
    euti = (quantile(normal[,1], probs=runif(1,0,1))* wt_eu[,1]+ quantile(normal[,2], probs=runif(1,0,1))* wt_eu[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_eu[,3] 
           + quantile(normal[,4], probs=runif(1,0,1))* wt_eu[,4] + quantile(normal[,5], probs=runif(1,0,1))* wt_eu[,5] 
           + quantile(normal[,6], probs=runif(1,0,1))* wt_eu[,6]
    )
    
    EUTi[i] <- euti 
  }
  
  return(EUTi)
}

clim <- as.data.frame(clim)
write.table(clim, "Europe_maize_tmean_simulations_github_period1_indep.csv", sep=",", row.names=F)

# # ecdf
# 
# ecdfPlot(clim[,1], main=" Mean temperature  in °C in Europe", xlab=" Temperature in °C", ylab="F(x)")
# 
# for ( u in 1:10){
#   ecdfPlot(clim[,u], add=T, ecdf.col="grey55", ecdf.lwd=0.5)
#   
# }


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_maize <- read.table("maize_threshold25_version3_github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("Europe_maize_tmean_simulations_github_period1.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main=" Temperature in Europe May-Aug", xlab="Temperature (in °C) ", ylab="F(x)", xlim=c(16,21))

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.000318
# period 2: 0.0004044


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_maize [,8], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.0003" , "SE2 = 0.0004", c(paste("threshold = ", round(thres_maize[1,8], digits =2), sep=""))))




####


maize_global <- cbind(PC,PB, PAr, SI, TU)
write.table(maize_global, "maize_global.csv", sep=",", row.names=F)
clim <- read.table("maize_global.csv", sep=",", header=T)
PC <- clim[,1]
PB <- clim[,2]
PAr <- clim[,3]
SI <- clim[,4]
TU <- clim[,5]


######## maize  all ---------
## ## ##      ##       ##       ##        ##       ##
setwd("H:/DPhil/breadbaskets/Data collection")

# version 3: both indicators for USA and Australia
var <- read.csv("maize_climate_all_bb_version_3_github_weighted.csv", header=T, sep=",")
chart.Correlation (var[,2:10], method="kendall", pch=20, smooth=F,lines=F, main=" maize correlation structures", cex.main=0.9)


# extracting correlations between indicators of teh same BB
u_var1 <- pobs(var[,2:3])
u_var2 <- pobs(var[,5:6])
u_var3 <- pobs(var[,9:10])

a <- BiCopSelect(u_var1[,1], u_var1[,2], familyset=c(1:6))
b <- BiCopSelect(u_var2[,1], u_var2[,2], familyset=c(1:6))
c <- BiCopSelect(u_var3[,1], u_var3[,2], familyset=c(1:6))


simdata_var1 <- BiCopSim(rep, a$family, a$par)
simdata_var2 <- BiCopSim(rep, b$family, b$par)
simdata_var3 <- BiCopSim(rep, c$family, c$par)



####### risks of simultaneous negative climate conditions ###
## version 3 -----
setwd("H:/DPhil/breadbaskets/Data collection")
t25 <- read.csv("maize_threshold25_version3_github_weighted.csv", header=T, sep=",")

# uncertainty test:
t25 <- read.csv("maize_threshold25_prediction_0.9_version3_github_weighted.csv", header=T, sep=",")
t25l<- t25[2,]
t25u<- t25[3,]


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
PAr2 <- read.csv("Argentina_maize_tmax_simulations_github_period2.csv", header=T, sep=",")
PAr <- read.csv("Argentina_maize_cum_prec_simulations_github_period2.csv", header=T, sep=",")
SI <- read.csv("India_maize_tmax_simulations_github_period2.csv", header=T, sep=",")
PC <- read.csv("China_maize_cum_prec_simulations_github_period2.csv", header=T, sep=",")
PC2 <- read.csv("China_maize_tmax_simulations_github_period2.csv", header=T, sep=",")
TU <- read.csv("USA_maize_t29_simulations_github_period2.csv", header=T, sep=",")
EUp <- read.csv("Europe_maize_cum_prec_simulations_github_period2.csv", header=T, sep=",")
EUT <- read.csv("Europe_maize_tmean_simulations_github_period2.csv", header=T, sep=",")
PB <- read.csv("Brazil_maize_cum_prec_simulations_github_period2.csv", header=T, sep=",")

# version 3
u_var <- pobs(var[,2:10])

RVM = RVineStructureSelect(u_var,c(1:6),progress=TRUE)


clim <- foreach (i=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula")) %dopar% {
  
  
rep=1000
simdata_var <- RVineSim(rep, RVM)


####### risks of simultaneous negative climate conditions ###

#setwd("H:/DPhil/breadbaskets/Data collection")


# version 3

ALL0 <- matrix(nrow=rep, ncol=9)
for (n in 1:rep){
  all0 = c(quantile(PC2[,i], probs=simdata_var[n,1]), quantile(PC[,i], probs=simdata_var[n,2]) , quantile(PB[,i], probs=simdata_var[n,3]),
           quantile(PAr2[,i], probs=simdata_var[n,4]),quantile(PAr[,i], probs=simdata_var[n,5]),
           quantile(SI[,i], probs=simdata_var[n,6]),quantile(TU[,i], probs=simdata_var[n,7]),
           quantile(EUT[,i], probs=simdata_var[n,8]),quantile(EUp[,i], probs=simdata_var[n,9]))


  ALL0[n,] <- all0
}

ALL0 <- as.data.frame(ALL0)


# # version 3 independent BBs but dependence between clim.var of same BB

# ALL0 <- matrix(nrow=rep, ncol=9)
# for (n in 1:rep){
#   all0 = c(quantile(PC2[,i], probs=simdata_var1[n,1]), quantile(PC[,i], probs=simdata_var1[n,2]) , quantile(PB[,i], probs=runif(1,0,1)), 
#            quantile(PAr2[,i], probs=simdata_var2[n,1]),quantile(PAr[,i], probs=simdata_var2[n,2]),
#            quantile(SI[,i], probs=runif(1,0,1)),quantile(TU[,i], probs=runif(1,0,1)),
#            quantile(EUT[,i], probs=simdata_var3[n,1]),quantile(EUp[,i], probs=simdata_var3[n,2]))
#   
#   
#   ALL0[n,] <- all0
# }
# 
# ALL0 <- as.data.frame(ALL0)




###
# portion of BB below 25% threshold


# version 3 t25 : negative climate if one of two indicators exceeds threshold

for (l in 1:rep) {
  ALL0$V10[l] <- sum((ifelse(ALL0$V1[l]>t25l[1] | ALL0$V2[l]<t25u[2] ,1,0))[1,],(ifelse(ALL0$V3[l]<t25u[3],1,0))[1,],
                    (ifelse(ALL0$V4[l]>t25l[4] | ALL0$V5[l]<t25u[5] ,1,0))[1,],(ifelse(ALL0$V6[l]>t25l[6],1,0))[1,],(ifelse(ALL0$V7[l]>t25l[7],1,0))[1,],
                    (ifelse(ALL0$V8[l]>t25l[8] | ALL0$V9[l]<t25u[9] ,1,0))[1,])
  
}

col <- c(colnames(var[2:10]), "BBs")
colnames(ALL0) <- col

return(ALL0[,10])
}

write.table(clim, "maize_all_version3_t25_period1_github_weighted_indep.csv", sep=",", row.names = F)
write.table(clim, "maize_all_version3_t25_highrisk_prediction_90_period2_github_weighted.csv", sep=",", row.names = F)



ALL0<- clim
setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
ALL0 <- read.csv("maize_all_version3_t25_period1_github_weighted.csv", sep=",", header=T)
ALL0 <- read.csv("maize_all_version3_t25_highrisk_prediction_90_period1_github_weighted.csv", sep=",", header=T)

# indep
ALL0 <- read.csv("maize_all_version3_t25_period1_github_weighted_indep.csv", sep=",", header=T)


ALL0_perc <- matrix(nrow=1000, ncol=7)
#period1
for (o in 1:1000){
  for (i in 0:6){
    ALL0_perc[o,i+1]<- sum(ALL0[,o]==i)/1000
  }}

m1 <- apply(ALL0_perc,2,mean)
s1 <- apply(ALL0_perc,2,sd)


ALL0 <- read.csv("maize_all_version3_t25_period2_github_weighted.csv", sep=",", header=T)
ALL0 <- read.csv("maize_all_version3_t25_highrisk_prediction_90_period2_github_weighted.csv", sep=",", header=T)

# indep
ALL0 <- read.csv("maize_all_version3_t25_period2_github_weighted_indep.csv", sep=",", header=T)


ALL0_perc <- matrix(nrow=1000, ncol=7)
#period2
for (o in 1:1000){
  for (i in 0:6){
    ALL0_perc[o,i+1]<- sum(ALL0[,o]==i)/1000
  }}

m2 <- apply(ALL0_perc,2,mean)
s2 <- apply(ALL0_perc,2,sd)

stats<- rbind(m1,s1,m2,s2)



write.table(stats, "maize_global_simultaneous_pdf_25_version_3_github_weighted_indep.csv", sep=",", row.names=c("mean_period1","sd_period1",  "mean_period2","sd_period2"))
write.table(stats, "maize_global_simultaneous_pdf_25_version_3_highrisk_prediction_90_github_weighted.csv", sep=",", #row.names=c("var" ,"mean_period1","sd_period1",  "mean_period2","sd_period2"),
            col.names=c(0:6))


# all dependencies and periods:
setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")


period1 <- read.csv("maize_all_version3_t25_period1_github_weighted.csv", sep=",", header=T)
period2 <- read.csv("maize_all_version3_t25_period2_github_weighted.csv", sep=",", header=T)

# threshold uncertainty analysis:
period1 <- read.csv("maize_all_version3_t25_highrisk_prediction_90_period1_github_weighted.csv", sep=",", header=T)
period2 <- read.csv("maize_all_version3_t25_highrisk_prediction_90_period2_github_weighted.csv", sep=",", header=T)



period1_mean <- apply(period1,2,sort)
period1_mean <- apply(period1_mean, 1, mean)
period2_mean <- apply(period2, 2, sort)
period2_mean <- apply(period2_mean, 1, mean)

period1i <- read.csv("maize_all_version3_t25_period1_github_weighted_indep.csv", sep=",", header=T)
period2i <- read.csv("maize_all_version3_t25_period2_github_weighted_indep.csv", sep=",", header=T)

period1i_mean <- apply(period1i,2,sort)
period1i_mean <- apply(period1i_mean, 1, mean)
period2i_mean <- apply(period2i, 2, sort)
period2i_mean <- apply(period2i_mean, 1, mean)


mean(period2_mean)- mean(period1_mean)

# Wilcoxon (or Mann-Whitney-Wilcoxon) test -> assumes normal distribution for large sample numbers
wilcox.test(period1_mean, period2_mean, alternative = "less")
wilcox.test(period1_mean, period2_mean)

# -> p=0; p<=0.05 or 0.001 means populations are nonidentical

# between dependencies:
# Wilcoxon (or Mann-Whitney-Wilcoxon) test -> assumes normal distribution
wilcox.test(period2_mean, period2i_mean)
wilcox.test(period1_mean, period1i_mean)

# between periods:
# Wilcoxon (or Mann-Whitney-Wilcoxon) test -> assumes normal distribution
wilcox.test(period1i_mean, period2i_mean, alternative = "greater")




## portion below 25% threshold  ---------

# change of risks between two periods


t25_prop_maize_bb <- matrix(ncol=9, nrow=1000)

for ( i in 1:1000){
  t25_prop_maize_bb[i,1] <- length( which( PC2[,i] >t25[1][1,] ) )/10000
  t25_prop_maize_bb[i,2] <- length( which( PC[,i]<t25[2][1,] ) )/10000
  t25_prop_maize_bb[i,3] <- length( which( PB[,i]<t25[3][1,] ) )/10000
  t25_prop_maize_bb[i,4] <- length( which( PAr2[,i]>t25[4][1,] ) )/10000
  t25_prop_maize_bb[i,5] <- length( which( PAr[,i]<t25[5][1,] ) )/10000
  t25_prop_maize_bb[i,6] <- length( which( SI[,i] >t25[6][1,] ) )/10000
  t25_prop_maize_bb[i,7] <- length( which( TU[,i]>t25[7][1,] ) )/10000
  t25_prop_maize_bb[i,8] <- length( which( EUT[,i] >t25[8][1,] ) )/10000
  t25_prop_maize_bb[i,9] <- length( which( EUp[,i] <t25[9][1,] ) )/10000
  
}

colnames(t25_prop_maize_bb) <- colnames(t25)
write.table(t25_prop_maize_bb, "proportion_maize_25_global_bb_period1_github.csv", sep=",", row.names=F)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
prop1_25 <- read.table("proportion_maize_25_global_bb_period1_github.csv", sep=",", header=T)
prop2_25 <- read.table("proportion_maize_25_global_bb_period2_github.csv", sep=",", header=T)

diff_25 <- prop2_25 - prop1_25
diff_25_stats <- matrix(ncol=9, nrow=2)
diff_25_stats[1,] <- apply(diff_25, 2, mean)
diff_25_stats[2,] <- apply(diff_25, 2, sd)
colnames(diff_25_stats) <- colnames(prop2_25)
write.table(diff_25_stats, "proportion_diff_stats_maize_25_global_bb_github.csv", sep=",", row.names=F)


################### SOYBEAN ##############################

setwd("H:/DPhil/breadbaskets/Data collection")
res <- read.table("resid_soybean.csv", sep=",", header=T)

t25_prop_soy <- matrix(nrow=2, ncol=36)
colnames(t25_prop_soy) <- colnames(res[,2:37])

setwd("H:/DPhil/breadbaskets/Data collection")
wt <- read.table("soybean_average_area_weight.csv", sep=",", header=T)
wt_us <- wt[,13:23]
wt_in <- wt[,10:12]
wt_ch <- wt[,1:9]
wt_br <- wt[,24:32]
wt_ar <- wt[,33:36]



### USA --------
setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
t30_us <- read.csv("USA_soybean_BB_t30_May_Sep.csv", header=T, sep=",")

## usa t30 -----
u_usa_t30 <- pobs(t30_us[,2:12])
RVM = RVineStructureSelect(u_usa_t30,c(1:6),progress=TRUE)

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
simdata_usa_t30 <- RVineSim(rep, RVM)


# marginal distributions

# i=12
# plot(fitdist(t30_us[,i],"gamma",start=list(a=0,b=1),method="mme"))
# #plot(fitdist(t29_us[,i],"exp"))
# plot(fitdist(t30_us[,i],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(t30_us[,i],"norm"))
# 
# for ( i in 2:12){
#                   a <- fitdist(t30_us[2:47,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#                   b <- fitdist(t30_us[2:47,i],"norm")
#                   c <- fitdist(t30_us[2:47,i],"gamma",start=list(a=0,b=1),method="mme")
#                   print(summary(a)$aic)
#                   print(summary(b)$aic)
#                   print(summary(c)$aic)
#                   print(summary(a)$bic)
#                   print(summary(b)$bic)
#                   print(summary(c)$bic)}

#gamma:3,4,10,11 , norm:5,6,8,9,12   gumbel: 2,7



#a
        
        x= c(5,6,8,9,12)
        normal <- matrix(nrow=rep, ncol=11)
        for ( i in x){
          Normal<-fitdist(t30_us[,i],"norm")
          normal[,i-1] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        }
        y=c(2,7)
        for ( i in y){
          Gumbel<-fitdist(t30_us[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
          normal[,i-1] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
        }
        r=c(3,4,10,11)
        for ( i in r){
          Normal<-fitdist(t30_us[,i],"gamma", method="mme")
          normal[,i-1] <- rgamma(rep,Normal$estimate[1], Normal$estimate[2])
        }

        colnames(normal) <- colnames(t30_us[,2:12])


# CDF
TU <- vector("numeric", rep)
for (i in 1:rep){
  tu = (quantile(normal[,1], probs=simdata_usa_t30[i,1])*wt_us[,1] + quantile(normal[,2], probs=simdata_usa_t30[i,2])*wt_us[,2] + quantile(normal[,3], probs=simdata_usa_t30[i,3])*wt_us[,3] 
        + quantile(normal[,4], probs=simdata_usa_t30[i,4])*wt_us[,4] + quantile(normal[,5], probs=simdata_usa_t30[i,5])*wt_us[,5] +  quantile(normal[,6], probs=simdata_usa_t30[i,6])*wt_us[,6]
        + quantile(normal[,7], probs=simdata_usa_t30[i,7])*wt_us[,7] +  quantile(normal[,8], probs=simdata_usa_t30[i,8])*wt_us[,8] +  quantile(normal[,9], probs=simdata_usa_t30[i,9])*wt_us[,9]
        + quantile(normal[,10], probs=simdata_usa_t30[i,10])*wt_us[,10] + quantile(normal[,11], probs=simdata_usa_t30[i,11])*wt_us[,11])
  
  TU[i] <- tu 
}
return(TU)
}

clim <- as.data.frame(clim)
write.table(clim, "USA_soybean_t30_simulations_github.csv", sep=",", row.names=F)

# days per month: divided through 5 months
ecdfPlot(TU/5, main=" Average number of days per months above 30°C during growing season USA", xlab="Days above 30°C", ylab="F(x)", cex.main=0.8)
ecdfPlot(TU, add=T, ecdf.col="grey55")

#### 1967-1990

t30_us<- t30_us[2:25,]
u_usa_t30 <- pobs(t30_us[,2:12])
RVM = RVineStructureSelect(u_usa_t30,c(1:6),progress=TRUE)

clim <- foreach (z=1:10,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
simdata_usa_t30 <- RVineSim(rep, RVM)


# marginal distributions
# 
# i=9
# plot(fitdist(t30_us[,i],"gamma",start=list(a=0,b=1),method="mme"))
# #plot(fitdist(t29_us[,i],"exp"))
# plot(fitdist(t30_us[,i],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(t30_us[,i],"norm"))
# 
# for ( i in 2:12){
#   a <- fitdist(t30_us[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(t30_us[,i],"norm")
#   c <- fitdist(t30_us[,i],"gamma",start=list(a=0,b=1),method="mme")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(c)$aic)
#   print(summary(a)$bic)
#   print(summary(b)$bic)
#   print(summary(c)$bic)}

#gamma: 4,5 norm:8,9,12   gumbel: 2,3,6,7,10,11


#b

      x= c(8,9,12)
      normal <- matrix(nrow=rep, ncol=11)
      for ( i in x){
        Normal<-fitdist(t30_us[,i],"norm")
        normal[,i-1] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      }
      y=c(2,3,6,7,10,11)
      for ( i in y){
        Gumbel<-fitdist(t30_us[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        normal[,i-1] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      }
      r=c(4,5)
      for ( i in r){
        Normal<-fitdist(t30_us[,i],"gamma", method="mme")
        normal[,i-1] <- rgamma(rep,Normal$estimate[1], Normal$estimate[2])
      }
      colnames(normal) <- colnames(t30_us[,2:12])


# CDF
TU <- vector("numeric", rep)
for (i in 1:rep){
  tu = (quantile(normal[,1], probs=simdata_usa_t30[i,1])*wt_us[,1] + quantile(normal[,2], probs=simdata_usa_t30[i,2])*wt_us[,2] + quantile(normal[,3], probs=simdata_usa_t30[i,3])*wt_us[,3] 
        + quantile(normal[,4], probs=simdata_usa_t30[i,4])*wt_us[,4] + quantile(normal[,5], probs=simdata_usa_t30[i,5])*wt_us[,5] +  quantile(normal[,6], probs=simdata_usa_t30[i,6])*wt_us[,6]
        + quantile(normal[,7], probs=simdata_usa_t30[i,7])*wt_us[,7] +  quantile(normal[,8], probs=simdata_usa_t30[i,8])*wt_us[,8] +  quantile(normal[,9], probs=simdata_usa_t30[i,9])*wt_us[,9]
        + quantile(normal[,10], probs=simdata_usa_t30[i,10])*wt_us[,10] + quantile(normal[,11], probs=simdata_usa_t30[i,11])*wt_us[,11])
  
  TU[i] <- tu 
}

return(TU)
}

clim <- as.data.frame(clim)
write.table(clim, "USA_soybean_t30_simulations_github_period1.csv", sep=",", row.names=F)

par(mfrow=c(1,1))
ecdfPlot(TU/5, main=" Average number of days per month above 30°C during growing season USA", xlab="Days above 30°C", ylab="F(x)", cex.main=0.8)


# CDF independent
TUi <- vector("numeric", rep)
for (i in 1:rep){
  tui = (quantile(normal[,1], probs=runif(1,0,1))*wt_us[,1]+ quantile(normal[,2], probs=runif(1,0,1))*wt_us[,2] + quantile(normal[,3], probs=runif(1,0,1))*wt_us[,3] 
        + quantile(normal[,4], probs=runif(1,0,1))*wt_us[,4]+ quantile(normal[,5], probs=runif(1,0,1))*wt_us[,5]+  quantile(normal[,6], probs=runif(1,0,1))*wt_us[,6]
        + quantile(normal[,7], probs=runif(1,0,1))*wt_us[,7]+  quantile(normal[,8], probs=runif(1,0,1))*wt_us[,8]+  quantile(normal[,9], probs=runif(1,0,1))*wt_us[,9]
        + quantile(normal[,10], probs=runif(1,0,1))*wt_us[,10]  + quantile(normal[,11], probs=runif(1,0,1))*wt_us[,11])
  
  TUi[i] <- tui 
}

#### 1991-2012

t30_us<- t30_us[26:47,]
u_usa_t30 <- pobs(t30_us[,2:12])
RVM = RVineStructureSelect(u_usa_t30,c(1:6),progress=TRUE)

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
simdata_usa_t30 <- RVineSim(rep, RVM)


# marginal distributions


# i=12
# plot(fitdist(t30_us[,i],"gamma",start=list(a=0,b=1),method="mme"))
# #plot(fitdist(t29_us[,i],"exp"))
# plot(fitdist(t30_us[,i],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(t30_us[,i],"norm"))
# 
# 
# for ( i in 2:12){
#   a <- fitdist(t30_us[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(t30_us[,i],"norm")
#   c <- fitdist(t30_us[,i],"gamma",start=list(a=0,b=1),method="mme")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(c)$aic)
#   print(summary(a)$bic)
#   print(summary(b)$bic)
#   print(summary(c)$bic)}

#gamma: 2,3,4,10,11  norm:5,6,7,8,9,12   gumbel:

#c 
      x= c(5,6,7,8,9, 12)
      normal <- matrix(nrow=rep, ncol=11)
      for ( i in x){
        Normal<-fitdist(t30_us[,i],"norm")
        normal[,i-1] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      }
      r=c(2,3,4,10,11)
      for ( i in r){
        Normal<-fitdist(t30_us[,i],"gamma", method="mme")
        normal[,i-1] <- rgamma(rep,Normal$estimate[1], Normal$estimate[2])
      }
      colnames(normal) <- colnames(t30_us[,2:12])


# CDF
TU <- vector("numeric", rep)
for (i in 1:rep){
  tu = tu = (quantile(normal[,1], probs=simdata_usa_t30[i,1])*wt_us[,1] + quantile(normal[,2], probs=simdata_usa_t30[i,2])*wt_us[,2] + quantile(normal[,3], probs=simdata_usa_t30[i,3])*wt_us[,3] 
             + quantile(normal[,4], probs=simdata_usa_t30[i,4])*wt_us[,4] + quantile(normal[,5], probs=simdata_usa_t30[i,5])*wt_us[,5] +  quantile(normal[,6], probs=simdata_usa_t30[i,6])*wt_us[,6]
             + quantile(normal[,7], probs=simdata_usa_t30[i,7])*wt_us[,7] +  quantile(normal[,8], probs=simdata_usa_t30[i,8])*wt_us[,8] +  quantile(normal[,9], probs=simdata_usa_t30[i,9])*wt_us[,9]
             + quantile(normal[,10], probs=simdata_usa_t30[i,10])*wt_us[,10] + quantile(normal[,11], probs=simdata_usa_t30[i,11])*wt_us[,11])
  
  TU[i] <- tu 
}
return(TU)
}

clim <- as.data.frame(clim)
write.table(clim, "USA_soybean_t30_simulations_github_period2.csv", sep=",", row.names=F)

ecdfPlot(TU/5, add=T, ecdf.col="grey55")
legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))


# CDF independent
TUi <- vector("numeric", rep)
for (i in 1:rep){
  tui = (quantile(normal[,1], probs=runif(1,0,1))*wt_us[,1]+ quantile(normal[,2], probs=runif(1,0,1))*wt_us[,2] + quantile(normal[,3], probs=runif(1,0,1))*wt_us[,3] 
         + quantile(normal[,4], probs=runif(1,0,1))*wt_us[,4]+ quantile(normal[,5], probs=runif(1,0,1))*wt_us[,5]+  quantile(normal[,6], probs=runif(1,0,1))*wt_us[,6]
         + quantile(normal[,7], probs=runif(1,0,1))*wt_us[,7]+  quantile(normal[,8], probs=runif(1,0,1))*wt_us[,8]+  quantile(normal[,9], probs=runif(1,0,1))*wt_us[,9]
         + quantile(normal[,10], probs=runif(1,0,1))*wt_us[,10]  + quantile(normal[,11], probs=runif(1,0,1))*wt_us[,11])
  
  TUi[i] <- tui 
}


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_soybean <- read.table("soybean_threshold25_bb_version_3 _github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("USA_soybean_t30_simulations_github_period1.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main="Days above 30°C in the USA May-Sep", xlab="Number of days", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.005587
# period 2: 0.006753


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_soybean [,4], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.006" , "SE2 = 0.007", c(paste("threshold = ", round(thres_soybean[1,4], digits =2), sep=""))))





######### Brazil -------
setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
### cum precipitation -------

cum_prec <- read.csv("Brazil_soybean_BB_cum_prec_Nov_Mar.csv", header=T, sep=",")
cum_prec<- cum_prec[,-1]

u_br_prec <- pobs(cum_prec[1:24,])
u_br_prec <- pobs(cum_prec[25:46,])
u_br_prec <- pobs(cum_prec)

cum_prec <- cum_prec[1:24,] #b
cum_prec <- cum_prec[25:46,] #c

RVM = RVineStructureSelect(u_br_prec,c(1:6),progress=TRUE)

clim <- foreach (z=1:10,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
simdata_br_prec <- RVineSim(rep, RVM)


# marginal distributions

# i=9
# plot(fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[,i],"norm"))
# plot(fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# 
# for ( i in 1:9){
#                       a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#                       b <- fitdist(cum_prec[,i],"norm")
#                       c <- fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme")
#                       print(summary(a)$aic)
#                       print(summary(b)$aic)
#                       print(summary(c)$aic)
#                       #print(summary(a)$bic)
#                       #print(summary(b)$bic)
#                       #print(summary(c)$bic)
#                       }
  
#norm: 2,7,9   gumbel:3,4   gamma: 1,5,6,8

# a

      # normal <- matrix(nrow=rep, ncol=9)
      # t = c(2,7,9)
      # for (i in t){
      #   Normal<-fitdist(cum_prec[,i],"norm")
      #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # x = c(3,4)
      # for ( i in x){
      #   Gumbel<-fitdist(cum_prec[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
      #   normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      # }
      # r=c(1,5,6,8)
      # for ( i in r){
      #   Normal<-fitdist(cum_prec[,i],"gamma", method="mme")
      #   normal[,i] <- rgamma(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # colnames(normal) <- colnames(cum_prec[,1:9])


####  1967-1990


# i=9
# plot(fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[,i],"norm"))
# plot(fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# for ( i in 1:9){
#                       a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#                       b <- fitdist(cum_prec[,i],"norm")
#                       c <- fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme")
#                       print(summary(a)$aic)
#                       print(summary(b)$aic)
#                       print(summary(c)$aic)
#                       # print(summary(a)$bic)
#                       # print(summary(b)$bic)
#                       # print(summary(c)$bic)
#                       }

#normal:5,9   gumbel: 6   gamma: 1,2,3,4,7,8

#b

      normal <- matrix(nrow=rep, ncol=9)
      t = c(5,9)
      for (i in t){
        Normal<-fitdist(cum_prec[,i],"norm")
        normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      }
      x = c(6)
      for ( i in x){
        Gumbel<-fitdist(cum_prec[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      }

      r=c(1,2,3,4,7,8)
      for ( i in r){
        Normal<-fitdist(cum_prec[,i],"gamma", method="mme")
        normal[,i] <- rgamma(rep,Normal$estimate[1], Normal$estimate[2])
      }
      colnames(normal) <- colnames(cum_prec[,1:9])


# 1991-2012


# i=9
# plot(fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[,i],"norm"))
# plot(fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# 
# for ( i in 1:9){
#                       a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#                       b <- fitdist(cum_prec[,i],"norm")
#                       c <- fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme")
#                       print(summary(a)$aic)
#                       print(summary(b)$aic)
#                       print(summary(c)$aic)}
#                       # print(summary(a)$bic)
#                       # print(summary(b)$bic)
#                       # print(summary(c)$bic)}


#gumbel: 3,4,6  , normal:1,2,5,7,9  gamma: 8

#c

      # normal <- matrix(nrow=rep, ncol=9)
      # t = c(1,2,5,7,9)
      # for (i in t){
      #   Normal<-fitdist(cum_prec[,i],"norm")
      #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # x = c(3,4,6)
      # for ( i in x){
      #   Gumbel<-fitdist(cum_prec[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
      #   normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      # }
      # 
      # r=c(8)
      # for ( i in r){
      #   Normal<-fitdist(cum_prec[,i],"gamma", method="mme")
      #   normal[,i] <- rgamma(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # colnames(normal) <- colnames(cum_prec[,1:9])
      # 


# CDF
PB <- vector("numeric", rep)
for (i in 1:rep){
  pb = (quantile(normal[,1], probs=simdata_br_prec[i,1])*wt_br[,1] + quantile(normal[,2], probs=simdata_br_prec[i,2])*wt_br[,2]  + quantile(normal[,3], probs=simdata_br_prec[i,3])*wt_br[,3]  
        + quantile(normal[,4], probs=simdata_br_prec[i,4])*wt_br[,4] + quantile(normal[,5], probs=simdata_br_prec[i,5])*wt_br[,5] 
        + quantile(normal[,6], probs=simdata_br_prec[i,6])*wt_br[,6] + quantile(normal[,7], probs=simdata_br_prec[i,7])*wt_br[,7] 
        + quantile(normal[,8], probs=simdata_br_prec[i,8])*wt_br[,8] + quantile(normal[,9], probs=simdata_br_prec[i,9])*wt_br[,9] 
  )
  
  PB[i] <- pb 
}

return(PB)
}

clim <- as.data.frame(clim)
write.table(clim, "Brazil_soybean_cum_prec_simulations_github_period2.csv", sep=",", row.names=F)

# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_soybean <- read.table("soybean_threshold25_bb_version_3 _github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("Brazil_soybean_cum_prec_simulations_github_period2.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main=" Cumulative precipitation in Brazil Nov-Mar", xlab="Precipitation (in mm)", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.03506
# period 2: 0.05718


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_soybean [,5], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.035" , "SE2 = 0.057", c(paste("threshold = ", round(thres_soybean[1,5], digits =2), sep=""))))



ecdfPlot(PB, main=" Cumulative growing season precipitation Brazil", xlab="Precipitation in mm", ylab="F(x)")

ecdfPlot(PB, add=T, ecdf.col="grey55")

legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))


# CDF independent
PBi <- vector("numeric", rep)
for (i in 1:rep){
  pbi = (quantile(normal[,1], probs=runif(1,0,1))*wt_br[,1] + quantile(normal[,2], probs=runif(1,0,1))*wt_br[,2]  + quantile(normal[,3], probs=runif(1,0,1))*wt_br[,3]  
        + quantile(normal[,4], probs=runif(1,0,1))*wt_br[,4] + quantile(normal[,5], probs=runif(1,0,1))*wt_br[,5] 
        + quantile(normal[,6], probs=runif(1,0,1))*wt_br[,6] + quantile(normal[,7], probs=runif(1,0,1))*wt_br[,7] 
        + quantile(normal[,8], probs=runif(1,0,1))*wt_br[,8] + quantile(normal[,9], probs=runif(1,0,1))*wt_br[,9] 
  )
  
  PBi[i] <- pbi 
}

############# India --------------

## cumulative precipitation Jun_Nov ------

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
cum_prec <- read.csv("India_soybean_BB_cum_prec_Jun_Nov.csv", header=T, sep=",")
cum_prec <- cum_prec[-47,-1]
cum_prec <- cum_prec[,-4]
# 1966 refers to agricultural year 1966_67 and influences 1966_67 yields


u_in_prec <-  pobs(cum_prec[1:24,])
u_in_prec <- pobs(cum_prec[25:46,])
u_in_prec <- pobs(cum_prec)

cum_prec<- cum_prec[1:24,] #b
cum_prec<- cum_prec[25:46,] #c

RVM = RVineStructureSelect(u_in_prec,c(1:6),progress=TRUE)

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
simdata_in_prec <- RVineSim(rep, RVM)


# marginal distributions


# plot(fitdist(cum_prec[,2],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[,2],"norm"))
# plot(fitdist(cum_prec[,2],"gamma",start=list(a=0,b=1),method="mme"))
# 
# for ( i in 1:4){
#   a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(cum_prec[,i],"norm")
#   c <- fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(c)$aic)
#   # print(summary(a)$bic)
#   # print(summary(b)$bic)
#   # print(summary(c)$bic)
#   }

# gamma: 2  gumbel: 1, norm: 3

# a
      
      # normal <- matrix(nrow=rep, ncol=3)
      # t = c(3)
      # for (i in t){
      #   Normal<-fitdist(cum_prec[,i],"norm")
      #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # x = c(1)
      # for ( i in x){
      #   Gumbel<-fitdist(cum_prec[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
      #   normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      # }
      # x = c(2)
      # for ( i in x){
      #   Normal<-fitdist(cum_prec[,i],"gamma", method="mme")
      #   normal[,i] <- rgamma(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # colnames(normal) <- colnames(cum_prec)


#### 1967-1990



# i=3
# plot(fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[,i],"norm"))
# plot(fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# 
# for ( i in 1:3){
#   a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(cum_prec[,i],"norm")
#   c <- fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(c)$aic)
#   # print(summary(a)$bic)
#   # print(summary(b)$bic)
#   # print(summary(c)$bic)
#   }

#gamma: 1,3  normal: 2

#b

      # normal <- matrix(nrow=rep, ncol=3)
      # x = c(1,3)
      # for ( i in x){
      #   Normal<-fitdist(cum_prec[,i],"gamma", method="mme")
      #   normal[,i] <- rgamma(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # Normal<-fitdist(cum_prec[,2],"norm")
      # normal[,2] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # colnames(normal) <- colnames(cum_prec[,1:3])


#### 1991_2012



# i=2
# plot(fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[,i],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[,i],"norm"))
# plot(fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# 
# for ( i in 1:3){
#   a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(cum_prec[,i],"norm")
#   c <- fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(c)$aic)
#   # print(summary(a)$bic)
#   # print(summary(b)$bic)
#   # print(summary(c)$bic)
#   }

# gumbel: 1,2   norm: 3

#c
# 
      normal <- matrix(nrow=rep, ncol=3)
      x = c(1:2)
      for ( i in x){
        Gumbel<-fitdist(cum_prec[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      }
      Normal <-fitdist(cum_prec[,3],"norm")
      normal[,3] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])

      colnames(normal) <- colnames(cum_prec)


# CDF
SI <- vector("numeric", rep)
for (i in 1:rep){
  si = (quantile(normal[,1], probs=simdata_in_prec[i,1])*wt_in[,1] 
        + quantile(normal[,2], probs=simdata_in_prec[i,2])*wt_in[,2] 
        + quantile(normal[,3], probs=simdata_in_prec[i,3]) *wt_in[,3]
  )
  
  SI[i] <- si 
}
return(SI)
}

clim <- as.data.frame(clim)
write.table(clim, "India_soybean_cum_prec_simulations_github_period2_new.csv", sep=",", row.names=F)

# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_soybean <- read.table("soybean_threshold25_bb_version_3 _github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("India_soybean_cum_prec_simulations_github_period2.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main="Cumulative precipitation in India Jun-Nov", xlab="Precipitation (in mm)", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.08793
# period 2: 0.0882


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_soybean [,2], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.088" , "SE2 = 0.088", c(paste("threshold = ", round(thres_soybean[1,2], digits =2), sep=""))))


ecdfPlot(SI, main="Cumulative precipitation between June and November in India", xlab="Precipitation (mm)", ylab="F(x)", cex.main=0.8)

ecdfPlot(SI, add=T, ecdf.col="grey55")

legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))

# CDF independent

SIi <- vector("numeric", rep)
for (i in 1:rep){
  sii = (quantile(normal[,1], probs=runif(1,0,1))*wt_in[,1]
        + quantile(normal[,2], probs=runif(1,0,1)) *wt_in[,2]
        + quantile(normal[,3], probs=runif(1,0,1)) *wt_in[,3]
  )
  
  SIi[i] <- sii 
}


## t30 (version 2) -----------

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
t30 <- read.csv("India_soybean_BB_t30_Jun_Nov.csv", header=T, sep=",")
t30<- t30[-1,2:4]

u_in_t30 <- pobs(t30[1:24,])
u_in_t30 <- pobs(t30[25:46,])
u_in_t30 <- pobs(t30)

t30<- t30[1:24,] #b
t30<- t30[25:46,] #c

RVM = RVineStructureSelect(u_in_t30,c(1:6),progress=TRUE)

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
simdata_in_t30 <- RVineSim(rep, RVM)


# marginal distributions


# 
# plot(fitdist(t30[,3],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(t30[,3],"norm"))
# 
# 
# for ( i in 1:3){
#   a <- fitdist(t30[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(t30[,i],"norm")
#   c <- fitdist(t30[,i],"gamma",start=list(a=0,b=1),method="mme")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(c)$aic)
#   # print(summary(a)$bic)
#   # print(summary(b)$bic)
#   # print(summary(c)$bic)
#   }

#norm 1,2,3 , gumbel: 

#a

      # normal <- matrix(nrow=rep, ncol=3)
      # x = c(1:3)
      # for ( i in x){
      #   Normal <-fitdist(t30[,i],"norm")
      #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # colnames(normal) <- colnames(t30)



#### 1967-1990


# 
# plot(fitdist(t30[,3],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(t30[,3],"norm"))
# plot(fitdist(t30[,3],"gamma",start=list(a=0,b=1),method="mme"))
# 
# 
# for ( i in 1:3){
#   a <- fitdist(t30[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(t30[,i],"norm")
#   c <- fitdist(t30[,i],"gamma",start=list(a=0,b=1),method="mme")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(c)$aic)
#   # print(summary(a)$bic)
#   # print(summary(b)$bic)
#   # print(summary(c)$bic)
#   }

#norm 3 gamma: 1,2 

#b

      # normal <- matrix(nrow=rep, ncol=3)
      # x = c(3)
      # for ( i in x){
      #   Normal <-fitdist(t30[,i],"norm")
      #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # l = c(1,2)
      # for ( i in l){
      #   Normal<-fitdist(t30[,i],"gamma", method="mme")
      #   normal[,i] <- rgamma(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # 
      # colnames(normal) <- colnames(t30)

# 1991-2012
# 
# plot(fitdist(t30[,3],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(t30[,3],"norm"))
# plot(fitdist(t30[,3],"gamma",start=list(a=0,b=1),method="mme"))
# 
# 
# for ( i in 1:3){
#   a <- fitdist(t30[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(t30[,i],"norm")
#   c <- fitdist(t30[,i],"gamma",start=list(a=0,b=1),method="mme")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(c)$aic)
#   # print(summary(a)$bic)
#   # print(summary(b)$bic)
#   # print(summary(c)$bic)
#   }

#norm:1,2,3 gumbel:

#c

      normal <- matrix(nrow=rep, ncol=3)
      x = c(1:3)
      for ( i in x){
        Normal <-fitdist(t30[,i],"norm")
        normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      }

      colnames(normal) <- colnames(t30)


# CDF
SI2 <- vector("numeric", rep)
for (i in 1:rep){
  si = (quantile(normal[,1], probs=simdata_in_t30[i,1])*wt_in[,1]
        + quantile(normal[,2], probs=simdata_in_t30[i,2]) *wt_in[,2]
        + quantile(normal[,3], probs=simdata_in_t30[i,3]) *wt_in[,3]
        )
  
  SI2[i] <- si 
}
return(SI2)
}
clim <- as.data.frame(clim)
write.table(clim, "India_soybean_t30_simulations_github_period2.csv", sep=",", row.names=F)


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_soybean <- read.table("soybean_threshold25_bb_version_3 _github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("India_soybean_t30_simulations_github_period2.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main="Days above 30°C in India Jun-Nov", xlab="Number of days", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.00728
# period 2: 0.008204


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_soybean [,3], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.007" , "SE2 = 0.008", c(paste("threshold = ", round(thres_soybean[1,3], digits =2), sep=""))))


# ecdfPlot(SI/6, main="Average number of days per month above 30°C during growing season in India", xlab="Days per month", ylab="F(x)", cex.main=0.8)
# 
# ecdfPlot(SI/6, add=T, ecdf.col="grey55")
# 
# legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))

# shade are below curve
fx <- ecdf(SI/6)

cord.x <- c(16,seq(16,quantile(SI/6, probs=0.4),0.01),quantile(SI/6, probs=0.4))
cord.y <- c(0,fx(seq(16,quantile(SI/6, probs=0.4),0.01)),0)
polygon(cord.x,cord.y,col='black', density=11)


# CDF independent

SI2i <- vector("numeric", rep)
for (i in 1:rep){
  sii = (quantile(normal[,1], probs=runif(1,0,1))*wt_in[,1]
        + quantile(normal[,2], probs=runif(1,0,1))*wt_in[,2] 
        + quantile(normal[,3], probs=runif(1,0,1)) *wt_in[,3]
  )
  
  SI2i[i] <- sii 
}


######### China ----

setwd("H:/DPhil/breadbaskets/Data collection")
t25 <- read.csv("soybean_threshold25.csv", header=T, sep=",")
# China: 
t25 <- t25[,1:9]


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
t30 <- read.csv("China_soybean_BB_t30_May_Sep.csv", header=T, sep=",")
t30<- t30[-1,]

u_ch_t30 <- pobs(t30[1:24,])
u_ch_t30 <- pobs(t30[25:46,])
u_ch_t30 <- pobs(t30)


t30<- t30[1:24,] #b
t30<- t30[25:46,] #c

RVM = RVineStructureSelect(u_ch_t30,c(1:6),progress=TRUE)

clim <- foreach (z=1:10,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
simdata_ch_t30 <- RVineSim(rep, RVM)
 
# marginal distributions

# 
# i=2
# plot(fitdist(t30[,i],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(t30[,i],"norm"))
# plot(fitdist(t30[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# 
# for ( i in 1:9){
#   a <- fitdist(t30[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(t30[,i],"norm")
#   c <- fitdist(t30[,i],"gamma",start=list(a=0,b=1),method="mme")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(c)$aic)
#   # print(summary(a)$bic)
#   # print(summary(b)$bic)
#   # print(summary(c)$bic)
#   }

#norm: 2,3,6,7 , gumbel: 9  gamma: 1,4,5,8

#a

      # normal <- matrix(nrow=rep, ncol=9)
      # x = c(2,3,6,7)
      # for ( i in x){
      #   Normal <-fitdist(t30[,i],"norm")
      #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # x = c(9)
      # for ( i in x){
      #   Gumbel<-fitdist(t30[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
      #   normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      # }
      # r=c(1,4,5,8)
      # for ( i in r){
      #   Normal<-fitdist(t30[,i],"gamma", method="mme")
      #   normal[,i] <- rgamma(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # 
      # colnames(normal) <- colnames(t30)



#### 1967-1990

# 
# i=9
# plot(fitdist(t30[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(t30[,i],"norm"))
# plot(fitdist(t30[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# 
# for ( i in 1:9){
#   a <- fitdist(t30[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(t30[,i],"norm")
#   c <- fitdist(t30[,i],"gamma",start=list(a=0,b=1),method="mme")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(c)$aic)
#   # print(summary(a)$bic)
#   # print(summary(b)$bic)
#   # print(summary(c)$bic)
 # }

#norm: 2,3,5,6,7,9  gumbel: 8  gamma: 1,4

#b

      normal <- matrix(nrow=rep, ncol=9)
      x = c(2,3,5,6,7,9)
      for ( i in x){
        Normal <-fitdist(t30[,i],"norm")
        normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      }

      x = c(8)
      for ( i in x){
        Gumbel<-fitdist(t30[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      }

      r=c(1,4)
      for ( i in r){
        Normal<-fitdist(t30[,i],"gamma", method="mme")
        normal[,i] <- rgamma(rep,Normal$estimate[1], Normal$estimate[2])
      }
      colnames(normal) <- colnames(t30)


# 1991-2012


# plot(fitdist(t30[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(t30[,i],"norm"))
# plot(fitdist(t30[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# 
# for ( i in 1:9){
#   a <- fitdist(t30[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(t30[,i],"norm")
#   c <- fitdist(t30[,i],"gamma",start=list(a=0,b=1),method="mme")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(c)$aic)
#   # print(summary(a)$bic)
#   # print(summary(b)$bic)
#   # print(summary(c)$bic)
#   }

#norm: 3,5,6,7  gumbel:9    gamma: 1,2,4,8

#c

      # normal <- matrix(nrow=rep, ncol=9)
      # x = c(3,5,6,7)
      # for ( i in x){
      #   Normal <-fitdist(t30[,i],"norm")
      #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # 
      # x = c(9)
      # for ( i in x){
      #   Gumbel<-fitdist(t30[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
      #   normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      # }
      # 
      # r=c(1,2,4,8)
      # for ( i in r){
      #   Normal<-fitdist(t30[,i],"gamma", method="mme")
      #   normal[,i] <- rgamma(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # colnames(normal) <- colnames(t30)



# CDF
# CTp2 <- vector("numeric", rep)
# for (i in 1:rep){
#   ct = (quantile(normal[,1], probs=simdata_ch_t30[i,1])* wt_ch[,1]
#         + quantile(normal[,2], probs=simdata_ch_t30[i,2]) * wt_ch[,2]
#         + quantile(normal[,3], probs=simdata_ch_t30[i,3])* wt_ch[,3] + quantile(normal[,4], probs=simdata_ch_t30[i,4])* wt_ch[,4]
#         + quantile(normal[,5], probs=simdata_ch_t30[i,5]) * wt_ch[,5]
#         + quantile(normal[,6], probs=simdata_ch_t30[i,6])* wt_ch[,6] + quantile(normal[,7], probs=simdata_ch_t30[i,7])* wt_ch[,7]
#         + quantile(normal[,8], probs=simdata_ch_t30[i,8])* wt_ch[,8] 
#         + quantile(normal[,9], probs=simdata_ch_t30[i,9])* wt_ch[,9]  
#   )
#   
#   CTp2[i] <- ct 
# }


# simultaneous climate risks in provinces

# !! all t30 neg correlated with yields
# 
# CT0 <- matrix(nrow=rep, ncol=9)
# for (i in 1:rep){
#   ct0 = c(quantile(normal[,1], probs=simdata_ch_t30[i,1]),
#           quantile(normal[,2], probs=simdata_ch_t30[i,2]), 
#           quantile(normal[,3], probs=simdata_ch_t30[i,3]), quantile(normal[,4], probs=simdata_ch_t30[i,4]),
#           quantile(normal[,5], probs=simdata_ch_t30[i,5]), 
#           quantile(normal[,6], probs=simdata_ch_t30[i,6]), quantile(normal[,7], probs=simdata_ch_t30[i,7]),
#           quantile(normal[,8], probs=simdata_ch_t30[i,8]), 
#           quantile(normal[,9], probs=simdata_ch_t30[i,9]))
#   
#   CT0[i,] <- ct0 
# }


# independence between provinces

CT0i <- matrix(nrow=rep, ncol=9)
for (i in 1:rep){
  ct0i = c(quantile(normal[,1], probs=runif(1,0,1)),
           quantile(normal[,2], probs=runif(1,0,1)), 
           quantile(normal[,3], probs=runif(1,0,1)), quantile(normal[,4], probs=runif(1,0,1)),
           quantile(normal[,5], probs=runif(1,0,1)), 
           quantile(normal[,6], probs=runif(1,0,1)), quantile(normal[,7], probs=runif(1,0,1)),
           quantile(normal[,8], probs=runif(1,0,1)), 
           quantile(normal[,9], probs=runif(1,0,1)))
  
  CT0i[i,] <- ct0i 
}
CT0 <- as.data.frame(CT0i)



#CT0 <- as.data.frame(CT0)


# 25% threshold
for (l in 1:rep) {
  CT0$V10[l] <- sum((ifelse(CT0$V1[l]>t25[1],1,0))[1,],(ifelse(CT0$V2[l]>t25[2],1,0))[1,],(ifelse(CT0$V3[l]>t25[3],1,0))[1,],
                    (ifelse(CT0$V4[l]>t25[4],1,0))[1,],(ifelse(CT0$V5[l]>t25[5],1,0))[1,], (ifelse(CT0$V6[l]>t25[6],1,0))[1,],
                    (ifelse(CT0$V7[l]>t25[7],1,0))[1,], (ifelse(CT0$V8[l]>t25[8],1,0))[1,], (ifelse(CT0$V9[l]>t25[9],1,0))[1,]
  )
  
}

col <- c(colnames(simdata_ch_t30), "BBs")
colnames(CT0) <- col

return(CT0[,10])
}


# return(CTp2)
# }

clim <- as.data.frame(clim)
write.table(clim, "China_soybean_t30_simulations_all_provinces_github_period1_indep.csv", sep=",", row.names=F)


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_soybean <- read.table("soybean_threshold25_bb_version_3 _github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("China_soybean_t30_simulations_github_period1.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main="Days above 30°C in China May-Sep", xlab="Number of days", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.003084
# period 2: 0.00245


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_soybean [,1], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.003" , "SE2 = 0.002", c(paste("threshold = ", round(thres_soybean[1,1], digits =2), sep=""))))


ecdfPlot(CT/5, main="Average number of days per month above 30°C during growing season in China", xlab="Days", ylab="F(x)", cex.main=0.8)

ecdfPlot(CT/5, add=T, ecdf.col="grey55")
legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))

my.CDF <- ecdf(CT/5)
1- my.CDF(7.9)



setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
ALL0 <- read.csv("China_soybean_t30_simulations_all_provinces_github_period1_indep.csv", sep=",", header=T)

ALL0_perc <- matrix(nrow=1000, ncol=10)
#period1
for (o in 1:1000){
  for (i in 0:9){
    ALL0_perc[o,i+1]<- sum(ALL0[,o]==i)/10000 
  }}

m1 <- apply(ALL0_perc,2,mean)
s1 <- apply(ALL0_perc,2,sd)



ALL0 <- read.csv("China_soybean_t30_simulations_all_provinces_github_period2_indep.csv", sep=",", header=T)

ALL0_perc <- matrix(nrow=1000, ncol=10)
#period2
for (o in 1:1000){
  for (i in 0:9){
    ALL0_perc[o,i+1]<- sum(ALL0[,o]==i)/10000 
  }}

m2 <- apply(ALL0_perc,2,mean)
s2 <- apply(ALL0_perc,2,sd)

stats<- rbind(m1,s1,m2,s2)

write.table(stats, "China_soybean_t30_simultaneous_pdf_25_version_3_github_indep.csv", sep=",", row.names=c("mean_period1","sd_period1",  "mean_period2","sd_period2"))




# CDF independent
CTip2 <- vector("numeric", rep)
for (i in 1:rep){
  cti = (quantile(normal[,1], probs=runif(1,0,1))* wt_ch[,1]
        + quantile(normal[,2], probs=runif(1,0,1))* wt_ch[,2] 
        + quantile(normal[,3], probs=runif(1,0,1))* wt_ch[,3] + quantile(normal[,4], probs=runif(1,0,1))* wt_ch[,4]
        + quantile(normal[,5], probs=runif(1,0,1))* wt_ch[,5] 
        + quantile(normal[,6], probs=runif(1,0,1))* wt_ch[,6] + quantile(normal[,7], probs=runif(1,0,1))* wt_ch[,7]
        + quantile(normal[,8], probs=runif(1,0,1))* wt_ch[,8] 
        + quantile(normal[,9], probs=runif(1,0,1))* wt_ch[,9]  
  )
  
  CTip2[i] <- cti 
}

# individual probabilities of exceeding threshold per states

setwd("H:/DPhil/breadbaskets/Data collection")
t25 <- read.csv("soybean_threshold25.csv", header=T, sep=",")
# China: 
t25 <- t25[,1:9]


# !! all t30 neg correlated with yields

CT0 <- matrix(nrow=rep, ncol=9)
for (i in 1:rep){
  ct0 = c(quantile(normal[,1], probs=simdata_ch_t30[i,1]),
         quantile(normal[,2], probs=simdata_ch_t30[i,2]), 
         quantile(normal[,3], probs=simdata_ch_t30[i,3]), quantile(normal[,4], probs=simdata_ch_t30[i,4]),
         quantile(normal[,5], probs=simdata_ch_t30[i,5]), 
          quantile(normal[,6], probs=simdata_ch_t30[i,6]), quantile(normal[,7], probs=simdata_ch_t30[i,7]),
         quantile(normal[,8], probs=simdata_ch_t30[i,8]), 
         quantile(normal[,9], probs=simdata_ch_t30[i,9]))
  
  CT0[i,] <- ct0 
}
CT0 <- as.data.frame(CT0)

# independent
CT0i <- matrix(nrow=rep, ncol=9)
for (i in 1:rep){
  ct0i = c(quantile(normal[,1], probs=runif(1,0,1)),
          quantile(normal[,2], probs=runif(1,0,1)), 
          quantile(normal[,3], probs=runif(1,0,1)), quantile(normal[,4], probs=runif(1,0,1)),
          quantile(normal[,5], probs=runif(1,0,1)), 
          quantile(normal[,6], probs=runif(1,0,1)), quantile(normal[,7], probs=runif(1,0,1)),
          quantile(normal[,8], probs=runif(1,0,1)), 
          quantile(normal[,9], probs=runif(1,0,1)))
  
  CT0i[i,] <- ct0i 
}
CT0i <- as.data.frame(CT0i)


# 1967-1990


for ( i in x){
  t25_prop_soy[1,i] <- length( which( CT0[i]>t25[i][1,] ) )/10000
}


# 1990-2012

for ( i in x){
  t25_prop_soy[2,i] <- length( which( CT0[i]>t25[i][1,] ) )/10000
}


# number of states/provinces with risks


write.table(CT0_perc, "Soybean_china_simultaneous_cdf.csv", sep=",", row.names=F)

setwd("H:/DPhil/breadbaskets/Data collection")

CT0_perc <- read.csv("Soybean_china_simultaneous_cdf.csv", sep=",", header=T)

hist(CT0$V11, freq=F, breaks=10)
mean(CT0$V12)
plot(ecdf(CT0$V11))

bp<-barplot(CT0_perc[,1], col="grey84", ylab="F(x)", xlab="Number of provinces with simultaneous climate risks", 
         col.axis="black" )
text( x=bp, y=CT0_perc[,1], labels=round(CT0_perc[,1], digits=2), font=2,  pos=1, cex=0.7)

bp2= barplot(CT0_perc[,2], add=T, col="blue",density=12, names.arg=c("< 1","< 2", "< 3", "< 4","< 5","< 6","< 7","< 8","< 9","9" ))
mtext(text =round(CT0_perc[,2], digits=2), side = 1, col="blue",font=2, cex=0.7,at = bp2, line = 0)

legend("topleft", legend=c("1967-1990",  "1991-2012"), bty="n",cex=0.8,lty=c (1,1),lwd=2,col=c("black","blue"))



# pdf

bp<-barplot(CT0_perc[,3], col="grey84", ylab="f(x)", xlab="Number of provinces with simultaneous climate risks", 
            col.axis="black" , ylim=c(0,0.25))
text( x=bp, y=CT0_perc[,3], labels=round(CT0_perc[,3], digits=3), font=2,  pos=3, cex=0.7)

bp2= barplot(CT0_perc[,4], add=T, col="blue",density=12, names.arg=c("< 1","< 2", "< 3", "< 4","< 5","< 6","< 7","< 8","< 9","9" ))
mtext(text =round(CT0_perc[,4], digits=3), side = 1, col="blue",font=2, cex=0.7,at = bp2, line = 0)

legend("topleft", legend=c("1967-1990",  "1991-2012"), bty="n",cex=0.8,lty=c (1,1),lwd=2,col=c("black","blue"))


# 25% threshold
for (i in 1:rep) {
  CT0$V10[i] <- sum((ifelse(CT0$V1[i]>t25[1],1,0))[1,],(ifelse(CT0$V2[i]>t25[2],1,0))[1,],(ifelse(CT0$V3[i]>t25[3],1,0))[1,],
                    (ifelse(CT0$V4[i]>t25[4],1,0))[1,],(ifelse(CT0$V5[i]>t25[5],1,0))[1,], (ifelse(CT0$V6[i]>t25[6],1,0))[1,],
                    (ifelse(CT0$V7[i]>t25[7],1,0))[1,], (ifelse(CT0$V8[i]>t25[8],1,0))[1,], (ifelse(CT0$V9[i]>t25[9],1,0))[1,]
  )
  #CT0$V11[i]<- CT0$V10[i]/9
}

col <- c(colnames(simdata_ch_t30), "BBs")
colnames(CT0) <- col
write.table(CT0, "soybean_china_version3_t25_period2_vx.csv", sep=";", row.names = F)

# 25% threshold independent
for (i in 1:rep) {
  CT0i$V10[i] <- sum((ifelse(CT0i$V1[i]>t25[1],1,0))[1,],(ifelse(CT0i$V2[i]>t25[2],1,0))[1,],(ifelse(CT0i$V3[i]>t25[3],1,0))[1,],
                    (ifelse(CT0i$V4[i]>t25[4],1,0))[1,],(ifelse(CT0i$V5[i]>t25[5],1,0))[1,], (ifelse(CT0i$V6[i]>t25[6],1,0))[1,],
                    (ifelse(CT0i$V7[i]>t25[7],1,0))[1,], (ifelse(CT0i$V8[i]>t25[8],1,0))[1,], (ifelse(CT0i$V9[i]>t25[9],1,0))[1,]
  )
  #CT0i$V11[i]<- CT0i$V10[i]/9
}

col <- c(colnames(simdata_ch_t30), "BBs")
colnames(CT0i) <- col
write.table(CT0i, "soybean_china_version3_t25_period2_vx_indep.csv", sep=";", row.names = F)
CT0i <- read.table("soybean_china_version3_t25_period2_vx_indep.csv", sep=";", header=T)

CT0_perc <- matrix(nrow=10, ncol=2)

for (i in 1:10){
  CT0_perc[i,1]<- sum(CT0$BBs<i)/10000
}

for (i in 1:10){
  CT0_perc[i,2]<- sum(CT0$BBs<i)/10000
}
colnames(CT0_perc) <- c("1967-1990", "1991-2012")

write.table(CT0_perc, "Soybean_china_simultaneous_cdf_25_vx.csv", sep=";", row.names=F)


# independent
CT0_perci <- matrix(nrow=10, ncol=2)

for (i in 1:10){
  CT0_perci[i,1]<- sum(CT0$BBs<i)/10000
}

for (i in 1:10){
  CT0_perci[i,2]<- sum(CT0$BBs<i)/10000
}
colnames(CT0_perci) <- c("1967-1990", "1991-2012")

write.table(CT0_perci, "Soybean_china_simultaneous_cdf_25_vx_indep.csv", sep=";", row.names=F)



setwd("H:/DPhil/breadbaskets/Data collection")

CT0_perc <- read.csv("Soybean_china_simultaneous_cdf_25.csv", sep=";", header=T)

hist(CT0$V11, freq=F, breaks=10)
mean(CT0$V12)
plot(ecdf(CT0$V11))

bp<-barplot(CT0_perc[,1], col="grey84", ylab="F(x)", xlab="Number of provinces with simultaneous climate risks", 
            col.axis="black" )
text( x=bp, y=CT0_perc[,1], labels=round(CT0_perc[,1], digits=2), font=2,  pos=1, cex=0.7)

bp2= barplot(CT0_perc[,2], add=T, col="blue",density=12, names.arg=c("< 1","< 2", "< 3", "< 4","< 5","< 6","< 7","< 8","< 9","9" ))
mtext(text =round(CT0_perc[,2], digits=2), side = 1, col="blue",font=2, cex=0.7,at = bp2, line = 0)

legend("topleft", legend=c("1967-1990",  "1991-2012"), bty="n",cex=0.8,lty=c (1,1),lwd=2,col=c("black","blue"))

# as line

bp<-plot(CT0_perc[,1], type="o", ylab="F(x)", xlab="Number of provinces with simultaneous climate risks" )
text( x=bp, y=CT0_perc[,1], labels=round(CT0_perc[,1], digits=2), font=2,  pos=1, cex=0.7)

lines(CT0_perc[,2], type="o", col="blue", names.arg=c("< 1","< 2", "< 3", "< 4","< 5","< 6","< 7","< 8","< 9","9" ))
mtext(text =round(CT0_perc[,2], digits=2), side = 1, col="blue",font=2, cex=0.7,at = bp2, line = 0)

legend("topleft", legend=c("1967-1990",  "1991-2012"), bty="n",cex=0.8,lty=c (1,1),lwd=2,col=c("black","blue"))




setwd("H:/DPhil/breadbaskets/Data collection")
ch_perc <- read.csv("soybean_china_simultaneous_cdf_25_vx.csv", sep=";", header=T)

bp<-barplot(ch_perc[,3], col="grey84", ylab="Relative frequency of occurrence", xlab="Number of breadbaskets with simultaneous climate risks", 
            col.axis="black" , ylim=c(0,0.3))
#x = c(1,3,4)
#text( x=bp[x,], y=ALL0_perc[x,4], labels=round(ALL0_perc[x,3], digits=3), font=2,  pos=3, cex=0.7)
text( x=bp[1,], y=ch_perc[1,4], labels=round(ch_perc[1,3], digits=3), font=2,  adj=c(0.5,-3),  cex=0.7)
text( x=bp[3,], y=ch_perc[3,3], labels=round(ch_perc[3,3], digits=3), font=2,  adj=c(0.5,-3),  cex=0.7)
text( x=bp[4,], y=ch_perc[4,3], labels=round(ch_perc[4,3], digits=3), font=2,  adj=c(0.5,-3),  cex=0.7)
text( x=bp[2,], y=ch_perc[2,3], labels=round(ch_perc[2,3], digits=3), font=2,  adj=c(0.5,-3),  cex=0.7)
text( x=bp[5,], y=ch_perc[5,3], labels=round(ch_perc[5,3], digits=3), font=2,  adj=c(0.5,-3),  cex=0.7)
text( x=bp[6,], y=ch_perc[6,3], labels=round(ch_perc[6,3], digits=3), font=2,  adj=c(0.5,-3),  cex=0.7)
text( x=bp[7,], y=ch_perc[7,3], labels=round(ch_perc[7,3], digits=3), font=2,  adj=c(0.5,-3),  cex=0.7)
text( x=bp[8,], y=ch_perc[8,4], labels=round(ch_perc[8,3], digits=3), font=2,  adj=c(0.5,-3),  cex=0.7)
text( x=bp[9,], y=ch_perc[9,4], labels=round(ch_perc[9,3], digits=3), font=2,  adj=c(0.5,-3),  cex=0.7)
text( x=bp[10,], y=ch_perc[10,4], labels=round(ch_perc[10,3], digits=3), font=2,  adj=c(0.5,-3),  cex=0.7)

bp2= barplot(ch_perc[,4], add=T, col="blue",density=12, names.arg=c("0","1","2", "3", "4","5", "6", "7", "8", "9"))

#mtext(text =round(ALL0_perc[,4], digits=3), side = 1, col="blue",font=2, cex=0.7,at = bp2, line = 0)
#x = c(1,3,4)
#text( x=bp2[x,], y=ALL0_perc[x,4], labels=round(ALL0_perc[x,4], digits=3), col="blue",font=2,  pos=3, cex=0.7)
text( x=bp2[1,], y=ch_perc[1,4], labels=round(ch_perc[1,4], digits=3), col="blue",font=2,  adj=c(0.5,-1),  cex=0.7)
text( x=bp2[3,], y=ch_perc[3,3], labels=round(ch_perc[3,4], digits=3), col="blue",font=2,  adj=c(0.5,-1),  cex=0.7)
text( x=bp2[4,], y=ch_perc[4,3], labels=round(ch_perc[4,4], digits=3), col="blue",font=2,  adj=c(0.5,-1),  cex=0.7)
text( x=bp2[2,], y=ch_perc[2,3], labels=round(ch_perc[2,4], digits=3), col="blue",font=2,  adj=c(0.5,-1),  cex=0.7)
text( x=bp2[5,], y=ch_perc[5,3], labels=round(ch_perc[5,4], digits=3), col="blue",font=2,  adj=c(0.5,-1),  cex=0.7)
text( x=bp2[6,], y=ch_perc[6,3], labels=round(ch_perc[6,4], digits=3), col="blue",font=2,  adj=c(0.5,-1),  cex=0.7)
text( x=bp2[7,], y=ch_perc[7,3], labels=round(ch_perc[7,4], digits=3), col="blue",font=2,  adj=c(0.5,-1),  cex=0.7)
text( x=bp2[8,], y=ch_perc[8,4], labels=round(ch_perc[8,4], digits=3), col="blue",font=2,  adj=c(0.5,-1),  cex=0.7)
text( x=bp2[9,], y=ch_perc[9,4], labels=round(ch_perc[9,4], digits=3), col="blue",font=2,  adj=c(0.5,-1),  cex=0.7)
text( x=bp2[10,], y=ch_perc[10,4], labels=round(ch_perc[10,4], digits=3), col="blue",font=2,  adj=c(0.5,-1),  cex=0.7)

legend("topleft", legend=c("1967-1990",  "1991-2012"), bty="n",cex=0.8,lty=c (1,1),lwd=2,col=c("black","blue"))



# plot dependencies
setwd("H:/DPhil/breadbaskets/Data collection")
ch_perc <- read.csv("soybean_china_simultaneous_cdf_25_vx_all_dependencies.csv", sep=";", header=F)
colnames(ch_perc) <- c("periods",0:9)
x=c(1,3)
y=c(2,4)
bp<-barplot(as.matrix(ch_perc[x,2:11]), col=c("black", "blue" ), ylab="Relative frequency of occurrence", xlab="Number of breadbaskets with simultaneous climate risks", 
            col.axis="black" , ylim=c(0,0.4), beside=T)
text( x=bp, y=as.matrix(ch_perc[x,2:11]), labels=round(as.matrix(ch_perc[x,2:11]), digits=3), font=2,  pos=3, cex=0.5)

#bp<-barplot(as.matrix(ch_perc[y,2:11]), col=c( "grey84","cyan4"), ylab="Relative frequency of occurrence", xlab="Number of breadbaskets with simultaneous climate risks", 
            #col.axis="black" , ylim=c(0,0.4), beside=T)
bp<-barplot(as.matrix(ch_perc[y,2:11]), col=c( "black", "blue" ), ylab="Relative frequency of occurrence", xlab="Number of breadbaskets with simultaneous climate risks", 
            col.axis="black" , ylim=c(0,0.4), beside=T)
text( x=bp, y=as.matrix(ch_perc[y,2:11]), labels=round(as.matrix(ch_perc[y,2:7]), digits=3), font=2,  pos=3, cex=0.5)

legend(x=16,y=0.4, title= "Period 1",legend=c("hist dependencies",  "independence betw provinces"), bty="n",cex=0.8,lty=c (1,1),lwd=2,col=c("black", "blue" ))
#legend(x=16,y=0.4,,title = "Period 2",legend=c("hist dependencies",  "indep betw provinces"), bty="n",cex=0.8,lty=c (1,1),lwd=2,col=c("grey84","cyan4"))
legend(x=16, y=0.4,title = "Period 2",legend=c("hist dependencies",  "independence betw provinces"), bty="n",cex=0.8,lty=c (1,1),lwd=2,col=c("black", "blue" ))


# test for difference between periods and between dependencies
# normally distributed?

CT5000 <- sample(CT, 5000, replace=TRUE)
shapiro.test(CT5000)
CT5000i <- sample(CTi, 5000, replace=TRUE)
shapiro.test(CT5000i)
hist(CTi,add=T,  freq=F)
# between periods:
# Wilcoxon (or Mann-Whitney-Wilcoxon) test -> assumes normal distribution
wilcox.test(CTi, CTip2)

# between dependencies:
# Wilcoxon (or Mann-Whitney-Wilcoxon) test -> assumes normal distribution
wilcox.test(CT, CTi)
wilcox.test(CTp2, CTip2)



# for threshold exceedance

period1 <- read.csv("soybean_china_version3_t25_period1_vx.csv", sep=";", header=T)
period2 <- read.csv("soybean_china_version3_t25_period2_vx.csv", sep=";", header=T)
period1 <- period1[,10]
period2 <- period2[,10]

period1i <- read.csv("soybean_china_version3_t25_period1_vx_indep.csv", sep=";", header=T)
period2i <- read.csv("soybean_china_version3_t25_period2_vx_indep.csv", sep=";", header=T)
period1i <- period1i[,10]
period2i <- period2i[,10]

# between periods:
# Wilcoxon (or Mann-Whitney-Wilcoxon) test -> assumes normal distribution
wilcox.test(period1i, period2i)

# between dependencies:
# Wilcoxon (or Mann-Whitney-Wilcoxon) test -> assumes normal distribution
wilcox.test(period2, period2i, alternative = "less")




# each case separate
pvalue <- matrix (nrow=10, ncol=1)
for (i in 0:9){
  pois <-poisson.test(c(sum(period2==i),	sum(period2i==i)), c(10000, 10000), alternative = c("greater"))
  pvalue[i+1,] <- pois$p.value
}

# -> significant differences except for i=0

######### Argentina ------

## cum prec ---------
setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
cum_prec <- read.csv("Argentina_soybean_BB_cum_prec_Nov_Mar.csv", header=T, sep=",")
cum_prec <- cum_prec[,-1]

u_ar_prec <-  pobs(cum_prec[1:24,])
u_ar_prec <- pobs(cum_prec[25:46,])
u_ar_prec <- pobs(cum_prc)

cum_prec <- cum_prec[1:24,] #b
cum_prec <- cum_prec[25:46,] #c


RVM = RVineStructureSelect(u_ar_prec,c(1:6),progress=TRUE)

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
simdata_ar_prec <- RVineSim(rep, RVM)


# marginal distributions
# 
# i=4
# plot(fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[,i],"norm"))
# plot(fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# for ( i in 1:4){
#   a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(cum_prec[,i],"norm")
#   c <- fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(c)$aic)
#   # print(summary(a)$bic)
#   # print(summary(b)$bic)
#   # print(summary(c)$bic)
#   }

# norm: 1  gamma: 2,4   gumbel: 3

#a
# 
#       normal <- matrix(nrow=rep, ncol=4)
#       for ( i in 1){
#         Normal<-fitdist(cum_prec[,i],"norm")
#         normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
#       }
#       x=c(2:4)
#       for (i in x){
#         Gamma<-fitdist(cum_prec[,i],"gamma", method="mme")
#         normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
#       }
#       x=c(3)
#       for ( i in x) {
#         Gumbel<-fitdist(cum_prec[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
#         normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
#       }
#       colnames(normal) <- colnames(cum_prec)

# 1967-1990


# i=4
# plot(fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[,i],"norm"))
# plot(fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# 
# for ( i in 1:4){
#   a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(cum_prec[,i],"norm")
#   c <- fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(c)$aic)
#   # print(summary(a)$bic)
#   # print(summary(b)$bic)
#   # print(summary(c)$bic)
#   }

#norm: 1,2,3,4

#b

        # normal <- matrix(nrow=rep, ncol=4)
        # x=c(1:4)
        # for ( i in x){
        #   Normal<-fitdist(cum_prec[,i],"norm")
        #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        # }
        # colnames(normal) <- colnames(cum_prec)


# 1991-2012

# 
# i=4
# plot(fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(cum_prec[,i],"norm"))
# plot(fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# for ( i in 1:4){
#   a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(cum_prec[,i],"norm")
#   c <- fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(c)$aic)
#   # print(summary(a)$bic)
#   # print(summary(b)$bic)
#   # print(summary(c)$bic)
#   }

# norm: 1  gamma:  gumbel: 2,3,4

  # c
        
      normal <- matrix(nrow=rep, ncol=4)
      for ( i in 1){
        Normal<-fitdist(cum_prec[,i],"norm")
        normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      }

      x=c(2,3,4)
      for ( i in x) {
      Gumbel<-fitdist(cum_prec[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
      normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      }

      colnames(normal) <- colnames(cum_prec)


# CDF
ART <- vector("numeric", rep)
for (i in 1:rep){
  art = (quantile(normal[,1], probs=simdata_ar_prec[i,1])* wt_ar[,1]+ quantile(normal[,2], probs=simdata_ar_prec[i,2])* wt_ar[,2] + quantile(normal[,3], probs=simdata_ar_prec[i,3])* wt_ar[,3] 
         + quantile(normal[,4], probs=simdata_ar_prec[i,4])* wt_ar[,4])
  
  ART[i] <- art
}
return(ART)
}

clim <- as.data.frame(clim)
write.table(clim, "Argentina_soybean_cum_prec_simulations_github_period2.csv", sep=",", row.names=F)


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_soybean <- read.table("soybean_threshold25_bb_version_3 _github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("Argentina_soybean_cum_prec_simulations_github_period2.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main="Cumulative precipitation in Argentina Nov-Mar", xlab="Precipitation (in mm) ", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.0626
# period 2: 0.0689


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_soybean [,6], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.063" , "SE2 = 0.069", c(paste("threshold = ", round(thres_soybean[1,6], digits =2), sep=""))))


ecdfPlot(ART, main=" Cumulative precipitation between November and March in Argentina", xlab="Precipitation (mm) ", ylab="F(x)")
ecdfPlot(ART, add=T, ecdf.col="grey55")

legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))

par(mfrow=c(1,1))

# CDF independent

ARTi <- vector("numeric", rep)
for (i in 1:rep){
  arti = (quantile(normal[,1], probs=runif(1,0,1))* wt_ar[,1] + quantile(normal[,2], probs=runif(1,0,1))* wt_ar[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_ar[,3] 
         + quantile(normal[,4], probs=runif(1,0,1))* wt_ar[,4])
  
  ARTi[i] <- arti
}


## tmax (for version 2) --------

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
tmax <- read.csv("Argentina_soybean_BB_tmax_Jan_Mar.csv", header=T, sep=",")
tmax <- tmax[-1,]


u_ar_tmax <- pobs(tmax[,2:5])
u_ar_tmax <- pobs(tmax[1:24,2:5])
u_ar_tmax <- pobs(tmax[25:46,2:5])

tmax <- tmax[1:24,2:5] #b
tmax <- tmax[25:46,2:5] #c

RVM = RVineStructureSelect(u_ar_tmax,c(1:6),progress=TRUE)

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
rep=10000
simdata_ar_tmax <- RVineSim(rep, RVM)


# marginal distributions


# i=3
# plot(fitdist(tmax[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
# plot(fitdist(tmax[,i],"norm"))
# plot(fitdist(tmax[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# for ( i in 2:5){
#   a <- fitdist(tmax[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(tmax[,i],"norm")
#   c <- fitdist(tmax[,i],"gamma",start=list(a=0,b=1),method="mme")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(c)$aic)
#   # print(summary(a)$bic)
#   # print(summary(b)$bic)
#   # print(summary(c)$bic)
#   }

# normal: 3,4,5 gamma: 2

#a

    # normal <- matrix(nrow=rep, ncol=4)
    # for ( i in 3:5){
    #   Normal<-fitdist(tmax[,i],"norm")
    #   normal[,i-1] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
    # }
    # Gamma<-fitdist(tmax[,2],"gamma", method="mme")
    # normal[,1] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
    # colnames(normal) <- colnames(tmax[,2:5])


# 1967-1990

# 
# i=1
# plot(fitdist(tmax[,i],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(tmax[,i],"norm"))
# plot(fitdist(tmax[,i],"gamma",method="mme"))
# 
# 
# for ( i in 1:4){
#   a <- fitdist(tmax[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(tmax[,i],"norm")
#   c <- fitdist(tmax[,i],"gamma",start=list(a=0,b=1),method="mme")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(c)$aic)
#   # print(summary(a)$bic)
#   # print(summary(b)$bic)
#   # print(summary(c)$bic)
#   }

#normal:  gumbel:1,2,3,4  gamma: 
#b

      # normal <- matrix(nrow=rep, ncol=4)
      # x=c(1:4)
      # for (i in x){
      #   Gumbel<-fitdist(tmax[,i],"gumbel", start=list(a=0,b=1),optim.method="Nelder-Mead")
      #   normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      # }
      # colnames(normal) <- colnames(tmax)


# 1991-2012
# 
# i=4
# plot(fitdist(tmax[,i],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
# plot(fitdist(tmax[,i],"norm"))
# plot(fitdist(tmax[,i],"gamma",start=list(a=0,b=1),method="mme"))
# 
# 
# for ( i in 1:4){
#   a <- fitdist(cum_prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
#   b <- fitdist(cum_prec[,i],"norm")
#   c <- fitdist(cum_prec[,i],"gamma",start=list(a=0,b=1),method="mme")
#   print(summary(a)$aic)
#   print(summary(b)$aic)
#   print(summary(c)$aic)
#   # print(summary(a)$bic)
#   # print(summary(b)$bic)
#   # print(summary(c)$bic)
#   }

#gumbel:2,3,4 , norm= 1  gamma:

      #c 
      
      normal <- matrix(nrow=rep, ncol=4)
      for ( i in 1){
        Normal<-fitdist(tmax[,i],"norm")
        normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      }

      x=c(2,3,4)
      for ( i in x) {
        Gumbel<-fitdist(tmax[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      }
      colnames(normal) <- colnames(tmax)


# CDF
ART2 <- vector("numeric", rep)
for (i in 1:rep){
  art = (quantile(normal[,1], probs=simdata_ar_tmax[i,1])* wt_ar[,1]+ quantile(normal[,2], probs=simdata_ar_tmax[i,2])* wt_ar[,2] + quantile(normal[,3], probs=simdata_ar_tmax[i,3])* wt_ar[,3] 
         + quantile(normal[,4], probs=simdata_ar_tmax[i,4])* wt_ar[,4])
  
  ART2[i] <- art
}
return(ART2)
}

clim <- as.data.frame(clim)
write.table(clim, "Argentina_soybean_tmax_simulations_github_period2.csv", sep=",", row.names=F)


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_soybean <- read.table("soybean_threshold25_bb_version_3 _github_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("Argentina_soybean_tmax_simulations_github_period2.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main="Maximum temperature in Argentina Jan-Mar", xlab="Temperature (in °C) ", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.00051148
# period 2: 0.00069


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_soybean [,7], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.0005" , "SE2 = 0.0007", c(paste("threshold = ", round(thres_soybean[1,7], digits =2), sep=""))))



ecdfPlot(ART, main=" Maximum temperature in °C Argentina", xlab="Temperature in °C", ylab="F(x)")
ecdfPlot(ART, add=T, ecdf.col="grey55")

legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))

par(mfrow=c(1,1))

# CDF independent

ART2i <- vector("numeric", rep)
for (i in 1:rep){
  arti = (quantile(normal[,1], probs=runif(1,0,1))* wt_ar[,1]+ quantile(normal[,2], probs=runif(1,0,1))* wt_ar[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_ar[,3] 
         + quantile(normal[,4], probs=runif(1,0,1))* wt_ar[,4])
  
  ART2i[i] <- arti
}



############### soybean all ----------
## ## ##      ##       ##       ##        ##       ##

####### risks of simultaneous negative climate conditions ###
## version 3 -----

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
ART2 <- read.csv("Argentina_soybean_tmax_simulations_github_period1.csv", header=T, sep=",")
ART <- read.csv("Argentina_soybean_cum_prec_simulations_github_period1.csv", header=T, sep=",")
SI <- read.csv("India_soybean_cum_prec_simulations_github_period1.csv", header=T, sep=",")
SI2 <- read.csv("India_soybean_t30_simulations_github_period1.csv", header=T, sep=",")
CT <- read.csv("China_soybean_t30_simulations_github_period1.csv", header=T, sep=",")
TU <- read.csv("USA_soybean_t30_simulations_github_period1.csv", header=T, sep=",")
PB <- read.csv("Brazil_soybean_cum_prec_simulations_github_period1.csv", header=T, sep=",")



setwd("H:/DPhil/breadbaskets/Data collection")

var <- read.csv("soybean_climate_all_bb_version_3.csv", header=T, sep=",")
require(PerformanceAnalytics)
chart.Correlation (var[,2:8], method="kendall", pch=20, smooth=F,lines=F, main=" soybean climate correlation structures", cex.main=0.9)

u_var <- pobs(var[,2:8])

rep=10000
# extracting correlations between indicators of the same BB
u_var1 <- pobs(var[,3:4])
u_var2 <- pobs(var[,7:8])

a <- BiCopSelect(u_var1[,1], u_var1[,2], familyset=c(1:6))
b <- BiCopSelect(u_var2[,1], u_var2[,2], familyset=c(1:6))


simdata_var1 <- BiCopSim(rep, a$family, a$par)
simdata_var2 <- BiCopSim(rep, b$family, b$par)



setwd("H:/DPhil/breadbaskets/Data collection")
t25 <- read.csv("soybean_threshold25_bb_version_3 _github_weighted.csv", header=T, sep=",")

# uncertainty test:
t25 <- read.csv("soybean_threshold25_prediction_0.9_version3_github_weighted.csv", header=T, sep=",")
t25l<- t25[2,]
t25u<- t25[3,]


RVM = RVineStructureSelect(u_var,c(1:6),progress=TRUE)


clim <- foreach (i=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula")) %dopar% {
  
rep=10000
#simdata_var <- RVineSim(rep, RVM)

####### risks of simultaneous negative climate conditions ###

# version 3
# 
# ALL0 <- matrix(nrow=rep, ncol=7)
# for (n in 1:rep){
#   all0 = c(quantile(CT[,i], probs=simdata_var[n,1]), quantile(SI[,i], probs=simdata_var[n,2]) , quantile(SI2[,i], probs=simdata_var[n,3]) ,quantile(TU[,i], probs=simdata_var[n,4]),
#            quantile(PB[,i], probs=simdata_var[n,5]),quantile(ART[,i], probs=simdata_var[n,6]) ,quantile(ART2[,i], probs=simdata_var[n,7]))
# 
# 
#   ALL0[n,] <- all0
# }
# 
# ALL0 <- as.data.frame(ALL0)



# # version 3 indep BBs
# 
ALL0 <- matrix(nrow=rep, ncol=7)
for (n in 1:rep){
  all0 = c(quantile(CT[,i], probs=runif(1,0,1)), quantile(SI[,i], probs=simdata_var1[n,1]) , quantile(SI2[,i], probs=simdata_var1[n,2]) ,quantile(TU[,i], probs=runif(1,0,1)),
           quantile(PB[,i], probs=runif(1,0,1)),quantile(ART[,i], probs=simdata_var2[n,1]) ,quantile(ART2[,i], probs=simdata_var2[n,2]))


  ALL0[n,] <- all0
}

ALL0 <- as.data.frame(ALL0)

 

# ###

## t25 version 3--
for (l in 1:rep) {
  ALL0$V8[l] <- sum((ifelse(ALL0$V1[l]>t25[1],1,0))[1,],(ifelse(ALL0$V2[l]<t25[2] | ALL0$V3[l]>t25[3],1,0))[1,],(ifelse(ALL0$V4[l]>t25[4],1,0))[1,],
                    (ifelse(ALL0$V5[l]<t25[5],1,0))[1,],(ifelse(ALL0$V6[l]<t25[6] | ALL0$V7[l]>t25[7],1,0))[1,])
 
}
col <- c(colnames(var[,2:8]), "BBs")
colnames(ALL0) <- col

return(ALL0[,8])
}

write.table(clim, "soybean_all_version3_t25_period1_github_weighted_indep.csv", sep=",", row.names = F)
write.table(clim, "soybean_all_version3_t25_lowrisk_prediction_90_period2_github_weighted.csv", sep=",", row.names = F)






# significant change between 2 periods?? 

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")

period1 <- read.csv("soybean_all_version3_t25_period1_github_weighted.csv", sep=",", header=T)
period2 <- read.csv("soybean_all_version3_t25_period2_github_weighted.csv", sep=",", header=T)

# threshold uncertainty analysis:
period1 <- read.csv("soybean_all_version3_t25_lowrisk_prediction_90_period1_github_weighted.csv", sep=",", header=T)
period2 <- read.csv("soybean_all_version3_t25_lowrisk_prediction_90_period2_github_weighted.csv", sep=",", header=T)


period1_mean <- apply(period1,2,sort)
period1_mean <- apply(period1_mean, 1, mean)
period2_mean <- apply(period2, 2, sort)
period2_mean <- apply(period2_mean, 1, mean)

period1i <- read.csv("soybean_all_version3_t25_period1_github_weighted_indep.csv", sep=",", header=T)
period2i <- read.csv("soybean_all_version3_t25_period2_github_weighted_indep.csv", sep=",", header=T)

period1i_mean <- apply(period1i,2,sort)
period1i_mean <- apply(period1i_mean, 1, mean)
period2i_mean <- apply(period2i, 2, sort)
period2i_mean <- apply(period2i_mean, 1, mean)


mean(period2_mean)- mean(period1_mean)

# Wilcoxon (or Mann-Whitney-Wilcoxon) test -> assumes normal distribution for large sample numbers
wilcox.test(period1_mean, period1i_mean, alternative = "less")
wilcox.test(period2_mean, period2i_mean)

# -> p=0; p<=0.05 or 0.001 means populations are nonidentical

# between dependencies:
# Wilcoxon (or Mann-Whitney-Wilcoxon) test -> assumes normal distribution
wilcox.test(period2_mean, period2i_mean)
wilcox.test(period1_mean, period1i_mean)

# between periods:
# Wilcoxon (or Mann-Whitney-Wilcoxon) test -> assumes normal distribution
wilcox.test(period1i_mean, period2i_mean, alternative = "greater")

# each case separate

pvalue <- matrix (nrow=6, ncol=1)

for (i in 0:5){
  pois <-poisson.test(c(sum(period1==i),	sum(period1i==i)), c(10000000, 10000000), alternative = c("two.sided"))
  pvalue[i+1,1] <- pois$p.value
}



# -> period 1: all pvalues p< 0.001! significant difference between all cases
# -> period 2: all pvalues p< 0.001! significant difference between all cases



# change of simultabeous risks between two periods
setwd("H:/DPhil/breadbaskets/Data collection")
t25 <- read.csv("soybean_threshold25_bb_version_3 _github_weighted.csv", header=T, sep=",")

ALL0<- clim
setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
ALL0 <- read.csv("soybean_all_version3_t25_period1_github_weighted.csv", sep=",", header=T)
ALL0 <- read.csv("soybean_all_version3_t25_lowrisk_prediction_90_period1_github_weighted.csv", sep=",", header=T)

#indep
ALL0 <- read.csv("soybean_all_version3_t25_period1_github_weighted_indep.csv", sep=",", header=T)

ALL0_perc <- matrix(nrow=1000, ncol=6)
#period1
for (o in 1:1000){
  for (i in 0:5){
    ALL0_perc[o,i+1]<- sum(ALL0[,o]==i)/10000
  }}

m1 <- apply(ALL0_perc,2,mean)
s1 <- apply(ALL0_perc,2,sd)


ALL0 <- read.csv("soybean_all_version3_t25_period2_github_weighted.csv", sep=",", header=T)
ALL0 <- read.csv("soybean_all_version3_t25_lowrisk_prediction_90_period2_github_weighted.csv", sep=",", header=T)

# indep
ALL0 <- read.csv("soybean_all_version3_t25_period2_github_weighted_indep.csv", sep=",", header=T)

ALL0_perc <- matrix(nrow=1000, ncol=6)

#period2
for (o in 1:1000){
  for (i in 0:5){
    ALL0_perc[o,i+1]<- sum(ALL0[,o]==i)/10000
  }}

m2 <- apply(ALL0_perc,2,mean)
s2 <- apply(ALL0_perc,2,sd)

stats<- rbind(m1,s1,m2,s2)

write.table(stats, "soybean_global_simultaneous_pdf_25_version_3_github_weighted_indep.csv", sep=",", row.names=c("mean_period1","sd_period1",  "mean_period2","sd_period2"))
write.table(stats, "soybean_global_simultaneous_pdf_25_version_3_lowrisk_prediction_90_github_weighted.csv", sep=",", #row.names=c("var" ,"mean_period1","sd_period1",  "mean_period2","sd_period2"),
            col.names=c(0:5))


## portion below 25% threshold  ---------

# change of risks between two periods
setwd("H:/DPhil/breadbaskets/Data collection")
t25 <- read.csv("soybean_threshold25_bb_version_3 _github_weighted.csv", header=T, sep=",")


t25_prop_soybean_bb <- matrix(ncol=7, nrow=1000)

for ( i in 1:1000){
  t25_prop_soybean_bb[i,1] <- length( which( CT[,i] >t25[1][1,] ) )/10000
  t25_prop_soybean_bb[i,2] <- length( which( SI[,i]<t25[2][1,] ) )/10000
  t25_prop_soybean_bb[i,3] <- length( which( SI2[,i]>t25[3][1,] ) )/10000
  t25_prop_soybean_bb[i,4] <- length( which( TU[,i]>t25[4][1,] ) )/10000
  t25_prop_soybean_bb[i,5] <- length( which( PB[,i]<t25[5][1,] ) )/10000
  t25_prop_soybean_bb[i,6] <- length( which( ART[,i]<t25[6][1,] ) )/10000
  t25_prop_soybean_bb[i,7] <- length( which( ART2[,i]>t25[7][1,] ) )/10000
   
}

colnames(t25_prop_soybean_bb) <- colnames(t25)
write.table(t25_prop_soybean_bb, "proportion_soybean_25_global_bb_period2_github.csv", sep=",", row.names=F)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
prop1_25 <- read.table("proportion_soybean_25_global_bb_period1_github.csv", sep=",", header=T)
prop2_25 <- read.table("proportion_soybean_25_global_bb_period2_github.csv", sep=",", header=T)

diff_25 <- prop2_25 - prop1_25
diff_25_stats <- matrix(ncol=7, nrow=2)
diff_25_stats[1,] <- apply(diff_25, 2, mean)
diff_25_stats[2,] <- apply(diff_25, 2, sd)
colnames(diff_25_stats) <- colnames(prop2_25)
write.table(diff_25_stats, "proportion_diff_stats_soybean_25_global_bb_github.csv", sep=",", row.names=F)




### ## ## ### ## # # ## ## ## # # ## ## #
### RICE #######################################


setwd("H:/DPhil/breadbaskets/Data collection")
wt <- read.table("rice_average_area_weight.csv", sep=",", header=T)
wt_in <- wt[,12:21]
wt_ch <- wt[,1:11]
wt_ind <- wt[,22:26]


### China ########
### Solar radiation --------

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
sr <- read.csv("China_rice_BB_dswr_Feb_Nov.csv", header=T, sep=",")
sr<- sr[-1,]

u_ch_sr <- pobs(sr[1:24,]) #b
u_ch_sr <- pobs(sr[25:46,]) #c
u_ch_sr <- pobs(sr) #a

RVM = RVineStructureSelect(u_ch_sr,c(1:6),progress=TRUE)

sr <- sr[25:46,] #c
sr <- sr[1:24,] #b

clim <- foreach (i=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
  rep=10000
  simdata_ch_sr <- RVineSim(rep, RVM)
  
  
  # marginal distributions
  
  
  # plot(fitdist(sr[,7],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
  # plot(fitdist(sr[,7],"norm"))
  # 
  # 
  #
  # for ( i in 1:11){
  #   a <- fitdist(sr[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #   b <- fitdist(sr[,i],"norm")
  #   c <- fitdist(sr[,i],"gamma",start=list(a=0,b=1),method="mme")
  #   print(summary(a)$aic)
  #   print(summary(b)$aic)
  #   print(summary(c)$aic)
  #   # print(summary(a)$bic)
  #   # print(summary(b)$bic)
  #   # print(summary(c)$bic)
  # }
  
  # #norm for 1,5,8,10,11  gumbel: 4,7  gamma: 2,3,6,9
  
  #a 
  # 
  
            # normal <- matrix(nrow=rep, ncol=11)
            # x = c(4,7)
            # for ( i in x){
            #   Gumbel<-fitdist(sr[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
            #   normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
            # }
            # t=c(1,5,8,10,11)
            # for (i in t){
            # Normal<-fitdist(sr[,i],"norm")
            # normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
            # }
            # l=c(2,3,6,9)
            # for (i in l){
            #   Gamma<-fitdist(sr[,i],"gamma", method="mme")
            #   normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
            # }
            # colnames(normal) <- colnames(sr[,1:11])

  
  #### 1967-1990
  
  
  # plot(fitdist(sr[,7],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
  # plot(fitdist(sr[,7],"norm"))
  # # 
  #           for ( i in 1:11){
  #             a <- fitdist(sr[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #             b <- fitdist(sr[,i],"norm")
  #             c <- fitdist(sr[,i],"gamma",start=list(a=0,b=1),method="mme")
  #             print(summary(a)$aic)
  #             print(summary(b)$aic)
  #             print(summary(c)$aic)
  #             # print(summary(a)$bic)
  #             # print(summary(b)$bic)
  #             # print(summary(c)$bic)
  #           }
  # 
  
  #gumbel: 4,7 , normal: 1,5,8,10,11  gamma: 1,6,8,10,11
  
  #b
  
            
            normal <- matrix(nrow=rep, ncol=11)
            x = c(4,7)
            for ( i in x){
              Gumbel<-fitdist(sr[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
              normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
            }
            t=c(1,5,8,10,11)
            for (i in t){
              Normal<-fitdist(sr[,i],"norm")
              normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
            }
            l=c(2,3,6,9)
            for (i in l){
              Gamma<-fitdist(sr[,i],"gamma", method="mme")
              normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
            }
            colnames(normal) <- colnames(sr[,1:11])
  
  
  # 1991-2012
  

  # plot(fitdist(sr[,1],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
  #  plot(fitdist(sr[,1],"norm"))
  #  plot(fitdist(sr[,1],"gamma",start=list(a=0,b=1),method="mme"))
  #  plot(fitdist(sr[,1],"unif", method="mle"))
  #  
  # # 
  #           
  # for ( i in 1:11){
  #   a <- fitdist(sr[,i],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead")
  #   b <- fitdist(sr[,i],"norm")
  #   c <- fitdist(sr[,i],"gamma",start=list(a=0,b=1),method="mme")
  #   d <-fitdist(sr[,i], "unif", method="mle")
  #   print(summary(a)$aic)
  #   print(summary(b)$aic)
  #   print(summary(c)$aic)
  #   # print(summary(a)$bic)
  #   # print(summary(b)$bic)
  #   # print(summary(c)$bic)
  # }
  # 
  # #gumbel: 4,7 , normal: 1,5,6, 8,10,11  gamma: 2,3,9
  # 
  # #c
  # 
  #           
  #           normal <- matrix(nrow=rep, ncol=11)
  #           x = c(4,7)
  #           for ( i in x){
  #             Gumbel<-fitdist(sr[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #             normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
  #           }
  #           t=c(1,5,6,8,10,11)
  #           for (i in t){
  #             Normal<-fitdist(sr[,i],"norm")
  #             normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
  #           }
  #           l=c(2,3,9)
  #           for (i in l){
  #             Gamma<-fitdist(sr[,i],"gamma", method="mme")
  #             normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
  #           }
  #           colnames(normal) <- colnames(sr[,1:11])
  # 
  
  # CDF
  SRC <- vector("numeric", rep)
  for (i in 1:rep){
    src = (quantile(normal[,1], probs=simdata_ch_sr[i,1])* wt_ch[,1]+ quantile(normal[,2], probs=simdata_ch_sr[i,2])* wt_ch[,2] + quantile(normal[,3], probs=simdata_ch_sr[i,3])* wt_ch[,3] 
          + quantile(normal[,4], probs=simdata_ch_sr[i,4])* wt_ch[,4]+ quantile(normal[,5], probs=simdata_ch_sr[i,5])* wt_ch[,5]
          + quantile(normal[,6], probs=simdata_ch_sr[i,6])* wt_ch[,6]+ quantile(normal[,7], probs=simdata_ch_sr[i,7])* wt_ch[,7]
          + quantile(normal[,8], probs=simdata_ch_sr[i,8])* wt_ch[,8]+ quantile(normal[,9], probs=simdata_ch_sr[i,9])* wt_ch[,9]
          + quantile(normal[,10], probs=simdata_ch_sr[i,10])* wt_ch[,6]+ quantile(normal[,11], probs=simdata_ch_sr[i,11])* wt_ch[,11]
    )
    
    SRC[i] <- src 
  }
  
  return(SRC)
}

clim <- as.data.frame(clim)
write.table(clim, "China_rice_dswr_simulations_github_period1.csv", sep=",", row.names=F)

# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_rice <- read.table("rice_threshold25_bb_github_version_3_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("China_rice_dswr_simulations_github_period2.csv", header=T, sep=",")


# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main="Solar radiation in China Feb-Nov ", xlab="Solar radiation (in kW/m2) ", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.00254
# period 2: 0.00343


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_rice [,1], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.003" , "SE2 = 0.003", c(paste("threshold = ", round(thres_rice[1,1], digits =2), sep=""))))


# 
# ecdfPlot(PC, main=" Feb- Nov Solar Radiation China", xlab="Solar radiation in W/m2", ylab="F(x)")
# ecdfPlot(PC, add=T, ecdf.col="grey55")
# 



# CDF independent
SRCi <- vector("numeric", rep)
for (i in 1:rep){
  srci = (quantile(normal[,1], probs=runif(1,0,1))* wt_ch[,1]+ quantile(normal[,2], probs=runif(1,0,1))* wt_ch[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_ch[,3] 
         + quantile(normal[,4], probs=runif(1,0,1))* wt_ch[,4]+ quantile(normal[,5], probs=runif(1,0,1))* wt_ch[,5]
         + quantile(normal[,6], probs=runif(1,0,1))* wt_ch[,6]+ quantile(normal[,7], probs=runif(1,0,1))* wt_ch[,7]
  )
  
  SRCi[i] <- srci
}



####### India ##########

### Solar radiation ---------

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
sr <- read.csv("India_rice_BB_dswr_Jun_Sep.csv", header=T, sep=",")
sr<- sr[-1,]


u_in_sr <- pobs(sr[1:24,]) #b
u_in_sr <- pobs(sr[25:46,]) #c
u_in_sr <- pobs(sr) #a

RVM = RVineStructureSelect(u_in_sr,c(1:6),progress=TRUE)

sr <- sr[25:46,] #c
sr <- sr[1:24,] #b


clim <- foreach (i=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
  rep=10000
  simdata_in_sr <- RVineSim(rep, RVM)
  
  
  # marginal distributions
  
  # 
  #  plot(fitdist(sr[,7],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
  #  plot(fitdist(sr[,7],"norm"))
  # # 
  # # 
  # # 
  # for ( i in 1:10){
  #   a <- fitdist(sr[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #   b <- fitdist(sr[,i],"norm")
  #   c <- fitdist(sr[,i],"gamma",start=list(a=0,b=1),method="mme")
  #   print(summary(a)$aic)
  #   print(summary(b)$aic)
  #   print(summary(c)$aic)
  #   # print(summary(a)$bic)
  #   # print(summary(b)$bic)
  #   # print(summary(c)$bic)
  # }
  
  # #gamma: 2,3,4,5,7,10  gumbel: 1,6,8   norm: 9
  
  #a 
  
      # normal <- matrix(nrow=rep, ncol=10)
      # x = c(1,6,8)
      # for ( i in x){
      #   Gumbel<-fitdist(sr[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
      #   normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      # }
      # t=c(9)
      # for (i in t){
      #   Normal<-fitdist(sr[,i],"norm")
      #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # l=c(2,3,4,5,7,10)
      # for (i in l){
      #   Gamma<-fitdist(sr[,i],"gamma", method="mme")
      #   normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      # }
      # colnames(normal) <- colnames(sr[,1:10])

  
  #### 1967-1990
  
  # # i=5
  # # plot(fitdist(sr[,5],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
  # # plot(fitdist(sr[,5],"norm"))
  # # # 
  # for ( i in 1:10){
  #   a <- fitdist(sr[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #   b <- fitdist(sr[,i],"norm")
  #   c <- fitdist(sr[,i],"gamma",start=list(a=0,b=1),method="mme")
  #   print(summary(a)$aic)
  #   print(summary(b)$aic)
  #   print(summary(c)$aic)
  #   # print(summary(a)$bic)
  #   # print(summary(b)$bic)
  #   # print(summary(c)$bic)
  # }
  # 
  #gumbel:1,3,4,5,6,7,8 , gamma: 2,9,10
  
  #b
  
     #  normal <- matrix(nrow=rep, ncol=10)
     #  l=c(2,9,10)
     #  for (i in l){
     #    Gamma<-fitdist(sr[,i],"gamma", method="mme")
     #    normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
     #  }
     #  x = c(1,3,4,5,6,7,8)
     #  for ( i in x){
     #    Gumbel<-fitdist(sr[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
     #    normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
     #  }
     # colnames(normal) <- colnames(sr[,1:10])
  
   # mean <- apply(normal,2,mean)
   # mean(mean)
  
  # 1991-2012
  
  
  # # plot(fitdist(sr[,7],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
  # # plot(fitdist(sr[,7],"norm"))
  # # 
  # for ( i in 1:10){
  #   a <- fitdist(sr[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #   b <- fitdist(sr[,i],"norm")
  #   c <- fitdist(sr[,i],"gamma",start=list(a=0,b=1),method="mme")
  #   print(summary(a)$aic)
  #   print(summary(b)$aic)
  #   print(summary(c)$aic)
  #   # print(summary(a)$bic)
  #   # print(summary(b)$bic)
  #   # print(summary(c)$bic)
  # }
  
  #gumbel: 1,2,5,6,8  normal:4,7,9   gamma:3,10 
  
  #c

      normal <- matrix(nrow=rep, ncol=10)
      l=c(3,10)
      for (i in l){
        Gamma<-fitdist(sr[,i],"gamma", method="mme")
        normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      }
      x = c(1,2,5,6,8)
      for ( i in x){
        Gumbel<-fitdist(sr[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      }
      t=c(4,7,9)
      for (i in t){
        Normal<-fitdist(sr[,i],"norm")
        normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      colnames(normal) <- colnames(sr[,1:10])
      }


  # CDF
  SRIn <- vector("numeric", rep)
  for (i in 1:rep){
    srin = ((quantile(normal[,1], probs=simdata_in_sr[i,1])* wt_in[,1]) + (quantile(normal[,2], probs=simdata_in_sr[i,2])* wt_in[,2]) + (quantile(normal[,3], probs=simdata_in_sr[i,3])* wt_in[,3]) 
           + (quantile(normal[,4], probs=simdata_in_sr[i,4])* wt_in[,4])+ (quantile(normal[,5], probs=simdata_in_sr[i,5])* wt_in[,5])
           + (quantile(normal[,6], probs=simdata_in_sr[i,6])* wt_in[,6])+ (quantile(normal[,7], probs=simdata_in_sr[i,7])* wt_in[,7])
           + (quantile(normal[,8], probs=simdata_in_sr[i,8])* wt_in[,8])+ (quantile(normal[,9], probs=simdata_in_sr[i,9])* wt_in[,9])
           + (quantile(normal[,10], probs=simdata_in_sr[i,10])* wt_in[,10]))
    
    SRIn[i] <- srin 
  }
  
  # SRIn <- vector("numeric", rep)
  # for (i in 1:rep){
  #   srin = (quantile(normal[,1], probs=simdata_in_sr[i,1]) + quantile(normal[,2], probs=simdata_in_sr[i,2]) + quantile(normal[,3], probs=simdata_in_sr[i,3])
  #           + quantile(normal[,4], probs=simdata_in_sr[i,4])+ quantile(normal[,5], probs=simdata_in_sr[i,5])
  #           + quantile(normal[,6], probs=simdata_in_sr[i,6])+ quantile(normal[,7], probs=simdata_in_sr[i,7])
  #           + quantile(normal[,8], probs=simdata_in_sr[i,8])+ quantile(normal[,9], probs=simdata_in_sr[i,9])
  #           + quantile(normal[,10], probs=simdata_in_sr[i,10]))/10
  #   
  #   SRIn[i] <- srin 
  # }
  
  return(SRIn)
}

clim <- as.data.frame(clim)
write.table(clim, "India_rice_dswr_simulations_github_period2.csv", sep=",", row.names=F)


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_rice <- read.table("rice_threshold25_bb_github_version_3_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("India_rice_dswr_simulations_github_period2.csv", header=T, sep=",")
hist(clim[,1], add=T, col="yellow", freq=F)

# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main="Solar radiation in India Jun-Sep ", xlab="Solar radiation (in kW/m2) ", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.004356
# period 2: 0.003974


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_rice [,4], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.004" , "SE2 = 0.004", c(paste("threshold = ", round(thres_rice[1,4], digits =2), sep=""))))



# ecdfPlot(sr, main="Solar Radiation India", xlab="Solar radiation in W/m2", ylab="F(x)")
sr <- apply(sr,1,mean)
ecdfPlot(SRIn, add=T, ecdf.col="red")




# CDF independent
SRIni <- vector("numeric", rep)
for (i in 1:rep){
  srini = (quantile(normal[,1], probs=runif(1,0,1))* wt_in[,1]+ quantile(normal[,2], probs=runif(1,0,1))* wt_in[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_in[,3] 
          + quantile(normal[,4], probs=runif(1,0,1))* wt_in[,4]+ quantile(normal[,5], probs=runif(1,0,1))* wt_in[,5]
          + quantile(normal[,6], probs=runif(1,0,1))* wt_in[,6]+ quantile(normal[,7], probs=runif(1,0,1))* wt_in[,7]
          + quantile(normal[,8], probs=runif(1,0,1))* wt_in[,8]+ quantile(normal[,9], probs=runif(1,0,1))* wt_in[,9]
          + quantile(normal[,10], probs=runif(1,0,1))* wt_in[,10]
  )
  
  SRIni[i] <- srini
}



##### Precipitation ---------

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
prec <- read.csv("India_rice_BB_cum_prec_Jun_Sep.csv", header=T, sep=",")
prec<- prec[-1,]


u_in_prec <- pobs(prec[1:24,]) #b
u_in_prec <- pobs(prec[25:46,]) #c
u_in_prec <- pobs(prec) #a

RVM = RVineStructureSelect(u_in_prec,c(1:6),progress=TRUE)

prec <- prec[25:46,] #c
prec <- prec[1:24,] #b

clim <- foreach (i=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
  rep=10000
  simdata_in_prec <- RVineSim(rep, RVM)
  
  
  # marginal distributions
  
  
  # plot(fitdist(prec[,7],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
  # plot(fitdist(prec[,7],"norm"))
  # 
  # 
  # # 
  # for ( i in 1:10){
  #   a <- fitdist(prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #   b <- fitdist(prec[,i],"norm")
  #   c <- fitdist(prec[,i],"gamma",start=list(a=0,b=1),method="mme")
  #   print(summary(a)$aic)
  #   print(summary(b)$aic)
  #   print(summary(c)$aic)
  #   # print(summary(a)$bic)
  #   # print(summary(b)$bic)
  #   # print(summary(c)$bic)
  # }
  
  # #gamma: 2,8  gumbel: 1,10   norm: 3,4,5,6,7,9
  
  #a 
  
      # normal <- matrix(nrow=rep, ncol=10)
      # x = c(1,10)
      # for ( i in x){
      #   Gumbel<-fitdist(prec[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
      #   normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      # }
      # t=c(3,4,5,6,7,9)
      # for (i in t){
      #   Normal<-fitdist(prec[,i],"norm")
      #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # l=c(2,8)
      # for (i in l){
      #   Gamma<-fitdist(prec[,i],"gamma", method="mme")
      #   normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      # }
      # colnames(normal) <- colnames(prec[,1:10])
  
  
  #### 1967-1990
  
  # i=5
  # plot(fitdist(prec[,5],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
  # plot(fitdist(prec[,5],"norm"))
  # # 
  # for ( i in 1:10){
  #   a <- fitdist(prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #   b <- fitdist(prec[,i],"norm")
  #   c <- fitdist(prec[,i],"gamma",start=list(a=0,b=1),method="mme")
  #   print(summary(a)$aic)
  #   print(summary(b)$aic)
  #   print(summary(c)$aic)
  #   # print(summary(a)$bic)
  #   # print(summary(b)$bic)
  #   # print(summary(c)$bic)
  # }
  
  #gumbel:1,8,10 , gamma: 2,5 norm: 3,4,6,7,9
  
  #b
  
      # normal <- matrix(nrow=rep, ncol=10)
      # l=c(2,5)
      # for (i in l){
      #   Gamma<-fitdist(prec[,i],"gamma", method="mme")
      #   normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      # }
      # t=c(3,4,6,7,9)
      # for (i in t){
      #   Normal<-fitdist(prec[,i],"norm")
      #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # x = c(1,8,10)
      # for ( i in x){
      #   Gumbel<-fitdist(prec[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
      #   normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      # }
      # colnames(normal) <- colnames(prec[,1:10])
      
  
  # 1991-2012
  
  
  # # plot(fitdist(prec[,7],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
  # # plot(fitdist(prec[,7],"norm"))
  # # 
  # for ( i in 1:10){
  #   a <- fitdist(prec[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #   b <- fitdist(prec[,i],"norm")
  #   c <- fitdist(prec[,i],"gamma",start=list(a=0,b=1),method="mme")
  #   print(summary(a)$aic)
  #   print(summary(b)$aic)
  #   print(summary(c)$aic)
  #   # print(summary(a)$bic)
  #   # print(summary(b)$bic)
  #   # print(summary(c)$bic)
  # }
  # 
  # #gumbel: 1,6,10  normal:3,4,5,9   gamma:2,7,8
  # 
  # #c
  # 
      normal <- matrix(nrow=rep, ncol=10)
      l=c(2,7,8)
      for (i in l){
        Gamma<-fitdist(prec[,i],"gamma", method="mme")
        normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      }
      x = c(1,6,10)
      for ( i in x){
        Gumbel<-fitdist(prec[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      }
      t=c(3,4,5,9)
      for (i in t){
        Normal<-fitdist(prec[,i],"norm")
        normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
        colnames(normal) <- colnames(prec[,1:10])
      }

  # 
      
  # CDF
  PIn <- vector("numeric", rep)
  for (i in 1:rep){
    pin = (quantile(normal[,1], probs=simdata_in_prec[i,1])* wt_in[,1] + quantile(normal[,2], probs=simdata_in_prec[i,2])* wt_in[,2] + quantile(normal[,3], probs=simdata_in_prec[i,3])* wt_in[,3] 
            + quantile(normal[,4], probs=simdata_in_prec[i,4])* wt_in[,4]+ quantile(normal[,5], probs=simdata_in_prec[i,5])* wt_in[,5]
            + quantile(normal[,6], probs=simdata_in_prec[i,6])* wt_in[,6]+ quantile(normal[,7], probs=simdata_in_prec[i,7])* wt_in[,7]
            + quantile(normal[,8], probs=simdata_in_prec[i,8])* wt_in[,8]+ quantile(normal[,9], probs=simdata_in_prec[i,9])* wt_in[,9]
            + quantile(normal[,10], probs=simdata_in_prec[i,10])* wt_in[,10])
    
    PIn[i] <- pin 
  }
  
  return(PIn)
}

clim <- as.data.frame(clim)
write.table(clim, "India_rice_cum_prec_simulations_github_period2.csv", sep=",", row.names=F)


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_rice <- read.table("rice_threshold25_bb_github_version_3_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("India_rice_cum_prec_simulations_github_period1.csv", header=T, sep=",")

# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main="Cumulative precipitation in India Jun-Sep ", xlab="Precipitation (in mm) ", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.05211 
# period 2: 0.046619


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_rice [,3], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.052" , "SE2 = 0.047", c(paste("threshold = ", round(thres_rice[1,3], digits =2), sep=""))))


# ecdfPlot(PIn, main="Precipitation India", xlab="Precipitation in mm", ylab="F(x)")
# ecdfPlot(PIn, add=T, ecdf.col="grey55")




# CDF independent
PIni <- vector("numeric", rep)
for (i in 1:rep){
  pini = (quantile(normal[,1], probs=runif(1,0,1))* wt_in[,1]+ quantile(normal[,2], probs=runif(1,0,1))* wt_in[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_in[,3] 
           + quantile(normal[,4], probs=runif(1,0,1))* wt_in[,4]+ quantile(normal[,5], probs=runif(1,0,1))* wt_in[,5]
           + quantile(normal[,6], probs=runif(1,0,1))* wt_in[,6]+ quantile(normal[,7], probs=runif(1,0,1))* wt_in[,7]
           + quantile(normal[,8], probs=runif(1,0,1))* wt_in[,8]+ quantile(normal[,9], probs=runif(1,0,1))* wt_in[,9]
           + quantile(normal[,10], probs=runif(1,0,1))* wt_in[,10]
  )
  
  PIni[i] <- pini
}



##### Tmax -----------

# tmax
setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
tmax <- read.csv("India_rice_BB_tmax_Jun_Sep.csv", header=T, sep=",")
tmax<- tmax[-1,]

u_in_tmax <- pobs(tmax[1:24,])  #b
u_in_tmax <- pobs(tmax[25:46,]) #c
u_in_tmax <- pobs(tmax) #a

RVM = RVineStructureSelect(u_in_tmax,c(1:6),progress=TRUE)

tmax<- tmax[1:24,] #b
tmax<- tmax[25:46,] #c

clim <- foreach (z=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
  rep=10000
  simdata_in_tmax <- RVineSim(rep, RVM)
  
  
  # marginal distributions
  # 
  # # 
  # plot(fitdist(tmax[,7],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead"))
  # plot(fitdist(tmax[,7],"norm"))
  # plot(fitdist(tmax[,7],"gamma",start=list(a=0,b=1),method="mme"))
  # 
  # 
  # for ( i in 1:10){
  #     a <- fitdist(tmax[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #     b <- fitdist(tmax[,i],"norm")
  #     c <- fitdist(tmax[,i],"gamma",start=list(a=0,b=1),method="mme")
  #     print(summary(a)$aic)
  #     print(summary(b)$aic)
  #     print(summary(c)$aic)
  #     # print(summary(a)$bic)
  #     # print(summary(b)$bic)
  #     # print(summary(c)$bic)
  #     }
  # 
  #norm 3,5,7,8,9,10 , gamma:1,4  gumbel: 2,6
  
  #a
  
      # normal <- matrix(nrow=rep, ncol=10)
      # x = c(3,5,7,8,9,10)
      # 
      # for ( i in x){
      #   Normal <-fitdist(tmax[,i],"norm")
      #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # l=c(1,4)
      # for (i in l){
      #           Gamma<-fitdist(tmax[,i],"gamma", method="mme")
      #           normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      # }
      # x = c(2,6)
      # for ( i in x){
      #   Gumbel<-fitdist(tmax[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
      #   normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      # }
      # colnames(normal) <- colnames(tmax)

  
  #### 1967-1990
  
  
  
  # plot(fitdist(tmax[,7],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
  # plot(fitdist(tmax[,7],"norm"))
  # 
  # # 
  # # 
  #     for ( i in 1:10){
  #     a <- fitdist(tmax[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #     b <- fitdist(tmax[,i],"norm")
  #     c <- fitdist(tmax[,i],"gamma",start=list(a=0,b=1),method="mme")
  #     print(summary(a)$aic)
  #     print(summary(b)$aic)
  #     print(summary(c)$aic)
  #     # print(summary(a)$bic)
  #     # print(summary(b)$bic)
  #     # print(summary(c)$bic)
  #     }

  #norm 2  gamma: 3,4,5,6,7,8,9,10  gumbel: 1
  
  #b
      # 
      normal <- matrix(nrow=rep, ncol=10)
      x = c(2)
      for ( i in x){
        Normal <-fitdist(tmax[,i],"norm")
        normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      }
      l=c(3:10)
      for (i in l){
        Gamma<-fitdist(tmax[,i],"gamma", method="mme")
        normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      }
      x = c(1)
      for ( i in x){
        Gumbel<-fitdist(tmax[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      }
      colnames(normal) <- colnames(tmax)

  
      # 1991-2012
  
  
  # #plot(fitdist(tmax[,7],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
  # #plot(fitdist(tmax[,7],"norm"))
  # # # 
  #     for ( i in 1:10){
  #       a <- fitdist(tmax[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #       b <- fitdist(tmax[,i],"norm")
  #       c <- fitdist(tmax[,i],"gamma",start=list(a=0,b=1),method="mme")
  #       print(summary(a)$aic)
  #       print(summary(b)$aic)
  #       print(summary(c)$aic)
  #       # print(summary(a)$bic)
  #       # print(summary(b)$bic)
  #       # print(summary(c)$bic)
  #     }
  
  #norm:1,5,7,10  gamma:3,4,6,9  gumbel: 2,8
  
  #c 
  
      
      # normal <- matrix(nrow=rep, ncol=10)
      # x = c(1,5,7,10)
      # for ( i in x){
      #   Normal <-fitdist(tmax[,i],"norm")
      #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # l=c(3,4,6,9)
      # for (i in l){
      #   Gamma<-fitdist(tmax[,i],"gamma", method="mme")
      #   normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      # }
      # x = c(2,8)
      # for ( i in x){
      #   Gumbel<-fitdist(tmax[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
      #   normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      # }
      # colnames(normal) <- colnames(tmax)

  
  # CDF
  SI <- vector("numeric", rep)
  for (i in 1:rep){
    si = (quantile(normal[,1], probs=simdata_in_tmax[i,1])* wt_in[,1] + quantile(normal[,2], probs=simdata_in_tmax[i,2])* wt_in[,2] + quantile(normal[,3], probs=simdata_in_tmax[i,3])* wt_in[,3] 
          + quantile(normal[,4], probs=simdata_in_tmax[i,4])* wt_in[,4] + quantile(normal[,5], probs=simdata_in_tmax[i,5])* wt_in[,5]+  quantile(normal[,6], probs=simdata_in_tmax[i,6])* wt_in[,6]
          + quantile(normal[,7], probs=simdata_in_tmax[i,7])* wt_in[,7]+ quantile(normal[,8], probs=simdata_in_tmax[i,8])* wt_in[,8]+  quantile(normal[,9], probs=simdata_in_tmax[i,9])* wt_in[,9]
          + quantile(normal[,10], probs=simdata_in_tmax[i,10])* wt_in[,10])
    
    SI[i] <- si 
  }
  return(SI)
}

clim <- as.data.frame(clim)
write.table(clim, "India_rice_tmax_simulations_github_period1.csv", sep=",", row.names=F)


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_rice <- read.table("rice_threshold25_bb_github_version_3_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("India_rice_tmax_simulations_github_period2.csv", header=T, sep=",")

# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main=" Maximum temperature in India Jun-Sep", xlab="Maximum temperature (in °C) ", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.0002429
# period 2: 0.000161


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_rice [,2], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.00024" , "SE2 = 0.00016", c(paste("threshold = ", round(thres_rice[1,2], digits =2), sep=""))))


# 
# ecdfPlot(SI, main=" Max temperature in India", xlab="Max temperature", ylab="F(x)")
# ecdfPlot(SI, main=" Max temperature in India", xlab="Max temperature", ylab="F(x)")
# ecdfPlot(SI, add=T, ecdf.col="grey55")
# 



# CDF independent

SIi <- vector("numeric", rep)
for (i in 1:rep){
  sii = (quantile(normal[,1], probs=runif(1,0,1))* wt_in[,1]+ quantile(normal[,2], probs=runif(1,0,1))* wt_in[,2] + quantile(normal[,3], probs=runif(1,0,1)) * wt_in[,3]
         + quantile(normal[,4], probs=runif(1,0,1))* wt_in[,4]+ quantile(normal[,5], probs=runif(1,0,1))* wt_in[,5]+  quantile(normal[,6], probs=runif(1,0,1))* wt_in[,6]
         + quantile(normal[,7], probs=runif(1,0,1))* wt_in[,7]+ quantile(normal[,8], probs=runif(1,0,1))* wt_in[,8]+  quantile(normal[,9], probs=runif(1,0,1))* wt_in[,9]
         + quantile(normal[,10], probs=runif(1,0,1))* wt_in[,10])
  
  SIi[i] <- sii 
}



############Indonesia ######

### Solar radiation ---------

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
sr <- read.csv("Indonesia_rice_BB_dswr_Nov_Feb_geographic_units_weighted.csv", header=T, sep=",")
sr<- sr[,-1]


u_ind_sr <- pobs(sr[1:24,]) #b
u_ind_sr <- pobs(sr[25:46,]) #c
u_ind_sr <- pobs(sr) #a
RVM = RVineStructureSelect(u_ind_sr,c(1:6),progress=TRUE)

sr <- sr[25:46,] #c
sr <- sr[1:24,] #b

clim <- foreach (i=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula", "fitdistrplus", "gumbel")) %dopar% {
  
  rep=10000
  simdata_ind_sr <- RVineSim(rep, RVM)
  
  
  # marginal distributions
  
  
  # plot(fitdist(sr[,7],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
  # plot(fitdist(sr[,7],"norm"))
  # 
  # 
  # # 
  # for ( i in 1:5){
  #         a <- fitdist(sr[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #         b <- fitdist(sr[,i],"norm")
  #         c <- fitdist(sr[,i],"gamma",start=list(a=0,b=1),method="mme")
  #         print(summary(a)$aic)
  #         print(summary(b)$aic)
  #         print(summary(c)$aic)
  #         # print(summary(a)$bic)
  #         # print(summary(b)$bic)
  #         # print(summary(c)$bic)
  #       }

  # #gamma for 1,2,5  gumbel: 3,4
  
  #a 
  
            # normal <- matrix(nrow=rep, ncol=5)
            # x = c(3,4)
            # for ( i in x){
            #   Gumbel<-fitdist(sr[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
            #   normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
            # }
            # l=c(1,2,5)
            # for (i in l){
            #   Gamma<-fitdist(sr[,i],"gamma", method="mme")
            #   normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
            # }
            #  colnames(normal) <- colnames(sr[,1:5])
            # 
  
  #### 1967-1990
  
  # i=5
  # plot(fitdist(sr[,5],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
  # plot(fitdist(sr[,5],"norm"))
  # # 
  #   for ( i in 1:5){
  #   a <- fitdist(sr[,i],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead")
  #   b <- fitdist(sr[,i],"norm")
  #   c <- fitdist(sr[,i],"gamma",start=list(a=0,b=1),method="mme")
  #   print(summary(a)$aic)
  #   print(summary(b)$aic)
  #   print(summary(c)$aic)
  #   # print(summary(a)$bic)
  #   # print(summary(b)$bic)
  #   # print(summary(c)$bic)
  # }
  
  #norm: 1 , gamma: 2,3,4,5
  
  #b
  
      # normal <- matrix(nrow=rep, ncol=5)
      # 
      # l=c(2,3,4,5)
      # for (i in l){
      #   Gamma<-fitdist(sr[,i],"gamma", method="mme")
      #   normal[,i] <- rgamma(rep,Gamma$estimate[1], Gamma$estimate[2])
      # }
      # t=c(1)
      # for (i in t){
      #   Normal<-fitdist(sr[,i],"norm")
      #   normal[,i] <- rnorm(rep,Normal$estimate[1], Normal$estimate[2])
      # }
      # colnames(normal) <- colnames(sr[,1:5])
  
  
  # 1991-2012
  
  
  
  # plot(fitdist(sr[,7],"gumbel",start=list(a=0,b=1),optim.method="Nelder-Mead"))
  # plot(fitdist(sr[,7],"norm"))
  # 
  # for ( i in 1:5){
  #   a <- fitdist(sr[,i],"gumbel",start=list(a=-3,b=3),optim.method="Nelder-Mead")
  #   b <- fitdist(sr[,i],"norm")
  #   c <- fitdist(sr[,i],"gamma",start=list(a=0,b=1),method="mme")
  #   print(summary(a)$aic)
  #   print(summary(b)$aic)
  #   print(summary(c)$aic)
  #   # print(summary(a)$bic)
  #   # print(summary(b)$bic)
  #   # print(summary(c)$bic)
  # }
  # 
  # #gumbel: 1:5
  # 
  # #c
  # 
      normal <- matrix(nrow=rep, ncol=5)
      x = c(1:5)
      for ( i in x){
        Gumbel<-fitdist(sr[,i],"gumbel", start=list(a=-3,b=3),optim.method="Nelder-Mead")
        normal[,i] <- rgumbel(rep,Gumbel$estimate[1], Gumbel$estimate[2])
      }
    
      colnames(normal) <- colnames(sr[,1:5])

  
  
  # CDF
  SRI <- vector("numeric", rep)
  for (i in 1:rep){
    sri = (quantile(normal[,1], probs=simdata_ind_sr[i,1])* wt_ind[,1] + quantile(normal[,2], probs=simdata_ind_sr[i,2])* wt_ind[,2] + quantile(normal[,3], probs=simdata_ind_sr[i,3])* wt_ind[,3] 
           + quantile(normal[,4], probs=simdata_ind_sr[i,4])* wt_ind[,4]+ quantile(normal[,5], probs=simdata_ind_sr[i,5])* wt_ind[,5]
           
    )
    
    SRI[i] <- sri 
  }
  
  return(SRI)
}

clim <- as.data.frame(clim)
write.table(clim, "Indonesia_rice_dswr_simulations_github_period2.csv", sep=",", row.names=F)


# plots
setwd("H:/DPhil/breadbaskets/Data collection")

thres_rice <- read.table("rice_threshold25_bb_github_version_3_weighted.csv", sep=",", header=T)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
clim <- read.csv("Indonesia_rice_dswr_simulations_github_period2.csv", header=T, sep=",")

# plot 

impute_resolution <- 10000
values_to_impute = seq(
  min(clim)
  , max(clim)
  , length.out = impute_resolution
)

ecdfs = matrix(nrow=ncol(clim),ncol=length(values_to_impute))


for(i in 1:ncol(clim)){ #assumes column 1 is true_data
  this_ecdf = ecdf(clim[,i])
  ecdfs[i,] = this_ecdf(values_to_impute)
}



ecdfPlot(clim[,1], ecdf.col="white", main="Solar radiation in Indonesia Nov-Feb ", xlab="Solar radiation (in kW/m2) ", ylab="F(x)")

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf, lwd=3
  
)

mean_ecdf = colMeans(ecdfs)
lines(
  x = values_to_impute
  , y = mean_ecdf
  , lwd=3, col="grey55"
)



ECDF <- matrix(nrow=10000,ncol=1000)

for ( i in 1:1000){
  s <- sort(clim[,i], decreasing=F)
  ECDF[,i] <-s
}

# standard error calculation: st.err= sd/sqrt(n) with n=1000

ERROR <- matrix(ncol=10000, nrow=1)
for (i in 1:10000){
  error <- sd(ECDF[i,]/sqrt(1000))
  ERROR[,i] <- error
}
mean_error <-mean (ERROR[1,])
# period 1: 0.00538
# period 2: 0.004964


legend("topleft", legend=c("1967-1990",  "1991-2012"), cex=0.9,lty=c (1,1),lwd=2,col=c("black","gray55"))
abline(v=thres_rice [,5], lty=2)
legend("bottomright", cex=0.8, bty= "n", legend=c("SE1 = 0.005" , "SE2 = 0.005", c(paste("threshold = ", round(thres_rice[1,5], digits =2), sep=""))))


# ecdfPlot(PC, main="Nov-Feb Solar Radiation Indonesia", xlab="Solar radiation in W/m2", ylab="F(x)")
# ecdfPlot(PC, add=T, ecdf.col="grey55")




# CDF independent
SRIi <- vector("numeric", rep)
for (i in 1:rep){
  srii = (quantile(normal[,1], probs=runif(1,0,1))* wt_ind[,1]+ quantile(normal[,2], probs=runif(1,0,1))* wt_ind[,2] + quantile(normal[,3], probs=runif(1,0,1))* wt_ind[,3] 
          + quantile(normal[,4], probs=runif(1,0,1))* wt_ind[,4]+ quantile(normal[,5], probs=runif(1,0,1))* wt_ind[,5]
          + quantile(normal[,6], probs=runif(1,0,1))* wt_ind[,6]+ quantile(normal[,7], probs=runif(1,0,1))* wt_ind[,7]
  )
  
  SRIi[i] <- srii
}




## ### ## ### ## ## ### ## ### ## # # ## ## ## 
############# Rice all #####################

setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
IND <- read.csv("Indonesia_rice_dswr_simulations_github_period2.csv", header=T, sep=",")
SI <- read.csv("India_rice_cum_prec_simulations_github_period2.csv", header=T, sep=",")
SI2 <- read.csv("India_rice_dswr_simulations_github_period2.csv", header=T, sep=",")
SI3 <- read.csv("India_rice_tmax_simulations_github_period2.csv", header=T, sep=",")
CT <- read.csv("China_rice_dswr_simulations_github_period2.csv", header=T, sep=",")


setwd("H:/DPhil/breadbaskets/Data collection")

var <- read.csv("rice_climate_all_bb_version_3_github_weighted.csv", header=T, sep=",")
require(PerformanceAnalytics)
chart.Correlation (na.omit(var[,2:6]), method="kendall", pch=20, smooth=F,lines=F, main=" Rice climate correlation structures", cex.main=0.9)

u_var <- pobs(var[,2:6])

# extracting correlations between indicators of the same BB
u_var <- pobs(var[,3:5])


setwd("H:/DPhil/breadbaskets/Data collection")
t25 <- read.csv("rice_threshold25_bb_github_version_3_weighted.csv", header=T, sep=",")

# uncertainty test:
t25 <- read.csv("rice_threshold25_prediction_0.9_version3_github_weighted.csv", header=T, sep=",")
t25l<- t25[2,]
t25u<- t25[3,]


RVM = RVineStructureSelect(u_var,c(1:6),progress=TRUE)

clim <- foreach (i=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula")) %dopar% {

  rep=1000
  simdata_var <- RVineSim(rep, RVM)
  
  ####### risks of simultaneous negative climate conditions ###
  
  # version 3
  
  ALL0 <- matrix(nrow=rep, ncol=5)
  for (n in 1:rep){
    all0 = c(quantile(CT[,i], probs=simdata_var[n,1]), quantile(SI3[,i], probs=simdata_var[n,2]) , quantile(SI[,i], probs=simdata_var[n,3]) ,quantile(SI2[,i], probs=simdata_var[n,4]),
             quantile(IND[,i], probs=simdata_var[n,5]))
    ALL0[n,] <- all0
  }

  ALL0 <- as.data.frame(ALL0)
  # 
  # # version 3 indep BBs
  # 
  # ALL0 <- matrix(nrow=rep, ncol=5)
  # for (n in 1:rep){
  #   all0 = c(quantile(CT[,i], probs=runif(1,0,1)), quantile(SI3[,i], probs=simdata_var[n,1]) , quantile(SI[,i], probs=simdata_var[n,2]) ,quantile(SI2[,i], probs=simdata_var[n,3]), 
  #            quantile(IND[,i], probs=runif(1,0,1)))
  #   ALL0[n,] <- all0
  # }
  # 
  # ALL0 <- as.data.frame(ALL0)
  
  
   
  ## t25 version 3--
  for (l in 1:rep) {
    ALL0$V6[l] <- sum((ifelse(ALL0$V1[l]<t25l[1],1,0))[1,],(ifelse(ALL0$V2[l]>t25u[2] | ALL0$V3[l]<t25l[3] | ALL0$V4[l]>t25u[4],1,0))[1,],
                      (ifelse(ALL0$V5[l]>t25u[5],1,0))[1,])
    
  }
  col <- c(colnames(var[,2:6]), "BBs")
  colnames(ALL0) <- col
  
  return(ALL0[,6])
}

write.table(clim, "rice_all_version3_t25_period1_github_weighted_indep.csv", sep=",", row.names = F)
write.table(clim, "rice_all_version3_t25_lowrisk_prediction_90_period2_github_weighted.csv", sep=",", row.names = F)


# ## t25 version 3 indep BBs 
# for (i in 1:rep) {
#   ALL0i$V8[i] <- sum((ifelse(ALL0i$V1[i]>t25[1],1,0))[1,],(ifelse(ALL0i$V2[i]<t25[2] | ALL0i$V3[i]>t25[3],1,0))[1,],(ifelse(ALL0i$V4[i]>t25[4],1,0))[1,],
#                      (ifelse(ALL0i$V5[i]<t25[5],1,0))[1,],(ifelse(ALL0i$V6[i]<t25[5] | ALL0i$V7[i]>t25[5],1,0))[1,])
#   
# }
# col <- c(colnames(simdata_var), "BBs")
# colnames(ALL0i) <- col
# write.table(ALL0i, "soybean_all_version3_t25_period2_vx_indep.csv", sep=";", row.names = F)
# 
# ## t25 version 3 indep BBs and states
# for (i in 1:rep) {
#   ALL0ii$V8[i] <- sum((ifelse(ALL0ii$V1[i]>t25[1],1,0))[1,],(ifelse(ALL0ii$V2[i]<t25[2] | ALL0ii$V3[i]>t25[3],1,0))[1,],(ifelse(ALL0ii$V4[i]>t25[4],1,0))[1,],
#                       (ifelse(ALL0ii$V5[i]<t25[5],1,0))[1,],(ifelse(ALL0ii$V6[i]<t25[5] | ALL0ii$V7[i]>t25[5],1,0))[1,])
#   
# }
# col <- c(colnames(simdata_var), "BBs")
# colnames(ALL0ii) <- col
# write.table(ALL0ii, "soybean_all_version3_t25_period2_vx_indep_BBs_states.csv", sep=";", row.names = F)
# 
# # 



# change of simultabeous risks between two periods

setwd("H:/DPhil/breadbaskets/Data collection")
t25 <- read.csv("rice_threshold25_bb_github_version_3_weighted.csv", header=T, sep=",")

ALL0<- clim
setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
ALL0 <- read.csv("rice_all_version3_t25_period1_github_weighted.csv", sep=",", header=T)
ALL0 <- read.csv("rice_all_version3_t25_highrisk_prediction_90_period1_github_weighted.csv", sep=",", header=T)

#indep
ALL0 <- read.csv("rice_all_version3_t25_period1_github_weighted_indep.csv", sep=",", header=T)

ALL0_perc <- matrix(nrow=998, ncol=4)
#period1
for (o in 1:998){
  for (i in 0:3){
    ALL0_perc[o,i+1]<- sum(ALL0[,o]==i)/1000
  }}

m1 <- apply(ALL0_perc,2,mean)
s1 <- apply(ALL0_perc,2,sd)


ALL0 <- read.csv("rice_all_version3_t25_period2_github_weighted.csv", sep=",", header=T)
ALL0 <- read.csv("rice_all_version3_t25_highrisk_prediction_90_period2_github_weighted.csv", sep=",", header=T)

#indep
ALL0 <- read.csv("rice_all_version3_t25_period2_github_weighted_indep.csv", sep=",", header=T)

ALL0_perc <- matrix(nrow=1000, ncol=4)
#period2
for (o in 1:1000){
  for (i in 0:3){
    ALL0_perc[o,i+1]<- sum(ALL0[,o]==i)/1000 
  }}

m2 <- apply(ALL0_perc,2,mean)
s2 <- apply(ALL0_perc,2,sd)

stats<- rbind(m1,s1,m2,s2)


write.table(stats, "rice_global_simultaneous_pdf_25_version_3_github_weighted_indep.csv", sep=",", row.names=c("mean_period1","sd_period1",  "mean_period2","sd_period2"))
write.table(stats, "rice_global_simultaneous_pdf_25_version_3_highrisk_prediction_90_github_weighted.csv", sep=",", #row.names=c("var" ,"mean_period1","sd_period1",  "mean_period2","sd_period2"),
            col.names=c(0:3))


## portion below/above 25% threshold  ---------

# change of risks between two periods
setwd("H:/DPhil/breadbaskets/Data collection")
t25 <- read.csv("rice_threshold25_bb_github_version_3_weighted.csv", header=T, sep=",")


t25_prop_soybean_bb <- matrix(ncol=5, nrow=1000)

for ( i in 1:1000){
  t25_prop_soybean_bb[i,1] <- length( which( CT[,i] < t25[1][1,] ))/10000
  t25_prop_soybean_bb[i,2] <- length( which( SI3[,i]>t25[2][1,] ))/10000
  t25_prop_soybean_bb[i,3] <- length( which( SI[,i]<t25[3][1,] ))/10000
  t25_prop_soybean_bb[i,4] <- length( which( SI2[,i]>t25[4][1,] ))/10000
  t25_prop_soybean_bb[i,5] <- length( which( IND[,i]>t25[5][1,] ))/10000
  
}

colnames(t25_prop_soybean_bb) <- colnames(t25)
write.table(t25_prop_soybean_bb, "proportion_rice_25_global_bb_period2_github.csv", sep=",", row.names=F)


setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
prop1_25 <- read.table("proportion_rice_25_global_bb_period1_github.csv", sep=",", header=T)
prop2_25 <- read.table("proportion_rice_25_global_bb_period2_github.csv", sep=",", header=T)

diff_25 <- prop2_25 - prop1_25
diff_25_stats <- matrix(ncol=5, nrow=2)
diff_25_stats[1,] <- apply(diff_25, 2, mean)
diff_25_stats[2,] <- apply(diff_25, 2, sd)
colnames(diff_25_stats) <- colnames(prop2_25)
write.table(diff_25_stats, "proportion_diff_stats_rice_25_global_bb_github.csv", sep=",", row.names=F)



# significant change between 2 periods?? 


period1 <- read.csv("rice_all_version3_t25_period1_github_weighted.csv", sep=",", header=T)
period2 <- read.csv("rice_all_version3_t25_period2_github_weighted.csv", sep=",", header=T)

# threshold uncertainty analysis:
period1 <- read.csv("rice_all_version3_t25_lowrisk_prediction_90_period1_github_weighted.csv", sep=",", header=T)
period2 <- read.csv("rice_all_version3_t25_lowrisk_prediction_90_period2_github_weighted.csv", sep=",", header=T)


period1_mean <- apply(period1,2,sort)
period1_mean <- apply(period1_mean, 1, mean)
period2_mean <- apply(period2, 2, sort)
period2_mean <- apply(period2_mean, 1, mean)


period1i <- read.csv("rice_all_version3_t25_period1_github_weighted_indep.csv", sep=",", header=T)
period2i <- read.csv("rice_all_version3_t25_period2_github_weighted_indep.csv", sep=",", header=T)

period1i_mean <- apply(period1i,2,sort)
period1i_mean <- apply(period1i_mean, 1, mean)
period2i_mean <- apply(period2i, 2, sort)
period2i_mean <- apply(period2i_mean, 1, mean)

mean(period1_mean)- mean(period2_mean)


# Wilcoxon (or Mann-Whitney-Wilcoxon) test -> assumes normal distribution for large sample numbers
wilcox.test(period1_mean, period2_mean)
# -> p=0; p<=0.05 or 0.001 means populations are nonidentical



# between periods:
# Wilcoxon (or Mann-Whitney-Wilcoxon) test -> assumes normal distribution
wilcox.test(period2_mean, period2i_mean)
wilcox.test(period1_mean, period2_mean, alternative= "less", var.equal = F)



# each case separate

pvalue <- matrix (nrow=4, ncol=1)

for (i in 0:3){
  pois <-poisson.test(c(sum(period1==i),	sum(period1i==i)), c(10000000, 10000000), alternative = c("two.sided"))
  pvalue[i+1,1] <- pois$p.value
}



# -> period 1: all pvalues p< 0.001! significant difference between all cases, except for 2
# -> period 2: all pvalues p< 0.001! significant difference between all cases




#### ## ### ### ### ### #### ##
###### plots for paper 3 ############
setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
wheat <- read.table("wheat_global_simultaneous_pdf_25_version_3_github_weighted_v2.csv", sep=",", header=T)
maize <- read.table("maize_global_simultaneous_pdf_25_version_3_github_weighted.csv", sep=",", header=T)
soybean <- read.table("soybean_global_simultaneous_pdf_25_version_3_github_weighted.csv", sep=",", header=T)
rice <- read.table("rice_global_simultaneous_pdf_25_version_3_github_weighted.csv", sep=",", header=T)

soybean <- read.table("China_soybean_t30_simultaneous_pdf_25_version_3_github.csv", sep=",", header=T)

# threshold uncertainty
setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
wheat <- read.table("wheat_global_simultaneous_pdf_25_version_3_highrisk_prediction_90_github_weighted_v2.csv", sep=",", header=T)
maize <- read.table("maize_global_simultaneous_pdf_25_version_3_lowrisk_prediction_90_github_weighted.csv", sep=",", header=T)
soybean <- read.table("soybean_global_simultaneous_pdf_25_version_3_lowrisk_prediction_90_github_weighted.csv", sep=",", header=T)
rice <- read.table("rice_global_simultaneous_pdf_25_version_3_lowrisk_prediction_90_github_weighted.csv", sep=",", header=T)



# independent
setwd("H:/DPhil/breadbaskets/Analysis/BB climate indicators")
wheat_i <- read.table("wheat_global_simultaneous_pdf_25_version_3_github_weighted_indep_v2.csv", sep=",", header=T)
maize_i <- read.table("maize_global_simultaneous_pdf_25_version_3_github_weighted_indep.csv", sep=",", header=T)
soybean_i <- read.table("soybean_global_simultaneous_pdf_25_version_3_github_weighted_indep.csv", sep=",", header=T)
rice_i <- read.table("rice_global_simultaneous_pdf_25_version_3_github_weighted_indep.csv", sep=",", header=T)

soybean_i <- read.table("China_soybean_t30_simultaneous_pdf_25_version_3_github_indep.csv", sep=",", header=T)



setwd("H:/DPhil/breadbaskets/Data collection")
thres_soy <- read.table("soybean_threshold25_bb_version_3 _github_weighted.csv", sep=",", header=T)
thres_maize <- read.table("maize_threshold25_version3_github_weighted.csv", sep=",", header=T)
thres_wheat <- read.table("wheat_threshold25_version3_github_weighted.csv", sep=",", header=T)
thres_rice <- read.table("rice_threshold25_bb_github_version_3_weighted.csv", sep=",", header=T)


# barplot with Standard error
# http://monkeysuncle.stanford.edu/?p=485&cpage=1#comments

error.bar <- function(x, y, upper, lower=upper, length=0.05,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3,lwd=1,length=length, ...)
}

# WHEAT

wheat_m <- as.matrix(rbind(wheat[1,2:9], wheat[3,2:9]))
wheat_s <- as.matrix(rbind(wheat[2,2:9], wheat[4,2:9]))

bp<-barplot(wheat_m,col=c("#2D607D", "#958A86"), ylab="Annual probability of occurrence", xlab="Number of breadbaskets with simultaneous climate risks",
            col.axis="black", ylim=c(0,1), beside=T, names.arg=c("0","1","2", "3", "4","5", "6", "7 "))

text( x=bp, y=wheat_m, labels=round(wheat_m, digits=3), font=2,  pos=3, cex=0.8)

legend("topleft", legend=c("1967-1990",  "1991-2012"),bty= "n", cex=1, fill=c("#2D607D", "#958A86"),
       box.col = c("black", "black"))

error.bar(bp,wheat_m,wheat_s)

# MAIZE

maize_m <- as.matrix(rbind(maize[1,1:7], maize[3,1:7]))
maize_s <- as.matrix(rbind(maize[2,1:7], maize[4,1:7]))

bp<-barplot(maize_m,col=c("#2D607D", "#958A86"), ylab="Annual probability of occurrence", xlab="Number of breadbaskets with simultaneous climate risks",
            col.axis="black", ylim=c(0,1), beside=T, names.arg=c("0","1","2", "3", "4","5", "6"))

text( x=bp, y=maize_m, labels=round(maize_m, digits=3), font=2,  pos=3, cex=0.8)

legend("topright", legend=c("1967-1990",  "1991-2012"),bty= "n", cex=1, fill=c("#2D607D", "#958A86"),
       box.col = c("black", "black"))

error.bar(bp,maize_m,maize_s)

# SOYBEAN

soybean_m <- as.matrix(rbind(soybean[1,2:7], soybean[3,2:7]))
soybean_s <- as.matrix(rbind(soybean[2,2:7], soybean[4,2:7]))

# threshold uncertainty
soybean_m <- as.matrix(rbind(soybean[1,1:6], soybean[3,1:6]))
soybean_s <- as.matrix(rbind(soybean[2,1:6], soybean[4,1:6]))


bp<-barplot(soybean_m,col=c("#2D607D", "#958A86"), ylab="Annual probability of occurrence", xlab="Number of breadbaskets with simultaneous climate risks",
            col.axis="black", ylim=c(0,0.6), beside=T, names.arg=c("0","1","2", "3", "4","5"))

text( x=bp, y=soybean_m, labels=round(soybean_m, digits=3), font=2,  pos=3, cex=0.8)

legend("topleft", legend=c("1967-1990",  "1991-2012"),bty= "n", cex=1, fill=c("#2D607D", "#958A86"),
       box.col = c("black", "black"))

error.bar(bp,soybean_m,soybean_s)

# RICE

rice_m <- as.matrix(rbind(rice[1,2:5], rice[3,2:5]))
rice_s <- as.matrix(rbind(rice[2,2:5], rice[4,2:5]))

# uncertainty analysis
rice_m <- as.matrix(rbind(rice[1,1:4], rice[3,1:4]))
rice_s <- as.matrix(rbind(rice[2,1:4], rice[4,1:4]))

bp<-barplot(rice_m,col=c("#2D607D", "#958A86"), ylab="Annual probability of occurrence", xlab="Number of breadbaskets with simultaneous climate risks",
            col.axis="black", ylim=c(0,1.1), beside=T, names.arg=c("0","1","2", "3"))

text( x=bp, y=rice_m, labels=round(rice_m, digits=3), font=2,  pos=3, cex=0.8)

legend("topright", legend=c("1967-1990",  "1991-2012"),bty= "n", cex=1, fill=c("#2D607D", "#958A86"),
       box.col = c("black", "black"))

error.bar(bp,rice_m,rice_s)


# SOYBEAN T30 IN CHINA

soybean_m <- as.matrix(rbind(soybean[1,2:11], soybean[3,2:11]))
soybean_s <- as.matrix(rbind(soybean[2,2:11], soybean[4,2:11]))

bp<-barplot(soybean_m,col=c("#2D607D", "#958A86"), ylab="Annual probability of occurrence", xlab="Number of provinces with simultaneous climate risks",
            col.axis="black", ylim=c(0,0.25), beside=T, names.arg=c("0","1","2", "3", "4","5", "6", "7", "8", "9"))

a=c(1)
b=c(2)

text( x=bp[a,], y=soybean_m[a,], labels=round(soybean_m[a,], digits=3), font=2,  adj=c(0.6,-2), cex=0.7)
text( x=bp[b,], y=soybean_m[b,], labels=round(soybean_m[b,], digits=3), font=2, adj=c(0.3,-2),  cex=0.7)



legend("topleft", legend=c("1967-1990",  "1991-2012"),bty= "n", cex=1, fill=c("#2D607D", "#958A86"),
       box.col = c("black", "black"))

error.bar(bp,soybean_m,soybean_s)



# comparison with independence between BBs

# WHEAT

wheat_m <- as.matrix(rbind(wheat[1,2:9], wheat_i[1,2:9], wheat[3,2:9], wheat_i[3,2:9]))
wheat_s <- as.matrix(rbind(wheat[2,2:9], wheat_i[2,2:9], wheat[4,2:9], wheat_i[4,2:9] ))

bp<-barplot(wheat_m,col=c("#2D607D", "#69A6C9","#958A86",  "#CDC7C5"), ylab="Annual probability of occurrence", xlab="Number of breadbaskets with simultaneous climate risks",
            col.axis="black", ylim=c(0,0.6), beside=T, names.arg=c("0","1","2", "3", "4","5", "6", "7"))

#text( x=bp, y=wheat_m, labels=round(wheat_m, digits=3), font=2,  pos=3, cex=0.8)

a=c(1,3)
b=c(2,4)

text( x=bp[a,], y=wheat_m[a,], labels=round(wheat_m[a,], digits=3), font=2,  pos=3, cex=0.7)
text( x=bp[b,], y=wheat_m[b,], labels=round(wheat_m[b,], digits=3), font=2, adj=c(0.5,-3),  cex=0.7)


legend("topleft", legend=c("1967-1990", "independent BBs", "1991-2012",  "independent BBs"),bty= "n", cex=1, fill=c("#2D607D","#69A6C9","#958A86", "#CDC7C5"),
       box.col = c("black", "black"))

error.bar(bp,wheat_m,wheat_s)


# MAIZE

maize_m <- as.matrix(rbind(maize[1,2:8], maize_i[1,2:8], maize[3,2:8], maize_i[3,2:8]))
maize_s <- as.matrix(rbind(maize[2,2:8], maize_i[2,2:8],maize[4,2:8], maize_i[4,2:8]))

bp<-barplot(maize_m,col=c("#2D607D", "#69A6C9","#958A86",  "#CDC7C5"), ylab="Annual probability of occurrence", xlab="Number of breadbaskets with simultaneous climate risks",
            col.axis="black", ylim=c(0,0.6), beside=T, names.arg=c("0","1","2", "3", "4","5", "6"))
a=c(1,3)
b=c(2,4)

text( x=bp[a,], y=maize_m[a,], labels=round(maize_m[a,], digits=3), font=2,  pos=3, cex=0.7)
text( x=bp[b,], y=maize_m[b,], labels=round(maize_m[b,], digits=3), font=2, adj=c(0.5,-3),  cex=0.7)


legend("topleft", legend=c("1967-1990", "independent BBs", "1991-2012",  "independent BBs"),bty= "n", cex=1, fill=c("#2D607D","#69A6C9","#958A86", "#CDC7C5"),
       box.col = c("black", "black"))

error.bar(bp,maize_m,maize_s)


# SOYBEAN

soybean_m <- as.matrix(rbind(soybean[1,1:6], soybean_i[1,1:6], soybean[3,1:6], soybean_i[3,1:6]))
soybean_s <- as.matrix(rbind(soybean[2,1:6], soybean_i[2,1:6], soybean[4,1:6], soybean_i[4,1:6]))

bp<-barplot(soybean_m,col=c("#2D607D", "#69A6C9","#958A86",  "#CDC7C5"), ylab="Annual probability of occurrence", xlab="Number of breadbaskets with simultaneous climate risks",
            col.axis="black", ylim=c(0,0.6), beside=T, names.arg=c("0","1","2", "3", "4","5"))
a=c(1,3)
b=c(2,4)

text( x=bp, y=soybean_m, labels=round(soybean_m, digits=3), font=2,  pos=3, cex=0.7)

text( x=bp[a,], y=soybean_m[a,], labels=round(soybean_m[a,], digits=3), font=2,  pos=3, cex=0.7)
text( x=bp[b,], y=soybean_m[b,], labels=round(soybean_m[b,], digits=3), font=2, adj=c(0.5,-3),  cex=0.7)


legend("topleft", legend=c("1967-1990", "independent BBs", "1991-2012",  "independent BBs"),bty= "n", cex=1, fill=c("#2D607D","#69A6C9","#958A86", "#CDC7C5"),
       box.col = c("black", "black"))

error.bar(bp,soybean_m,soybean_s)

# RICE

rice_m <- as.matrix(rbind(rice[1,2:5],rice_i[1,2:5], rice[3,2:5], rice_i[3,2:5]))
rice_s <- as.matrix(rbind(rice[2,2:5], rice_i[2,2:5], rice[4,2:5], rice_i[4,2:5]))

bp<-barplot(rice_m,col=c("#2D607D", "#69A6C9","#958A86",  "#CDC7C5"), ylab="Annual probability of occurrence", xlab="Number of breadbaskets with simultaneous climate risks",
            col.axis="black", ylim=c(0,0.6), beside=T, names.arg=c("0","1","2", "3"))
a=c(1,3)
b=c(2,4)

text( x=bp, y=rice_m, labels=round(rice_m, digits=3), font=2,  pos=3, cex=0.7)

#text( x=bp[a,], y=rice_m[a,], labels=round(rice_m[a,], digits=3), font=2,  pos=3, cex=0.7)
#text( x=bp[b,], y=rice_m[b,], labels=round(rice_m[b,], digits=3), font=2, adj=c(0.5,-3),  cex=0.7)

#text( x=bp, y=rice_m, labels=round(rice_m, digits=3), font=2,  pos=3, cex=0.8)

legend("topleft", legend=c("1967-1990", "independent BBs", "1991-2012",  "independent BBs"),bty= "n", cex=1, fill=c("#2D607D","#69A6C9","#958A86", "#CDC7C5"),
       box.col = c("black", "black"))

error.bar(bp,rice_m,rice_s)


# SOYBEAN T30 IN CHINA

soybean_m <- as.matrix(rbind(soybean[1,2:11], soybean_i[1,2:11], soybean[3,2:11], soybean_i[3,2:11]))
soybean_s <- as.matrix(rbind(soybean[2,2:11],soybean_i[2,2:11], soybean[4,2:11], soybean_i[4,2:11]))

bp<-barplot(soybean_m,col=c("#2D607D", "#69A6C9","#958A86",  "#CDC7C5"), ylab="Annual probability of occurrence", xlab="Number of provinces with simultaneous climate risks",
            col.axis="black", ylim=c(0,0.4), beside=T, names.arg=c("0","1","2", "3", "4","5", "6", "7", "8", "9"))

a=c(1,3)
b=c(2,4)

text( x=bp[a,], y=soybean_m[a,], labels=round(soybean_m[a,], digits=3), font=2,  adj=c(0.6,-2), cex=0.7)
text( x=bp[b,], y=soybean_m[b,], labels=round(soybean_m[b,], digits=3), font=2, adj=c(0.3,-2),  cex=0.7)



legend("topleft", legend=c("1967-1990", "independent BBs", "1991-2012",  "independent BBs"),bty= "n", cex=1, fill=c("#2D607D","#69A6C9","#958A86", "#CDC7C5"),
       box.col = c("black", "black"))

error.bar(bp,soybean_m,soybean_s)


