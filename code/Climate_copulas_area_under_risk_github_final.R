install.packages("foreach", repos="http://cran.rstudio.com/")
install.packages("copula", repos="http://cran.rstudio.com/")
install.packages("CDVine", repos="http://cran.rstudio.com/")
install.packages("VineCopula", repos="http://cran.rstudio.com/")
install.packages("EnvStats", repos="http://cran.rstudio.com/")
install.packages("QRM", repos="http://cran.rstudio.com/")
install.packages("doParallel", repos="http://cran.rstudio.com/")
install.packages("logspline", repos="http://cran.rstudio.com/")
install.packages("fitdistrplus", repos="http://cran.rstudio.com/")
install.packages("wesanderson", repos="http://cran.rstudio.com/")



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
require(wesanderson)

library(doParallel)
cl <- makeCluster(30)
cl <- makeCluster(4)
clusterExport(cl, c("dgumbel", "pgumbel", "qgumbel", "rgumbel"))
registerDoParallel(cl)


######## wheat all ---------

setwd("~/Analysis/BB climate indicators")

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
setwd("~/Data collection")
t25 <- read.csv("wheat_threshold25_version3_github_weighted_v2.csv", header=T, sep=",")

# crop area in 2012 and 1990
area <- read.csv("wheat_maize_soybean_rice_BB_area_ha_2012_1990.csv", header=T, sep=",")

area <- area [,13:19]

area_all <- apply(area,1,sum)

# pie chart of crop area:
lbls <- c("China", "India", "USA", "Argentina", "Australia", "EU", "Russia/Ukraine")
pie(as.numeric(area[2,]), labels = lbls, main="Wheat", cex=2, col=wes_palette(7, name = "Darjeeling2", type = "continuous"))
pie(as.numeric(area[1,]), labels = lbls, main="Wheat", cex=2,col=wes_palette(7, name = "Darjeeling2", type = "continuous"))



setwd("~/Analysis/BB climate indicators")
ART <- read.csv("Argentina_wheat_tmin_simulations_github_period1_v2.csv", header=T, sep=",")
SI <- read.csv("India_wheat_spi_simulations_github_period1.csv", header=T, sep=",")
PC <- read.csv("China_wheat_cum_prec_simulations_github_period1.csv", header=T, sep=",")
SU <- read.csv("USA_wheat_cum_prec_simulations_github_period1.csv", header=T, sep=",")
SU2 <- read.csv("USA_wheat_t34_simulations_github_period1.csv", header=T, sep=",")
AUP <- read.csv("Australia_wheat_cum_prec_simulations_github_period1.csv", header=T, sep=",")
AUP2 <- read.csv("Australia_wheat_tmax_simulations_github_period1.csv", header=T, sep=",")
EUC <- read.csv("Europe_wheat_cum_prec_simulations_github_period1.csv", header=T, sep=",")
EUT <- read.csv("Europe_wheat_t34_simulations_github_period1.csv", header=T, sep=",")
RUKC <- read.csv("RussUkr_wheat_cum_prec_simulations_github_period1.csv", header=T, sep=",")
RUKT <- read.csv("RussUkr_wheat_tmean_simulations_github_period1.csv", header=T, sep=",")


# area in 2012 for period 2
r=1
# area in 1990 for period 1
#r=2


# version 3
u_var <- pobs(var[,2:12])

RVM = RVineStructureSelect(u_var,c(1:6),progress=TRUE)


clim <- foreach (i=1:10,  .combine="cbind", .packages=c("CDVine", "VineCopula")) %dopar% {
  
  
  rep=10000
  simdata_var <- RVineSim(rep, RVM)
  
#  version 3
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
  #             quantile(SU[,i], probs=simdata_var1[n,1]), quantile(SU2[,i], probs=simdata_var1[n,2]),quantile(AUP[,i], probs=simdata_var2[n,1]),quantile(AUP2[,i], probs=simdata_var2[n,2]),
  #             quantile(EUC[,i], probs=simdata_var3[n,1]),quantile(EUT[,i], probs=simdata_var3[n,2]),quantile(RUKC[,i], probs=simdata_var4[n,1]),quantile(RUKT[,i], probs=simdata_var4[n,2]))
  #   
  #   ALL0[n,] <- all0i
  # }
  # 
  # ALL0 <- as.data.frame(ALL0)
  
  
  
  ###
  # portion below 25% threshold ## version 3 -----------
  
  for (l in 1:rep) {
    ALL0$V12[l] <- sum((ifelse(ALL0$V1[l]>t25[1],area[r,4],0))[1,],(ifelse(ALL0$V2[l]<t25[2],area[r,2],0))[1,],(ifelse(ALL0$V3[l]<t25[3],area[r,1],0))[1,],
                       (ifelse(ALL0$V4[l]<t25[4] | ALL0$V5[l]>t25[5],area[r,3],0))[1,],(ifelse(ALL0$V6[l]<t25[6] | ALL0$V7[l]>t25[7],area[r,5],0))[1,],
                       (ifelse(ALL0$V8[l]<t25[8] | ALL0$V9[l]>t25[9],area[r,6],0))[1,],(ifelse(ALL0$V10[l]<t25[10] | ALL0$V11[l]>t25[11],area[r,7],0))[1,])
    
  }
  col <- c(colnames(var[,2:12]), "BBs")
  colnames(ALL0) <- col
  
  return(ALL0[,12])
}


#write.table(clim, "wheat_all_version3_riskarea_t25_period1_github_weighted_v2.csv", sep=",", row.names = F)


# sum(ALL0$V6)/10000
# hist(ALL0$V7, freq=F)
# mean(ALL0$V7)




ALL0<- clim
setwd("~/Analysis/BB climate indicators")
ALL0 <- read.csv("wheat_all_version3_riskarea_t25_period1_github_weighted_v2.csv", sep=",", header=T)
# indep
#ALL0 <- read.csv("wheat_all_version3_t25_period1_github_weighted_indep_v2.csv", sep=",", header=T)


sor1 <- apply(ALL0,2,sort)
sor1 <- (sor1/area_all[2])*100
m1 <- apply(sor1,1,mean)
s1 <- apply(sor1,1,sd)


ALL0 <- read.csv("wheat_all_version3_riskarea_t25_period2_github_weighted_v2.csv", sep=",", header=T)
# indep
#ALL0 <- read.csv("wheat_all_version3_t25_period2_github_weighted_indep_v2.csv", sep=",", header=T)

sor2 <- apply(ALL0,2,sort)
sor2 <- (sor2/area_all[1])*100
m2 <- apply(sor2,1,mean)
s2 <- apply(sor2,1,sd)
stats<- rbind(m1,s1,m2,s2)

stats <-t(stats)

write.table(stats, "wheat_global_simultaneous_arearisk_pdf_25_version_3_github_weighted_v2.csv", sep=",")


#hist(stats[,1], freq=F, col= "blue")
#hist(stats[,1],col="white", xlim=c(0,100), freq=F)
#hist(stats[,3], freq=F, col="blaCK", density= 20, add=T)

setwd("~/Analysis/BB climate indicators")
stats <- read.csv("wheat_global_simultaneous_arearisk_pdf_25_version_3_github_weighted_v2.csv", sep=",", header=T)

plot(density(stats[,1]), col="#2D607D", lwd= 3, xlim=c(0,100),xaxs="i", main="Wheat ", xlab= " Area experiencing climatic risks (in %)")
lines(density(stats[,3]), lwd=3, col= "#958A86")
legend("topleft", legend=c("1967-1990",  "1991-2012"),bty= "n", cex=1, fill=c("#2D607D", "#958A86"),
       box.col = c("black", "black"))

ecdfPlot(stats[,1], ecdf.col="#2D607D", ecdf.lwd= 3, xlim=c(0,100),xaxs="i", main="Wheat ", xlab= " Area experiencing climatic risks (in %)")
ecdfPlot(stats[,3], add=T, ecdf.lwd=3, ecdf.col= "#958A86")
legend("topleft", legend=c("1967-1990",  "1991-2012"),bty= "n", cex=1, fill=c("#2D607D", "#958A86"),
       box.col = c("black", "black"))


#### ##
######## maize  all ---------
## ## ##      ##       ##       ##        ##       ##


setwd("~/Data collection")

# version 3: both indicators for USA and Australia
var <- read.csv("maize_climate_all_bb_version_3_github_weighted.csv", header=T, sep=",")
chart.Correlation (var[,2:10], method="kendall", pch=20, smooth=F,lines=F, main=" maize correlation structures", cex.main=0.9)


# crop area in 2012 and 1990
area <- read.csv("wheat_maize_soybean_rice_BB_area_ha_2012_1990.csv", header=T, sep=",")

area <- area [,2:7]

area_all <- apply(area,1,sum)

# pie chart of crop area:
lbls <- c("China", "Brazil", "Argentina", "India", "USA", "EU")
pie(as.numeric(area[2,]), labels = lbls, cex=2, main="Maize", col=wes_palette(7, name = "Darjeeling2", type = "continuous"))
pie(as.numeric(area[1,]), labels = lbls, cex=2, main="Maize", col=wes_palette(7, name = "Darjeeling2", type = "continuous"))



# extracting correlations between indicators of the same BB
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
setwd("~/Data collection")
t25 <- read.csv("maize_threshold25_version3_github_weighted.csv", header=T, sep=",")

setwd("~/Analysis/BB climate indicators")
PAr2 <- read.csv("Argentina_maize_tmax_simulations_github_period1.csv", header=T, sep=",")
PAr <- read.csv("Argentina_maize_cum_prec_simulations_github_period1.csv", header=T, sep=",")
SI <- read.csv("India_maize_tmax_simulations_github_period1.csv", header=T, sep=",")
PC <- read.csv("China_maize_cum_prec_simulations_github_period1.csv", header=T, sep=",")
PC2 <- read.csv("China_maize_tmax_simulations_github_period1.csv", header=T, sep=",")
TU <- read.csv("USA_maize_t29_simulations_github_period1.csv", header=T, sep=",")
EUp <- read.csv("Europe_maize_cum_prec_simulations_github_period1.csv", header=T, sep=",")
EUT <- read.csv("Europe_maize_tmean_simulations_github_period1.csv", header=T, sep=",")
PB <- read.csv("Brazil_maize_cum_prec_simulations_github_period1.csv", header=T, sep=",")

# area in 2012 for period 2
#r=1
# area in 1990 for period 1
r=2

# version 3
u_var <- pobs(var[,2:10])

RVM = RVineStructureSelect(u_var,c(1:6),progress=TRUE)


clim <- foreach (i=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula")) %dopar% {
  
  
  rep=10000
  simdata_var <- RVineSim(rep, RVM)
  
  
  ####### risks of simultaneous negative climate conditions ###
  
  #setwd("~/Data collection")
  
  
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
  # 
  
  
  
  ###
  # portion of BB below 25% threshold
  
  
  # version 3 t25 : negative climate if one of two indicators exceeds threshold
  
  for (l in 1:rep) {
    ALL0$V10[l] <- sum((ifelse(ALL0$V1[l]>t25[1] | ALL0$V2[l]<t25[2] ,area[r,1],0))[1,],(ifelse(ALL0$V3[l]<t25[3],area[r,2],0))[1,],
                       (ifelse(ALL0$V4[l]>t25[4] | ALL0$V5[l]<t25[5] ,area[r,3],0))[1,],(ifelse(ALL0$V6[l]>t25[6],area[r,4],0))[1,],(ifelse(ALL0$V7[l]>t25[7],area[r,5],0))[1,],
                       (ifelse(ALL0$V8[l]>t25[8] | ALL0$V9[l]<t25[9] ,area[r,6],0))[1,])
    
  }
  
  col <- c(colnames(var[2:10]), "BBs")
  colnames(ALL0) <- col
  
  return(ALL0[,10])
}

write.table(clim, "maize_all_version3_riskarea_t25_period1_github_weighted.csv", sep=",", row.names = F)


setwd("~/Data collection")

# crop area in 2012 and 1990
area <- read.csv("wheat_maize_soybean_rice_BB_area_ha_2012_1990.csv", header=T, sep=",")

area <- area [,2:7]

area_all <- apply(area,1,sum)


ALL0<- clim
setwd("~/Analysis/BB climate indicators")
ALL0 <- read.csv("maize_all_version3_riskarea_t25_period1_github_weighted.csv", sep=",", header=T)


sor1 <- apply(ALL0,2,sort)
sor1 <- (sor1/area_all[2])*100
m1 <- apply(sor1,1,mean)
s1 <- apply(sor1,1,sd)


ALL0 <- read.csv("maize_all_version3_riskarea_t25_period2_github_weighted.csv", sep=",", header=T)


sor2 <- apply(ALL0,2,sort)
sor2 <- (sor2/area_all[1])*100
m2 <- apply(sor2,1,mean)
s2 <- apply(sor2,1,sd)
stats<- rbind(m1,s1,m2,s2)

stats <-t(stats)


write.table(stats, "maize_global_simultaneous_pdf_25_version_3_github_weighted.csv", sep=",") # row.names=c("mean_period1","sd_period1",  "mean_period2","sd_period2"))


setwd("~/Analysis/BB climate indicators")
stats <- read.csv("maize_global_simultaneous_pdf_25_version_3_github_weighted.csv", sep=",", header=T)

hist(stats [,3])
plot(density(stats[,1]), col="#2D607D", lwd= 3, xlim=c(0,100),ylim=c(0,0.025),xaxs="i", main="Maize ", xlab= " Area experiencing climatic risks (in %)")
lines(density(stats[,3]), lwd=3, col= "#958A86")
legend("topleft", legend=c("1967-1990",  "1991-2012"),bty= "n", cex=1, fill=c("#2D607D", "#958A86"),
       box.col = c("black", "black"))

ecdfPlot(stats[,1], ecdf.col="#2D607D", ecdf.lwd= 3, xlim=c(0,100),xaxs="i", main="Maize ", xlab= " Area experiencing climatic risks (in %)")
ecdfPlot(stats[,3], add=T, ecdf.lwd=3, ecdf.col= "#958A86")
legend("topleft", legend=c("1967-1990",  "1991-2012"),bty= "n", cex=1, fill=c("#2D607D", "#958A86"),
       box.col = c("black", "black"))



############### soybean all ----------
## ## ##      ##       ##       ##        ##       ##

####### risks of simultaneous negative climate conditions ###
## version 3 -----

setwd("~/Analysis/BB climate indicators")
ART2 <- read.csv("Argentina_soybean_tmax_simulations_github_period1.csv", header=T, sep=",")
ART <- read.csv("Argentina_soybean_cum_prec_simulations_github_period1.csv", header=T, sep=",")
SI <- read.csv("India_soybean_cum_prec_simulations_github_period1.csv", header=T, sep=",")
SI2 <- read.csv("India_soybean_t30_simulations_github_period1.csv", header=T, sep=",")
CT <- read.csv("China_soybean_t30_simulations_github_period1.csv", header=T, sep=",")
TU <- read.csv("USA_soybean_t30_simulations_github_period1.csv", header=T, sep=",")
PB <- read.csv("Brazil_soybean_cum_prec_simulations_github_period1.csv", header=T, sep=",")

setwd("~/Data collection")
# crop area in 2012 and 1990
area <- read.csv("wheat_maize_soybean_rice_BB_area_ha_2012_1990.csv", header=T, sep=",")

area <- area [,21:25]

area_all <- apply(area,1,sum)

# pie chart of crop area:
lbls <- c("China", "India", "USA",  "Brazil", "Argentina")
pie(as.numeric(area[2,]), labels = lbls, cex=2, main="Soybean", col=wes_palette(7, name = "Darjeeling2", type = "continuous"))
pie(as.numeric(area[1,]), labels = lbls, cex=2, main="Soybean", col=wes_palette(7, name = "Darjeeling2", type = "continuous"))



# area in 2012 for period 2
r=1
# area in 1990 for period 1
r=2

setwd("~/Data collection")

var <- read.csv("soybean_climate_all_bb_version_3.csv", header=T, sep=",")
require(PerformanceAnalytics)
chart.Correlation (var[,2:8], method="kendall", pch=20, smooth=F,lines=F, main=" soybean climate correlation structures", cex.main=0.9)


u_var <- pobs(var[,2:8])


# extracting correlations between indicators of the same BB
u_var1 <- pobs(var[,3:4])
u_var2 <- pobs(var[,7:8])

a <- BiCopSelect(u_var1[,1], u_var1[,2], familyset=c(1:6))
b <- BiCopSelect(u_var2[,1], u_var2[,2], familyset=c(1:6))

rep=10000
simdata_var1 <- BiCopSim(rep, a$family, a$par)
simdata_var2 <- BiCopSim(rep, b$family, b$par)



setwd("~/Data collection")
t25 <- read.csv("soybean_threshold25_bb_version_3 _github_weighted.csv", header=T, sep=",")

RVM = RVineStructureSelect(u_var,c(1:6),progress=TRUE)


clim <- foreach (i=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula")) %dopar% {
  
  rep=10000
  simdata_var <- RVineSim(rep, RVM)
  
  ####### risks of simultaneous negative climate conditions ###
  
  # version 3
  # 
  ALL0 <- matrix(nrow=rep, ncol=7)
  for (n in 1:rep){
    all0 = c(quantile(CT[,i], probs=simdata_var[n,1]), quantile(SI[,i], probs=simdata_var[n,2]) , quantile(SI2[,i], probs=simdata_var[n,3]) ,quantile(TU[,i], probs=simdata_var[n,4]),
             quantile(PB[,i], probs=simdata_var[n,5]),quantile(ART[,i], probs=simdata_var[n,6]) ,quantile(ART2[,i], probs=simdata_var[n,7]))


    ALL0[n,] <- all0
  }

  ALL0 <- as.data.frame(ALL0)

  
  
  # # version 3 indep BBs
  # 
  # ALL0 <- matrix(nrow=rep, ncol=7)
  # for (n in 1:rep){
  #   all0 = c(quantile(CT[,i], probs=runif(1,0,1)), quantile(SI[,i], probs=simdata_var1[n,1]) , quantile(SI2[,i], probs=simdata_var1[n,2]) ,quantile(TU[,i], probs=runif(1,0,1)),
  #            quantile(PB[,i], probs=runif(1,0,1)),quantile(ART[,i], probs=simdata_var2[n,1]) ,quantile(ART2[,i], probs=simdata_var2[n,2]))
  #   
  #   
  #   ALL0[n,] <- all0
  # }
  # 
  # ALL0 <- as.data.frame(ALL0)
  
  
  
  # ###
  

  
  ## t25 version 3--
  for (l in 1:rep) {
    ALL0$V8[l] <- sum((ifelse(ALL0$V1[l]>t25[1],area[r,1],0))[1,],(ifelse(ALL0$V2[l]<t25[2] | ALL0$V3[l]>t25[3],area[r,2],0))[1,],(ifelse(ALL0$V4[l]>t25[4],area[r,3],0))[1,],
                      (ifelse(ALL0$V5[l]<t25[5],area[r,4],0))[1,],(ifelse(ALL0$V6[l]<t25[6] | ALL0$V7[l]>t25[7],area[r,5],0))[1,])
    
  }
  col <- c(colnames(var[,2:8]), "BBs")
  colnames(ALL0) <- col
  
  return(ALL0[,8])
}

write.table(clim, "soybean_all_version3_riskarea_t25_period1_github_weighted.csv", sep=",", row.names = F)



setwd("~/Data collection")
# crop area in 2012 and 1990
area <- read.csv("wheat_maize_soybean_rice_BB_area_ha_2012_1990.csv", header=T, sep=",")

area <- area [,21:25]

area_all <- apply(area,1,sum)



ALL0<- clim
setwd("~/Analysis/BB climate indicators")
ALL0 <- read.csv("soybean_all_version3_riskarea_t25_period1_github_weighted.csv", sep=",", header=T)



sor1 <- apply(ALL0,2,sort)
sor1 <- (sor1/area_all[2])*100
m1 <- apply(sor1,1,mean)
s1 <- apply(sor1,1,sd)


ALL0 <- read.csv("soybean_all_version3_riskarea_t25_period2_github_weighted.csv", sep=",", header=T)


sor2 <- apply(ALL0,2,sort)
sor2 <- (sor2/area_all[1])*100
m2 <- apply(sor2,1,mean)
s2 <- apply(sor2,1,sd)
stats<- rbind(m1,s1,m2,s2)

stats <-t(stats)


write.table(stats, "soybean_global_simultaneous_pdf_25_version_3_github_weighted.csv", sep=",")

hist(stats [,3])
hist(stats [,1])
plot(density(stats[,1]), col="#2D607D", lwd= 3, xlim=c(0,100), ylim=c(0,0.03),xaxs="i", main="Soybean ", xlab= " Area experiencing climatic risks (in %)")
lines(density(stats[,3]), lwd=3, col= "#958A86")
legend("topleft", legend=c("1967-1990",  "1991-2012"),bty= "n", cex=1, fill=c("#2D607D", "#958A86"),
       box.col = c("black", "black"))


setwd("~/Analysis/BB climate indicators")
stats <- read.csv("soybean_global_simultaneous_pdf_25_version_3_github_weighted.csv", sep=",", header=T)

ecdfPlot(stats[,1], ecdf.col="#2D607D", ecdf.lwd= 3, xlim=c(0,100),xaxs="i", main="Soybean ", xlab= " Area experiencing climatic risks (in %)")
ecdfPlot(stats[,3], add=T, ecdf.lwd=3, ecdf.col= "#958A86")
legend("topleft", legend=c("1967-1990",  "1991-2012"),bty= "n", cex=1, fill=c("#2D607D", "#958A86"),
       box.col = c("black", "black"))




## ### ## ### ## ## ### ## ### ## # # ## ## ## 
############# Rice all #####################

setwd("~/Analysis/BB climate indicators")
IND <- read.csv("Indonesia_rice_dswr_simulations_github_period1.csv", header=T, sep=",")
SI <- read.csv("India_rice_cum_prec_simulations_github_period1.csv", header=T, sep=",")
SI2 <- read.csv("India_rice_dswr_simulations_github_period1.csv", header=T, sep=",")
SI3 <- read.csv("India_rice_tmax_simulations_github_period1.csv", header=T, sep=",")
CT <- read.csv("China_rice_dswr_simulations_github_period1.csv", header=T, sep=",")

# area in 2012 for period 2
#r=1
# area in 1990 for period 1
r=2

setwd("~/Data collection")

var <- read.csv("rice_climate_all_bb_version_3_github_weighted.csv", header=T, sep=",")
require(PerformanceAnalytics)
chart.Correlation (na.omit(var[,2:6]), method="kendall", pch=20, smooth=F,lines=F, main=" Rice climate correlation structures", cex.main=0.9)

u_var <- pobs(var[,2:6])

# extracting correlations between indicators of the same BB
u_var <- pobs(var[,3:5])

# crop area in 2012 and 1990
area <- read.csv("wheat_maize_soybean_rice_BB_area_ha_2012_1990.csv", header=T, sep=",")
area <- area [,9:11]

area_all <- apply(area,1,sum)

# pie chart of crop area:
lbls <- c("China", "India", "Indonesia")
pie(as.numeric(area[2,]), labels = lbls, main="Rice", cex=2,col=wes_palette(7, name = "Darjeeling2", type = "continuous"))
pie(as.numeric(area[1,]), labels = lbls, main="Rice", cex=2, col=wes_palette(7, name = "Darjeeling2", type = "continuous"))


setwd("~/Data collection")
t25 <- read.csv("rice_threshold25_bb_github_version_3_weighted.csv", header=T, sep=",")

RVM = RVineStructureSelect(u_var,c(1:6),progress=TRUE)

clim <- foreach (i=1:1000,  .combine="cbind", .packages=c("CDVine", "VineCopula")) %dopar% {
  
  rep=10000
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
    ALL0$V6[l] <- sum((ifelse(ALL0$V1[l]<t25[1],area[r,1],0))[1,],(ifelse(ALL0$V2[l]>t25[2] | ALL0$V3[l]<t25[3] | ALL0$V4[l]>t25[4],area[r,2],0))[1,],
                      (ifelse(ALL0$V5[l]>t25[5],area[r,3],0))[1,])
    
  }
  col <- c(colnames(var[,2:6]), "BBs")
  colnames(ALL0) <- col
  
  return(ALL0[,6])
}

write.table(clim, "rice_all_version3_riskarea_t25_period1_github_weighted.csv", sep=",", row.names = F)

setwd("~/Data collection")
# crop area in 2012 and 1990
area <- read.csv("wheat_maize_soybean_rice_BB_area_ha_2012_1990.csv", header=T, sep=",")
area <- area [,9:11]

area_all <- apply(area,1,sum)


ALL0<- clim
setwd("~/Analysis/BB climate indicators")
ALL0 <- read.csv("rice_all_version3_riskarea_t25_period1_github_weighted.csv", sep=",", header=T)



sor1 <- apply(ALL0,2,sort)
sor1 <- (sor1/area_all[2])*100
m1 <- apply(sor1,1,mean)
s1 <- apply(sor1,1,sd)


ALL0 <- read.csv("rice_all_version3_riskarea_t25_period2_github_weighted.csv", sep=",", header=T)


sor2 <- apply(ALL0,2,sort)
sor2 <- (sor2/area_all[1])*100
m2 <- apply(sor2,1,mean)
s2 <- apply(sor2,1,sd)
stats<- rbind(m1,s1,m2,s2)

stats <-t(stats)


write.table(stats, "rice_global_simultaneous_pdf_25_version_3_github_weighted.csv", sep=",")


setwd("~/Analysis/BB climate indicators")
stats <- read.csv("rice_global_simultaneous_pdf_25_version_3_github_weighted.csv", sep=",", header=T)


hist(stats [,1])
plot(density(stats[,1]), col="#2D607D", lwd= 3, xlim=c(0,100), ylim=c(0,0.03),xaxs="i", main="Rice ", xlab= " Area experiencing climatic risks (in %)")
lines(density(stats[,3]), lwd=3, col= "#958A86")
legend("topleft", legend=c("1967-1990",  "1991-2012"),bty= "n", cex=1, fill=c("#2D607D", "#958A86"),
       box.col = c("black", "black"))


ecdfPlot(stats[,1], ecdf.col="#2D607D", ecdf.lwd= 3, xlim=c(0,100),xaxs="i", main="Rice ", xlab= " Area experiencing climatic risks (in %)")
ecdfPlot(stats[,3], add=T, ecdf.lwd=3, ecdf.col= "#958A86")
legend("topleft", legend=c("1967-1990",  "1991-2012"),bty= "n", cex=1, fill=c("#2D607D", "#958A86"),
       box.col = c("black", "black"))
