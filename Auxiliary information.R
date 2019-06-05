# A pilot scheme for studying milk yield, breeds, feeding and management practice of cattle and buffaloes were
# conducted in eastern Uttar Pradesh during 1957-1958. The given data below presents total number of milch cows 
# in the 19 selected villages of dry region as enumerated in the rainy season 1957 and as given by Census, 1956.
setwd(dir = "D:\\STUDY PROCESS\\PG Project")
Si.No.Villages <- 1:19
RainySeason_1957 <- c(35,38,71,4,63,4,14,7,66,44,8,229,27,30,29,29,97,30,40)

Census_1956 <- c(47,46,253,19,121,4,5,7,50,162,9,256,74,28,41,27,25,40,66)
Data <- data.frame(Si.No.Villages,RainySeason_1957,Census_1956);Data
#write.csv(Data,"Naturaldata.csv")
Pop_Ybar = mean(RainySeason_1957);Pop_Ybar
Pop_Xbar = mean(Census_1956);Pop_Xbar

# Let Xi = Census_1956, and Yi = RainySeason_1957
Xi = Census_1956;Yi = RainySeason_1957;  Xi;Yi


# Drawing samples out of these population
factorial(19)/(factorial(19-5)*factorial(5))

samples_X <- t(combn(x = Xi,m = 5))
samples_Y <- t(combn(x = Yi,m = 5))
cor(Xi,Yi)

s.cor <- c()
for(i in 1:nrow(samples_X)){
  s.cor[i] <- cor(samples_X[i,],samples_Y[i,])
}
coR <- as.data.frame(table(round(s.cor,1)))
colnames(coR) <- c("sample_correlations","Frequency");coR
prop.table(table(s.cor >= 0.7))
#write.csv(coR,"cor.csv")
# Just performing a test
#
X_bars <- rowMeans(samples_X)                        #
par(mfrow=c(1,2))                                    #
plot(y = X_bars,x = rep(mean(Xi),11628))             #
lines(Xi,Xi)                                         #
abline(a=mean(X_bars),0)
abline(a=quantile(X_bars)[2],0,col="red")
abline(a=quantile(X_bars)[4],0,col="red")
boxplot(X_bars)                                      #
prop.table(table(X_bars>=quantile(X_bars)[2]         #
                 & X_bars<=quantile(X_bars)[4]))     #
par(mfrow=c(1,1))                                    #
data <- data.frame(Yi,Xi)                            #
library(ggplot2)                                     #
ggplot(data = data,aes(x = Xi,y = Yi))+              #
  geom_point()+geom_density2d(aes(x = Xi,y=Yi))      #
                                                     #   
#
yi.s <- samples_Y
xi.s <- samples_X
xi.m <- rowMeans(xi.s)
beta <- c()
for (i in 1:nrow(yi.s)){
  beta[i] <- coef(lm(yi.s[i,]~xi.s[i,]))[2]
}

yi.m <- rowMeans(yi.s);IQR(yi.m) # ----------------------------------mean of all yi_s samples ( For sample mean estimator) 
yi.p <- (yi.m*xi.m)/mean(Xi);IQR(yi.p) # --------------------------  ( For product estimator) 
yi.r <- (yi.m*mean(Xi))/xi.m ;IQR(yi.r) # --------------------------  ( For ratio estimator) 
yi.e <- yi.m*exp((mean(Xi)-xi.m)/(mean(Xi)+xi.m));IQR(yi.e) # -----  ( For exponential estimator) 
yi.sq <- yi.m*(mean(Xi)/xi.m)^.5  ;IQR(yi.sq) # --
yi.reg <- yi.m + (beta*(Pop_Xbar-xi.m));IQR(yi.reg)

#_______________________________________
par(mfrow=c(1,2))                                    
plot(x = rep(mean(Xi),11628),yi.m)
abline(a=mean(yi.m),0)
abline(a=mean(Yi),0,col="green")
abline(a=quantile(yi.m)[2],0,col="red")
abline(a=quantile(yi.m)[4],0,col="red")
boxplot(yi.m)
IQR(yi.m)
max(yi.m)
#_______________________________________
par(mfrow=c(1,2))                                    
plot(x = rep(mean(Xi),11628),yi.r)
abline(a=mean(yi.r),0)
abline(a=mean(Yi),0,col="green")
abline(a=quantile(yi.r)[2],0,col="red")
abline(a=quantile(yi.r)[4],0,col="red")
boxplot(yi.r)
IQR(yi.r)


#_______________________________________
par(mfrow=c(1,2))                                    
plot(x = rep(mean(Xi),11628),yi.p)
abline(a=mean(yi.p),0)
abline(a=mean(Yi),0,col="green")
abline(a=quantile(yi.p)[2],0,col="red")
abline(a=quantile(yi.p)[4],0,col="red")
boxplot(yi.p)
IQR(yi.p)
min(yi.p)
max(yi.p)
#_______________________________________
par(mfrow=c(1,2))                                    
plot(x = rep(mean(Xi),11628),yi.e)
abline(a=mean(yi.e),0)
abline(a=mean(Yi),0,col="green")
abline(a=quantile(yi.e)[2],0,col="red")
abline(a=quantile(yi.e)[4],0,col="red")
boxplot(yi.e)
IQR(yi.e)
min(yi.e)
max(yi.e)

#_______________________________________
par(mfrow=c(1,2))                                    
plot(x = rep(mean(Xi),11628),yi.sq)
abline(a=mean(yi.sq),0)
abline(a=mean(Yi),0,col="green")
abline(a=quantile(yi.sq)[2],0,col="red")
abline(a=quantile(yi.sq)[4],0,col="red")
boxplot(yi.sq)
IQR(yi.sq)
min(yi.sq)
max(yi.sq)

#_______________________________________
par(mfrow=c(1,2))                                    
plot(x = rep(mean(Xi),11628),yi.reg)
abline(a=mean(yi.reg),0)
abline(a=mean(Yi),0,col="green")
abline(a=quantile(yi.reg)[2],0,col="red")
abline(a=quantile(yi.reg)[4],0,col="red")
boxplot(yi.reg)
IQR(yi.reg)
max(yi.reg)

estimates <- data.frame(yi.m,yi.r,yi.p,yi.e,yi.sq,yi.reg)

summary_est <- sapply(estimates, summary)
IQR_est <- sapply(estimates,IQR)
range_est <- diff(sapply(estimates, range))
estimate.tables <-rbind(summary_est,IQR_est,range_est) 
#write.csv(estimate.tables,"summaryestimator.csv")

