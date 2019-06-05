#_________________                 
#_________________ Generating Biveriate Populations with values Rho = 0.25,0.5,0.75,0.85,0.95  
#_________________ and mean(X) = 15 , mean(Y) = 30, var(X) = 9 ,var(Y) = 25                   
#_________________  

# Creating the function for Generating biveriate normal value.
#  setwd("D:\\STUDY PROCESS\\PG Project")

library(mvtnorm)
library(MASS)
Rbvn <- function(muX =15,muY =30,rho,sX=3,sY=5,N) {
  mu <- c(muX,muY)
  sigma <- matrix(c(sX^2,sX*sY*rho,sX*sY*rho,sY^2),nrow = 2,ncol = 2)
  bvn <- mvrnorm(N, mu = mu, Sigma = sigma )
  colnames(bvn) <- c("X","Y")
  bvn
}


# Using the function to generate all desired populations

rho5 <- c(0.25,0.5,0.75,0.85,0.95) # 5 Rho values 
N3 <- c(50,75,100) # 3 Population Sizes
# seeds <- c(151,223,224,322,296,9,76,121,123,472,78,81,83,106,489,488)
Pop_list <- list(NULL)
k <- 10 #using the seed form 10 onwards
count <- 1


for(i in rho5){
  for (j in N3) {
    
    set.seed(k)
    Rbvn(rho = i,N = j) -> Pop_list[[count]]
    k <- k+1
    count <- count+1
    
  }
}

Pop_list # Here is the population list 

# Matrix containing the Population Mean Variance coefficients and ....

Pop_Mat <- matrix(0,15,7)
length(Pop_list[[1]][,1])
colnames(Pop_Mat) <- c("N","Rho","Xbar","Ybar","VarX","VarY","B")
for (i in 1:15) {
  Pop_Mat[i,1] <- length(Pop_list[[i]][,1])
  Pop_Mat[i,2] <- cor(Pop_list[[i]][,1],Pop_list[[i]][,2])
  Pop_Mat[i,3] <- mean(Pop_list[[i]][,1])
  Pop_Mat[i,4] <- mean(Pop_list[[i]][,2])
  Pop_Mat[i,5] <- var(Pop_list[[i]][,1])
  Pop_Mat[i,6] <- var(Pop_list[[i]][,2])
  Pop_Mat[i,7] <- lm(Pop_list[[i]][,2]~
                       Pop_list[[i]][,1])$coeff[2]
}
Pop_Mat
write.csv(x =Pop_Mat ,"Population_Matrix.csv")
#_________________                 
#_________________ Sample Draw                                                                 
#_________________                                                                            
#_________________  

Sample_list <- list(NULL)
cnt <- 1

for(s in c(5,10,15)){
  for (i in 1:15) {
    set.seed(i)
    sample(1:nrow(Pop_list[[i]]),size = s) -> n
    Pop_list[[i]][n,] -> Sample_list[[cnt]]
    cnt <- cnt+1
  }
}
Sample_list # The 45 Sample drwan from 15 populations first 15 samples for sample size 5 than another 15 for n = 10 so on  n = 15

# Matrix containing the sample mean , sample variance, sample correlation coefficient

Sample_Mat <- matrix(0,45,8)
colnames(Sample_Mat) <- c("N","n","r","xbar","ybar","varx","vary","b")
for (i in 1:45) {
  Sample_Mat[i,2] <- length(Sample_list[[i]][,1])
  Sample_Mat[i,3] <- cor(Sample_list[[i]][,1],Sample_list[[i]][,2])
  Sample_Mat[i,4] <- mean(Sample_list[[i]][,1])
  Sample_Mat[i,5] <- mean(Sample_list[[i]][,2])
  Sample_Mat[i,6] <- var(Sample_list[[i]][,1])
  Sample_Mat[i,7] <- var(Sample_list[[i]][,2])
  Sample_Mat[i,8] <- lm(Sample_list[[i]][,2]~Sample_list[[i]][,1])$coeff[2]
}
Sample_Mat[,1] <- rep(Pop_Mat[,1],3)

Sample_Mat # Shows the sample mean , sample variance, sample correlation coefficient
write.csv(x = Sample_Mat,file = "Sample_Matrix.csv")

# Comparison with sample mean estimator, product estimator and Ratio estimator

D <- rbind(Pop_Mat,Pop_Mat,Pop_Mat)
Comparison_mat <- matrix(0,45,10)
colnames(Comparison_mat) <- c("Rho","N","n","Y_bar","y_bar","yexp_bar","yr_bar","ysq_bar","yreg_bar","Appropriate")
for (i in 1:45) {
  Comparison_mat[i,1] <- D[i,2]
  Comparison_mat[i,2] <- D[i,1]
  Comparison_mat[i,3] <- Sample_Mat[i,2]
  Comparison_mat[i,4] <- D[i,4]
  Comparison_mat[i,5] <- Sample_Mat[i,5]
  Comparison_mat[i,6] <- Sample_Mat[i,5]*exp((D[i,3]-Sample_Mat[i,4])/(D[i,3]+Sample_Mat[i,4])) # column Sample_Mat[,5] is ybar Change to exponential
  Comparison_mat[i,7] <- (Sample_Mat[i,5]*D[i,3]) / Sample_Mat[i,4]
  Comparison_mat[i,8] <- Sample_Mat[i,5]*(D[i,3] / Sample_Mat[i,4])^0.5
  Comparison_mat[i,9] <- Sample_Mat[i,5] + (   (coef(lm(Sample_list[[i]][,"Y"]~Sample_list[[i]][,"X"]))[2])  *  (  D[i,3]-Sample_Mat[i,4]  )    )
  Comparison_mat[i,10] <- which.min(abs(Comparison_mat[i,5:9]-Comparison_mat[i,4]))
}# add squreroot

Comparison_mat
write.csv(Comparison_mat,"Comparison_Matrix.csv")

attach(as.data.frame(Comparison_mat))
Comparison_mat
Appropriate

Comparison_mat[,]

attach(G)

G <- as.data.frame(Comparison_mat)
G[ N >=100 & Rho >= 0.7,c("N","n","Rho","Appropriate")] -> l
l
write.csv(l,"Highly_Correlated")
tabulate(l[,"Appropriate"])

kk <-tabulate(Comparison_mat[,"Appropriate"])
write.csv(x = kk,file = "appro.freq.csv")
k<-table(round(Comparison_mat[,"Rho"],3),Comparison_mat[,"Appropriate"])

write.csv(x = k,file = "Appropriate wrt Rho2.csv")

plot(Comparison_mat[,"Rho"]~Comparison_mat[,"Appropriate"],col=c(Appropriate))
abline(a = 0.5,b = 0)

mosaicplot(k,color = c("yellow","lightblue","green","pink","forestgreen"))
