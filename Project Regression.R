# Write your model
# Simple linear regression
# Check list
# Sxx,Syy,Sxy(Checked)(Mahmoud and Zyad).
# Beta 1,Beta0(Checked)(Mahmoud and Zyad).
# SSR,SSE,SST,coeffecient(correlation, determination)(checked)(Bisho and Hamdi).
# MSR,MSE,F0,Anova table,f-test(Checked)(ziad and ali).
# Confidence interval for estimator at given significant level(Checked)(gaber and safy).
# Calculation of C.l. for mean response and new observation(Gowely and Ashraf).
# Scatter plot contain fitted line(Checked)(Mahmoud).
#----------------------Mahmoud Essam and Zyad Ashraf--------------------------#
Data <- data.frame(
  x = c(1,2,4,6,8),
  y = c(4,3,1,2,0)
)
# Getting needed data
x <- Data$x
y <- Data$y
n <- length(x)
xbar<-mean(x)
ybar<-mean(y)
# Calculating Sxx and Sxy and Syy
Sxx<- sum(x^2) -n*(xbar^2)
Syy <- sum(y^2) -n*(ybar^2)
Sxy <- sum(x*y) -n*xbar*ybar
# Calculating intercept(Beta Node) and Slope(Beta 1)
Beta_1 <- Sxy / Sxx
Beta_0 <- ybar - Beta_1*xbar
# Plotting data 
plot(x,y,main = "Fitted Model",
     xlab ="Features",
     ylab="target")
abline(a=Beta_0,b=Beta_1,col="red",lwd=3)
#----------------------Mahmoud Essam and Zyad Ashraf--------------------------#
#-----------------------------------------------------------------------------#
#----------------------mohamed hamdy and bishoy-------------------------------#
#calculating sum squares (regression, error,total)
SSR <-Beta_1^2*Sxx
SST <-Syy
SSE <-SST-SSR
#calculating coeffecient(correlation, determination)
Rsquare<-as.numeric(SSR/SST)
print(paste('dependent variable explained by an independent variable in regression model ',Rsquare*100,'%'))
if(Beta_1<0){
  r<-0-sqrt(Rsquare)
  print(paste("the model has negative correlation about ",r))
  
}else{
  r<-sqrt(Rsquare)
  print(paste("the model has positive correlation about ",r))
}
#----------------------mohamed hamdy and bishoy-------------------------------#
#-----------------------------------------------------------------------------#
#----------------------Ali elsayed  and ziad ashraf --------------------------#
#DEGREE OF FREEDOM 
DFR=1
DFE=n-2
DFT= DFR+DFE
#calculating Mean sum squares(regression,error)
MSR<-SSR/DFR
MSE<-SSE/DFE
# calculate F0
F0 <- MSR / MSE
#anova table
ANOVA=matrix(c(SSR,SSE,SST,DFR,DFE,DFT,MSR,MSE,"",F0,"",""),ncol=4)
row.names(ANOVA)=c("Treatment","Error","Total")
colnames(ANOVA)=c("Sum square","Degree of freedom","Mean sum square","F table")
ANOVA<- as.table(ANOVA)
ANOVA
# calculate f_test 
alpha <- as.numeric(readline("Enter significance level : "))
Fc<- qf(alpha, DFR, DFE)
if (F0 > Fc) {
  print("Reject H0, There's relation between X and Y")
} else {
  print("dont reject H0, There's no relation")
}
#----------------------Ali elsayed  and ziad ashraf --------------------------#
#-----------------------------------------------------------------------------#
#----------------------Mohamed Hassan and Safy Fathy--------------------------#
#Confidence Interval For B0 
n=length(y)
t <- qt(alpha/2, n - 2,lower.tail = F)
Upper_B0=Beta_0+t*sqrt(MSE * ( 1/n + ( (mean(x))^2 / Sxx ) ) )
Lower_B0=Beta_0-t*sqrt(MSE * ( 1/n + ( (mean(x))^2 / Sxx ) ) )
cat("Confidence Interval for B0 is :",Lower_B0," < B0 < ",Upper_B0,"\n")

#Confidence Interval for B1 
Upper_B1=Beta_1+(t*sqrt(MSE/Sxx))
Lower_B1=Beta_1-(t*sqrt(MSE/Sxx))
cat("Confidence Interval for B1 is :",Lower_B1," < B1 < ",Upper_B1,"\n")
z<- lm(y~x,data=Data)
confint(z)
#----------------------Mohamed Hassan and Safy Fathy--------------------------#
#-----------------------------------------------------------------------------#
