# Write your model
# Simple linear regression
# Check list
# Sxx,Syy,Sxy(Checked)(Mahmoud and Zyad).
# Beta 1,Beta0(Checked)(Mahmoud and Zyad).
# SSR,SSE,SST,MSR,MSE,MST(checked)(Bisho and Hamdi).
# Anova table(Checked)(ziad and ali).
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
Sxx<- round(sum(x^2) -n*(xbar^2),2)
Syy <- round(sum(y^2) -n*(ybar^2),2)
Sxy <- round(sum(x*y) -n*xbar*ybar,2)
# Calculating intercept(Beta Node) and Slope(Beta 1)
Beta_1 <- round(Sxy / Sxx,2)
Beta_0 <- round(ybar - Beta_1*xbar,2)
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
# calculate F0
F0 <- MSR / MSE
#DEGREE OF FREEDOM 
DFR=1
DFE=n-2
DFT= DFR+DFE
#calculating Mean sum squares(regression,error)
MSR<-SSR/DFR
MSE<-SSE/DFE
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
Confidence_Interval_of_B1 = function(C){
  c = 1-((1-C)/2)
  t = qt(c,df=(n-2))
  margin =  t * sqrt(MSE/Sxx)
  lower_bound = Beta_1-margin
  upper_bound = Beta_1+margin
  CI = c(lower_bound,upper_bound)
  return(CI)
  
}
B1 = Confidence_Interval_of_B1(0.95)
print(paste0("B1 is between interval " , data.frame(B1)))


Confidence_Interval_of_B0 = function(C){
  c = 1-((1-C)/2)
  t = qt(c,df=(n-2))
  margin =  t * sqrt(MSE*((1/n)+(xbar^2/Sxx)))
  lower_bound = Beta_0-margin
  upper_bound = Beta_0+margin
  CI = c(lower_bound,upper_bound) 
  return(CI)
  
}

B0 = Confidence_Interval_of_B0(0.95)
print(paste0("B0 is between interval " , data.frame(B0) ))
#----------------------Mohamed Hassan and Safy Fathy--------------------------#
#-----------------------------------------------------------------------------#
