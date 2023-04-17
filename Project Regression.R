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
n=length(y)
t <- qt(alpha/2, n - 2,lower.tail = F)
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
print(paste0("B1 is between interval " , data.frame(B1)))
#----------------------Mohamed Hassan and Safy Fathy--------------------------#
#-----------------------------------------------------------------------------#

#--------------------Abdelrhman Ashraf and Abdullah Hussein--------------------#
#------------------------------------------------------------------------------#

Xn = as.numeric(readline("Enter the X for mean response: "))
RatX=B0+B1*Xn
RatX

alpha = 0.05
t=qt(alpha/2, length(x)-2,lower.tail = F)
t

CL = as.numeric((1-alpha)*100)
L= RatX - t * sqrt(MSE*(1/length(x)+(Xn-Xbar)^2/Sxx))
U=RatX+ t * sqrt(MSE*(1/length(x)+(Xn-Xbar)^2/Sxx))

print(paste("Confidence Intervat for mean response at confidence level", CL,"%"))
print(paste("[",L, ",",U,"]"))

Xnew = as.numeric(readline("Enter the X for new observation: "))
RatXnew=B0+B1*Xnew
RatXnew

alpha = 0.05
t=qt(alpha/2, length(x)-2,lower.tail = F)
t

CL = as.numeric((1-alpha)*100)
Lnew= RatXnew - t * sqrt(MSE*(1+1/length(x)+(Xnew-Xbar)^2/Sxx))
Unew= RatXnew + t * sqrt(MSE*(1+1/length(x)+(Xnew-Xbar)^2/Sxx))

print(paste("Confidence Intervat for new observation at confidence level", CL,"%"))
print(paste("[",Lnew, ",",Unew,"]"))

#--------------------Abdelrhman Ashraf and Abdullah Hussein--------------------#
#------------------------------------------------------------------------------#