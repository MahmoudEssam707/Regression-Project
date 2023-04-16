# Write your model
# Simple linear regression
# Check list
# Sxx,Syy,Sxy(Checked)(Mahmoud and Zyad).
# Beta 1,Beta0(Checked)(Mahmoud and Zyad).
# SSR,SSE,SST,MSR,MSE,MST(Bisho and Hamdi).
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
# Calculating Sxx and Sxy and Syy
Sxx <- round(sum(x^2) -n*(mean(x)^2),2)
Syy <- round(sum(y^2) -n*(mean(y)^2),2)
Sxy <- round(sum(x*y) -n*mean(x)*mean(y),2)
# Calculating intercept(Beta Node) and Slope(Beta 1)
Beta_1 <- round(Sxy / Sxx,2)
Beta_0 <- round(mean(y) - Beta_1*mean(x),2)
# Plotting data 
plot(x,y)
abline(a=Beta_0,b=Beta_1)
#----------------------Mahmoud Essam and Zyad Ashraf--------------------------#
#-----------------------------------------------------------------------------#
#----------------------Ali elsayed  and ziad ashraf --------------------------#
F0 <- MSR / MSE # calculate F0
#DEGREE OF FREEDOM 
DFR=1
DFE=n-2
DFT= DFR+DFE
#MEAN SQUARE
MSR=SSR
MSE=SSE/DFE
#anova table
ANOVA=matrix(c(SSR,SSE,SST,DFR,DFE,DFT,MSR,MSE,"",F0,"",""),ncol=4)
row.names(ANOVA)=c("Treatment","Error","Total")
colnames(ANOVA)=c("Sum square","Degree of freedom","Mean sum square","F table")
ANOVA<- as.table(ANOVA)
# calculate f_test 
alpha= as.numeric(readline("enter the alpha:"));
Fc<- qf(alpha, DFR, DFE)
if (F0 >Fc) {
  print("Reject H0 and has relation")
} else {
  print("dont reject H0 has no relation")
}

#----------------------Ali elsayed  and ziad ashraf --------------------------#
#-----------------------------------------------------------------------------#
#----------------------Mohamed Hassan and Safy Fathy--------------------------#
Confidence_Interval_of_B1 = function(C){
  c = 1-((1-C)/2)
  t = qt(c,df=(n-2))
  margin =  t * sqrt(MSE/Sxx)
  lower_bound = B1_hat-margin
  upper_bound = B1_hat+margin
  CI = c(lower_bound,upper_bound)
  return(CI)
  
}
B1 = Confidence_Interval_of_B1(0.95)
print(paste0("B1 is between interval " , data.frame(B1)))


Confidence_Interval_of_B0 = function(C){
  c = 1-((1-C)/2)
  t = qt(c,df=(n-2))
  margin =  t * sqrt(MSE*((1/n)+(x_bar^2/Sxx)))
  lower_bound = B0_hat-margin
  upper_bound = B0_hat+margin
  CI = c(lower_bound,upper_bound) 
  return(CI)
  
}

B0 = Confidence_Interval_of_B0(0.95)
print(paste0("B0 is between interval " , data.frame(B0) ))
#----------------------Mohamed Hassan and Safy Fathy--------------------------#
#-----------------------------------------------------------------------------#