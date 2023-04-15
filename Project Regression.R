# Write your model
# Simple linear regression
# Check list
# Sxx,Syy,Sxy(Checked)(Mahmoud and Zyad).
# Beta 1,Beta0(Checked)(Mahmoud and Zyad).
# SSR,SSE(Bisho and Hamdi).
# Anova table(ziad and ali).
# Confidence interval for estimator at given significant level(gaber and safy).
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
# Calculating Sxx and Sxy and Syy
Sxx <- sum(x^2) -length(x)*(mean(x)^2) 
Syy <- sum(y^2) -length(y)*(mean(y)^2)
Sxy <- sum(x*y) -length(x)*mean(x)*mean(y)
# Calculating intercept(Beta Node) and Slope(Beta 1)
Beta_1 <- Sxy / Sxx
Beta_0 <- mean(y) - Beta_1*mean(x)
# Plotting data 
plot(x,y)
abline(a=Beta_0,b=Beta_1)
#----------------------Mahmoud Essam and Zyad Ashraf--------------------------#

