#Hw3- Question5
library(tidyverse)
library(ggplot2)
options(digits = 5)
n = 100 # 點數
set.seed(1)
sin_x<-sin(seq(0, 2*pi, length.out = n))
esp<-rnorm(n=n, mean = 0, sd=0.09)
y = sin_x+esp
par(mfrow=c(1,2))
plot(sin_x,main = "sin(x)",ylab = "f(x)",xlab="x")
plot(y,main = "sin(x)+esp",ylab = "f(x)",xlab="x") 

##spline smooth

mse<-function(a,b){         #mse function
  mean((a-b)^2)
} 
length(unique(y))  #y內樣本是否為唯一值, 此為100即均為唯一

h<-c()
for(i in 2:100){        # df 範圍為(1,nx] , nx the number of unique x values 
  spline<-smooth.spline(y,df=i)
  k<-mse(spline$y,sin_x)
  h<-c(h,k)
}
which(h==min(h)) #找df值使mse達最小  ,df=8,  mse=0.00038

spline<-smooth.spline(y,df=8)
par(mfrow=c(1,2))
plot(spline,main = "spline_df=8",ylab = "f(x)",xlab="x")
plot(sin_x,main = "sin(x)+esp",ylab = "f(x)",xlab="x")

##lowess smooth

p<-c()
for (i in seq(0.01, 1, length = 100)) {
  r<-mse(lowess(y,f=i)$y,sin_x)
  p<-c(p,r)
}
which(p==min(p))  #找f使其mse達最小,f=17*0.1 ,mse=0.00065
mse(lowess(y,f=0.17)$y,sin_x)
lowess_f<-lowess(y,f=0.17)
par(mfrow=c(1,2))
plot(lowess_f,main = "lowess_f=0.17",ylab = "f(x)",xlab="x")
plot(sin_x,main = "sin(x)",ylab = "f(x)",xlab="x")  #lowess_f=0.17 與 sin(x) 作圖比較

##running mean smooth
require(igraph)
rn<-running_mean(y, binwidth=2) # binwidth取2 原本100個點只會生成99個
length(rn)  #99
runmean<-c(y[1],rn) #把原始起點加入，使為100個點
par(mfrow=c(1,2))
plot(runmean,main = "running mean",ylab = "f(x)",xlab="x")
plot(sin_x,main = "sin(x)",ylab = "f(x)",xlab="x")  #lowess_f=0.15 與 sin(x) 作圖比較

##圖形比較
par(mfrow=c(2,2))
plot(sin_x,main = "sin(x)",ylab = "f(x)",xlab="x")
plot(spline,main = "spline",ylab = "f(x)",xlab="x")
plot(lowess_smooth,main = "lowess",ylab = "f(x)",xlab="x")
plot(runmean,main = "running mean",ylab = "f(x)",xlab="x")

##模擬1000次進行MSE比較
s<-c();l<-c();ru<-c()
for(i in 1:1000){
  esp<-rnorm(n=n, mean = 0, sd=0.09)
  y = sin_x+esp
  spline<-smooth.spline(y,df=8)
  lowess_f<-lowess(y,f=0.15)
  rn<-running_mean(y, binwidth=2)
  runmean<-c(y[1],rn)
  ms<-mse(spline$y,sin_x) 
  ml<-mse(lowess(y,f=0.15)$y,sin_x)
  mn<-mse(runmean,sin_x)
  s<-c(m,ms)
  l<-c(l,ml)
  rn<-c(rn,mn)
}

mse_spline<-mean(s)
mse_lowess<-mean(l)
mse_runmean<-mean(rn)
mse_value <- rbind(mse_spline,mse_lowess,mse_runmean)
rownames(mse_value) <- c("Spline","lowess","runmean")
colnames(mse_value) <- "mse"
kable(mse_value,"html") %>% 
  kable_styling(bootstrap_options = 'striped', full_width = F) %>% 
  add_header_above(c(" " = 1, "compare mse" = 1))


if (min(mse_spline,mse_lowess,mse_runmean)==mse_spline) {
  print('Spline smoothing is the best')
} else if(min(mse_spline,mse_lowess,mse_runmean)==mse_lowess) {
  print('Lowess smoothing is the best')
} else  {
  print('running mean smoothing is the best')
}     
#"Spline smoothing is the best"