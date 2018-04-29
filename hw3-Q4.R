#Question 4
library(tidyverse)
library(magrittr)
library(kableExtra)
library(knitr)
##
set.seed(101)
x = rbeta(100,2,3)    #隨機生成100個beta(2,3)
y = seq(0,1,length.out=100) #[0.1]間取100個等距
hist(x,breaks = 5,probability = T,ylim = c(0,2),main = "100 samples of Beta(2,3)",y="f(x)")
lines(y,dbeta(y,2,3),col="blue",lty=2)
rug(x)  # 把樣本點位置畫出
legend("right","Beta(2,3)",lty=2,col="blue")
#比較Kernel 函數
layout(matrix(1:3, ncol = 3))
hist(x, xlab = "x", ylab = "Frequency",
     probability = TRUE, main = "Gaussian kernel",
     border = "gray")
lines(density(x, width = 0.2), lwd = 2)
rug(x)
hist(x, xlab = "x", ylab = "Frequency",
     probability = TRUE, main = "Rectangular kernel",
     border = "gray")
lines(density(x, width = 0.2, window = "rectangular"), lwd = 2)
rug(x)
hist(x, xlab = "x", ylab = "Frequency",
     probability = TRUE, main = "Triangular kernel",
     border = "gray")
lines(density(x, width = 0.2, window = "triangular"), lwd = 2)
rug(x)
#Gaussian kernel 較為平滑


#density estimator function
set.seed(101)
x <- rbeta(100,2,3)
h=0.2
#histogram density estimator
hist_est=function(x,h){
  w=function(x,a,b){
    if (x<=b & x>=a) {return(1)}
    else {return(0)}}
  n=length(x)
  sx=seq(min(x),max(x),by=h)
  a=sx[-length(sx)]
  b=sx[-1]
  ni=NULL
  for (j in 1:length(a)){
    ni[j]=sum(x<=b[j] & x>=a[j])}
  t1=NULL
  for (i in sort(x)){
    t0=NULL
    for (j in 1:length(a)){
      wi=w(i,a[j],b[j])
      t0=c(t0,wi)}
    y=1/n*sum(ni/h*t0)
    t1=c(t1,y)}
  return(t1)}

# naive density estimator
naive_est=function(x,h){
  w=function(y){
    if (abs(y)<1) {return(1/2)}
    else {return(0)}}
  n=length(x)
  sx=seq(min(x),max(x),length=100)
  t1=NULL
  for (i in sx){
    t0=NULL
    for (j in x){
      wei=w((i-j)/h)
      t0=c(t0,wei)}
    y=1/n*sum(1/h*t0)
    t1=c(t1,y)}
  return(t1)}

# kernel density estimator
# norm
kernel_norm=function(x,h){
  w=function(y){ dnorm(y) }
  n=length(x)
  sx=seq(min(x),max(x),length=100)
  t1=NULL
  for (i in sx){
    t0=NULL
    for (j in x){
      wei=w((i-j)/h)
      t0=c(t0,wei)}
    
    y=1/n*sum(1/h*t0)
    t1=c(t1,y)}
  
  return(t1)}

## 資料處理
k <- sort(x)   #sort samples
k1 <- dbeta(k,2,3)
k2 <- hist_est(k,h)
k3 <- naiveest(k,h)
k4 <- kernel_norm(k,h)
data <- cbind(k,k1,k2,k3,k4) %>% as.data.frame()
colnames(data) <- c("sample","Beta(2,3)","hist","naive","kernal")
dat <- gather(data,key = "type",value = "value",2:5) #data 依照type排列
dat$type %<>% as.factor() #type轉成factor
## 作圖
ggplot(data = dat)+
  geom_histogram(mapping = aes(x=sample,y=..density..),color="white")+
  geom_line(mapping = aes(x=sample,y=value,color=type,group=type),size=1.2)+
  theme(legend.title = element_text(size=20, face="bold"),legend.text = element_text(size = 16, face = "bold"),legend.position = c(0.8,0.8))+
  theme(panel.grid =element_blank())

##可以發現 

mse<-function(a,b){         #mse function
  mean((a-b)^2)
} 

hist_mse<-mse(k1,k2)   #0.046
naive_mse<-mse(k1,k3)  # 0.128
kernel_mse<-mse(k1,k4) #0.174

mse_value <- rbind(hist_mse,naive_mse,kernel_mse)
rownames(mse_value) <- c("hist","navie","kernel")
colnames(mse_value) <- "mse"
kable(mse_value,"html") %>% 
  kable_styling(bootstrap_options = 'striped', full_width = F) %>% 
  add_header_above(c(" " = 1, "compare mse" = 1))

if (min(hist_mse,naive_mse,kernel_mse)==hist_mse) {
  print('histogram density estimate is the best')
} else if(min(hist_mse,naive_mse,kernel_mse)==naive_mse) {
  print('naive density estimate is the best')
} else  {
  print('kernel density estimate is the best')
}
#比較其MSE ，"histogram density estimate is the best"
