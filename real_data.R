library(readxl)
library(randomForest)
library(grf)
library(BART)
library(surfin)
library(MASS)

k = 100
airfoil_accuracy = rep(0,k)
airfoil_width = rep(0,k)
auto_mpg_accuracy = rep(0,k)
auto_mpg_width = rep(0,k)
ccpp_accuracy = rep(0,k)
ccpp_width = rep(0,k)
boston_house_accuracy = rep(0,k)
boston_house_width = rep(0,k)
ccs_accuracy = rep(0,k)
ccs_width = rep(0,k)

source("utils.R")

for(repeat_time in 1:k){
  #########air foil#############
  data=read.delim("airfoil_self_noise.dat.txt", header=FALSE)
  data=data.frame(data)
  s=sample(seq(1,nrow(data)),nrow(data),replace=FALSE)
  train=data[s[1:1000],1:6]
  test=data[s[1001:nrow(data)],1:6]
  X=matrix(as.numeric(as.matrix(train[,1:5])),1000,5)
  Y=train[,6]
  X_test=matrix(as.numeric(as.matrix(test[,1:5])),503,5)
  Y_test=test[,6]
  ntest = nrow(X_test)
  
  mu_hat = generate_fiducial_sample_pi(X, Y, X_test)
  lower3_pred=apply(mu_hat,1,quantile,prob=0.025)
  upper3_pred=apply(mu_hat,1,quantile,prob=0.975)
 
  airfoil_accuracy[repeat_time]=length(which(Y_test>=lower3_pred &Y_test<=upper3_pred))/ntest
  airfoil_width[repeat_time]=mean(upper3_pred-lower3_pred)
  
  ##########boston###############
  data(Boston)
  data <- Boston
  ntrain=400
  ntest=106
  s=sample(seq(1,nrow(data)),nrow(data),replace=FALSE)
  train=data[s[1:ntrain],]
  test=data[s[(ntrain+1):nrow(data)],]
  X=matrix(as.numeric(as.matrix(train[,1:13])),ntrain,13)
  Y=train[,14]
  X_test=matrix(as.numeric(as.matrix(test[,1:13])),ntest,13)
  Y_test=test[,14]
  ntest = nrow(X_test)
  
  mu_hat = generate_fiducial_sample_pi(X, Y, X_test)
  lower3_pred=apply(mu_hat,1,quantile,prob=0.025)
  upper3_pred=apply(mu_hat,1,quantile,prob=0.975)
  
  boston_house_accuracy[repeat_time]=length(which(Y_test>=lower3_pred &Y_test<=upper3_pred))/ntest
  boston_house_width[repeat_time]=mean(upper3_pred-lower3_pred)
  
  ########ccs####################
  data=read_excel("Concrete_Data.xls")
  ntrain=750
  ntest=280
  s=sample(seq(1,nrow(data)),nrow(data),replace=FALSE)
  train=data[s[1:ntrain],]
  test=data[s[(ntrain+1):nrow(data)],]
  X=matrix(as.numeric(as.matrix(train[,1:8])),ntrain,8)
  Y=as.numeric(as.matrix(train[,9]))
  X_test=matrix(as.numeric(as.matrix(test[,1:8])),ntest,8)
  Y_test=as.numeric(as.matrix(test[,9]))
  ntest = nrow(X_test)
  
  mu_hat = generate_fiducial_sample_pi(X, Y, X_test)
  lower3_pred=apply(mu_hat,1,quantile,prob=0.025)
  upper3_pred=apply(mu_hat,1,quantile,prob=0.975)
  
  ccs_accuracy[repeat_time]=length(which(Y_test>=lower3_pred &Y_test<=upper3_pred))/ntest
  ccs_width[repeat_time]=mean(upper3_pred-lower3_pred)
  
  ######ccpp###################
  data=read_excel("Folds5x2_pp.xlsx")
  ntrain=8000
  ntest=1568
  s=sample(seq(1,nrow(data)),nrow(data),replace=FALSE)
  train=data[s[1:ntrain],]
  test=data[s[(ntrain+1):nrow(data)],]
  X=matrix(as.numeric(as.matrix(train[,1:4])),ntrain,4)
  Y=as.numeric(as.matrix(train[,5]))
  X_test=matrix(as.numeric(as.matrix(test[,1:4])),ntest,4)
  Y_test=as.numeric(as.matrix(test[,5]))
  ntest = nrow(X_test)
  
  mu_hat = generate_fiducial_sample_pi(X, Y, X_test)
  lower3_pred=apply(mu_hat,1,quantile,prob=0.025)
  upper3_pred=apply(mu_hat,1,quantile,prob=0.975)
  
  ccpp_accuracy[repeat_time]=length(which(Y_test>=lower3_pred &Y_test<=upper3_pred))/ntest
  ccpp_width[repeat_time]=mean(upper3_pred-lower3_pred)
  
  #######auto mpg#################
  data=read.table("auto-mpg.data.txt", quote="\"", comment.char="")
  data=data.frame(data)
  colnames(data)=c('mpg','cylinders','displacement','horsepower','weight','acceleration','model_year','origin','car_name')
  data=data[-which(data$horsepower=='?'),]
  s=sample(seq(1,nrow(data)),nrow(data),replace=FALSE)
  train=data[s[1:314],1:8]
  test=data[s[315:nrow(data)],1:8]
  X=matrix(as.numeric(as.matrix(train[,2:8])),314,7)
  Y=train[,1]
  X_test=matrix(as.numeric(as.matrix(test[,2:8])),78,7)
  Y_test=test[,1]
  ntest = nrow(X_test)
  
  mu_hat = generate_fiducial_sample_pi(X, Y, X_test)
  lower3_pred=apply(mu_hat,1,quantile,prob=0.025)
  upper3_pred=apply(mu_hat,1,quantile,prob=0.975)
  
  auto_mpg_accuracy[repeat_time]=length(which(Y_test>=lower3_pred &Y_test<=upper3_pred))/ntest
  auto_mpg_width[repeat_time]=mean(upper3_pred-lower3_pred)
  print(repeat_time)
}
paste0(mean(airfoil_accuracy)*100,'  (',round(mean(airfoil_width),2),') & ',mean(auto_mpg_accuracy)*100,'  (',round (mean(auto_mpg_width),2),') & ',mean(ccpp_accuracy)*100,'  (',round (mean(ccpp_width),2),') & ',mean(boston_house_accuracy)*100,'  (',round (mean(boston_house_width),2),') & ',mean(ccs_accuracy)*100,'  (',round (mean(ccs_width),2),')  \\')

