library(randomForest)
library(grf)
library(BART)
library(surfin)
library(foreach)
library(doParallel)
cl = makeCluster(32) 
registerDoParallel(cl)

simualtion = function(n,p,s,num_iteration,true_model_name){
  foreach (j = 1:num_iteration, .combine = c) %dopar%{
    
    library(randomForest)
    library(grf)
    library(BART)
    library(surfin)
    
    source("utils.R")
    fart_result = rep(0,13)
    bart_result = rep(0,13)
    rf_result = rep(0,7)
    boot_result = rep(0,7)
    ##############generate data######################
    X = matrix(runif(n*p), n, p)
    Y = generate_response(X, true_model_name)+s*rnorm(n)
    X_test = matrix(runif(p), 1, p)
    true_value = generate_response(X_test, true_model_name)
    Y_test =true_value+s*rnorm(1)
    ###############fart#############################
    start_time = Sys.time()
    result = generate_fiducial_sample(X, Y, X_test)
    end_time=Sys.time()
    fart_result[7] = end_time-start_time
    fart_result[1:6] = summarize_ci_fiducial_bart(as.matrix(result[[1]]), true_value)
    fart_result[8:13] = summarize_ci_fiducial_bart(t(as.matrix(result[[2]])), s)
    
    ##################ij############################
    start_time=Sys.time()
    r.forest = regression_forest(X, Y, num.trees = 1000)
    r.pred = predict(r.forest, X_test, estimate.variance = TRUE)
    end_time=Sys.time()
    rf_result[7] = end_time-start_time
    rf_result[1:6] = summarize_ci_rest(true_value, r.pred$predictions, sqrt(r.pred$variance.estimates))
    
    ###########bart#################################
    start_time = Sys.time()
    post = wbart(X,Y)
    bart_pred = predict(post, X_test)
    end_time=Sys.time()
    bart_result[7] = end_time-start_time
    bart_result[1:6] = summarize_ci_fiducial_bart(t(as.matrix(bart_pred)), true_value)
    bart_result[8:13] = summarize_ci_fiducial_bart(t(as.matrix(post$sigma)), s)
    
    ################bootstrap######################################
    start_time=Sys.time()
    bootstrap_fit = forest(X,Y,var.type="ustat",B=25,ntree=5000)
    temp = predict(bootstrap_fit,rbind(X_test,X_test),individualTrees = T)
    u_test = temp$predicted
    u_test_all = temp$predictedAll
    ustat = forest.varU(u_test_all,bootstrap_fit)
    end_time=Sys.time()
    boot_result[7] = end_time-start_time
    boot_result[1:6] = summarize_ci_rest(true_value, ustat$y.hat[1], sqrt(ustat$var.hat[1]))
    c(fart_result,bart_result,rf_result,boot_result)
  }
}

num_iteration = 1000
s = 1

n = 50
p = 2
s = 1
true_model_name = 'cosine'
result = simualtion(n,p,s,num_iteration,true_model_name)
save(result, file = 'cosine_50_2.RData')

n = 200
s = 1
result = simualtion(n,p,s,num_iteration,true_model_name)
save(result, file = 'cosine_200_2.RData')

n = 50
p = 50
s = 1
true_model_name = 'xor'
result = simualtion(n,p,s,num_iteration,true_model_name)
save(result, file = 'xor_50_50.RData')

n = 200
s = 1
result = simualtion(n,p,s,num_iteration,true_model_name)
save(result, file = 'xor_200_50.RData')

n = 50
p = 500
s = 1
true_model_name = 'and'
result = simualtion(n,p,s,num_iteration,true_model_name)
save(result, file = 'and_50_500.RData')

n = 200
s = 1
result = simualtion(n,p,s,num_iteration,true_model_name)
save(result, file = 'and_200_500.RData')

source("utils.R")

write_table("table_1")
write_table("table_2")
write_table("table_3")
write_table("table_4")
write_table("table_5")
write_table("table_6")