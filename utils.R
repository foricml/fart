library(randomForest)
library(grf)
library(BART)
library(surfin)

###true model
cosine = function(x){
  return(3*cos(pi*(x[1]+x[2])))
}

xor = function(x){
  a=b=0
  if(x[1]>0.6 |x[2]>0.6){
    a=1
  }
  if(x[3]>0.6| x[4]>0.6){
    b=1
  }
  return(5*a+b)
}

and = function(x){
  a = 0
  if(x[1]>0.3 & x[2]>0.3 & x[3]>0.3 & x[4]>0.3){
    a=1
  }
  return(10*a)
}

find_lable = function(x,nodes){
  i=1
  while(!nodes[[i]]$is_leaf){
    if(x[nodes[[i]]$split_variable]>nodes[[i]]$split_value){
      i=nodes[[i]]$right_child
    }else{
      i=nodes[[i]]$left_child
    }
  }
  return(i)
}

find_ni = function(label,train_label){
  return(length(which(train_label==label)))
}

toprob = function(p) {p = exp(p-max(p,na.rm=1)); p/sum(p)}

refit = function(prediction_label,prediction_index,len,nodes,Y){
  ### prediction_label: label of the data used for prediction
  ### prediction_index: index of the data used for prediction
  ### len: number of possible labels
  ### nodes: the corresponding tree
  result=rep(0,len)
  count=rep(0,len)
  ### add a new element-- parent
  nodes[[1]]$parent=0
  for(i in 1:length(nodes)){
    if(!nodes[[i]]$is_leaf){
      nodes[[nodes[[i]]$left_child]]$parent=i
      nodes[[nodes[[i]]$right_child]]$parent=i
    }
  }
  for(i in length(nodes):1){
    if(nodes[[i]]$is_leaf){
      index=prediction_index[which(prediction_label==i)]
      result[i]=sum(Y[index])
      count[i]=length(index)
    }else{
      result[i]=result[nodes[[i]]$left_child]+result[nodes[[i]]$right_child]
      count[i]=count[nodes[[i]]$left_child]+count[nodes[[i]]$right_child]
    }
  }
  for(i in 1:length(nodes)){
    if(count[i]==0){
      parent_id=nodes[[i]]$parent
      while(count[parent_id]==0){
        parent_id=nodes[[parent_id]]$parent
      }
      result[i]=result[parent_id]
      count[i]=count[parent_id]
    }
  }
  return(list(result=result/count,count=count))
}
summarize_ci_fiducial_bart = function(fiducial_sample, true_value){
  result = rep(0,6)
  lower = apply(fiducial_sample,1,quantile,prob=0.05)
  upper = apply(fiducial_sample,1,quantile,prob=0.95)
  result[1] = as.numeric(true_value>=lower & true_value<=upper)
  result[2] = upper-lower
  lower=apply(fiducial_sample,1,quantile,prob=0.025)
  upper=apply(fiducial_sample,1,quantile,prob=0.975)
  result[3] = as.numeric(true_value>=lower & true_value<=upper)
  result[4] = upper-lower
  lower=apply(fiducial_sample,1,quantile,prob=0.005)
  upper=apply(fiducial_sample,1,quantile,prob=0.995)
  result[5] = as.numeric(true_value>=lower & true_value<=upper)
  result[6] = upper-lower
  return(result)
}

summarize_ci_rest = function(true_value, predicted_value, sigma_hat){
  result = rep(0,6)
  result[1] =  as.numeric(abs(true_value-predicted_value)<sigma_hat*qnorm(0.95))
  result[2] = 2*sigma_hat*qnorm(0.95)
  result[3] =  as.numeric(abs(true_value-predicted_value)<sigma_hat*qnorm(0.975))
  result[4] = 2*sigma_hat*qnorm(0.975)
  result[5] =  as.numeric(abs(true_value-predicted_value)<sigma_hat*qnorm(0.995))
  result[6] = 2*sigma_hat*qnorm(0.995)
  return(result)
}

generate_response = function(X, true_model_name){
  if(true_model_name == 'cosine'){
    return(apply(X, 1, cosine))
  }else if(true_model_name == 'xor'){
    return(apply(X, 1, xor))
  }else{
    return(apply(X, 1, and))
  }
}

generate_fiducial_sample = function(X, Y, X_test){
  n = nrow(X)
  ntree = 500
  pred_test = matrix(0,1,ntree)
  sse = rep(0,ntree)
  nleaf = rep(0,ntree)
  train_label = matrix(0,n,ntree)
  test_label = matrix(0,1,ntree)
  n_i = matrix(0,1,ntree)
  nodes_list = c()
  sample_index = matrix(0,round(n/2),ntree)
  possible_node_size=seq(round(n/20),round(n/5))
  for(i in 1:ntree){
    r.forest = regression_forest(X, Y, num.trees = 1,min.node.size=sample(possible_node_size,1))
    sample_index[,i]=which(is.na(r.forest$predictions))
    r.pred=predict(r.forest,X[sample_index[,i],])
    sse[i]=sum((Y[sample_index[,i]]-r.pred$predictions)^2)
    nodes=get_tree(r.forest,1)
    nodes=nodes$nodes
    nodes_list=c(nodes_list,list(nodes))
    for(repeat_time in 1:length(nodes)){
      if(nodes[[repeat_time]]$is_leaf){
        nleaf[i]=nleaf[i]+1
      }
    }
    ptest=predict(r.forest, X_test)
    pred_test[,i] = ptest$predictions
    train_label[,i] = apply(X,1,find_lable,nodes=nodes)
    test_label[,i] = apply(X_test,1,find_lable,nodes=nodes)
    n_i[,i] = sapply(test_label[,i],find_ni,train_label=train_label[,i])
  }
  prob = lgamma((n/2-nleaf)/2)-log(pi)*((n/2-nleaf)/2)-log(sse)*(n/2-nleaf-1)/2-nleaf*log(n/2)/2
  prob = toprob(prob) 
  mu = matrix(0,1,1000)
  sigma = rep(0,1000)
  for( i in 1:1000){
    index=sample(seq(1,ntree),1,prob=prob)
    nodes=nodes_list[[index]]
    index_rest=seq(1,n)
    index_rest=index_rest[-sample_index[,index]]
    index_for_prediction=sample(index_rest,round(n/4))
    label_for_prediction=train_label[,index][index_for_prediction]
    refit_tree=refit(label_for_prediction,index_for_prediction,max(test_label[,index],train_label[,index]),nodes,Y)
    sigma[i]=sqrt(sse[index]/rchisq(1,n/2-nleaf[index]))
    mu[,i]=refit_tree$result[test_label[,index]]+sigma[i]/sqrt(refit_tree$count[test_label[,index]])*rnorm(1)
  }
  return(list(mu, sigma))
}

generate_fiducial_sample_pi = function(X, Y, X_test){
  n = nrow(X)
  n_test = nrow(X_test)
  ntree = 5000
  pred_test = matrix(0,n_test,ntree)
  sse = rep(0,ntree)
  nleaf = rep(0,ntree)
  train_label = matrix(0,n,ntree)
  test_label = matrix(0,n_test,ntree)
  n_i = matrix(0,n_test,ntree)
  nodes_list = c()
  sample_index = matrix(0,round(n/2),ntree)
  possible_node_size=seq(round(n/20),round(n/5))
  for(i in 1:ntree){
    r.forest = regression_forest(X, Y, num.trees = 1,min.node.size=sample(possible_node_size,1))
    sample_index[,i]=which(is.na(r.forest$predictions))
    r.pred=predict(r.forest,X[sample_index[,i],])
    sse[i]=sum((Y[sample_index[,i]]-r.pred$predictions)^2)
    nodes=get_tree(r.forest,1)
    nodes=nodes$nodes
    nodes_list=c(nodes_list,list(nodes))
    for(repeat_time in 1:length(nodes)){
      if(nodes[[repeat_time]]$is_leaf){
        nleaf[i]=nleaf[i]+1
      }
    }
    ptest=predict(r.forest, X_test)
    pred_test[,i] = ptest$predictions
    train_label[,i] = apply(X,1,find_lable,nodes=nodes)
    test_label[,i] = apply(X_test,1,find_lable,nodes=nodes)
    n_i[,i] = sapply(test_label[,i],find_ni,train_label=train_label[,i])
  }
  prob = lgamma((n/2-nleaf)/2)-log(pi)*((n/2-nleaf)/2)-log(sse)*(n/2-nleaf-1)/2-nleaf*log(n/2)/2
  prob = toprob(prob) 
  mu_pi = matrix(0,n_test,1000)
  sigma = rep(0,1000)
  for( i in 1:1000){
    index=sample(seq(1,ntree),1,prob=prob)
    nodes=nodes_list[[index]]
    index_rest=seq(1,n)
    index_rest=index_rest[-sample_index[,index]]
    index_for_prediction=sample(index_rest,round(n/4))
    label_for_prediction=train_label[,index][index_for_prediction]
    refit_tree=refit(label_for_prediction,index_for_prediction,max(test_label[,index],train_label[,index]),nodes,Y)
    sigma[i]=sqrt(sse[index]/rchisq(1,n/2-nleaf[index]))
    mu_pi[,i]=refit_tree$result[test_label[,index]]+sigma[i]*sqrt(1+(1/refit_tree$count[test_label[,index]]))*rnorm(n_test)
  }
  return(mu_pi)
}

summary = function(result, table_name){
  fart_result = result[1:13]
  bart_result = result[14:26]
  rf_result = result[27:33]
  boot_result = result[34:40]
  if(table_name == 'table_1'){
    return(paste0(fart_result[1]*100,'  (',round(fart_result[2],2),') & ',boot_result[1]*100,'  (',round (boot_result[2],2),') & ',rf_result[1]*100,'  (',round (rf_result[2],2),') & ',bart_result[1]*100,'  (',round (bart_result[2],2),')  \\'))
  }else if(table_name == 'table_2'){
    return(paste0(fart_result[3]*100,'  (',round(fart_result[4],2),') & ',boot_result[3]*100,'  (',round (boot_result[4],2),') & ',rf_result[3]*100,'  (',round (rf_result[4],2),') & ',bart_result[3]*100,'  (',round (bart_result[4],2),')  \\'))
  }else if(table_name == 'table_3'){
    return(paste0(fart_result[5]*100,'  (',round(fart_result[6],2),') & ',boot_result[5]*100,'  (',round (boot_result[6],2),') & ',rf_result[5]*100,'  (',round (rf_result[6],2),') & ',bart_result[5]*100,'  (',round (bart_result[6],2),')  \\'))
  }else if(table_name == 'table_4'){
    return(paste0(fart_result[8]*100,'  (',round(fart_result[9],2),') & ',bart_result[8]*100,'  (',round (bart_result[9],2),')  \\'))
  }else if(table_name == 'table_5'){
    return(paste0(fart_result[10]*100,'  (',round(fart_result[11],2),') & ',bart_result[10]*100,'  (',round (bart_result[11],2),')  \\'))
  }else{
    return(paste0(fart_result[12]*100,'  (',round(fart_result[13],2),') & ',bart_result[12]*100,'  (',round (bart_result[13],2),')  \\'))
  }
}

write_table = function(table_name){
  file_name = paste0(table_name, '.txt')
  title = 'function & n & p & FART& Bootstrap & Jackknife & BART '
  write (title,file=file_name)
  
  load('cosine_50_2.RData')
  result = matrix(result, 40, 1000)
  result = apply(result, 1, mean)
  row = paste0('Cosine & 50 & 2 & ',summary(result, table_name))
  write (row,file = file_name, append=TRUE)
  
  load('cosine_200_2.RData')
  result = matrix(result, 40, 1000)
  result = apply(result, 1, mean)
  row = paste0('Cosine & 200 & 2 & ',summary(result, table_name))
  write (row,file = file_name, append=TRUE)
  
  load('xor_50_50.RData')
  result = matrix(result, 40, 1000)
  result = apply(result, 1, mean)
  row = paste0('XOR & 50 & 50 & ',summary(result, table_name))
  write (row,file = file_name, append=TRUE)
  
  load('xor_200_50.RData')
  result = matrix(result, 40, 1000)
  result = apply(result, 1, mean)
  row = paste0('XOR & 200 & 50 & ',summary(result, table_name))
  write (row,file = file_name, append=TRUE)
  
  load('and_50_500.RData')
  result = matrix(result, 40, 1000)
  result = apply(result, 1, mean)
  row = paste0('AND & 50 & 500 & ',summary(result, table_name))
  write (row,file = file_name, append=TRUE)
  
  load('and_200_500.RData')
  result = matrix(result, 40, 1000)
  result = apply(result, 1, mean)
  row = paste0('AND & 200 & 500 & ',summary(result, table_name))
  write (row,file = file_name, append=TRUE)
}