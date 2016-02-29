########################################################
##############                            ##############
##############---------Yelp Project-------##############
##############                            ##############
########################################################
rm(list=ls())
setwd('C:/Users/bingx_000/Dropbox/yelp_project')

### ------------     load data     ---------------------
largedata = read.csv('features_per_link.txt',header=F)
colnames(largedata)=c("Y","i","j","d1","d2","N1","N2","elite","name","fan","city","busi")
largedata = largedata[,-(2:5)]   # I don't consider the degrees of i and j
head(largedata)
summary(largedata)
attach(largedata)
sum(Y==1) # 3930 edges

n = nrow(largedata)
index = sample(1:n)
# use a small sample because of the memory limitation of R
smalldata = largedata[1:500000,]

### ----------- Preprocessing the data -----------------

#### split the data into training set (70%), validation set (15%) and test set (15%) ####
split_data = function(data,ratio1,ratio2)
{
  set.seed(123456)
  total_index = sample(1:nrow(data))
  n_train = round(ratio1*length(total_index))
  train_index = total_index[1:n_train]
  n_valid = n_train + round(ratio2*length(total_index))
  valid_index = total_index[(n_train+1):n_valid]
  test_index = total_index[(n_valid+1):length(total_index)]
  train = data[train_index,]
  valid = data[valid_index,]
  test = data[test_index,]
  return(list(train=train,valid=valid,test=test))
}

splitted = split_data(smalldata,0.70,0.15)

raw_trainset = splitted[[1]]
raw_validset = splitted[[2]]
raw_testset = splitted[[3]]

#### standardize the inputs to N(0,1) ####
# use the scales obtained on the training set to standardize the validation and test set
scaled_attr = scale(raw_trainset[,-1],center=T,scale=T)
trainset = cbind(Y=raw_trainset[,1],scaled_attr)
scales = attr(scaled_attr,"scaled:scale")
centers = attr(scaled_attr,"scaled:center")
validset = cbind(Y=raw_validset[,1],scale(raw_validset[,-1],center=centers,scale=scales))
testset = cbind(Y=raw_testset[,1],scale(raw_testset[,-1],center=centers,scale=scales))

##########################################################
#####------------ Multi-Perceptron using SGD -------------
### initialize W and b
initialize_W = function(hiddens)
{
  n = length(hiddens)
  weights = vector("list",n-1)
  for (i in 1:(n-1))
  {
    weights[[i]] = matrix(NA,nrow=hiddens[i+1],ncol=hiddens[i])
    for (j in 1:hiddens[i+1])
    {
      for (k in 1:hiddens[i])
      {
        weights[[i]][j,k] = rnorm(1,0,sd=1/sqrt(hiddens[i]))
      }
    }
  }
  return(weights)
}
initialize_b = function(hiddens)
{
  n = length(hiddens)
  biases = vector("list",n-1)
  for (i in 1:(n-1))
  {
    biases[[i]] = matrix(NA,nrow=hiddens[i+1],ncol=1)
    for (j in 1:hiddens[i+1])
    {
      biases[[i]][j,1] = rnorm(1)
    }
  }
  return(biases)
}

### activation function by using logistic
sigmoid = function(x)
{
  return(1/(1+exp(-x)))
}
sigmoid_prime = function(x)
{
  return(sigmoid(x)*(1-sigmoid(x)))
}

### feed forward
feedforward = function(x,weights,biases)
{
  x = matrix(as.numeric(x),ncol=1)
  for (i in 1:length(weights))
  {
    x = sigmoid(weights[[i]]%*%x+biases[[i]])
  }
  return(x)
}

### back-propogation
backprop = function(x,y,weights,biases)
{
  n = length(weights)
  as = vector("list",n)
  zs = vector("list",n)
  a = x
  for (i in 1:n)
  {
    z = weights[[i]]%*%a + biases[[i]]
    zs[[i]] = z
    a = sigmoid(z)
    as[[i]] = a
  }
  nabla_w = vector("list",n)
  nabla_b = vector("list",n)
  delta = as[[n]] - y
  nabla_b[[n]] = delta
  nabla_w[[n]] = delta%*%t(as[[n-1]])
  for (h in 1:(n-1))
  {
    zh = zs[[n-h]]
    sp = sigmoid_prime(zh)
    delta = (t(weights[[n-h+1]])%*%delta)*sp
    nabla_b[[n-h]] = delta
    if (h==n-1)
    {
      nabla_w[[n-h]] = delta%*%t(x)
    }
    else
    {
      nabla_w[[n-h]] = delta%*%t(as[[n-h-1]])
    }
  }
  return(list(w=nabla_w,b=nabla_b))
}

### update the weights
update = function(data,k,c,lambda,weights,biases,total_length,iter)
{
  nabla_w = vector("list",length(weights))
  nabla_b = vector("list",length(biases))
  for (i in 1:length(weights))
  {
    nabla_w[[i]] = 0
    nabla_b[[i]] = 0
  }
  batch_size = nrow(data)
  for (i in 1:batch_size)
  {
    x = matrix(as.numeric(data[i,-1]),ncol=1)
    y = data[i,1]
    gradients = backprop(x,y,weights,biases)
    iter = iter + 1
    for (j in 1:length(weights))
    {
      nabla_w[[j]] = nabla_w[[j]]+gradients$w[[j]]
      nabla_b[[j]] = nabla_b[[j]]+gradients$b[[j]]
    }
  }
  for (i in 1:length(weights))
  {
    weights[[i]] = weights[[i]]*(1-lambda/(k+iter/c)/total_length) - nabla_w[[i]]/batch_size/(k+iter/c)
    biases[[i]] = biases[[i]] - nabla_b[[i]]/batch_size/(k+iter/c)
  }
  return(list(weights=weights,biases=biases,iter=iter))
}

### compute loss 
compute_loss = function(train,test,weights,biases,lambda,thresh)
{
  n_train = nrow(train)
  n_test = nrow(test)
  y_fitted = numeric(n_train)
  y_pred = numeric(n_test)
  for (i in 1:n_train)
  {
    y_fitted[i] = feedforward(train[i,-1],weights,biases)
  }
  for (i in 1:n_test)
  {
    y_pred[i] = feedforward(test[i,-1],weights,biases)
  }
  w_loss = 0
  for (i in 1:length(weights))
  {
    w_loss = w_loss + norm(weights[[i]],type='2') 
  }
  train_L = -mean(train[,1]*log(y_fitted)+(1-train[,1])*log(1-y_fitted)) + 0.5*lambda/n_train*w_loss
  #train_l = mean(as.numeric(y_fitted>thresh) != train[,1])
  test_L = -mean(test[,1]*log(y_pred)+(1-test[,1])*log(1-y_pred)) + 0.5*lambda/n_test*w_loss
  #test_l = mean(as.numeric(y_pred>thresh) != test[,1])
  return(list(train_L=train_L,y_fitted=y_fitted,test_L=test_L,y_pred=y_pred))
}

### stochastic gradient descent
SGD = function(train_data,test_data,epoch,batch_size,k,c,lambda,
               hiddens,thresh,startweights=NULL,startbiases=NULL)
{
  # if no initial weights, randomly generate
  if (missing(startweights))
  {
    weights = initialize_W(hiddens)
  }
  else
  {
    weights = startweights
  }
  if (missing(startbiases))
  {
    biases = initialize_b(hiddens)
  }
  else
  {
    biases = startbiases
  }
  train_logit = test_logit = NULL
  iter = 0
  best_test_error = Inf
  # for each epoch, split the data into mini-batch
  for (i in 1:epoch)
  {
    length = nrow(train_data)
    index = sample(1:length)
    shuffled_data = train_data[index,]
    times = round(length/batch_size)
    record_frequency = round(0.2*times)
    for (j in 1:(times-1))
    {
      mini_batch = shuffled_data[((j-1)*batch_size+1):(j*batch_size),]
      updated_weights = update(mini_batch,k,c,lambda,weights,biases,length,iter)
      weights = updated_weights$weights
      biases = updated_weights$biases
      iter = updated_weights$iter
      # check whether we need to record
      if (j%%record_frequency == 0)
      {
        losses = compute_loss(train_data,test_data,weights,biases,lambda,thresh)
        cat(j,'/',times,"of epoch",i,":Training loss is",losses$train_L,'and test loss is', losses$test_L,'\n')
        #cat('Training error is',losses$train_l,'and test error is',losses$test_l,'\n')
        train_logit = c(train_logit,losses$train_L)
        #train_01 = c(train_01,losses$train_l)
        test_logit = c(test_logit,losses$test_L)
        #test_01 = c(test_01,losses$test_l)
        if (losses$test_L < best_test_error)
        {
          best_model = list(w=weights,b=biases,trainL=losses$train_L,y_fitted=losses$y_fitted,
                            testL=losses$test_L,y_pred=losses$y_pred)
          best_test_error = losses$test_L
        }
      }
    }
  }
  return(list(w=weights,b=biases,trainL=train_logit,testL=test_logit,bestmodel=best_model))
}
##########################################################

### --------- Yelp Features of Users  ---------------- 

###########################################################
#########                                        ##########
#########----------Logistic Regression-----------##########
#########                                        ##########
###########################################################

# write the formulae to fit logistic regression
name_columns = colnames(trainset)
formulae = paste(name_columns[1],"~",paste(name_columns[-1],collapse="+"))

# fit logistic regression
logis.model = glm(formulae,data=data.frame(trainset),family=binomial(link="logit"))

# output the summary
library(xtable)
xtable(summary(logis.model),digits=4)

# extract the fitted value
LG.fitted = logis.model$fitted.values
# get predicted value on the validation set
LG.pred = predict(logis.model,newdata=data.frame(testset),type="response")

###########################################################
########                                       ############
########---------Perform Neural Network------- ############
########                                       ############
###########################################################

#### 2-layer NN ####
#### choose hyper-parameters by trials and get the following hyper-parameters
mlp.result = vector("list",3)
lambda = c(0.01,0.1,1,10,100)
for (i in 1:5)
{
  mlp.result[[i]] = SGD(trainset,validset,6,100,0.1,500000,lambda=lambda[i],hiddens=c(7,4,1))
}
NN.valid.model = SGD(trainset,validset,6,100,0.1,500000,lambda=0.1,hiddens=c(7,4,1))
#### use chosen hyper-parameter fit model on the training and test on the test set
NN.model = SGD(trainset,testset,5,100,0.1,500000,lambda=0.1,hiddens=c(7,4,1))
# get prediction and fitted values
NN.fitted = NN.model$bestmodel$y_fitted
NN.pred = NN.model$bestmodel$y_pred

# define a function: for a given thresh, calculate the TURE POSITIVE RATE and FALSE POSITIVE Rate
get_TPR_FPR = function(fitted,response,thresh)
{
  N_true = sum(response==1)
  N_false = sum(response==0)
  TPR = sum(fitted[response==1]>=thresh)/N_true
  FPR = sum(fitted[response==0]>=thresh)/N_false
  accuracy = mean(as.numeric(fitted>=thresh)==response)
  return(list(TPR=TPR,FPR=FPR,accuracy=accuracy))
}

# get the ROC curve on training set and validation set for Neural Network and logistic regression
thresh = seq(0,1,by=0.0001)
NN_ROC_test = NN_ROC_trainset = matrix(NA,nrow=2,ncol=length(thresh))
LG_ROC_trainset = LG_ROC_test = matrix(NA,nrow=2,ncol=length(thresh))
LG_accuracy_train=NN_accuracy_train=LG_accuracy_test=NN_accuracy_test=numeric(length(thresh))
for (i in 1:length(thresh))
{
  NN_tmp_train = get_TPR_FPR(NN.fitted,trainset[,1],thresh[i])
  NN_tmp_test = get_TPR_FPR(NN.pred,testset[,1],thresh[i])
  LG_tmp_train = get_TPR_FPR(LG.fitted,trainset[,1],thresh[i])
  LG_tmp_test = get_TPR_FPR(LG.pred,testset[,1],thresh[i])
  NN_accuracy_train[i] = NN_tmp_train$accuracy
  LG_accuracy_train[i] = LG_tmp_train$accuracy
  NN_accuracy_test[i] = NN_tmp_test$accuracy
  LG_accuracy_test[i] = LG_tmp_test$accuracy
  NN_ROC_trainset[1,i] = NN_tmp_train$TPR
  NN_ROC_trainset[2,i] = NN_tmp_train$FPR
  NN_ROC_test[1,i] = NN_tmp_test$TPR
  NN_ROC_test[2,i] = NN_tmp_test$FPR
  LG_ROC_trainset[1,i] = LG_tmp_train$TPR
  LG_ROC_trainset[2,i] = LG_tmp_train$FPR
  LG_ROC_test[1,i] = LG_tmp_test$TPR
  LG_ROC_test[2,i] = LG_tmp_test$FPR
}

# plot the ROC curve
par(mar=c(4,4,2,2))
plot(NN_ROC_trainset[2,],NN_ROC_trainset[1,],xlim=c(-0.1,1.1),ylim=c(-0.1,1.1),main="ROC curve on training set",
     xlab="False Positive Rate", ylab="True Positive Rate",type="l",lty=1,col=2,lwd=2)
points(LG_ROC_trainset[2,],LG_ROC_trainset[1,],type="l",lty=1,col=3,lwd=2)
segments(0,0,1,1,lty=2)
legend(0.5,0.2,legend=c("Neural Network","Logistic"),bty="n",col=2:3,lty=1,lwd=2)
plot(NN_ROC_test[2,],NN_ROC_test[1,],xlim=c(-0.1,1.1),ylim=c(-0.1,1.1),main="ROC curve on test set",
     xlab="False Positive Rate", ylab="True Positive Rate",type="l",lty=1,col=2,lwd=2)
points(LG_ROC_test[2,],LG_ROC_test[1,],type="l",lty=1,col=3,lwd=2)
segments(0,0,1,1,lty=2)
legend(0.5,0.2,legend=c("Neural Network","Logistic"),bty="n",col=2:3,lty=1,lwd=2)

