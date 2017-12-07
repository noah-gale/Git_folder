# Data work on the classification


# Set the working Directory to whatever you need it to be.
setwd("/Users/noahgale/Desktop/UMich Classes and Clubs/Stat 503/Project")


# Laod the libraries
install.packages("NeuralNetTools")
library(NeuralNetTools)
library(plyr); require(dplyr)
require(caTools)
library(e1071)
library(ISLR)
require(class)
library(sparsediscrim)
require(neuralnet)
require(ggplot2)
require(knitr)
library(gridExtra)
# Let's do NNets

clean_polish_dt = read.table("cleandata.txt" , header = TRUE)

pca = prcomp(clean_polish_dt[,-c(58,57,56,47,46,27,26,21,14)])

X = as.matrix(clean_polish_dt[,-c(58,57,56,47,46,27,26,21,14)])
M = pca$rotation[,1:5]

pca_polish_data = as.data.frame((X %*% M))
pca_polish_data = log((pca_polish_data^2))
pca_polish_data = cbind(pca_polish_data,clean_polish_dt[,c(58,57,56,47,46,27,26,21,14)])
pca_polish_data$year = as.factor(pca_polish_data$year)


data_sp = pca_polish_data%>%group_by(year) # group them by year / we are using the cleaned file after imputation !!
# install.packages("caTools")

data_sp$rownumber = 1:nrow(data_sp) # an indicator column for each row to check if the split worked fine

set.seed(123) 
sample = sample.split(data_sp, SplitRatio = 2/3) # we pick 2/3 split ratio
train = subset(data_sp, sample == TRUE)%>%as.data.frame() # 1/3 proportion of the training set
test = subset(data_sp, sample == FALSE)%>%as.data.frame() # 2/3 proportion for the test set 

# the split worked fine so if we want
# we can delete the extra column
train = subset(train , select = -rownumber)
test = subset(test , select = -rownumber)

#train = train[as.numeric(train$year) == 5,]
#test = test[as.numeric(test$year) == 5,]

train2 = train
test2 = test


train = data.frame(train[,c(1:6,8:14)])
test = data.frame(test[,c(1:6,8:14)])
train[,14] = train2[,7]
test[,14] = test2[,7]
# I think we only need e1071 library



xnames = colnames(train2)
xnames = xnames[c(1:6,8:14)]
nntrain = data.frame(train2[c(1:6,8:14)])
nntest = data.frame(test2[c(1:6,8:14)])
nntrain$class = train2[,7]
nntest$class = test2[,7]

# have to break down V58 into 2 columns
nntrain2 = data.frame(model.matrix(~factor(nntrain$class)-1))
nntest2 = data.frame(model.matrix(~factor(nntest$class)-1))

#get rid of year for now:
nntrain = nntrain[,-6]
nntest = nntest[,-6]
#Now see if we can center the data:
nntrain4 = scale(data.frame(nntrain[,1:12]))
nntest4 = scale(data.frame(nntest[,1:12]))


nntrain5 = data.frame(nntrain4)
nntest5 = data.frame(nntest4)


# Add back the two columns of BK and non-BK
nntrain6 = cbind(nntrain5, nntrain2)
nntest6 = cbind(nntest5, nntest2)
colnames(nntrain6)[13] = "Not_BK"
colnames(nntrain6)[14] = "BK"
colnames(nntest6)[13] = "Not_BK"
colnames(nntest6)[14] = "BK"


# Now we do NNEts

n = names(nntrain6[,1:12])
f= as.formula(paste("BK + Not_BK ~", paste(n, collapse = " + ")))
# Does this work? No. 2, and 3 layer NNets don't work.
# nn1 = neuralnet(f, data= nntrain6, hidden= 10, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)



pred = function(nn, dat) {
  yhat = compute(nn, dat)$net.result
  yhat = apply(yhat, 1, round) # rounding is better
  return(yhat)
}


## Need to make a function that makes 30-ish NNEts.
NN = list()
Mean_test_error = as.list(rep(0, 27))
Mean_train_error = as.list(rep(0, 27))
table_test = list()
table_train = list()

# Input for Minimum Nodes
min = 3
# Input for Max Nodes:
max = 30

NN[[1]] = neuralnet(f, data= nntrain6, hidden= 3, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[2]] = neuralnet(f, data= nntrain6, hidden= 4, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[3]] = neuralnet(f, data= nntrain6, hidden= 5, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[4]] = neuralnet(f, data= nntrain6, hidden= 6, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[5]] = neuralnet(f, data= nntrain6, hidden= 7, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[6]] = neuralnet(f, data= nntrain6, hidden= 8, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[7]] = neuralnet(f, data= nntrain6, hidden= 9, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[8]] = neuralnet(f, data= nntrain6, hidden= 10, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[9]] = neuralnet(f, data= nntrain6, hidden= 11, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[10]] = neuralnet(f, data= nntrain6, hidden= 12, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[11]] = neuralnet(f, data= nntrain6, hidden= 13, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[12]] = neuralnet(f, data= nntrain6, hidden= 14, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[13]] = neuralnet(f, data= nntrain6, hidden= 15, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[14]] = neuralnet(f, data= nntrain6, hidden= 16, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[15]] = neuralnet(f, data= nntrain6, hidden= 17, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[16]] = neuralnet(f, data= nntrain6, hidden= 18, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[17]] = neuralnet(f, data= nntrain6, hidden= 19, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[18]] = neuralnet(f, data= nntrain6, hidden= 20, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[19]] = neuralnet(f, data= nntrain6, hidden= 21, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[20]] = neuralnet(f, data= nntrain6, hidden= 22, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[21]] = neuralnet(f, data= nntrain6, hidden= 23, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[22]] = neuralnet(f, data= nntrain6, hidden= 24, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[23]] = neuralnet(f, data= nntrain6, hidden= 25, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[24]] = neuralnet(f, data= nntrain6, hidden= 26, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[25]] = neuralnet(f, data= nntrain6, hidden= 27, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[26]] = neuralnet(f, data= nntrain6, hidden= 28, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[27]] = neuralnet(f, data= nntrain6, hidden= 29, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
NN[[28]] = neuralnet(f, data= nntrain6, hidden= 30, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)



NN1_plot = plot(NN[[1]], file = "NN1_plot_3_nodes")
NN2_plot = plot(NN[[2]], file = "NN1_plot_4_nodes")
NN3_plot = plot(NN[[3]], file = "NN1_plot_5_nodes")
NN4_plot = plot(NN[[4]], file = "NN1_plot_6_nodes")
NN5_plot = plot(NN[[5]], file = "NN1_plot_7_nodes")
NN6_plot = plot(NN[[6]], file = "NN1_plot_8_nodes")
NN7_plot = plot(NN[[7]], file = "NN1_plot_9_nodes")
NN8_plot = plot(NN[[8]], file = "NN1_plot_10_nodes")



# New Loop, we only need the tables:
for(i in 1:28){
  train_result = as.matrix(t(pred(NN[[i]], nntrain6[,1:12])), nrow = length(nntrain6[,1]), ncol = 2)
  test_result = as.matrix(t(pred(NN[[i]], nntest6[,1:12])), nrow = length(nntest6[,1]), ncol= 2)
  table_test[[i]] = table(test_result[1:length(nntest[,1])],  nntest6$BK)
  table_train[[i]] = table(train_result[1:length(nntrain[,1])],  nntrain6$BK)
  Mean_test_error[[i]] = mean(test_result != nntest6[,c("BK","Not_BK")])
  Mean_train_error[[i]] = mean(train_result != nntrain6[,c("BK","Not_BK")])
}


saveRDS(table_test, file = "table_test_list")
saveRDS(table_train, file = "table_train_list")
saveRDS(Mean_test_error, file = "mean_test_error_list")
saveRDS(Mean_train_error, file = "mean_train_error_list")

# then use VariableName = readRDS() to load the files back


# basic plots of the error rates
plot(1:28, Mean_test_error, main = "Plot of Mean Test Error for NNets", xlab = "Neural Net Node Number")
plot(1:28, Mean_train_error, main = "Plot of Mean Train Error for NNets", xlab = "Neural Net Node Number")


## FALSE POSITIVE RATE:
test_FPR_NNet = sapply(table_test, function(x) 1 -  x[2,2]/(x[2,1]+x[2,2]))
train_FPR_NNet = sapply(table_train, function(x) 1 -  x[2,2]/(x[2,1]+x[2,2]))

df_FPR_NNet = data.frame(Error = c(test_FPR_NNet,train_FPR_NNet), 
                    Set = c(rep("Test",28),rep("Train",28)),
                    Nodes = seq(from = 3,to = 30, by = 1))

g1 = ggplot(df_FPR_NNet, aes(x = Nodes, y = Error, color = Set)) + 
  geom_line() + 
  labs(title = "False Positive Rate on Testing and Training Sets NNet")

## FALSE NEGATIVE RATE:

testing_FNR_NNet = sapply(table_test, function(x) x[1,2]/(x[1,1]+x[1,2]))
training_FNR_NNet = sapply(table_train, function(x) x[1,2]/(x[1,1]+x[1,2]))

df_FNR_NNet = data.frame(Error = c(testing_FNR_NNet,training_FNR_NNet), 
                    Set = c(rep("Test",28),rep("Train",28)),
                    Nodes = seq(from = 3,to = 30, by = 1))

g2 = ggplot(df_FNR_NNet, aes(x = Nodes, y = Error, color = Set)) + 
  geom_line() + 
  labs(title = "False Negative Rate on Testing and Training Sets NNEts")

mean_test_error_matrix = rep(0,28)
mean_train_error_matrix = rep(0, 28)
for(i in 1:28){
  mean_test_error_matrix[i] = Mean_test_error[[i]]
  mean_train_error_matrix[i] = Mean_train_error[[i]]
}

df_MTE_NNet = data.frame(Error = c(mean_test_error_matrix,mean_train_error_matrix), 
                         Set = c(rep("Test",28),rep("Train",28)),
                         Nodes = seq(from = 3,to = 30, by = 1))

g3 = ggplot(df_MTE_NNet, aes(x = Nodes, y = Error, color = Set)) + 
  geom_line() + 
  labs(title = "Mean Error Rate on Testing and Training Sets NNEts")


grid.arrange(g1,g2,g3, ncol = 3, nrow = 1)


# We're going to pick 5 Nodes as the best, since the training and test errors are closest at around Nodes=5,  

## Results for best NNEt:
# 5 nodes corresponds with 3rd in list.
r = 3
train_result = as.matrix(t(pred(NN[[r]], nntrain6[,1:12])), nrow = length(nntrain6[,1]), ncol = 2)
test_result = as.matrix(t(pred(NN[[r]], nntest6[,1:12])), nrow = length(nntest6[,1]), ncol= 2)

DT_NNet = cbind(test[,1:2], pred = as.factor(test_result[,1]))
q1 = qplot(DT_NNet$PC1, DT_NNet$PC2, color = DT_NNet$pred, alpha = DT_NNet$pred, xlim = c(-10,40), ylim = c(-10,40))

test_result2= data.frame(test_result)
q2 = qplot(test$PC1, test$PC2, color = as.factor(test$V1), alpha = as.factor(test$V1), xlim = c(-10,25), ylim = c(-10,40))
grid.arrange(q1,q2, ncol = 2, nrow = 1)



## COMPUTING NNET DATA:

library(nnet)
# We have to do it with NNet package.
fake_data = rbind(nntrain6, nntest6)
LEKNNET  = nnet.formula(f, data = fake_data, subset = length(nntrain6[,1]), size = 8, lineout = FALSE, abstol = 0.01)

lekprofile(LEKNNET, xsel = NULL, ysel = NULL, group_vals = seq(0, 1, by = 0.1))
