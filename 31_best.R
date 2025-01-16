set.seed(15)
library(class)
# RKNN-FS is an innovative feature selection procedure for "small n, large p problems." RKNN-FS is
# based on Random KNN (RKNN), a novel generalization of traditional nearest-neighbor modeling. RKNN consists of
# an ensemble of base k-nearest neighbor models, each constructed from a random subset of the input variables
# Ref: Li, S., Harner, E.J. & Adjeroh, D.A. Random KNN feature selection - a fast and stable alternative to Random Forests. 
# BMC Bioinformatics 12, 450 (2011). https://doi.org/10.1186/1471-2105-12-450
# After feature selection with rKNN, final model is built with KNN, see: https://sms.wgtn.ac.nz/foswiki/pub/Events/WWPMS2019/WebHome/E_James_Harner_2019.pdf
#import the data 
dataset<-load("class_data.RData")


require('rknn')
set.seed(20081031)

#Before selecting features, it is a good practice to split data into train test
#Moreover, the rknn algorithm requires a test set to run
train=sample(1:nrow(x),300)
dat.rnn<- rknn(data=xdata[train,], y=ydata[train], newdata=xdata[-train,],
               r=821, mtry=55, seed=20081031)
#check the confusion matrix and accuracy of rknn
#Recall, we are only using rknn to select model for knn
confusion(ydata[-train], fitted(dat.rnn))
mean(fitted(dat.rnn)==ydata[-train])

#Feature selection using rknn
?rknnBeg
rnn.beg<- rknnBeg(xdata[train,], ydata[train], seed=155)
#plot the mean accuracy depending on number of features selected
plot(rnn.beg)

#check "better" set of features
better.set<- prebestset(rnn.beg)
#view the features
better.set

#Recursive Backward Elimination Feature Selection with Random KNN to select "best"
#set out of better set

rnn.bel<- rknnBel(xdata[train,better.set], ydata[train], seed=155)
plot(rnn.bel, ylim=c(0.5, 1))

#check the best set of features
best.set<- bestset(rnn.bel)
cat(best.set, sep=", ")


#Use LOO-CV to select optimal k for best features
#KNN.cv does CV by estimating LOO-CV
set.seed(15)
res <- vector(length=99)
c <- seq(from = 2, to = 100, by = 1)
for (i in c) {
  results = knn.cv(x[,best.set], y, k=i)
  res[[i]] = mean(results==y)
}
#select the best k which has the max accuracy in the vector
best_k = which.max(res)

#view the best k
best_k

#view test error with best k
1-max(res)
best_test_error <- 1-max(res)
#Conclusion: The test error improved to 0.173 from 0.245


classifier_knn <- knn(train = x[,best.set], test = x[,best.set],
                      cl = y,
                      k = 3)
classifier_knn

save(classifier_knn,0.173,file="31.RData")