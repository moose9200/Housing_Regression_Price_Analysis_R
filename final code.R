rm(list=ls())

library(ggplot2)

#set current working directory
setwd("C:/Users/Moose/Desktop/housemarket kaggale")

train = read.csv("train.csv")


  
#####################################################################

mis=sapply(train, function(x) sum(is.na(x))) # missing values by column
mis= as.data.frame(mis)

cn = row.names(mis)
mis = cbind(mis,cn)
row.names(mis)=NULL
mis =mis[order(-mis$mis),]
mis$mis = mis$mis/nrow(train)*100



#bar plot missing values percentage vs Column name

ggplot(data = mis[1:40,], 
       aes(x=reorder(cn,-mis),
           y = mis)) + geom_bar(stat = "identity", fill = "grey") + 
  xlab("Parameter") + ggtitle("Missing_Data_Percentage (Train)") + theme_bw()


#scatter plot missing values percentage vs Column name
ggplot(mis[1:20,], aes_string(x = "mis", y = "cn")) +
  geom_point(size = 2, shape = 23)
###########################################################################

# Look for columns having more than 40% of missing values and remove them

train = train[-c(25,11,8,161,162,163)]

mis = mis[-c(1,2,3,4,5,6),]

#############################################################################

#missing value tratment

mis_col=train[colSums(is.na(train)) > 0]

library(DMwR)
mis_col =knnImputation(mis_col)
write.csv(mis_col,"knn_imputed.csv", row.names = F)
knn_imputed = read.csv("knn_imputed.csv")

#########################################################################

train=train[!colSums(is.na(train)) > 0] # select columns with non missing values

train =cbind(train,knn_imputed) #bind non missing values with imputed missing values by knn
sum(is.na(train))

##########################################################################

#devide categorical and numeric data into different data set

train_num = as.data.frame(Filter(is.numeric, train))
train_cat = as.data.frame(Filter(is.factor, train))
train_cat[,1]=NULL
 
#Outlier treatment for numeric dataframe

capping <- function(x){
  for (i in which(sapply(x, is.numeric))) {
    quantiles <- quantile( x[,i], c(.05, .95 ), na.rm =TRUE)
    x[,i] = ifelse(x[,i] < quantiles[1] , quantiles[1], x[,i])
    x[,i] = ifelse(x[,i] > quantiles[2] , quantiles[2], x[,i])}
  x}



# Replacing extreme values with percentiles
without_outliers_num= capping(train_num)

train = NULL
knn_imputed = NULL

library(clusterSim) #normlization

normalize<-function(object){ return((object-min(object))/(max(object)-min(object))) }


without_outliers_nor_num= normalize(without_outliers_num)[,-225]

without_outliers_nor_num=cbind(without_outliers_nor_num,without_outliers_num$price_doc)
without_outliers_num= NULL

names(without_outliers_nor_num)[270]="price_doc"

 
# load the library
library(mlbench)
library(rpart)
library(caret)

p = cbind(without_outliers_nor_num,train_cat)

fit = rpart(price_doc ~ ., data = p, method = "anova")

#variable Importance

gbmImp <- varImp(fit, scale = FALSE)

gbmImp


#Choose Important columns

imp_col = subset(gbmImp, Overall > 0)


#Subsetting only thoese column from dataset which are important
q = p[,c(row.names(imp_col))]


#target variable has been excluded by gbm so we have to bind our target variable again
q = cbind.data.frame(q,p$price_doc)

names(q)[18] = "price_doc"

sub_area = dummy(q$ sub_area )

q = cbind(q,sub_area )
which( colnames(q)=="sub_area" )

q[17] = NULL

q=sapply(q,function(x) as.numeric(x))
which( colnames(q)=="price_doc" )

q= as.data.frame(q)

write.csv(q, "trainxgb.csv", row.names = FALSE)


#Model BUilding 

train = q[sample(nrow(q), 22000, replace = F), ]
test = q[!(1:nrow(q)) %in% as.numeric(row.names(train)), ]


library(data.table)
train = as.data.table(train)
test = as.data.table(test)

library(xgboost)
library(caret)


which( colnames(q)=="sub_area" )



#Model Bilding
dtrain = xgb.DMatrix(data = as.matrix(train[,-c('price_doc'),with=F]), label = train$price_doc)
dtest = xgb.DMatrix(data = as.matrix(test[,-c('price_doc'),with=F]), label = test$price_doc)



params <- list(booster = "gbtree", objective = "reg:linear", 
               eta=0.3, gamma=0, max_depth=5, min_child_weight=1, subsample=1, colsample_bytree=1)

xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 82, watchlist = list(val=dtest,train=dtrain), 
                   print_every_n = 10, early_stop_round = 10, maximize = F , eval_metric = "error")

xgbpred <- predict (xgb1,dtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)
confusionMatrix (xgbpred, test$final_status)











# Prection on test Data set
test_file = read.csv("test.csv")


#to combine id in final result take a backup of this

id = as.data.frame(test_file[1])

#Select important columns which are calculated earlier and stored in imp_col
test_file = test_file[,c(row.names(imp_col))]

sapply(test_file, function(x) sum(is.na(x)))

test_file = knnImputation(test_file)

sum(is.na(test_file))

test_file_num = as.data.frame(Filter(is.numeric, test_file))
test_file_cat = as.data.frame(Filter(is.factor, test_file))

test_without_outliers_num= capping(test_file_num)

test_without_outliers_num_nor= normalize(test_without_outliers_num)

test_data= cbind(test_without_outliers_num_nor,test_file_cat)


sub_area = dummy(test_data$ sub_area )

test_data = cbind(test_data,sub_area )
which( colnames(test_data)=="sub_area" )

test_data[17] = NULL

test_data=sapply(test_data,function(x) as.numeric(x))

test_data = as.data.frame(test_data)


write.csv(test_data, "test_xgb.csv", row.names = FALSE)






test_prediction = predict(fit1, test_data)

test_prediction = cbind(id,test_prediction)
names(test_prediction)[2] = "price_doc"
write.csv(test_prediction, "Predicted_result.csv", row.names = FALSE)



