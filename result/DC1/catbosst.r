library(tidyverse)
library(catboost)
library(caTools)
library(pROC)
cat_df <- select(df2, lmt, everything())
cat_df[, 2:length(cat_df)] <- sapply(cat_df[, 2:length(cat_df)], 
                                  function(x){factor(x, levels = sort(unique(x)))}) %>% 
  data.frame()

# 0.75578 ,auc0.72
set.seed(99228)
model_train <- cat_df[1:nrow(tt), ]
model_test <- cat_df[(nrow(tt)+ 1): nrow(cat_df), ]  #测试数据
ind <- sample(nrow(tt), size = 0.8*nrow(tt))
dtrain <- cbind(tt[ind, 'target'], model_train[ind, ])
dtest <- cbind(tt[-ind, 'target'], model_train[-ind, ])


dtrain <- catboost.load_pool(data = dtrain[, -1],
                                label = dtrain$target,
                             cat_features = c(3:length(dtrain)))
dtest <- catboost.load_pool(data = dtest[, -1],
                               label = dtest$target,
                            cat_features = c(3:length(dtrain)))
d_test = catboost.load_pool(data = model_test,
                            cat_features = c(3:length(dtrain)))
fit_params <- list(learning_rate = 0.1,
                   iterations = 500,
                   loss_function = 'Logloss', #CrossEntropy
                   custom_loss = c('AUC'),
                   l2_leaf_reg = 30, #17
                   use_best_model = T,
                   #border_count = 32,
                   depth = 5,
                   class_weights = c(1, 20), #c(1, 20)
                   rsm = 0.85, #列抽样比例
                   random_seed = 1518,
                   od_type = 'Iter',  #检测过拟合类型
                   od_wait = 30,
                   #train_dir =  '/train_dir', #储存trian信息
                   logging_level = 'Info')

cat <- catboost.train(dtrain, dtest, fit_params)
w <- cbind(tt[-ind, 'target'], model_train[-ind, ])
w <- catboost.load_pool(data = w,
                        cat_features = c(3:length(w)))
re <- catboost.predict(cat, w, prediction_type = 'Probability')
roc1 <- roc(tt$target[-ind], re)
plot(roc1,print.auc=T, auc.polygon=T, auc.polygon.col="skyblue")
roc1 <- auc(tt$target[-ind], re, levels = c(0, 1))

cat_res <- catboost.predict(cat, d_test, prediction_type = 'Probability') %>% 
  data.frame(id = test$id, target = .)
write.csv(cat_res, 'res1.csv', row.names = F)




set.seed(99228)
model_train = cat_df[1:nrow(tt), ]
model_test = cat_df[(nrow(tt)+ 1): nrow(cat_df), ]  #测试数据
ind = sample(nrow(tt), size = 0.8*nrow(tt))
dtrain = cbind(tt[ind, 'target'], model_train[ind, ])
dtest = cbind(tt[-ind, 'target'], model_train[-ind, ])


dtrain <- catboost.load_pool(data = dtrain[, -1],
                             label = dtrain$target,
                             cat_features = c(3:length(dtrain)))
dtest <- catboost.load_pool(data = dtest[, -1],
                            label = dtest$target,
                            cat_features = c(3:length(dtrain)))
d_test = catboost.load_pool(data = model_test,
                            cat_features = c(3:length(dtrain)))
fit_params <- list(learning_rate = 0.1,
                   iterations = 500,
                   loss_function = 'Logloss', #CrossEntropy
                   custom_loss = c('AUC'),
                   l2_leaf_reg = 30, #14
                   use_best_model = T,
                   #border_count = 32,
                   depth = 5,
                   class_weights = c(1, 20), #c(1, 20)
                   rsm = 0.85, #列抽样比例
                   random_seed = 1518,
                   od_type = 'Iter',  #检测过拟合类型
                   od_wait = 30,
                   #train_dir =  '/train_dir', #储存trian信息
                   logging_level = 'Info')

cat <- catboost.train(dtrain, dtest, fit_params)
w <- cbind(tt[-ind, 'target'], model_train[-ind, ])
w <- catboost.load_pool(data = w,
                        cat_features = c(3:length(w)))
re <- catboost.predict(cat, w, prediction_type = 'Probability')
roc1 <- roc(tt$target[-ind], re)
plot(roc1,print.auc=T, auc.polygon=T, auc.polygon.col="skyblue")
roc1 <- auc(tt$target[-ind], re, levels = c(0, 1))

cat_res <- catboost.predict(cat, d_test, prediction_type = 'Probability') %>% 
  data.frame(id = test$id, target = .)
write.csv(cat_res, 'res1.csv', row.names = F)


#lgb结果，A榜0.75781
d <- read_csv('C:/Users/nigpz/Desktop/r/dcdata/dtest_pre1.csv', col_names = F) %>% 
  data.frame(id = test$id, target = .) 
names(d) = c('id', 'target')
write.csv(d, 'res1.csv', row.names = F)


#二次
set.seed(2020)
model_train = cat_df[1:nrow(tt), ]
model_test = cat_df[(nrow(tt)+ 1): nrow(cat_df), ]  #测试数据
ind = sample(nrow(tt), size = 0.8*nrow(tt))
dtrain = cbind(tt[ind, 'target'], model_train[ind, ])
dtest = cbind(tt[-ind, 'target'], model_train[-ind, ])


dtrain <- catboost.load_pool(data = dtrain[, -1],
                             label = dtrain$target,
                             cat_features = c(3:length(dtrain)))
dtest <- catboost.load_pool(data = dtest[, -1],
                            label = dtest$target,
                            cat_features = c(3:length(dtrain)))
d_test = catboost.load_pool(data = model_test,
                            cat_features = c(3:length(dtrain)))
fit_params <- list(learning_rate = 0.1,
                   iterations = 500,
                   loss_function = 'Logloss', #CrossEntropy
                   custom_loss = c('AUC'),
                   l2_leaf_reg = 30, #14
                   use_best_model = T,
                   #border_count = 32,
                   depth = 4,
                   class_weights = c(1, 20), #c(1, 20)
                   rsm = 0.8, #列抽样比例
                   random_seed = 1518,
                   od_type = 'Iter',  #检测过拟合类型
                   od_wait = 30,
                   #train_dir =  '/train_dir', #储存trian信息
                   logging_level = 'Info')

cat <- catboost.train(dtrain, dtest, fit_params)
w <- cbind(tt[-ind, 'target'], model_train[-ind, ])
w <- catboost.load_pool(data = w,
                        cat_features = c(3:length(w)))
re <- catboost.predict(cat, w, prediction_type = 'Probability')
roc1 <- roc(tt$target[-ind], re)
plot(roc1,print.auc=T, auc.polygon=T, auc.polygon.col="skyblue")





